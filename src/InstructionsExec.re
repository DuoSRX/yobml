open Cpu
open Instructions

let wrapping_add = (a, b) => (a + b) land 0xFF
let wrapping_add16 = (a, b) => (a + b) land 0xFFFF

let load = (cpu, address) => Memory.load(cpu.memory, address)
let load16 = (cpu, address) => Memory.load16(cpu.memory, address)
let store = (cpu, address, value) => Memory.store(cpu.memory, address, value)

let load_next = (cpu) => load(cpu, cpu.pc)
let load_next16 = (cpu) => load16(cpu, cpu.pc)

// Call after every instruction to change the PC
// and increase the cycle count
let bump = (cpu, pc, cycles) => {
  {...cpu, pc:pc, cycle:(cpu.cycle+cycles)}
}

let nop = (cpu) => bump(cpu, cpu.pc, 4)

let cp_n = (cpu) => {
  let reg = get_register(cpu, A);
  let byte = load_next(cpu);
  let h = (byte land 0xF) > (reg land 0xF);
  set_flags(cpu, ~z=(reg == byte), ~n=true, ~h, ~c=(reg < byte), ());
  bump(cpu, cpu.pc + 1, 8)
}

let di = (cpu) =>
  bump({...cpu, ime: false}, cpu.pc, 4)

let ld_rr = (cpu, r1, r2) => {
  get_register(cpu, r2) |> set_register(cpu, r1)
  bump(cpu, cpu.pc, 4)
}

let ld_n = (cpu, r) => {
  load_next(cpu) |> set_register(cpu, r);
  bump(cpu, cpu.pc + 1, 8)
}

let ld_nn = (cpu, r16) => {
  load16(cpu, cpu.pc) |> set_register16(cpu, r16);
  bump(cpu, cpu.pc + 2, 12)
}

let ld_hl_d8 = (cpu) => {
  let address = get_register16(cpu, HL)
  load_next(cpu) |> store(cpu, address)
  bump(cpu, cpu.pc + 1, 12)
}

let ld_r16_a = (cpu, r16) => {
  let address = get_register16(cpu, r16);
  get_register(cpu, A) |> store(cpu, address);
  bump(cpu, cpu.pc, 8)
}

let ldd_hl_a = (cpu) => {
  let address = get_register16(cpu, HL);
  get_register(cpu, A) |> store(cpu, address);
  set_register16(cpu, HL, wrapping_add16(address, -1));
  bump(cpu, cpu.pc, 8)
}

let ldi_a_hl = (cpu) => {
  let address = get_register16(cpu, HL);
  load(cpu, address) |> set_register(cpu, A);
  set_register16(cpu, HL, wrapping_add16(address, 1));
  bump(cpu, cpu.pc, 8)
}

let ld_read_io_n = (cpu) => {
  let n = load_next(cpu);
  let byte = load(cpu, wrapping_add16(0xFF00, n));
  set_register(cpu, A, byte);
  bump(cpu, cpu.pc + 1, 12)
}

let ld_write_io_n = (cpu) => {
  let n = load_next(cpu);
  let address = wrapping_add16(0xFF00, n);
  store(cpu, address, get_register(cpu, A));
  bump(cpu, cpu.pc + 1, 12)
}

let inc = (cpu, r) => {
  wrapping_add(get_register(cpu, r), 1) |> set_register(cpu, r)
  bump(cpu, cpu.pc, 4)
}

let dec = (cpu, r) => {
  let reg = get_register(cpu, r);
  let value = wrapping_add(reg, -1);
  set_register(cpu, r, value);
  let h = (value land 0xF) > (reg land 0xF);
  set_flags(cpu, ~z=(value == 0), ~n=true, ~h, ());
  bump(cpu, cpu.pc, 4)
}

let jp = (cpu) => {
  let address = load_next16(cpu)
  bump(cpu, address, 16)
}

let jr = (cpu, flag, condition) => {
  let do_jump = has_flag(cpu, flag) == condition

  if (do_jump) {
    let byte = load_next(cpu);
    // Treat the byte as a signed integer
    let offset = (byte < 0x80) ? byte : (byte - 0x100);
    bump(cpu, cpu.pc + offset + 1, 12)
  } else {
    bump(cpu, cpu.pc + 1, 8)
  }
}

let xor = (cpu, r1, r2) => {
  let a = get_register(cpu, r1);
  let b = get_register(cpu, r2);
  let result = a lxor b;
  set_register(cpu, r1, result);
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=false, ~c=false, ())
  bump(cpu, cpu.pc, 4)
}

let execute = (cpu, instruction) => switch(instruction) {
  | Nop => cpu
  | Cp_n => cp_n(cpu)
  | Dec(r) => dec(cpu, r)
  | Di => di(cpu)
  | Inc(r) => inc(cpu, r)
  | Ld_read_io_n => ld_read_io_n(cpu)
  | Ld_write_io_n => ld_write_io_n(cpu)
  // | Ld_read_io_c => ld_read_io_c(cpu)
  // | Ld_write_io_c => ld_write_io_c(cpu)
  | Ld_r16_a(r) => ld_r16_a(cpu, r)
  | Ld_rr(r1, r2) => ld_rr(cpu, r1, r2)
  | Ld_n(r) => ld_n(cpu, r)
  | Ld_hl_d8 => ld_hl_d8(cpu)
  | Ld_nn(r) => ld_nn(cpu, r)
  | Ldd_hl_a => ldd_hl_a(cpu)
  | Ldi_a_hl => ldi_a_hl(cpu)
  | Jp => jp(cpu)
  | Jr(flag, cond) => jr(cpu, flag, cond)
  | Xor(r1, r2) => xor(cpu, r1, r2)
}
