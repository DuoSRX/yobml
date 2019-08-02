open Cpu
open Instructions

exception FooEx

let wrapping_add = (a, b) => (a + b) land 0xFF
let wrapping_add16 = (a, b) => (a + b) land 0xFFFF

let load = (cpu, address) => Memory.load(cpu.memory, address)
let load16 = (cpu, address) => Memory.load16(cpu.memory, address)
let store = (cpu, address, value) => {
  if (address == 0xFF01 || address == 0xFF02) {
    cpu.serial = [char_of_int(value), ...cpu.serial];
  }
  Memory.store(cpu.memory, address, value)
}
let store16 = (cpu, address, value) => Memory.store16(cpu.memory, address, value)

let load_next = (cpu) => load(cpu, cpu.pc)
let load_next16 = (cpu) => load16(cpu, cpu.pc)

let storage_load = (cpu, storage) => switch(storage) {
  | Register(r) => get_register(cpu, r)
  | Register16(r) => get_register16(cpu, r)
  | Pointer(r) => get_register16(cpu, r) |> load(cpu)
};

let storage_store = (cpu, storage, value) => switch(storage) {
  | Register(r) => set_register(cpu, r, value)
  | Register16(r) => set_register16(cpu, r, value)
  | Pointer(r) => get_register16(cpu, r)->store(cpu, _, value)
};

// Call after every instruction to change the PC
// and increase the cycle count
let bump = (cpu, pc, cycles) => {
  {...cpu, pc:pc, cycle:(cpu.cycle+cycles)}
}

let nop = (cpu) => bump(cpu, cpu.pc, 4)

let call = (cpu) => {
  let address = load_next16(cpu)
  let sp = wrapping_add16(cpu.sp, -2)
  store16(cpu, sp, cpu.pc + 2)
  bump({...cpu, sp}, address, 24)
}

let call_cond = (cpu, flag, condition) => {
  if (has_flag(cpu, flag) == condition) {
    call(cpu)
  } else {
    bump(cpu, cpu.pc + 2, 12)
  }
}

let ret = (cpu) => {
  let pc = load16(cpu, cpu.sp)
  let sp = wrapping_add16(cpu.sp, 2)
  bump({...cpu, sp}, pc, 16)
}

let ret_cond = (cpu, flag, condition) => {
  if (has_flag(cpu, flag) == condition) {
     let pc = load16(cpu, cpu.sp)
     let sp = wrapping_add16(cpu.sp, 2)
    bump({...cpu, sp}, pc, 20)
  } else {
    bump(cpu, cpu.pc + 2, 8)
  }
}

let add = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let result = (a + b) land 0xFF
  set_register(cpu, A, result)
  let h = (result land 0xF) < (b land 0xF)
  let c = result < b
  set_flags(cpu, ~c, ~h, ~n=false, ~z=(result == 0), ())
  bump(cpu, cpu.pc, 8)
}

let and_ = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let result = a land b
  set_register(cpu, r, result)
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=true, ~c=false, ())
  bump(cpu, cpu.pc, 4)
}

let and_hl = (cpu) => {
  let a = get_register(cpu, A)
  let b = load(cpu, get_register16(cpu, HL))
  let result = a land b
  set_register(cpu, A, result)
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=true, ~c=false, ())
  bump(cpu, cpu.pc, 8)
}

let cp_n = (cpu) => {
  let reg = get_register(cpu, A);
  let byte = load_next(cpu);
  let h = (byte land 0xF) > (reg land 0xF);
  set_flags(cpu, ~z=(reg == byte), ~n=true, ~h, ~c=(reg < byte), ());
  bump(cpu, cpu.pc + 1, 8)
}

let cpl = (cpu) => {
  get_register(cpu, A) lxor 0xFF |> set_register(cpu, A);
  set_flags(cpu, ~n=true, ~h=true, ())
  bump(cpu, cpu.pc, 4)
}

let di = (cpu) =>
  bump({...cpu, ime: false}, cpu.pc, 4)

let ei = (cpu) =>
  bump({...cpu, ime: true}, cpu.pc, 4)

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

let ld_a_r16 = (cpu, r16) => {
  let address = get_register16(cpu, r16);
  load(cpu, address) |> set_register(cpu, A)
  bump(cpu, cpu.pc, 8)
}

let ld_a_a16 = (cpu) => {
  let byte = load_next16(cpu) |> load(cpu)
  set_register(cpu, A, byte)
  bump(cpu, cpu.pc + 2, 16)
}

let ldi_hl_a = (cpu) => {
  let address = get_register16(cpu, HL);
  get_register(cpu, A) |> store(cpu, address);
  set_register16(cpu, HL, wrapping_add16(address, 1));
  bump(cpu, cpu.pc, 8)
}

let ldi_a_hl = (cpu) => {
  let address = get_register16(cpu, HL);
  load(cpu, address) |> set_register(cpu, A);
  set_register16(cpu, HL, wrapping_add16(address, 1));
  bump(cpu, cpu.pc, 8)
}

let ldd_hl_a = (cpu) => {
  let address = get_register16(cpu, HL);
  get_register(cpu, A) |> store(cpu, address);
  set_register16(cpu, HL, wrapping_add16(address, -1));
  bump(cpu, cpu.pc, 8)
}


let ld_hl_r = (cpu, r) => {
  let address = get_register16(cpu, HL)
  get_register(cpu, r) |> store(cpu, address)
  bump(cpu, cpu.pc, 8)
}

let ld_r_hl = (cpu, r) => {
  let address = get_register16(cpu, HL);
  load(cpu, address) |> set_register(cpu, r);
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

let ld_read_io_c = (cpu) => {
  let n = get_register(cpu, C)
  let byte = load(cpu, wrapping_add16(0xFF00, n));
  set_register(cpu, A, byte);
  bump(cpu, cpu.pc + 1, 12)
}

let ld_write_io_c = (cpu) => {
  let n = get_register(cpu, C)
  let address = wrapping_add16(0xFF00, n);
  store(cpu, address, get_register(cpu, A));
  bump(cpu, cpu.pc + 1, 12)
}

let ld_sp = (cpu) => {
  let word = load_next16(cpu)
  bump({...cpu, sp:word}, cpu.pc + 2, 12)
}

let ld_a16_a = (cpu) => {
  let address = load_next16(cpu)
  store(cpu, address, get_register(cpu, A))
  bump(cpu, cpu.pc + 2, 16)
}

let inc = (cpu, r) => {
  let reg = get_register(cpu, r);
  let value = wrapping_add(reg, 1);
  set_register(cpu, r, value);
  let h = (value land 0xF) < (reg land 0xF);
  set_flags(cpu, ~z=(value == 0), ~n=false, ~h, ());
  bump(cpu, cpu.pc, 4)
}

let inc16 = (cpu, r) => {
  let value = get_register16(cpu, r)
  set_register16(cpu, r, wrapping_add16(value, 1))
  bump(cpu, cpu.pc, 8)
}

let dec = (cpu, r) => {
  let reg = get_register(cpu, r);
  let value = wrapping_add(reg, -1);
  set_register(cpu, r, value);
  let h = (value land 0xF) > (reg land 0xF);
  set_flags(cpu, ~z=(value == 0), ~n=true, ~h, ());
  bump(cpu, cpu.pc, 4)
}

let dec16 = (cpu, r) => {
  let reg = get_register16(cpu, r);
  let value = wrapping_add(reg, -1);
  set_register16(cpu, r, value);
  bump(cpu, cpu.pc, 8)
}

let dec_hl = (cpu) => {
  let address = get_register16(cpu, HL)
  let a = load(cpu, address)
  let value = wrapping_add(a, -1);
  store(cpu, address, value);
  let h = (value land 0xF) > (a land 0xF);
  set_flags(cpu, ~z=(value == 0), ~n=true, ~h, ());
  bump(cpu, cpu.pc, 12)
}

let adc_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a + b + carry
  set_register(cpu, A, result land 0xFF)
  let c = result > 0xFF
  let h = (a land 0xF) + (b land 0xF) + carry > 0xF
  set_flags(cpu, ~z=(result == 0), ~n=false, ~c, ~h, ())
  bump(cpu, cpu.pc + 1, 8)
}

let add_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let value = wrapping_add(a, b)
  set_register(cpu, A, value)
  let h = (value land 0xF) < (b land 0xF)
  let c = value < b
  set_flags(cpu, ~h, ~c, ~n=false, ~z=(value == 0), ());
  bump(cpu, cpu.pc + 1, 8)
}

let sub_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let value = wrapping_add(a, -b)
  set_register(cpu, A, value)
  let h = (value land 0xF) > (a land 0xF)
  let c = value > a
  set_flags(cpu, ~h, ~c, ~n=true, ~z=(a == b), ());
  bump(cpu, cpu.pc + 1, 8)
}

let jp = (cpu) => {
  let address = load_next16(cpu)
  bump(cpu, address, 16)
}

let jr_e8 = (cpu) => {
  let byte = load_next(cpu);
  // Treat the byte as a signed integer
  let offset = (byte < 0x80) ? byte : (byte - 0x100);
  bump(cpu, cpu.pc + offset + 1, 12)
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

let and_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let value = a land b
  set_register(cpu, A, value)
  set_flags(cpu, ~c=false, ~h=true, ~n=false, ~z=(value == 0), ())
  bump(cpu, cpu.pc + 1, 4)
}

let ora = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let value = a lor b
  set_register(cpu, A, value)
  set_flags(cpu, ~z=(value == 0), ~n=false, ~c=false, ~h=false, ())
  bump(cpu, cpu.pc, 4)
}

let or_hl = (cpu) => {
  let a = get_register(cpu, A)
  let b = get_register16(cpu, HL) |> load(cpu)
  let value = a lor b
  set_register(cpu, A, value)
  set_flags(cpu, ~z=(value == 0), ~n=false, ~c=false, ~h=false, ())
  bump(cpu, cpu.pc, 8)
}

let push16 = (cpu, r16) => {
  let sp = wrapping_add16(cpu.sp, -2)
  store16(cpu, sp, get_register16(cpu, r16))
  bump({...cpu, sp}, cpu.pc, 16)
}

let pop16 = (cpu, r16) => {
  let value = load16(cpu, cpu.sp)
  let sp = wrapping_add16(cpu.sp, 2)
  set_register16(cpu, r16, value)
  bump({...cpu, sp}, cpu.pc, 12)
}

let rr = (cpu, storage) => {
  let a = storage_load(cpu, storage)
  let carry = has_flag(cpu, C) ? 1 : 0
  let c = (a land 1) == 1
  let value = (carry lsl 7) lor (a lsr 1)
  storage_store(cpu, storage, value)
  set_flags(cpu, ~n=false, ~h=false, ~z=(value == 0), ~c , ())
  bump(cpu, cpu.pc, 8)
}

let rra = (cpu) => {
  let a = get_register(cpu, A)
  let carry = has_flag(cpu, C) ? 1 : 0
  let c = (a land 1) == 1
  let value = (carry lsl 7) lor (a lsr 1)
  set_register(cpu, A, value)
  set_flags(cpu, ~n=false, ~h=false, ~z=false, ~c , ())
  bump(cpu, cpu.pc, 8)
}

let rst = (cpu, n) => {
  let sp = wrapping_add(cpu.sp, -2)
  store16(cpu, sp, cpu.pc)
  bump({...cpu, sp}, n, 16)
}

let srl = (cpu, r) => {
  let a = get_register(cpu, r)
  let result = a lsr 1
  set_register(cpu, r, result)
  set_flags(cpu, ~h=false, ~n=false, ~z=(result == 0), ~c=(a land 1 == 1), ())
  bump(cpu, cpu.pc, 8)
}

let srl_hl = (cpu) => {
  let a = get_register16(cpu, HL) |> load(cpu)
  let result = a lsr 1
  store(cpu, a, result)
  set_flags(cpu, ~h=false, ~n=false, ~z=(result == 0), ~c=(a land 1 == 1), ())
  bump(cpu, cpu.pc, 16)
}

let swap = (cpu, r) => {
  let a = get_register(cpu, r)
  let lo = (a land 0xF) lsl 4
  let hi = (a land 0xF0) lsr 4
  let result = lo lor hi
  set_register(cpu, r, result)
  set_flags(cpu, ~z=(result == 0), ~c=false, ~n=false, ~h=false, ())
  bump(cpu, cpu.pc, 8)
}

let swap_hl = (cpu) => {
  let address = get_register16(cpu, HL)
  let a = load(cpu, address)
  let lo = (a land 0xF) lsl 4
  let hi = (a land 0xF0) lsr 4
  let result = lo lor hi
  store(cpu, address, result)
  set_flags(cpu, ~z=(result == 0), ~c=false, ~n=false, ~h=false, ())
  bump(cpu, cpu.pc, 16)
}

let xor = (cpu, r) => {
  let a = get_register(cpu, A);
  let b = get_register(cpu, r);
  let result = a lxor b;
  set_register(cpu, A, result);
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=false, ~c=false, ())
  bump(cpu, cpu.pc, 4)
}

let xor_d8 = (cpu) => {
  let a = get_register(cpu, A);
  let b = load_next(cpu)
  let result = a lxor b;
  set_register(cpu, A, result);
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=false, ~c=false, ())
  bump(cpu, cpu.pc + 1, 8)
}

let xor_hl = (cpu) => {
  let a = get_register(cpu, A);
  let b = get_register16(cpu, HL) |> load(cpu)
  let result = a lxor b;
  set_register(cpu, A, result);
  set_flags(cpu, ~z=(result == 0), ~n=false, ~h=false, ~c=false, ())
  bump(cpu, cpu.pc, 8)
}

let execute = (cpu, instruction) => switch(instruction) {
  | Nop => cpu
  | Add(r) => add(cpu, r)
  | Add_d8 => add_d8(cpu)
  | Adc_d8 => adc_d8(cpu)
  | And(r) => and_(cpu, r)
  | And_hl => and_hl(cpu)
  | And_d8 => and_d8(cpu)
  | Call => call(cpu)
  | CallCond(flag ,cond) => call_cond(cpu, flag, cond)
  | Cp_n => cp_n(cpu)
  | Cpl => cpl(cpu)
  | Dec(r) => dec(cpu, r)
  | Dec16(r) => dec16(cpu, r)
  | Dec_hl => dec_hl(cpu)
  | Di => di(cpu)
  | Ei => ei(cpu)
  | Inc(r) => inc(cpu, r)
  | Inc16(r) => inc16(cpu, r)
  | Ld_sp => ld_sp(cpu)
  | Ld_read_io_n => ld_read_io_n(cpu)
  | Ld_write_io_n => ld_write_io_n(cpu)
  | Ld_read_io_c => ld_read_io_c(cpu)
  | Ld_write_io_c => ld_write_io_c(cpu)
  | Ld_a_r16(r) => ld_a_r16(cpu, r)
  | Ld_r16_a(r) => ld_r16_a(cpu, r)
  | Ld_a_a16 => ld_a_a16(cpu)
  | Ld_rr(r1, r2) => ld_rr(cpu, r1, r2)
  | Ld_n(r) => ld_n(cpu, r)
  | Ld_hl_d8 => ld_hl_d8(cpu)
  | Ld_r_hl(r) => ld_r_hl(cpu, r)
  | Ld_hl_r(r) => ld_hl_r(cpu, r)
  | Ld_nn(r) => ld_nn(cpu, r)
  | Ldd_hl_a => ldd_hl_a(cpu)
  | Ldi_a_hl => ldi_a_hl(cpu)
  | Ldi_hl_a => ldi_hl_a(cpu)
  | Ld_a16_a => ld_a16_a(cpu)
  | Jp => jp(cpu)
  | Jr_e8 => jr_e8(cpu)
  | Jr(flag, cond) => jr(cpu, flag, cond)
  | Or(r) => ora(cpu, r)
  | Or_hl => or_hl(cpu)
  | Pop16(r) => pop16(cpu, r)
  | Push16(r) => push16(cpu, r)
  | Ret => ret(cpu)
  | RetCond(flag, cond) => ret_cond(cpu, flag, cond)
  | Rr(s) => rr(cpu, s)
  | Rra => rra(cpu)
  | Rst(n) => rst(cpu, n)
  | Srl(r) => srl(cpu, r)
  | Srl_hl => srl_hl(cpu)
  | Swap(r) => swap(cpu, r)
  | Swap_hl => swap_hl(cpu)
  | Sub_d8 => sub_d8(cpu)
  | Xor(r) => xor(cpu, r)
  | Xor_d8 => xor_d8(cpu)
  | Xor_hl => xor_hl(cpu)
}
