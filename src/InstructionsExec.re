open Cpu
open Instructions

exception FooEx

let wrapping_add = (a, b) => (a + b) land 0xFF
let wrapping_add16 = (a, b) => (a + b) land 0xFFFF
let signed = (v) => v > 0x7F ? -((lnot(v) + 1) land 0xFF) : v

let load = (cpu, address) => Memory.load(cpu.memory, address)
let load16 = (cpu, address) => Memory.load16(cpu.memory, address)
let store = (cpu, address, value) => {
  if (address == 0xFF01) {
    cpu.serial = [String.make(1, Char.chr(value)), ...cpu.serial];
  }
  // if (address == 0xFFB6) {
  //   Js.log(Instructions.decode(Memory.load(cpu.memory, cpu.pc - 1)) |> Instructions.pretty)
  //   Js.log(Printf.sprintf("%04X: %04X = %02X [B:%02X, C:%02X, HL:%04X]", cpu.pc, address, value, cpu.registers.b, cpu.registers.c, get_register16(cpu, HL)));
  //  };
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
let halt = (cpu) => bump({...cpu, halted: true}, cpu.pc, 4)

let call = (cpu) => {
  let address = load_next16(cpu)
  let sp = wrapping_add16(get_register16(cpu, SP), -2)
  store16(cpu, sp, cpu.pc + 2)
  set_register16(cpu, SP, sp)
  bump(cpu, address, 24)
}

let call_cond = (cpu, flag, condition) => {
  if (has_flag(cpu, flag) == condition) {
    call(cpu)
  } else {
    bump(cpu, cpu.pc + 2, 12)
  }
}

let ret = (cpu) => {
  let sp = get_register16(cpu, SP)
  let pc = load16(cpu, sp)
  wrapping_add16(sp, 2) |> set_register16(cpu, SP)
  bump(cpu, pc, 16)
}

let reti = (cpu) => {
  let sp = get_register16(cpu, SP)
  let pc = load16(cpu, sp)
  wrapping_add16(sp, 2) |> set_register16(cpu, SP)
  bump({...cpu, ime: true}, pc, 16)
}

let ret_cond = (cpu, flag, condition) => {
  if (has_flag(cpu, flag) == condition) {
    let sp = get_register16(cpu, SP)
    let pc = load16(cpu, sp)
    wrapping_add16(sp, 2) |> set_register16(cpu, SP)
    bump(cpu, pc, 20)
  } else {
    bump(cpu, cpu.pc, 8)
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

let bit = (cpu, bit, r) => {
  let a = get_register(cpu, r);
  let z = (a lsr bit) land 1 != 1;
  set_flags(cpu, ~z, ~h=true, ~n=false, ())
  bump(cpu, cpu.pc, 8)
}

let ccf = (cpu) => {
  set_flags(cpu, ~h=false, ~n=false, ~c=(!has_flag(cpu, C)), ())
  bump(cpu, cpu.pc, 4)
}

let cp = (cpu, r) => {
  let a = get_register(cpu, A);
  let b = get_register(cpu, r)
  let h = (a land 0xF) > (b land 0xF);
  set_flags(cpu, ~z=(a == b), ~n=true, ~h, ~c=(a < b), ());
  bump(cpu, cpu.pc + 1, 4)
}

let cp_hl = (cpu) => {
  let reg = get_register(cpu, A);
  let byte = get_register16(cpu, HL) |> load(cpu)
  let h = (byte land 0xF) > (reg land 0xF);
  set_flags(cpu, ~z=(reg == byte), ~n=true, ~h, ~c=(reg < byte), ());
  bump(cpu, cpu.pc + 1, 8)
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

exception NoDAA

let daa = (cpu) => {
  // raise(NoDAA)
  // let a = get_register(cpu, A)
  // let adj1 = has_flag(cpu, H) ? 0x06 : 0
  // let adj2 = adj1 lor (has_flag(cpu, C) ? 0x60 : 0)
  // let adj3 = if (has_flag(cpu, N)) {
  //   wrapping_add(a, -adj2)
  // } else {
  // }

  bump(cpu, cpu.pc, 4)
}

let di = (cpu) =>
  bump({...cpu, ime: false}, cpu.pc, 4)

let ei = (cpu) => {
  // load(cpu, 0xFFFF) |> store(cpu, 0xFF0F)
  bump({...cpu, ime: true}, cpu.pc, 4)
}

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

let ld_hl_sp_e8 = (cpu) => {
  let a = get_register16(cpu, SP)
  let b = load_next(cpu) |> signed
  let value = wrapping_add16(a, b)
  let h = ((a land 0xF) + (b land 0xF)) > 0xF
  let c = ((a land 0xFF) + (b land 0xFF)) > 0xFF
  set_flags(cpu, ~h, ~c, ~n=false, ~z=false, ())
  set_register16(cpu, HL, value)
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

let ldd_a_hl = (cpu) => {
  let address = get_register16(cpu, HL);
  load(cpu, address) |> set_register(cpu, A);
  set_register16(cpu, HL, wrapping_add16(address, -1));
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

let ld_sp_hl = (cpu) => {
  get_register16(cpu, HL) |> set_register16(cpu, SP)
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
  bump(cpu, cpu.pc, 12)
}

let ld_write_io_c = (cpu) => {
  let n = get_register(cpu, C)
  let address = wrapping_add16(0xFF00, n);
  store(cpu, address, get_register(cpu, A));
  bump(cpu, cpu.pc, 12)
}

let ld_sp = (cpu) => {
  load_next16(cpu) |> set_register16(cpu, SP)
  bump(cpu, cpu.pc + 2, 12)
}

let ld_a16_a = (cpu) => {
  let address = load_next16(cpu)
  store(cpu, address, get_register(cpu, A))
  bump(cpu, cpu.pc + 2, 16)
}

let ld_a16_sp = (cpu) => {
  let address = load_next16(cpu)
  get_register16(cpu, SP) |> store16(cpu, address)
  bump(cpu, cpu.pc + 2, 20)
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

let inc_hl = (cpu) => {
  let hl = get_register16(cpu, HL)
  let value = wrapping_add(load(cpu, hl), 1)
  set_flags(cpu, ~z=(value==0), ~h=(value land 0xF == 0), ~n=false, ());
  store(cpu, hl, value)
  bump(cpu, cpu.pc, 12)
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
  let value = wrapping_add16(reg, -1);
  set_register16(cpu, r, value);
  bump(cpu, cpu.pc, 8)
}

let dec_hl = (cpu) => {
  let address = get_register16(cpu, HL)
  let a = load(cpu, address)
  let value = wrapping_add16(a, -1);
  store(cpu, address, value);
  // let h = (value land 0xF) > (a land 0xF);
  // set_flags(cpu, ~z=(value == 0), ~n=true, ~h, ());
  bump(cpu, cpu.pc, 8)
}

let adc = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a + b + carry
  set_register(cpu, A, result land 0xFF)
  let c = result > 0xFF
  let h = (a land 0xF) + (b land 0xF) + carry > 0xF
  set_flags(cpu, ~z=(result land 0xFF == 0), ~n=false, ~c, ~h, ())
  bump(cpu, cpu.pc + 1, 8)
}

let adc_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a + b + carry
  set_register(cpu, A, result land 0xFF)
  let c = result > 0xFF
  let h = (a land 0xF) + (b land 0xF) + carry > 0xF
  set_flags(cpu, ~z=(result land 0xFF == 0), ~n=false, ~c, ~h, ())
  bump(cpu, cpu.pc + 1, 8)
}

let adc_hl = (cpu) => {
  let a = get_register(cpu, A)
  let b = get_register16(cpu, HL) |> load(cpu)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a + b + carry
  set_register(cpu, A, result land 0xFF)
  let c = result > 0xFF
  let h = (a land 0xF) + (b land 0xF) + carry > 0xF
  set_flags(cpu, ~z=(result land 0xFF == 0), ~n=false, ~c, ~h, ())
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

let add_hl = (cpu) => {
  let a = get_register(cpu, A)
  let b = get_register16(cpu, HL) |> load(cpu)
  let result = a + b
  set_register(cpu, A, result land 0xFF)
  let c = a > result
  let h = (a land 0xF) > (result land 0xF)
  set_flags(cpu, ~h, ~c, ~n=false, ~z=(result land 0xFF == 0), ())
  bump(cpu, cpu.pc, 8)
}

let add_hl_r16 = (cpu, r) => {
  let hl = get_register16(cpu, HL)
  let a = get_register16(cpu, r)
  let result = hl + a
  let h = (hl land 0xFFF) + (a land 0xFFF) > 0xFFF
  set_register16(cpu, HL, result land 0xFFFF)
  set_flags(cpu, ~h, ~c=(result > 0xFFFF), ~n=false, ())
  bump(cpu, cpu.pc, 8)
}

let add_sp_e8 = (cpu) => {
  let a = get_register16(cpu, SP)
  let b = load_next(cpu) |> signed
  let h = ((a land 0xF) + (b land 0xF)) > 0xF
  let c = ((a land 0xFF) + (b land 0xFF)) > 0xFF
  set_flags(cpu, ~h, ~c, ~n=false, ~z=false, ())
  let sp = wrapping_add16(a, b)
  set_register16(cpu, SP, sp)
  bump(cpu, cpu.pc + 1, 16)
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

let jp_cond = (cpu, flag, condition) => {
  if (has_flag(cpu, flag) == condition) {
    let address = load_next16(cpu)
    bump(cpu, address, 16)
  } else {
    bump(cpu, cpu.pc + 2, 12)
  }
}

let jp_hl = (cpu) => {
  let address = get_register16(cpu, HL)
  bump(cpu, address, 4)
}

let jr_e8 = (cpu) => {
  let byte = load_next(cpu);
  // Treat the byte as a signed integer
  let offset = if (byte > 0x7F) {
    -((lnot(byte) + 1) land 0xFF)
  } else {
    byte
  }
  bump(cpu, cpu.pc + offset + 1, 12)
}

let jr = (cpu, flag, condition) => {
  let do_jump = has_flag(cpu, flag) == condition

  if (do_jump) {
    let byte = load_next(cpu);
    // Treat the byte as a signed integer
    let offset = if (byte > 0x7F) {
      -((lnot(byte) + 1) land 0xFF)
    } else {
      byte
    }

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

let or_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let value = a lor b
  set_register(cpu, A, value)
  set_flags(cpu, ~z=(value == 0), ~n=false, ~c=false, ~h=false, ())
  bump(cpu, cpu.pc + 1, 8)
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
  let sp = get_register16(cpu, SP)->wrapping_add16(_, -2)
  store16(cpu, sp, get_register16(cpu, r16))
  set_register16(cpu, SP, sp)
  bump(cpu, cpu.pc, 16)
}

let pop16 = (cpu, r16) => {
  let value = load16(cpu, get_register16(cpu, SP))
  let sp = get_register16(cpu, SP)->wrapping_add16(_, 2)
  set_register16(cpu, r16, value)
  set_register16(cpu, SP, sp)
  bump(cpu, cpu.pc, 12)
}

let pop_af = (cpu) => {
  let af = load16(cpu, get_register16(cpu, SP)) land 0xFFF0
  let sp = get_register16(cpu, SP)->wrapping_add16(_, 2)
  set_register16(cpu, AF, af)
  set_register16(cpu, SP, sp)
  set_flags(cpu,
    ~z=(af land 0x80 > 0), ~n=(af land 0x40 > 0),
    ~h=(af land 0x20 > 0), ~c=(af land 0x10 > 0), ())
  bump(cpu, cpu.pc, 12)
}

let res = (cpu, bit, r) => {
  let reg = get_register(cpu, r)
  set_register(cpu, r, reg land (lnot (1 lsl bit)))
  bump(cpu, cpu.pc, 8)
}

let res_hl = (cpu, bit) => {
  let address = get_register16(cpu, HL)
  let byte = load(cpu, address)
  store(cpu, address, byte land (lnot (1 lsl bit)))
  bump(cpu, cpu.pc, 16)
}

let rla = (cpu) => {
  let a = get_register(cpu, A)
  let prev_carry = has_flag(cpu, C) ? 1 : 0
  let c = (a lsr 7) land 1
  let a = (a lsl 1) lor prev_carry
  let c = (c == 0)
  set_flags(cpu, ~c, ~n=false, ~h=false, ~z=false, ())
  set_register(cpu, A, a)
  bump(cpu, cpu.pc, 4)
}

let rlca = (cpu) => {
  let a = get_register(cpu, A)
  let c = a land 0x80 > 0
  let a = a lsl 1
  let a = c ? a lor 1 : a
  set_flags(cpu, ~c, ~n=false, ~h=false, ~z=false, ())
  set_register(cpu, A, a)
  bump(cpu, cpu.pc, 4)
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
  let sp = wrapping_add16(get_register16(cpu, SP), -2)
  store16(cpu, sp, cpu.pc)
  set_register16(cpu, SP, sp)
  bump(cpu, n, 16)
}

let sbc = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a - b - carry
  set_register(cpu, A, result land 0xFF)
  let c = result < 0
  let h = (a land 0xF) - (b land 0xF) - carry < 0
  let z = result land 0xFF == 0
  set_flags(cpu, ~c, ~h, ~z, ~n=true, ())
  bump(cpu, cpu.pc, 4)
}

let sbc_d8 = (cpu) => {
  let a = get_register(cpu, A)
  let b = load_next(cpu)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a - b - carry
  set_register(cpu, A, result land 0xFF)
  let c = result < 0
  let h = (a land 0xF) - (b land 0xF) - carry < 0
  let z = result land 0xFF == 0
  set_flags(cpu, ~c, ~h, ~z, ~n=true, ())
  bump(cpu, cpu.pc + 1, 4)
}

let sbc_hl = (cpu) => {
  let a = get_register(cpu, A)
  let b = get_register16(cpu, HL) |> load(cpu)
  let carry = has_flag(cpu, C) ? 1 : 0
  let result = a - b - carry
  set_register(cpu, A, result land 0xFF)
  let c = result < 0
  let h = (a land 0xF) - (b land 0xF) - carry < 0
  let z = result land 0xFF == 0
  set_flags(cpu, ~c, ~h, ~z, ~n=true, ())
  bump(cpu, cpu.pc, 8)
}

let scf = (cpu) => {
  set_flags(cpu, ~c=true, ~h=false, ~n=false, ())
  bump(cpu, cpu.pc, 4)
}

let set_hl = (cpu, n) => {
  let address = get_register16(cpu, HL)
  let value = load(cpu, address)
  store(cpu, address, (value lor (1 lsl n)))
  bump(cpu, cpu.pc, 16)
}

let sla = (cpu, r) => {
  let a = get_register(cpu, r)
  let result = a lsl 1
  set_flags(cpu, ~n=false, ~h=false, ~z=(result == 0), ~c=(result land 0x80 > 0), ())
  bump(cpu, cpu.pc, 8)
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

let sub_r8 = (cpu, r) => {
  let a = get_register(cpu, A)
  let b = get_register(cpu, r)
  let result = wrapping_add(a, -b)
  set_register(cpu, A, result)
  let h = (result land 0xF) > (a land 0xF)
  set_flags(cpu, ~h, ~z=(a == b), ~n=true, ~c=(result > a), ())
  bump(cpu, cpu.pc, 4)
}
// let open Uint8 in
  // let a = get_register cpu a in
  // let r = get_register cpu reg in
  // let res = sub a r in
  // set_register cpu Registers.a (res);
  // let hc = (res land (Char.chr 0x0F)) > (a land (Char.chr 0x0F)) in
  // write_flags cpu.rg ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  // t, 8


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
  | Adc(r) => adc(cpu, r)
  | Adc_d8 => adc_d8(cpu)
  | Adc_hl => adc_hl(cpu)
  | Add(r) => add(cpu, r)
  | Add_d8 => add_d8(cpu)
  | Add_hl => add_hl(cpu)
  | Add_hl_r16(r) => add_hl_r16(cpu, r)
  | Add_sp_e8 => add_sp_e8(cpu)
  | And(r) => and_(cpu, r)
  | And_hl => and_hl(cpu)
  | And_d8 => and_d8(cpu)
  | Bit(n,r) => bit(cpu, n, r)
  | Call => call(cpu)
  | CallCond(flag ,cond) => call_cond(cpu, flag, cond)
  | Ccf => ccf(cpu)
  | Cp(r) => cp(cpu, r)
  | Cp_hl => cp_hl(cpu)
  | Cp_n => cp_n(cpu)
  | Cpl => cpl(cpu)
  | Daa => daa(cpu)
  | Dec(r) => dec(cpu, r)
  | Dec16(r) => dec16(cpu, r)
  | Dec_hl => dec_hl(cpu)
  | Di => di(cpu)
  | Ei => ei(cpu)
  | Halt => halt(cpu)
  | Inc(r) => inc(cpu, r)
  | Inc16(r) => inc16(cpu, r)
  | Inc_hl => inc_hl(cpu)
  | Jp => jp(cpu)
  | JpCond(flag, cond) => jp_cond(cpu, flag, cond)
  | Jp_hl => jp_hl(cpu)
  | Jr_e8 => jr_e8(cpu)
  | Jr(flag, cond) => jr(cpu, flag, cond)
  | Ld_sp => ld_sp(cpu)
  | Ld_read_io_n => ld_read_io_n(cpu)
  | Ld_write_io_n => ld_write_io_n(cpu)
  | Ld_read_io_c => ld_read_io_c(cpu)
  | Ld_write_io_c => ld_write_io_c(cpu)
  | Ld_a_r16(r) => ld_a_r16(cpu, r)
  | Ld_a16_sp => ld_a16_sp(cpu)
  | Ld_r16_a(r) => ld_r16_a(cpu, r)
  | Ld_a_a16 => ld_a_a16(cpu)
  | Ld_rr(r1, r2) => ld_rr(cpu, r1, r2)
  | Ld_n(r) => ld_n(cpu, r)
  | Ld_hl_d8 => ld_hl_d8(cpu)
  | Ld_hl_sp_e8 => ld_hl_sp_e8(cpu)
  | Ld_r_hl(r) => ld_r_hl(cpu, r)
  | Ld_hl_r(r) => ld_hl_r(cpu, r)
  | Ld_nn(r) => ld_nn(cpu, r)
  | Ldd_hl_a => ldd_hl_a(cpu)
  | Ldd_a_hl => ldd_a_hl(cpu)
  | Ldi_a_hl => ldi_a_hl(cpu)
  | Ldi_hl_a => ldi_hl_a(cpu)
  | Ld_a16_a => ld_a16_a(cpu)
  | Ld_sp_hl => ld_sp_hl(cpu)
  | Or(r) => ora(cpu, r)
  | Or_d8 => or_d8(cpu)
  | Or_hl => or_hl(cpu)
  | Pop16(r) => pop16(cpu, r)
  | Pop_af => pop_af(cpu)
  | Push16(r) => push16(cpu, r)
  | Res(n, r) => res(cpu, n, r)
  | Res_hl(n) => res_hl(cpu, n)
  | Ret => ret(cpu)
  | Reti => reti(cpu)
  | RetCond(flag, cond) => ret_cond(cpu, flag, cond)
  | Rla => rla(cpu)
  | Rlca => rlca(cpu)
  | Rr(s) => rr(cpu, s)
  | Rra => rra(cpu)
  | Rst(n) => rst(cpu, n)
  | Scf => scf(cpu)
  | Set_hl(n) => set_hl(cpu, n)
  | Sla(r) => sla(cpu, r)
  | Srl(r) => srl(cpu, r)
  | Srl_hl => srl_hl(cpu)
  | Sub(r) => sub_r8(cpu, r)
  | Sbc(r) => sbc(cpu, r)
  | Sbc_d8 => sbc_d8(cpu)
  | Sbc_hl => sbc_hl(cpu)
  | Sub_d8 => sub_d8(cpu)
  | Swap(r) => swap(cpu, r)
  | Swap_hl => swap_hl(cpu)
  | Xor(r) => xor(cpu, r)
  | Xor_d8 => xor_d8(cpu)
  | Xor_hl => xor_hl(cpu)
}
