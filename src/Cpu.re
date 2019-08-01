open Belt

[@bs.deriving jsConverter]
type t = {
  sp : int, // stack pointer
  pc : int, // program counter

  ime : bool, // interrupt enable

  registers: Registers.t,
  memory: Memory.t
}

type cpu_flag = Z | N | H | C

let make = (rom) => {
  // let wram = Array.append([|0x36, 0x3E, 0x44, 0x0, 0x12|], Array.make(0x1000, 0))
  let memory = Memory.make(~rom)
  let registers = Registers.make();
  { sp: 0xFFFE, pc: 0x100, ime: false, memory, registers };
};

let get_register = (cpu, register) =>
  Registers.get(cpu.registers, register)

let get_register16 = (cpu, register) =>
  Registers.get16(cpu.registers, register)

let set_register = (cpu, register, value) =>
  Registers.set(cpu.registers, register, value);

let set_register16 = (cpu, register, value) =>
  Registers.set16(cpu.registers, register, value);

let has_flag = (cpu, flag) => {
  switch(flag) {
    | Z => cpu.registers.f land 0x80 > 0
    | N => cpu.registers.f land 0x40 > 0
    | H => cpu.registers.f land 0x20 > 0
    | C => cpu.registers.f land 0x10 > 0
  }
}

let set_flag = (cpu, flag) => {
  switch(flag) {
    | Z => cpu.registers.f = cpu.registers.f lor 0x80
    | N => cpu.registers.f = cpu.registers.f lor 0x40
    | H => cpu.registers.f = cpu.registers.f lor 0x20
    | C => cpu.registers.f = cpu.registers.f lor 0x10
  };
}

let unset_flag = (cpu, flag) => {
  switch(flag) {
    | Z => cpu.registers.f = cpu.registers.f land (lnot(0x80) land 0xFF)
    | N => cpu.registers.f = cpu.registers.f land (lnot(0x40) land 0xFF)
    | H => cpu.registers.f = cpu.registers.f land (lnot(0x20) land 0xFF)
    | C => cpu.registers.f = cpu.registers.f land (lnot(0x10) land 0xFF)
  };
}

let set_flags = (cpu, ~z=?, ~n=?, ~h=?, ~c=?, ()) => {
  let switch_flag = (value, flag) => {
    switch(value) {
      | Some(true) => set_flag(cpu, flag)
      | Some(false) => unset_flag(cpu, flag)
      | None => ()
    }
  };
  switch_flag(z, Z);
  switch_flag(n, N);
  switch_flag(h, H);
  switch_flag(c, C);
}

let bump_pc = (cpu, n) => {...cpu, pc: cpu.pc + n}
