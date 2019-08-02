[@bs.deriving jsConverter]
type t = {
  pc : int, // program counter
  cycle: int,

  ime : bool, // interrupt enable

  registers: Registers.t,
  memory: Memory.t,

  mutable serial: list(string) // for debugging purpose only
}

type cpu_flag = Z | N | H | C

let make = (rom) => {
  let memory = Memory.make(~rom)
  let registers = Registers.make();
  { pc: 0x100, cycle: 0, ime: false, memory, registers, serial: [] };
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

let flag_to_string = (flag) => {
  switch(flag) {
    | Z => "Z"
    | N => "N"
    | H => "H"
    | C => "C"
  };
}

let bump_pc = (cpu, n) => {...cpu, pc: cpu.pc + n}
