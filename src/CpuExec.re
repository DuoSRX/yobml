open Cpu
open Printf

let pretty_flags = (cpu) => {
  Array.map((flag) => has_flag(cpu, flag) ? flag_to_string(flag) : "-" , [|C, H, N, Z|])
  |> Js.Array.joinWith("")
}

let trace = (cpu, instruction) => {
  Js.log(sprintf("AF:%04X BC:%04X DE:%04X HL:%04X SP:%04X [%s] %04X: %02X %02X %02X  %s",
    get_register16(cpu, AF),
    get_register16(cpu, BC),
    get_register16(cpu, DE),
    get_register16(cpu, HL),
    get_register16(cpu, SP),
    pretty_flags(cpu),
    cpu.pc,
    Memory.load(cpu.memory, cpu.pc),
    Memory.load(cpu.memory, cpu.pc + 1),
    Memory.load(cpu.memory, cpu.pc + 2),
    Instructions.pretty(instruction))
  );
}

// let step = (~cpu, ~breakpoints) => {
let step = (~cpu) => {
  let (cpu, instruction) = switch(Memory.load(cpu.memory, cpu.pc)) {
  | 0xCB => {
      let byte = Memory.load(cpu.memory, cpu.pc + 1);
      let instruction = Instructions.decode_cb(byte);
      // trace(cpu, instruction);
      ({...cpu, pc: cpu.pc + 2}, instruction)
    }
  | opcode => {
      let instruction = Instructions.decode(opcode);
      // trace(cpu, instruction);
      ({...cpu, pc: cpu.pc + 1}, instruction)
    }
  };

  // if (List.mem(cpu.pc, breakpoints)) {
  //   ()
  // };

  // let cpu = {...cpu, pc: cpu.pc + 1};
  (InstructionsExec.execute(cpu, instruction), instruction)
}
