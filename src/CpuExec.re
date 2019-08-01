open Cpu
open Printf

let trace = (cpu, instruction) => {
  Js.log(sprintf("AF:%04X BC:%04X DE:%04X HL:%04X %04X: %s",
    get_register16(cpu, AF),
    get_register16(cpu, BC),
    get_register16(cpu, DE),
    get_register16(cpu, HL),
    cpu.pc,
    Instructions.pretty(instruction))
  );
}

let step = (~cpu, ~breakpoints) => {
  let instruction = Memory.load(cpu.memory, cpu.pc) |> Instructions.decode;
  trace(cpu, instruction);

  // if (List.mem(cpu.pc, breakpoints)) {
  //   ()
  // };

  let cpu = {...cpu, pc: cpu.pc + 1};
  (InstructionsExec.execute(cpu, instruction), instruction)
}
