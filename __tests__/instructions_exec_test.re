open Jest
open Expect;

let reset (pc) = {
  let rom = Array.make(0x2000, 0);
  {...(Cpu.make(rom)), pc: pc}
}

describe("CALL", () => {
  test("Change PC to the next 2 bytes", () => {
    let cpu = reset(0);
    cpu.memory.rom[0] = 0x34
    cpu.memory.rom[1] = 0x12
    let cpu = cpu->InstructionsExec.execute(_, Call);
    expect(cpu.pc) |> toEqual(0x1234)
  })

  test("Decrement SP by 2", () => {
    let cpu = reset(0)->InstructionsExec.execute(_, Call);
    expect(Cpu.get_register16(cpu, SP)) |> toEqual(0xFFFC)
  })

  test("Stores PC+2 at SP", () => {
    let cpu = reset(0x1234)->InstructionsExec.execute(_, Call);
    expect(InstructionsExec.load16(cpu, 0xFFFC)) |> toEqual(0x1234 + 2)
  })
});
