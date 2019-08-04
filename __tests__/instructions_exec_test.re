open Cpu
open Jest
open Expect;

let reset (pc) = {
  let rom = Array.make(0x2000, 0);
  let gpu = Gpu.make(~rom);
  let memory = Memory.make(~rom, ~gpu);
  {...Cpu.make(~memory), pc}
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

// describe("BIT", () => {
//   test("Change PC to the next 2 bytes", () => {
//     let cpu = reset(0);
//     Cpu.set_register(cpu, A, 0x1101_1101)
//     let bit = (cpu, bit, r) => {
//   let a = get_register(cpu, r);
//   let z = (a lsr bit) land 1 == 1;
//   set_flags(cpu, ~z, ~h=true, ~n=false, ())
//   bump(cpu, cpu.pc, 8)
// }
    // let cpu = cpu->InstructionsExec.execute(_, Bit(A, 2));
    // expect(get_register(cpu, F)) |> toEqual(_0000)
  // })
// });