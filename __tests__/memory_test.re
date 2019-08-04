open Jest
open Expect;

let gpu = Gpu.make(~rom=[||])

describe("load16", () => {
  test("Load two bytes", () => {
    let mem = Memory.make(~rom=[|0x34, 0x12|], ~gpu);
    expect(Memory.load16(mem, 0)) |> toBe(0x1234)
  })
})

describe("store16", () => {
  test("Stores two bytes", () => {
    let mem = Memory.make(~rom=[|0, 0|], ~gpu)
    Memory.store16(mem, 0, 0x1234);
    expect((mem.rom[0], mem.rom[1])) |> toEqual((0x34, 0x12))
  })
})
