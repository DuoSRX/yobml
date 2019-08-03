open Printf

type t = {
  rom: array(int),
  wram: array(int),
  // vram: array(int),
  exram: array(int),
  oam: array(int),
  io: array(int),
  hram: array(int),
  gpu: Gpu.t
}

let make = (~rom, ~gpu) => {
  {
    rom,
    gpu,
    wram: Array.make(0x2000, 0),
    // vram: Array.make(0x2000, 0),
    // External ram FIXME: this depends on the mapper
    exram: Array.make(0x2000, 0),
    oam: Array.make(0xA0, 0),
    io: Array.make(0x80, 0),
    hram: Array.make(0x80, 0)
  }
}

exception MemoryAccessUnimplement(string)

let load = (mem, address) => {
  // sprintf("Read @ %04X", address) |> Js.log
  if (address == 0xFF44) {
    mem.gpu.ly
  } else if (address < 0x8000) {
    mem.rom[address]
  } else if (address < 0xA000) {
    mem.gpu.vram[address land 0x1FFF]
  } else if (address >= 0xA000 && address < 0xC000) { // FIXME: mapper
    mem.exram[address land 0x1FFF]
  } else if (address >= 0xC000 && address < 0xE000) {
    mem.wram[address land 0x1FFF]
  } else if (address >= 0xE000 && address < 0xFE00) { // Mirror of C000-DDFF
    mem.wram[(address - 0x2000) land 0x1FFF]
  } else if (address >= 0xFE00 && address <= 0xFE9F) {
    mem.oam[address land 0x9F]
  } else if (address >= 0xFEA0 && address <= 0xFEFF) {
    0 // Some games try to read/write here. No-op
  } else if (address >= 0xFF00 && address < 0xFF80) {
    mem.io[address land 0x7F]
  } else if (address >= 0xFF80) {
    mem.hram[address land 0x7F]
  } else {
    raise(MemoryAccessUnimplement(sprintf("%04X", address)))
  }
}

let store = (mem, address, value) => {
  // sprintf("Write @ %04X", address) |> Js.log
          // Js.log(Memory.load(cpu.memory, 0x89B0))
        // Js.log(Memory.load(cpu.memory, 0x9820))

  if (address < 0x8000) {
    mem.rom[address] = value
  } else if (address < 0xA000) {
    mem.gpu.vram[address land 0x1FFF] = value
  } else if (address >= 0xA000 && address < 0xC000) { // FIXME: mapper
    mem.exram[address land 0x1FFF] = value
  } else if (address >= 0xC000 && address < 0xE000) {
    mem.wram[address land 0x1FFF] = value
  } else if (address >= 0xE000 && address < 0xFE00) { // Mirror of C000-DDFF
    mem.wram[(address - 0x2000) land 0x1FFF] = value
  } else if (address >= 0xFE00 && address <= 0xFE9F) {
    mem.oam[address land 0x9F] = value
  } else if (address >= 0xFEA0 && address <= 0xFEFF) {
    () // Some games try to read/write here. No-op
  } else if (address >= 0xFF00 && address < 0xFF80) {
    mem.io[address land 0x7F] = value
  } else if (address >= 0xFF80) {
    mem.hram[address land 0x7F] = value
  } else {
    raise(MemoryAccessUnimplement(sprintf("%04X", address)))
  }
}

let load16 = (mem, address) => {
  let lo = load(mem, address)
  let hi = load(mem, address + 1)
  lo lor (hi lsl 8)
}

let store16 = (mem, address, value) => {
  let lo = value land 0xFF
  let hi = (value land 0xFF00) lsr 8
  store(mem, address, lo)
  store(mem, address + 1, hi)
}
