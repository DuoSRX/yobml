open Printf

type t = {
  rom: array(int),
  wram: array(int),
  exram: array(int),
  io: array(int),
  hram: array(int),
  gpu: Gpu.t,
  input: Input.t
}

let make = (~rom, ~gpu, ~input) => {
  {
    rom,
    gpu,
    input,
    wram: Array.make(0x2000, 0),
    // External ram FIXME: this depends on the mapper
    exram: Array.make(0x2000, 0),
    io: Array.make(0x80, 0),
    hram: Array.make(0x80, 0),
  }
}

exception InvalidMemoryAccess(string)

let load = (mem, address) => {
  // if (address >= 0xFF04 && address <= 0xFF07) { Js.log(Printf.sprintf("%04X", address)) };

  if (address == 0xFF44) {
    mem.gpu.ly
  } else if (address == 0xFF00) {
    Input.get(mem.input)
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
    mem.gpu.oam[address land 0x9F]
    // mem.oam[address land 0x9F]
  } else if (address >= 0xFEA0 && address <= 0xFEFF) {
    0 // Some games try to read/write here. No-op
  } else if (address >= 0xFF00 && address < 0xFF80) {
    mem.io[address - 0xFF00]
  } else if (address >= 0xFF80 && address <= 0xFFFF) {
    mem.hram[address - 0xFF80]
  } else {
    raise(InvalidMemoryAccess(sprintf("Invalid memory access at %04X", address)))
  }
}

let store = (mem, address, value) => {
  // if (address >= 0xFF04 && address <= 0xFF07) { Js.log(Printf.sprintf("%04X = %02X", address, value)) };
  // if (address == 0xFFFF) { Js.log(Printf.sprintf("%04X = %02X", address, value)) };

  if (address == 0xFF00) {
    Input.set(mem.input, value)
  } else if (address == 0xFF46) {
    // TODO: handle clock cycles for DMA (move into GPU or CPU?)
    let start = (value lsl 8) land 0xFFFF
    for (offset in 0 to 0x9F) {
      mem.gpu.oam[offset] = load(mem, start + offset)
    }
  } else if (address < 0x8000) {
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
    mem.gpu.oam[address land 0x9F] = value
    // mem.oam[address land 0x9F] = value
  } else if (address >= 0xFEA0 && address <= 0xFEFF) {
    () // Some games try to read/write here. No-op
  } else if (address >= 0xFF00 && address < 0xFF80) {
    mem.io[address - 0xFF00] = value
  } else if (address >= 0xFF80 && address <= 0xFFFF) {
    mem.hram[address - 0xFF80] = value
  } else {
    raise(InvalidMemoryAccess(sprintf("Invalid memory write at %04X = %02X", address, value)))
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
