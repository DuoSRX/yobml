open Printf

type t = {
  cartridge: Cartridge.t,
  wram: array(int),
  io: array(int),
  hram: array(int),
  gpu: Gpu.t,
  input: Input.t,
  timer: Timer.t
}

let make = (~cartridge, ~gpu, ~input, ~timer) => {
  {
    cartridge,
    gpu,
    input,
    timer,
    wram: Array.make(0x2000, 0),
    io: Array.make(0x80, 0),
    hram: Array.make(0x80, 0),
  }
}

exception InvalidMemoryAccess(string)

let load = (mem, address) => {
  if (address == 0xFF04) {
    mem.timer.div
  } else if (address == 0xFF05) {
    mem.timer.tima
  } else if (address == 0xFF06) {
    mem.timer.tma
  } else if (address == 0xFF07) {
    mem.timer.tac
  } else if (address == 0xFF42) {
    mem.gpu.scroll_y
  } else if (address == 0xFF43) {
    mem.gpu.scroll_x
  } else if (address == 0xFF44) {
    mem.gpu.ly
  } else if (address == 0xFF45) {
    mem.gpu.ly
  } else if (address == 0xFF00) {
    Input.get(mem.input)
  } else if (address < 0x8000) {
    mem.cartridge.load(address)
  } else if (address < 0xA000) {
    mem.gpu.vram[address land 0x1FFF]
  } else if (address >= 0xA000 && address <= 0xBFFF) {
    mem.cartridge.load(address)
  } else if (address >= 0xC000 && address < 0xE000) {
    mem.wram[address land 0x1FFF]
  } else if (address >= 0xE000 && address < 0xFE00) { // Mirror of C000-DDFF
    mem.wram[(address - 0x2000) land 0x1FFF]
  } else if (address >= 0xFE00 && address <= 0xFE9F) {
    mem.gpu.oam[address land 0x9F]
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
  if (address == 0xFF00) {
    Input.set(mem.input, value)
  } else if (address == 0xFF04) {
    mem.timer.div = value
  } else if (address == 0xFF05) {
    mem.timer.tima = value
  } else if (address == 0xFF06) {
    mem.timer.tma = value
  } else if (address == 0xFF07) {
    mem.timer.tac = value
  } else if (address == 0xFF42) {
    mem.gpu.scroll_y = value
  } else if (address == 0xFF43) {
    mem.gpu.scroll_x = value
  } else if (address == 0xFF44) {
    mem.gpu.ly = 0
  } else if (address == 0xFF45) {
    Js.log(value)
    mem.gpu.lyc = value
  } else if (address == 0xFF46) {
    // TODO: handle clock cycles for DMA (move into GPU or CPU?)
    let start = (value lsl 8) land 0xFFFF
    for (offset in 0 to 0x9F) {
      mem.gpu.oam[offset] = load(mem, start + offset)
    }
  } else if (address < 0x8000) {
    mem.cartridge.store(address, value)
  } else if (address < 0xA000) {
    mem.gpu.vram[address land 0x1FFF] = value
  } else if (address >= 0xA000 && address <= 0xBFFF) {
    mem.cartridge.store(address, value)
  } else if (address >= 0xC000 && address < 0xE000) {
    mem.wram[address land 0x1FFF] = value
  } else if (address >= 0xE000 && address < 0xFE00) { // Mirror of C000-DDFF
    mem.wram[(address - 0x2000) land 0x1FFF] = value
  } else if (address >= 0xFE00 && address <= 0xFE9F) {
    mem.gpu.oam[address land 0x9F] = value
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
