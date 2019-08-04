// http://bgb.bircd.org/pandocs.htm#videodisplay

type mode = VBlank | HBlank | OamRead | LcdTransfer

type t = {
  mode: mode,
  lcd: int,
  control: int,
  ly: int,
  cycles: int,
  frame: array(int),
  vram: array(int),
  rom: array(int),
  mutable interrupts: int,
  mutable new_frame: bool
}

let make = (~rom) => {
  mode: HBlank,
  lcd: 0x80,
  control: 0,
  ly: 0,
  cycles: 0,
  frame: Array.make(160 * 144 * 4, 0x7F),
  vram: Array.make(0x2000, 0),
  rom,
  interrupts: 0,
  new_frame: false
}

let load = (gpu, address) => {
  if (address < 0x8000) {
    gpu.rom[address]
  } else if (address >= 0x8000 && address <= 0x9FFF) {
    gpu.vram[address land 0x1FFF]
  } else {
    failwith("Oh no")
  }
}

let store = (gpu, address, value) => {
  if (address >= 0x8000 && address <= 0x9FFF) {
    gpu.vram[address land 0x1FFF] = value
  } else {
    failwith("Oh no")
  }
}

let color_map = [|0xFF, 0xA0, 0x50, 0x0|]
let signed = (v) => v > 0x7F ? -((lnot(v) + 1) land 0xFF) : v

let render_background = (gpu, io_regs) => {
  let palette = io_regs[0x47]
  let colors = [|
    palette land 3,
    (palette lsr 2) land 3,
    (palette lsr 4) land 3,
    (palette lsr 6) land 3,
  |]

  let ly = gpu.ly
  // http://bgb.bircd.org/pandocs.htm#lcdpositionandscrolling
  let scroll_x = 0 // TODO: use actual x in FF41
  let scroll_y = 0 // TODO: use actual y in FF42
  let ff40 = io_regs[0x40]
  let tile_data = ff40 land 0x10 > 0 ? 0x8000 : 0x9000 // 8800 or 9000?
  let tile_map = ff40 land 0x8 > 0 ? 0x9C00 : 0x9800
  let y = ((scroll_y + ly) / 8) mod 32
  let y_offset = (scroll_y + ly) mod 8

  for (px in 0 to 159) {
    let x = (((scroll_x + px) / 8) mod 32) land 0xFFFF
    let tile = load(gpu, ((tile_map + (y * 32) + x) land 0xFFFF))
    let ptr = switch(tile_data) {
    | 0x9000 => (tile_data + signed(tile) * 0x10) land 0xFFFF
    | _ => (tile_data + tile * 0x10) land 0xFFFF
    }
    let ptr = (ptr + (y_offset * 2)) land 0xFFFF
    let p0 = load(gpu, ptr)
    let p1 = load(gpu, ptr + 1)
    let colb = -(((px + scroll_x) mod 8) - 7)
    let coln = ((p1 lsr colb) land 1 == 1) ? 1 : 0
    let coln = (coln lsl 1) lor ((p0 lsr colb) land 1 == 1 ? 1 : 0)
    let color = color_map[colors[coln]]

    let offset = (ly * 160 + px) * 4 // 160 pixel per row, 4 byte per pixel (RGBA)
    gpu.frame[offset + 0] = color
    gpu.frame[offset + 1] = color
    gpu.frame[offset + 2] = color
    gpu.frame[offset + 3] = 0xFF
  }
}

// let interrupt = (memory, int_dst) => {
//   let current = Memory.load(memory, 0xFF0F);
//   Memory.store(memory, 0xFF0F, current lor int_dst)
// }

let set_mode = (gpu, mode) => {
  let cleared = gpu.lcd land 0b1111_1100
  switch(mode) {
  | HBlank => {...gpu, mode: HBlank, lcd: cleared}
  | VBlank => {...gpu, mode: VBlank, lcd: cleared + 1}
  | OamRead => {...gpu, mode: OamRead, lcd: cleared + 2}
  | LcdTransfer => {...gpu, mode: LcdTransfer, lcd: cleared + 3}
  }
}

let step = (gpu, cycles, lcd_on, io_regs) => {
  let cycles = gpu.cycles + cycles
  let gpu = {...gpu, interrupts: 0} // reset interrupts... bleh

  let gpu' = switch (gpu.mode) {
  | OamRead when cycles >= 80 => {
      let cycles = cycles - 80;
      set_mode({...gpu, cycles}, LcdTransfer)
    }
  | LcdTransfer when cycles >= 172 => {
      let cycles = cycles - 172;
      if (lcd_on) { render_background(gpu, io_regs) };
      // TODO: render window
      // TODO: render objs
      set_mode({...gpu, cycles, interrupts:2}, HBlank)
    }
  | HBlank when cycles >= 204 => {
      let cycles = cycles - 204;
      let ly = gpu.ly + 1
      if (ly == 144) {
        set_mode({...gpu, cycles, ly, interrupts: 3, new_frame: true}, VBlank)
      } else {
        set_mode({...gpu, cycles, ly, interrupts: 2}, OamRead)
      }
    }
  | VBlank when cycles >= 456 => {
      let cycles = cycles - 456
      let ly = gpu.ly + 1
      if (ly >= 154) {
        set_mode({...gpu, cycles, ly:0, interrupts: 2}, OamRead)
      } else {
        {...gpu, cycles, ly}
      }
    }
  | _ => { {...gpu, cycles} }
  }

  // TODO: Ly and Lyc compare + interrupt
  gpu'
}
