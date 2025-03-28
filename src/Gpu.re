open Utils

// http://bgb.bircd.org/pandocs.htm#videodisplay

type mode = VBlank | HBlank | OamRead | LcdTransfer

type t = {
  mode: mode,
  lcd: int,
  mutable control: int,
  mutable ly: int,
  mutable lyc: int,
  cycles: int,
  frame: array(int),
  vram: array(int),
  oam: array(int),
  cartridge: Cartridge.t,
  mutable scroll_x: int,
  mutable scroll_y: int,
  mutable window_x: int,
  mutable window_y: int,
  mutable interrupts: int,
  mutable new_frame: bool
}

let make = (~cartridge) => {
  mode: HBlank,
  lcd: 0x80,
  control: 0,
  ly: 0,
  lyc: 0,
  scroll_x: 0,
  scroll_y: 0,
  window_x: 0,
  window_y: 0,
  cycles: 0,
  frame: Array.make(160 * 144, 0),
  vram: Array.make(0x2000, 0),
  oam: Array.make(0xA0, 0),
  cartridge,
  interrupts: 0,
  new_frame: false
}

let load = (gpu, address) => {
  if (address < 0x8000) {
    gpu.cartridge.load(address)
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

let set_pixel = (gpu, ~x, ~y, ~color) => {
  let offset = y * 160 + x
  gpu.frame[offset] = color // R
}

let get_pixel = (gpu, ~x, ~y) => {
  let offset = y * 160 + x
  gpu.frame[offset]
}

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
  let scroll_x = gpu.scroll_x
  let scroll_y = gpu.scroll_y
  let tile_data = gpu.control land 0x10 > 0 ? 0x8000 : 0x9000
  let tile_map = gpu.control land 0x8 > 0 ? 0x9C00 : 0x9800
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
    let color = colors[coln]
    set_pixel(gpu, ~x=px, ~y=ly, ~color)
  }
}

// Holy copy-pasta batman. Refactor with render_background somehow?
let render_window = (gpu, io_regs) => {
  let palette = io_regs[0x47]
  let colors = [|
    palette land 3,
    (palette lsr 2) land 3,
    (palette lsr 4) land 3,
    (palette lsr 6) land 3,
  |]

  let ly = gpu.ly
  let tile_data = gpu.control land 0x10 > 0 ? 0x8000 : 0x9000
  let tile_map = gpu.control land 0x40 > 0 ? 0x9C00 : 0x9800
  let window_y = ly - gpu.window_y
  let y = window_y / 8
  let y_offset = window_y mod 8
  let window_x = gpu.window_x - 7

  for (px in 0 to 159) {
    if (px < window_x) {
      ()
    } else {
      let x = (px - window_x) / 8
      let tile = load(gpu, (tile_map + (y * 32) + x land 0xFFFF))
      let ptr = switch(tile_data) {
      | 0x9000 => (tile_data + signed(tile) * 0x10) land 0xFFFF
      | _ => (tile_data + tile * 0x10) land 0xFFFF
      }
      let ptr = (ptr + (y_offset * 2)) land 0xFFFF
      let p0 = load(gpu, ptr)
      let p1 = load(gpu, ptr + 1)
      let colb = 7 - px mod 8
      let pix0 = is_bit_set(p0, colb) ? 1 : 0
      let pix1 = is_bit_set(p1, colb) ? 2 : 0
      let coln = pix0 lor pix1
      let color = colors[coln]
      set_pixel(gpu, ~x=px, ~y=ly, ~color)
    }
  }
}

type sprite = { x:int, y:int, index:int, attrs: int }

let sprite_palette = (palette) => {
  [|
    0,
    (palette lsr 2) land 3,
    (palette lsr 4) land 3,
    (palette lsr 6) land 3,
  |]
}

let render_sprites = (gpu, io_regs) => {
  let sprite_height = gpu.control land 0x4 > 0 ? 16 : 8
  let ly = gpu.ly

  let rec loop = (n) => {
    let y = gpu.oam[n]
    let x = gpu.oam[n + 1]
    let index = gpu.oam[n + 2]
    let attrs = gpu.oam[n + 3];
    let sprite = { x, y, index, attrs }

    let y = sprite.y - 16
    let on_scanline = (y <= ly) && (y + sprite_height) > ly;
    let x = sprite.x - 8

    if (on_scanline) {
      let y_offset = (sprite.attrs land 0x40 > 0) ? (sprite_height - 1) - (ly - y) : ly - y // Y Flip
      let ptr = (sprite.index * 16) + (y_offset * 2)
      let lo = load(gpu, 0x8000 + ptr)
      let hi = load(gpu, 0x8000 + ptr + 1)

      for (idx_x in 0 to 7) {
        let pixel_x = x + idx_x;
        if ((pixel_x >= 0) && (pixel_x <= 160)) {
          let bit = (sprite.attrs land 0x20 > 0) ? idx_x : 7 - idx_x // X Flip
          let pixel = (hi lsr bit land 1 == 1) ? 2 : 0
          let pixel = (lo lsr bit land 1 == 1) ? pixel lor 1 : pixel
          let palette_number = (sprite.attrs land 0x8 == 0) ? io_regs[0x48] : io_regs[0x49]
          let colors = sprite_palette(palette_number)
          let color = colors[pixel]
          if (pixel != 0 && sprite.attrs land 0x80 == 0 || get_pixel(gpu, ~x=pixel_x, ~y=ly) == 0) {
            // TODO: obj priority
            set_pixel(gpu, ~x=pixel_x, ~y=ly, ~color)
          }
        }
      }
    }
    if (n < 156) { loop(n + 4) }
  }

  loop(0)
}

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

  let gpu = switch (gpu.mode) {
  | OamRead when cycles >= 80 => {
      let cycles = cycles - 80;
      set_mode({...gpu, cycles}, LcdTransfer)
    }
  | LcdTransfer when cycles >= 172 => {
      let cycles = cycles - 172;
      if (lcd_on) {
        if (is_bit_set(gpu.control, 0)) {
          ()// render_background(gpu, io_regs)
        } else {
          Array.fill(gpu.frame, 0, 160 * 144, 0)
        }
        // if (is_bit_set(gpu.control, 5)) {
          render_window(gpu, io_regs)
        // }
        render_sprites(gpu, io_regs)
      }
      set_mode({...gpu, cycles}, HBlank)
    }
  | HBlank when cycles >= 204 => {
      let cycles = cycles - 204;
      let ly = gpu.ly + 1
      if (ly == 144) {
        set_mode({...gpu, cycles, ly, interrupts: 1, new_frame: true}, VBlank)
      } else {
        set_mode({...gpu, cycles, ly}, OamRead)
      }
    }
  | VBlank when cycles >= 456 => {
      let cycles = cycles - 456
      let ly = gpu.ly + 1
      if (ly >= 154) {
        set_mode({...gpu, cycles, ly:0}, OamRead)
      } else {
        {...gpu, cycles, ly}
      }
    }
  | _ => { {...gpu, cycles} }
  }

  // TODO: Ly and Lyc compare + interrupt
  if (gpu.ly == gpu.lyc) {
    {...gpu, lcd: set_bit(gpu.lcd, 6), interrupts: gpu.interrupts lor 2}
  } else {
    {...gpu, lcd: set_bit(gpu.lcd, 6)}
  };
}
