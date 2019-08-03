// http://bgb.bircd.org/pandocs.htm#videodisplay

type mode = VBlank | HBlank | OamRead | LcdTransfer

type t = {
  mode: mode,
  lcd: int,
  control: int,
  ly: int,
  cycles: int,
  frame: array(array(int)),
  vram: array(int),
  rom: array(int)
}

let make = (~rom) => {
  mode: HBlank,
  lcd: 0x80,
  control: 0,
  ly: 0,
  cycles: 0,
  frame: Array.make_matrix(144, 160, 0),
  vram: Array.make(0x2000, 0),
  rom
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

let render_background = (gpu) => {
  // TODO: check if display is LCD is on
  // TODO: fetch "colors" from palette
  // TODO: set pixel color in frame

  let ly = gpu.ly
  // http://bgb.bircd.org/pandocs.htm#lcdpositionandscrolling
  let scroll_x = 0 // TODO: use actual x in FF41
  let scroll_y = 0 // TODO: use actual y in FF42
  let tile_data = 0x8000 // TODO: use actual data in FF40 (bit 4)
  let tile_map = 0x9800 // TODO: use actual data in FF40 (bit 3)
  let y = ((scroll_y + ly) / 8) mod 32
  let y_offset = (scroll_y + ly) mod 8

  for (px in 0 to 159) {
    let x = (((scroll_x + px) / 8) mod 32) land 0xFFFF
    let tile = load(gpu, ((tile_map + (y * 32) + x) land 0xFFFF))
    let ptr = (tile_data + tile * 0x10) land 0xFFFF // TODO: handle signed tile when tile_data <> 0x9000
    let ptr = ptr + (y_offset * 2) land 0xFFFF
    let p0 = load(gpu, ptr)
    let p1 = load(gpu, ptr + 1)
    let colb = -(((px + scroll_x) mod 8) - 7)
    let coln = ((p1 lsr colb) land 1 == 1) ? 1 : 0
    let coln = (coln lsl 1) lor ((p0 lsr colb) land 1 == 1 ? 1 : 0)
    // TODO: actual "colors"
    let color = switch(coln) {
      | 0x0 => 0
      | 0x1 => 1//0x40
      | 0x2 => 2//0x80
      | 0x3 => 3//0xEE
      | _ => failwith("impossiburu")
    }
    // if (color != 0) {
    //   Js.log(color);
    // };
    gpu.frame[ly][px] = color
  }
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

let step = (gpu, cycles) => {
  let cycles = gpu.cycles + cycles
  let gpu' = switch (gpu.mode) {
  | OamRead when cycles >= 80 => {
      let cycles = cycles - 80;
      set_mode({...gpu, cycles}, LcdTransfer)
    }
  | LcdTransfer when cycles >= 172 => {
      let cycles = cycles - 172;
      render_background(gpu);
      // TODO: render window
      // TODO: render objs
      // TODO: trigger interrupt
      set_mode({...gpu, cycles}, HBlank)
    }
  | HBlank when cycles >= 204 => {
      let cycles = cycles - 204;
      let ly = gpu.ly + 1
      if (ly == 144) {
        // TODO: set redraw
        // TODO: interrupts (Vblank + LcdStats)
        set_mode({...gpu, cycles, ly}, VBlank)
      } else {
        // TODO: interrupt LCD Stats
        set_mode({...gpu, cycles, ly}, OamRead)
      }
    }
  | VBlank when cycles >= 456 => {
      let cycles = cycles - 456
      let ly = gpu.ly + 1
      if (ly == 154) {
        // TODO: interrupt LCD stats
        set_mode({...gpu, cycles, ly:0}, OamRead)
      } else {
        {...gpu, cycles, ly}
      }
    }
  | _ => { {...gpu, cycles} }
  }

  // TODO: Ly and Lyc compare + interrupt
  gpu'
}
