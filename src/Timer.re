open Utils

// http://gbdev.gg8.se/wiki/articles/Timer_and_Divider_Registers
type t = {
  mutable cycles: int, // machine cycles
  mutable counter: int,
  mutable div_counter: int,

  mutable div: int,  // FF04
  mutable tima: int, // FF05
  mutable tma: int,  // FF06

  // Bit  2   - Timer Enable
  // Bits 1-0 - Input Clock Select
  //  00: CPU Clock / 1024 (DMG, CGB:   4096 Hz, SGB:   ~4194 Hz)
  //  01: CPU Clock / 16   (DMG, CGB: 262144 Hz, SGB: ~268400 Hz)
  //  10: CPU Clock / 64   (DMG, CGB:  65536 Hz, SGB:  ~67110 Hz)
  //  11: CPU Clock / 256  (DMG, CGB:  16384 Hz, SGB:  ~16780 Hz)
  mutable tac: int   // FF07
}

let make = () => {
  cycles: 0,
  counter: 0,
  div_counter: 0,
  div: 0,
  tima: 0,
  tma: 0,
  tac: 0
}

let clock_select = (timer) => {
  if (timer.tac land 0x4 == 0) {
    0
  } else {
    switch(timer.tac land 0x3) {
    | 0 => 1024
    | 1 => 16
    | 2 => 64
    | _ => 256
    }
  }
}

let tick = (timer, cy) => {
  timer.cycles = timer.cycles + cy;

  // The CPU runs at around 4 MHz and div increments at 16 MHz
  // So we only run the process every 4 CPU cycles
  if (timer.cycles >= 4) {
    timer.cycles = timer.cycles - 4
    timer.counter = timer.counter + 1
    timer.div_counter = timer.div_counter + 1
    if (timer.div_counter == 16) {
      timer.div = wrapping_add(timer.div, 1)
      timer.div_counter = 0
    }
  };

  let cselect = clock_select(timer)
  if (cselect > 0 && timer.counter > cselect) {
    timer.counter = 0
    timer.tima = wrapping_add(timer.tima, 1)
    if (timer.tima == 0) {
      timer.tima = timer.tma;
      true
    } else {
      false
    }
  } else {
    false
  }
}