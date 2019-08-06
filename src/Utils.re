let wrapping_add = (a, b) => (a + b) land 0xFF

let wrapping_add16 = (a, b) => (a + b) land 0xFFFF

let signed = (v) => v > 0x7F ? -((lnot(v) + 1) land 0xFF) : v

let is_bit_set = (byte, bit) => {
  byte land (1 lsl bit) > 0
}

let set_bit = (byte, bit) => {
  byte lor (1 lsl bit)
}

let clear_bit = (byte, bit) => {
  byte land (lnot(1 lsl bit))
}

let split_word = (word) => {
  let lo = word land 0xFF
  let hi = (word land 0xFF00) lsr 8;
  (lo, hi)
}