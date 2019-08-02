[@bs.deriving jsConverter]
type t = {
  mutable a : int,
  mutable b : int,
  mutable c : int,
  mutable d : int,
  mutable e : int,
  mutable f : int,
  mutable h : int,
  mutable l : int,
  mutable sp : int,
}

let make = () => {
  { a: 1, b: 0, c: 0x13, d: 0, e: 0xD8, f: 0xB0, h: 1, l: 0x4D, sp: 0xFFFE };
}

type register =
  | A | B | C | D | E | F | H | L;

type register16 =
  | AF | BC | DE | HL | SP;

let get = (regs, reg) => switch(reg) {
  | A => regs.a
  | B => regs.b
  | C => regs.c
  | D => regs.d
  | E => regs.e
  | F => regs.f
  | H => regs.h
  | L => regs.l
}

let set = (regs, reg, value) => switch(reg) {
  | A => regs.a = value
  | B => regs.b = value
  | C => regs.c = value
  | D => regs.d = value
  | E => regs.e = value
  | F => regs.f = value
  | H => regs.h = value
  | L => regs.l = value
}

let get16 = (regs, reg) => switch(reg) {
  | AF => (regs.a lsl 8) lor regs.f
  | BC => (regs.b lsl 8) lor regs.c
  | DE => (regs.d lsl 8) lor regs.e
  | HL => (regs.h lsl 8) lor regs.l
  | SP => regs.sp
}

let set16 = (regs, reg, value) => {
  let hi = value lsr 8;
  let lo = value land 0xFF;
  switch(reg) {
  | AF => regs.a = hi; regs.f = lo
  | BC => regs.b = hi; regs.c = lo
  | DE => regs.d = hi; regs.e = lo
  | HL => regs.h = hi; regs.l = lo
  | SP => regs.sp = value
  }
}

let to_string = (reg) => switch(reg) {
  | A => "A"
  | B => "B"
  | C => "C"
  | D => "D"
  | E => "E"
  | F => "F"
  | H => "H"
  | L => "L"
}

let to_string16 = (reg) => switch(reg) {
  | AF => "AF"
  | BC => "BC"
  | DE => "DE"
  | HL => "HL"
  | SP => "SP"
}