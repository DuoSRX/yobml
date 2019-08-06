// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Input$Yobml = require("./Input.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function make(cartridge, gpu, input, timer) {
  return /* record */[
          /* cartridge */cartridge,
          /* wram */Caml_array.caml_make_vect(8192, 0),
          /* io */Caml_array.caml_make_vect(128, 0),
          /* hram */Caml_array.caml_make_vect(128, 0),
          /* gpu */gpu,
          /* input */input,
          /* timer */timer
        ];
}

var InvalidMemoryAccess = Caml_exceptions.create("Memory-Yobml.InvalidMemoryAccess");

function load(mem, address) {
  if (address === 65284) {
    return mem[/* timer */6][/* div */3];
  } else if (address === 65285) {
    return mem[/* timer */6][/* tima */4];
  } else if (address === 65286) {
    return mem[/* timer */6][/* tma */5];
  } else if (address === 65287) {
    return mem[/* timer */6][/* tac */6];
  } else if (address === 65346) {
    return mem[/* gpu */4][/* scroll_y */11];
  } else if (address === 65347) {
    return mem[/* gpu */4][/* scroll_x */10];
  } else if (address === 65348 || address === 65349) {
    return mem[/* gpu */4][/* ly */3];
  } else if (address === 65280) {
    return Input$Yobml.get(mem[/* input */5]);
  } else if (address < 32768) {
    return Curry._1(mem[/* cartridge */0][/* load */1], address);
  } else if (address < 40960) {
    return Caml_array.caml_array_get(mem[/* gpu */4][/* vram */7], address & 8191);
  } else if (address >= 40960 && address <= 49151) {
    return Curry._1(mem[/* cartridge */0][/* load */1], address);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_get(mem[/* wram */1], address & 8191);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_get(mem[/* wram */1], address - 8192 & 8191);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_get(mem[/* gpu */4][/* oam */8], address & 159);
  } else if (address >= 65184 && address <= 65279) {
    return 0;
  } else if (address >= 65280 && address < 65408) {
    return Caml_array.caml_array_get(mem[/* io */2], address - 65280 | 0);
  } else if (address >= 65408 && address <= 65535) {
    return Caml_array.caml_array_get(mem[/* hram */3], address - 65408 | 0);
  } else {
    throw [
          InvalidMemoryAccess,
          Curry._1(Printf.sprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        "Invalid memory access at ",
                        /* Int */Block.__(4, [
                            /* Int_X */8,
                            /* Lit_padding */Block.__(0, [
                                /* Zeros */2,
                                4
                              ]),
                            /* No_precision */0,
                            /* End_of_format */0
                          ])
                      ]),
                    "Invalid memory access at %04X"
                  ]), address)
        ];
  }
}

function store(mem, address, value) {
  if (address === 65280) {
    return Input$Yobml.set(mem[/* input */5], value);
  } else if (address === 65284) {
    mem[/* timer */6][/* div */3] = value;
    return /* () */0;
  } else if (address === 65285) {
    mem[/* timer */6][/* tima */4] = value;
    return /* () */0;
  } else if (address === 65286) {
    mem[/* timer */6][/* tma */5] = value;
    return /* () */0;
  } else if (address === 65287) {
    mem[/* timer */6][/* tac */6] = value;
    return /* () */0;
  } else if (address === 65346) {
    mem[/* gpu */4][/* scroll_y */11] = value;
    return /* () */0;
  } else if (address === 65347) {
    mem[/* gpu */4][/* scroll_x */10] = value;
    return /* () */0;
  } else if (address === 65348) {
    mem[/* gpu */4][/* ly */3] = 0;
    return /* () */0;
  } else if (address === 65349) {
    mem[/* gpu */4][/* lyc */4] = value;
    return /* () */0;
  } else if (address === 65350) {
    var start = (value << 8) & 65535;
    for(var offset = 0; offset <= 159; ++offset){
      Caml_array.caml_array_set(mem[/* gpu */4][/* oam */8], offset, load(mem, start + offset | 0));
    }
    return /* () */0;
  } else if (address < 32768) {
    return Curry._2(mem[/* cartridge */0][/* store */2], address, value);
  } else if (address < 40960) {
    return Caml_array.caml_array_set(mem[/* gpu */4][/* vram */7], address & 8191, value);
  } else if (address >= 40960 && address <= 49151) {
    return Curry._2(mem[/* cartridge */0][/* store */2], address, value);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_set(mem[/* wram */1], address & 8191, value);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_set(mem[/* wram */1], address - 8192 & 8191, value);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_set(mem[/* gpu */4][/* oam */8], address & 159, value);
  } else if (address >= 65184 && address <= 65279) {
    return /* () */0;
  } else if (address >= 65280 && address < 65408) {
    return Caml_array.caml_array_set(mem[/* io */2], address - 65280 | 0, value);
  } else if (address >= 65408 && address <= 65535) {
    return Caml_array.caml_array_set(mem[/* hram */3], address - 65408 | 0, value);
  } else {
    throw [
          InvalidMemoryAccess,
          Curry._2(Printf.sprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        "Invalid memory write at ",
                        /* Int */Block.__(4, [
                            /* Int_X */8,
                            /* Lit_padding */Block.__(0, [
                                /* Zeros */2,
                                4
                              ]),
                            /* No_precision */0,
                            /* String_literal */Block.__(11, [
                                " = ",
                                /* Int */Block.__(4, [
                                    /* Int_X */8,
                                    /* Lit_padding */Block.__(0, [
                                        /* Zeros */2,
                                        2
                                      ]),
                                    /* No_precision */0,
                                    /* End_of_format */0
                                  ])
                              ])
                          ])
                      ]),
                    "Invalid memory write at %04X = %02X"
                  ]), address, value)
        ];
  }
}

function load16(mem, address) {
  var lo = load(mem, address);
  var hi = load(mem, address + 1 | 0);
  return lo | (hi << 8);
}

function store16(mem, address, value) {
  var lo = value & 255;
  var hi = ((value & 65280) >>> 8);
  store(mem, address, lo);
  return store(mem, address + 1 | 0, hi);
}

exports.make = make;
exports.InvalidMemoryAccess = InvalidMemoryAccess;
exports.load = load;
exports.store = store;
exports.load16 = load16;
exports.store16 = store16;
/* No side effect */
