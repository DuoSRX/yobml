// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function make(rom) {
  return /* record */[
          /* rom */rom,
          /* wram */Caml_array.caml_make_vect(8192, 0),
          /* vram */Caml_array.caml_make_vect(8192, 0),
          /* exram */Caml_array.caml_make_vect(8192, 0),
          /* oam */Caml_array.caml_make_vect(160, 0),
          /* io */Caml_array.caml_make_vect(128, 0),
          /* hram */Caml_array.caml_make_vect(128, 0)
        ];
}

var MemoryAccessUnimplement = Caml_exceptions.create("Memory-Yobml.MemoryAccessUnimplement");

function load(mem, address) {
  if (address < 32768) {
    return Caml_array.caml_array_get(mem[/* rom */0], address);
  } else if (address < 40960) {
    return Caml_array.caml_array_get(mem[/* vram */2], address & 8191);
  } else if (address >= 40960 && address < 49152) {
    return Caml_array.caml_array_get(mem[/* exram */3], address & 8191);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_get(mem[/* wram */1], address & 8191);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_get(mem[/* wram */1], address - 8192 & 8191);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_get(mem[/* oam */4], address & 159);
  } else if (address >= 65184 && address <= 65279) {
    return 0;
  } else if (address >= 65280 && address < 65408) {
    return 148;
  } else if (address >= 65408) {
    return Caml_array.caml_array_get(mem[/* hram */6], address & 127);
  } else {
    throw [
          MemoryAccessUnimplement,
          Curry._1(Printf.sprintf(/* Format */[
                    /* Int */Block.__(4, [
                        /* Int_X */8,
                        /* Lit_padding */Block.__(0, [
                            /* Zeros */2,
                            4
                          ]),
                        /* No_precision */0,
                        /* End_of_format */0
                      ]),
                    "%04X"
                  ]), address)
        ];
  }
}

function store(mem, address, value) {
  if (address < 32768) {
    return Caml_array.caml_array_set(mem[/* rom */0], address, value);
  } else if (address < 40960) {
    return Caml_array.caml_array_set(mem[/* vram */2], address & 8191, value);
  } else if (address >= 40960 && address < 49152) {
    return Caml_array.caml_array_set(mem[/* exram */3], address & 8191, value);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_set(mem[/* wram */1], address & 8191, value);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_set(mem[/* wram */1], address - 8192 & 8191, value);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_set(mem[/* oam */4], address & 159, value);
  } else if (address >= 65184 && address <= 65279) {
    return /* () */0;
  } else if (address >= 65280 && address < 65408) {
    return Caml_array.caml_array_set(mem[/* io */5], address & 127, value);
  } else if (address >= 65408) {
    return Caml_array.caml_array_set(mem[/* hram */6], address & 127, value);
  } else {
    throw [
          MemoryAccessUnimplement,
          Curry._1(Printf.sprintf(/* Format */[
                    /* Int */Block.__(4, [
                        /* Int_X */8,
                        /* Lit_padding */Block.__(0, [
                            /* Zeros */2,
                            4
                          ]),
                        /* No_precision */0,
                        /* End_of_format */0
                      ]),
                    "%04X"
                  ]), address)
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
exports.MemoryAccessUnimplement = MemoryAccessUnimplement;
exports.load = load;
exports.store = store;
exports.load16 = load16;
exports.store16 = store16;
/* No side effect */
