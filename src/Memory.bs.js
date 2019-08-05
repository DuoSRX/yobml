// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function make(rom, gpu) {
  return /* record */[
          /* rom */rom,
          /* wram */Caml_array.caml_make_vect(8192, 0),
          /* exram */Caml_array.caml_make_vect(8192, 0),
          /* oam */Caml_array.caml_make_vect(160, 0),
          /* io */Caml_array.caml_make_vect(128, 0),
          /* hram */Caml_array.caml_make_vect(128, 0),
          /* gpu */gpu
        ];
}

var InvalidMemoryAccess = Caml_exceptions.create("Memory-Yobml.InvalidMemoryAccess");

function load(mem, address) {
  if (address >= 65284 && address <= 65287) {
    console.log(Curry._1(Printf.sprintf(/* Format */[
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
                ]), address));
  }
  if (address === 65348) {
    return mem[/* gpu */6][/* ly */3];
  } else if (address === 65280) {
    return 255;
  } else if (address < 32768) {
    return Caml_array.caml_array_get(mem[/* rom */0], address);
  } else if (address < 40960) {
    return Caml_array.caml_array_get(mem[/* gpu */6][/* vram */6], address & 8191);
  } else if (address >= 40960 && address < 49152) {
    return Caml_array.caml_array_get(mem[/* exram */2], address & 8191);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_get(mem[/* wram */1], address & 8191);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_get(mem[/* wram */1], address - 8192 & 8191);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_get(mem[/* oam */3], address & 159);
  } else if (address >= 65184 && address <= 65279) {
    return 0;
  } else if (address >= 65280 && address < 65408) {
    return Caml_array.caml_array_get(mem[/* io */4], address - 65280 | 0);
  } else if (address >= 65408 && address <= 65535) {
    return Caml_array.caml_array_get(mem[/* hram */5], address - 65408 | 0);
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
  if (address >= 65284 && address <= 65287) {
    console.log(Curry._2(Printf.sprintf(/* Format */[
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
                    ]),
                  "%04X = %02X"
                ]), address, value));
  }
  if (address === 65535) {
    console.log(Curry._2(Printf.sprintf(/* Format */[
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
                    ]),
                  "%04X = %02X"
                ]), address, value));
  }
  if (address < 32768) {
    return Caml_array.caml_array_set(mem[/* rom */0], address, value);
  } else if (address < 40960) {
    return Caml_array.caml_array_set(mem[/* gpu */6][/* vram */6], address & 8191, value);
  } else if (address >= 40960 && address < 49152) {
    return Caml_array.caml_array_set(mem[/* exram */2], address & 8191, value);
  } else if (address >= 49152 && address < 57344) {
    return Caml_array.caml_array_set(mem[/* wram */1], address & 8191, value);
  } else if (address >= 57344 && address < 65024) {
    return Caml_array.caml_array_set(mem[/* wram */1], address - 8192 & 8191, value);
  } else if (address >= 65024 && address <= 65183) {
    return Caml_array.caml_array_set(mem[/* oam */3], address & 159, value);
  } else if (address >= 65184 && address <= 65279) {
    return /* () */0;
  } else if (address >= 65280 && address < 65408) {
    return Caml_array.caml_array_set(mem[/* io */4], address - 65280 | 0, value);
  } else if (address >= 65408 && address <= 65535) {
    return Caml_array.caml_array_set(mem[/* hram */5], address - 65408 | 0, value);
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
