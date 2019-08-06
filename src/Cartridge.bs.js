// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var CartridgeHeader$Yobml = require("./CartridgeHeader.bs.js");

function in_range(a, b, n) {
  if (Caml_obj.caml_greaterequal(n, a)) {
    return Caml_obj.caml_lessequal(n, b);
  } else {
    return false;
  }
}

function make_rom_only(rom, header) {
  var load = function (address) {
    if (in_range(0, 32767, address)) {
      return Caml_array.caml_array_get(rom, address);
    } else if (in_range(40960, 49151, address)) {
      return 0;
    } else {
      return Pervasives.failwith(Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Unhandled rom read $",
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
                          "Unhandled rom read $%04X"
                        ]), address));
    }
  };
  var store = function (param, param$1) {
    return /* () */0;
  };
  return /* record */[
          /* header */header,
          /* load */load,
          /* store */store
        ];
}

function make_mbc1(rom, header) {
  var mode = /* record */[/* contents : Rom */4102640];
  var rom_bank = /* record */[/* contents */1];
  var ram_bank = /* record */[/* contents */0];
  Caml_array.caml_make_vect(Caml_primitive.caml_int_min(header[/* ram_size */2], 2048), 0);
  var load = function (address) {
    if (in_range(0, 16383, address)) {
      return Caml_array.caml_array_get(rom, address);
    } else if (in_range(16384, 32767, address)) {
      var offset = (rom_bank[0] << 14);
      return Caml_array.caml_array_get(rom, offset + (address & 16383) | 0);
    } else if (in_range(40960, 49151, address)) {
      return 0;
    } else {
      return Pervasives.failwith(Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Unhandled rom read $",
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
                          "Unhandled rom read $%04X"
                        ]), address));
    }
  };
  var store = function (address, value) {
    if (in_range(0, 8191, address)) {
      return /* () */0;
    } else if (in_range(8192, 16383, address)) {
      rom_bank[0] = rom_bank[0] & 224 | value & 31;
      return /* () */0;
    } else if (in_range(16384, 24575, address)) {
      var match = mode[0];
      if (match >= 4102640) {
        var match$1 = value === 0;
        rom_bank[0] = rom_bank[0] & 31 | ((
            match$1 ? 1 : value
          ) << 5);
        return /* () */0;
      } else {
        ram_bank[0] = value & 3;
        return /* () */0;
      }
    } else if (in_range(24576, 32767, address)) {
      var match$2 = (value & 1) === 0;
      mode[0] = match$2 ? /* Rom */4102640 : /* Ram */4099518;
      return /* () */0;
    } else if (in_range(40960, 49151, address)) {
      return /* () */0;
    } else {
      return Pervasives.failwith(Curry._2(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Unhandled rom write $",
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
                          "Unhandled rom write $%04X = %02X"
                        ]), address, value));
    }
  };
  return /* record */[
          /* header */header,
          /* load */load,
          /* store */store
        ];
}

function make(rom) {
  var header = CartridgeHeader$Yobml.make(rom);
  console.log(header);
  var match = header[/* cartridge_type */0];
  switch (match) {
    case 0 : 
        return make_rom_only(rom, header);
    case 1 : 
        return make_mbc1(rom, header);
    case 2 : 
        return Pervasives.failwith("Unimplemented MBC type");
    
  }
}

exports.in_range = in_range;
exports.make_rom_only = make_rom_only;
exports.make_mbc1 = make_mbc1;
exports.make = make;
/* No side effect */
