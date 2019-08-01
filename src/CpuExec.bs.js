// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Cpu$Yobml = require("./Cpu.bs.js");
var Memory$Yobml = require("./Memory.bs.js");
var Instructions$Yobml = require("./Instructions.bs.js");
var InstructionsExec$Yobml = require("./InstructionsExec.bs.js");

function trace(cpu, instruction) {
  console.log(Curry._6(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "AF:",
                    /* Int */Block.__(4, [
                        /* Int_X */8,
                        /* Lit_padding */Block.__(0, [
                            /* Zeros */2,
                            4
                          ]),
                        /* No_precision */0,
                        /* String_literal */Block.__(11, [
                            " BC:",
                            /* Int */Block.__(4, [
                                /* Int_X */8,
                                /* Lit_padding */Block.__(0, [
                                    /* Zeros */2,
                                    4
                                  ]),
                                /* No_precision */0,
                                /* String_literal */Block.__(11, [
                                    " DE:",
                                    /* Int */Block.__(4, [
                                        /* Int_X */8,
                                        /* Lit_padding */Block.__(0, [
                                            /* Zeros */2,
                                            4
                                          ]),
                                        /* No_precision */0,
                                        /* String_literal */Block.__(11, [
                                            " HL:",
                                            /* Int */Block.__(4, [
                                                /* Int_X */8,
                                                /* Lit_padding */Block.__(0, [
                                                    /* Zeros */2,
                                                    4
                                                  ]),
                                                /* No_precision */0,
                                                /* Char_literal */Block.__(12, [
                                                    /* " " */32,
                                                    /* Int */Block.__(4, [
                                                        /* Int_X */8,
                                                        /* Lit_padding */Block.__(0, [
                                                            /* Zeros */2,
                                                            4
                                                          ]),
                                                        /* No_precision */0,
                                                        /* String_literal */Block.__(11, [
                                                            ": ",
                                                            /* String */Block.__(2, [
                                                                /* No_padding */0,
                                                                /* End_of_format */0
                                                              ])
                                                          ])
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ])
                              ])
                          ])
                      ])
                  ]),
                "AF:%04X BC:%04X DE:%04X HL:%04X %04X: %s"
              ]), Cpu$Yobml.get_register16(cpu, /* AF */0), Cpu$Yobml.get_register16(cpu, /* BC */1), Cpu$Yobml.get_register16(cpu, /* DE */2), Cpu$Yobml.get_register16(cpu, /* HL */3), cpu[/* pc */1], Instructions$Yobml.pretty(instruction)));
  return /* () */0;
}

function step(cpu, breakpoints) {
  var instruction = Instructions$Yobml.decode(Memory$Yobml.load(cpu[/* memory */5], cpu[/* pc */1]));
  trace(cpu, instruction);
  var cpu_000 = /* sp */cpu[/* sp */0];
  var cpu_001 = /* pc */cpu[/* pc */1] + 1 | 0;
  var cpu_002 = /* cycle */cpu[/* cycle */2];
  var cpu_003 = /* ime */cpu[/* ime */3];
  var cpu_004 = /* registers */cpu[/* registers */4];
  var cpu_005 = /* memory */cpu[/* memory */5];
  var cpu$1 = /* record */[
    cpu_000,
    cpu_001,
    cpu_002,
    cpu_003,
    cpu_004,
    cpu_005
  ];
  return /* tuple */[
          InstructionsExec$Yobml.execute(cpu$1, instruction),
          instruction
        ];
}

exports.trace = trace;
exports.step = step;
/* Instructions-Yobml Not a pure module */
