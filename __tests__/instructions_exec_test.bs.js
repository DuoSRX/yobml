// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Cpu$Yobml = require("../src/Cpu.bs.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Console$Yobml = require("../src/Console.bs.js");
var InstructionsExec$Yobml = require("../src/InstructionsExec.bs.js");

function reset(pc) {
  var rom = Caml_array.caml_make_vect(8192, 0);
  var $$console = Console$Yobml.make(rom);
  var init = $$console[/* cpu */0];
  return /* record */[
          /* pc */pc,
          /* cycle */init[/* cycle */1],
          /* ime */init[/* ime */2],
          /* registers */init[/* registers */3],
          /* memory */init[/* memory */4],
          /* halted */init[/* halted */5],
          /* serial */init[/* serial */6]
        ];
}

Jest.describe("CALL", (function (param) {
        Jest.test("Change PC to the next 2 bytes", (function (param) {
                var cpu = reset(0);
                Caml_array.caml_array_set(cpu[/* memory */4][/* rom */0], 0, 52);
                Caml_array.caml_array_set(cpu[/* memory */4][/* rom */0], 1, 18);
                var cpu$1 = InstructionsExec$Yobml.execute(cpu, /* Call */7);
                return Jest.Expect[/* toEqual */12](4660, Jest.Expect[/* expect */0](cpu$1[/* pc */0]));
              }));
        Jest.test("Decrement SP by 2", (function (param) {
                var __x = reset(0);
                var cpu = InstructionsExec$Yobml.execute(__x, /* Call */7);
                return Jest.Expect[/* toEqual */12](65532, Jest.Expect[/* expect */0](Cpu$Yobml.get_register16(cpu, /* SP */4)));
              }));
        return Jest.test("Stores PC+2 at SP", (function (param) {
                      var __x = reset(4660);
                      var cpu = InstructionsExec$Yobml.execute(__x, /* Call */7);
                      return Jest.Expect[/* toEqual */12](4662, Jest.Expect[/* expect */0](InstructionsExec$Yobml.load16(cpu, 65532)));
                    }));
      }));

exports.reset = reset;
/*  Not a pure module */
