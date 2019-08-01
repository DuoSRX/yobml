// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Array = require("bs-platform/lib/js/array.js");
var Cpu$Yobml = require("./Cpu.bs.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var CpuExec$Yobml = require("./CpuExec.bs.js");

function make(param) {
  var file = Fs.readFileSync("./roms/01-special.gb", "binary");
  var rom = $$Array.mapi((function (n, param) {
          return Caml_string.get(file, n);
        }), Caml_array.caml_make_vect(file.length, 0));
  return /* record */[
          /* cpu */Cpu$Yobml.make(rom),
          /* running */true
        ];
}

function run($$console) {
  var _console = $$console;
  var _steps = 0;
  while(true) {
    var steps = _steps;
    var $$console$1 = _console;
    var match = CpuExec$Yobml.step($$console$1[/* cpu */0], /* [] */0);
    var cpu = match[0];
    if (steps < 10000000) {
      _steps = steps + 1 | 0;
      _console = /* record */[
        /* cpu */cpu,
        /* running */$$console$1[/* running */1]
      ];
      continue ;
    } else {
      return CpuExec$Yobml.trace(cpu, match[1]);
    }
  };
}

exports.make = make;
exports.run = run;
/* fs Not a pure module */
