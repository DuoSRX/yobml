// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Memory$Yobml = require("./Memory.bs.js");
var Console$Yobml = require("./Console.bs.js");

var $$console = Console$Yobml.make(/* () */0);

Curry._1(Printf.printf(/* Format */[
          /* Int */Block.__(4, [
              /* Int_X */8,
              /* Lit_padding */Block.__(0, [
                  /* Zeros */2,
                  2
                ]),
              /* No_precision */0,
              /* End_of_format */0
            ]),
          "%02X"
        ]), Memory$Yobml.load($$console[/* memory */2], 32755));

Console$Yobml.run($$console);

exports.$$console = $$console;
/* console Not a pure module */
