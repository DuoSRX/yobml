// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Printf = require("bs-platform/lib/js/printf.js");
var Memory$Yobml = require("../Memory.bs.js");
var Console$Yobml = require("../Console.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var RegistersComponent$Yobml = require("./RegistersComponent.bs.js");

var fetch_rom = (
  async function logFetch(url) {
    const response = await fetch(url);
    const buffer = await response.arrayBuffer();
    return new Uint8Array(buffer);
  }
  );

var get_display = ( function() { return document.getElementById("display") } );

var display = (
    function display(canvas, pixels) {
      var ctx = canvas.getContext("2d");
      var imageData = new ImageData(new Uint8ClampedArray(pixels), 160, 144)
      ctx.putImageData(imageData, 0, 0);
    }
  );

var ConsoleFailure = Caml_exceptions.create("ConsoleComponent-Yobml.ConsoleFailure");

var steps = /* record */[/* contents */0];

var $$console = /* record */[/* contents */Console$Yobml.make(/* array */[])];

function step(canvas) {
  while(!$$console[0][/* gpu */1][/* new_frame */9]) {
    var tmp;
    try {
      tmp = Console$Yobml.step($$console[0]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Memory$Yobml.InvalidMemoryAccess) {
        var msg = Curry._2(Printf.sprintf(/* Format */[
                  /* String_literal */Block.__(11, [
                      "Console crash at $",
                      /* Int */Block.__(4, [
                          /* Int_X */8,
                          /* Lit_padding */Block.__(0, [
                              /* Zeros */2,
                              4
                            ]),
                          /* No_precision */0,
                          /* String_literal */Block.__(11, [
                              ". Reason: ",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* End_of_format */0
                                ])
                            ])
                        ])
                    ]),
                  "Console crash at $%04X. Reason: %s"
                ]), $$console[0][/* cpu */0][/* pc */0], exn[1]);
        throw [
              ConsoleFailure,
              msg
            ];
      } else {
        throw exn;
      }
    }
    $$console[0] = tmp;
  };
  Curry._2(display, canvas, $$console[0][/* gpu */1][/* frame */5]);
  $$console[0][/* gpu */1][/* new_frame */9] = false;
  steps[0] = steps[0] + 1 | 0;
  if (steps[0] < 2000) {
    setTimeout((function (param) {
            return step(canvas);
          }), 16);
    return /* () */0;
  } else {
    console.log("Done");
    return /* () */0;
  }
}

var initial_state = /* record */[
  /* running */false,
  /* loading */false
];

function ConsoleComponent(Props) {
  var match = React.useReducer((function (state, action) {
          if (action) {
            return /* record */[
                    /* running */state[/* running */0],
                    /* loading */false
                  ];
          } else {
            return /* record */[
                    /* running */state[/* running */0],
                    /* loading */true
                  ];
          }
        }), initial_state);
  var dispatch = match[1];
  React.useEffect((function () {
          Curry._1(fetch_rom, "http://localhost:8000/roms/drmario.gb").then((function (rom) {
                  Curry._1(dispatch, /* Loaded */1);
                  $$console[0] = Console$Yobml.make(rom);
                  step(Curry._1(get_display, /* () */0));
                  return Promise.resolve(/* () */0);
                }));
          return undefined;
        }), ([]));
  return React.createElement("div", undefined, match[0][/* loading */1] ? React.createElement("div", undefined, "Loading...") : null, React.createElement("canvas", {
                  id: "display"
                }), React.createElement(RegistersComponent$Yobml.make, {
                  console: $$console
                }));
}

var make = ConsoleComponent;

exports.fetch_rom = fetch_rom;
exports.get_display = get_display;
exports.display = display;
exports.ConsoleFailure = ConsoleFailure;
exports.steps = steps;
exports.$$console = $$console;
exports.step = step;
exports.initial_state = initial_state;
exports.make = make;
/* fetch_rom Not a pure module */
