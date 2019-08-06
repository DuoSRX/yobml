// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Printf = require("bs-platform/lib/js/printf.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Memory$Yobml = require("../Memory.bs.js");
var Console$Yobml = require("../Console.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Instructions$Yobml = require("../Instructions.bs.js");
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

var $$console = /* record */[/* contents */Console$Yobml.make(Caml_array.caml_make_vect(512, 0))];

function step(canvas) {
  while(!$$console[0][/* gpu */1][/* new_frame */10]) {
    var tmp;
    try {
      tmp = Console$Yobml.step($$console[0]);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      var exit = 0;
      var msg;
      if (exn[0] === Memory$Yobml.InvalidMemoryAccess || exn[0] === Instructions$Yobml.OpcodeNotImplemented) {
        msg = exn[1];
        exit = 1;
      } else {
        throw exn;
      }
      if (exit === 1) {
        var msg$1 = Curry._2(Printf.sprintf(/* Format */[
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
                ]), $$console[0][/* cpu */0][/* pc */0], msg);
        console.log(msg$1);
        throw [
              ConsoleFailure,
              msg$1
            ];
      }
      
    }
    $$console[0] = tmp;
  };
  Curry._2(display, canvas, $$console[0][/* gpu */1][/* frame */5]);
  $$console[0][/* gpu */1][/* new_frame */10] = false;
  requestAnimationFrame((function (param) {
          return step(canvas);
        }));
  return /* () */0;
}

var initial_state = /* record */[
  /* loading */false,
  /* running */false
];

function handle_key_down(dispatch, ev) {
  return Curry._1(dispatch, /* KeyDown */Block.__(0, [ev.key]));
}

function handle_key_up(dispatch, ev) {
  return Curry._1(dispatch, /* KeyUp */Block.__(1, [ev.key]));
}

function ConsoleComponent(Props) {
  var match = React.useReducer((function (state, action) {
          if (typeof action === "number") {
            switch (action) {
              case 0 : 
                  return /* record */[
                          /* loading */true,
                          /* running */state[/* running */1]
                        ];
              case 1 : 
                  return /* record */[
                          /* loading */false,
                          /* running */state[/* running */1]
                        ];
              case 2 : 
                  $$console[0][/* tracing */5] = !$$console[0][/* tracing */5];
                  return state;
              
            }
          } else if (action.tag) {
            Console$Yobml.key_up($$console[0], action[0]);
            return state;
          } else {
            Console$Yobml.key_down($$console[0], action[0]);
            return state;
          }
        }), initial_state);
  var dispatch = match[1];
  React.useEffect((function () {
          Curry._1(fetch_rom, "http://localhost:8000/roms/drmario.gb").then((function (rom) {
                  Curry._1(dispatch, /* Loaded */1);
                  $$console[0] = Console$Yobml.make(rom);
                  requestAnimationFrame((function (param) {
                          return step(Curry._1(get_display, /* () */0));
                        }));
                  return Promise.resolve(/* () */0);
                }));
          addEventListener("keydown", (function (ev) {
                  return Curry._1(dispatch, /* KeyDown */Block.__(0, [ev.key]));
                }));
          addEventListener("keyup", (function (ev) {
                  return Curry._1(dispatch, /* KeyUp */Block.__(1, [ev.key]));
                }));
          return (function (param) {
                    removeEventListener("keydown", (function (ev) {
                            return Curry._1(dispatch, /* KeyDown */Block.__(0, [ev.key]));
                          }));
                    removeEventListener("keyup", (function (ev) {
                            return Curry._1(dispatch, /* KeyUp */Block.__(1, [ev.key]));
                          }));
                    return /* () */0;
                  });
        }), ([]));
  return React.createElement("div", undefined, match[0][/* loading */0] ? React.createElement("div", undefined, "Loading...") : null, React.createElement("canvas", {
                  id: "display"
                }), React.createElement("button", {
                  id: "tracing",
                  onClick: (function (param) {
                      return Curry._1(dispatch, /* ToggleTracing */2);
                    })
                }, "Toggle tracing"), React.createElement(RegistersComponent$Yobml.make, {
                  console: $$console
                }));
}

var make = ConsoleComponent;

exports.fetch_rom = fetch_rom;
exports.get_display = get_display;
exports.display = display;
exports.ConsoleFailure = ConsoleFailure;
exports.$$console = $$console;
exports.step = step;
exports.initial_state = initial_state;
exports.handle_key_down = handle_key_down;
exports.handle_key_up = handle_key_up;
exports.make = make;
/* fetch_rom Not a pure module */
