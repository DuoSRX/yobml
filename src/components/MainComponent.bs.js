// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var ConsoleComponent$Yobml = require("./ConsoleComponent.bs.js");

var roms = /* array */[
  /* tuple */[
    "drmario",
    "Dr Mario"
  ],
  /* tuple */[
    "supermarioland",
    "Super Mario Land"
  ],
  /* tuple */[
    "tetris",
    "Tetris"
  ],
  /* tuple */[
    "pokemon_red",
    "Pokemon Red"
  ],
  /* tuple */[
    "cpu_instrs",
    "Blargg CPU Tests"
  ]
];

function MainComponent(Props) {
  var match = React.useState((function () {
          return "http://localhost:8000/roms/pokemon_red.gb";
        }));
  var setRom = match[1];
  var romURL = match[0];
  var handleChange = function (ev) {
    var rom = ev.target.value;
    if (rom === "") {
      return /* () */0;
    } else {
      return Curry._1(setRom, (function (param) {
                    return "http://localhost:8000/roms/" + (rom + ".gb");
                  }));
    }
  };
  return React.createElement("div", undefined, React.createElement("select", {
                  onChange: handleChange
                }, React.createElement("option", undefined), $$Array.map((function (param) {
                        var name = param[0];
                        return React.createElement("option", {
                                    key: name,
                                    value: name
                                  }, param[1]);
                      }), roms)), romURL !== undefined ? React.createElement(ConsoleComponent$Yobml.make, {
                    romURL: romURL
                  }) : null);
}

var make = MainComponent;

exports.roms = roms;
exports.make = make;
/* react Not a pure module */
