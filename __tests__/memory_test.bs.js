// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Memory$Yobml = require("../src/Memory.bs.js");

Jest.describe("load16", (function (param) {
        return Jest.test("Load two bytes", (function (param) {
                      var mem = Memory$Yobml.make(/* array */[
                            52,
                            18
                          ]);
                      return Jest.Expect[/* toBe */2](4660, Jest.Expect[/* expect */0](Memory$Yobml.load16(mem, 0)));
                    }));
      }));

Jest.describe("store16", (function (param) {
        return Jest.test("Stores two bytes", (function (param) {
                      var mem = Memory$Yobml.make(/* array */[
                            0,
                            0
                          ]);
                      Memory$Yobml.store16(mem, 0, 4660);
                      return Jest.Expect[/* toEqual */12](/* tuple */[
                                  52,
                                  18
                                ], Jest.Expect[/* expect */0](/* tuple */[
                                      Caml_array.caml_array_get(mem[/* rom */0], 0),
                                      Caml_array.caml_array_get(mem[/* rom */0], 1)
                                    ]));
                    }));
      }));

/*  Not a pure module */