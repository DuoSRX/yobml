// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Memory$Yobml = require("./Memory.bs.js");
var Registers$Yobml = require("./Registers.bs.js");

function tToJs(param) {
  return {
          sp: param[/* sp */0],
          pc: param[/* pc */1],
          cycle: param[/* cycle */2],
          ime: param[/* ime */3],
          registers: param[/* registers */4],
          memory: param[/* memory */5]
        };
}

function tFromJs(param) {
  return /* record */[
          /* sp */param.sp,
          /* pc */param.pc,
          /* cycle */param.cycle,
          /* ime */param.ime,
          /* registers */param.registers,
          /* memory */param.memory
        ];
}

function make(rom) {
  var memory = Memory$Yobml.make(rom);
  var registers = Registers$Yobml.make(/* () */0);
  return /* record */[
          /* sp */65534,
          /* pc */256,
          /* cycle */0,
          /* ime */false,
          /* registers */registers,
          /* memory */memory
        ];
}

function get_register(cpu, register) {
  return Registers$Yobml.get(cpu[/* registers */4], register);
}

function get_register16(cpu, register) {
  return Registers$Yobml.get16(cpu[/* registers */4], register);
}

function set_register(cpu, register, value) {
  return Registers$Yobml.set(cpu[/* registers */4], register, value);
}

function set_register16(cpu, register, value) {
  return Registers$Yobml.set16(cpu[/* registers */4], register, value);
}

function has_flag(cpu, flag) {
  switch (flag) {
    case 0 : 
        return (cpu[/* registers */4][/* f */5] & 128) > 0;
    case 1 : 
        return (cpu[/* registers */4][/* f */5] & 64) > 0;
    case 2 : 
        return (cpu[/* registers */4][/* f */5] & 32) > 0;
    case 3 : 
        return (cpu[/* registers */4][/* f */5] & 16) > 0;
    
  }
}

function set_flag(cpu, flag) {
  switch (flag) {
    case 0 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] | 128;
        return /* () */0;
    case 1 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] | 64;
        return /* () */0;
    case 2 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] | 32;
        return /* () */0;
    case 3 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] | 16;
        return /* () */0;
    
  }
}

function unset_flag(cpu, flag) {
  switch (flag) {
    case 0 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] & Pervasives.lnot(128) & 255;
        return /* () */0;
    case 1 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] & Pervasives.lnot(64) & 255;
        return /* () */0;
    case 2 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] & Pervasives.lnot(32) & 255;
        return /* () */0;
    case 3 : 
        cpu[/* registers */4][/* f */5] = cpu[/* registers */4][/* f */5] & Pervasives.lnot(16) & 255;
        return /* () */0;
    
  }
}

function set_flags(cpu, z, n, h, c, param) {
  var switch_flag = function (value, flag) {
    if (value !== undefined) {
      if (value) {
        return set_flag(cpu, flag);
      } else {
        return unset_flag(cpu, flag);
      }
    } else {
      return /* () */0;
    }
  };
  switch_flag(z, /* Z */0);
  switch_flag(n, /* N */1);
  switch_flag(h, /* H */2);
  return switch_flag(c, /* C */3);
}

function bump_pc(cpu, n) {
  return /* record */[
          /* sp */cpu[/* sp */0],
          /* pc */cpu[/* pc */1] + n | 0,
          /* cycle */cpu[/* cycle */2],
          /* ime */cpu[/* ime */3],
          /* registers */cpu[/* registers */4],
          /* memory */cpu[/* memory */5]
        ];
}

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.make = make;
exports.get_register = get_register;
exports.get_register16 = get_register16;
exports.set_register = set_register;
exports.set_register16 = set_register16;
exports.has_flag = has_flag;
exports.set_flag = set_flag;
exports.unset_flag = unset_flag;
exports.set_flags = set_flags;
exports.bump_pc = bump_pc;
/* No side effect */
