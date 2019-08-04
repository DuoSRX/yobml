// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Char = require("bs-platform/lib/js/char.js");
var $$String = require("bs-platform/lib/js/string.js");
var Cpu$Yobml = require("./Cpu.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Memory$Yobml = require("./Memory.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var FooEx = Caml_exceptions.create("InstructionsExec-Yobml.FooEx");

function wrapping_add(a, b) {
  return a + b & 255;
}

function wrapping_add16(a, b) {
  return a + b & 65535;
}

function signed(v) {
  var match = v > 127;
  if (match) {
    return -(Pervasives.lnot(v) + 1 & 255) | 0;
  } else {
    return v;
  }
}

function load(cpu, address) {
  return Memory$Yobml.load(cpu[/* memory */4], address);
}

function load16(cpu, address) {
  return Memory$Yobml.load16(cpu[/* memory */4], address);
}

function store(cpu, address, value) {
  if (address === 65281) {
    cpu[/* serial */5] = /* :: */[
      $$String.make(1, Char.chr(value)),
      cpu[/* serial */5]
    ];
  }
  return Memory$Yobml.store(cpu[/* memory */4], address, value);
}

function store16(cpu, address, value) {
  return Memory$Yobml.store16(cpu[/* memory */4], address, value);
}

function load_next(cpu) {
  return Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
}

function load_next16(cpu) {
  return Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
}

function storage_load(cpu, storage) {
  switch (storage.tag | 0) {
    case 0 : 
        return Cpu$Yobml.get_register(cpu, storage[0]);
    case 1 : 
        return Cpu$Yobml.get_register16(cpu, storage[0]);
    case 2 : 
        var address = Cpu$Yobml.get_register16(cpu, storage[0]);
        return Memory$Yobml.load(cpu[/* memory */4], address);
    
  }
}

function storage_store(cpu, storage, value) {
  switch (storage.tag | 0) {
    case 0 : 
        return Cpu$Yobml.set_register(cpu, storage[0], value);
    case 1 : 
        return Cpu$Yobml.set_register16(cpu, storage[0], value);
    case 2 : 
        return store(cpu, Cpu$Yobml.get_register16(cpu, storage[0]), value);
    
  }
}

function bump(cpu, pc, cycles) {
  return /* record */[
          /* pc */pc,
          /* cycle */cpu[/* cycle */1] + cycles | 0,
          /* ime */cpu[/* ime */2],
          /* registers */cpu[/* registers */3],
          /* memory */cpu[/* memory */4],
          /* serial */cpu[/* serial */5]
        ];
}

function nop(cpu) {
  return bump(cpu, cpu[/* pc */0], 4);
}

function call(cpu) {
  var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
  var sp = wrapping_add16(Cpu$Yobml.get_register16(cpu, /* SP */4), -2);
  store16(cpu, sp, cpu[/* pc */0] + 2 | 0);
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  return bump(cpu, address, 24);
}

function call_cond(cpu, flag, condition) {
  if (Cpu$Yobml.has_flag(cpu, flag) === condition) {
    return call(cpu);
  } else {
    return bump(cpu, cpu[/* pc */0] + 2 | 0, 12);
  }
}

function ret(cpu) {
  var sp = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var pc = Memory$Yobml.load16(cpu[/* memory */4], sp);
  Cpu$Yobml.set_register16(cpu, /* SP */4, wrapping_add16(sp, 2));
  return bump(cpu, pc, 16);
}

function reti(cpu) {
  var sp = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var pc = Memory$Yobml.load16(cpu[/* memory */4], sp);
  Cpu$Yobml.set_register16(cpu, /* SP */4, wrapping_add16(sp, 2));
  return bump(/* record */[
              /* pc */cpu[/* pc */0],
              /* cycle */cpu[/* cycle */1],
              /* ime */true,
              /* registers */cpu[/* registers */3],
              /* memory */cpu[/* memory */4],
              /* serial */cpu[/* serial */5]
            ], pc, 16);
}

function ret_cond(cpu, flag, condition) {
  if (Cpu$Yobml.has_flag(cpu, flag) === condition) {
    var sp = Cpu$Yobml.get_register16(cpu, /* SP */4);
    var pc = Memory$Yobml.load16(cpu[/* memory */4], sp);
    Cpu$Yobml.set_register16(cpu, /* SP */4, wrapping_add16(sp, 2));
    return bump(cpu, pc, 20);
  } else {
    return bump(cpu, cpu[/* pc */0], 8);
  }
}

function add(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var result = a + b & 255;
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  var h = (result & 15) < (b & 15);
  var c = result < b;
  Cpu$Yobml.set_flags(cpu, result === 0, false, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function and_(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var result = a & b;
  Cpu$Yobml.set_register(cpu, r, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, true, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function and_hl(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var b = Memory$Yobml.load(cpu[/* memory */4], address);
  var result = a & b;
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, true, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function bit(cpu, bit$1, r) {
  var a = Cpu$Yobml.get_register(cpu, r);
  var z = ((a >>> bit$1) & 1) === 1;
  Cpu$Yobml.set_flags(cpu, z, false, true, undefined, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function ccf(cpu) {
  Cpu$Yobml.set_flags(cpu, undefined, false, false, !Cpu$Yobml.has_flag(cpu, /* C */3), /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function cp(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var h = (a & 15) > (b & 15);
  Cpu$Yobml.set_flags(cpu, a === b, true, h, a < b, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function cp_n(cpu) {
  var reg = Cpu$Yobml.get_register(cpu, /* A */0);
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var h = ($$byte & 15) > (reg & 15);
  Cpu$Yobml.set_flags(cpu, reg === $$byte, true, h, reg < $$byte, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function cpl(cpu) {
  Cpu$Yobml.set_register(cpu, /* A */0, Cpu$Yobml.get_register(cpu, /* A */0) ^ 255);
  Cpu$Yobml.set_flags(cpu, undefined, true, true, undefined, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

var NoDAA = Caml_exceptions.create("InstructionsExec-Yobml.NoDAA");

function daa(cpu) {
  return bump(cpu, cpu[/* pc */0], 4);
}

function di(cpu) {
  return bump(/* record */[
              /* pc */cpu[/* pc */0],
              /* cycle */cpu[/* cycle */1],
              /* ime */false,
              /* registers */cpu[/* registers */3],
              /* memory */cpu[/* memory */4],
              /* serial */cpu[/* serial */5]
            ], cpu[/* pc */0], 4);
}

function ei(cpu) {
  store(cpu, 65295, Memory$Yobml.load(cpu[/* memory */4], 65535));
  return bump(/* record */[
              /* pc */cpu[/* pc */0],
              /* cycle */cpu[/* cycle */1],
              /* ime */true,
              /* registers */cpu[/* registers */3],
              /* memory */cpu[/* memory */4],
              /* serial */cpu[/* serial */5]
            ], cpu[/* pc */0], 4);
}

function ld_rr(cpu, r1, r2) {
  Cpu$Yobml.set_register(cpu, r1, Cpu$Yobml.get_register(cpu, r2));
  return bump(cpu, cpu[/* pc */0], 4);
}

function ld_n(cpu, r) {
  Cpu$Yobml.set_register(cpu, r, Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]));
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function ld_nn(cpu, r16) {
  Cpu$Yobml.set_register16(cpu, r16, Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]));
  return bump(cpu, cpu[/* pc */0] + 2 | 0, 12);
}

function ld_hl_d8(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  store(cpu, address, Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]));
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 12);
}

function ld_hl_sp_e8(cpu) {
  var a = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var b = signed(Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]));
  var value = wrapping_add16(a, b);
  var h = ((a & 15) + (b & 15) | 0) > 15;
  var c = ((a & 255) + (b & 255) | 0) > 255;
  Cpu$Yobml.set_flags(cpu, false, false, h, c, /* () */0);
  Cpu$Yobml.set_register16(cpu, /* HL */3, value);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 12);
}

function ld_r16_a(cpu, r16) {
  var address = Cpu$Yobml.get_register16(cpu, r16);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_a_r16(cpu, r16) {
  var address = Cpu$Yobml.get_register16(cpu, r16);
  Cpu$Yobml.set_register(cpu, /* A */0, Memory$Yobml.load(cpu[/* memory */4], address));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_a_a16(cpu) {
  var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], address);
  Cpu$Yobml.set_register(cpu, /* A */0, $$byte);
  return bump(cpu, cpu[/* pc */0] + 2 | 0, 16);
}

function ldi_hl_a(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  Cpu$Yobml.set_register16(cpu, /* HL */3, wrapping_add16(address, 1));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ldi_a_hl(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  Cpu$Yobml.set_register(cpu, /* A */0, Memory$Yobml.load(cpu[/* memory */4], address));
  Cpu$Yobml.set_register16(cpu, /* HL */3, wrapping_add16(address, 1));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ldd_hl_a(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  Cpu$Yobml.set_register16(cpu, /* HL */3, wrapping_add16(address, -1));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_hl_r(cpu, r) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  store(cpu, address, Cpu$Yobml.get_register(cpu, r));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_r_hl(cpu, r) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  Cpu$Yobml.set_register(cpu, r, Memory$Yobml.load(cpu[/* memory */4], address));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_sp_hl(cpu) {
  Cpu$Yobml.set_register16(cpu, /* SP */4, Cpu$Yobml.get_register16(cpu, /* HL */3));
  return bump(cpu, cpu[/* pc */0], 8);
}

function ld_read_io_n(cpu) {
  var n = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var address = wrapping_add16(65280, n);
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], address);
  Cpu$Yobml.set_register(cpu, /* A */0, $$byte);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 12);
}

function ld_write_io_n(cpu) {
  var n = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var address = wrapping_add16(65280, n);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 12);
}

function ld_read_io_c(cpu) {
  var n = Cpu$Yobml.get_register(cpu, /* C */2);
  var address = wrapping_add16(65280, n);
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], address);
  Cpu$Yobml.set_register(cpu, /* A */0, $$byte);
  return bump(cpu, cpu[/* pc */0], 12);
}

function ld_write_io_c(cpu) {
  var n = Cpu$Yobml.get_register(cpu, /* C */2);
  var address = wrapping_add16(65280, n);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  return bump(cpu, cpu[/* pc */0], 12);
}

function ld_sp(cpu) {
  Cpu$Yobml.set_register16(cpu, /* SP */4, Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]));
  return bump(cpu, cpu[/* pc */0] + 2 | 0, 12);
}

function ld_a16_a(cpu) {
  var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
  store(cpu, address, Cpu$Yobml.get_register(cpu, /* A */0));
  return bump(cpu, cpu[/* pc */0] + 2 | 0, 16);
}

function ld_a16_sp(cpu) {
  var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
  store16(cpu, address, Cpu$Yobml.get_register16(cpu, /* SP */4));
  return bump(cpu, cpu[/* pc */0] + 2 | 0, 20);
}

function inc(cpu, r) {
  var reg = Cpu$Yobml.get_register(cpu, r);
  var value = wrapping_add(reg, 1);
  Cpu$Yobml.set_register(cpu, r, value);
  var h = (value & 15) < (reg & 15);
  Cpu$Yobml.set_flags(cpu, value === 0, false, h, undefined, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function inc16(cpu, r) {
  var value = Cpu$Yobml.get_register16(cpu, r);
  Cpu$Yobml.set_register16(cpu, r, wrapping_add16(value, 1));
  return bump(cpu, cpu[/* pc */0], 8);
}

function inc_hl(cpu) {
  var hl = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var value = wrapping_add(Memory$Yobml.load(cpu[/* memory */4], hl), 1);
  Cpu$Yobml.set_flags(cpu, value === 0, false, (value & 15) === 0, undefined, /* () */0);
  store(cpu, hl, value);
  return bump(cpu, cpu[/* pc */0], 12);
}

function dec(cpu, r) {
  var reg = Cpu$Yobml.get_register(cpu, r);
  var value = wrapping_add(reg, -1);
  Cpu$Yobml.set_register(cpu, r, value);
  var h = (value & 15) > (reg & 15);
  Cpu$Yobml.set_flags(cpu, value === 0, true, h, undefined, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function dec16(cpu, r) {
  var reg = Cpu$Yobml.get_register16(cpu, r);
  var value = wrapping_add16(reg, -1);
  Cpu$Yobml.set_register16(cpu, r, value);
  return bump(cpu, cpu[/* pc */0], 8);
}

function dec_hl(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var a = Memory$Yobml.load(cpu[/* memory */4], address);
  var value = wrapping_add16(a, -1);
  store(cpu, address, value);
  return bump(cpu, cpu[/* pc */0], 8);
}

function adc_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var match = Cpu$Yobml.has_flag(cpu, /* C */3);
  var carry = match ? 1 : 0;
  var result = (a + b | 0) + carry | 0;
  Cpu$Yobml.set_register(cpu, /* A */0, result & 255);
  var c = result > 255;
  var h = (((a & 15) + (b & 15) | 0) + carry | 0) > 15;
  Cpu$Yobml.set_flags(cpu, result === 0, false, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function add_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var value = wrapping_add(a, b);
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  var h = (value & 15) < (b & 15);
  var c = value < b;
  Cpu$Yobml.set_flags(cpu, value === 0, false, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function add_hl_r16(cpu, r) {
  var hl = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var a = Cpu$Yobml.get_register16(cpu, r);
  var result = hl + a | 0;
  var h = ((hl & 4095) + (a & 4095) | 0) > 4095;
  Cpu$Yobml.set_register16(cpu, /* HL */3, result & 65535);
  Cpu$Yobml.set_flags(cpu, undefined, false, h, result > 65535, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function add_sp_e8(cpu) {
  var a = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var b = signed(Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]));
  var h = ((a & 15) + (b & 15) | 0) > 15;
  var c = ((a & 255) + (b & 255) | 0) > 255;
  Cpu$Yobml.set_flags(cpu, false, false, h, c, /* () */0);
  var sp = wrapping_add16(a, b);
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 16);
}

function sub_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var value = wrapping_add(a, -b | 0);
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  var h = (value & 15) > (a & 15);
  var c = value > a;
  Cpu$Yobml.set_flags(cpu, a === b, true, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function jp(cpu) {
  var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
  return bump(cpu, address, 16);
}

function jp_cond(cpu, flag, condition) {
  if (Cpu$Yobml.has_flag(cpu, flag) === condition) {
    var address = Memory$Yobml.load16(cpu[/* memory */4], cpu[/* pc */0]);
    return bump(cpu, address, 16);
  } else {
    return bump(cpu, cpu[/* pc */0] + 2 | 0, 12);
  }
}

function jp_hl(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  return bump(cpu, address, 4);
}

function jr_e8(cpu) {
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var offset = $$byte > 127 ? -(Pervasives.lnot($$byte) + 1 & 255) | 0 : $$byte;
  return bump(cpu, (cpu[/* pc */0] + offset | 0) + 1 | 0, 12);
}

function jr(cpu, flag, condition) {
  var do_jump = Cpu$Yobml.has_flag(cpu, flag) === condition;
  if (do_jump) {
    var $$byte = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
    var offset = $$byte > 127 ? -(Pervasives.lnot($$byte) + 1 & 255) | 0 : $$byte;
    return bump(cpu, (cpu[/* pc */0] + offset | 0) + 1 | 0, 12);
  } else {
    return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
  }
}

function and_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var value = a & b;
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  Cpu$Yobml.set_flags(cpu, value === 0, false, true, false, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 4);
}

function ora(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var value = a | b;
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  Cpu$Yobml.set_flags(cpu, value === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function or_hl(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var b = Memory$Yobml.load(cpu[/* memory */4], address);
  var value = a | b;
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  Cpu$Yobml.set_flags(cpu, value === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function push16(cpu, r16) {
  var sp = wrapping_add16(Cpu$Yobml.get_register16(cpu, /* SP */4), -2);
  store16(cpu, sp, Cpu$Yobml.get_register16(cpu, r16));
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  return bump(cpu, cpu[/* pc */0], 16);
}

function pop16(cpu, r16) {
  var address = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var value = Memory$Yobml.load16(cpu[/* memory */4], address);
  var sp = wrapping_add16(Cpu$Yobml.get_register16(cpu, /* SP */4), 2);
  Cpu$Yobml.set_register16(cpu, r16, value);
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  return bump(cpu, cpu[/* pc */0], 12);
}

function pop_af(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* SP */4);
  var af = Memory$Yobml.load16(cpu[/* memory */4], address) & 65520;
  var sp = wrapping_add16(Cpu$Yobml.get_register16(cpu, /* SP */4), 2);
  Cpu$Yobml.set_register16(cpu, /* AF */0, af);
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  Cpu$Yobml.set_flags(cpu, (af & 128) > 0, (af & 64) > 0, (af & 32) > 0, (af & 16) > 0, /* () */0);
  return bump(cpu, cpu[/* pc */0], 12);
}

function res(cpu, bit, r) {
  var reg = Cpu$Yobml.get_register(cpu, r);
  Cpu$Yobml.set_register(cpu, r, reg & Pervasives.lnot((1 << bit)));
  return bump(cpu, cpu[/* pc */0], 8);
}

function res_hl(cpu, bit) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var $$byte = Memory$Yobml.load(cpu[/* memory */4], address);
  store(cpu, address, $$byte & Pervasives.lnot((1 << bit)));
  return bump(cpu, cpu[/* pc */0], 16);
}

function rlca(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var c = (a & 128) > 0;
  var a$1 = (a << 1);
  var a$2 = c ? a$1 | 1 : a$1;
  Cpu$Yobml.set_flags(cpu, false, false, false, c, /* () */0);
  Cpu$Yobml.set_register(cpu, /* A */0, a$2);
  return bump(cpu, cpu[/* pc */0], 4);
}

function rr(cpu, storage) {
  var a = storage_load(cpu, storage);
  var match = Cpu$Yobml.has_flag(cpu, /* C */3);
  var carry = match ? 1 : 0;
  var c = (a & 1) === 1;
  var value = (carry << 7) | (a >>> 1);
  storage_store(cpu, storage, value);
  Cpu$Yobml.set_flags(cpu, value === 0, false, false, c, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function rra(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var match = Cpu$Yobml.has_flag(cpu, /* C */3);
  var carry = match ? 1 : 0;
  var c = (a & 1) === 1;
  var value = (carry << 7) | (a >>> 1);
  Cpu$Yobml.set_register(cpu, /* A */0, value);
  Cpu$Yobml.set_flags(cpu, false, false, false, c, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function rst(cpu, n) {
  var sp = wrapping_add16(Cpu$Yobml.get_register16(cpu, /* SP */4), -2);
  store16(cpu, sp, cpu[/* pc */0]);
  Cpu$Yobml.set_register16(cpu, /* SP */4, sp);
  return bump(cpu, n, 16);
}

function sbc(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var match = Cpu$Yobml.has_flag(cpu, /* C */3);
  var carry = match ? 1 : 0;
  var result = (a - b | 0) - carry | 0;
  Cpu$Yobml.set_register(cpu, /* A */0, result & 255);
  var c = result < 0;
  var h = (((a & 15) - (b & 15) | 0) - carry | 0) < 0;
  var z = (result & 255) === 0;
  Cpu$Yobml.set_flags(cpu, z, true, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function sbc_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var match = Cpu$Yobml.has_flag(cpu, /* C */3);
  var carry = match ? 1 : 0;
  var result = (a - b | 0) - carry | 0;
  Cpu$Yobml.set_register(cpu, /* A */0, result & 255);
  var c = result < 0;
  var h = (((a & 15) - (b & 15) | 0) - carry | 0) < 0;
  var z = (result & 255) === 0;
  Cpu$Yobml.set_flags(cpu, z, true, h, c, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function scf(cpu) {
  Cpu$Yobml.set_flags(cpu, undefined, false, false, true, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function srl(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, r);
  var result = (a >>> 1);
  Cpu$Yobml.set_register(cpu, r, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, (a & 1) === 1, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function srl_hl(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var a = Memory$Yobml.load(cpu[/* memory */4], address);
  var result = (a >>> 1);
  store(cpu, a, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, (a & 1) === 1, /* () */0);
  return bump(cpu, cpu[/* pc */0], 16);
}

function sub_r8(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var result = wrapping_add(a, -b | 0);
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  var h = (result & 15) > (a & 15);
  Cpu$Yobml.set_flags(cpu, a === b, true, h, result > a, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function swap(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, r);
  var lo = ((a & 15) << 4);
  var hi = ((a & 240) >>> 4);
  var result = lo | hi;
  Cpu$Yobml.set_register(cpu, r, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function swap_hl(cpu) {
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var a = Memory$Yobml.load(cpu[/* memory */4], address);
  var lo = ((a & 15) << 4);
  var hi = ((a & 240) >>> 4);
  var result = lo | hi;
  store(cpu, address, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 16);
}

function xor(cpu, r) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Cpu$Yobml.get_register(cpu, r);
  var result = a ^ b;
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 4);
}

function xor_d8(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var b = Memory$Yobml.load(cpu[/* memory */4], cpu[/* pc */0]);
  var result = a ^ b;
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0] + 1 | 0, 8);
}

function xor_hl(cpu) {
  var a = Cpu$Yobml.get_register(cpu, /* A */0);
  var address = Cpu$Yobml.get_register16(cpu, /* HL */3);
  var b = Memory$Yobml.load(cpu[/* memory */4], address);
  var result = a ^ b;
  Cpu$Yobml.set_register(cpu, /* A */0, result);
  Cpu$Yobml.set_flags(cpu, result === 0, false, false, false, /* () */0);
  return bump(cpu, cpu[/* pc */0], 8);
}

function execute(cpu, instruction) {
  if (typeof instruction === "number") {
    switch (instruction) {
      case 0 : 
          return adc_d8(cpu);
      case 1 : 
          return add_d8(cpu);
      case 2 : 
          return add_sp_e8(cpu);
      case 3 : 
          return and_d8(cpu);
      case 4 : 
          return and_hl(cpu);
      case 5 : 
          return call(cpu);
      case 6 : 
          return ccf(cpu);
      case 7 : 
          return cp_n(cpu);
      case 8 : 
          return cpl(cpu);
      case 9 : 
          return daa(cpu);
      case 10 : 
          return dec_hl(cpu);
      case 11 : 
          return di(cpu);
      case 12 : 
          return ei(cpu);
      case 13 : 
          return inc_hl(cpu);
      case 14 : 
          return jp(cpu);
      case 15 : 
          return jp_hl(cpu);
      case 16 : 
          return jr_e8(cpu);
      case 17 : 
          return ld_sp(cpu);
      case 18 : 
          return ld_read_io_n(cpu);
      case 19 : 
          return ld_write_io_n(cpu);
      case 20 : 
          return ld_read_io_c(cpu);
      case 21 : 
          return ld_write_io_c(cpu);
      case 22 : 
          return ldd_hl_a(cpu);
      case 23 : 
          return ldi_a_hl(cpu);
      case 24 : 
          return ldi_hl_a(cpu);
      case 25 : 
          return ld_a16_a(cpu);
      case 26 : 
          return ld_a16_sp(cpu);
      case 27 : 
          return ld_sp_hl(cpu);
      case 28 : 
          return ld_a_a16(cpu);
      case 29 : 
          return ld_hl_d8(cpu);
      case 30 : 
          return ld_hl_sp_e8(cpu);
      case 31 : 
          return or_hl(cpu);
      case 32 : 
          return cpu;
      case 33 : 
          return pop_af(cpu);
      case 34 : 
          return ret(cpu);
      case 35 : 
          return reti(cpu);
      case 36 : 
          return rlca(cpu);
      case 37 : 
          return rra(cpu);
      case 38 : 
          return sbc_d8(cpu);
      case 39 : 
          return scf(cpu);
      case 40 : 
          return srl_hl(cpu);
      case 41 : 
          return sub_d8(cpu);
      case 42 : 
          return swap_hl(cpu);
      case 43 : 
          return xor_d8(cpu);
      case 44 : 
          return xor_hl(cpu);
      
    }
  } else {
    switch (instruction.tag | 0) {
      case 0 : 
          return add(cpu, instruction[0]);
      case 1 : 
          return add_hl_r16(cpu, instruction[0]);
      case 2 : 
          return and_(cpu, instruction[0]);
      case 3 : 
          return bit(cpu, instruction[0], instruction[1]);
      case 4 : 
          return call_cond(cpu, instruction[0], instruction[1]);
      case 5 : 
          return cp(cpu, instruction[0]);
      case 6 : 
          return dec(cpu, instruction[0]);
      case 7 : 
          return dec16(cpu, instruction[0]);
      case 8 : 
          return inc(cpu, instruction[0]);
      case 9 : 
          return inc16(cpu, instruction[0]);
      case 10 : 
          return jp_cond(cpu, instruction[0], instruction[1]);
      case 11 : 
          return jr(cpu, instruction[0], instruction[1]);
      case 12 : 
          return ld_r16_a(cpu, instruction[0]);
      case 13 : 
          return ld_rr(cpu, instruction[0], instruction[1]);
      case 14 : 
          return ld_hl_r(cpu, instruction[0]);
      case 15 : 
          return ld_r_hl(cpu, instruction[0]);
      case 16 : 
          return ld_a_r16(cpu, instruction[0]);
      case 17 : 
          return ld_n(cpu, instruction[0]);
      case 18 : 
          return ld_nn(cpu, instruction[0]);
      case 19 : 
          return ora(cpu, instruction[0]);
      case 20 : 
          return pop16(cpu, instruction[0]);
      case 21 : 
          return push16(cpu, instruction[0]);
      case 22 : 
          return res(cpu, instruction[0], instruction[1]);
      case 23 : 
          return res_hl(cpu, instruction[0]);
      case 24 : 
          return ret_cond(cpu, instruction[0], instruction[1]);
      case 25 : 
          return rr(cpu, instruction[0]);
      case 26 : 
          return rst(cpu, instruction[0]);
      case 27 : 
          return sbc(cpu, instruction[0]);
      case 28 : 
          return srl(cpu, instruction[0]);
      case 29 : 
          return sub_r8(cpu, instruction[0]);
      case 30 : 
          return swap(cpu, instruction[0]);
      case 31 : 
          return xor(cpu, instruction[0]);
      
    }
  }
}

exports.FooEx = FooEx;
exports.wrapping_add = wrapping_add;
exports.wrapping_add16 = wrapping_add16;
exports.signed = signed;
exports.load = load;
exports.load16 = load16;
exports.store = store;
exports.store16 = store16;
exports.load_next = load_next;
exports.load_next16 = load_next16;
exports.storage_load = storage_load;
exports.storage_store = storage_store;
exports.bump = bump;
exports.nop = nop;
exports.call = call;
exports.call_cond = call_cond;
exports.ret = ret;
exports.reti = reti;
exports.ret_cond = ret_cond;
exports.add = add;
exports.and_ = and_;
exports.and_hl = and_hl;
exports.bit = bit;
exports.ccf = ccf;
exports.cp = cp;
exports.cp_n = cp_n;
exports.cpl = cpl;
exports.NoDAA = NoDAA;
exports.daa = daa;
exports.di = di;
exports.ei = ei;
exports.ld_rr = ld_rr;
exports.ld_n = ld_n;
exports.ld_nn = ld_nn;
exports.ld_hl_d8 = ld_hl_d8;
exports.ld_hl_sp_e8 = ld_hl_sp_e8;
exports.ld_r16_a = ld_r16_a;
exports.ld_a_r16 = ld_a_r16;
exports.ld_a_a16 = ld_a_a16;
exports.ldi_hl_a = ldi_hl_a;
exports.ldi_a_hl = ldi_a_hl;
exports.ldd_hl_a = ldd_hl_a;
exports.ld_hl_r = ld_hl_r;
exports.ld_r_hl = ld_r_hl;
exports.ld_sp_hl = ld_sp_hl;
exports.ld_read_io_n = ld_read_io_n;
exports.ld_write_io_n = ld_write_io_n;
exports.ld_read_io_c = ld_read_io_c;
exports.ld_write_io_c = ld_write_io_c;
exports.ld_sp = ld_sp;
exports.ld_a16_a = ld_a16_a;
exports.ld_a16_sp = ld_a16_sp;
exports.inc = inc;
exports.inc16 = inc16;
exports.inc_hl = inc_hl;
exports.dec = dec;
exports.dec16 = dec16;
exports.dec_hl = dec_hl;
exports.adc_d8 = adc_d8;
exports.add_d8 = add_d8;
exports.add_hl_r16 = add_hl_r16;
exports.add_sp_e8 = add_sp_e8;
exports.sub_d8 = sub_d8;
exports.jp = jp;
exports.jp_cond = jp_cond;
exports.jp_hl = jp_hl;
exports.jr_e8 = jr_e8;
exports.jr = jr;
exports.and_d8 = and_d8;
exports.ora = ora;
exports.or_hl = or_hl;
exports.push16 = push16;
exports.pop16 = pop16;
exports.pop_af = pop_af;
exports.res = res;
exports.res_hl = res_hl;
exports.rlca = rlca;
exports.rr = rr;
exports.rra = rra;
exports.rst = rst;
exports.sbc = sbc;
exports.sbc_d8 = sbc_d8;
exports.scf = scf;
exports.srl = srl;
exports.srl_hl = srl_hl;
exports.sub_r8 = sub_r8;
exports.swap = swap;
exports.swap_hl = swap_hl;
exports.xor = xor;
exports.xor_d8 = xor_d8;
exports.xor_hl = xor_hl;
exports.execute = execute;
/* No side effect */
