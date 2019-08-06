// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Utils$Yobml = require("./Utils.bs.js");

function make(cartridge) {
  return /* record */[
          /* mode : HBlank */1,
          /* lcd */128,
          /* control */0,
          /* ly */0,
          /* lyc */0,
          /* cycles */0,
          /* frame */Caml_array.caml_make_vect(92160, 127),
          /* vram */Caml_array.caml_make_vect(8192, 0),
          /* oam */Caml_array.caml_make_vect(160, 0),
          /* cartridge */cartridge,
          /* scroll_x */0,
          /* scroll_y */0,
          /* interrupts */0,
          /* new_frame */false
        ];
}

function load(gpu, address) {
  if (address < 32768) {
    return Curry._1(gpu[/* cartridge */9][/* load */1], address);
  } else if (address >= 32768 && address <= 40959) {
    return Caml_array.caml_array_get(gpu[/* vram */7], address & 8191);
  } else {
    return Pervasives.failwith("Oh no");
  }
}

function store(gpu, address, value) {
  if (address >= 32768 && address <= 40959) {
    return Caml_array.caml_array_set(gpu[/* vram */7], address & 8191, value);
  } else {
    return Pervasives.failwith("Oh no");
  }
}

var color_map = /* array */[
  /* record */[
    /* r */155,
    /* g */188,
    /* b */15
  ],
  /* record */[
    /* r */139,
    /* g */172,
    /* b */15
  ],
  /* record */[
    /* r */48,
    /* g */98,
    /* b */48
  ],
  /* record */[
    /* r */15,
    /* g */56,
    /* b */15
  ]
];

function set_pixel(gpu, x, y, color) {
  var offset = ((Caml_int32.imul(y, 160) + x | 0) << 2);
  Caml_array.caml_array_set(gpu[/* frame */6], offset + 0 | 0, color[/* r */0]);
  Caml_array.caml_array_set(gpu[/* frame */6], offset + 1 | 0, color[/* g */1]);
  Caml_array.caml_array_set(gpu[/* frame */6], offset + 2 | 0, color[/* b */2]);
  return Caml_array.caml_array_set(gpu[/* frame */6], offset + 3 | 0, 255);
}

function render_background(gpu, io_regs) {
  var palette = Caml_array.caml_array_get(io_regs, 71);
  var colors = /* array */[
    palette & 3,
    (palette >>> 2) & 3,
    (palette >>> 4) & 3,
    (palette >>> 6) & 3
  ];
  var ly = gpu[/* ly */3];
  var scroll_x = gpu[/* scroll_x */10];
  var scroll_y = gpu[/* scroll_y */11];
  var ff40 = Caml_array.caml_array_get(io_regs, 64);
  var match = (ff40 & 16) > 0;
  var tile_data = match ? 32768 : 36864;
  var match$1 = (ff40 & 8) > 0;
  var tile_map = match$1 ? 39936 : 38912;
  var y = ((scroll_y + ly | 0) / 8 | 0) % 32;
  var y_offset = (scroll_y + ly | 0) % 8;
  for(var px = 0; px <= 159; ++px){
    var x = ((scroll_x + px | 0) / 8 | 0) % 32 & 65535;
    var tile = load(gpu, (tile_map + (y << 5) | 0) + x & 65535);
    var ptr = tile_data !== 36864 ? tile_data + (tile << 4) & 65535 : tile_data + (Utils$Yobml.signed(tile) << 4) & 65535;
    var ptr$1 = ptr + (y_offset << 1) & 65535;
    var p0 = load(gpu, ptr$1);
    var p1 = load(gpu, ptr$1 + 1 | 0);
    var colb = -((px + scroll_x | 0) % 8 - 7 | 0) | 0;
    var match$2 = ((p1 >>> colb) & 1) === 1;
    var coln = match$2 ? 1 : 0;
    var match$3 = ((p0 >>> colb) & 1) === 1;
    var coln$1 = (coln << 1) | (
      match$3 ? 1 : 0
    );
    var color = Caml_array.caml_array_get(color_map, Caml_array.caml_array_get(colors, coln$1));
    set_pixel(gpu, px, ly, color);
  }
  return /* () */0;
}

function render_sprites(gpu, io_regs) {
  var palette = Caml_array.caml_array_get(io_regs, 73);
  var colors = /* array */[
    0,
    (palette >>> 2) & 3,
    (palette >>> 4) & 3,
    (palette >>> 6) & 3
  ];
  var match = (gpu[/* control */2] & 4) > 0;
  var sprite_height = match ? 16 : 8;
  var ly = gpu[/* ly */3];
  var _n = 0;
  while(true) {
    var n = _n;
    var y = Caml_array.caml_array_get(gpu[/* oam */8], n);
    var x = Caml_array.caml_array_get(gpu[/* oam */8], n + 1 | 0);
    var index = Caml_array.caml_array_get(gpu[/* oam */8], n + 2 | 0);
    var attrs = Caml_array.caml_array_get(gpu[/* oam */8], n + 3 | 0);
    var y$1 = y - 16 | 0;
    var on_scanline = y$1 <= ly && (y$1 + sprite_height | 0) > ly;
    var x$1 = x - 8 | 0;
    if (on_scanline) {
      var match$1 = (attrs & 64) > 0;
      var y_offset = match$1 ? (sprite_height - 1 | 0) - (ly - y$1 | 0) | 0 : ly - y$1 | 0;
      var ptr = (index << 4) + (y_offset << 1) | 0;
      var lo = load(gpu, 32768 + ptr | 0);
      var hi = load(gpu, (32768 + ptr | 0) + 1 | 0);
      for(var idx_x = 0; idx_x <= 7; ++idx_x){
        var pixel_x = x$1 + idx_x | 0;
        if (pixel_x >= 0 && pixel_x <= 160) {
          var match$2 = (attrs & 32) > 0;
          var bit = match$2 ? idx_x : 7 - idx_x | 0;
          var match$3 = ((hi >>> bit) & 1) === 1;
          var pixel = match$3 ? 2 : 0;
          var match$4 = ((lo >>> bit) & 1) === 1;
          var pixel$1 = match$4 ? pixel | 1 : pixel;
          var color = Caml_array.caml_array_get(color_map, Caml_array.caml_array_get(colors, pixel$1));
          if (pixel$1 !== 0) {
            set_pixel(gpu, pixel_x, ly, color);
          }
          
        }
        
      }
    }
    if (n < 156) {
      _n = n + 4 | 0;
      continue ;
    } else {
      return 0;
    }
  };
}

function set_mode(gpu, mode) {
  var cleared = gpu[/* lcd */1] & 252;
  switch (mode) {
    case 0 : 
        return /* record */[
                /* mode : VBlank */0,
                /* lcd */cleared + 1 | 0,
                /* control */gpu[/* control */2],
                /* ly */gpu[/* ly */3],
                /* lyc */gpu[/* lyc */4],
                /* cycles */gpu[/* cycles */5],
                /* frame */gpu[/* frame */6],
                /* vram */gpu[/* vram */7],
                /* oam */gpu[/* oam */8],
                /* cartridge */gpu[/* cartridge */9],
                /* scroll_x */gpu[/* scroll_x */10],
                /* scroll_y */gpu[/* scroll_y */11],
                /* interrupts */gpu[/* interrupts */12],
                /* new_frame */gpu[/* new_frame */13]
              ];
    case 1 : 
        return /* record */[
                /* mode : HBlank */1,
                /* lcd */cleared,
                /* control */gpu[/* control */2],
                /* ly */gpu[/* ly */3],
                /* lyc */gpu[/* lyc */4],
                /* cycles */gpu[/* cycles */5],
                /* frame */gpu[/* frame */6],
                /* vram */gpu[/* vram */7],
                /* oam */gpu[/* oam */8],
                /* cartridge */gpu[/* cartridge */9],
                /* scroll_x */gpu[/* scroll_x */10],
                /* scroll_y */gpu[/* scroll_y */11],
                /* interrupts */gpu[/* interrupts */12],
                /* new_frame */gpu[/* new_frame */13]
              ];
    case 2 : 
        return /* record */[
                /* mode : OamRead */2,
                /* lcd */cleared + 2 | 0,
                /* control */gpu[/* control */2],
                /* ly */gpu[/* ly */3],
                /* lyc */gpu[/* lyc */4],
                /* cycles */gpu[/* cycles */5],
                /* frame */gpu[/* frame */6],
                /* vram */gpu[/* vram */7],
                /* oam */gpu[/* oam */8],
                /* cartridge */gpu[/* cartridge */9],
                /* scroll_x */gpu[/* scroll_x */10],
                /* scroll_y */gpu[/* scroll_y */11],
                /* interrupts */gpu[/* interrupts */12],
                /* new_frame */gpu[/* new_frame */13]
              ];
    case 3 : 
        return /* record */[
                /* mode : LcdTransfer */3,
                /* lcd */cleared + 3 | 0,
                /* control */gpu[/* control */2],
                /* ly */gpu[/* ly */3],
                /* lyc */gpu[/* lyc */4],
                /* cycles */gpu[/* cycles */5],
                /* frame */gpu[/* frame */6],
                /* vram */gpu[/* vram */7],
                /* oam */gpu[/* oam */8],
                /* cartridge */gpu[/* cartridge */9],
                /* scroll_x */gpu[/* scroll_x */10],
                /* scroll_y */gpu[/* scroll_y */11],
                /* interrupts */gpu[/* interrupts */12],
                /* new_frame */gpu[/* new_frame */13]
              ];
    
  }
}

function step(gpu, cycles, lcd_on, io_regs) {
  var cycles$1 = gpu[/* cycles */5] + cycles | 0;
  var gpu$1 = /* record */[
    /* mode */gpu[/* mode */0],
    /* lcd */gpu[/* lcd */1],
    /* control */gpu[/* control */2],
    /* ly */gpu[/* ly */3],
    /* lyc */gpu[/* lyc */4],
    /* cycles */gpu[/* cycles */5],
    /* frame */gpu[/* frame */6],
    /* vram */gpu[/* vram */7],
    /* oam */gpu[/* oam */8],
    /* cartridge */gpu[/* cartridge */9],
    /* scroll_x */gpu[/* scroll_x */10],
    /* scroll_y */gpu[/* scroll_y */11],
    /* interrupts */0,
    /* new_frame */gpu[/* new_frame */13]
  ];
  var match = gpu$1[/* mode */0];
  var gpu$2;
  var exit = 0;
  switch (match) {
    case 0 : 
        if (cycles$1 >= 456) {
          var cycles$2 = cycles$1 - 456 | 0;
          var ly = gpu$1[/* ly */3] + 1 | 0;
          gpu$2 = ly >= 154 ? set_mode(/* record */[
                  /* mode */gpu$1[/* mode */0],
                  /* lcd */gpu$1[/* lcd */1],
                  /* control */gpu$1[/* control */2],
                  /* ly */0,
                  /* lyc */gpu$1[/* lyc */4],
                  /* cycles */cycles$2,
                  /* frame */gpu$1[/* frame */6],
                  /* vram */gpu$1[/* vram */7],
                  /* oam */gpu$1[/* oam */8],
                  /* cartridge */gpu$1[/* cartridge */9],
                  /* scroll_x */gpu$1[/* scroll_x */10],
                  /* scroll_y */gpu$1[/* scroll_y */11],
                  /* interrupts */gpu$1[/* interrupts */12],
                  /* new_frame */gpu$1[/* new_frame */13]
                ], /* OamRead */2) : /* record */[
              /* mode */gpu$1[/* mode */0],
              /* lcd */gpu$1[/* lcd */1],
              /* control */gpu$1[/* control */2],
              /* ly */ly,
              /* lyc */gpu$1[/* lyc */4],
              /* cycles */cycles$2,
              /* frame */gpu$1[/* frame */6],
              /* vram */gpu$1[/* vram */7],
              /* oam */gpu$1[/* oam */8],
              /* cartridge */gpu$1[/* cartridge */9],
              /* scroll_x */gpu$1[/* scroll_x */10],
              /* scroll_y */gpu$1[/* scroll_y */11],
              /* interrupts */gpu$1[/* interrupts */12],
              /* new_frame */gpu$1[/* new_frame */13]
            ];
        } else {
          exit = 1;
        }
        break;
    case 1 : 
        if (cycles$1 >= 204) {
          var cycles$3 = cycles$1 - 204 | 0;
          var ly$1 = gpu$1[/* ly */3] + 1 | 0;
          gpu$2 = ly$1 === 144 ? set_mode(/* record */[
                  /* mode */gpu$1[/* mode */0],
                  /* lcd */gpu$1[/* lcd */1],
                  /* control */gpu$1[/* control */2],
                  /* ly */ly$1,
                  /* lyc */gpu$1[/* lyc */4],
                  /* cycles */cycles$3,
                  /* frame */gpu$1[/* frame */6],
                  /* vram */gpu$1[/* vram */7],
                  /* oam */gpu$1[/* oam */8],
                  /* cartridge */gpu$1[/* cartridge */9],
                  /* scroll_x */gpu$1[/* scroll_x */10],
                  /* scroll_y */gpu$1[/* scroll_y */11],
                  /* interrupts */1,
                  /* new_frame */true
                ], /* VBlank */0) : set_mode(/* record */[
                  /* mode */gpu$1[/* mode */0],
                  /* lcd */gpu$1[/* lcd */1],
                  /* control */gpu$1[/* control */2],
                  /* ly */ly$1,
                  /* lyc */gpu$1[/* lyc */4],
                  /* cycles */cycles$3,
                  /* frame */gpu$1[/* frame */6],
                  /* vram */gpu$1[/* vram */7],
                  /* oam */gpu$1[/* oam */8],
                  /* cartridge */gpu$1[/* cartridge */9],
                  /* scroll_x */gpu$1[/* scroll_x */10],
                  /* scroll_y */gpu$1[/* scroll_y */11],
                  /* interrupts */gpu$1[/* interrupts */12],
                  /* new_frame */gpu$1[/* new_frame */13]
                ], /* OamRead */2);
        } else {
          exit = 1;
        }
        break;
    case 2 : 
        if (cycles$1 >= 80) {
          var cycles$4 = cycles$1 - 80 | 0;
          gpu$2 = set_mode(/* record */[
                /* mode */gpu$1[/* mode */0],
                /* lcd */gpu$1[/* lcd */1],
                /* control */gpu$1[/* control */2],
                /* ly */gpu$1[/* ly */3],
                /* lyc */gpu$1[/* lyc */4],
                /* cycles */cycles$4,
                /* frame */gpu$1[/* frame */6],
                /* vram */gpu$1[/* vram */7],
                /* oam */gpu$1[/* oam */8],
                /* cartridge */gpu$1[/* cartridge */9],
                /* scroll_x */gpu$1[/* scroll_x */10],
                /* scroll_y */gpu$1[/* scroll_y */11],
                /* interrupts */gpu$1[/* interrupts */12],
                /* new_frame */gpu$1[/* new_frame */13]
              ], /* LcdTransfer */3);
        } else {
          exit = 1;
        }
        break;
    case 3 : 
        if (cycles$1 >= 172) {
          var cycles$5 = cycles$1 - 172 | 0;
          if (lcd_on) {
            render_background(gpu$1, io_regs);
            render_sprites(gpu$1, io_regs);
          }
          gpu$2 = set_mode(/* record */[
                /* mode */gpu$1[/* mode */0],
                /* lcd */gpu$1[/* lcd */1],
                /* control */gpu$1[/* control */2],
                /* ly */gpu$1[/* ly */3],
                /* lyc */gpu$1[/* lyc */4],
                /* cycles */cycles$5,
                /* frame */gpu$1[/* frame */6],
                /* vram */gpu$1[/* vram */7],
                /* oam */gpu$1[/* oam */8],
                /* cartridge */gpu$1[/* cartridge */9],
                /* scroll_x */gpu$1[/* scroll_x */10],
                /* scroll_y */gpu$1[/* scroll_y */11],
                /* interrupts */gpu$1[/* interrupts */12],
                /* new_frame */gpu$1[/* new_frame */13]
              ], /* HBlank */1);
        } else {
          exit = 1;
        }
        break;
    
  }
  if (exit === 1) {
    gpu$2 = /* record */[
      /* mode */gpu$1[/* mode */0],
      /* lcd */gpu$1[/* lcd */1],
      /* control */gpu$1[/* control */2],
      /* ly */gpu$1[/* ly */3],
      /* lyc */gpu$1[/* lyc */4],
      /* cycles */cycles$1,
      /* frame */gpu$1[/* frame */6],
      /* vram */gpu$1[/* vram */7],
      /* oam */gpu$1[/* oam */8],
      /* cartridge */gpu$1[/* cartridge */9],
      /* scroll_x */gpu$1[/* scroll_x */10],
      /* scroll_y */gpu$1[/* scroll_y */11],
      /* interrupts */gpu$1[/* interrupts */12],
      /* new_frame */gpu$1[/* new_frame */13]
    ];
  }
  if (gpu$2[/* ly */3] === gpu$2[/* lyc */4]) {
    return /* record */[
            /* mode */gpu$2[/* mode */0],
            /* lcd */Utils$Yobml.set_bit(gpu$2[/* lcd */1], 6),
            /* control */gpu$2[/* control */2],
            /* ly */gpu$2[/* ly */3],
            /* lyc */gpu$2[/* lyc */4],
            /* cycles */gpu$2[/* cycles */5],
            /* frame */gpu$2[/* frame */6],
            /* vram */gpu$2[/* vram */7],
            /* oam */gpu$2[/* oam */8],
            /* cartridge */gpu$2[/* cartridge */9],
            /* scroll_x */gpu$2[/* scroll_x */10],
            /* scroll_y */gpu$2[/* scroll_y */11],
            /* interrupts */gpu$2[/* interrupts */12] | 2,
            /* new_frame */gpu$2[/* new_frame */13]
          ];
  } else {
    return /* record */[
            /* mode */gpu$2[/* mode */0],
            /* lcd */Utils$Yobml.set_bit(gpu$2[/* lcd */1], 6),
            /* control */gpu$2[/* control */2],
            /* ly */gpu$2[/* ly */3],
            /* lyc */gpu$2[/* lyc */4],
            /* cycles */gpu$2[/* cycles */5],
            /* frame */gpu$2[/* frame */6],
            /* vram */gpu$2[/* vram */7],
            /* oam */gpu$2[/* oam */8],
            /* cartridge */gpu$2[/* cartridge */9],
            /* scroll_x */gpu$2[/* scroll_x */10],
            /* scroll_y */gpu$2[/* scroll_y */11],
            /* interrupts */gpu$2[/* interrupts */12],
            /* new_frame */gpu$2[/* new_frame */13]
          ];
  }
}

exports.make = make;
exports.load = load;
exports.store = store;
exports.color_map = color_map;
exports.set_pixel = set_pixel;
exports.render_background = render_background;
exports.render_sprites = render_sprites;
exports.set_mode = set_mode;
exports.step = step;
/* No side effect */
