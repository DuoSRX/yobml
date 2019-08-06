// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';


function make(param) {
  return /* record */[
          /* cycles */0,
          /* counter */0,
          /* div_counter */0,
          /* div */0,
          /* tima */0,
          /* tma */0,
          /* tac */0
        ];
}

function wrapping_add(a, b) {
  return a + b & 255;
}

function clock_select(timer) {
  if ((timer[/* tac */6] & 4) === 0) {
    return 0;
  } else {
    var match = timer[/* tac */6] & 3;
    switch (match) {
      case 0 : 
          return 1024;
      case 1 : 
          return 16;
      case 2 : 
          return 64;
      default:
        return 256;
    }
  }
}

function tick(timer, cy) {
  timer[/* cycles */0] = timer[/* cycles */0] + cy | 0;
  if (timer[/* cycles */0] >= 4) {
    timer[/* cycles */0] = timer[/* cycles */0] - 4 | 0;
    timer[/* counter */1] = timer[/* counter */1] + 1 | 0;
    timer[/* div_counter */2] = timer[/* div_counter */2] + 1 | 0;
    if (timer[/* div_counter */2] === 16) {
      timer[/* div */3] = wrapping_add(timer[/* div */3], 1);
      timer[/* div_counter */2] = 0;
    }
    
  }
  var cselect = clock_select(timer);
  if (cselect > 0 && timer[/* counter */1] > cselect) {
    timer[/* counter */1] = 0;
    timer[/* tima */4] = wrapping_add(timer[/* tima */4], 1);
    if (timer[/* tima */4] === 0) {
      timer[/* tima */4] = timer[/* tma */5];
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

exports.make = make;
exports.wrapping_add = wrapping_add;
exports.clock_select = clock_select;
exports.tick = tick;
/* No side effect */
