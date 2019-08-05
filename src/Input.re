// http://gbdev.gg8.se/wiki/articles/Joypad_Input

type button =
  | Start
  | Select
  | A
  | B
  | Left
  | Right
  | Down
  | Up

type t = {
  mutable dpad: int,
  mutable buttons: int,
  mutable selector: int,
}

let button_to_bit = (button) => switch(button) {
| A      | Right => 1
| B      | Left  => 2
| Select | Up    => 4
| Start  | Down  => 8
}

let make () = { dpad: 0xF, buttons: 0xF, selector: 0xC0 }

let key_down = (input, key) => {
  switch key {
  | A | B | Select | Start => input.buttons = input.buttons land (lnot(button_to_bit(key)))
  | _ => input.dpad = input.dpad land (lnot(button_to_bit(key)))
  };
}

let key_up = (input, key) => {
  switch key {
  | A | B | Select | Start => input.buttons = input.buttons lor button_to_bit(key)
  | _ => input.dpad = input.dpad lor button_to_bit(key)
  };  ()
}

let get = (input) => {
  if (input.selector land 0x20 > 0) {
    input.dpad lor input.selector
  } else if (input.selector land 0x10 > 0) {
    input.buttons lor input.selector
  } else {
    input.selector
  }
}

let set = (input, value) => {
  input.selector = value lor 0xC0
}
