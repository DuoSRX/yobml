type t = {
  header: CartridgeHeader.t,
  load : (int) => int,
  store : (int, int) => unit
}

let in_range = (a, b, n) => n >= a && n <= b

let make_rom_only = (~rom, ~header) => {
  let load = (address) => switch(address) {
  | address when in_range(0x0000, 0x7FFF, address) => rom[address]
  | address when in_range(0xA000, 0xBFFF, address) => 0
  | _ => failwith(Printf.sprintf("Unhandled rom read $%04X", address))
  };

  // Can't write to ROM
  let store = (_, _) => {()};

    {
    header,
    load,
    store
    }
}

let make = (~rom) => {
  let header = CartridgeHeader.make(~rom);
  switch(header.cartridge_type) {
  | CartridgeHeader.RomOnly => make_rom_only(~rom, ~header)
  | _ => failwith("Unimplemented MBC type")
  }
}
