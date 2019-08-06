open CartridgeHeader

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

let make_mbc1 = (~rom, ~header) => {
  let mode = ref(`Rom)
  let rom_bank = ref(1)
  let ram_bank = ref(0)
  let ram = Array.make(min(header.ram_size, 0x800), 0)

  let load = (address) => switch(address) {
  | address when in_range(0x0000, 0x3FFF, address) => rom[address]
  | address when in_range(0x4000, 0x7FFF, address) => {
    let offset = rom_bank^ * 0x4000
    rom[offset + address land 0x3FFF]
  }
  | address when in_range(0xA000, 0xBFFF, address) => {
    // let offset = ram_bank^ * (header.ram_size)
    // ram[offset + address land (header.ram_size - 1)]
    0
  }
  | _ => failwith(Printf.sprintf("Unhandled rom read $%04X", address))
  };

  let store = (address, value) => switch(address) {
  | address when in_range(0x0000, 0x1FFF, address) =>
    () // RAM Enable. We can ignore it.
  | address when in_range(0x2000, 0x3FFF, address) =>
    rom_bank := (rom_bank^ land 0b1110_0000) lor (value land 0b0001_1111)
  | address when in_range(0x4000, 0x5FFF, address) => switch(mode^) {
    | `Rom => rom_bank := (rom_bank^ land 0b0001_1111) lor ((value == 0 ? 1 : value) lsl 5)
    | `Ram => ram_bank := value land 3
  }
  | address when in_range(0x6000, 0x7FFF, address) =>
    mode := value land 1 == 0 ? `Rom : `Ram
  | address when in_range(0xA000, 0xBFFF, address) => {
    ()
    // let offset = ram_bank^ * (header.ram_size)
    // ram[offset + address land (header.ram_size - 1)] = value
  }
  | _ => failwith(Printf.sprintf("Unhandled rom write $%04X = %02X", address, value))
  };

  {
    header,
    load,
    store
  }
}

let make = (~rom) => {
  let header = CartridgeHeader.make(~rom);
  Js.log(header)
  switch(header.cartridge_type) {
  | RomOnly => make_rom_only(~rom, ~header)
  | MBC1 => make_mbc1(~rom, ~header)
  | _ => failwith("Unimplemented MBC type")
  }
}
