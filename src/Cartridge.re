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
    rom[offset + (address land 0x3FFF)]
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
    let value = value land 0x1F
    let value = value == 0 ? 1 : value
    rom_bank := (rom_bank^ land 0b0110_0000) lor value;
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

let make_mbc3 = (~rom, ~header) => {
  let rom_bank = ref(1)
  let ram_bank = ref(0)
  let ram = Array.make(max(header.ram_size, 0x800), 0)

  let load = (address) => switch(address) {
  | address when in_range(0x0000, 0x3FFF, address) => rom[address]
  | address when in_range(0x4000, 0x7FFF, address) => {
    let offset = rom_bank^ * 0x4000
    rom[offset + (address land 0x3FFF)]
  }
  | address when in_range(0xA000, 0xBFFF, address) => {
    ram[(ram_bank^ * 0x2000) + address - 0xA000]
  }
  | _ => failwith(Printf.sprintf("Unhandled rom read $%04X", address))
  };

  let store = (address, value) => switch(address) {
  | address when in_range(0x0000, 0x1FFF, address) =>
    () // RAM Enable. We can ignore it.
  | address when in_range(0x2000, 0x3FFF, address) =>
    let value = value land 0x7F
    rom_bank := (value == 0 ? 1 : value)
  | address when in_range(0x4000, 0x5FFF, address) => {
    if (value >= 0 && value <= 0) {
      ram_bank := value land 3
    }
  }
  | address when in_range(0x6000, 0x7FFF, address) =>
    () // TODO: Latch clock data
  | address when in_range(0xA000, 0xBFFF, address) => {
    ram[(ram_bank^ * 0x2000) + address - 0xA000] = value
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
  switch(header.cartridge_type) {
  | RomOnly => make_rom_only(~rom, ~header)
  | MBC1 => make_mbc1(~rom, ~header)
  | MBC3 => make_mbc3(~rom, ~header)
  // | _ => failwith("Unimplemented MBC type")
  }
}
