type cartridge_type = RomOnly | MBC1 | MBC2;

// http://gbdev.gg8.se/wiki/articles/The_Cartridge_Header
type t = {
  cartridge_type,
  rom_size: int,
  ram_size: int
};

let get_cartridge_type = (n) => switch(n) {
| 0x00 => RomOnly
| 0x01 | 0x02 | 0x03 => MBC1
| 0x05 | 0x08 | 0x09 => MBC2
| _ => failwith("Unknown cartridge type")
}

let get_rom_size = (n) =>
  0x8000 lsl n

let get_ram_size = (n) => switch(n) {
| 0 => 0
| 1 => 0x800
| 2 => 0x2000
| 3 => 0x8000
| 4 => 0x20000
| _ => failwith("Unhandled ram size")
}

let make = (~rom) => {
  cartridge_type: get_cartridge_type(rom[0x147]),
  rom_size: get_rom_size(rom[0x148]),
  ram_size: get_ram_size(rom[0x149]),
}
