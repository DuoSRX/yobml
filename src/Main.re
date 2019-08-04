// let file = Node.Fs.readFileSync("./roms/tetris.gb", `binary);
// let file = Node.Fs.readFileSync("./roms/01-special.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/02-interrupts.gb", `binary); // FAIL: e2 (EI)
// let file = Node.Fs.readFileSync("./roms/03-op sp,hl.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/04-op r,imm.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/05-op rp.gb", `binary); //PASS
// let file = Node.Fs.readFileSync("./roms/06-ld r,r.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/07-jr,jp,call,ret,rst.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/08-misc instrs.gb", `binary); // PASS
// let file = Node.Fs.readFileSync("./roms/09-op r,r.gb", `binary); // FAIL: missing opcodes (ADC...etc)
let file = Node.Fs.readFileSync("./roms/Dr. Mario (World).gb", `binary);
let rom = Array.make(String.length(file), 0)
  |> Array.mapi((n, _) => String.get(file, n) |> int_of_char);

let console = Console.make(rom);

// Array.iteri((n, byte) => {
//   Memory.store(console.memory, 0xFFB6 + n, byte)
// })

Printf.printf("%02X", Memory.load(console.memory, 0x7FF3))

let () = Console.run(console)
