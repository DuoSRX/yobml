let console = Console.make();

// let payload = [|0x3e,0xc0,0xe0,0x46,0x3e,0x28,0x3d,0x20,0xfd,0xc9|]

// Array.iteri((n, byte) => {
//   Memory.store(console.memory, 0xFFB6 + n, byte)
// })

Printf.printf("%02X", Memory.load(console.memory, 0x7FF3))

let () = Console.run(console)
