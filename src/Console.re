type t = {
  cpu: Cpu.t,
  running: bool,
  // breakpoints: list(int)
};

let make = () => {
  // let file = Node.Fs.readFileSync("./roms/tetris.gb", `binary);
  let file = Node.Fs.readFileSync("./roms/01-special.gb", `binary);
  let rom = Array.make(String.length(file), 0)
    |> Array.mapi((n, _) => String.get(file, n) |> int_of_char );

  {
    // cpu: Cpu.make(Tetris.rom),
    cpu: Cpu.make(rom),
    running: true,
    // breakpoints: [0x20C]
  }
};

let run = (console) => {
  let rec loop = (console, steps) => {
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu, ~breakpoints=[]);
    if (steps < 10000000) {
      loop({...console, cpu}, steps + 1);
    } else {
      CpuExec.trace(cpu, instruction)
    }
    // onStep(cpu);
  }

  loop(console, 0)
}
