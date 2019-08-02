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
    cpu: Cpu.make(rom),
    running: true,
    // breakpoints: [0x20C]
  }
};

let run = (console) => {
  let rec loop = (console, steps) => {
    // let (cpu, instruction) = CpuExec.step(~cpu=console.cpu, ~breakpoints=[]);
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu);
    // if (steps < 10000000000 && cpu.pc != 0xC068) {
    // if (steps < 10000000 || cpu.pc != 0xC7D2) {
    if (steps < 100000000) {
      loop({...console, cpu}, steps + 1);
    } else {
        CpuExec.trace(cpu, instruction)
        Js.log(cpu.serial)
        Js.log(cpu.memory.io)
    }
    // onStep(cpu);
  }

  loop(console, 0)
}
