type t = {
  cpu: Cpu.t,
  running: bool,
  // breakpoints: list(int)
};

let make = () => {
  // let file = Node.Fs.readFileSync("./roms/tetris.gb", `binary);
  // let file = Node.Fs.readFileSync("./roms/01-special.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/02-interrupts.gb", `binary); // FAIL: e2 (EI)
  // let file = Node.Fs.readFileSync("./roms/03-op sp,hl.gb", `binary); // FAIL: e8 f8 (ADD SP, r8 and LD HL, SP+r8)
  // let file = Node.Fs.readFileSync("./roms/04-op r,imm.gb", `binary); // FAIL: missing opcodes
  // let file = Node.Fs.readFileSync("./roms/05-op rp.gb", `binary); //PASS
  // let file = Node.Fs.readFileSync("./roms/06-ld r,r.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/07-jr,jp,call,ret,rst.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/08-misc instrs.gb", `binary); // PASS
  let file = Node.Fs.readFileSync("./roms/09-op r,r.gb", `binary); // FAIL: missing opcodes (ADC...etc)
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
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu);
    if (steps < 10000000) {
      loop({...console, cpu}, steps + 1);
    } else {
        CpuExec.trace(cpu, instruction)
        Js.log(Js.Array.joinWith("", Array.of_list(cpu.serial |> List.rev)))
    }
    // onStep(cpu);
  }

  loop(console, 0)
}
