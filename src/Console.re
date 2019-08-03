open Printf

type t = {
  cpu: Cpu.t,
  gpu: Gpu.t,
  memory: Memory.t,
};

let make = () => {
  let file = Node.Fs.readFileSync("./roms/tetris.gb", `binary);
  // let file = Node.Fs.readFileSync("./roms/01-special.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/02-interrupts.gb", `binary); // FAIL: e2 (EI)
  // let file = Node.Fs.readFileSync("./roms/03-op sp,hl.gb", `binary); // FAIL: e8 f8 (ADD SP, r8 and LD HL, SP+r8)
  // let file = Node.Fs.readFileSync("./roms/04-op r,imm.gb", `binary); // FAIL: missing opcodes
  // let file = Node.Fs.readFileSync("./roms/05-op rp.gb", `binary); //PASS
  // let file = Node.Fs.readFileSync("./roms/06-ld r,r.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/07-jr,jp,call,ret,rst.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/08-misc instrs.gb", `binary); // PASS
  // let file = Node.Fs.readFileSync("./roms/09-op r,r.gb", `binary); // FAIL: missing opcodes (ADC...etc)
  let rom = Array.make(String.length(file), 0)
    |> Array.mapi((n, _) => String.get(file, n) |> int_of_char);

  let gpu = Gpu.make(~rom);
  let memory = Memory.make(~rom, ~gpu);
  let cpu = Cpu.make(~memory);

  {
    cpu,
    gpu,
    memory,
  }
};

let run = (console) => {
  let rec loop = (console, steps) => {
    let prev_cy = console.cpu.cycle
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu);
    let gpu = Gpu.step(console.gpu, cpu.cycle - prev_cy)

    let memory = {...cpu.memory, gpu}
    let cpu = {...cpu, memory}
    if (steps < 200000) {
      loop({...console, cpu, gpu, memory}, steps + 1)
    } else {
        CpuExec.trace(cpu, instruction)
        // Js.log(sprintf("%02X", Gpu.load(console.gpu, 0x9949)))
        // Js.log(Js.Array.joinWith("", Array.of_list(cpu.serial |> List.rev)))
        // Array.iter((row) => {
        //   Array.iter((px) => {
        //     //  .:-=+*#%@
        //     let sigil = switch(px) {
        //     | 0 => " " //"."
        //     | 1 => "."//"-"
        //     | 2 => "=" //"o"
        //     | 3 => "@" //"X"
        //     | _ => "?"
        //     }
        //     Printf.printf("%s", sigil)
        //   }, row);
        //   Printf.printf("\n")
        // }, console.gpu.frame);
    }
    // onStep(cpu);
  }

  loop(console, 0)
}
