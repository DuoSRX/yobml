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

let interrupt = (cpu: Cpu.t) => {
  if (cpu.ime) {
    let ise = Memory.load(cpu.memory, 0xFFFF)
    let isf = Memory.load(cpu.memory, 0xFF0F)

    if (isf == 0) {
      cpu
    } else {
      // Js.log(sprintf("ISE %02X - ISF %02X", ise, isf))
      // TODO: Refactor this bullshit
      if (ise land 1 > 0 && isf land 1 > 0) { // VBLANK
        // Js.log("VBLANK")
        let sp = Cpu.get_register16(cpu, SP) + 2
        Memory.store16(cpu.memory, sp, cpu.pc)
        Cpu.set_register16(cpu, SP, sp);
        Memory.store(cpu.memory, 0xFF0F, isf land 0x1111_1110);
        {...cpu, pc:0x40, ime:false }
      } else if (ise land 2 > 0 && isf land 2 > 0) { //LCDSTATS
        // Js.log("LCDSTATS")
        let sp = Cpu.get_register16(cpu, SP) + 2
        Memory.store16(cpu.memory, sp, cpu.pc)
        Cpu.set_register16(cpu, SP, sp);
        Memory.store(cpu.memory, 0xFF0F, isf land 0x1111_1101);
        {...cpu, pc:0x48, ime:false }
      } else { // TODO: Timer, Serial, Joypad ints
        cpu
      }
    }
  } else {
    cpu
  }
}

let run = (console) => {
  let rec loop = (console, steps) => {
    let prev_cy = console.cpu.cycle
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu);
    let gpu = Gpu.step(console.gpu, cpu.cycle - prev_cy);

    let gpu = if (gpu.interrupts > 0) {
      let isf = Memory.load(cpu.memory, 0xFF0F);
      Memory.store(cpu.memory, 0xFF0F, isf lor gpu.interrupts);
      {...gpu, interrupts: 0}
    } else { gpu }

    // let cpu = interrupt(cpu)

    if (gpu.new_frame) {
      printf("%s", "\033c")
      Array.iter((row) => {
        Array.iter((px) => {
          //  .:-=+*#%@
          let sigil = switch(px) {
          | 0 => " " //"."
          | 1 => "."//"-"
          | 2 => "=" //"o"
          | 3 => "@" //"X"
          | _ => "?"
          }
          Printf.printf("%s", sigil)
        }, row);
        Printf.printf("\n")
      }, console.gpu.frame);
      gpu.new_frame = false
    }

    let memory = {...cpu.memory, gpu}
    let cpu = {...cpu, memory}

    if (steps < 220000) {
      loop({...console, cpu, gpu, memory}, steps + 1)
    } else {
        CpuExec.trace(cpu, instruction)
        // Js.log(sprintf("%02X", Gpu.load(console.gpu, 0x9949)))
        // Js.log(Js.Array.joinWith("", Array.of_list(cpu.serial |> List.rev)))
        // print_string("\e[3J")
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
