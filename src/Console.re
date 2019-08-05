type t = {
  cpu: Cpu.t,
  gpu: Gpu.t,
  memory: Memory.t,
  input: Input.t
};

let make = (rom) => {
  let gpu = Gpu.make(~rom);
  let input = Input.make();
  let memory = Memory.make(~rom, ~gpu, ~input);
  let cpu = Cpu.make(~memory);

  {
    cpu,
    gpu,
    memory,
    input
  }
};

let interrupt = (cpu: Cpu.t) => {
  cpu.halted = false

  if (cpu.ime) {
    let ise = Memory.load(cpu.memory, 0xFFFF)
    let isf = Memory.load(cpu.memory, 0xFF0F)

    if (isf == 0) {
      cpu
    } else {
      // Js.log(Printf.sprintf("ISE %02X - ISF %02X", ise, isf))
      // TODO: Refactor this bullshit
      if (ise land 1 > 0 && isf land 1 > 0) { // VBLANK
        // Js.log("VBLANK")
        let sp = Cpu.get_register16(cpu, SP) - 2
        Memory.store16(cpu.memory, sp, cpu.pc)
        Cpu.set_register16(cpu, SP, sp);
        Memory.store(cpu.memory, 0xFF0F, isf land 0x1111_1110);
        {...cpu, pc:0x40, ime:false }
      } else if (ise land 2 > 0 && isf land 2 > 0) { //LCDSTATS
        // Js.log("LCDSTATS")
        let sp = Cpu.get_register16(cpu, SP) - 2
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

    let lcd_on = Memory.load(cpu.memory, 0xFF40) land 0x80 > 0;
    let gpu = Gpu.step(console.gpu, cpu.cycle - prev_cy, lcd_on, cpu.memory.io);

    if (gpu.interrupts > 0) {
      let isf = Memory.load(cpu.memory, 0xFF0F);
      Memory.store(cpu.memory, 0xFF0F, isf lor gpu.interrupts);
      gpu.interrupts = 0
    }

    let cpu = interrupt(cpu)

    // if (gpu.new_frame && lcd_on && steps mod 10000 == 0) {
    if (gpu.new_frame && lcd_on) {
      // on_frame(gpu.frame);
      // Js.log("new frame");
      gpu.new_frame = false
    }
    // if (gpu.new_frame && lcd_on && steps mod 10000 == 0) {
      // printf("%s", "\033c")
    //   Array.iter((row) => {
    //     Array.iter((px) => {
    //       //  .:-=+*#%@
    //       let sigil = switch(px) {
    //       | 0 => " " //"."
    //       | 1 => "."//"-"
    //       | 2 => "=" //"o"
    //       | 3 => "@" //"X"
    //       | _ => "?"
    //       }
    //       Printf.printf("%s", sigil)
    //     }, row);
    //     Printf.printf("\n")
    //   }, console.gpu.frame);
    //   gpu.new_frame = false
    // }

    let memory = {...cpu.memory, gpu}
    let cpu = {...cpu, memory}

    if (steps < 4000000) {
      loop({...console, cpu, gpu, memory}, steps + 1)
    } else {
      CpuExec.trace(cpu, instruction)
    }
    // onStep(cpu);
  }

  loop(console, 0)
}

let key_to_button = (key) => {
  open Input;
  switch(key) {
  | "Enter" => Some(Start)
  | " " => Some(Select)
  | "z" => Some(B)
  | "x" => Some(A)
  | "ArrowLeft" => Some(Left)
  | "ArrowRight" => Some(Right)
  | "ArrowDown" => Some(Down)
  | "ArrowUp" => Some(Up)
  | _ => None
  }
}

let key_down = (console, key) => {
  switch(key_to_button(key)) {
  | Some(k) => Input.key_down(console.input, k)
  | None => ()
  };
}

let key_up = (console, key) => {
  switch(key_to_button(key)) {
  | Some(k) => Input.key_up(console.input, k)
  | None => ()
  };
}

let step = (console) => {
  let prev_cy = console.cpu.cycle
  let cpu = if (console.cpu.halted) {
    {...console.cpu, cycle: console.cpu.cycle + 4}
  } else {
    let (cpu, _instruction) = CpuExec.step(~cpu=console.cpu);
    cpu
  }

  let lcd_on = Memory.load(cpu.memory, 0xFF40) land 0x80 > 0;
  let gpu = Gpu.step(console.gpu, cpu.cycle - prev_cy, lcd_on, cpu.memory.io);

  if (gpu.interrupts > 0) {
    let isf = Memory.load(cpu.memory, 0xFF0F);
    Memory.store(cpu.memory, 0xFF0F, isf lor gpu.interrupts);
    gpu.interrupts = 0
  }

  let cpu = interrupt(cpu)

  let memory = {...cpu.memory, gpu};
  let cpu = {...cpu, memory};
  {...console, cpu, memory, gpu}
}
