open Utils

type t = {
  cpu: Cpu.t,
  gpu: Gpu.t,
  memory: Memory.t,
  input: Input.t,
  timer: Timer.t,
  mutable tracing: bool
};

let make = (rom) => {
  let gpu = Gpu.make(~rom);
  let input = Input.make();
  let timer = Timer.make();
  let memory = Memory.make(~rom, ~gpu, ~input, ~timer);
  let cpu = Cpu.make(~memory);

  {
    cpu,
    gpu,
    memory,
    input,
    timer,
    tracing: false
  }
};

let request_interrupt = (cpu:Cpu.t, n) => {
  let isf = Memory.load(cpu.memory, 0xFF0F);
  Memory.store(cpu.memory, 0xFF0F, isf lor n)
}

let is_interrupt_enabled = (cpu:Cpu.t, n) => {
  Memory.load(cpu.memory, 0xFFFF)->is_bit_set(_, n)
}

let interrupt_vector = (n) => switch(n) {
| 0 => 0x40
| 1 => 0x48
| 2 => 0x50
| 3 => 0x58
| _ => 0x60
}

let interrupt = (cpu: Cpu.t) => {
  cpu.halted = false

  let rec check_interrupts = (n) => {
    let if_val = Memory.load(cpu.memory, 0xFF0F);
    if (is_bit_set(if_val, n) && is_interrupt_enabled(cpu, n)) {
      let if_val = clear_bit(if_val, n)
      Memory.store(cpu.memory, 0xFF0F, if_val)
      Cpu.push16(cpu, cpu.pc);
      {...cpu, pc: interrupt_vector(n), ime: false }
    } else {
      // TODO: got up to 5, missing joypad and serial
      n == 4 ? cpu : check_interrupts(n + 1)
    }
  }

  cpu.ime ? check_interrupts(0) : cpu
}

// TODO: Delete this? It was originally used in console mode
let run = (console) => {
  let rec loop = (console, steps) => {
    let prev_cy = console.cpu.cycle
    let (cpu, instruction) = CpuExec.step(~cpu=console.cpu, ~tracing=false);

    let lcd_on = Memory.load(cpu.memory, 0xFF40) land 0x80 > 0;
    let gpu = Gpu.step(console.gpu, cpu.cycle - prev_cy, lcd_on, cpu.memory.io);

    if (gpu.interrupts > 0) {
      let isf = Memory.load(cpu.memory, 0xFF0F);
      Memory.store(cpu.memory, 0xFF0F, isf lor gpu.interrupts);
      gpu.interrupts = 0
    }

    let cpu = interrupt(cpu)

    if (gpu.new_frame && lcd_on) {
      gpu.new_frame = false
    }
    let memory = {...cpu.memory, gpu}
    let cpu = {...cpu, memory}

    if (steps < 4000000) {
      loop({...console, cpu, gpu, memory}, steps + 1)
    } else {
      CpuExec.trace(cpu, instruction)
    }
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
    let (cpu, _instruction) = CpuExec.step(~cpu=console.cpu, ~tracing=console.tracing);
    cpu
  };
  let elapsed = cpu.cycle - prev_cy;

  let timer_int = Timer.tick(console.timer, elapsed / 4)
  if (timer_int) {
    request_interrupt(cpu, 4)
  }

  let lcd_on = Memory.load(cpu.memory, 0xFF40) land 0x80 > 0;
  let gpu = Gpu.step(console.gpu, elapsed, lcd_on, cpu.memory.io);

  if (gpu.interrupts > 0) {
    request_interrupt(cpu,gpu.interrupts)
    gpu.interrupts = 0
  };

  let cpu = interrupt(cpu)

  let memory = {...cpu.memory, gpu};
  let cpu = {...cpu, memory};
  {...console, cpu, memory, gpu}
}
