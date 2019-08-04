let fetch_rom: (string) => Js.Promise.t(array(int)) = [%bs.raw
  {|
  async function logFetch(url) {
    const response = await fetch(url);
    const buffer = await response.arrayBuffer();
    return new Uint8Array(buffer)
  }
  |}
];

type canvas;
type context;
[@bs.send] external getContext : (canvas, string) => context = "getContext";

let display: canvas = [%bs.raw {| document.getElementById("display") |}];
let ctx = getContext(display, "2d");

let display: (context, array(int)) => unit = [%bs.raw
  {|
    function display(ctx, pixels) {
      var imageData = new ImageData(new Uint8ClampedArray(pixels), 160, 144)
      ctx.putImageData(imageData, 0, 0);
    }
  |}
];

[@bs.val] external setTimeout : (unit => unit, int) => unit = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";

let steps = ref(0);

exception ConsoleFailure(string)

let rec step = (console:Console.t) => {
  let console = ref(console)
  while (!console^.gpu.new_frame) {
    console := (try (Console.step(console^)) {
    | Memory.InvalidMemoryAccess(msg) => {
      let msg = Printf.sprintf("Console crash at %04X. Reason: %s", console^.cpu.pc, msg)
      raise(ConsoleFailure(msg))
    }
    })
  }

  display(ctx, console^.gpu.frame)
  console^.gpu.new_frame = false

  steps := steps^ + 1
  if (steps^ < 2000) {
    // Js.log("Step")
    setTimeout(() => step(console^), 1);
  } else {
    Js.log("Done")
  }
}

fetch_rom("http://localhost:8000/roms/tetris.gb")
// fetch_rom("http://localhost:8000/roms/drmario.gb")
// fetch_rom("http://localhost:8000/roms/01-special.gb")
// fetch_rom("http://localhost:8000/roms/02-interrupts.gb")
// fetch_rom("http://localhost:8000/roms/03-op_sp_hl.gb")
// fetch_rom("http://localhost:8000/roms/04-op_r_imm.gb")
// fetch_rom("http://localhost:8000/roms/10-bit_ops.gb")
|> Js.Promise.then_(rom => {
  let console = Console.make(rom);
  step(console)
  Js.Promise.resolve()
});


