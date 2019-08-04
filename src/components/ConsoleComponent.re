[@bs.val] external setTimeout : (unit => unit, int) => unit = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";

let fetch_rom: (string) => Js.Promise.t(array(int)) = [%bs.raw
  {|
  async function logFetch(url) {
    const response = await fetch(url);
    const buffer = await response.arrayBuffer();
    return new Uint8Array(buffer);
  }
  |}
];

type canvas;
// type context;
// [@bs.send] external getContext : (canvas, string) => context = "getContext";
let get_display: unit => canvas = [%bs.raw {| function() { return document.getElementById("display") } |}];
// let ctx = getContext(display, "2d");

let display: (canvas, array(int)) => unit = [%bs.raw
  {|
    function display(canvas, pixels) {
      var ctx = canvas.getContext("2d");
      var imageData = new ImageData(new Uint8ClampedArray(pixels), 160, 144)
      ctx.putImageData(imageData, 0, 0);
    }
  |}
];

exception ConsoleFailure(string)
let steps = ref(0);

let console = ref(Console.make([||]));

let rec step = (canvas) => {
  while (!console^.gpu.new_frame) {
    console := (try (Console.step(console^)) {
    | Memory.InvalidMemoryAccess(msg) => {
      let msg = Printf.sprintf("Console crash at $%04X. Reason: %s", console^.cpu.pc, msg)
      raise(ConsoleFailure(msg))
    }
    })
  }

  display(canvas, console^.gpu.frame)
  console^.gpu.new_frame = false

  steps := steps^ + 1
  if (steps^ < 2000) {
    setTimeout(() => step(canvas), 16);
  } else {
    Js.log("Done")
  }
};

// fetch_rom("http://localhost:8000/roms/drmario.gb")
// fetch_rom("http://localhost:8000/roms/01-special.gb")
// fetch_rom("http://localhost:8000/roms/02-interrupts.gb")
// fetch_rom("http://localhost:8000/roms/03-op_sp_hl.gb")
// fetch_rom("http://localhost:8000/roms/04-op_r_imm.gb")
// fetch_rom("http://localhost:8000/roms/10-bit_ops.gb")

type state = { running: bool, loading: bool };
type action = Loading | Loaded;
let initial_state = { running: false, loading: false };

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer((state, action) => switch(action) {
  | Loading => {...state, loading: true}
  | Loaded => {...state, loading: false}
  }, initial_state)

  React.useEffect0(() => {
    fetch_rom("http://localhost:8000/roms/drmario.gb")
    // fetch_rom("http://localhost:8000/roms/tetris.gb")
    |> Js.Promise.then_(rom => {
      dispatch(Loaded);
      console := Console.make(rom);
      step(get_display())
      Js.Promise.resolve()})
    |> ignore;
    None
  });

  <div>
    {if (state.loading) {
      <div>{ReasonReact.string("Loading...")}</div>;
    } else {
      ReasonReact.null
    }}
    <canvas id="display"></canvas>
    <RegistersComponent console/>
  </div>
}
