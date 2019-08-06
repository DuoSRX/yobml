[@bs.val] external setTimeout : (unit => unit, int) => unit = "setTimeout";
[@bs.val] external clearTimeout : float => unit = "clearTimeout";
[@bs.val] external requestAnimationFrame : (int => unit) => int = "requestAnimationFrame"
type document;
[@bs.val] external document : (unit) => document = "document"

[@bs.val]
external add_keyboard_event_listener :
  (string, ReactEvent.Keyboard.t => unit) => unit =
  "addEventListener";

[@bs.val]
external remove_keyboard_event_listener :
  (string, ReactEvent.Keyboard.t => unit) => unit =
  "removeEventListener";

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

let console = ref(Console.make([||]));
let rec step = (canvas) => {
  while (!console^.gpu.new_frame) {
    console := (try (Console.step(console^)) {
    | Memory.InvalidMemoryAccess(msg)
    | Instructions.OpcodeNotImplemented(msg) => {
      let msg = Printf.sprintf("Console crash at $%04X. Reason: %s", console^.cpu.pc, msg)
      Js.log(msg)
      raise(ConsoleFailure(msg))
    }
    })
  }

  display(canvas, console^.gpu.frame)
  console^.gpu.new_frame = false
  requestAnimationFrame(_ => step(canvas)) |> ignore
};

type state = { loading: bool, running: bool };
type action = Loading | Loaded | KeyDown(string) | KeyUp(string);
let initial_state = { loading: false, running: false };

let handle_key_down = (dispatch, ev) =>
  dispatch(KeyDown(ReactEvent.Keyboard.key(ev)))

let handle_key_up = (dispatch, ev) =>
  dispatch(KeyUp(ReactEvent.Keyboard.key(ev)));

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer((state, action) => switch(action) {
  | Loading => {...state, loading: true}
  | Loaded => {...state, loading: false}
  | KeyDown(key) => Console.key_down(console^, key); state
  | KeyUp(key) => Console.key_up(console^, key); state
  }, initial_state)

  React.useEffect0(() => {
    // fetch_rom("http://localhost:8000/roms/flappyboy.gb")
    // fetch_rom("http://localhost:8000/roms/drmario.gb")
    // fetch_rom("http://localhost:8000/roms/tetris.gb")
    // fetch_rom("http://localhost:8000/roms/01-special.gb")
    // fetch_rom("http://localhost:8000/roms/02-interrupts.gb")
    // fetch_rom("http://localhost:8000/roms/03-op_sp_hl.gb")
    // fetch_rom("http://localhost:8000/roms/04-op_r_imm.gb")
    // fetch_rom("http://localhost:8000/roms/09-op_r_r.gb")
    // fetch_rom("http://localhost:8000/roms/10-bit_ops.gb")
    fetch_rom("http://localhost:8000/roms/11-op_a_hl.gb")
    |> Js.Promise.then_(rom => {
      dispatch(Loaded);
      console := Console.make(rom);
      requestAnimationFrame(_ => step(get_display())) |> ignore
      Js.Promise.resolve()})
    |> ignore;

    add_keyboard_event_listener("keydown", (ev) => dispatch(KeyDown(ReactEvent.Keyboard.key(ev))));
    add_keyboard_event_listener("keyup", (ev) => dispatch(KeyUp(ReactEvent.Keyboard.key(ev))));
    Some(() => {
      remove_keyboard_event_listener("keydown", (ev) => dispatch(KeyDown(ReactEvent.Keyboard.key(ev))));
      remove_keyboard_event_listener("keyup", (ev) => dispatch(KeyUp(ReactEvent.Keyboard.key(ev))));
    })
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
