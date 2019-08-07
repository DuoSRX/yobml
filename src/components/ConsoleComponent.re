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
type context;
[@bs.send] external getContext : (canvas, string) => context = "getContext";
[@bs.send] external putImageData : (context, array(int), int, int) => unit = "putImageData";
let get_display: unit => canvas = [%bs.raw {| function() { return document.getElementById("display") } |}];
let get_context = () => getContext(get_display(), "2d");

let display: (context, array(int)) => unit = [%bs.raw
  {|
    function display(ctx, pixels) {
      var imageData = new ImageData(new Uint8ClampedArray(pixels), 160, 144)
      ctx.putImageData(imageData, 0, 0);
    }
  |}
];

exception ConsoleFailure(string)

let console = ref(Console.make(Array.make(0x200, 0)));
type color = { r: int, g: int, b: int}
let color_map = [|
  { r: 0x9B, g: 0xBC, b: 0x0F }, // lightest green
  { r: 0x8B, g: 0xAC, b: 0x0F }, // light green
  { r: 0x30, g: 0x62, b: 0x30 }, // dark green
  { r: 0x0F, g: 0x38, b: 0x0F }, // darkest green
|]
// let color_map = [|0xFF, 0xA0, 0x50, 0x0|]
let pixels = Array.make(160 * 144 * 4, 0); // 160 lines * 144 columns * 4 bytes

let rec step = (context) => {
  while (!console^.gpu.new_frame) {
    console := (try (Console.step(console^)) {
    | Memory.InvalidMemoryAccess(msg)
    | Instructions.OpcodeNotImplemented(msg) => {
      let msg = Printf.sprintf("Console crash at $%04X. Reason: %s", console^.cpu.pc, msg)
      Js.log(msg)
      raise(ConsoleFailure(msg))
    }
    })
  };

  Array.iteri((i, px) => {
    let color = color_map[px]
    pixels[i * 4 + 0] = color.r // R
    pixels[i * 4 + 1] = color.g // G
    pixels[i * 4 + 2] = color.b // B
    pixels[i * 4 + 3] = 0xFF    // A
  }, console^.gpu.frame);

  display(context, pixels);
  console^.gpu.new_frame = false
  requestAnimationFrame(_ => step(context)) |> ignore
};

type state = { loading: bool, running: bool };
type action = Loading | Loaded | KeyDown(string) | KeyUp(string) | ToggleTracing;
let initial_state = { loading: false, running: false };

let handle_key_down = (dispatch, ev) =>
  dispatch(KeyDown(ReactEvent.Keyboard.key(ev)))

let handle_key_up = (dispatch, ev) =>
  dispatch(KeyUp(ReactEvent.Keyboard.key(ev)));

[@react.component]
let make = (~romURL) => {
  let (state, dispatch) = React.useReducer((state, action) => switch(action) {
  | Loading => {...state, loading: true}
  | Loaded => {...state, loading: false}
  | KeyDown(key) => Console.key_down(console^, key); state
  | KeyUp(key) => Console.key_up(console^, key); state
  | ToggleTracing => console^.tracing = !console^.tracing; state
  }, initial_state)

  React.useEffect1(() => {
    fetch_rom(romURL)
    |> Js.Promise.then_(rom => {
      dispatch(Loaded);
      console := Console.make(rom);
      step(get_context()) |> ignore
      Js.Promise.resolve()})
    |> ignore;

    add_keyboard_event_listener("keydown", (ev) => dispatch(KeyDown(ReactEvent.Keyboard.key(ev))));
    add_keyboard_event_listener("keyup", (ev) => dispatch(KeyUp(ReactEvent.Keyboard.key(ev))));
    Some(() => {
      remove_keyboard_event_listener("keydown", (ev) => dispatch(KeyDown(ReactEvent.Keyboard.key(ev))));
      remove_keyboard_event_listener("keyup", (ev) => dispatch(KeyUp(ReactEvent.Keyboard.key(ev))));
    })
  }, [|romURL|]);

  <div>
    {if (state.loading) {
      <div>{ReasonReact.string("Loading...")}</div>;
    } else {
      ReasonReact.null
    }}
    <canvas id="display" ></canvas>
    // <button id="tracing" onClick={_ => dispatch(ToggleTracing)}>
    //   {ReasonReact.string("Toggle tracing")}
    // </button>
    // <RegistersComponent console/>
  </div>
}
