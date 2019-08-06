[@bs.val] external setTimeout : (unit => unit, int) => unit = "setTimeout";
[@bs.val] external setInterval : (unit => unit, int) => unit = "setInterval";

type state = {
  af: int,
  bc: int,
  de: int,
  hl: int,
  sp: int,
  pc: int,
  flags: string,
  buttons: int,
  dpad: int
};

let initial_state = {
  af: 0,
  bc: 0,
  de: 0,
  hl: 0,
  sp: 0,
  pc: 0,
  flags: "----",
  buttons: 0,
  dpad: 0
};

let b2hex = (byte) => Printf.sprintf("$%02X", byte);
let w2hex = (word) => Printf.sprintf("$%04X", word);

[@react.component]
let make = (~console:ref(Console.t)) => {
  let (state, setState) = React.useState(_ => initial_state);

  // React.useEffect0(() => {
  //   setInterval(() => {
  //     let cpu = console^.cpu
  //     let af = Cpu.get_register16(cpu, AF)
  //     let bc = Cpu.get_register16(cpu, BC)
  //     let de = Cpu.get_register16(cpu, DE)
  //     let hl = Cpu.get_register16(cpu, HL)
  //     let sp = Cpu.get_register16(cpu, SP)
  //     let flags = CpuExec.pretty_flags(cpu)
  //     let buttons = console^.input.buttons
  //     let dpad = console^.input.dpad

  //     setState(_s => {pc:cpu.pc, af, bc, de, hl, sp, flags, buttons, dpad})
  //   }, 16);
  //   None
  // });

  <code>
    <div id="reg-PC">{ReasonReact.string("PC = " ++ w2hex(state.pc))}</div>
    <div id="reg-AF">{ReasonReact.string("AF = " ++ w2hex(state.af))}</div>
    <div id="reg-BC">{ReasonReact.string("BC = " ++ w2hex(state.bc))}</div>
    <div id="reg-DE">{ReasonReact.string("DE = " ++ w2hex(state.de))}</div>
    <div id="reg-HL">{ReasonReact.string("HL = " ++ w2hex(state.hl))}</div>
    <div id="reg-PC">{ReasonReact.string("SP = " ++ w2hex(state.sp))}</div>
    <div id="flags">{ReasonReact.string("Flags = " ++ state.flags)}</div>
    <div id="buttons">{ReasonReact.string("Buttons = " ++ w2hex(state.buttons))}</div>
    <div id="dpad">{ReasonReact.string("DPad = " ++ w2hex(state.dpad))}</div>
  </code>
}
