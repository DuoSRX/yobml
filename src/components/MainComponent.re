let roms = [|
  ("drmario", "Dr Mario"),
  ("supermarioland", "Super Mario Land"),
  ("tetris", "Tetris"),
  ("pokemon_red", "Pokemon Red"),
  ("cpu_instrs", "Blargg CPU Tests"),
|];

[@react.component]
let make = () => {
  // let (romURL, setRom) = React.useState(() => None);
  let (romURL, setRom) = React.useState(() => Some("http://localhost:8000/roms/pokemon_red.gb"));

  let handleChange = (ev) => {
    switch(ReactEvent.Form.target(ev)##value) {
    | "" => ()
    | rom => setRom(_ => Some("http://localhost:8000/roms/" ++ rom ++ ".gb"))
    }
  };

  <div>
    <select onChange={handleChange}>
      <option></option>
      {Array.map(((name, title)) =>
        <option key={name} value={name}>{ReasonReact.string(title)}</option>
      , roms) |> ReasonReact.array}
    </select>
    {switch(romURL) {
    | None => ReasonReact.null
    | Some(romURL) => <ConsoleComponent romURL />
    }}
  </div>
}