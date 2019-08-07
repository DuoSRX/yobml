[@react.component]
let make = (~cartridge:Cartridge.t) => {
  let header = cartridge.header;

  <ul>
    <li>{ReasonReact.string("Title: (TODO)")}</li>
    <li>{ReasonReact.string("Mapper: " ++ CartridgeHeader.mapper_to_string(header))}</li>
    <li>{ReasonReact.string("ROM Size: " ++ (header.rom_size |> string_of_int) ++ " bytes")}</li>
  </ul>
}
