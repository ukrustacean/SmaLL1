module SmaLL1.Terminal

open SmaLL1.BasicTypes

let RegexTerminal n r =
    { Name = n
      Matcher = Pattern.matcher <| RegExp r }

let SimpleTerminal n s =
    { Name = n
      Matcher = Pattern.matcher <| Simple s }

let CustomTerminal n f =
    { Name = n
      Matcher = Pattern.matcher <| Custom f }
    
let parsePrefix s { Name = name; Matcher = f } =
    f s |> Option.map (fun (value, left) -> ({ Name = name; Value = value }, left))