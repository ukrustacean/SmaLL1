module SmaLL1.Pattern

open SmaLL1.BasicTypes
open System.Text.RegularExpressions

let simpleMatch pat : Matcher =
        let rec helper a b =
            match (a, b) with
            | [], b -> Some b
            | a, [] -> Some a
            | x :: xs, y :: ys when x = y -> helper xs ys
            | _ -> None

        fun s ->
            helper <| List.ofSeq pat <| List.ofSeq s
            |> Option.map (fun x -> (pat, x |> List.toArray |> System.String))

let regexpMatch regex : Matcher =
    let r = Regex <| "^" + regex

    fun s ->
        let v = r.Match(s).Value
        if v = "" then None else Some(v, r.Replace(s, ""))

let customMatch f : Matcher = f

let matcher =
    function
    | RegExp r -> regexpMatch r
    | Simple s -> simpleMatch s
    | Custom c -> customMatch c

let skipPattern pat s =
    matcher pat s |> Option.map snd |> Option.defaultValue s