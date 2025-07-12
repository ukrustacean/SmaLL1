module SmaLL1.Pattern

open SmaLL1.BasicTypes
open System.Text.RegularExpressions

let private simpleMatch (pat: string) (s: string, pos: int) =
    let l = pat.Length

    if s.Length - pos >= l && s.Substring(pos, l) = pat then
        Some(pat, l + pos)
    else
        None

let private regexpMatch (regex: Regex) (s: string, pos: int) =
    let r = Regex <| "\G" + regex.ToString()
    let v = r.Match(s, pos).Value
    if v = "" then None else Some(v, v.Length + pos)

let private customMatch f : Matcher = f

let matcher =
    function
    | RegExp r -> regexpMatch r
    | Simple s -> simpleMatch s
    | Custom c -> customMatch c
