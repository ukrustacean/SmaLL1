module SmaLL1.Pattern

open SmaLL1.BasicTypes
open System.Text.RegularExpressions

let private simpleMatch (pat: string) (s: string) =
    if s.StartsWith pat then
        Some(pat, s[pat.Length ..])
    else
        None

let private regexpMatch (regex: string) (s: string) =
    let r = Regex <| "^" + regex
    let v = r.Match(s).Value
    if v = "" then None else Some(v, s[v.Length ..])

let private customMatch f : Matcher = f

let matcher =
    function
    | RegExp r -> regexpMatch r
    | Simple s -> simpleMatch s
    | Custom c -> customMatch c
