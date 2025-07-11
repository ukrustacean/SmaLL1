module SmaLL1.Program

open SmaLL1.BasicTypes
open SmaLL1.Terminal
open SmaLL1.Grammar

let keyword s = SimpleTerminal s s

let program =
    """
make = function(i,d) {
    if( d == 0 ) return $array(i,null,null);
    var i2 = 2 * i;
    d -= 1;
    $array(i,make(i2-1,d),make(i2,d));
};

check = function(n) {
    if( n[1] == null ) return n[0];
    return n[0] + check(n[1]) - check(n[2]);
}

var arg = $int($loader.args[0]);
if( arg == null ) arg = 10;
var min_depth = 4;
var max_depth = if( min_depth + 2 < arg ) arg else min_depth + 2;
var stretch_depth = max_depth + 1

var c = check(make(0,stretch_depth));
$print("stretch tree of depth ",stretch_depth,"\t check: ",c,"\n");

var long_lived_tree = make(0,max_depth);

loop_depths = function(d) {
    if( d <= max_depth ) {
        var niter = 1 << (max_depth - d + min_depth);
        var c = 0;
        var i = 0;
        while( i < niter ) {
            i += 1;
            c += check(make(i,d))+check(make(0-i,d));
        }
        $print(2*niter,"\t trees of depth ",d,"\t check: ",c,"\n");
        loop_depths(d + 2);
    }
}

loop_depths(min_depth);
$print("long lived tree of depth ",max_depth,"\t check: ",check(long_lived_tree),"\n");
"""

let main' _ =
    let skips = [ Simple " "; Simple "\t"; Simple "\n"; Simple "\r" ]

    let terminals =
        [ keyword "function"
          keyword "return"
          keyword "true"
          keyword "false"
          keyword "null"
          keyword "this"
          keyword "var"
          keyword "while"
          keyword "do"
          keyword "if"
          keyword "else"
          keyword "try"
          keyword "catch"
          keyword "return"
          keyword "break"
          keyword "continue"
          keyword "switch"
          keyword "default"

          keyword "=>"
          keyword "=="
          keyword "="
          keyword "$"
          keyword "{"
          keyword "}"
          keyword "("
          keyword ")"
          keyword "["
          keyword "]"
          keyword ";"
          keyword ","
          keyword "."

          RegExpTerminal "String" "\"[^\"]*\""
          RegExpTerminal "BinOp" "[!=*/<>&|^%+:-]+"
          RegExpTerminal "HexNumber" "0x[0-9A-Fa-f]+"
          RegExpTerminal "FloatNumber" "[0-9]+\.[0-9]*"
          RegExpTerminal "IntNumber" "[0-9]+"
          RegExpTerminal "Ident" "[a-zA-Z_@][a-zA-Z0-9_@]*" ]

    let result = lex skips terminals program

    result
    |> List.rev
    |> List.iter (fun { Name = name; Value = value } -> printf $"`{value}` ")

    printfn ""

    0
