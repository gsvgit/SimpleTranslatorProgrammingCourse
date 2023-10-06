// For more information see https://aka.ms/fsharp-console-apps

open Parsers.Combinators
open Parsers.Interpreter
(*
Parsers.Combinators.run Parsers.Combinators.ident "ab1cd"
|> fun x -> printfn $"{x}"


run (p_list (p_alt (p_char '3') (p_char '5')) (fmap (fun _ -> ()) (p_char ';'))) "5;3;5;3"
|> fun x -> printfn $"{x}"

*)


let res = run Parsers.ExprParser.p_prog "x=2*10+2*3*4+20
var=x+x
print:var*2
print=var*x
print:print"

match res with
| None -> printfn "Incorrect input"
| Some (rest,ast) ->
    //printfn $"Res: {res}"
    eval_prog ast


(*
let res = run Parsers.ExprParser.p_prog "x=2*10+2*3*4+20
print=x+x
print:print*2"

match res with
| None -> printfn "Incorrect input"
| Some (rest,str) -> printfn $"Res:{str}, rest: {rest}"
*)
