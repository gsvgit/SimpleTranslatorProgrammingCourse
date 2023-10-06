module Parsers.Interpreter

open System.Collections.Generic
open Parsers.AST

let rec eval (context:Dictionary<_,_>) expr =
    match expr with
    | Number n -> n
    | Mult l -> l |> List.map (eval context) |> List.reduce (*)
    | Add l ->  l |> List.map (eval context) |> List.reduce (+)
    | Var v_name ->
        if context.ContainsKey v_name
        then context[v_name]
        else failwithf $"Var with name {v_name} not declared."

let rec eval_stmt context stmt =
    match stmt with
    | Asgn (v_name, expr) ->
        Some (v_name, eval context expr)
    | Print expr ->
        printfn $"{eval context expr}"
        None

let eval_prog (statements:list<sourceAst>) =
    let context = Dictionary<string, int>()
    List.fold
        (fun (context:Dictionary<_,_>) stmt ->
                let res = eval_stmt context stmt
                match res with
                | Some (v_name, res) ->
                    if context.ContainsKey v_name
                    then context[v_name] <- res
                    else context.Add(v_name, res)
                | None -> ()
                context)
        context statements
    |> ignore



