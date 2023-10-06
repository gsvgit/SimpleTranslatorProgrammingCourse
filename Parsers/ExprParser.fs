module Parsers.ExprParser

open Parsers.Combinators
open Parsers.AST

let p_ident: Parser<string> =
    p_some (satisfy (fun x -> List.contains x ['a'..'z']))
    |> fmap (fun res -> res |> Array.ofList |> System.String)

let p_num =
        p_seq (satisfy (fun x -> List.contains x ['1'..'9']))
              (fun res -> fmap (fun tl -> res :: tl) (p_many (satisfy (fun x -> List.contains x ['0'..'9']))))
        |> fmap (fun res -> res |> Array.ofList |> System.String |> int |> Number)

let p_mult =
    p_list (p_alt p_num (fmap Var p_ident)) (p_ignore (p_char '*'))
    |> fmap Mult

let p_add =
    p_list p_mult (p_ignore (p_char '+'))
    |> fmap Add

let p_asgn = p_seq p_ident
                   (fun ident_name -> p_seq (p_ignore (p_char '='))
                                            (fun _ -> fmap (fun expr -> Asgn (ident_name, expr)) p_add))


let p_kw_print = p_kw "print"

let p_print = p_seq p_kw_print (fun _ -> p_seq (p_char ':') (fun res -> fmap Print p_add))

let p_prog = p_list (p_alt p_print p_asgn) (p_ignore (p_char '\n'))
