module Parsers.Combinators

type Parser<'result> = list<char> -> Option<list<char>*'result>

let satisfy predicate: Parser<char> =
    fun input ->
        match input with
        | hd::tl when predicate hd -> Some(tl, hd)
        | _ -> None

let p_char ch: Parser<char> =
    satisfy (fun x -> ch = x)

let p_epsilon:Parser<unit> = fun input -> Some(input,())

let p_seq (p1: Parser<'r1>) (p2:'r1 -> Parser<'r2>): Parser<'r2> =
    fun input ->
        match p1 input with
        | None -> None
        | Some (rest, result) -> p2 result rest

let p_alt (p1:Parser<'r>) (p2:Parser<'r>): Parser<'r> =
    fun input ->
        match p1 input with
        | None -> p2 input
        | x -> x

let fmap (f:'r1 -> 'r2) (p:Parser<'r1>): Parser<'r2> =
    fun input ->
        match p input with
        | None -> None
        | Some(rest, res) -> Some(rest, f res)

let rec p_many (p:Parser<'r>): Parser<list<'r>> =
    p_alt (p_seq p (fun res -> fmap (fun tl -> res::tl) (p_many p)))
          (fmap (fun _ -> []) p_epsilon)

let p_some (p:Parser<'r>): Parser<list<'r>> =
    p_seq p (fun res -> fmap (fun tl -> res::tl) (p_many p))

let p_list (p_elem:Parser<'elem>) (p_sep:Parser<unit>) =
    p_seq p_elem (fun res -> fmap (fun tl -> res :: tl)
                                  (p_many (p_seq p_sep (fun _ -> p_elem))))

let p_ignore p = fmap (fun _ -> ()) p

let p_kw (kw:string): Parser<string> =
    let chars = kw.ToCharArray()
    Array.fold
        (fun parser curChar ->
            p_seq parser (fun res -> fmap (fun char -> char::res) (p_char curChar)))
        (fmap (fun _ -> []) p_epsilon)
        chars
    |> fmap (fun charList -> charList |> List.rev |> Array.ofList |> System.String)


let run =
    fun (p: Parser<'r>) (input: string) ->
        p (List.ofArray <| input.ToCharArray())
