module A

module CoreParser =
  type 'a t = char list -> ('a * char list) list

  let (>>=) (p: 'a t) (f: 'a -> 'b t) = fun s ->
    List.concat [for (r, s') in p s -> (f r) s']

  let (>>) p q = p >>= fun _ -> q
  
  let mreturn r = fun s -> [(r, s)]
  let lambda    = fun s -> []
  let item      = fun s -> match s with [] -> [] | h :: s -> [(h, s)]
  let sat cond  = item >>= fun c -> if cond c then mreturn c else lambda
  
  let char c    = sat ((=) c)
  let digit     = sat (fun c -> (List.tryFind ((=) c) ['0'..'9']).IsSome) 
  let alpha     = sat (fun c ->
    (List.tryFind ((=) c) (List.append ['a'..'z'] ['A'..'Z'])).IsSome)

  let (<|>) p q = fun s ->
    match p s with
    | [] -> q s
    | rs -> rs  
  let (++) p q = fun s -> List.append (p s) (q s)
  
  let rec many0 p = many1 p <|> mreturn []
  and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

  let rec symbol cs =
    match cs with
    | [] -> mreturn [] 
    | c::cs' -> char c >> symbol cs' >> mreturn cs

  let (~&) (s: string   ) = s.ToCharArray() |> List.ofArray
  let (~%) (l: char list) = new string(Array.ofList l)

  let map (f: 'a -> 'b) (p: 'a t): 'b t = fun s ->
    p s |> List.map (fun (e, s) -> (f e, s)) 
  
  let number  = map (fun s -> %s |> System.Int32.Parse) (many1 digit) 
  let word    = map (~%) (many1 alpha)
  let space f = many0 (char ' ') >> f

// ---------

module Calculator =
  type t = Num of int
         | Var of string
         | BinOp of char * t * t
  
  exception NotOperation 
  let op s =
    match s with
    | '+' -> (+)
    | '-' -> (-)
    | '*' -> ( * )
    | '/' -> (/)
    | _   -> raise NotOperation

  module Lexer =
    open CoreParser
    
    type t = LParen | RParen | Num of int | Var of string | BinOp of char
    
    exception EmptyList
    let fold1 f s =
      match s with
      | h :: s -> List.fold f h s
      | [] -> raise EmptyList

    let l: (t list) CoreParser.t =
      let paren c s = map (fun _ -> c) (char s)
      let lparen = paren LParen '('
      let rparen = paren RParen ')'
      let n      = map Num number
      let v      = map Var word
      let b      = ['+'; '-'; '*'; '/'] |>
                   List.map char |>
                   fold1 (<|>) |>
                   map BinOp
      (lparen <|> rparen <|> n <|> v <|> b) |> space |> many0
 
  module Parser =
    let parse s =
      let lexems = s |> Lexer.l
      "asdf"

open Calculator.Lexer
open CoreParser

[<EntryPoint>]
let main argv =
  let s = "(5 + x)"
  printfn "%A" (l &s)
  0 // return an integer exit code
