module A

module CoreParser =
  type 'a t = char list -> ('a * char list) list

  let (>>=) (p: 'a t) (f: 'a -> 'b t) = fun s ->
    List.concat [for (r, s') in p s -> (f r) s']

  let mreturn r = fun s -> [(r, s)]
  let lambda    = fun s -> []
  let item      = fun s -> match s with [] -> [] | h :: s -> [(h, s)]
  let sat cond  = item >>= fun c -> if cond c then mreturn c else lambda
  
  let (>>) p q = p >>= fun _ -> q
  let (<<) p q = p >>= fun rs -> q >> mreturn rs
  
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
  
  let number = map (fun s -> %s |> System.Int32.Parse) (many1 digit) 
  let word   = map (~%) (many1 alpha)
  let spaces = many0 (char ' ')
  let sp   f = spaces >> f << spaces 
  
  let paren p =
    let lparen = char '('
    let rparen = char ')'
    sp lparen >>= (fun _ -> p() << sp rparen)
  let paren' p = paren (fun _ -> p)  
  
  let cparen p =
    let lcparen = char '{'
    let rcparen = char '}'
    sp lcparen >>= (fun _ -> p() << sp rcparen)
  let cparen' p = cparen (fun _ -> p)  

// ---------

module Expr =
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

  module Parser =
    open CoreParser

    exception EmptyList
    let fold1 f s =
      match s with
      | h :: s -> List.fold f h s
      | [] -> raise EmptyList

    let binOp  = ['+'; '-'; '*'; '/'] |>
                 List.map char |>
                 fold1 (<|>)
    
    let term = (map Num number) <|> (map Var word) |> sp
    let binOpP p = term >>= fun leftArg ->
                  sp binOp >>= fun op ->
                  map (fun s -> BinOp (op, leftArg, s)) (p()) 

    let rec parse _: t CoreParser.t =
      binOpP parse <|> term <|> paren parse

  let (>>=) a f =
    match a with
    | None -> None
    | Some  a -> f a 
  let mreturn a = Some a

  let rec calc env e =
    match e with
    | Num n -> Some n
    | Var x -> env  x
    | BinOp (o, x, y) ->
      calc env x >>= fun vx -> 
      calc env y >>= fun vy -> 
      (op o) vx vy |> mreturn 

module Interpreter =
  type t = Read   of string
         | Write  of Expr.t
         | Assign of string * Expr.t
         | Seq    of t * t
         | If     of Expr.t * t * t
         | While  of Expr.t * t

  open CoreParser
  let readP   = symbol &"read"  >> paren' (map (fun x -> Read x) word)
  let writeP  = symbol &"write" >> paren' (map Write (Expr.Parser.parse ()))
  let assignP = (sp word << sp (symbol &":=")) >>= fun x ->
                map (fun e -> Assign (x, e)) (Expr.Parser.parse ())

  let ifP    p = (symbol &"if" |> sp)  >>
                 (Expr.Parser.parse () |> sp) >>= fun e ->
                 p() |> cparen' >>= fun op1 ->
                 (symbol &"else" |> sp) >>
                 (p() |> cparen' |> map (fun op2 -> If (e, op1, op2)))
  let whileP p = (symbol &"while" |> sp)  >>
                 (Expr.Parser.parse () |> sp) >>= fun e ->
                 (p() |> cparen' |> map (fun op -> While (e, op)))
  
  let term   p = readP <|> writeP <|> assignP <|> ifP p <|> whileP p
  
  let seqP   p = (term p << sp (char ';')) >>= fun x ->
                 (p() |> sp |> map (fun y -> Seq (x, y)))
  
  let rec parse _: t CoreParser.t =
    seqP parse <|> term parse


open Expr.Parser
open CoreParser

[<EntryPoint>]
let main argv =
  let s1 = &"while (x + 5) { read(y); write(x) }"
  let e1 = Interpreter.parse () s1
  printfn "%A" e1
(*
  let s1 = &"(5 + 5 - (x + 7))"
  let e1 = parse () s1
  let s2 = &"(5 + 5 - (y + 7))"
  let e2 = parse () s2
  let f e = e |> List.map (fun (x, _) -> x)
              |> List.map (Expr.calc (fun x -> if x = "x" then Some 5 else None))
              |> printfn "%A"
  f e1
  f e2*)
  0 // return an integer exit code
