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
  let spaces = many0 (char ' ' <|> char '\t' <|> char '\n' <|> char '\010')
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

module Parser =
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

module Interpreter =
  open Parser
  open Expr
  open Expr.Parser
  open System

  let updateEnv env v e = fun x -> if x = v then e else env x


  let readS env v = printf "Read(%s): " v
                    let e = try 
                              Console.ReadLine() |> System.Int32.Parse |> Some
                            with
                            | _ -> None
                    updateEnv env v e
  let writeS env e = calc env e |> printfn "%A" 
                     env
  let assignS env v e = updateEnv env v (calc env e)


  exception UndefinedValues
  let rec bs (env: string -> int option) (op: Parser.t) =
    match op with
    | Read v  -> readS env v
    | Write e -> writeS env e 
    | Assign (v, e) -> assignS env v e 
    | Seq (op1, op2) -> let nenv = bs env op1
                        bs nenv op2
    | If (e, op1, op2) -> match calc env e with
                          | None -> raise UndefinedValues
                          | Some 0 -> bs env op2
                          | Some _ -> bs env op1
    | While (e, body) -> match calc env e with
                         | None -> raise UndefinedValues
                         | Some 0 -> env
                         | Some _ -> let nenv = bs env body
                                     bs nenv op

  let rec ss (env: string -> int option) (op: Parser.t) =
    match op with 
    | Read v  -> readS env v, None
    | Write e -> writeS env e, None
    | Assign (v, e) -> assignS env v e, None
    | Seq (op1, op2) -> let (nenv, nop1) = ss env op1
                        match nop1 with
                        | None -> nenv, Some op2
                        | Some op -> nenv, Some (Seq (op, op2))
    | If (e, op1, op2) -> match calc env e with
                          | None -> raise UndefinedValues
                          | Some 0 -> env, Some op2
                          | Some _ -> env, Some op1
    | While (e, body) -> match calc env e with
                         | None -> raise UndefinedValues
                         | Some 0 -> env, None
                         | Some _ -> env, Some (Seq (body, op))

open Expr.Parser
open CoreParser

[<EntryPoint>]
let main argv =
  let s1 = &"read(x);
             read(n);
             res := 1;
             while (n) {
              res := res * x;
              n := n - 1
            };
            write(res)"
  let inter = Interpreter.bs (fun _ -> None)
  let e0 = Parser.parse () s1
  let e = e0 |> List.map (fun (x, _) -> x)
  printfn "%A" e0
  e |> List.map inter |> ignore
(*
  let e1 = Parser.parse () s1
  printfn "%A" e1
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
