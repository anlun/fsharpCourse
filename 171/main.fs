let convertToBinaryList n =
  let rec convert n acc =
    if n > 0
    then convert (n / 2) ((n % 2) :: acc)
    else acc
  convert n []

let rec listToString l =
  match l with
  | [] -> ""
  | (x :: xs) -> x.ToString () + listToString xs

let decToBin n = n |> convertToBinaryList |> listToString
                 |> printf "%s\n"

(*  *)

let rec fastPower a k =
  if k <= 0
  then 1
  else
    let t = if k % 2 = 0 then 1 else a 
    t * fastPower (a * a) (k / 2)

(* *)

type list = Nil | Cons of int * list

let add x l = Cons (x, l)
let rec removeByValue x l =
  match l with
  | Nil -> Nil
  | Cons (y, l) -> if y = x then l else Cons (y, removeByValue x l)
let pop l =
  match l with
  | Nil -> Nil
  | Cons (x, l) -> l
let rec print l =
  match l with
  | Nil -> printf "\n"
  | Cons (x, l) -> printf "%i, " x; print l 

type Peano = Zero | S of Peano

let suc    (p : Peano) = S p
let minus1 (p : Peano) =
  match p with
  | Zero -> Zero
  | S p  -> p

let rec plus a b =
  match a with
  | Zero -> b
  | S  a -> S (plus a b)

[<EntryPoint>]
let main args =
  printf "%A\n" (minus1 (S (S Zero)))
  0
