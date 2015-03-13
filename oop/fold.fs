type Tree<'A> =
  | Empty
  | Node of 'A * Tree<'A> * Tree<'A>
  static member (+) (t1: Tree<'A>, t2: Tree<'A>) =
    t1

let rec fold f i t =
  match t with
  | Empty -> i
  | Node (x, l, r) ->
    let xf = f i x
    let lf = fold f xf l
    let rf = fold f lf r
    rf
(* val fold : f:('a -> 'b -> 'a) -> i:'a -> t:Tree<'b> -> 'a *)

let rec fold' f i t =
  match t with
  | Empty -> i
  | Node (x, l, r) ->
    let lf = fold' f i l
    let rf = fold' f i r
    f x lf rf
(* val fold' : f:('a -> 'b -> 'b -> 'b) -> i:'b -> t:Tree<'a> -> 'b *)

let rec sum  t = fold  (+) 0 t
let rec sum' t = fold' (fun x l r -> x + l + r) 0 t
(*
val sum : t:Tree<int> -> int
val sum' : t:Tree<int> -> int
*)

let rec sum''  t = fold (+) 0.0 t
(* val sum'' : t:Tree<float> -> float *)
let rec sum''' t = fold (+) ""  t
(* val sum''' : t:Tree<string> -> string *)

let rec gSum (zero : 'A) t = fold (+) zero t
(* val gSum : zero:int -> t:Tree<int> -> int *)

let rec gSum' (zero : 'A) (t : Tree<'A>) = fold (+) zero t
(*
fold.fs(37,50): warning FS0064:
This construct causes code to be less
generic than indicated by the type annotations.
The type variable 'A has been constrained to be type 'int'.

val gSum' : zero:int -> t:Tree<int> -> int
*)

let rec gSum'' plus (zero : 'A) (t : Tree<'A>) = fold plus zero t
(* val gSum'' : plus:('A -> 'A -> 'A) -> zero:'A -> t:Tree<'A> -> 'A *)

let rec inline sum1 (z : 'A) t = fold (+) z t 
(*
val inline sum1 :
  z: ^A -> t:Tree< ^a> ->  ^A
    when ( ^A or  ^a) : (static member ( + ) :  ^A *  ^a ->  ^A)
*)

let inline sum_tree t =
  fold (fun acc fl ->  acc + fl) Unchecked.defaultof<'T> t
(*
val inline sum_tree :
  t:Tree< ^a> ->  ^T
    when ( ^T or  ^a) : (static member ( + ) :  ^T *  ^a ->  ^T)
*)

let t1 = Node (5, Empty, Node (6, Empty, Empty))
let t2 = Node (5.0, Empty, Node (6.0, Empty, Empty))
let t3 = Node ("5.0", Empty, Node ("6.0", Empty, Empty))

let t4 = Node (t1, Empty, Node (t1, Empty, Empty))