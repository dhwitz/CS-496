let rec fact (n:int) : int =
    match n with
    | 0 -> 1
    | n -> n * fact(n-1)

let rec fact2 : int -> int = function
    | 0 -> 1
    | n -> n * fact(n-1)

let rec sumList : int list -> int = function
    | [] -> 0
    | x::xs -> x + sumList xs

let rec length : 'a list -> int = function
    | [] -> 0
    | _::xs -> 1 + length xs

let rec bump : int list -> int list = function
    | [] -> []
    | x::xs -> (x+1) :: bump xs

let rec repeat : 'a -> int -> 'a list = fun e n ->
    match n with
    | 0 -> []
    | n -> e :: repeat e (n-1)

let rec stutter : int -> 'a list -> 'a list = fun n xs ->
    match xs with
    | [] -> []
    | y::ys -> repeat y n @ stutter n ys

let rec remAdjDupl : 'a list -> 'a list = function
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> 
        if x=y
        then remAdjDupl (y::xs)
        else x::remAdjDupl (y::xs)

(* End of first ocaml class *)

let rec append : 'a list -> 'a list -> 'a list = fun xs ys ->
    match xs with
    | [] -> ys
    | x::xs -> x :: append xs ys

let rec reverse : 'a list -> 'a list = function
    | [] -> []
    | x::xs -> reverse xs @ [x]

let rec freverse : 'a list -> 'a list -> 'a list = fun xs acc ->
    match xs with
    | [] -> acc
    | y::ys -> freverse ys (y::acc)

(* ******************************** *)
(* Higher order programming schemes *)
(* ******************************** *)

let rec succl : int list -> int list = function
    | [] -> []
    | x::xs -> (x+1) :: succl xs

let rec upperl : char list -> char list = function
    | [] -> []
    | c::cs -> Char.uppercase_ascii c :: upperl cs

let rec zerol : int list -> bool list =  function
    | [] -> []
    | x::xs -> (x=0) :: zerol xs

let rec map : ('a -> 'b) -> 'a list -> 'b list = 
    fun f xs ->
        match xs with
            | [] -> []
            | y::ys -> f y :: map f ys



let rec gtz = function
    | [] -> []
    | x::xs ->
        if x>0
        then x :: gtz xs
        else gtz xs

let rec upc = function 
    | [] -> []
    | c::cs ->
        if Char.uppercase_ascii c=c
        then c:: upc cs
        else upc cs

let rec nzl = function
    | [] -> []
    | x::xs ->
        if x != []
        then x :: nzl xs
        else nzl xs

let rec filter : ('a -> bool) -> 'a list -> 'a list = 
    fun p xs ->
    match xs with
        | [] -> []
        | y::ys ->
            if p y 
            then y :: filter p ys
            else filter p ys



let rec suml : int list -> int = function
    | [] -> 0
    | x::xs -> x + suml xs

let rec andl : bool list -> bool = function
    | [] -> true
    | x::xs -> x && andl xs

let rec concat : 'a list list -> 'a list = function
    | [] -> []
    | xs::xss -> xs @ concat xss

let rec foldl : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b = 
    fun f a xs ->
    match xs with
    | [] -> a
    | x::xs -> f x (foldl f a xs)

let rec foldr : ( 'b -> 'a -> 'b) -> 'b -> 'a list -> 'b = 
    fun f a xs ->
    match xs with
    | [] -> a
    | x::xs -> foldr f (f a x) xs

let rec take : int -> 'a list -> 'a list = fun n xs ->
    match n, xs with
    | 0, _ -> []
    | n, [] -> []
    | n, x::xs -> x:: take (n-1) xs

(* let take' n xs = foldl ?? ?? xs n *)

type tree = Empty | Node of int*tree*tree

let t1 = Node(7, 
                Node(5, Empty, Empty),
                Node(9, Node(11, Empty, Empty), Empty))

let rec sizet = function
    | Empty -> 0
    | Node(_, lt, rt) -> 1 + sizet lt + sizet rt

let is_leaf = function
    | Node(_, Empty, Empty) -> true
    | _ -> false


type 'a option = None | Some of 'a

let rec lookup : 'a -> ('a*'b) list -> 'b option = fun k  d ->
    match d with
    | [] -> None
    | (x,y)::xs ->
        if x = k
        then Some y
        else lookup k xs

type ('a,'b) either = Left of 'a | Right of 'b

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let t1 = Node(7, 
                Node(5, Empty, Empty),
                Node(9, Node(11, Empty, Empty), Empty))

let t2 = Node(true, 
                Node(false, Empty, Empty),
                Node(false, Node(true, Empty, Empty), Empty))

let rec sizet : 'a tree -> int =  function
    | Empty -> 0
    | Node(_, lt, rt) -> 1 + sizet lt + sizet rt

let rec mirror : 'a tree -> 'a tree = function
    | Empty -> Empty
    | Node(i,lt,rt) -> Node(i, mirror rt, mirror lt)

let rec mapt : ('a -> 'b) -> 'a tree -> 'b tree = fun f t ->
    match t with
    | Empty -> Empty
    | Node(i, lt, rt) -> Node(f i, mapt f lt, mapt f rt)

let rec foldt = fun a f t ->
    match t with
    | Empty -> a
    | Node(i, lt, rt) -> f i (foldt a f lt) (foldt a f rt)

type 'a gtree = Node of 'a * 'a gtree list

let leaf n = Node(n, [])
let gt1 = 
    Node(5,[
        Node(2,[leaf 14]);
        Node(7,[leaf 8; leaf 11; leaf 12])
    ])

let rec mapgt = fun f t ->
    match t with
    | Node(i, []) -> Node(f i, [])
    | Node(i, xs) -> Node(f i, map(mapgt f) xs)

let rec sizet : int gtree -> int = function
    | Node(i, []) -> 1
    | Node(i, xs) -> 1 + List.fold_left (fun i a -> i+a) 0 (List.map sizet xs)



