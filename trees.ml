
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let t1: int tree = Node(74, 
                Node(50, Empty, Node(62, Empty, Empty)),
                Node(88, Node(83, Empty, Empty), Empty))

let rec find : 'a -> 'a tree -> bool = fun k t ->
    match t with
    | Empty -> false
    | Node(x, lt, rt) when k = x -> true
    | Node(x, lt, rt) -> 
        if k < x
        then find k lt
        else find k rt
    
let rec add : 'a -> 'a tree -> 'a tree = fun k t ->
    match t with
    | Empty -> Node(k, Empty, Empty)
    | Node(x, lt, rt) when x = k -> t
    | Node(x, lt, rt) ->
        if k < x
        then Node(x, add k lt, rt)
        else Node(x, lt, add k rt)

let rec max : 'a tree -> 'a = fun t ->
    match t with
    | Empty -> failwith "Cannot be empty"
    | Node(x, lt, Empty) -> x
    | Node(x, _, rt) -> max rt

let rec remove : 'a -> 'a tree -> 'a tree = fun k t ->
    match t with
    | Empty -> failwith "No element"
    | Node(x, lt, Empty) when x = k -> lt
    | Node(x, Empty, rt) when x = k -> rt
    | Node(x, lt, rt) when x = k -> 
        let m = max lt
        in Node(m, remove m lt, rt)
    | Node(x, lt, rt) ->
        if k > x
        then Node(x, lt, remove k rt)
        else Node(x, remove k lt, rt)

