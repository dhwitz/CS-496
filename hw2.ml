(*  
    David Horowitz 
    I pledge my honor that I have abided by the Stevens honor system.
*)

type dTree = Leaf of int | Node of char*dTree*dTree

let tLeft = Node('w', 
                Node('x', Leaf(2), Leaf(5)), 
                Leaf(8))

let tRight = Node('w',
                Node('x', Leaf(2), Leaf(5)),
                Node('y', Leaf(7), Leaf(5)))

let rec dTree_height : dTree -> int = fun tree ->
    match tree with
    | Leaf(_) -> 0
    | Node(_, lt, rt) -> 1 + max (dTree_height lt) (dTree_height rt)

let rec dTree_size : dTree -> int = fun tree ->
    match tree with
    | Leaf(_) -> 1
    | Node(_, lt, rt) -> 1 + dTree_size lt + dTree_size rt

let rec dTree_paths : dTree -> int list list = fun tree ->
    match tree with 
    | Leaf(_) -> [[]]
    | Node(_, lt, rt) -> 
        List.map (fun x -> 0::x) (dTree_paths lt) @ List.map (fun x -> 1::x) (dTree_paths rt)

let rec dTree_is_perfect : dTree -> bool = fun tree ->
    match tree with
    | Leaf(_) -> true
    | Node(_, lt, rt) ->
        if dTree_height lt = dTree_height rt
        then true && dTree_is_perfect lt && dTree_is_perfect rt
        else false

let rec dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree = fun f g t ->
    match t with
    | Leaf(x) -> Leaf(g x)
    | Node(x, lt, rt) -> Node(f x, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree : char list -> dTree = fun lst ->
    match lst with
    | [] -> failwith "Empty list is not allowed"
    | [x] -> Node(x, Leaf(0), Leaf(0))
    | x::xs -> Node(x, list_to_tree xs, list_to_tree xs)

let rec replace_leaf : dTree -> int list -> int -> dTree = fun tree lst n ->
    match tree with
    | Leaf(_) -> Leaf(n)
    | Node(x, lt, rt) ->
        match lst with
        | [] -> Node(x, lt, rt)
        | [y] -> 
            if y = 0
            then Node(x, Leaf(n), rt)
            else Node(x, lt, Leaf(n))
        | y::ys ->
            if y = 0
            then Node(x, replace_leaf lt ys n, rt)
            else Node(x, lt, replace_leaf rt ys n)

let rec replace_leaf_at : dTree -> (int list * int) list -> dTree = fun tree graph ->
    match graph with 
    | [] -> failwith "Graph can't be empty"
    | [x] ->  replace_leaf tree (fst x) (snd x)
    | x::xs -> replace_leaf_at (replace_leaf tree (fst x) (snd x)) xs

let bf_to_tree : (char list * (int list * int) list) -> dTree = fun enc ->
    replace_leaf_at (list_to_tree (fst enc)) (snd enc)

