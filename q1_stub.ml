


(* type definition of a binary tree *)
type 'a tree = Empty | Node of 'a*'a tree*'a tree



let rec take n xs =
  match n,xs with
  | 0,_ -> []
  | n,x::xs when n>0 -> x::take (n-1) xs
  | n,_ -> []

let rec drop n xs =
  match n,xs with
  | 0,xs -> xs
  | n,x::xs when n>0 -> drop (n-1) xs
  | n,xs -> xs

(* the split helper function *)
let split xs =
  let n = List.length xs
  in (take (n/2) xs,drop (n/2) xs)




(* Implement the mt function below *)
let rec mt :  (int -> int) -> int list ->int tree = fun f lst ->
  match xs with 
  | [x] -> Node(f x, Empty, Empty)
  | ys -> 
    let (fst,snd) = split ys
    in let Node(i1, lt1, rt1) = mt f fst
    in let Node(i2, lt2, rt2) = mt f snd
    in Node(f (i1+i2), Node(i1, lt1, rt1), Node(i2,lt2,rt2)


let rec mt2 :  (int -> int) -> int list ->int tree = fun f lst ->
  match xs with 
  | [x] -> Node(f x, Empty, Empty)
  | ys -> 
    let (fst,snd) = split ys
    in match mt f fst with
      | (Node(i1, lt1, rt1) as t1) -> 
      (match mt f snd with
        | (Node(i2, lt2, rt2) as t2) -> Node(f (i1+i2), t1, t2)
        | _ -> failwith "error"
      | _ -> failwith "error"