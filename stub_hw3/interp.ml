open Ast
open Ds

let is_treeVal = function
  | TreeVal l -> true
  | _ -> false

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Div(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | Mul(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Abs(e1)         -> 
    let v1 = eval en e1
    in if (numVal_to_num v1) < 0
    then NumVal((numVal_to_num v1) * -1)
    else v1
  | Cons(e1, e2) ->
    if is_listVal (eval en e2) = false
    then failwith "Error, must cons to a list"
    else ListVal(eval en e1 :: listVal_to_list (eval en e2))
  | Hd(e1)          ->
    let lst = listVal_to_list (eval en e1)
    in if List.length lst = 0
    then failwith "List cannot be empty"
    else List.hd lst
  | Tl(e1)          -> 
    let lst = listVal_to_list (eval en e1)
    in if List.length lst = 0
    then failwith "List cannot be empty"
    else ListVal(List.tl lst)
  | Empty(e1)       -> 
    if is_listVal (eval en e1) = true
    then BoolVal(List.length (listVal_to_list (eval en e1)) = 0)
    else if is_treeVal (eval en e1) = true
      then BoolVal(treeVal_to_tree (eval en e1) = Empty)
      else failwith "Must be a list or a tree"
  | EmptyList       -> ListVal([])
  | EmptyTree       -> TreeVal(Empty)
  | Node(e1,lt,rt)  -> 
    let left = treeVal_to_tree (eval en lt)
    in let right = treeVal_to_tree (eval en rt)
    in TreeVal(Node((eval en e1), left, right))
  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) -> 
    let tree1 = treeVal_to_tree (eval en target)
    in match tree1 with
    
      | Empty -> eval en emptycase
      | Node(ev1, ev2, ev3) -> 
        let en1 = extend_env en id_e ev1
        in let en2 = extend_env en1 id_lt (TreeVal(ev2))
        in let en3 = extend_env en2 id_rt (TreeVal(ev3))
        in eval en3 nodecase

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
