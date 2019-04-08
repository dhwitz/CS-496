
(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | RefVal of int
  | UnitVal
  | PairVal of exp_val*exp_val
  | ListVal of exp_val list
  | TreeVal of btree 
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*Ast.expr*env
and
  btree =
  | EmptyNodeVal of Ast.texpr
  | NodeVal of exp_val*btree*btree

(* Operations on environments *)

let empty_env (x:unit):env = EmptyEnv

let extend_env (env:env) (id:string) (v:exp_val):env = ExtendEnv(id,v,env)

let rec apply_env (env:env) (id:string):exp_val option =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (key,value,env) ->
    if id=key
    then Some value
    else apply_env env id
  | ExtendEnvRec(key,param,body,en) ->
    if id=key
    then Some (ProcVal(param,body,env))
    else apply_env en id


(* operations on expressed values *)

let numVal_to_num =  function
  |  NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool =  function
  |  BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let refVal_to_int =  function
  |  RefVal n -> n
  | _ -> failwith "Expected a reference!"

let unitVal_to_unit = function
  | UnitVal -> ()
  | _ -> failwith "Expected a unit!"

let pairVal_to_fst = function
  | PairVal(exp_val1, _) -> exp_val1
  | _ -> failwith "Expected a pair!"

let pairVal_to_snd = function
  | PairVal(_, exp_val2) -> exp_val2
  | _ -> failwith "Expected a pair!"

let listVal_to_list = function
  | ListVal n -> n
  | _ -> failwith "Expected a list!"

let treeVal_to_tree = function
  | TreeVal n -> n
  | _ -> failwith "Expected a tree!"


let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | RefVal i -> "RefVal (" ^ string_of_int i ^ ")"
  | UnitVal -> "UnitVal"
  | PairVal(fst, snd) -> "PairVal ("^string_of_expval fst^","^string_of_expval snd^")"
  | ListVal l -> "ListVal " ^ string_of_listval l
  | TreeVal t -> "TreeVal " ^ string_of_btree t
and
  string_of_listval e = String.concat ";" (List.map (fun (x) -> "("^string_of_expval x^")") e)
and
  string_of_env  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> "("^id^","^string_of_expval v^")"^string_of_env env
  | ExtendEnvRec(id,param,body,env) -> "("^id^","^param^","^Ast.string_of_expr body^")"^string_of_env env
and
  string_of_btree = function
  | EmptyNodeVal t -> "EmptyNode"
  | NodeVal(data, lst, rst) -> "NodeVal(" ^ (string_of_expval data) ^ "," ^ (string_of_btree lst) ^ "," ^ (string_of_btree rst) ^ ")"
