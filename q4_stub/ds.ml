(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | ListVal of exp_val list
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of (Ast.dec list)*env
  (* Old implementation (without mutual recursion) *)
  (* | LetrecEnv of (string*string*Ast.expr*env) *)

(* Operations on environments *)

let rec apply_env (env:env) (id:string):exp_val option =
  match env with
  | EmptyEnv                       -> failwith "Id not found"
  | ExtendEnv (key, value, saved_env) ->
    if id=key then
      Some value
    else apply_env saved_env id
  | ExtendEnvRec (decs, saved_env)    -> failwith "Implement me"
  (* Old implementation (without mutual recursion) *)
  (* | LetrecEnv (name, var, body, saved_env) ->
   *   if id=name then
   *     ProcVal(var, body, env)
   *   else apply_env saved_env id *)

(* operations on expressed values *)

let numVal_to_num =  function
  | NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool =  function
  | BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let listVal_to_list =  function
  | ListVal l -> l
  | _ -> failwith "Expected a list!"

let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | ListVal l -> "ListVal " ^ string_of_listval l
and
  string_of_listval e = String.concat ";" (List.map (fun (x) -> "("^string_of_expval x^")") e)
and
  string_of_env = function
  | EmptyEnv -> ""
  | ExtendEnv (key, value, saved_env) ->
    "(" ^ key ^ "," ^ string_of_expval value ^ "); " ^ string_of_env saved_env
  | ExtendEnvRec (decs, saved_env) ->
    "(" ^ Ast.string_of_decs decs ^ " ); " ^ string_of_env saved_env
