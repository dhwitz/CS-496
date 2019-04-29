
(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | RefVal of int
  | UnitVal
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*Ast.expr*env
  | ExtendEnvMod of string*env*env

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
  | ExtendEnvMod(moduleName,bindings,env) -> apply_env env id

let rec apply_env_qual (env:env) (mid:string) (id:string):exp_val option =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (key,value,env) -> apply_env_qual env mid id
  | ExtendEnvRec(key,param,body,env) -> apply_env_qual env mid id
  | ExtendEnvMod(moduleName,bindings,env) ->
    if mid=moduleName
    then apply_env bindings id
    else apply_env_qual env  mid  id

let rec lookup_module (env:env) (mid:string) :env option =
  match env with
  | EmptyEnv -> None
  | ExtendEnv (key,value,env) -> lookup_module env mid
  | ExtendEnvRec(key,param,body,env) -> lookup_module env mid
  | ExtendEnvMod(moduleName,bindings,env) ->
    if mid=moduleName
    then Some bindings
    else lookup_module env  mid

let rec append_env (env1:env) (env2:env) :env =
  match env2 with
  | EmptyEnv -> env1
  | ExtendEnv (key,value,env) -> ExtendEnv(key,value, append_env env1 env)
  | ExtendEnvRec(key,param,body,env) -> ExtendEnvRec(key,param,body,
  append_env env1 env)
  | ExtendEnvMod(moduleName,bindings,env) ->
  ExtendEnvMod(moduleName,bindings,append_env env1 env)




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


let rec string_of_expval = function
  |  NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^id^","^Ast.string_of_expr body^","^ string_of_env env^")"
  | RefVal i -> "RefVal ("^string_of_int i^")"
  | UnitVal -> "UnitVal"
and
  string_of_env  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> "("^id^","^string_of_expval v^")"^string_of_env env
  | ExtendEnvRec(id,param,body,env) -> "("^id^","^param^","^Ast.string_of_expr body^")"^string_of_env env
  | ExtendEnvMod(id,defs,env) -> "Module "^id^"["^ (string_of_env
                                                      defs)^"]\n"^string_of_env env
