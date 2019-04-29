open Ast

let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

let snd (_,y) = y
  
(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)
    
type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv
  | ExtendTEnvMod of string*tenv*tenv
                     
let rec string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,t,tenv) -> "("^id^","^string_of_texpr t^")"^string_of_tenv tenv
  | ExtendTEnvMod(id,decls,tenv) -> "( Module "^id^","^string_of_tenv
                                    decls^")"^string_of_tenv tenv


let rec lookup_module_type (tenv:tenv) (mid:string) :tenv option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,ty,tenv) -> lookup_module_type tenv mid
  | ExtendTEnvMod(moduleName,tbindings,tenv) ->
    if mid=moduleName
    then Some tbindings
    else lookup_module_type tenv  mid

let rec append_tenv (tenv1:tenv) (tenv2:tenv) :tenv =
  match tenv2 with
  | EmptyTEnv -> tenv1
  | ExtendTEnv (key,ty,tenv) -> ExtendTEnv(key,ty, append_tenv tenv1 tenv)
  | ExtendTEnvMod(moduleName,tbindings,tenv) ->
  ExtendTEnvMod(moduleName,tbindings,append_tenv tenv1 tenv)
exception Subtype_failure of string
                       
let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)


let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) ->
    if id=key
    then Some value
    else apply_tenv tenv1 id
  | ExtendTEnvMod(m_name,m_type,tenv1) -> apply_tenv tenv1 id

let rec apply_tenv_qual (tenv:tenv) (id_module:string) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv (key,value,tenv1) -> apply_tenv_qual tenv1 id_module id
  | ExtendTEnvMod(m_name,m_type,tenv1) ->
        if id_module=m_name
    then apply_tenv m_type id
    else apply_tenv_qual tenv1 id_module id

  
let init_tenv () =
     extend_tenv "x"  IntType 
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env
  | ExtendTEnvMod(m_name,m_type,env) ->  "module("^m_name^","^string_of_tenv m_type^")"^string_of_tenv env

let reverse_tenv tenv =
  let rec reverse_tenv' acc = function
    | EmptyTEnv ->  acc
    | ExtendTEnv(id,ty,tenv_tail) -> reverse_tenv'
  (ExtendTEnv(id,ty,acc)) tenv_tail
    | ExtendTEnvMod(mid,body,tenv_tail) ->reverse_tenv'
  (ExtendTEnvMod(mid,body,acc)) tenv_tail
  in reverse_tenv' EmptyTEnv tenv

(*  ;;;;;;;;;;;;;;;; type checker ;;;;;;;;;;;;;;;; *)

let rec is_subtype (actual:tenv) (expected: vdecl list) =
  match actual, expected with
  | _,[] -> true               
  | EmptyTEnv,_::_ -> false
  | ExtendTEnv(ida,tya,tenv),(AValDecl(ide,tye))::ys ->
    if ida=ide
    then tya=tye && is_subtype tenv ys
    else is_subtype tenv ((AValDecl(ide,tye))::ys)
  | _,_ -> failwith "Case not reachable"

let var_decls_to_tenv var_decls:tenv =
  List.fold_left (fun env (AValDecl(var,decl))  ->
      ExtendTEnv(var, decl,env)) EmptyTEnv var_decls
  
let rec
  type_of_prog (AProg (ms,e)) = type_of_expr (add_module_type_definitions (init_tenv ()) ms) e
and
  type_of_module_body en (AModBody vdefs):tenv =
  reverse_tenv @@ snd @@ List.fold_left (fun (env,renv) (AValDef(var,decl))  ->
      let ty = type_of_expr env decl
          in (ExtendTEnv(var, ty,env),ExtendTEnv(var, ty,renv))) (en,EmptyTEnv) vdefs
and
  (*  code for when interface of modules are not type-checked 
   *
   * add_module_type_definitions env ms:tenv =
   * List.fold_left (fun en (AMod(mname,minterface,mbody)) ->
   *     ExtendTEnvMod(mname,type_of_module_body en mbody,en) ) env ms *)
  add_module_type_definitions tenv:(mdecl list->tenv)  = function
  | [] -> tenv
  | AMod(mname,ASimpleInterface(expected_iface),mbody)::ms ->
    let i_body = type_of_module_body tenv mbody
    in if (is_subtype i_body expected_iface)
    then 
         add_module_type_definitions
        (ExtendTEnvMod(mname,var_decls_to_tenv expected_iface,tenv))
        ms
    else raise (Subtype_failure(mname))
and
  type_of_expr en = function 
  | Int n          -> IntType
  | Var id          ->
    (match apply_tenv en id with
    | None -> failwith @@ "Variable "^id^" undefined"
    | Some texp -> texp)
  | QualVar(module_id,var_id) -> 
    (match apply_tenv_qual en module_id var_id with
     | None -> failwith @@ "Variable "^var_id^" undefined"
     | Some texp ->  texp)
  | ITE(e1, e2, e3)    ->
    let t1 = type_of_expr en e1 
    in let t2 = type_of_expr en e2
    in let t3 = type_of_expr en e3 
    in if t1=BoolType && t2=t3 
    then t2
    else failwith "ITE: Type error"
  | Add(e1, e2) | Mul(e1,e2) | Sub(e1,e2) | Div(e1,e2)    ->
    let t1 = type_of_expr en e1 in
    let t2 = type_of_expr en e2  in
    if t1=IntType && t2=IntType
    then IntType
    else failwith "Add: arguments must be ints"
  | IsZero(e) ->
    let t1 = type_of_expr en e  in
    if t1=IntType
    then BoolType
    else failwith "Zero?: argument must be int"
  | Let(x, e1, e2) ->
    let t1 = type_of_expr en e1
    in type_of_expr (extend_tenv x t1 en) e2
  | Proc(x,ty,e)      ->
    let tc= type_of_expr (extend_tenv x ty en) e
    in FuncType(ty,tc)
  | App(e1,e2)     ->
    let t1 = type_of_expr en e1 
    in let t2 = type_of_expr en e2 
    in (match t1 with
    | FuncType(td,tcd) when td=t2 -> tcd 
    | FuncType(td,tcd) -> failwith "App: argument does not have correct type" 
    | _ -> failwith "App: LHS must be function type")
  | Letrec(tRes,id,param,tParam,body,e) ->
    let t=type_of_expr (extend_tenv param tParam
                          (extend_tenv id (FuncType(tParam,tRes)) en))
        body
    in if t=tRes 
    then type_of_expr (extend_tenv id (FuncType(tParam,tRes)) en) e
    else failwith
        "LetRec: Types of recursive function does not match declaration"
  | Set(id,e) ->
      failwith "EXPLICIT-REFS: Set not a valid operation"
  | BeginEnd(es) ->
    List.fold_left (fun v e -> type_of_expr en e) UnitType es
  | NewRef(e) ->
    let t=type_of_expr en e
    in RefType(t)
  | DeRef(e) ->
    let t1 = type_of_expr en e
    in (match t1 with
    | RefType(t) -> t
    | _ -> failwith "DeRef: Must deref a ref type")             
  | SetRef(e1,e2) ->
    let t1=type_of_expr en e1
    in let t2=type_of_expr en e2
    in (match t1 with
    | RefType tval when tval=t2 -> UnitType
    | _ -> failwith "SetRef: type of LHS and RHS do not match")
  | Open(module_id,e) -> (match lookup_module_type en module_id with
      | None ->  failwith "Module not defined"
      | Some tenv -> type_of_expr (append_tenv en tenv) e)
  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv en;
    UnitType          
    


