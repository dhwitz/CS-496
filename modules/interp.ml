open Ast
open Ds


(* Auxiliary operations *)
    
let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

let snd (_,y) = y

(* The store *)
  
let g_store = Store.empty_store 20 (NumVal 0)

(* Initial environment *)

let init_env =
  ExtendEnv("i", (NumVal 1),
            ExtendEnv("v", (NumVal 5),
                      ExtendEnv("x", (NumVal 10),
                                EmptyEnv)))



let rec apply_proc f a =
  match f with
  | ProcVal (x,b,env) ->
    eval_expr (extend_env env x a) b
  | _ -> failwith "apply_proc: Not a procVal"
and
  eval_expr (en:env) (e:expr) :exp_val =
  match e with
  | Int n          -> NumVal n
  | Var id          ->
    (match apply_env en id with
     | None -> failwith @@ "Var: Variable "^id^" undefined"
     | Some ev -> ev)
  | QualVar(module_id,var_id) -> 
    (match apply_env_qual en module_id var_id with
     | None -> failwith @@ "QualVar: Variable "^var_id^" undefined"
                           ^ string_of_env en
     | Some ev ->  ev)
  | ITE(e1, e2, e3)    ->
    let v1 = eval_expr en e1 in
    if boolVal_to_bool v1
    then eval_expr en e2
    else eval_expr en e3
  | Add(e1, e2)    ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Mul(e1, e2)    ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Sub(e1, e2)    ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | IsZero(e) ->
    let v1 = eval_expr en e  in
    BoolVal (numVal_to_num v1=0)
  | Let(x, e1, e2) ->
    let v1 = eval_expr en e1
    in eval_expr (extend_env en x v1) e2
  | Proc(x,t,e)      -> ProcVal (x,e,en)
  | App(e1,e2)     ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2 in
    apply_proc v1 v2
  | Letrec(tr,id,param,tparam,body,e) ->
    eval_expr (ExtendEnvRec(id,param,body,en)) e
  | BeginEnd(es) ->
    List.fold_left (fun v e -> eval_expr en e) (NumVal 28) es
  | NewRef(e) ->
    RefVal(Store.new_ref g_store (eval_expr en e))
  | DeRef(e) ->
    let v1 = eval_expr en e
    in Store.deref g_store (refVal_to_int v1)
  | SetRef(e1,e2) ->
    let v1=eval_expr en e1
    in let v2=eval_expr en e2
    in Store.set_ref g_store (refVal_to_int v1) v2;
    UnitVal
  | Open(id,e) -> 
    (match lookup_module en id with
      | None -> failwith "Module does not exist"
      | Some env -> eval_expr (append_env en env) e)
  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_env en;
    print_string "\nStore:\n";
    List.iteri (fun i s -> print_string (string_of_int i^"->"
                                         ^s^"\n")) @@ List.map
      string_of_expval @@ Store.store_to_list g_store;
    UnitVal
  | _ -> failwith("Not implemented: "^string_of_expr e)
and
  add_module_definition (en:env) (AModBody vdefs):env =
    snd @@ List.fold_left (fun (globalEnv, localEnv) (AValDef(id, e)) -> 
      (ExtendEnv(id,(eval_expr globalEnv e), globalEnv), 
        ExtendEnv(id, (eval_expr globalEnv e), localEnv))) (en, empty_env ()) vdefs
and
  add_module_definitions (en:env) (ms:mdecl list):env =
    List.fold_left (fun acc (AMod(id, iface, body)) -> 
      ExtendEnvMod(id, add_module_definition en body, acc)) en ms
 and
   eval_prog (Ast.AProg(ms,e)) =
   eval_expr (add_module_definitions (empty_env ()) ms) e



(* Code examples *)

let ex1 = "
let x = 7  
in let y = 2 
   in let y = let x = x-1 
              in x-y 
      in (x-8)- y"

let ex2 = "
   let g = 
      let counter = 0 
      in proc(d:int) {
         begin 
           set counter = counter+1; 
           counter
         end
         }
   in (g 11)-(g 22)"

let ex3 = "
  let g = 
     let counter = newref(0) 
     in proc (d:int) {
         begin
          setref(counter, deref(counter)+1);
          deref(counter)
         end
       }
  in (g 11) - (g 22)"

let ex4 = "
   let g = 
      let counter = 0 
      in proc(d:int) {
         begin 
           set counter = counter+1; 
           counter
         end
         }
   in (proc (x:int) { x + x }
(g 0))"
(* 3 in call_by-name *)
(* 2 in call_by-need *)

let ex5 = "
let a = 3
in let p = proc(x) { set x = 4 }
in begin 
         (p a); 
         a 
       end"

let ex6 = "let p = proc(x) { 5-x } in (p 3)"
(* 2 *)

let ex7 = "
let a = 3
in let f = proc(x) { proc(y) { set x = x-y }}
in begin
((f a) 2);
a
end"
(* 1 *)

let ex8 = "
let swap = proc (x) { proc (y) {
                      let temp = x
                      in begin 
                          set x = y;
                          set y = temp
                         end
                      } 
            }
         in let a = 33
         in let b = 44
         in begin
             ((swap a) b);
             a_b
            end"
(* 11 *)

let ex9 = "
letrec fact (x) = if zero?(x) then 1 else x*(fact (x-1)) 
in (fact 7)"
(* 5040 *)

let ex10 = "
letrec infiniteLoop (x) = (infiniteLoop (x+1)) 
in let f = proc (z) { 11 }
in (f (infiniteLoop 0))"







(*
       ;; if the interpreter always called the typechecker, or put
       ;; only declared variables in the module, this would raise an
       ;; error.  Exercise: make this modification.
*)

let ex_modules_take_one_value_but_interface_bad = "
         module m interface []  body [u = 3]
         from m take u"
         (*
; this version for permissive interp
        error 3
; this version for strict interp
;         error error
        *)

let ex_modules_take_bad_value =
  "module m interface []  body [u = 3]
         from m take x"
(*error error *)       

let ex_modules_two_vals = "
module m
 interface 
  [u : int 
   v : int] 
 body
  [u = 44
   v = 33]
  (from m take u)-(from m take v)"
(*     int 11*)


let ex_modules_two_vals_bad_interface_1 =
  "module m interface [u : int v : bool]  
                  body [u = 44 v = 33]
         (from m take u)-(from m take v)"
(*        error 11*)

let ex_modules_extra_vals_are_ok_1 = "
          module m interface [x : int] body [x = 3 y = 4]
          from m take x"
(*        int 3*)

let ex_module_extra_vals_are_ok_2 = "
          module m interface [y : int] body [x = 3 y = 4]
          from m take y"
(*         int*)

let ex_modules_two_vals_bad_interface_14 =
  "module m interface 
            [v : int 
             u : bool]
          body 
           [v = zero?(0) u = 33]
         (from m take u)-(from m take v)"
(*      error*)


let ex_modules_check_let_1 =
  "module m interface      [u : int v : int]
                  body [u = 44  v = -(u,11)]
         -(from m take u, from m take v)"
(*     int 11*)

let ex_modules_check_let_2_0 =
  "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int] 
          body 
           [v = (from m1 take u)-11]
         (from m1 take u)-(from m2 take v)"
(*     int 11*)

let ex_modules_check_let_2_05 =
  "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int] body [v = (from m1 take u)-11]
         33"
(*      int 33)                       ; doesn't actually import
        anything *)

let ex_modules_check_let_2_1 =
  "module m1 interface [u : int] body [u = 44]
         module m2   
          interface [v : int] 
          body [v = (from m1 take u)-11]
         (from m1 take u)-(from m2 take v)"
(*        int 11*)

let ex_modules_20 = "
module m2
interface [v : int] 
body
[v = (from m1 take u) - 11]
module m1 interface [u : int] body [u = 44]
(from m1 take u) - (from m2 take v)"
(* error *)

