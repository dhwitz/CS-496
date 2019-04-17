open Ast

let from_some = function
  | None -> failwith "from_some: None"
  | Some v -> v

(*  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;; *)
    
type tenv =
  | EmptyTEnv
  | ExtendTEnv of string*texpr*tenv
                    
let empty_tenv () = EmptyTEnv

let extend_tenv id t tenv = ExtendTEnv(id,t,tenv)

let rec dups = function
  | [] -> false
  | x::xs -> List.exists ((=) x) xs || dups xs

let rec apply_tenv (tenv:tenv) (id:string):texpr option =
  match tenv with
  | EmptyTEnv -> None
  | ExtendTEnv(id2,t,tenv2) ->
    if id=id2
    then Some t
    else apply_tenv tenv2 id
  
let init_tenv () =
     extend_tenv "x"  IntType 
     @@ extend_tenv "v" IntType
     @@ extend_tenv "i"  IntType
     @@ empty_tenv ()

let rec  string_of_tenv  = function
  | EmptyTEnv -> ""
  | ExtendTEnv(id,v,env) -> "("^id^","^string_of_texpr v^")"^string_of_tenv env




let rec type_of_prog = function
  | AProg e -> type_of_expr (empty_tenv ()) e
and
  type_of_expr tenv = function 
  | Int n -> IntType
  | Var id ->
    (match apply_tenv tenv id with
     | None -> failwith  "Variable not declared"
     | Some t -> t)
  | Add(e1,e2) ->
    let t1 = type_of_expr tenv e1
    in let t2 = type_of_expr tenv e2
    in if (t1=IntType && t2=IntType)
    then IntType
    else failwith "add: arguments do not have type int"
  | Let(id,e,body) ->
    let t1 = type_of_expr tenv e
    in let t2 = type_of_expr (extend_tenv id t1 tenv) body
    in t2
  | IsZero(e) ->
    let t = type_of_expr tenv e
    in if t=IntType
    then BoolType
      else failwith "zero: argument is not of type int"
  | ITE(e1,e2,e3) ->
    let t1 = type_of_expr tenv e1
    in let t2 = type_of_expr tenv e2
    in let t3 = type_of_expr tenv e3
    in if t1=BoolType
       then (if t2=t3
    then t2
    else failwith "ite: both branches must have same type")
       else failwith "ite: condition must be boolean"
  | Proc(id,t,body) ->
    FuncType(t,type_of_expr (extend_tenv id t tenv)  body)
  | Record(fs) -> 
    if dups (fst (List.split fs))
    then failwith "No duplicates allowed in a record."
    else record_helper (fst (List.split fs ) (snd (List.split fs )) tenv

  | Proj(e, field_id) -> 
    let t1 = type_of_expr tenv e
    in (match apply_tenv tenv field_id with
      | None -> failwith "Field not found in the record"
      | Some _ -> t1)
  | Debug ->
    print_string "Environment:\n";
    print_string @@ string_of_tenv tenv;
    UnitType
  | _ -> failwith "implement"          
and 
  rec record_helper = fun ids expresisons tenv
    |  [] -> []
    | x::xs, y::ys -> [(x, (type_of_expr tenv y))] @ record_helper xs ys tenv

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let chk (e:string) : texpr =
  e |> parse |> type_of_prog

let chk2 (e:string) : string =
  e |> parse |> type_of_prog |> Ast.string_of_texpr

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
(* 3 in call-by-name *)
(* 2 in call-by-need *)

let ex5 = "
let a = 3
in let p = proc(x) { set x = 4 }
in begin 
         (p a); 
         a 
       end"

let ex6 = "let p = proc(x:int) { 5-x } in (p 3)"
(* 2 *)
  
let ex7 = "
let a = 3
in let f = proc(x:int) { proc(y:int) { set x = x-y }}
in begin
((f a) 2);
a
end"
(* 1 *)

let ex8 = "
let swap = proc (x:int) { proc (y:int) {
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
             a-b
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

