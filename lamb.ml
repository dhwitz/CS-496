(*
type lterm =
    | Var of string
    | App of lterm*lterm
    | Lam of string*lterm

let l1 = Lam("x", Var "x")
let l2 = Lam("x", App(Var "x", Var "y"))

let rec fv = function
    | Var x -> [x]
    | App(e1, e2) -> fv e1 @ fv e2
    | Lam(v,e) -> remove (fv e) v
*)

type term = 
    | Var of string 
    | Lam of string*term
    | App of term*term

let t1 = App(Lam("x", Var"x"), Lam("y", Var"y"))

let tid = Lam("x", Var"x")

let t2 = App(Var"x", Lam("y", Var"y"))

let t3 = App(Lam("w", Var"w"), Lam("z", Var"z"))





let rec replace_variable = fun e sv tv -> 
    match e with
    | Var v when v=sv -> Var tv
    | Var v -> Var v
    | Lam(v, body) -> Lam(v, replace_variable body sv tv)
    | App(e1,e2) ->
        App(replace_variable e1 sv tv, replace_variable e2 sv tv)


let rec alpha = fun n e1 e2 -> 
    match e1,e2 with 
    | Var x, Var y -> (x=y, n)
    | App(e1, e2), App(e3,e4) ->
        let  (b1,n1) = alpha n e1 e3
        in let (b2, n2) = alpha n1 e2 e4
        in (b1 && b2, n2)
    | Lam(v1, body1), Lam(v2, body2) ->
        let freshvar = "_"^string_of_int n
        in let body1r = replace_variable body1 v1 freshvar
        in let body2r = replace_variable body2 v2 freshvar
        in alpha (n+1) body1r body2r
    | _,_ -> (false, n)

let rec subset = fun i m x n ->
    match m with
    | Var v when v = x -> (n,i)
    | Var v -> (Var v, i)
    | App(e1,e2) ->
        let (e1s, i1) = subset i e1 x n
        in let (e2s, i2) = subset i1 e2 x n
        in (App(e1s, e2s), i2)
    | Lam(v, body) ->
        let freshvar = "_"^string_of_int i
        in let bodyr = replace_variable body v freshvar
        in let (body_subset, i1) = subset (i+1) bodyr x n
        in (Lam(freshvar, body_subset), i1)

let rec eval = fun i e ->
    match e with 
    | Lam(v, body) -> (Lam(v, body), i)
    | App(m1, m2) ->
        let (lam(x,n), i1) = eval i m1
        in let (v, i2) = eval i1 m2
        in let (nsubst, i3) = subst i2 n x v
        in eval i3 nsubst
    | _ -> failwith "error: term incomplete"