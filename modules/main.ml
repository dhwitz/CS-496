

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



(* Interpret an expression *)
let interp (e:string) : Ds.exp_val =
  e |> parse |> Interp.eval_prog 

(* Type-check an expression *)
let chk (e:string) : Ast.texpr =
  e |> parse |> Checker.type_of_prog 

let interpf (filename:string) : Ds.exp_val =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; UnitVal
with End_of_file ->
  close_in chan;
  List.fold_left (fun ac s -> s^ac) "" !lines |> parse |>  Interp.eval_prog 
  
