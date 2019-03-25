
(* Mutable state in OCaml

a) references
b) mutable fields in records
c) arrays
*)

(* a) References and local state *)

let c =
  let state = ref 0
  in fun () -> state:= !state+1; !state

type stack = { push: int -> unit; pop: unit -> int }
             
let s=
  let state = ref []
  in { push = (fun n -> state:= n::!state) ;
       pop = (fun () -> let top = List.hd !state in state:=List.tl !state;top)}

(*
   b) We represent linked-lists and binary trees in OCaml just as you might do in Java 
*) 

(* ************* *)
(* linked-lists *)
(* ************* *)

(*
type node = { data : int;
              mutable next : node option}

type llist = { mutable head: node option;
               mutable size: int}

let new_ll : unit -> llist = fun () ->
  { head = None; size=0}

let add_first_ll : int -> llist -> unit = fun n ll ->
  ll.head <- Some {data=n; next=ll.head};
  ll.size<-ll.size+1 

let string_of_ll : llist -> string = fun ll ->
  let rec string_of_ll' = function
    | None -> ""
    | Some n -> string_of_int n.data^","^string_of_ll' n.next
  in
  string_of_ll' ll.head 
*)


(* ************* *)
(* binary trees *)
(* ************* *)

type 'a bnode = { data: 'a;
                  mutable left: 'a bnode option;
                  mutable right: 'a bnode option}

type 'a tree = { mutable root: 'a bnode option} 

let new_btree = fun () ->
  { root = None }

let join_btree :  'a tree -> 'a tree -> 'a -> 'a tree = fun lt rt d ->
  { root = Some { data=d; left = lt.root; right=rt.root} }

let mirror_btree : 'a tree  -> unit = fun t ->
  let rec mirror_btree' = function
    | None -> None
    | Some n ->
      let temp = mirror_btree' n.right
      in
      n.right <- mirror_btree' n.left;
      n.left <- temp;
      Some n
   in t.root <- mirror_btree' t.root
          
(* operations:
- prune:  int -> 'a btree -> 'a btree 
- string_of_btree : 'a btree -> string   pre-order traversal
*)
  


