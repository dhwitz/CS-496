(* David Horowitz *)
(* I pledge my honor that I have abided by the Stevens Honor System. *)

type program = int list

(* A helper function for the colored function. Converts a program diretion into coordinate changes*)
let create_cords : int -> int*int -> int*int = fun com (x,y) ->
    match com with
    | 2 -> (x+1,y)
    | 3 -> (x, y+1)
    | 4 -> (x-1, y)
    | 5 -> (x, y-1)
    | _ -> (x,y)

(* A helper function for the colored function. Creates the coordinate list. *)
let rec colored_helper : int*int -> program -> (int*int) list = fun cords prog ->
    match prog with
    | [] -> []
    | x::xs -> 
        if x = 0 || x = 1
        then colored_helper cords xs
        else create_cords x cords :: colored_helper (create_cords x cords) xs

(* Cretaes a list of coordinates. This function removes dupliates. *)
let colored : int*int -> program -> (int*int) list = fun cords prog ->
    List.sort_uniq compare (colored_helper cords prog)

(* A helper function for the equivalent function. Uses colored as an intermediate. *)
let rec equivalent_helper : (int*int) list -> (int*int) list -> bool = fun list1 list2 ->
    match list1, list2 with
    | [], [] -> true
    | _::_, [] -> false
    | [], _::_ -> false
    | (x1,y1)::xs, (x2,y2)::ys ->
        if compare (x1,y1) (x2,y2) != 0
        then false 
        else equivalent_helper xs ys

(* Compares 2 programs and checks for equavelency. *)
let equivalent : program -> program -> bool = fun prog1 prog2 ->
    equivalent_helper (colored (0,0) prog1) (colored (0,0) prog2)

(* A helper function for the mirror image function. Has the moves needed to flip a program.*)
let mirror_fliper : int -> int = function
    | 2 -> 4
    | 3 -> 5
    | 5 -> 3
    | 4 -> 2
    | n -> n

(* Returns the mirror of a program, *)
let mirror_image : program -> program = fun prog ->
    List.map mirror_fliper prog

(*A helper function for the rotate 90 function. Has the moves needed to turn a program 90 degrees.*)
let rotate_helper : int -> int = function
    | 2 -> 5
    | 3 -> 2
    | 4 -> 3
    | 5 -> 4
    | n -> n

(* Rotates a program by 90 degrees clockwise. *)
let rotate_90 : program -> program = fun prog ->
    List.map rotate_helper prog

(* Returns a list with n copies of x. *)
let rec repeat : int -> 'a -> 'a list = fun n x ->
    match n with
    | 0 -> []
    | rep -> x :: repeat (n-1) x

(* Enlarges a program n-fold. *)
let rec pantograph : program -> int -> program = fun prog n -> 
    match prog with
    | [] -> []
    | x::xs -> 
        if x = 0 || x = 1
        then x :: pantograph xs n
        else (repeat n x) @ (pantograph xs n)

(* A helper function for the compress program. Returns the compressed program in reverse. *)
let rec compress_helper : program -> (int*int) list -> (int*int) list = fun prog accum ->
    match prog, accum with
    | [], [] -> []
    | x::xs, [] -> compress_helper xs [(1,x)]
    | [], _ -> accum
    | x::xs, ((yNum,y)::ys as yys) -> 
        if x = y
        then compress_helper xs ((yNum+1, y) :: ys)
        else compress_helper xs ((1,x) :: yys)
(* Compresses a program using tuples. Reverses the result from the helper function. *)
let compress : program -> (int*int) list = fun prog ->
    List.rev (compress_helper prog [])

(* Decompresses a program that was compressed with the compress function. *)
let rec decompress : (int*int) list -> program = fun accum ->
    match accum with
    | [] -> []
    | (xNum, x)::xs -> repeat xNum x @ decompress xs