type 'a bnode = {data: 'a;
                 mutable left : 'a bnode option;
                 mutable right : 'a bnode option}

type 'a tree = { mutable root : 'a bnode option}

let new_btree = fun () ->
    {root = None}

let join_btree = fun lt rt d ->
    {root = Some { data= d; left = lt.root; right = rt.root}}

let mirror_btree = fun t ->
    let rec mirror_btree' = function
        | None -> None
        | Some n -> n.left <- mirror_btree' n.right
    t.root <- mirror_btree' t.root


