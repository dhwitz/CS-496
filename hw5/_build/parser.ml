
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNITTYPE
    | TYPE
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | REFTYPE
    | RBRACE
    | RANGLE
    | PROC
    | PLUS
    | PIPE
    | OF
    | NEWREF
    | MINUS
    | LPAREN
    | LETREC
    | LET
    | LBRACE
    | LANGLE
    | ISZERO
    | INTTYPE
    | INT of (
# 22 "parser.mly"
       (int)
# 35 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "parser.mly"
       (string)
# 42 "parser.ml"
  )
    | EQUALS
    | EOF
    | END
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | CONSTRUCTOR of (
# 24 "parser.mly"
       (string)
# 54 "parser.ml"
  )
    | COMMA
    | COLON
    | CASE
    | BOOLTYPE
    | BEGIN
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState139
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState131
  | MenhirState129
  | MenhirState127
  | MenhirState125
  | MenhirState123
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState100
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState52
  | MenhirState49
  | MenhirState44
  | MenhirState43
  | MenhirState41
  | MenhirState40
  | MenhirState38
  | MenhirState35
  | MenhirState31
  | MenhirState28
  | MenhirState25
  | MenhirState19
  | MenhirState15
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState3
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 150 "parser.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 162 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) = 
# 243 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 168 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) = 
# 144 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x )
# 178 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_cdecl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cs : (Ast.cdecl list)) = _v in
        let ((_menhir_stack, _menhir_s), (id : (
# 23 "parser.mly"
       (string)
# 194 "parser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 178 "parser.mly"
                                  ( TypeDecl(id,cs) )
# 201 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.cdecl list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.cdecl))) = _menhir_stack in
        let _v : (Ast.cdecl list) = 
# 223 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 212 "parser.ml"
         in
        _menhir_goto_nonempty_list_cdecl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_texpr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.texpr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.texpr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.texpr list) = 
# 243 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 230 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_texpr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.texpr list)) = _v in
        let _v : (Ast.texpr list) = 
# 144 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x )
# 240 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_texpr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | INTTYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | REFTYPE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | UNITTYPE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_goto_nonempty_list_branch_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.branch list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (body : (Ast.expr))), _), _, (bs : (Ast.branch list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 176 "parser.mly"
                                         ( Case(body,bs) )
# 289 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.branch))), _, (xs : (Ast.branch list))) = _menhir_stack in
        let _v : (Ast.branch list) = 
# 223 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 305 "parser.ml"
         in
        _menhir_goto_nonempty_list_branch_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 382 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 405 "parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 241 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 410 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_cdecl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cdecl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PIPE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | DIVIDED | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | OF | PLUS | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TYPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Ast.cdecl))) = _menhir_stack in
        let _v : (Ast.cdecl list) = 
# 221 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 435 "parser.ml"
         in
        _menhir_goto_nonempty_list_cdecl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_goto_loption_separated_nonempty_list_COMMA_texpr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (id : (
# 24 "parser.mly"
       (string)
# 457 "parser.ml"
        ))), _, (xs : (Ast.texpr list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.cdecl) = let es = 
# 232 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 465 "parser.ml"
         in
        
# 194 "parser.mly"
               (CDec(id,es) )
# 470 "parser.ml"
         in
        _menhir_goto_cdecl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_texpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 203 "parser.mly"
                                 ( t1 )
# 501 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ID _ | RANGLE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.texpr) = 
# 202 "parser.mly"
                                    ( FuncType(t1,t2) )
# 524 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.texpr) = 
# 204 "parser.mly"
                          ( RefType(t1) )
# 541 "parser.ml"
         in
        _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | INTTYPE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | REFTYPE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | UNITTYPE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.texpr))) = _menhir_stack in
            let _v : (Ast.texpr list) = 
# 241 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 578 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_texpr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | CASE ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | CONSTRUCTOR _v ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
                | DEBUG ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | DEREF ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | ID _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
                | IF ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | INT _v ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
                | ISZERO ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | LET ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | LETREC ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | LPAREN ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | NEWREF ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | PROC ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | SET ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | SETREF ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | TYPE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOLTYPE ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                        | ID _v ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
                        | INTTYPE ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                        | LPAREN ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                        | REFTYPE ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                        | UNITTYPE ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUALS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | CASE ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | CONSTRUCTOR _v ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                | DEBUG ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | DEREF ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | ID _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                | IF ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | INT _v ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
                | ISZERO ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | LET ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | LETREC ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | LPAREN ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | NEWREF ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | PROC ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | SET ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | SETREF ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | TYPE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x )
# 808 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 243 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 820 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_branch : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.branch) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Ast.branch))) = _menhir_stack in
        let _v : (Ast.branch list) = 
# 221 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 841 "parser.ml"
         in
        _menhir_goto_nonempty_list_branch_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 24 "parser.mly"
       (string)
# 852 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | LANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState87 in
            let _v : (string list) = 
# 142 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 915 "parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x )
# 939 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 243 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( x :: xs )
# 951 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONSTRUCTOR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | INTTYPE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | REFTYPE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | UNITTYPE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RANGLE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState6 in
                let _v : (Ast.texpr list) = 
# 142 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 1172 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_texpr__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | DIVIDED | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | OF | PIPE | PLUS | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 24 "parser.mly"
       (string)
# 1184 "parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.cdecl) = 
# 192 "parser.mly"
                             (CDec(id,[]) )
# 1190 "parser.ml"
             in
            _menhir_goto_cdecl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 201 "parser.mly"
               ( UnitType )
# 1214 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | INTTYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | REFTYPE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | UNITTYPE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | INTTYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | REFTYPE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | UNITTYPE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 199 "parser.mly"
              ( IntType )
# 1272 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 1279 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 23 "parser.mly"
       (string)
# 1287 "parser.ml"
    )) = _v in
    let _v : (Ast.texpr) = 
# 198 "parser.mly"
            ( UserType(id) )
# 1292 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 200 "parser.mly"
               ( BoolType )
# 1304 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState72 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1376 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 155 "parser.mly"
                                  ( Mul(e1,e2) )
# 1391 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | OF | PLUS | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 153 "parser.mly"
                                 ( Add(e1,e2) )
# 1410 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 156 "parser.mly"
                                    ( Div(e1,e2) )
# 1425 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | OF | PLUS | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 154 "parser.mly"
                                  ( Sub(e1,e2) )
# 1444 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState83 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONSTRUCTOR _v ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | CONSTRUCTOR _ | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (id : (
# 24 "parser.mly"
       (string)
# 1510 "parser.ml"
            ))), _, (xs : (string list))), _, (tgt : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.branch) = let es = 
# 232 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 1518 "parser.ml"
             in
            
# 189 "parser.mly"
                              (Branch(id,es,tgt) )
# 1523 "parser.ml"
             in
            _menhir_goto_branch _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | CONSTRUCTOR _ | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 24 "parser.mly"
       (string)
# 1548 "parser.ml"
            ))), _, (tgt : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.branch) = 
# 186 "parser.mly"
                                        (Branch(id,[],tgt) )
# 1554 "parser.ml"
             in
            _menhir_goto_branch _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState106 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState105 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [ x ] )
# 1625 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState108 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 163 "parser.mly"
                                      ( DeRef(e) )
# 1655 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState112 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | OF | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 165 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1810 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 161 "parser.mly"
                                       ( IsZero(e) )
# 1840 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState117 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | OF | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1929 "parser.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 157 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 1937 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState120 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | OF | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((((_menhir_stack, _menhir_s), _, (tr : (Ast.texpr))), (x : (
# 23 "parser.mly"
       (string)
# 2024 "parser.ml"
            ))), (y : (
# 23 "parser.mly"
       (string)
# 2028 "parser.ml"
            ))), _, (targ : (Ast.texpr))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 158 "parser.mly"
                                                                                                            ( Letrec(tr,x,y,targ,e1,e2) )
# 2039 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 170 "parser.mly"
                                      ( Sub(Int 0, e) )
# 2069 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 168 "parser.mly"
                               (e)
# 2128 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState127 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 160 "parser.mly"
                                           ( App(e1,e2) )
# 2165 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState129 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 162 "parser.mly"
                                       ( NewRef(e) )
# 2197 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState131 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 2225 "parser.ml"
            ))), _, (t : (Ast.texpr))), _, (e : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 159 "parser.mly"
                                                                             ( Proc(x,t,e) )
# 2236 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | OF | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TYPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 2263 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 166 "parser.mly"
                                    ( Set(x,e) )
# 2270 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState134 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState136 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 164 "parser.mly"
                                                          ( SetRef(e1,e2) )
# 2363 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState139 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 89 "parser.mly"
       (Ast.prog)
# 2388 "parser.ml"
            ) = 
# 121 "parser.mly"
                 ( AProg e )
# 2392 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 89 "parser.mly"
       (Ast.prog)
# 2399 "parser.ml"
            )) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | TIMES ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (id : (
# 24 "parser.mly"
       (string)
# 2429 "parser.ml"
        ))), _, (xs : (Ast.expr list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.expr) = let es = 
# 232 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 2436 "parser.ml"
         in
        
# 174 "parser.mly"
             (Variant(id,es) )
# 2441 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs : (Ast.expr list)) = _v in
    let _v : (Ast.expr list) = let es = 
# 232 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( xs )
# 2459 "parser.ml"
     in
    
# 183 "parser.mly"
                                            ( es )
# 2464 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (es : (Ast.expr list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 167 "parser.mly"
                             ( BeginEnd(es) )
# 2481 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PIPE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOLTYPE ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | ID _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
                | INTTYPE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | LPAREN ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | REFTYPE ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | UNITTYPE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState41 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState41 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 171 "parser.mly"
                     ( Unit )
# 3125 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | INTTYPE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REFTYPE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UNITTYPE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CASE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CONSTRUCTOR _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | DEBUG ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | DEREF ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ID _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | INT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | ISZERO ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LET ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LETREC ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NEWREF ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | PROC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SET ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SETREF ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | TYPE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 3289 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 22 "parser.mly"
       (int)
# 3297 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 150 "parser.mly"
              ( Int i )
# 3302 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 3354 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 23 "parser.mly"
       (string)
# 3362 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 151 "parser.mly"
             ( Var x )
# 3367 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 152 "parser.mly"
            ( Debug )
# 3435 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 24 "parser.mly"
       (string)
# 3442 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | CASE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | CONSTRUCTOR _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | DEBUG ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DEREF ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ID _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | ISZERO ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LET ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LETREC ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NEWREF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PROC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SETREF ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | TYPE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState65 in
            let _v : (Ast.expr list) = 
# 142 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 3494 "parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | BEGIN | CASE | COMMA | CONSTRUCTOR _ | DEBUG | DEREF | DIVIDED | ELSE | END | EOF | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | OF | PLUS | PROC | RANGLE | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TYPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (id : (
# 24 "parser.mly"
       (string)
# 3506 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 172 "parser.mly"
                       (Variant(id,[]) )
# 3511 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState67 in
        let _v : (Ast.expr list) = 
# 142 "/home/vid/.opam/default/lib/menhir/standard.mly"
    ( [] )
# 3612 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 89 "parser.mly"
       (Ast.prog)
# 3635 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CASE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CONSTRUCTOR _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | DEBUG ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/home/vid/.opam/default/lib/menhir/standard.mly"
  

# 3691 "parser.ml"
