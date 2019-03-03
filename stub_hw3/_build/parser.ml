
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TL
    | TIMES
    | THEN
    | RPAREN
    | RBRACE
    | PLUS
    | OF
    | NODE
    | MINUS
    | LPAREN
    | LET
    | LBRACE
    | ISZERO
    | INT of (
# 22 "parser.mly"
       (int)
# 24 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "parser.mly"
       (string)
# 31 "parser.ml"
  )
    | HD
    | EQUALS
    | EOF
    | EMPTYTREE
    | EMPTYLIST
    | EMPTY
    | ELSE
    | DIVIDED
    | CONS
    | COMMA
    | CASET
    | ARROW
    | ABS
  
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
  | MenhirState79
  | MenhirState77
  | MenhirState70
  | MenhirState65
  | MenhirState63
  | MenhirState55
  | MenhirState51
  | MenhirState40
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState16
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 92 "parser.ml"

let rec _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 144 "parser.mly"
                                    ( Abs(e) )
# 280 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 140 "parser.mly"
                                  ( Mul(e1,e2) )
# 299 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | MINUS | OF | PLUS | RBRACE | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 138 "parser.mly"
                                 ( Add(e1,e2) )
# 318 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 141 "parser.mly"
                                    ( Div(e1,e2) )
# 335 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | MINUS | OF | PLUS | RBRACE | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 139 "parser.mly"
                                  ( Sub(e1,e2) )
# 354 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EMPTYTREE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ARROW ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | ABS ->
                            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | CASET ->
                            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | CONS ->
                            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | EMPTY ->
                            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | EMPTYLIST ->
                            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | EMPTYTREE ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | HD ->
                            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | ID _v ->
                            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                        | IF ->
                            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | INT _v ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                        | ISZERO ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | LET ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | LPAREN ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | NODE ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | TL ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
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
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NODE ->
                let _menhir_stack = Obj.magic _menhir_stack in
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
                        | COMMA ->
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
                                | COMMA ->
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
                                        | RPAREN ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | ARROW ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | ABS ->
                                                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | CASET ->
                                                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | CONS ->
                                                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | EMPTY ->
                                                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | EMPTYLIST ->
                                                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | EMPTYTREE ->
                                                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | HD ->
                                                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | ID _v ->
                                                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                                                | IF ->
                                                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | INT _v ->
                                                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                                                | ISZERO ->
                                                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | LET ->
                                                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | LPAREN ->
                                                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | NODE ->
                                                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | TL ->
                                                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))), (id1 : (
# 23 "parser.mly"
       (string)
# 632 "parser.ml"
            ))), (id2 : (
# 23 "parser.mly"
       (string)
# 636 "parser.ml"
            ))), (id3 : (
# 23 "parser.mly"
       (string)
# 640 "parser.ml"
            ))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _19 = () in
            let _17 = () in
            let _16 = () in
            let _14 = () in
            let _12 = () in
            let _10 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 154 "parser.mly"
                              ( CaseT(e1,e2,id1,id2,id3,e3) )
# 658 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 149 "parser.mly"
                                                        ( Cons(e1, e2) )
# 750 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 148 "parser.mly"
                                      ( Empty(e) )
# 783 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 146 "parser.mly"
                                   ( Hd(e) )
# 816 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | OF | RBRACE | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 155 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 965 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 143 "parser.mly"
                                       ( IsZero(e) )
# 996 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | OF | RBRACE | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1083 "parser.ml"
            ))), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 142 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 1091 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 158 "parser.mly"
                                      ( Sub(Int 0, e) )
# 1122 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 156 "parser.mly"
                               (e)
# 1154 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 151 "parser.mly"
                                                                      ( Node(e1,e2,e3) )
# 1305 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 147 "parser.mly"
                                   ( Tl(e) )
# 1338 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 75 "parser.mly"
       (Ast.expr)
# 1364 "parser.ml"
            ) = 
# 107 "parser.mly"
                    ( e )
# 1368 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 75 "parser.mly"
       (Ast.expr)
# 1375 "parser.ml"
            )) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState5 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | ABS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | CASET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | CONS ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | EMPTY ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | EMPTYLIST ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | EMPTYTREE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | HD ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | ISZERO ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NODE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TL ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
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

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 1799 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 22 "parser.mly"
       (int)
# 1807 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 136 "parser.mly"
              ( Int i )
# 1812 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 1860 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 23 "parser.mly"
       (string)
# 1868 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 137 "parser.mly"
             ( Var x )
# 1873 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 150 "parser.mly"
                ( EmptyTree )
# 1937 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 145 "parser.mly"
                ( EmptyList )
# 1949 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ABS ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | CASET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | CONS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | EMPTY ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | EMPTYLIST ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | EMPTYTREE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | HD ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | ISZERO ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | NODE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | TL ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
# 75 "parser.mly"
       (Ast.expr)
# 2165 "parser.ml"
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
    | ABS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CASET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CONS ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTY ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYLIST ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYTREE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HD ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NODE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/home/vid/.opam/default/lib/menhir/standard.mly"
  

# 2217 "parser.ml"
