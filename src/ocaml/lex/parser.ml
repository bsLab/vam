type token =
    Tident of (string)
  | Tchar of (int)
  | Tstring of (string)
  | Taction of (Syntax.location)
  | Trule
  | Tparse
  | Tand
  | Tequal
  | Tend
  | Tor
  | Tunderscore
  | Teof
  | Tlbracket
  | Trbracket
  | Tstar
  | Tmaybe
  | Tplus
  | Tlparen
  | Trparen
  | Tcaret
  | Tdash
  | Tlet

open Parsing
# 18 "parser.mly"
open Syntax

(* Auxiliaries for the parser. *)

let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then Characters([Char.code (s.[n])])
    else Sequence(Characters([Char.code (s.[n])]), re_string (succ n))
  in re_string 0

let char_class c1 c2 =
  let rec cl n =
    if n > c2 then [] else n :: cl(succ n)
  in cl c1

let all_chars = char_class 0 255

let rec subtract l1 l2 =
  match l1 with
    [] -> []
  | a::r -> if List.mem a l2 then subtract r l2 else a :: subtract r l2
(* Line 29, file parser.ml *)
let yytransl_const = [|
  261 (* Trule *);
  262 (* Tparse *);
  263 (* Tand *);
  264 (* Tequal *);
  265 (* Tend *);
  266 (* Tor *);
  267 (* Tunderscore *);
  268 (* Teof *);
  269 (* Tlbracket *);
  270 (* Trbracket *);
  271 (* Tstar *);
  272 (* Tmaybe *);
  273 (* Tplus *);
  274 (* Tlparen *);
  275 (* Trparen *);
  276 (* Tcaret *);
  277 (* Tdash *);
  278 (* Tlet *);
    0|]

let yytransl_block = [|
  257 (* Tident *);
  258 (* Tchar *);
  259 (* Tstring *);
  260 (* Taction *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\004\000\
\007\000\007\000\009\000\009\000\008\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\010\000\010\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\007\000\001\000\000\000\005\000\000\000\003\000\000\000\003\000\
\003\000\002\000\003\000\000\000\002\000\001\000\001\000\001\000\
\001\000\003\000\002\000\002\000\002\000\003\000\002\000\003\000\
\001\000\002\000\001\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\031\000\005\000\000\000\000\000\000\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\025\000\016\000\017\000\014\000\015\000\000\000\
\000\000\000\000\000\000\012\000\000\000\006\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\020\000\021\000\
\000\000\013\000\000\000\000\000\000\000\000\000\018\000\000\000\
\024\000\000\000\011\000\028\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\010\000\013\000\041\000\016\000\028\000\
\029\000\034\000\048\000"

let yysindex = "\001\000\
\005\255\000\000\000\000\000\000\000\000\017\255\007\255\009\255\
\024\255\000\000\028\255\031\255\046\255\099\255\099\255\000\000\
\007\255\035\255\000\000\000\000\000\000\000\000\000\000\004\255\
\099\255\063\255\045\255\000\000\044\255\000\000\000\000\038\255\
\036\255\053\255\036\255\002\255\099\255\000\000\000\000\000\000\
\081\255\000\000\044\255\099\255\066\255\036\255\000\000\036\255\
\000\000\081\255\000\000\000\000"

let yyrindex = "\000\000\
\029\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\060\255\000\000\109\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\255\000\000\000\000\082\255\000\000\000\000\027\255\
\000\000\000\000\056\255\000\000\000\000\000\000\000\000\000\000\
\021\255\000\000\100\255\000\000\000\000\057\255\000\000\058\255\
\000\000\023\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\064\000\000\000\068\000\000\000\242\255\000\000\043\000\
\060\000\000\000\239\255"

let yytablesize = 119
let yytable = "\026\000\
\027\000\001\000\019\000\020\000\021\000\032\000\035\000\009\000\
\003\000\011\000\036\000\037\000\022\000\023\000\024\000\046\000\
\038\000\039\000\040\000\025\000\049\000\007\000\050\000\033\000\
\023\000\023\000\022\000\022\000\029\000\027\000\023\000\012\000\
\022\000\003\000\004\000\014\000\015\000\032\000\008\000\023\000\
\029\000\022\000\023\000\031\000\022\000\019\000\020\000\021\000\
\042\000\003\000\003\000\004\000\017\000\044\000\037\000\022\000\
\023\000\024\000\045\000\038\000\039\000\040\000\025\000\019\000\
\020\000\021\000\047\000\052\000\003\000\027\000\026\000\030\000\
\037\000\022\000\023\000\024\000\018\000\038\000\039\000\040\000\
\025\000\019\000\020\000\021\000\030\000\010\000\051\000\043\000\
\010\000\000\000\010\000\022\000\023\000\024\000\000\000\038\000\
\039\000\040\000\025\000\019\000\020\000\021\000\000\000\009\000\
\000\000\000\000\009\000\000\000\009\000\022\000\023\000\024\000\
\012\000\000\000\000\000\012\000\025\000\012\000\012\000"

let yycheck = "\014\000\
\015\000\001\000\001\001\002\001\003\001\002\001\024\000\001\001\
\004\001\001\001\025\000\010\001\011\001\012\001\013\001\033\000\
\015\001\016\001\017\001\018\001\019\001\005\001\037\000\020\001\
\004\001\005\001\004\001\005\001\002\001\044\000\010\001\008\001\
\010\001\005\001\005\001\008\001\006\001\002\001\022\001\019\001\
\014\001\019\001\022\001\009\001\022\001\001\001\002\001\003\001\
\004\001\004\001\022\001\022\001\007\001\010\001\010\001\011\001\
\012\001\013\001\021\001\015\001\016\001\017\001\018\001\001\001\
\002\001\003\001\014\001\002\001\009\001\014\001\014\001\014\001\
\010\001\011\001\012\001\013\001\013\000\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\017\000\004\001\044\000\028\000\
\007\001\255\255\009\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\001\001\002\001\003\001\255\255\004\001\
\255\255\255\255\007\001\255\255\009\001\011\001\012\001\013\001\
\004\001\255\255\255\255\007\001\018\001\009\001\010\001"

let yynames_const = "\
  Trule\000\
  Tparse\000\
  Tand\000\
  Tequal\000\
  Tend\000\
  Tor\000\
  Tunderscore\000\
  Teof\000\
  Tlbracket\000\
  Trbracket\000\
  Tstar\000\
  Tmaybe\000\
  Tplus\000\
  Tlparen\000\
  Trparen\000\
  Tcaret\000\
  Tdash\000\
  Tlet\000\
  "

let yynames_block = "\
  Tident\000\
  Tchar\000\
  Tstring\000\
  Taction\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 6 : 'header) in
    let _2 = (peek_val parser_env 5 : 'named_regexps) in
    let _4 = (peek_val parser_env 3 : 'definition) in
    let _5 = (peek_val parser_env 2 : 'other_definitions) in
    let _6 = (peek_val parser_env 1 : 'header) in
    Obj.repr((
# 65 "parser.mly"
          {header = _1;
           entrypoints = _4 :: List.rev _5;
           trailer = _6} ) : Syntax.lexer_definition))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Syntax.location) in
    Obj.repr((
# 71 "parser.mly"
          _1 ) : 'header))
; (fun parser_env ->
    Obj.repr((
# 73 "parser.mly"
          { start_pos = 0; end_pos = 0; start_line = 1; start_col = 0 } ) : 'header))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'named_regexps) in
    let _3 = (peek_val parser_env 2 : string) in
    let _5 = (peek_val parser_env 0 : 'regexp) in
    Obj.repr((
# 77 "parser.mly"
          Hashtbl.add named_regexps _3 _5 ) : 'named_regexps))
; (fun parser_env ->
    Obj.repr((
# 79 "parser.mly"
          () ) : 'named_regexps))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'other_definitions) in
    let _3 = (peek_val parser_env 0 : 'definition) in
    Obj.repr((
# 83 "parser.mly"
          _3::_1 ) : 'other_definitions))
; (fun parser_env ->
    Obj.repr((
# 85 "parser.mly"
          [] ) : 'other_definitions))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'entry) in
    Obj.repr((
# 89 "parser.mly"
          (_1,_3) ) : 'definition))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'case) in
    let _3 = (peek_val parser_env 0 : 'rest_of_entry) in
    Obj.repr((
# 93 "parser.mly"
          _2::List.rev _3 ) : 'entry))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'rest_of_entry) in
    Obj.repr((
# 95 "parser.mly"
          List.rev _2 ) : 'entry))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'rest_of_entry) in
    let _3 = (peek_val parser_env 0 : 'case) in
    Obj.repr((
# 99 "parser.mly"
          _3::_1 ) : 'rest_of_entry))
; (fun parser_env ->
    Obj.repr((
# 101 "parser.mly"
          [] ) : 'rest_of_entry))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'regexp) in
    let _2 = (peek_val parser_env 0 : Syntax.location) in
    Obj.repr((
# 105 "parser.mly"
          (_1,_2) ) : 'case))
; (fun parser_env ->
    Obj.repr((
# 109 "parser.mly"
          Characters all_chars ) : 'regexp))
; (fun parser_env ->
    Obj.repr((
# 111 "parser.mly"
          Characters [256] ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr((
# 113 "parser.mly"
          Characters [_1] ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr((
# 115 "parser.mly"
          regexp_for_string _1 ) : 'regexp))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'char_class) in
    Obj.repr((
# 117 "parser.mly"
          Characters _2 ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'regexp) in
    Obj.repr((
# 119 "parser.mly"
          Repetition _1 ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'regexp) in
    Obj.repr((
# 121 "parser.mly"
          Alternative(_1, Epsilon) ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'regexp) in
    Obj.repr((
# 123 "parser.mly"
          Sequence(_1, Repetition _1) ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'regexp) in
    let _3 = (peek_val parser_env 0 : 'regexp) in
    Obj.repr((
# 125 "parser.mly"
          Alternative(_1,_3) ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'regexp) in
    let _2 = (peek_val parser_env 0 : 'regexp) in
    Obj.repr((
# 127 "parser.mly"
          Sequence(_1,_2) ) : 'regexp))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'regexp) in
    Obj.repr((
# 129 "parser.mly"
          _2 ) : 'regexp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr((
# 131 "parser.mly"
          try
            Hashtbl.find named_regexps _1
          with Not_found ->
            prerr_string "Reference to unbound regexp name `";
            prerr_string _1;
            prerr_string "' at char ";
            prerr_int (Parsing.symbol_start());
            prerr_newline();
            exit 2 ) : 'regexp))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'char_class1) in
    Obj.repr((
# 143 "parser.mly"
          subtract all_chars _2 ) : 'char_class))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'char_class1) in
    Obj.repr((
# 145 "parser.mly"
          _1 ) : 'char_class))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : int) in
    let _3 = (peek_val parser_env 0 : int) in
    Obj.repr((
# 149 "parser.mly"
          char_class _1 _3 ) : 'char_class1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr((
# 151 "parser.mly"
          [_1] ) : 'char_class1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'char_class1) in
    let _2 = (peek_val parser_env 0 : 'char_class1) in
    Obj.repr((
# 153 "parser.mly"
          _1 @ _2 ) : 'char_class1))
(* Entry lexer_definition *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let lexer_definition (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Syntax.lexer_definition)
(* Line 157, file parser.mly *)

(* Line 304, file parser.ml *)
