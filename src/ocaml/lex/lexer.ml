# 17 "lexer.mll"
 
open Syntax
open Parser

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
and comment_depth = ref 0

exception Lexical_error of string * int * int

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length !string_buff then begin
    let new_buff = String.create (String.length !string_buff * 2) in
    String.blit !string_buff 0 new_buff 0 (String.length !string_buff);
    string_buff := new_buff
  end;
  !string_buff.[!string_index] <- c;
  incr string_index

let get_stored_string () =
  String.sub !string_buff 0 !string_index

let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c

let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
               10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                    (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))

let line_num = ref 1
let line_start_pos = ref 0

let handle_lexical_error fn lexbuf =
  let line = !line_num
  and column = Lexing.lexeme_start lexbuf - !line_start_pos in
  try
    fn lexbuf
  with Lexical_error(msg, _, _) ->
    raise(Lexical_error(msg, line, column))
let lex_tables = {
  Lexing.lex_base = 
   "\000\000\010\000\004\000\012\000\246\255\247\255\253\255\053\000\
    \005\000\007\000\248\255\254\255\255\255\014\000\252\255\098\000\
    \016\000\108\000\118\000\017\000\249\255\250\255\251\255\250\255\
    \255\255\167\000\172\000\174\000\253\255\177\000\187\000\033\000\
    \015\000\255\255\254\255\019\000\243\000\020\000\197\000\006\001\
    \021\000\233\255\179\000\254\255\251\255\034\000\020\000\237\255\
    \241\255\239\255\235\255\246\255\240\255\039\001\243\255\242\255\
    \236\255\244\255\247\255\245\255\234\255\123\001\025\000\207\001\
    \088\000\016\001\217\001\089\000\019\001";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\000\
    \009\000\009\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\006\000\255\255\001\000\255\255\255\255\255\255\009\000\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\022\000\017\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000";
  Lexing.lex_default = 
   "\041\000\004\000\020\000\004\000\000\000\000\000\000\000\013\000\
    \255\255\255\255\000\000\000\000\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\035\000\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\000\000\062\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\042\000\043\000\000\000\042\000\042\000\023\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\000\000\005\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \042\000\000\000\044\000\000\000\000\000\000\000\024\000\045\000\
    \046\000\047\000\048\000\049\000\006\000\050\000\006\000\012\000\
    \011\000\031\000\032\000\007\000\008\000\022\000\009\000\021\000\
    \020\000\020\000\014\000\022\000\021\000\051\000\028\000\052\000\
    \021\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\054\000\014\000\055\000\056\000\057\000\
    \025\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\058\000\059\000\036\000\063\000\020\000\
    \010\000\000\000\000\000\000\000\000\000\033\000\000\000\034\000\
    \000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \026\000\027\000\000\000\026\000\026\000\026\000\027\000\027\000\
    \026\000\026\000\027\000\027\000\068\000\000\000\016\000\068\000\
    \068\000\000\000\000\000\000\000\016\000\000\000\000\000\026\000\
    \000\000\028\000\000\000\000\000\026\000\000\000\027\000\000\000\
    \016\000\000\000\000\000\068\000\016\000\000\000\016\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\000\000\
    \060\000\000\000\000\000\028\000\022\000\000\000\000\000\000\000\
    \000\000\028\000\010\000\000\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
    \000\000\028\000\037\000\028\000\068\000\000\000\000\000\068\000\
    \068\000\255\255\255\255\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\068\000\000\000\255\255\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\000\000\000\000\000\000\000\000\061\000\037\000\
    \000\000\000\000\000\000\000\000\000\000\037\000\000\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\037\000\000\000\000\000\000\000\037\000\000\000\037\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\000\000\000\000\000\000\000\000\061\000\000\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\000\000\000\000\
    \000\000\000\000\061\000\000\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\064\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\064\000\000\000\000\000\000\000\000\000\
    \000\000\064\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\064\000\000\000\000\000\
    \000\000\064\000\000\000\064\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\002\000\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\002\000\000\000\
    \000\000\000\000\000\000\000\000\001\000\000\000\003\000\008\000\
    \009\000\001\000\001\000\003\000\003\000\013\000\003\000\016\000\
    \019\000\032\000\035\000\037\000\040\000\000\000\046\000\000\000\
    \062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\031\000\045\000\064\000\
    \067\000\255\255\255\255\255\255\255\255\001\000\255\255\001\000\
    \255\255\015\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\007\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \025\000\025\000\255\255\025\000\025\000\026\000\026\000\027\000\
    \026\000\026\000\027\000\027\000\042\000\255\255\015\000\042\000\
    \042\000\255\255\255\255\255\255\015\000\255\255\255\255\025\000\
    \255\255\025\000\255\255\255\255\026\000\255\255\027\000\255\255\
    \015\000\255\255\255\255\042\000\015\000\255\255\015\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\255\255\
    \000\000\255\255\255\255\025\000\002\000\255\255\255\255\255\255\
    \255\255\025\000\001\000\255\255\003\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\025\000\255\255\255\255\
    \255\255\025\000\036\000\025\000\068\000\255\255\255\255\068\000\
    \068\000\031\000\045\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\068\000\255\255\007\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\255\255\255\255\255\255\255\255\053\000\036\000\
    \255\255\255\255\255\255\255\255\255\255\036\000\255\255\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\036\000\255\255\255\255\255\255\036\000\255\255\036\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\255\255\255\255\255\255\255\255\053\000\255\255\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\061\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\255\255\255\255\
    \255\255\255\255\061\000\255\255\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\063\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\063\000\255\255\255\255\255\255\255\255\
    \255\255\063\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\063\000\255\255\255\255\
    \255\255\063\000\255\255\063\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255"
}

let rec main lexbuf = __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf state =
  match Lexing.engine lex_tables state lexbuf with
    0 -> (
# 74 "lexer.mll"
      main lexbuf )
  | 1 -> (
# 76 "lexer.mll"
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      main lexbuf )
  | 2 -> (
# 80 "lexer.mll"
      comment_depth := 1;
      handle_lexical_error comment lexbuf;
      main lexbuf )
  | 3 -> (
# 84 "lexer.mll"
      match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | s -> Tident s )
  | 4 -> (
# 92 "lexer.mll"
      reset_string_buffer();
      handle_lexical_error string lexbuf;
      Tstring(get_stored_string()) )
  | 5 -> (
# 96 "lexer.mll"
      Tchar(Char.code(Lexing.lexeme_char lexbuf 1)) )
  | 6 -> (
# 98 "lexer.mll"
      Tchar(Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) )
  | 7 -> (
# 100 "lexer.mll"
      Tchar(Char.code(char_for_decimal_code lexbuf 2)) )
  | 8 -> (
# 102 "lexer.mll"
      let n1 = Lexing.lexeme_end lexbuf
      and l1 = !line_num
      and s1 = !line_start_pos in
      brace_depth := 1;
      let n2 = handle_lexical_error action lexbuf in
      Taction({start_pos = n1; end_pos = n2;
               start_line = l1; start_col = n1 - s1}) )
  | 9 -> (
# 109 "lexer.mll"
           Tequal )
  | 10 -> (
# 110 "lexer.mll"
           Tor )
  | 11 -> (
# 111 "lexer.mll"
           Tunderscore )
  | 12 -> (
# 112 "lexer.mll"
           Tlbracket )
  | 13 -> (
# 113 "lexer.mll"
           Trbracket )
  | 14 -> (
# 114 "lexer.mll"
           Tstar )
  | 15 -> (
# 115 "lexer.mll"
           Tmaybe )
  | 16 -> (
# 116 "lexer.mll"
           Tplus )
  | 17 -> (
# 117 "lexer.mll"
           Tlparen )
  | 18 -> (
# 118 "lexer.mll"
           Trparen )
  | 19 -> (
# 119 "lexer.mll"
           Tcaret )
  | 20 -> (
# 120 "lexer.mll"
           Tdash )
  | 21 -> (
# 121 "lexer.mll"
           Tend )
  | 22 -> (
# 123 "lexer.mll"
      raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf),
              !line_num, Lexing.lexeme_start lexbuf - !line_start_pos)) )
  | n -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_main_rec lexbuf n

and action lexbuf = __ocaml_lex_action_rec lexbuf 1
and __ocaml_lex_action_rec lexbuf state =
  match Lexing.engine lex_tables state lexbuf with
    0 -> (
# 129 "lexer.mll"
      incr brace_depth;
      action lexbuf )
  | 1 -> (
# 132 "lexer.mll"
      decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else action lexbuf )
  | 2 -> (
# 135 "lexer.mll"
      reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      action lexbuf )
  | 3 -> (
# 140 "lexer.mll"
      action lexbuf )
  | 4 -> (
# 142 "lexer.mll"
      action lexbuf )
  | 5 -> (
# 144 "lexer.mll"
      action lexbuf )
  | 6 -> (
# 146 "lexer.mll"
      comment_depth := 1;
      comment lexbuf;
      action lexbuf )
  | 7 -> (
# 150 "lexer.mll"
      raise (Lexical_error("unterminated action", 0, 0)) )
  | 8 -> (
# 152 "lexer.mll"
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      action lexbuf )
  | 9 -> (
# 156 "lexer.mll"
      action lexbuf )
  | n -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_action_rec lexbuf n

and string lexbuf = __ocaml_lex_string_rec lexbuf 2
and __ocaml_lex_string_rec lexbuf state =
  match Lexing.engine lex_tables state lexbuf with
    0 -> (
# 160 "lexer.mll"
      () )
  | 1 -> (
# 162 "lexer.mll"
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf )
  | 2 -> (
# 166 "lexer.mll"
      store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf )
  | 3 -> (
# 169 "lexer.mll"
      store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf )
  | 4 -> (
# 172 "lexer.mll"
      raise(Lexical_error("unterminated string", 0, 0)) )
  | 5 -> (
# 174 "lexer.mll"
      store_string_char '\010';
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      string lexbuf )
  | 6 -> (
# 179 "lexer.mll"
      store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf )
  | n -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf n

and comment lexbuf = __ocaml_lex_comment_rec lexbuf 3
and __ocaml_lex_comment_rec lexbuf state =
  match Lexing.engine lex_tables state lexbuf with
    0 -> (
# 184 "lexer.mll"
      incr comment_depth; comment lexbuf )
  | 1 -> (
# 186 "lexer.mll"
      decr comment_depth;
      if !comment_depth = 0 then () else comment lexbuf )
  | 2 -> (
# 189 "lexer.mll"
      reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      comment lexbuf )
  | 3 -> (
# 194 "lexer.mll"
        comment lexbuf )
  | 4 -> (
# 196 "lexer.mll"
        comment lexbuf )
  | 5 -> (
# 198 "lexer.mll"
        comment lexbuf )
  | 6 -> (
# 200 "lexer.mll"
        comment lexbuf )
  | 7 -> (
# 202 "lexer.mll"
      raise(Lexical_error("unterminated comment", 0, 0)) )
  | 8 -> (
# 204 "lexer.mll"
      line_start_pos := Lexing.lexeme_end lexbuf;
      incr line_num;
      comment lexbuf )
  | 9 -> (
# 208 "lexer.mll"
      comment lexbuf )
  | n -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf n

;;
