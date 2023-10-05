(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ==================================
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF:
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     1.5.2005
**    $VERSION:     1.04
**
**    $INFO:
**
**  Shell module: common types and structures
**
**    $ENDOFINFO
**
*)

open Amoeba


(*
** Formatted string output specified with a printf like format string with
** additonal features.
**
** The format string:
**
**  %n: entry name
**  %c: row capability of the entry
**  %r: column rights
**  %i: info request 
**  %t: creation time
**  %s: status request
**  %h: table info header
**  %#: space width between table columns
**
**  These entry formatter can be extended with control specifiers:
**
**  %<length><justification>[n,c,...]
** 
*)

type format_type =
    | Format_name           (* n *)
    | Format_cap            (* c *)
    | Format_rights         (* r *)
    | Format_time           (* t *)
    | Format_info           (* i *)
    | Format_status         (* s *)
    | Format_header         (* h *)
    | Format_pad            (* # *)

type format_just =
    | Just_left             (* l *)
    | Just_right            (* r *)
    | Just_center           (* c *)

type format_spec = {
    fs_type: format_type;
    fs_len : int;
    fs_just: format_just;
}

type obj_type = 
    | File_obj
    | Dir_obj
    | Cap_obj       (* neither a file nor a directory - just handle cap *)
    | Nil_obj 

(*
** Default RPC timeout for Shell_dir module (msec)
** The copy and delete functions will use the double value.
*)
let shell_dir_timeout = ref 500

exception Format_error of string
exception Shell_ex of (status * string)


let shell_log_fun = ref (fun (str:string) -> ())

(*
** Set an optional shell log print function called on
** each shell operation.
*)

let shell_set_log func = 
    shell_log_fun := func


(*
** Parse the format string and return a list of all format
** specifiers.
*)

let parse_format fmt =
    let get_params str =
        if str <> "" then
        begin
            let len' = String.length str in
            let fmt_lens = ref "" in
            let fmt_juss = ref ' ' in
            String.iter (fun c ->
                let str1 = " " in
                str1.[0] <- c;
                match c with
                | '0'..'9' -> fmt_lens := !fmt_lens ^ str1;
                | 'l' | 'r' | 'c' -> if !fmt_juss = ' ' 
                                        then fmt_juss := c
                                        else raise (Format_error str);
                | _ -> raise (Format_error str);
                ) str;
            let n = ref 0 in
            if (protects (n := int_of_string !fmt_lens)) = false
                then raise (Format_error str);
            match !fmt_juss with
            | 'l' -> !n, Just_left;
            | 'r' -> !n, Just_right;
            | 'c' -> !n, Just_center;
            | _ -> !n, Just_left;
        end
        else
            0,Just_left 
        in

    let fmtl = Str.split (Str.regexp " ") fmt in
    let fmts = ref [] in
    List.iter (fun f ->
            if f.[0] <> '%' then
                raise (Format_error f);
            (*
            ** Last char determines format type
            *)
            let len = String.length f in
            let more = String.sub f 1 (len-2) in
            let fmt_len,fmt_just = get_params more in
            fmts := !fmts @ [{fs_type= (
                                match f.[len-1] with
                                | 'n' -> Format_name;
                                | 'r' -> Format_rights;
                                | 'c' -> Format_cap;
                                | 'i' -> Format_info;
                                | 's' -> Format_status;
                                | 't' -> Format_time;
                                | 'h' -> Format_header;
                                | '#' -> Format_pad;
                                | _ -> raise (Format_error f));
                              fs_len = fmt_len;
                              fs_just = fmt_just;}];
        ) fmtl;
    !fmts


let print_box len just str skip =
    let len' = String.length str in
    let len' = if len < len' && skip then len else len' in
    if len = 0 || len < len' then str else
    begin
        let box = String.create len in
        for i = 0 to len-1 do box.[i] <- ' '; done;
        match just with
        | Just_left   -> String.blit str 0 box 0 len'; box;
        | Just_right  -> String.blit str 0 box (len-len') len'; box;
        | Just_center -> let mid = max ((len/2) - (len'/2)) 0 in
                         String.blit str 0 box mid len'; box;
    end     

let nchars n c =
    let str = String.create n in
    for i = 0 to n-1 do str.[i] <- c; done;
    str


let print str =
    print_string str;
    print_newline ()


(*
** resolve '*' name string filter
*)
let is_filter name mask =
    let name_len = String.length name in
    let maskl = Str.split (Str.regexp "\*") mask in

    let end_star = let len' = String.length mask in
                           (mask.[len'-1] = '*') in
    let start_star = (mask.[0] = '*') in

    let first = ref true in

    let last_pos = ref 0 in
    let rec search li =
        match li with
        | hd::tl ->
            let mask_len = String.length hd in
            let pos = ref 0 in
            let mask = Str.global_replace (Str.regexp "\.") "\." hd in
            let found = protects (pos := Str.search_forward
                                                    (Str.regexp mask)
                                                       name !last_pos) in
            if not found then raise Not_found;

            last_pos := !pos + mask_len;

            (*
            ** Check first and last star filter
            *)
            if !first then
            begin
                first := false;
                if not start_star && !pos > 0 
                    then raise Not_found;
            end;

            if tl = [] then
            begin
                if not end_star && !last_pos < name_len
                    then raise Not_found;
            end;

            search tl;
        | [] -> ();      
                in

        let is_match = protects (search maskl) in
        is_match
