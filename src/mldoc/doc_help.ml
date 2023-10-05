(* 
** This file is part of the MLDOC System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              08/08/2002
**
** Changes:
**
**
**
** MLDOC is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The MLDOC is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
*) 

(*
** Help frontend.
*)


open Doc_core 
open Doc_html
open Doc_latex
open Doc_text
open Printf

(*
** Prepare the document and split it into sections and lower parts.
*)

type h_sec = {
    mutable h_sec_name: string;
    mutable h_sec_keywords: string list;    (* keyword list              *)
    mutable h_sec_ds: structure_block ref;  (* the subdocument           *)
    mutable h_sec_env: section_names list;  (* upper section environment *)
    mutable h_type: string;
}

type s_help = {
    mutable h_sections: h_sec list;
    mutable h_subsections: h_sec list;
    mutable h_units: h_sec list;
    mutable h_main: structure_block;
}

(*
** Generate a valid file name from a string (for example a link).
*)

let file_name str =
    let rs = " \|\\\\\|,\|/\|;\|-\|+\|'\|#\|%\|&\|\"\|!\|(\|)\|[\|]\|{\|}"^
             "\|~\|ä\|ö\|ü\|Ö\|Ä\|Ü" in
    let rp = "\1" in
    Str.global_replace 
            (Str.regexp rs)
            rp
            str


let cur_help = ref None  

let syntax_error ds str =
    failwith (
        "help_of_doc: "^str^
        " in file "^(!(ds.s_name))^
        " line "^(sprintf "%d" ds.s_line)
    )


(*
** Split the main document in sections
*)

let help_of_doc ~ds =
    let sh = {
        h_sections = [];
        h_subsections = [];
        h_units = [];
        h_main = ds;
    } in
    
    let cur_env = ref [] in

    (*
    ** Extract all text from a structure element inclusive his childs.
    *)

    let str_of_struc s =
        let str = ref "" in

        let rec iter se =
            (
                match se.s_content with 
                | S_Text t -> List.iter (fun s ->
                                        str := !str ^ " " ^ s;
                          ) t;
                | _ -> List.iter iter se.s_childs;
            );
        in
        iter s;
        !str 
    in    

    let search_keys ds =
        let kl = ref [] in
        let rec iter de = 
            match de.s_content with
            | S_Name -> kl := !kl @ (
                                        Str.split (Str.regexp " ")
                                                   (str_of_struc de)
                                    );
            | _ -> List.iter iter de.s_childs;
        in
        iter ds;
        !kl
    in
    
    let rec cont_trans ds =
        (
            match ds.s_content with
            | S_S1 ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "Generic section";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "S1 without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    );                    

                    sh.h_sections <- sh.h_sections @ [news];

                    cur_env := [Sec_s1 sname] @ (!cur_env) ;
                end;
            | S_S2 ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "Generic subsection";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "S2 without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    );                    

                    sh.h_subsections <- sh.h_subsections @ [news];

                    cur_env := [Sec_s2 sname] @ (!cur_env) ;
                end;
            | S_S3 ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "Generic unit";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "S3 without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    );                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_s3 sname] @ (!cur_env) ;
                end;
            | S_Package ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "Package";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Package without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    );                    

                    sh.h_sections <- sh.h_sections @ [news];

                    cur_env := [Sec_package sname] @ (!cur_env) ;
                end;
            | S_Program ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "Program";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Program without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    );                    

                    sh.h_sections <- sh.h_sections @ [news];

                    cur_env := [Sec_program sname] @ (!cur_env) ;
                end;
            | S_Module ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "ML-Module";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Module without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_module sname] @ (!cur_env) ;
                end;
            | S_Function ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "ML-Function";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Function without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_function sname] @ (!cur_env) ;
                end;
            | S_Type ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "ML-Type";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Type without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_type sname] @ (!cur_env) ;
                end;
            | S_Value ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "ML-Value";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Value without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_val sname] @ (!cur_env) ;
                end;
            | S_Class ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "ML-Class";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "Class without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_class sname] @ (!cur_env) ;
                end;
            | S_C ->
                begin
                    let dsr = ref ds in
                    let news = {
                        h_sec_name = "";
                        h_sec_keywords = [];
                        h_sec_ds = dsr;
                        h_sec_env = !cur_env;
                        h_type = "C-Interface";
                    } in 

                    let ds1 = List.hd ds.s_childs in
                    let sname =
                        match ds1.s_content with
                        | S_Name -> str_of_struc ds1;
                        | _ -> syntax_error ds "C-Interface without name";
                    in

                    news.h_sec_name <- sname;
                    news.h_sec_keywords <- (
                        Str.split (Str.regexp " ") sname
                    ) @ (search_keys ds);                    

                    sh.h_units <- sh.h_units @ [news];

                    cur_env := [Sec_cint sname] @ (!cur_env) ;
                end;
            | _ -> ();        
        );
        
        if (ds.s_childs <> []) then
        begin
            List.iter cont_trans ds.s_childs;
        end;

        (
            match ds.s_content with
            | S_S1 
            | S_S2
            | S_S3
            | S_Package
            | S_Program
            | S_Module
            | S_Clib 
            | S_Function
            | S_Type
            | S_Value
            | S_Class
            | S_C
                ->
                    cur_env := List.tl (!cur_env);
            | _ -> ();        
        );
            
    in
    cont_trans ds;
    sh

(*
** Build the current help database from document 'ds'.
*)
     
let help_load ~ds =
    cur_help := Some (help_of_doc ds)        

(*
** Save a document tree in a file in binary format 
*)

let help_write ~fname =
    let oc = open_out fname in
    let ds = match !cur_help with
                | Some hs -> hs.h_main ;
                | None -> failwith "no help document loaded";
    in
    output_value oc ds;
    close_out oc

(*
** Read a document tree form a file in binary format
*)

let help_read ~fname =
    let ic = open_in fname in
    let ds = input_value ic in
    close_in ic;
    help_load ds

(*
** and load it from a file
*)

let help_file ~fname =
    let at = atoms_of_file fname in
    let ds = tree_of_atoms at in
    cur_help := Some (help_of_doc ds)        


(*
** Set the current help device 
*)

type help_device = 
    | Help_TTY
    | Help_ASCII 
    | Help_HTML
    | Help_LATEX
    
let cur_dev = ref Help_TTY

let help_dev ~dev =
    cur_dev := dev
    

(*
** A user request for the help database.
*)

exception Found

let help ~name = 
    let glue sl =
        let str = ref "" in
        List.iter (fun s -> str := !str^" "^s) sl;
        !str
    in

    (*
    ** Search the string s1 in string s2. Length of s1 must be lower
    ** or equal the length of s2. Returns 0..100 for percent of matching.
    *)
        
    let search s1 s2 =
        let s1_l = String.length s1 in
        let s2_l = String.length s2 in
        if (s1_l > s2_l) then
            0
        else
        begin
            if (s1_l=s2_l &&
                s1 = s2) then
                100
            else
            begin
                try
                (
                  for i = 0 to (s2_l-s1_l)-1
                  do
                    let s2_sub = String.sub s2 i (s1_l) in
                    if (s1 = s2_sub) then
                        raise Found;
                  done;
                  0
                )
                with
                    | Found -> (s1_l*100/s2_l);
                    | _ -> 0 ;
            end;
        end
    in    
        
        

    match !cur_help with
    | Some sh ->
    begin
        let sl = Str.split (Str.regexp " ") name in
        let found = ref [] in
        let num_sl = List.length sl in

        (*
        ** Search first the sections 
        *)
        List.iter (
            fun sec ->
                let num_eq = ref 0 in
                List.iter ( 
                    fun key ->
                        List.iter ( fun sl -> 
                            num_eq := !num_eq + (search sl key)
                        ) sl;          
                ) sec.h_sec_keywords;
                if (!num_eq > 0) then
                    found := !found @ [
                        sec,
                        (!num_eq/num_sl)];
        ) sh.h_sections;  

        (*
        ** Subsections
        *)
        List.iter (
            fun sec ->
                let num_eq = ref 0 in
                List.iter ( 
                    fun key ->
                        List.iter ( fun sl -> 
                            num_eq := !num_eq + (search sl key)
                        ) sl;          
                ) sec.h_sec_keywords;
                if (!num_eq > 0) then
                    found := !found @ [
                        sec,
                        (!num_eq/num_sl)];
        ) sh.h_subsections;  

        (*
        ** Units
        *)
        List.iter (
            fun sec ->
                let num_eq = ref 0 in
                List.iter ( 
                    fun key ->
                        List.iter ( fun sl -> 
                            num_eq := !num_eq + (search sl key)
                        ) sl;          
                ) sec.h_sec_keywords;
                if (!num_eq > 0) then
                    found := !found @ [
                        sec,
                        (!num_eq/num_sl)];
        ) sh.h_units;  

        if (!found <> []) then
        begin
            let i = ref 1 in
            let l = ref 1 in
            
            (
            try
              List.iter (
                fun sec_p ->
                    let sec,per = sec_p in
                    printf "  %3d. %s: %s (%d%%)\n"
                           (!i)
                           sec.h_type
                           sec.h_sec_name
                           per;
                    incr i;
                    incr l;
                    if (!l>20) then
                    begin
                        output_string Pervasives.stdout
                                    "\n  --Continue-- ? [yY,nN] ";
                        let answer = read_line () in
                        (
                            match answer with
                            | "n" | "N" -> raise Exit;
                            | _ -> ();
                        );
                    end;
              ) !found;
            with Exit -> ();
            );
            
            output_string Pervasives.stdout
                            "\n>> ";
            let answer = read_line () in
            let choice = int_of_string answer in

            if (choice > 0 && choice <= (List.length !found)) then
            begin
                let sec,per = List.nth !found (choice-1) in
                let ds = sec.h_sec_ds in
                let secenv = sec.h_sec_env in
                let keys = sec.h_sec_keywords in
                let fname = (file_name (glue keys)) in
            
                let body = 
                {
                    s_parent = None;
                    s_childs = [!ds];
                    s_content = S_Body; 
                    s_attr = [];
                    s_line = 0;
                    s_name = (let r = ref "<BODY>" in r) ;
                                                                
                } in
                                
                match !cur_dev with
                | Help_TTY -> text_of_tree body [TEXT_terminal;TEXT_notoc]
                                           secenv;
                              
                | Help_ASCII -> text_of_tree body 
                                            [
                                                TEXT_notoc;
                                                TEXT_doc_name 
                                                    (fname^".txt")
                                            ]
                                            secenv;
                              
                | Help_LATEX -> tex_of_tree body
                                            [
                                                TEX_no_toc;
                                                TEX_head_inline;
                                                TEX_doc_name 
                                                    (fname^".tex")
                                            ]
                                            secenv;
                              
                | Help_HTML -> html_of_tree body 
                                            [
                                                HTML_single_doc 
                                                    (fname^".html")
                                            ]
                                            secenv;
                              
            end;                            
            
        end
        else
            output_string Pervasives.stdout
                          "No matching documents found!\n";

    end;
    | None -> failwith "help: No help document loaded."
    
    