(* 
** This file is part of the MLDOC System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              30/07/2002
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
** ASCII Text backend.
*)


open Doc_core 

(*
** Special structures with pre defined layout
*)

type fun_int = {
    mutable fun_name: structure_block list;
    mutable fun_retargs: structure_block list;
    mutable fun_args: structure_block list;
    mutable fun_curried: bool;
    mutable fun_comm: structure_block list;
}
and header_type = C_Header 
and hdr_int = {
    mutable hdr_name: structure_block list;
    mutable hdr_type: header_type;
    mutable hdr_comm: structure_block list;
}
and val_int = {
    mutable val_name: structure_block list;
    mutable val_args: structure_block list;
    mutable val_curried: bool;
    mutable val_comm: structure_block list;
}
and meth_int = {
    mutable meth_name: structure_block list;
    mutable meth_args: structure_block list;
    mutable meth_comm: structure_block list;
}
and obj_int = {
    mutable obj_name: structure_block list;
    mutable obj_args: structure_block list;
    mutable obj_comm: structure_block list;
}
and type_type = Type_List | Type_Structure | Type_Exception | Type_Empty
and type_int = {
    mutable type_name: structure_block list;
    mutable type_args: structure_block list;
    mutable type_type: type_type;
    mutable type_comm: structure_block list;
}
and class_int = {
    mutable class_name: structure_block list;
    mutable class_args: structure_block list;
    mutable class_obj: struc_special;
    mutable class_comm: structure_block list;
}
and table_int = {
    mutable table_name: structure_block list;
    mutable table_head: structure_block list;
    mutable table_rowlist: (structure_block list) list;
    mutable table_currow: structure_block list ref;    
    mutable table_cols: int;
    mutable table_norul: bool;
    mutable table_comm: structure_block list;
}
and struc_special = 
    | Spec_Table of table_int 
    | Spec_Hdr_Int of hdr_int
    | Spec_Fun_Int of fun_int 
    | Spec_Val_Int of val_int
    | Spec_Type_Int of type_int
    | Spec_Class_Int of class_int
    | Spec_Method_Int of meth_int
    | Spec_Object_Int of obj_int
    | Spec_None


(*
** Global document environment: all what the translater must currently
** know about special things. Depends on already translated 
** structure elements. 
*)


type list_type = List_Numbered | List_Unnumbered | List_Option 

type env = {
    
    mutable doc_name: string;
    mutable doc_to_stdout: bool;
    mutable doc_terminal: bool;
    mutable doc_notoc: bool;
        
    mutable ignore_head: bool;
    mutable ignore_childs: bool;

    (*
    ** Environments needing special treatment.
    *)

    mutable in_special: struc_special;
    mutable special_depth: int;

    mutable in_preform: bool;
    mutable in_example: bool;
    mutable in_interface: bool;

    mutable in_class: struc_special;
    mutable in_object: struc_special;
        

    mutable in_olist: int ref list;
    mutable in_ulist: int;
    mutable in_list: list_type list;

    mutable cur_package: section option;
    mutable cur_program: section option;

    mutable cur_module:  section list;   (* with submodules *)
    mutable cur_clib: section option;

    mutable cur_function: section option;
    mutable cur_type: section option;
    mutable cur_val: section option;
    mutable cur_class: section option;
    mutable cur_cint: section option;

    mutable cur_s1: section option;
    mutable cur_s2: section option;
    mutable cur_s3: section option;
    mutable cur_s4: section option;

    (*
    ** Table of content
    *)

    mutable toc: section;
    mutable cur_sec: section;

    mutable doc_indent  : int;
    mutable doc_col     : int;
    mutable doc_colwidth: int;
    mutable doc_row     : int;
    mutable doc_rowhight: int;
    

    mutable table_col   : int;
    mutable textwidth   : int;
}

type text_options = 
    | TEXT_doc_name of string
    | TEXT_terminal
    | TEXT_notoc

let cur_sb = ref empty_block

let syntax_error s =
    failwith (
            "text_of_tree: "^
            s^
            " in "^
            (!((!cur_sb).s_name))^
            " line "^
            (string_of_int (!cur_sb.s_line))
    )




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


(*
** Transform the structure tree to Text output.
**
** Argument: 
**
**  ds: structure tree
**
*)


let text_of_tree ~ds ~options ~sections =

    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = "main"; sec_type = ""} 
    in  

    let env = {
        doc_to_stdout   = true;
        doc_terminal    = false;
        doc_notoc       = false;
        doc_name        = "";
        ignore_head     = false;
        ignore_childs   = false;
        in_special      = Spec_None;
        special_depth   = 0;

        in_preform      = false;
        in_example      = false;
        in_interface    = false;
        in_class        = Spec_None;
        in_object       = Spec_None;
        
        in_olist        = [];
        in_ulist        = 0;
        in_list        = [];

        cur_package     = None;
        cur_program     = None;
        cur_module      = [];
        cur_clib        = None;
        cur_function    = None;
        cur_type        = None;
        cur_val         = None;
        cur_class       = None;
        cur_cint        = None;
        cur_s1          = None;
        cur_s2          = None;
        cur_s3          = None;
        cur_s4          = None;

        toc             = doc_head;
        cur_sec         = doc_head;
        
        doc_indent      = 0;
        doc_col         = 0;
        doc_row         = 0;
        doc_rowhight    = 20;
        doc_colwidth    = 80;

        table_col       = 0;
        textwidth       = 80;
    } in

    (*
    ** options
    *)

    List.iter (fun o ->
                match o with
                | TEXT_doc_name s  ->   env.doc_name <- s;
                                        env.doc_to_stdout <- false;
                | TEXT_terminal -> env.doc_terminal <- true;
                | TEXT_notoc -> env.doc_notoc <- true;
              ) options;


    (*
    ** External sections
    *)

    if (sections <> []) then
    List.iter (fun s ->
        let sec = 
            match s with
            | Sec_s1 s -> 
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type=""} in
                         env.cur_s1 <- Some sec;
                         sec;
            | Sec_s2 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type=""} in
                         env.cur_s2 <- Some sec;
                         sec;
            | Sec_s3 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type=""} in
                         env.cur_s3 <- Some sec;
                         sec;
            | Sec_s4 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type=""} in
                         env.cur_s4 <- Some sec;
                         sec;
            | Sec_package s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="Package"} in
                         env.cur_package <- Some sec;
                         sec;
            | Sec_program s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="Program"} in
                         env.cur_program <- Some sec;
                         sec;
            | Sec_module s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="ML-Module"} in
                         env.cur_module <- [sec]@env.cur_module;
                         sec;
            | Sec_function s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="ML-Function"} in
                         env.cur_function <- Some sec;
                         sec;
            | Sec_type s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="ML-Type"} in
                         env.cur_type <- Some sec;
                         sec;
            | Sec_val s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="ML-Value"} in
                         env.cur_val <- Some sec;
                         sec;
            | Sec_class s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="ML-Class"} in
                         env.cur_class <- Some sec;
                         sec;
            | Sec_cint s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name=s;sec_type="C"} in
                         env.cur_cint <- Some sec;
                         sec;
        in
        env.cur_sec.sec_childs <- env.cur_sec.sec_childs @ [sec];
        env.cur_sec <- sec;

        ) sections;
    


    (*
    ** Util functions
    *)


    let ulist_mark ul = 
        match ul with
        | 1 -> "*";
        | 2 -> "+";
        | 3 -> "-";
        | _ -> "#";
    in

    (*
    ** current output channel
    *)

    let oc_chan,oc_name = (
                            if env.doc_to_stdout = true then
                                Pervasives.stdout
                            else
                                open_out env.doc_name 
                          ),env.doc_name
    in


    (*
    ** Printing
    *)

    let spaces num =
        let fmt = "%"^(string_of_int num)^"s" in
        Printf.sprintf (Obj.magic fmt) ""
    in
    let fill num c =
        let str = String.create num in
        for i = 0 to num-1
        do
            str.[i] <- c;
        done;
        str
    in

    (*
    ** Text newline and row counter 
    *)
    let nl' () =
        if (env.doc_terminal = true) then
        begin
                env.doc_row <- env.doc_row + 1;
                if (env.doc_row > env.doc_rowhight) then
                begin
                    output_string oc_chan "\n  --Continue-- ? [yY,nN] ";
                    let answer = read_line () in
                    (
                        match answer with
                        | "n" 
                        | "N" -> raise Exit;
                        | _ -> ();
                    );        
                    Terminfo.backup 1;
                    output_string oc_chan (spaces (env.doc_colwidth-1));
                    Terminfo.backup 1;

                    env.doc_row <- 1;
                end;
        end;
        "\n";
    in
    
    let out_str str =
        let slen = String.length str in
        let oc = oc_chan in
        if (env.doc_col + slen > env.doc_colwidth-1) then
        begin
            output_string oc ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        output_string oc str;
        env.doc_col <- env.doc_col + slen;
    in

    (*
    ** Fuzzy text newline
    *)
    
    let nl () =
        let oc = oc_chan  in
        if (env.doc_col > env.doc_indent) then
        begin
            output_string oc_chan ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
    in

    let ind_incr () =
            env.doc_indent <- env.doc_indent + 2;
            output_string oc_chan ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
    in
    let ind_decr () =
            env.doc_indent <- env.doc_indent - 2;
            output_string oc_chan ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
    in
            

    (*
    ** Replace special characters in text strings
    *)

    let tex_text text =
        let str_of_c = String.create 1 in
        let trans_char c =
            match c with
            | 'a'..'z'  (* Speedup ? *)
            | 'A'..'Z' 
            | '0'..'9' -> str_of_c.[0] <- c; str_of_c;
            | _ -> str_of_c.[0] <- c; str_of_c;
        in
            
        let trans_line l =
            let llen    = String.length l in
            let s       = ref "" in
            for i = 0 to llen-1
            do
                s := !s^(trans_char l.[i]); 
            done;
            !s   
        in
        List.iter (fun s -> if (s<>"\n") then 
                                out_str ((trans_line s)^" ")
                            else
                                nl ();
                   ) text
    in

    (*
    ** Parse an attribute list
    *)
    let rec parse_attr_start attr =
        match attr with
        | a::tl ->
          (
            match a with
            | T_Bold    
            | T_Italic  
            | T_BoldItalic 
            | T_Type 
            | T_Symbol 
            | T_AsIs -> (); 
            | T_Superscript -> out_str "^[";
            | T_Subscript -> out_str "_[";
          );
        | [] -> ();
    in
    let rec parse_attr_end attr =
        match attr with
        | a::tl ->
          (
            match a with
            | T_Bold    
            | T_Italic  
            | T_Type 
            | T_AsIs  
            | T_BoldItalic 
            | T_Symbol -> ();
            | T_Superscript -> out_str "]";
            | T_Subscript -> out_str "]";
          );
        | [] -> ();
    in

    (*
    ** Format arguments. The argument must be a list of strings.
    *)
    let format_arg sl tilde =
        let a_str = ref "" in
        let a_name = ref "" in
        let a_type = ref "" in
        let have_sep = ref false in
        List.iter ( fun sa ->
                    a_str := !a_str ^" "^sa;                    
                  ) sl;        
        
        let a_strl = Str.split (Str.regexp ":") !a_str in

 
        if (List.length a_strl = 2 ) then
        begin
            have_sep := true;
            a_name := List.nth a_strl 0;
            a_type := List.nth a_strl 1;
        end;

        if (!have_sep = false) then
            tex_text  [!a_str]
        else
        begin
            if (!a_name.[1] <> '~' &&
                !a_name.[1] <> '?') then
            begin
                (*
                ** It's a symbolic name, not a label. Mark it.
                *)
                out_str "<";
                tex_text  [!a_name];
                out_str ">";
                tex_text  [":";!a_type];
            end
            else
            begin
                if (!a_name.[1] = '~') then
                    tex_text [
                        (
                            if (tilde=false) then 
                                (String.sub (!a_name) 2
                                    ((String.length !a_name)-2))
                            else
                                !a_name;
                        );
                        ":";!a_type
                    ]
                else
                    tex_text  [!a_name;":";!a_type];
            end;
        end;
    in 

    (*
    ** Convert a text structure element to a string
    *)

    let str_of_text tln =
        let str = ref "" in

        List.iter (fun t ->
            match t.s_content with
            | S_Text t -> List.iter (fun s ->
                                        str := !str ^ " " ^ s;
                          ) t;
            | _ -> syntax_error "str_of_text: not a S_Text element";
        ) tln;
        !str 
    in    

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

    (*
    ** Prepare a link. The 'ln' argument must be a list of
    ** generic text elements.
    *)

    let link ln =

        let str = str_of_text ln in

        out_str ("["^ str ^" ] ");
    in

    (*
    ** Text boxes
    *)
    
    let lr_box l r width =
        let llen = String.length l in
        let rlen = String.length r in

        let fillstr = spaces (
                                width -
                                env.doc_col -
                                llen -
                                rlen - 1
                      )
        in
        (l^fillstr^r)
    in
    
    let l_box l width =
        let llen = String.length l in
        let fillstr = spaces (
                                width -
                                env.doc_col -
                                llen - 1
                      )
        in
        (l^fillstr)
    in
    
    let r_box r width =
        let rlen = String.length r in
        let fillstr = spaces (
                                width -
                                env.doc_col -
                                rlen - 1
                      )
        in
        (fillstr^r)
    in
    
    let c_box c width =
        let len = String.length c in
        let lsp = (width/2 - len/2) in
        let rsp = (width/2 - len/2) in
        let fsp = width - rsp - lsp - len in
        (
                (spaces lsp)^
                c^
                (spaces (rsp+fsp))
        )
    in

    (*
    ** Print a table of content starting at a specific section.
    *)
    
    let print_toc sec =
        let depth = ref 1 in
        let toc = "Table of content" in
        out_str (
                (nl'())^
                (fill (env.doc_colwidth-1) '=')^
                (c_box toc env.doc_colwidth)^(nl'())^
                (fill (env.doc_colwidth-1) '=')^
                (nl'())
        ); 
        nl ();
        let rec iter s =
            if (s.sec_name <> "") then
            begin
                out_str (   (spaces (!depth*2))^   
                               (
                                    if (s.sec_type <> "") then
                                        (s.sec_type^": ")
                                    else
                                        ""
                                )^ 
                            s.sec_name
                ); nl ();
            end;

            if (s.sec_childs <> []) then
            begin
                incr depth;
                List.iter iter s.sec_childs;
                decr depth;
            end;
        in
        if (sec.sec_childs <> []) then
        begin
            List.iter iter sec.sec_childs;
        end;
    in

    (*
    ** Lists
    *)
    
    (*
    ** Build the current ordered list number 
    *)

    let rec list_onum ol =
        match ol with
        | hd::tl -> (list_onum tl)^
                        (if List.length tl > 0 then
                            "."
                         else
                            ""
                        )^(string_of_int !hd); 
        | [] -> ""
    in

    (*
    ** Section management
    *)

    (*
    ** Add a new section to the TOC and change to the new section
    *)

    let new_sec env sec =
        env.cur_sec.sec_childs <- env.cur_sec.sec_childs @ [sec];
        env.cur_sec <- sec;
    in
                

    (*
    ** A section was closed. Change to the parent again.
    *)

    let close_sec env =
        let next = env.cur_sec.sec_childs in
        env.cur_sec <- env.cur_sec.sec_parent;
    in


    let head () =  
        ()
    in

    (*
    ** The real worker: translate the structure elements.
    *)

    let rec cont_trans ds =

        cur_sb := ds;  
                

(*
**  => Action before the current structure element 
*) 

        (
          match ds.s_content with
            | S_Body -> 
                begin
                    ind_incr ();
                    head ();
                end;
            | S_Empty -> ();
            | S_Text t -> tex_text  t;

            | S_NL -> nl ();
            | S_TAB -> output_string (oc_chan) "  ";

            | S_Comment -> 
                begin
                    match env.in_special with
                    | Spec_Fun_Int fi -> 
                                    fi.fun_comm <- fi.fun_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Class_Int ci -> 
                                    ci.class_comm <- ci.class_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Val_Int vi -> 
                                    vi.val_comm <- vi.val_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Method_Int mi -> 
                                    mi.meth_comm <- mi.meth_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Object_Int oi -> 
                                    oi.obj_comm <- oi.obj_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Type_Int ti -> 
                                    ti.type_comm <- ti.type_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Table tb ->
                                    tb.table_comm <- tb.table_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Hdr_Int hr -> 
                                    hr.hdr_comm <- hr.hdr_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_None -> out_str "   (* ";
                end;

            
            | S_Paragraph -> 
                begin
                    
                    out_str (nl'()); 
                    ind_decr ();
                    

                    (*
                    ** The first child must be the paragraph name,
                    ** if any.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        List.iter cont_trans dna.s_childs;
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> (); 
                            end; 
                        | [] -> ();     (* ??? *)
                    
                    );
                    out_str (nl'()); ind_incr ();  
                end;

            | S_Attribute -> parse_attr_start ds.s_attr;

            | S_Example -> 
                begin
                    out_str (nl'());
                    ind_decr ();

                    (
                        (*
                        ** Is there a name description ? Must be the first
                        ** child.
                        *)
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        out_str "Example: ";
                                        cont_trans dna;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> out_str "Example"; 
                            end; 
                            | [] -> out_str "Example";
                    );
                    ind_incr (); ind_incr (); 
                    env.in_example <- true;
                end;

            | S_Preform -> 
                begin
                    out_str (nl'()); ind_incr ();
                    env.in_preform <- true;
                end;

            | S_Link -> 
                begin
                    link ds.s_childs;
                    env.ignore_childs <- true; 
                end;

            | S_OList -> 
                begin
                    if (env.in_ulist + (List.length env.in_olist) = 0) then
                    begin
                        out_str (nl'());
                    end;
            
                    let ol_cnt = ref 0 in
                    env.in_olist <- [ol_cnt] @ env.in_olist; 
                    out_str (nl'()); ind_incr (); 
                    env.in_list <- [List_Numbered] @
                                    env.in_list;
                end;
                                
            | S_UList -> 
                begin
                    if (env.in_ulist + (List.length env.in_olist) = 0) then
                    begin
                        out_str (nl'()); 
                    end;

                    env.in_ulist <- env.in_ulist + 1;
                    out_str (nl'()); ind_incr (); 
                    env.in_list <- [List_Unnumbered] @ 
                                    env.in_list;
                end;

            | S_OpList -> 
                begin
                    if (env.in_list = []) then
                    begin
                        nl ();
                        out_str (nl'()^(spaces env.doc_indent));
                    end;

                    (*
                    ** The first child can be an optional name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        let pname = (str_of_text dna.s_childs)
                                        in

                                        out_str pname;
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> out_str "Options"; 
                            end; 
                        | [] -> out_str "Options";     
                    
                    );

                    env.doc_indent <- env.doc_indent + 2;
                    out_str (nl'()^(spaces env.doc_indent)); 
                    env.in_list <- [List_Option] @ 
                                    env.in_list;
                end;

            | S_List_Item -> 
                begin
                    
                    nl (); 
                    match (List.hd env.in_list) with
                        | List_Unnumbered ->
                        begin
                            out_str ( 
                                (ulist_mark env.in_ulist)
                            ); 
                            env.doc_indent <- env.doc_indent + 4;
                            out_str (spaces 3);
                        end
                        | List_Numbered ->
                        begin
                            incr (List.hd env.in_olist);
                            let str = "("^(list_onum env.in_olist)^")"
                            in
                            out_str ( str );
                            env.doc_indent <- env.doc_indent + 4;
                            out_str (spaces 
                                    (4 - (String.length str)));
                        end
                        | List_Option -> 
                        begin

                            (*
                            ** The first child must be the option name.
                            *)

                            let str =
                                match ds.s_childs with
                                | dna::tl ->
                                begin
                                    match dna.s_content with
                                    | S_Name -> 
                                    begin
                                        env.ignore_head <- true;    
                                        (str_of_text dna.s_childs)
                                    end;
                                    | _ -> syntax_error "option list item without name";
                                end; 
                                | [] -> syntax_error "option list item without name";
                    
                            in
                            out_str ( str );
                            env.doc_indent <- env.doc_indent + 2;
                            out_str (nl'()^nl'()^(spaces env.doc_indent));
                        end;      
                end;

            | S_Table -> 
                begin
                        let rulers = match (List.hd ds.s_childs).s_content with
                                    | S_TableNoRulers ->
                                        begin
                                            env.ignore_head <- true;
                                            true
                                        end;
                                    | _ -> false
                        in
                
                        let cr = ref [] in
                        env.in_special <- Spec_Table {
                                    table_name = [];
                                    table_head = [];
                                    table_rowlist = [];
                                    table_currow = cr;
                                    table_cols = 0;
                                    table_norul = rulers;
                                    table_comm = [];
                        };
                        env.special_depth <- env.special_depth + 1;
                end;

            | S_TableNoRulers -> ();

            | S_TableHead -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_Table tb ->
                                    tb.table_head <- tb.table_head @ [ds];
                    | _ -> syntax_error "Invalid S_TableHead";            
                end;

            | S_TableRow -> 
                begin
                    env.table_col <- 0;
                    match env.in_special with
                    | Spec_Table tb ->
                                    let cr = ref [] in
                                    tb.table_currow  <- cr;
                    | _ -> syntax_error "Invalid S_TableRow";            
                end;

            | S_TableCol -> 
                begin
                    (* prevent down stream *)
                    env.special_depth <- env.special_depth + 1;
                    env.table_col <- env.table_col + 1;
                    match env.in_special with
                    | Spec_Table tb ->
                                    let row = tb.table_currow in
                                    row := !row @ [ds];
                    | _ -> syntax_error "Invalid S_TableCol";            
                end;


            | S_Title -> 
                begin
                    
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '*')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_TOC -> ();

            | S_Program -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();
                    
                    (*
                    ** The first child must be the program name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="Program";
                                        } in

                                        
                                        new_sec env sec;
                                        
                                        out_str (c_box
                                                   ("Program: "^pname)
                                                   (env.doc_colwidth-1)
                                        );

                                        env.ignore_head <- true;
                                        env.cur_program <- Some sec;
                                    end;
                                | _ -> syntax_error "Program without name"; 
                            end; 
                        | [] -> syntax_error "Program without name";
                    
                    );
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_incr ();
                end;

            | S_Package -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the package name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="Package";
                                        } in

                                        new_sec env sec;
                                        
                                        out_str (c_box
                                                    ("Package: "^pname)
                                                    (env.doc_colwidth-1)
                                        ); 

                                         
                                        env.ignore_head <- true;
                                        env.cur_package <- Some sec;
                                    end;
                                | _ -> syntax_error "Package without name"; 
                            end; 
                        | [] -> syntax_error "Package without name";
                    
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_Intro -> ();

            | S_S1 -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the section s1 name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;
                                        
                                        out_str (c_box
                                                    pname
                                                    (env.doc_colwidth-1)
                                        ); 

                                         
                                        env.ignore_head <- true;
                                        env.cur_s1 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end; 
                        | [] -> syntax_error "Section without name";
                    
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_S2 -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    (*
                    ** The first child must be the subsection name.
                    *)

                    let lbox =
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;
                                
                                        env.ignore_head <- true;
                                        env.cur_s2 <- Some sec;
                                        pname
                                    end;
                                | _ -> syntax_error "Subsection without name"; 
                            end; 
                        | [] -> syntax_error "Subsection without name";
                    
                    in           

                    let rbox =
                        if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                         sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                              | Some sec -> 
                                        (
                                                "Package: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with
                              | Some sec -> 
                                        (
                                                "Program: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
    
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_S3 -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    (*
                    ** The first child must be the unit name.
                    *)

                    let lbox = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)
                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;

                                        env.cur_s3 <- Some sec;
                                        env.ignore_head <- true;
                                        pname
                                    end;
                                | _ -> syntax_error "Unit without name"; 
                            end; 
                        | [] -> syntax_error "Unit without name";
                    
                    in

                    let rbox = 
                        if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_module <> []) then
                        begin
                            (
                                "ML-Module: "^
                                (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_clib <> None) then
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                    (
                                        "C-Library: "^
                                        sec.sec_name
                                    );
                              | None -> "";
                        end
                        else 
                            ""

                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_S4 -> 
                begin
                    out_str (nl'());
                    ind_decr ();
                    

                    (*
                    ** The first child must be the subunit name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let fname = (file_name pname)^".tex" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;
                                        List.iter cont_trans dna.s_childs;
                                        env.ignore_head <- true;    
                                        env.cur_s4 <- Some sec;
                                    end;
                                | _ -> syntax_error "subunit without name"; 
                            end; 
                        | [] -> syntax_error "subunit without name";     
                    
                    );
                    out_str (nl'()); ind_incr ();  
                end;

            | S_Desc -> 
                begin
                    out_str (nl'());
                    ind_decr ();
                    out_str "Description";                    
                    out_str (nl'()); ind_incr ();  
                end;


            | S_Clib -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    (*
                    ** The first child must be the clib name.
                    *)

                    let lbox =
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="C-Library";
                                        } in

                                        new_sec env sec;

                                        env.ignore_head <- true;
                                        env.cur_clib <- Some sec;

                                        ("C-Library: "^pname)
                                    end;
                                | _ -> syntax_error "C-Lib without name"; 
                            end; 
                        | [] -> syntax_error "C-Lib without name";
                    
                    in
           

                    let rbox =
                        if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                            | Some sec -> 
                                            (
                                                "Package: "^
                                                sec.sec_name
                                            );
                            | None -> "";
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with
                            | Some sec -> 
                                            (
                                                "Program: "^
                                                sec.sec_name
                                            );
                            | None -> "";
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                            | Some sec -> 
                                            sec.sec_name;
                            | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;


            | S_Module -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    (*
                    ** The first child must be the module name.
                    *)

                    let lbox =
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Module";
                                        } in

                                        new_sec env sec;

                                        env.ignore_head <- true;

                                        env.cur_module <- 
                                            [sec] @ env.cur_module;

                                        ("Module: "^pname)
                                    end;
                                | _ -> syntax_error "Module without name"; 
                            end; 
                        | [] -> syntax_error "Module without name";
                    
                    in
           

                    let rbox =
                        if (List.length env.cur_module > 1) then
                        begin
                            (*
                            ** This is a submodule !
                            *)
                            "ML-Module: "^
                            (List.nth env.cur_module 1).sec_name
                        end
                        else if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                              | Some sec -> 
                                                "Package: "^
                                                sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                               sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with  (* ??? *)
                              | Some sec -> 
                                                "Program: "^
                                                sec.sec_name;
                              | None -> "";
                        end
                        else
                            ""
                    in                    
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;


            | S_Function -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the function name.
                    *)

                    let lbox = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Function";
                                        } in

                                        new_sec env sec;

                                        env.cur_function <- Some sec;
                                        env.ignore_head <- true;

                                        ("ML-Function: "^pname)
                                    end;
                                | _ -> syntax_error "Function without name"; 
                            end; 
                        | [] -> syntax_error "Function without name";
                    
                    in

                    let rbox =
                        if (env.cur_module <> []) then
                        begin
                            (
                                "ML-Module: "^
                                (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                                sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       (
                                                "C-Library: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;


            | S_Type -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the type name.
                    *)

                    let lbox =
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Type";
                                        } in

                                        new_sec env sec;

                                        cont_trans dna;    

                                        env.cur_type <- Some sec;
                                        env.ignore_head <- true;
                                        ("ML-Type: "^pname)
                                    end;
                                | _ -> syntax_error "Type without name"; 
                            end; 
                        | [] -> syntax_error "Type without name";
                    
                    in

                    let rbox = 
                        if (env.cur_module <> []) then
                        begin
                            (
                                        "ML-Module: "^
                                        (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                                sec.sec_name;

                              | None -> "";
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       (
                                                "C-Library: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                    
                end;

            | S_Value -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the value name.
                    *)

                    let lbox = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Value";
                                        } in

                                        new_sec env sec;

                                        cont_trans dna;    

                                        env.cur_val <- Some sec;
                                        env.ignore_head <- true;

                                        ("ML-Value: "^pname)
                                    end;
                                | _ -> syntax_error "Value without name"; 
                            end; 
                        | [] -> syntax_error "Value without name";
                    
                    in

                    let rbox =
                        if (env.cur_module <> []) then
                        begin
                            (
                                        "ML-Module: "^
                                        (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                                sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                        (
                                                "C-Library: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                    
                end;

            | S_Class -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the class name.
                    *)

                    let lbox = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Class";
                                        } in

                                        new_sec env sec;

                                        cont_trans dna;    

                                        env.cur_class <- Some sec;
                                        env.ignore_head <- true;

                                        ("ML-Class: "^pname)
                                    end;
                                | _ -> syntax_error "Class without name"; 
                            end; 
                        | [] -> syntax_error "Class without name";
                    
                    in

                    let rbox =
                        if (env.cur_module <> []) then
                        begin
                            (
                                        "ML-Module: "^
                                        (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                                sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                        (
                                                "C-Library: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                    
                end;

            | S_C -> 
                begin
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    (*
                    ** The first child must be the C name.
                    *)

                    let lbox = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="C";
                                        } in

                                        new_sec env sec;

                                        cont_trans dna;    

                                        env.cur_cint <- Some sec;
                                        env.ignore_head <- true;

                                        ("C: "^pname)
                                    end;
                                | _ -> syntax_error "C without name"; 
                            end; 
                        | [] -> syntax_error "C without name";
                    
                    in

                    let rbox =
                        if (env.cur_module <> []) then (* ??? *)
                        begin
                            (
                                        "ML-Module: "^
                                        (List.hd env.cur_module).sec_name
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                                sec.sec_name;
                              | None -> "";
                        end
                        else if (env.cur_clib <> None) then 
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                        (
                                                "C-Library: "^
                                                sec.sec_name
                                        );
                              | None -> "";
                        end
                        else
                            ""
                    in
                    out_str (
                        (lr_box lbox rbox env.doc_colwidth)
                    );
                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                    
                end;
            


            | S_Fun_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_Fun_Int {
                                    fun_name = [];
                                    fun_retargs = [];
                                    fun_args = [];
                                    fun_curried = false;                         
                                    fun_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_Object_Int oi -> oi.obj_args <- 
                                                    oi.obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;

            | S_Mutable -> 
                begin
                    out_str ( 
                        "mutable "
                    );
                end;
            | S_Virtual -> 
                begin
                    out_str ( 
                        "virtual "
                    );
                end;
            | S_Private -> 
                begin
                    out_str ( 
                        "private "
                    );
                end;

            | S_Val_Interface ->
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_Val_Int {
                                    val_name = [];
                                    val_args = [];
                                    val_curried = false;                         
                                    val_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_Object_Int oi -> oi.obj_args <- 
                                                    oi.obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;

            | S_Module_Interface ->
                begin

                    nl ();

                    out_str "module ";

                    (*
                    ** The next child must be the module name
                    *)
                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        List.iter cont_trans
                                            dna.s_childs;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Module without name"; 
                            end; 
                            | [] -> syntax_error "Module without name";
                    );

                    env.doc_indent <- env.doc_indent + 2;
                    nl ();
                    
                    out_str "sig";

                    env.doc_indent <- env.doc_indent + 2;
                    nl ();
                
                end;

            | S_Method_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_Method_Int {
                                    meth_name = [];
                                    meth_args = [];
                                    meth_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_Object_Int oi -> oi.obj_args <- 
                                                    oi.obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;
                
            | S_Object_Interface -> 
                begin
                    env.special_depth <- env.special_depth - 1;
                    
                                (
                                    match env.in_special with
                                    | Spec_Class_Int ci -> ();
                                    | _ -> 
                                      syntax_error "ObjectInt outside class";
                                );

                                let oi = Spec_Object_Int {
                                    obj_name = [];
                                    obj_args = [];
                                    obj_comm = [];
                                } in
                                env.in_object <- oi;
                                env.in_special <- oi;                                
                                
                end;


            | S_Type_Interface -> 
                                env.in_special <- Spec_Type_Int {
                                    type_name = [];
                                    type_args = [];
                                    type_type = Type_List;
                                    type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

            | S_Exc_Interface -> 
                begin
                                env.in_special <- Spec_Type_Int {
                                    type_name = [ds];
                                    type_args = [];
                                    type_type = Type_Exception;
                                    type_comm = [];
                                };
                                env.ignore_childs <- true;
                end;


            | S_Class_Interface -> 
                begin
                        env.in_special <- Spec_Class_Int {
                                    class_name = [];
                                    class_args = [];
                                    class_obj = Spec_None;                         
                                    class_comm = [];
                                };
                        env.special_depth <- env.special_depth + 1;
                        env.in_class <- env.in_special;
                end;

            | S_Struc_Interface -> 
                                env.in_special <- Spec_Type_Int {
                                    type_name = [];
                                    type_args = [];
                                    type_type = Type_Structure;
                                    type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

            | S_C_Var_Interface -> 
                begin
                                env.in_special <- Spec_Val_Int {
                                    val_name = [];
                                    val_args = [];
                                    val_curried = false;                         
                                    val_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                end;

            | S_C_Fun_Interface -> 
                begin
                                env.in_special <- Spec_Fun_Int {
                                    fun_name = [];
                                    fun_retargs = [];
                                    fun_args = [];
                                    fun_curried = false;                         
                                    fun_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                end;                

            | S_C_Hdr_Interface -> 
                begin
                                env.in_special <- Spec_Hdr_Int {
                                    hdr_name = [ds];
                                    hdr_type = C_Header;                         
                                    hdr_comm = [];
                                };
                                env.ignore_childs <- true;
                end;


            | S_Interface -> 
                begin

                    out_str (nl'());
                    ind_decr ();
                    
                    env.in_interface <- true;
                    (
                        (*
                        ** Is there a name description ? Must be the first
                        ** child.
                        *)
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        out_str "Programming Interface: ";
                                        cont_trans dna;

                                        env.ignore_head <- true;
                                    end;
                                | _ -> out_str "Programming Interface"; 
                            end; 
                            | [] -> out_str "Programming Interface";
                    );
                    out_str (nl'()); ind_incr (); ind_incr (); 
                end;


            (*
            ** Specials
            *)

            | S_Name -> 
                begin
                    match env.in_special with
                    | Spec_Fun_Int fi -> 
                                    fi.fun_name <- fi.fun_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Class_Int ci -> 
                                    ci.class_name <- ci.class_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Val_Int vi -> 
                                    vi.val_name <- vi.val_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Method_Int mi -> 
                                    mi.meth_name <- mi.meth_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Object_Int oi -> 
                                    oi.obj_name <- oi.obj_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Type_Int ti -> 
                                    ti.type_name <- ti.type_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Table tb ->
                                    tb.table_name <- tb.table_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Hdr_Int hdr -> ();
                    | Spec_None -> 
                        begin
                            if (env.doc_terminal = true) then
                                Terminfo.standout true;
                        end;            
                end;

            | S_CurArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_Fun_Int fi -> fi.fun_args <- fi.fun_args @ [ds];
                                         fi.fun_curried <- true;
                    | Spec_Val_Int vi -> vi.val_args <- vi.val_args @ [ds];
                                         vi.val_curried <- true;
                    | Spec_Type_Int ti -> ti.type_args <- ti.type_args @ [ds];

                    | Spec_Method_Int mi -> mi.meth_args <- mi.meth_args @ [ds];

                    | Spec_Class_Int ci -> ci.class_args <- ci.class_args @ [ds];

                    | _ -> syntax_error "Invalid S_CurArg";            
                end;

            | S_UnCurArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_Fun_Int fi -> fi.fun_args <- fi.fun_args @ [ds];
                                         fi.fun_curried <- false;
                    | Spec_Val_Int vi -> vi.val_args <- vi.val_args @ [ds];
                                         vi.val_curried <- false;
                    | _ -> syntax_error "Invalid S_UnCurArg";            
                end;

            | S_RetArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_Fun_Int fi -> fi.fun_retargs <- fi.fun_retargs 
                                                           @ [ds];
                    | _ -> syntax_error "Invalid S_RetArg";            
                end;

        );

        if (ds.s_childs <> [] &&
            env.special_depth < 2 
           ) then
        begin
            if (env.ignore_childs = false) then
            begin
                if (env.ignore_head = false) then
                    List.iter cont_trans ds.s_childs
                else
                begin
                    env.ignore_head <- false;
                    List.iter cont_trans (List.tl ds.s_childs);
                end;            
            end
            else
                env.ignore_childs <- false;
        end;

            

(*
** Action after the current structure element 
*) 



        (
          match ds.s_content with

            | S_Empty -> ();

            | S_Text t -> ();

            | S_NL -> ();  

            | S_TAB -> ();

            | S_Comment -> 
                begin
                    let d () = env.special_depth <- env.special_depth - 1 in
                    match env.in_special with
                    | Spec_Fun_Int  fi  -> d ();
                    | Spec_Hdr_Int hr -> d ();
                    | Spec_Val_Int  vi  -> d (); 
                    | Spec_Type_Int ti  -> d ();
                    | Spec_Class_Int ti  -> d ();
                    | Spec_Object_Int ti  -> d ();
                    | Spec_Method_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_None -> 
                        begin
                            out_str " *)";
                        end;            
                end;


            
            | S_Paragraph -> 
                begin
                    out_str (nl'());
                    nl (); 
                end;

            | S_Attribute -> parse_attr_end ds.s_attr;

            | S_Example -> 
                begin
                    env.in_example <- false;
                    ind_decr (); nl ();
                end;

            | S_Preform -> 
                begin
                    env.in_preform <- false;
                    out_str (nl'());
                    ind_decr ();
                end;

            | S_OList ->    
                begin
                    env.in_olist <- match env.in_olist with
                                    | hd::tl -> tl;
                                    | [] -> syntax_error "Unbalanced olist";
                    ;
                    env.in_list <- List.tl env.in_list;

                    if (env.in_ulist + (List.length env.in_olist) = 0) then
                    begin
                        out_str (nl'()); 
                    end;
                    ind_decr ();
                end;

            | S_UList ->    
                begin
                    env.in_ulist <- env.in_ulist - 1;
                    env.in_list <- List.tl env.in_list;

                    if (env.in_ulist + (List.length env.in_olist) = 0) then
                    begin
                        out_str (nl'()); 
                    end;
                    ind_decr (); 
                end;

            | S_OpList ->    
                begin
                    env.in_list <- List.tl env.in_list;
                    env.doc_indent <- env.doc_indent - 2;
            
                    if (env.in_list = []) then
                    begin
                        nl ();
                    end;
                end;

            | S_List_Item -> 
                begin
                  (
                    match List.hd env.in_list with
                    | List_Numbered 
                    | List_Unnumbered ->
                        env.doc_indent <- env.doc_indent - 4;
                    | List_Option ->
                        env.doc_indent <- env.doc_indent - 2;
                        out_str (nl'()^(spaces env.doc_indent));
                  );  
                  nl ();
                end;

            | S_Name -> 
                begin
                    let d () = env.special_depth <- env.special_depth - 1 in
                    match env.in_special with
                    | Spec_Fun_Int  fi  -> d ();
                    | Spec_Val_Int  vi  -> d (); 
                    | Spec_Type_Int ti  -> d ();
                    | Spec_Class_Int ti  -> d ();
                    | Spec_Object_Int ti  -> d ();
                    | Spec_Method_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_Hdr_Int hdr -> ();
                    | Spec_None -> 
                        begin
                            if (env.doc_terminal = true) then
                                Terminfo.standout false;
                        end;            
                end;

            | S_Link -> ();

            | S_Table -> 
              begin
                let tb = 
                        match env.in_special with
                        | Spec_Table tb -> tb;
                        | _ -> syntax_error "Inavlid in_special";
                in 

                let th = tb.table_head in
                let tr = tb.table_rowlist in
                let trn = List.length tr in
                let tcn = tb.table_cols in
                let tbnr = tb.table_norul in
                
                env.special_depth <- env.special_depth - 1;
                env.in_special <- Spec_None;

                (*
                ** Compress a string paragraph (column data)
                ** to an array of lines with fixed width
                *)
                let colbox ds width center =
                    let str = str_of_struc ds in
                    let strl = String.length str in
                    if (strl <= width) then
                    begin
                        if (center=false) then
                            [|str^(fill (width-strl) ' ')|]
                        else
                            [|c_box str width|]
                    end
                    else
                    begin
                        let sl = Str.split (Str.regexp " ") str in
                        let cl = ref "" in                        
                        let bl = ref [] in
                        let ci = ref 0 in
                                                
                        
                        let rec iter s =
                            match s with
                            | hd::tl -> 
                                begin
                                    let hdl = String.length hd in
                                    if (!ci+hdl < width) then
                                    begin
                                        cl := !cl ^ " " ^hd;
                                        ci := !ci + hdl + 1;
                                    end
                                    else
                                    begin
                                        cl := !cl ^ (fill (width - !ci) ' ');
                                        bl := !bl @ [!cl]; 
                                        cl := " ";
                                        ci := 1;          
                                        if (!ci+hdl < width) then
                                        begin
                                            cl := !cl ^ hd;
                                            ci := !ci + hdl;
                                        end
                                        else
                                            (); (* ??? *)                         
                                    end;
                                    iter tl;
                                end;  
                            | [] -> ();
                        in
                        iter sl;
                        bl := !bl @ [!cl^ (fill (width - !ci) ' ')]; 
                        Array.of_list !bl;
                    end; 
                in

                (*
                ** Calculate column widths. 
                *)

                let tablewidth,colwidth =
                    let cols = env.table_col in
                    let ar = Array.create (cols+1) 0 in
                    let maxwidth = ((env.doc_colwidth - 10) / cols)
                                   in 
                    let minwidth = 5 in

                    let tablewidth = ref 0 in
                        
                    List.iter ( fun r ->
                        let icol = ref 1 in
                        List.iter ( fun tc ->
                            let str = str_of_struc tc in
                            let strl = String.length str
                            in

                            if (strl<maxwidth &&
                                strl>minwidth) then
                            begin
                                if (strl > ar.(!icol)) then
                                    ar.(!icol) <- strl;
                            end
                            else if (strl < minwidth) then
                            begin
                                if (minwidth > ar.(!icol)) then
                                    ar.(!icol) <- minwidth;
                            end
                            else
                                ar.(!icol) <- maxwidth;
                            incr icol;
                        ) r;
                    ) tr;
                    for i = 1 to cols 
                    do
                        tablewidth := !tablewidth + ar.(i);
                    done;

                    (*
                    ** Look for the table head and do the final adjustment.
                    *)
                    if (th <> []) then
                    begin
                        let str = str_of_struc (List.hd th) in
                        let strl = String.length str in

                        if(strl > !tablewidth &&
                           strl < maxwidth*cols) then
                        begin
                            tablewidth := strl+1;
                            for i = 1 to cols
                            do
                                ar.(i) <- !tablewidth / cols;
                            done; 
                        end
                        else if(strl > !tablewidth &&
                                strl > maxwidth*cols) then
                        begin
                            tablewidth := env.textwidth-20;
                            for i = 1 to cols
                            do
                                ar.(i) <- !tablewidth / cols;
                            done; 
                        end
                        
                    end;

                    !tablewidth,ar
                in

                out_str (nl'());ind_incr ();
                if (tbnr=false) then
                    out_str (fill (tablewidth) '-'); 
                nl ();
                
                (*
                ** Build up the table
                *)

                if (th <> []) then
                begin
                    (*
                    ** The table header
                    *)
                    Array.iter ( fun l -> out_str l;nl ())
                               (colbox (List.hd th) (tablewidth) true);
                    nl ();
                end;
                if (tbnr=false) then
                    out_str (fill (tablewidth) '-'); 
                nl ();

                List.iter (fun r ->
                        let ci = ref 1 in
                        let rl = List.length r in

                        let cl = ref [] in
                        let jmax = ref 0 in
                        
                        List.iter (fun c ->
                            let cle = (colbox c colwidth.(!ci) false) in
                            let clel = Array.length cle in
                            cl := !cl @ [cle];
                            if (clel > !jmax) then
                                jmax := clel;
                            incr ci;
                        ) r;          
                        let ca = Array.of_list !cl in
                        for j = 0 to (!jmax)-1
                        do
                            for i = 0 to env.table_col - 1
                            do
                                let cle = ca.(i) in
                                let cll = Array.length cle in
                                if (cll>j) then
                                    out_str (cle.(j))
                                else
                                    out_str (fill (colwidth.(i+1)) ' ');
                                
                            done;
                            nl ();
                        done;
                        nl ();
                    ) tr;

                if (tbnr=false) then
                begin
                    out_str (fill (tablewidth) '-'); 
                    nl ();
                end;
                
                ind_decr (); nl (); 
              end;

            | S_TableNoRulers -> ();

            | S_TableHead -> 
                env.special_depth <- env.special_depth - 1;

            | S_TableRow -> 
                begin
                    
                    match env.in_special with
                    | Spec_Table tb ->
                        begin
                                    let cr = tb.table_currow in
                                    tb.table_rowlist <- tb.table_rowlist
                                                        @ [!cr];
                                    if (env.table_col > tb.table_cols) then
                                        tb.table_cols <- env.table_col;
                        end;
                    | _ -> syntax_error "Invalid S_TableRow";            
                end;

            | S_TableCol -> 
                    env.special_depth <- env.special_depth - 1;
                      

            | S_Body -> 
                begin
                    if (env.toc.sec_childs <> [] &&
                        env.doc_notoc=false ) then
                    begin

                        (*
                        ** Print the table of content for the whole
                        ** document.
                        *)

                        print_toc env.toc;

                    end;
                    out_str (nl'()); ind_decr ();

                end;


            | S_Title -> 
                begin
                    ind_decr ();
                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '*')^
                        (nl'())
                    );
                end;

            | S_TOC -> ();

            | S_Program -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_program <- None;
                end;


            | S_S1 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s1 <- None;
                end;

            | S_Package -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_package <- None;
                end;

            | S_Module -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_module <- 
                      (
                        match env.cur_module with
                        | hd::tl -> tl;
                        | [] -> [];
                      );
                end;

            | S_S2 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s2 <- None ;
                end;


            | S_S3 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s3 <- None ;
                end;


            | S_S4 -> 
                begin
                    out_str (nl'());
                    nl (); 
                    close_sec env;
                    env.cur_s4 <- None;
                end;

            | S_Desc -> 
                begin
                    out_str (nl'());
                    nl (); 
                end;

            | S_Intro -> ();

            | S_Clib -> ();

            | S_Function -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_function <- None;
                end;

            | S_Type -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_type <- None;
                end;

            | S_Value -> 
                begin
                    out_str (nl'()); nl ();
                        close_sec env;
                        env.cur_val <- None;
                end;

            | S_Class -> 
                begin
                    out_str (nl'()); nl ();
                        close_sec env;
                        env.cur_class <- None;
                end;
            

            | S_C -> 
                begin
                    out_str (nl'()); nl ();
                        close_sec env;
                        env.cur_cint <- None;
                end;
            

            | S_Fun_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    

                    let fi = 
                        match env.in_special with
                        | Spec_Fun_Int fi -> fi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 

                    let rargs = fi.fun_retargs in
                    let fname = fi.fun_name in
                    let args = fi.fun_args in
                    let curried = fi.fun_curried in
                    let comm = fi.fun_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (fname = []) then
                        syntax_error "Function interface without name";
                    

                    if (comm <> []) then
                    begin
                        nl (); 
                        cont_trans (List.hd comm);
                        nl ();
                    end;

                    if (rargs <> []) then
                    begin
                                        
                        let n = List.length rargs in
                        let i = ref 1 in
                        

                        List.iter (fun r ->
    
                                    nl ();
                                    ind := env.doc_col ;
                                    
                                    if (!i = 1) then
                                    begin
                                        out_str "[ ";
                                    end
                                    else
                                    begin
                                        out_str "  ";
                                    end;

                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)

                                    let com = ref None in
                                    
                                    List.iter 
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t true;
                                          | S_Comment -> com := Some na;
                                          | _ -> cont_trans na;
                        
                                    ) r.s_childs;

                                    if (!i < n) then
                                    begin
                                        out_str " * ";
                                    end
                                    else
                                    begin
                                        out_str " ] = ";  
                                        ind := env.doc_col - !ind + 2;
                                        List.iter cont_trans 
                                                (List.hd fname).s_childs;
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            cont_trans com;
                                          end    
                                        | None -> ();  
                                    );

                                    nl ();

                                    incr i;
                                  ) rargs;

                    end;
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str (spaces !ind);
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    
                                    List.iter
                                    (fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t true;
                                          | S_Comment -> com := Some na;
                                          | _ -> cont_trans na;
                        
                                    ) a.s_childs;



                                    if (!i < n) then
                                    begin
                                        match curried with
                                        | true -> out_str " -> ";
                                        | false -> out_str " * ";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            cont_trans com;
                                          end    
                                        | None -> ();  
                                    );

                                    nl ();
                                
                                    incr i;
                                  ) args;
                    end; 
                    out_str (nl'()^(spaces env.doc_indent));
                end;

            | S_Mutable -> ();
            | S_Private -> ();
            | S_Virtual -> ();
                                    
            | S_Val_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    
                    
                    let vi = 
                        match env.in_special with
                        | Spec_Val_Int vi -> vi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let vname = vi.val_name in
                    let args = vi.val_args in
                    let curried = vi.val_curried in
                    let comm = vi.val_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (vname = []) then
                        syntax_error "Value interface without name";

                    nl ();
                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;
                    
                    ind := env.doc_col ;
                    
                    out_str "val ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "mutable "
                                    );
                                end;
                               | _ -> 
                                begin
                                    cont_trans ds;
                                end;
                              ) (List.hd vname).s_childs;


                    out_str ":  "; 
                     
                    ind := env.doc_col - !ind;
                    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->

                                    if (!i > 1) then
                                        out_str (spaces !ind);

                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)

                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    
                                    List.iter
                                    (fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t true;
                                          | S_Comment -> com := Some na;
                                          | _ -> cont_trans na;
                        
                                    ) a.s_childs;


                                    if (!i < n) then
                                    begin
                                        match curried with
                                        | true -> out_str " -> ";
                                        | false -> out_str " * ";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            cont_trans com;
                                          end    
                                        | None -> ();  
                                    );

                                    nl ();
                                    incr i;
                                  ) args;
                    end; 
                    out_str (nl'()^(spaces env.doc_indent));
                end;

            | S_Module_Interface ->
                begin
                    env.doc_indent <- env.doc_indent - 2;
                    nl ();
                    out_str "end";
                    env.doc_indent <- env.doc_indent - 2;
                    nl ();
                    
                end;

            | S_Class_Interface ->
                begin
                    let ind = ref env.doc_col in                    
                    env.special_depth <- env.special_depth - 1;

                    let ci =
                        match env.in_special with
                        | Spec_Class_Int ci -> ci;
                        | _ -> syntax_error "Invalid class interface";
                    in
                    let cname = ci.class_name in
                    let cargs = ci.class_args in
                    let comm = ci.class_comm in
                    
                    let co    = 
                        match ci.class_obj with
                        | Spec_Object_Int co -> co;
                        | _ -> syntax_error "Invalid class/object interface";
                    in
                    let oname = co.obj_name in
                    let oargs = co.obj_args in


                    if (cname = []) then
                        syntax_error "Class without name";

                    nl ();
                    
                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;

                    ind := env.doc_col;

                    out_str "class ";
                    ind := env.doc_col - 2;    

                    List.iter cont_trans 
                               (List.hd cname).s_childs;

                    out_str ": " ; nl ();
                    
                    if (cargs <> []) then
                    begin
                        let n = List.length cargs in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str (spaces !ind);
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    (
                                      match a.s_childs with
                                      | na::tl -> 
                                        (
                                          match na.s_content with
                                          | S_Text t -> format_arg t false;
                                          | _ -> List.iter
                                                   cont_trans
                                                   a.s_childs;
                                        );
                                      | [] -> ();   (* ???? *)
                                    );
                        
                                    if (!i < n) then
                                    begin
                                        out_str " -> ";
                                    end;
                                
                                    nl ();
                                    incr i;
                                  ) cargs;
                    end; 

                    if (oargs <> []) then
                    begin
                        out_str (spaces !ind);
                        ind := !ind + 2;
                        
                        out_str (
                            "object "
                        ); 
                        
                        let oldind = env.doc_indent in
                        env.doc_indent <- env.doc_indent + !ind;
                          
                        List.iter (fun i ->
                                    out_str ((nl'())^(spaces env.doc_indent));                   
                                    cont_trans i;
                                  ) oargs;
                        
                        out_str ((nl'())^spaces (env.doc_indent-2));

                        env.doc_indent <- oldind;

                        out_str "end"; nl ();
                        
                    end
                    else
                        syntax_error "class without object"; 
                    
                    out_str (nl'()^(spaces env.doc_indent));
                    
                    env.in_class <- Spec_None;
                    
                end;
           

            | S_Object_Interface ->
                begin
                    env.special_depth <- env.special_depth + 1;

                    match env.in_class with
                    | Spec_Class_Int ci -> 
                            env.in_special <- env.in_class;
                            ci.class_obj <- env.in_object;
                            env.in_object <- Spec_None;
                            
                    | _ -> syntax_error "Invalid class/object interface";
                end;

            | S_Method_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    
                    
                    let mi = 
                        match env.in_special with
                        | Spec_Method_Int mi -> mi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = mi.meth_name in
                    let args = mi.meth_args in
                    let comm = mi.meth_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (vname = []) then
                        syntax_error "Method interface without name";
                    
                    nl ();

                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;

                    ind := env.doc_col ;
                    
                    out_str "method ";


                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Private -> 
                                begin
                                    out_str (
                                        "private "
                                    );
                                end;
                               | S_Virtual -> 
                                begin
                                    out_str (
                                        "virtual "
                                    );
                                end;
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "mutable "
                                    );
                                end;
                               | _ -> 
                                begin
                                    cont_trans ds;
                                end;
                              ) (List.hd vname).s_childs;


                    out_str ":  "; 
                     
                    ind := env.doc_col - !ind;
                    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->

                                    if (!i > 1) then
                                        out_str (spaces !ind);
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)

                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    (
                                      match a.s_childs with
                                      | na::tl -> 
                                        (
                                          match na.s_content with
                                          | S_Text t -> format_arg t false;
                                          | _ -> List.iter
                                                   cont_trans
                                                   a.s_childs;
                                        );
                                      | [] -> ();   (* ???? *)
                                    );

                                    if (!i < n) then
                                    begin
                                        out_str " -> ";
                                    end;
                                    nl ();
                                    incr i;
                                  ) args;
                    end; 
                    out_str (nl'()^(spaces env.doc_indent));
                end;
            

            | S_Type_Interface  
            | S_Struc_Interface 
            | S_Exc_Interface ->
                begin
                    let ind = ref env.doc_col in                    
                    
                    let tp = 
                        match env.in_special with
                        | Spec_Type_Int tp -> tp;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let tname = tp.type_name in
                    let args = tp.type_args in
                    let tp_type = tp.type_type in
                    let comm = tp.type_comm in
                    
                    if (tp_type <> Type_Exception) then
                        env.special_depth <- env.special_depth - 1;

                    env.in_special <- Spec_None; 

                    if (tname = []) then
                        syntax_error "Type interface without name";
                    
                    nl ();

                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;

                    ind := env.doc_col;
                    
                    if (tp_type <> Type_Exception) then
                        out_str "type "
                    else
                        out_str "exception ";

                    List.iter cont_trans 
                               (List.hd tname).s_childs;

                    

                    if (tp_type <> Type_Exception) then
                        out_str (
                            " = "^
                                (
                                    match tp_type with
                                    | Type_List -> "";
                                    | Type_Structure -> " { ";
                                    | _ -> 
                                        syntax_error "Invalid Type";
                                )
                        );
    
                    ind := env.doc_col - !ind;

                    
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if(!i>1) then
                                        out_str (spaces !ind);
                                    
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    List.iter
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t ->
                                                out_str (str_of_struc na);
                                          | S_Comment -> com := Some na;
                                          | S_Mutable -> 
                                            begin
                                                out_str (
                                                    "mutable "
                                                );
                                            end;
                                          | _ -> cont_trans na;
                                    ) a.s_childs; 
                        


                                    if (!i < n) then
                                    begin
                                        match tp_type with
                                        | Type_List      -> out_str " |";
                                        | Type_Structure -> out_str ";";
                                        | _ ->
                                            syntax_error "Invalid Type";
                                    end
                                    else
                                    begin
                                        match tp_type with
                                        | Type_List      -> ();
                                        | Type_Structure -> out_str " } ";
                                        | _ ->
                                            syntax_error "Invalid Type";
                                    end;
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            cont_trans com;
                                          end    
                                        | None -> ();  
                                    );
                                    
                                    nl ();
                                    incr i;
                                  ) args;
                    end; 
                    out_str (nl'()^(spaces env.doc_indent));
                end;


            | S_C_Var_Interface -> 
                begin
                    if (env.in_interface = true) then
                    begin
                        ();
                    end;
                    if (env.in_interface = true) then
                    begin
                        ();
                    end;


                end;

            | S_C_Hdr_Interface -> 
                begin
                    let ind = ref env.doc_col in                    

                    let hr = 
                        match env.in_special with
                        | Spec_Hdr_Int hr -> hr;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let hname = hr.hdr_name in
                    let comm = hr.hdr_comm in
                    
                    env.in_special <- Spec_None; 

                    if (hname = []) then
                        syntax_error "C-Hdr interface without name";
                    
                    nl ();
                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;

                    out_str "#include < ";

                    List.iter cont_trans
                            (List.hd hname).s_childs;
                     
                    out_str ">;";                    
                    out_str (nl'()^(spaces env.doc_indent));
                end;


            | S_C_Fun_Interface -> 
                begin
                    let ind = ref env.doc_col in                    

                    let fi = 
                        match env.in_special with
                        | Spec_Fun_Int fi -> fi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 

                    let rargs = fi.fun_retargs in
                    let fname = fi.fun_name in
                    let args = fi.fun_args in
                    let curried = fi.fun_curried in
                    let comm = fi.fun_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (fname = []) then
                        syntax_error "C-Function interface without name";
                    
                    nl ();
                    if (comm <> []) then
                    begin
                        cont_trans (List.hd comm);
                        nl ();
                    end;
                    

                    let rargs_n = List.length rargs in
                    if (rargs_n = 1) then   (* only 1 argument ! *)
                    begin
                             
                            let r = List.hd rargs in           
                            (
                                    nl ();
                                    ind := env.doc_col;

                                    List.iter
                                            cont_trans
                                            r.s_childs;


                                    out_str " ";
                                    List.iter cont_trans 
                                                (List.hd fname).s_childs;
                                    out_str "(";
                                    ind := env.doc_col - !ind;
                            );

                    end;
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i>1) then
                                        out_str (spaces !ind);

                                    let com = ref None in

                                    List.iter
                                    ( fun na ->
                                        match na.s_content with
                                        | S_Comment -> com := Some na;
                                        | _ -> cont_trans na
                                    )a.s_childs;

                                    if (!i < n) then
                                        out_str ","
                                    else
                                        out_str ");";
                                        
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            cont_trans com;
                                          end    
                                        | None -> ();  
                                    );

                                    nl ();
                                
                                    incr i;
                                  ) args;
                    end; 
                    out_str (nl'()^(spaces env.doc_indent));
                end;


            | S_Interface -> 
                begin
                    out_str (nl'());
                    ind_decr (); 
                    env.in_interface <- false;
                end;


            | S_CurArg -> 
                    env.special_depth <- env.special_depth - 1;
                    

            | S_UnCurArg -> 
                    env.special_depth <- env.special_depth - 1;

            | S_RetArg -> 
                    env.special_depth <- env.special_depth - 1;


        );
    in
    if (env.doc_terminal = true) then
    begin  
        ignore(Terminfo.setup oc_chan);
        try
            cont_trans ds
        with
            | Exit -> ();
            | Failure s -> syntax_error s
    end
    else
    begin
        try
            cont_trans ds
        with
            Failure s -> syntax_error s
    end;

    if (env.doc_to_stdout = false) then
        close_out oc_chan
