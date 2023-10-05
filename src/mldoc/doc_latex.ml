(* 
** This file is part of the MLDOC System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              17/09/2002
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
** LaTeX backend.
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
    | Spec_Fun_Int of fun_int 
    | Spec_Hdr_Int of hdr_int
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


type list_type = O_List | U_List | P_List

type env = {
    
    mutable doc_name: string;
    mutable doc_head_inline: bool;
    mutable doc_no_toc: bool;
    mutable doc_color: bool;
    mutable doc_link_ref: bool;
    
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
    mutable in_module: bool;        

    mutable in_olist: int;
    mutable in_ulist: int;
    mutable in_list: list_type list;

    mutable in_subsup: bool;
    
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

    mutable table_col   : int;
    mutable textwidth   : float;
}

type tex_options = 
    | TEX_doc_name of string
    | TEX_head_inline
    | TEX_color
    | TEX_no_toc
    | TEX_link_ref

let cur_sb = ref empty_block

let syntax_error s =
    failwith (
            "tex_of_tree: "^
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
** Transform the structure tree to Tex 
**
** Argument: 
**
**  ds: structure tree
**
*)


let tex_of_tree ~ds ~options ~sections =

    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = "main"; sec_type = ""} 
    in  

    let env = {
        doc_head_inline = false;
        doc_name        = "manual.tex";
        doc_no_toc      = false;
        doc_color       = false;
        doc_link_ref    = false;
            
        ignore_head     = false;
        ignore_childs   = false;
        in_special      = Spec_None;
        special_depth   = 0;

        in_preform      = false;
        in_example      = false;
        in_interface    = false;

        in_class        = Spec_None;
        in_object       = Spec_None;
        in_module       = false;
        
        in_olist        = 0;
        in_ulist        = 0;
        in_list         = [];

        in_subsup       = false;
        
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
        doc_col         = 1;
        doc_colwidth    = 80;

        table_col       = 0;
        textwidth       = 16.5;
    } in

    (*
    ** options
    *)

    List.iter (fun o ->
                match o with
                | TEX_doc_name s  ->   env.doc_name <- s;
                | TEX_head_inline ->   env.doc_head_inline <- true;
                | TEX_color       ->   env.doc_color <- true;
                | TEX_no_toc      ->   env.doc_no_toc <- true;
                | TEX_link_ref    ->   env.doc_link_ref <- true;
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
        | 1 -> "$\\blacktriangleright$";
        | 2 -> "$\\vartriangleright$";
        | 3 -> "$\\lozenge$";
        | _ -> "$\\circ$";
    in

    (*
    ** current output channel
    *)

    let oc_chan = open_out env.doc_name in
    let oc_name = env.doc_name in


    (*
    ** Printing
    *)

    let spaces num =
        let fmt = "%"^(string_of_int env.doc_indent)^"s" in
        Printf.sprintf (Obj.magic fmt) ""
    in

    let out_str str =
        let slen = String.length str in
        let oc = oc_chan in
        if (env.doc_col + slen > env.doc_colwidth &&
            env.in_preform = false &&
            env.in_example = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        output_string oc str;
        env.doc_col <- env.doc_col + slen;
    in

    let nl () =
        let oc = oc_chan  in
        if (env.doc_col > env.doc_indent && 
            env.in_preform = false &&
            env.in_example = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
        else if (env.in_example = true) then
        begin 
            output_string oc ("} &&& \\cr\n");
            output_string oc ((spaces env.doc_indent)^
                              "& {\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end
        else if (env.in_preform = true) then
        begin 
            output_string oc ("} \\cr\n");
            output_string oc ((spaces env.doc_indent)^
                              "{\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end;
        
    in

    let ind_incr () =
        env.doc_indent <- env.doc_indent + 2;
        output_string (oc_chan) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in
    let ind_decr () =
        env.doc_indent <- env.doc_indent - 2;
        output_string (oc_chan) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in


    (*
    ** Replace special characters in text strings
    *)

    let next_c = ref ' ' in
    let ignore_next = ref false in
        
    let trans_char c =
            let str_of_c = String.create 1 in
            match c with
            | 'a'..'z'  (* Speedup ? *)
            | 'A'..'Z' 
            | '0'..'9' -> str_of_c.[0] <- c; str_of_c;
            | '~' -> "$^\sim$";
            | '=' -> "$=$";
            | '#' -> "$\\#$";
            | '*' -> "$*$";
            | '_' -> "$\\_$";
            | '{' -> "\\{";
            | '}' -> "\\}";
            | '\\' -> "$\\setminus$";
            | '-' -> 
                begin
                     match !next_c with
                     | '>' ->  ignore_next := true;
                               "$\\rightarrow$";
                    
                     | _ -> "$-$"; 
                end;
            | '>' -> "$>$";
            | '<' -> 
                begin
                     match !next_c with
                     | '-' ->  ignore_next := true;
                               "$\\leftarrow$";
                    
                     | _ -> "$<$"; 
                end;
            | _ -> str_of_c.[0] <- c; str_of_c;
    in
            
    let trans_line l =
            let llen    = String.length l in
            let s       = ref "" in
            
                
    
            for i = 0 to llen-1
            do
                if (i < (llen-1)) then
                    next_c := l.[i+1]
                else
                    next_c := ' ';
                
                    
                if (!ignore_next = false) then
                    s := !s^(trans_char l.[i])
                else
                    ignore_next := false;
            done;
            !s   
    in

    let tex_text text =
        List.iter (fun s -> if (env.in_subsup=false) then
                            begin
                                if (s<>"\n") then
                                    out_str ((trans_line s)^" ")
                                else
                                    nl ();
                            end
                            else
                            begin
                                out_str ((trans_line s)^"\\quad")
                            end;
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
            | T_Bold    ->      out_str "{\\bf ";
            | T_Italic  ->      out_str "{\\it ";
            | T_BoldItalic ->   out_str "{\\bf\\sl ";
            | T_Type ->         out_str "{\\tt ";
            | T_Symbol ->       ();
            | T_AsIs ->         out_str "{\\tt "; 
            | T_Superscript ->  out_str "$^{\\rm ";
                                env.in_subsup <- true;
            | T_Subscript ->    out_str "$_{\\rm ";
                                env.in_subsup <- true;
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
            | T_AsIs ->         out_str "}"; 
            | T_BoldItalic ->   out_str "}";
            | T_Symbol -> ();
            | T_Superscript 
            | T_Subscript ->    out_str "}$";
                                env.in_subsup <- false;
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
        begin
            tex_text  [!a_str]
        end
        else
        begin
            if (!a_name.[1] <> '~' &&
                !a_name.[1] <> '?') then
            begin


                if ((String.length !a_name) >= 8 &&
                    (String.sub !a_name 0 8)=" mutable") then
                begin
                    out_str "\\textcolor{red}{mutable} ";
                    a_name := " "^(
                                    String.sub !a_name 8 
                                         ((String.length !a_name)-8)
                                  )^" ";
                end;

                (*
                ** It's a symbolic name, not a label. Mark it.
                *)
                out_str "{\\it";
                tex_text  [!a_name];
                out_str "}";
                tex_text  [":";!a_type];
            end
            else
            begin
                if (!a_name.[1] = '~') then
                    tex_text  [
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
                    tex_text [!a_name;":";!a_type];
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
            | _ -> syntax_error
                    "str_of_text: not a S_Text element";
        ) tln;
        trans_line (!str) 
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

        if (env.doc_link_ref = false) then
            out_str ("\\fbox{\\sc\\small "^ str ^"} ")
        else
            out_str (
                "\\fbox{\\small {\\sc"^ 
                str 
                ^"} (p. \\pageref{"^
                (file_name str)^
                "})} "
            );
    in


    (*
    ** Print a table of content starting at a specific section.
    *)
    let print_toc sec =
      if (env.doc_no_toc=false) then
      begin
        let depth = ref 1 in
        nl (); out_str "\\vfill \\eject"; nl ();

        out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
        out_str "\\hrule"; nl ();
        out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad \\hfill"; 
        ind_incr (); nl ();
                                        
        out_str (
                    (if(env.doc_color=true) then
                        "\\textcolor{blue}"
                     else "")^
                     "{\\sc Table of Content "
                ); 

        out_str "} \\hfill"; nl ();
        out_str "\\quad \\vrule}\\hrule}"; 
        ind_decr (); nl (); ind_decr (); nl ();
        out_str "\\vskip \\baselineskip"; nl ();


        let rec iter s =
            if (s.sec_name <> "") then
            begin
                out_str (
                               "\\vbox{\\sc "^(
                                    if (s.sec_type <> "") then
                                        (s.sec_type^": ")
                                    else
                                        ""
                                  )^ 
                            s.sec_name^
                            "\\quad\\dotfill\\quad\\pageref{"^
                            (file_name s.sec_name)^
                            "}}"
                           
                ); nl ();
            end;

            if (s.sec_childs <> []) then
            begin
                out_str "\\vskip .2\\baselineskip"; nl();
                out_str (
                            "\\vbox{\\leftskip"^
                            (string_of_int (!depth*20))^
                            "pt "
                );
                incr depth;
                List.iter iter s.sec_childs;
                out_str "}"; nl ();
                out_str "\\vskip .2\\baselineskip"; nl();
                decr depth;
            end;
        in
        if (sec.sec_childs <> []) then
        begin
            List.iter iter sec.sec_childs;
        end;
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
        out_str (
                "\\label{"^
                (file_name sec.sec_name)^
                "}"
        );
    in
                

    (*
    ** A section was closed. Change to the parent again.
    *)

    let close_sec env =
        let next = env.cur_sec.sec_childs in
        env.cur_sec <- env.cur_sec.sec_parent;
    in


    let head () =  
        out_str "\\documentclass{report}"; nl ();
        out_str "\usepackage{newcent}"; nl ();
        out_str "\usepackage{pifont}"; nl ();
        out_str "\usepackage[dvips]{color}\n"; nl (); 

        out_str "\\parindent 0pt"; nl (); 
        out_str "\\pagestyle{plain}"; nl ();
        out_str "\\topmargin0cm"; nl ();
        out_str "\\headheight0pt"; nl ();
        out_str "\\headsep0pt"; nl ();
        out_str "\\textwidth16.5cm"; nl ();
        out_str "\\textheight23cm"; nl ();
        out_str "\\footskip1cm"; nl ();
        out_str "\\oddsidemargin0cm"; nl ();
        out_str "\\evensidemargin0cm\n"; nl ();
        if (env.doc_color=true) then
        begin
            out_str "\\renewcommand{\\labelitemi}{\\textcolor{blue}{\\ding{228}}}"; nl ();
            out_str "\\renewcommand{\\labelitemii}{\\textcolor{blue}{\\ding{169}}}"; nl ();
            out_str "\\renewcommand{\\labelitemiii}{\\textcolor{blue}{\\ding{72}}}"; nl ();
        end
        else
        begin
            out_str "\\renewcommand{\\labelitemi}{\\ding{228}}"; nl ();
            out_str "\\renewcommand{\\labelitemii}{\\ding{169}}"; nl ();
            out_str "\\renewcommand{\\labelitemiii}{\\ding{72}}"; nl ();
        end;
        out_str "\\renewcommand{\\arraystretch}{1.1}"; nl ();
        out_str "\\begin{document}"; nl ();
    in

    let tail () =
            out_str "\\end{document}"; nl ();
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
                    head ();
                end;
            | S_Empty -> ();
            | S_Text t -> tex_text  t;

            | S_NL -> out_str "\\\\"; nl ();
            | S_TAB -> output_string (oc_chan) "\\hskip 1em ";

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
                    | Spec_None -> out_str "{$\\gg\\:$\\it ";
                end;

            | S_Paragraph -> 
                begin
                    out_str "\\vskip .5\\baselineskip "; nl ();
                    out_str "{\\parindent 0pt \\vbox{\\sc ";
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
                    out_str "}"; nl (); 
                    out_str "\\leftskip 20pt "; 
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  
                end;

            | S_Attribute -> parse_attr_start ds.s_attr;

            | S_Example -> 
                begin
                    nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                    out_str (
                        "\\vbox{\\hfil"; 
                    );
                    ind_incr (); nl ();
                    out_str "\\vbox{\\hrule"; 
                    ind_incr (); nl ();
                    out_str ( 
                        "\\hbox{\\vrule\\qquad"
                    ); 
                    ind_incr (); nl ();


                

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
                                        out_str ( 
                                                (if env.doc_color=true then
                                                    "\\textcolor{blue}"
                                                 else
                                                    "")^
                                                "{\\rule[-.55em]{0pt}{1.7em}"
                                        );
                                        cont_trans dna;
                                        out_str "}";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> 
                                        out_str ( 
                                           (if env.doc_color=true then
                                                "\\textcolor{blue}"
                                            else
                                                "")^
                                            "{\\rule[-.55em]{0pt}{1.7em}"^
                                            "\\sc Programming Example}"
                                        );
                                    
                            end; 
                            | [] -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor{blue}"
                                         else
                                            "")^
                                        "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                        "Programming Example}"
                                    );

                    );

                    out_str "\\quad\\vrule}"; 
                    ind_decr (); nl (); 
                    out_str "\\hrule}";
                    ind_decr (); nl ();
                    out_str "}";
                    ind_decr (); nl ();
                                        
                    out_str "\\nobreak"; 
                    out_str "\\vskip \\baselineskip\\nobreak"; nl ();

                    nl (); out_str "\\halign to \\textwidth{"; ind_incr ();
                    nl (); out_str (
                        "\\textcolor[rgb]{.58,.76,1.0}{\\strut\\vrule width 5pt#}"^
                        "\\tabskip=0pt plus 1fil& "
                        );
                    nl (); out_str "  #\\hfil& #&\\hfil#\\hfil& ";
                    nl (); out_str "  \\tabskip=0pt#\\cr";
                
                    nl (); out_str " &&&\\cr "; nl ();

                    env.in_example <- true;
                    out_str "& {\\tt \\hskip 1em"; 
                end;

            | S_Preform -> 
                begin
                    nl (); out_str "\\vskip 2\\baselineskip"; nl ();
                    nl (); out_str "\\halign{"; ind_incr ();
                    nl (); out_str "\\hskip3em#\\hfil\\cr"; nl ();
                    out_str " \\cr"; nl ();
                    out_str "{\\tt \\hskip 1em"; 
                    env.in_preform <- true;
                end;

            | S_Link -> 
                begin
                    link ds.s_childs;
                    env.ignore_childs <- true; 
                end;

            | S_OList -> 
                begin
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                        out_str "\\begin{minipage}{.9\\textwidth}\\vbox{"; 
                        ind_incr (); 
                    end;

                    out_str (
                        "\\begin{enumerate}"
                    ); ind_incr (); nl (); 
                    env.in_olist <- env.in_olist + 1; 
                    env.in_list <- [O_List] @ env.in_list;
                end;
                                
            | S_UList -> 
                begin
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                        out_str "\\begin{minipage}{.9\\textwidth}\\vbox{"; 
                        ind_incr (); 
                    end;

                    out_str (
                        "\\begin{itemize}"
                    ); ind_incr (); nl ();
                    env.in_ulist <- env.in_ulist + 1;
                    env.in_list <- [U_List] @ env.in_list;
                end;

            | S_OpList ->
                begin
                    nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                    out_str (
                        "\\vbox{\\hfil"; 
                    );
                    ind_incr (); nl ();
                    out_str "\\vbox{\\hrule"; 
                    ind_incr (); nl ();
                    out_str ( 
                        "\\hbox{\\vrule\\qquad"
                    ); 
                    ind_incr (); nl ();

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
                                        out_str ( 
                                            "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                            pname^ 
                                            " }"
                                        );
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> 
                                    begin
                                        out_str (
                                            "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                            "Options }"
                                        ); 
                                    end;
                            end; 
                        | [] -> 
                            begin
                                out_str (
                                    "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                    "Options }"
                                );     
                            end;
                    
                    );

                    out_str "\\qquad\\vrule}"; 
                    ind_decr (); nl (); 
                    out_str "\\hrule}";
                    ind_decr (); nl ();
                    out_str "}";
                    ind_decr (); nl ();
                                        
                    out_str "\\nobreak"; 
                    out_str "\\vskip \\baselineskip\\nobreak"; nl ();

                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  

                    nl (); out_str "\\begin{description}";
                    ind_incr (); nl ();           
                    env.in_list <- [P_List] @ env.in_list;
                end;

            | S_List_Item -> 
                begin
                    match (List.hd env.in_list) with
                    | P_List ->
                        begin
                            nl (); out_str "\\item["; 
                            (*
                            ** The first child must be the option name.
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
                                    | _ -> syntax_error "option list item without name";
                                end; 
                                | [] -> syntax_error "option list item without name";
                    
                            );
                            out_str "]"; ind_incr ();
                            nl (); out_str "{\\hfill\\\\}"; nl ();   
                        end;
                        
                    | O_List | U_List ->
                        begin
                            out_str ( 
                                "\\item "
                            ); 
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
                    out_str "\\begin{center}"; nl ();
                    out_str ( 
                            "\\vbox{\\hrule\\vskip2pt\\hrule\\bf\\large"^
                            "\\vskip \\baselineskip"
                    ); nl ();
                    
                end;

            | S_TOC -> ();

            | S_Program -> 
                begin

                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad \\hfill"; 
                    ind_incr (); nl ();

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
                                        
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor{blue}"
                                             else "")^
                                             "{\\bf Program: "
                                        ); 
                                        cont_trans dna;
                                        out_str "}";
                                        env.ignore_head <- true;
                                        env.cur_program <- Some sec;
                                    end;
                                | _ -> syntax_error "Program without name"; 
                            end; 
                        | [] -> syntax_error "Program without name";
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    
                    );
                end;

            | S_Package -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad \\hfill"; 
                    ind_incr (); nl ();

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
                                        
                                        out_str (
                                                (if(env.doc_color=true) then
                                                    "\\textcolor{blue}"
                                                 else "")^
                                                "{\\sc Package: "
                                        ); 
                                        cont_trans dna;

                                        out_str "} \\hfill"; nl ();
                                         
                                        env.ignore_head <- true;
                                        env.cur_package <- Some sec;
                                    end;
                                | _ -> syntax_error "Package without name"; 
                            end; 
                        | [] -> syntax_error "Package without name";
                    
                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_Intro -> ();

            | S_S1 -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad \\hfill"; 
                    ind_incr (); nl ();


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
                                        
                                        out_str (
                                                (if env.doc_color=true then
                                                    "\\textcolor{blue}"
                                                 else
                                                    "")^
                                                "{\\sc "
                                        ); 
                                        cont_trans dna;

                                        out_str "} \\hfill"; nl ();
                                         
                                        env.ignore_head <- true;
                                        env.cur_s1 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end; 
                        | [] -> syntax_error "Section without name";
                    
                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_S2 -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();


                    (*
                    ** The first child must be the subsection name.
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
                                                    sec_type="";
                                        } in

                                        new_sec env sec;
                                        
                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\sc ";
                                        cont_trans dna;
                                        out_str "} \\hfill ";
                                        env.ignore_head <- true;
                                        env.cur_s2 <- Some sec;
                                    end;
                                | _ -> syntax_error "Subsection without name"; 
                            end; 
                        | [] -> syntax_error "Subsection without name";
                    
                    );
                    nl ();
           

                    (
                        if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc Package: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc Program: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
    
                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_S3 -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();


                    (*
                    ** The first child must be the unit name.
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
                                                    sec_type="";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\sc ";
                                        cont_trans dna;    
                                        out_str "} \\hfill";

                                        env.cur_s3 <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Unit without name"; 
                            end; 
                        | [] -> syntax_error "Unit without name";
                    
                    );
                    nl ();

                    (
                        if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc "^
                                        sec.sec_name^
                                        "}"
                                    );
                              | None -> ();
                        end
                        else if (env.cur_module <> []) then
                        begin
                            out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                                    );
                        end
                        else if (env.cur_clib <> None) then
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc C-Library: "^
                                        sec.sec_name^
                                        "}"
                                    );
                              | None -> ();
                        end

                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_S4 -> 
                begin
                    out_str "\\vskip .5\\baselineskip "; nl ();
                    out_str "{\\parindent 0pt \\vbox{\\sc ";

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
                    out_str "}"; nl (); 
                    out_str "\\leftskip 20pt \\rightskip 20pt ";
                    out_str "\\vskip .5\\baselineskip";
                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  
                end;

            | S_Desc -> 
                begin
                    out_str "\\vskip .5\\baselineskip "; nl ();
                    out_str "{\\parindent 0pt \\vbox{\\sc ";
                    out_str "Description";
                    out_str "}"; nl (); 
                    out_str "\\leftskip 20pt \\rightskip 20pt ";
                    out_str "\\vskip .5\\baselineskip";
                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  
                end;


            | S_Clib -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();



                    (*
                    ** The first child must be the clib name.
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
                                                    sec_type="C-Library";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\sc C-Library: ";
                                        cont_trans dna;
                                        out_str "} \\hfill ";

                                        env.ignore_head <- true;
                                        env.cur_clib <- Some sec;
                                    end;
                                | _ -> syntax_error "C-Lib without name"; 
                            end; 
                        | [] -> syntax_error "C-Lib without name";
                    
                    );
                    nl ();
           

                    (
                        if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                            | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc Package: "^
                                        sec.sec_name^
                                        "}"
                                    );
                            | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with
                            | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc Program: "^
                                        sec.sec_name^
                                        "}"
                                    );
                            | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                            | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc "^
                                        sec.sec_name^
                                        "}"
                                    );
                            | None -> ();
                        end;
                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;


            | S_Module -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();


                    (*
                    ** The first child must be the module name.
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
                                                    sec_type="ML-Module";
                                        } in

                                        new_sec env sec;

                                        out_str ( 
                                            (if env.doc_color=true then
                                                "\\textcolor{blue}"
                                             else
                                                "")^
                                            "{\\sc ML-Module: "
                                        );
                                        cont_trans dna;
                                        out_str "} \\hfill ";

                                        env.ignore_head <- true;

                                        env.cur_module <- 
                                            [sec] @ env.cur_module;
                                    end;
                                | _ -> syntax_error "Module without name"; 
                            end; 
                        | [] -> syntax_error "Module without name";
                    
                    );
                    nl ();
           

                    (
                        if (List.length env.cur_module > 1) then
                        begin
                            (*
                            ** This is a submodule !
                            *)
                           out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.nth env.cur_module 1).sec_name^
                                        "}"
                                        );
                        end
                        else if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                              | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc Package: "^
                                        sec.sec_name^
                                        "}"
                                    );
                              | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                     out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc "^
                                        sec.sec_name^
                                        "}"
                                     );
                              | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with  (* ??? *)
                              | Some sec -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc Program: "^
                                        sec.sec_name^
                                        "}"
                                    );
                              | None -> ();
                        end

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;


            | S_Function -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();

                    (*
                    ** The first child must be the function name.
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
                                                    sec_type="ML-Function";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";

                                        out_str "{\\sc ML-Function: ";
                                        cont_trans dna;    
                                        out_str "} \\hfill ";

                                        env.cur_function <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Function without name"; 
                            end; 
                        | [] -> syntax_error "Function without name";
                    
                    );
                    nl ();

                    (
                        if (env.cur_module <> []) then
                        begin
                           out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                                        );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc C-Library: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end;

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;


            | S_Type -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();

                    (*
                    ** The first child must be the type name.
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
                                                    sec_type="ML-Type";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";

                                        out_str "{\\sc ML-Type: ";
                                        cont_trans dna;    
                                        out_str "} \\hfill ";

                                        env.cur_type <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Type without name"; 
                            end; 
                        | [] -> syntax_error "Type without name";
                    
                    );

                    nl ();
                    (
                        if (env.cur_module <> []) then
                        begin
                            out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc C-Library: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end;

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_Value -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();
                    (*
                    ** The first child must be the value name.
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
                                                    sec_type="ML-Value";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";

                                        out_str "{\\sc ML-Value: ";
                                        cont_trans dna;    
                                        out_str "} \\hfill ";

                                        env.cur_val <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Value without name"; 
                            end; 
                        | [] -> syntax_error "Value without name";
                    
                    );

                    nl ();
                    (
                        if (env.cur_module <> []) then
                        begin
                            out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc C-Library: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end;

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_Class -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();
                    (*
                    ** The first child must be the value name.
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
                                                    sec_type="ML-Class";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";

                                        out_str "{\\sc ML-Class: ";
                                        cont_trans dna;    
                                        out_str "} \\hfill ";

                                        env.cur_class <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Class without name"; 
                            end; 
                        | [] -> syntax_error "Class without name";
                    
                    );

                    nl ();
                    (
                        if (env.cur_module <> []) then
                        begin
                            out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc C-Library: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end;

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                end;


            | S_C -> 
                begin
                    if (env.doc_head_inline = true) then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end
                    else
                    begin
                        nl (); out_str "\\vfill \\eject"; nl ();
                    end;

                    out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
                    out_str "\\hrule"; nl ();
                    out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad "; 
                    ind_incr (); nl ();
                    (*
                    ** The first child must be the value name.
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
                                                    sec_type="C";
                                        } in

                                        new_sec env sec;

                                        if env.doc_color=true then
                                            out_str "\\textcolor{blue}";

                                        out_str "{\\sc C: ";
                                        cont_trans dna;    
                                        out_str "} \\hfill ";

                                        env.cur_cint <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "C without name"; 
                            end; 
                        | [] -> syntax_error "C without name";
                    
                    );

                    nl ();
                    (
                        if (env.cur_module <> []) then  (* ??? *)
                        begin
                            out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\sc ML-Module: "^
                                        (List.hd env.cur_module).sec_name^
                                        "}"
                            );
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then 
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                            (if env.doc_color=true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\sc C-Library: "^
                                            sec.sec_name^
                                            "}"
                                        );
                              | None -> ();
                        end;

                    );
                    out_str "\\quad \\vrule}\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
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
                        "\\textcolor{red}{mutable }"
                    );
                end;
            | S_Virtual -> 
                begin
                    out_str ( 
                        "\\textcolor{red}{virtual }"
                    );
                end;
            | S_Private -> 
                begin
                    out_str ( 
                        "\\textcolor{red}{private }"
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
                    env.in_module <- true;

                    if (env.in_interface = false) then
                    begin
                        out_str "\\vskip 0.5\\baselineskip"; nl ();
                    end;
                                        
                    nl (); out_str "\\halign{"; ind_incr ();
                    nl (); out_str (
                            "\\textcolor{red}{\\strut\\vrule width 5pt"^
                            "\\qquad#}\\tabskip=0pt plus 1fil& "
                            );
                    nl (); out_str "  #\\hfil& #&\\hfil#\\hfil& ";
                    nl (); out_str "  \\tabskip=0pt#\\cr";
                    
                    if (env.in_interface = true) then
                    begin
                        out_str " &&&& \\cr"; nl ();
                    end;

                    out_str " module & ";
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
                                        out_str "{\\bf ";
                                        List.iter cont_trans
                                            dna.s_childs;
                                        out_str "}";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Module without name"; 
                            end; 
                            | [] -> syntax_error "Module without name";
                    );

                    out_str " &&& \\cr";
                    out_str " & sig &&& \\cr"; nl ();
                    
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



            | S_Struc_Interface -> 
                                env.in_special <- Spec_Type_Int {
                                    type_name = [];
                                    type_args = [];
                                    type_type = Type_Structure;
                                    type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

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
                    nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                    out_str (
                        "\\vbox{\\hfil"; 
                    );
                    ind_incr (); nl ();
                    out_str "\\vbox{\\hrule"; 
                    ind_incr (); nl ();
                    out_str ( 
                        "\\hbox{\\vrule\\qquad"
                    ); 
                    ind_incr (); nl ();


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
                                        out_str ( 
                                                (if env.doc_color=true then
                                                    "\\textcolor{red}"
                                                 else
                                                    "")^
                                                "{\\rule[-.55em]{0pt}{1.7em}"^
                                                "\\sc Programming Interface: "
                                        );
                                        cont_trans dna;
                                        out_str "}";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> 
                                        out_str ( 
                                           (if env.doc_color=true then
                                                "\\textcolor{red}"
                                            else
                                                "")^
                                            "{\\rule[-.55em]{0pt}{1.7em}"^
                                            "\\sc Programming Interface}"
                                        );
                                    
                            end; 
                            | [] -> 
                                    out_str (
                                        (if env.doc_color=true then
                                            "\\textcolor{red}"
                                         else
                                            "")^
                                        "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                        "Programming Interface}"
                                    );

                    );


                    out_str "\\quad\\vrule}"; 
                    ind_decr (); nl (); 
                    out_str "\\hrule}";
                    ind_decr (); nl ();
                    out_str "}";
                    ind_decr (); nl ();
                                        
                    out_str "\\nobreak"; 
                    out_str "\\vskip \\baselineskip\\nobreak"; nl ();
                    out_str "{"; ind_incr (); nl ();
                    env.in_interface <- true;

(*
                    nl (); out_str "\\halign to \\textwidth{"; ind_incr ();
                    nl (); out_str (
                            "\\textcolor{red}{\\strut\\vrule width 5pt"^
                            "#}\\tabskip=0pt plus 1fil& "
                           );
                    nl (); out_str "  #\\hfil& #&\\hfil#\\hfil& ";
                    nl (); out_str "  \\tabskip=0pt#\\cr";


                    out_str (
                                "\\noalign{\\nobreak\\vskip "^
                                ".5\\baselineskip}"
                    ); nl ();
*)
                    out_str (
                                "\\nobreak\\vskip "^
                                ".5\\baselineskip "
                    ); nl ();

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
                    | Spec_Hdr_Int hr -> ();
                    | Spec_None -> 
                        begin
                            out_str "{\\sl ";
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

                    | Spec_Method_Int mi -> mi.meth_args <- mi.meth_args @ [ds];

                    | Spec_Type_Int ti -> ti.type_args <- ti.type_args @ [ds];

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
                            out_str "$\\ll$}";
                        end;            
                end;

            | S_Paragraph -> 
                begin
                    out_str "}\\end{list}}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                end;

            | S_Attribute -> parse_attr_end ds.s_attr;

            | S_Example -> 
                begin
                    env.in_example <- false;
                    out_str "\\hfil} &&& \\cr "; nl ();
                    out_str "}";
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                end;

            | S_Preform -> 
                begin
                    env.in_preform <- false;
                    out_str "\\hfil} \\cr "; nl ();
                    out_str "}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                end;

            | S_OList ->    
                begin
                    out_str "\\end{enumerate}"; 
                    ind_decr (); nl ();
                    env.in_olist <- env.in_olist - 1;
                    env.in_list <- List.tl env.in_list;

                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str "}\\end{minipage}"; ind_decr ();
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end;
                end;

            | S_UList ->    
                begin
                    out_str "\\end{itemize}"; 
                    ind_decr (); nl ();
                    env.in_ulist <- env.in_ulist - 1;

                    env.in_list <- List.tl env.in_list;
                    
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str "}\\end{minipage}"; ind_decr ();
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end;
                end;

            | S_OpList ->
                begin
                    nl (); out_str "\\end{description}"; ind_decr (); nl ();
                    env.in_list <- List.tl env.in_list;

                    out_str "}\\end{list}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    nl (); out_str "\\vskip \\baselineskip"; nl ();
                end;

            | S_List_Item -> 
                begin
                    match (List.hd env.in_list) with
                    | P_List ->
                        begin
                            ind_decr (); nl ();
                        end;
                    | O_List | U_List ->
                        nl ();
                end;

            | S_Name -> 
                begin
                    let d () = env.special_depth <- env.special_depth - 1 in
                    match env.in_special with
                    | Spec_Fun_Int  fi  -> d ();
                    | Spec_Hdr_Int hr -> ();
                    | Spec_Val_Int  vi  -> d (); 
                    | Spec_Type_Int ti  -> d ();
                    | Spec_Class_Int ti  -> d ();
                    | Spec_Object_Int ti  -> d ();
                    | Spec_Method_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_None -> 
                        begin
                            out_str "}";
                        end;            
                end;

            | S_Link -> 
                    out_str "";

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
                ** Calcualte approx. column widths. Thanks to Tex.
                *)
                let tablewidth,colwidth =
                    let cwidth = 0.3 in
                    let cols = env.table_col in
                    let fcols = float_of_int cols in
                    let ar = Array.create (cols+1) 0.0 in
                    let maxwidth = (env.textwidth *. 0.8) /. 
                                   (float_of_int cols) in 
                    let minwidth = cwidth *. 5.0 in

                    let tablewidth = ref 0.0 in
                        
                    List.iter ( fun r ->
                        let icol = ref cols in
                        List.iter ( fun tc ->
                            let str = str_of_struc tc in
                            let strl = float_of_int (String.length str)
                            in
                            if (strl*.cwidth<maxwidth &&
                                strl*.cwidth>minwidth) then
                            begin
                                if (strl*.cwidth > ar.(!icol)) then
                                    ar.(!icol) <- strl*.cwidth
                            end
                            else if (strl*.cwidth < minwidth) then
                            begin
                                if (minwidth > ar.(!icol)) then
                                    ar.(!icol) <- minwidth
                            end
                            else
                                ar.(!icol) <- maxwidth;
                            decr icol;
                        ) r;
                    ) tr;
                    for i = cols downto 1 
                    do
                        tablewidth := !tablewidth +. ar.(i);
                    done;

                    (*
                    ** Look for the table head and do the final adjustment.
                    *)
                    if (th <> []) then
                    begin
                        let str = str_of_struc (List.hd th) in
                        let strl = float_of_int (String.length str) in

                        if(strl*.cwidth > !tablewidth &&
                           strl*.cwidth < maxwidth*.fcols) then
                        begin
                            tablewidth := (strl+.1.0)*.cwidth;
                            for i = cols downto 1
                            do
                                ar.(i) <- !tablewidth /. fcols;
                            done; 
                        end
                        else if(strl*.cwidth > !tablewidth &&
                                strl*.cwidth > maxwidth*.fcols) then
                        begin
                            tablewidth := env.textwidth*.0.7;
                            for i = cols downto 1
                            do
                                ar.(i) <- !tablewidth /. fcols;
                            done; 
                        end
                        
                    end;

                    !tablewidth,ar
                in

                let rec tb_head col =
                    if col > 1 then
                        (
                            if (tbnr=false) then            
                                "|p{"
                            else
                                "p{"
                        )^
                        (string_of_float (colwidth.(col)))^
                        "cm}"^(tb_head (col-1))
                    else if col > 0 then
                        (
                            if (tbnr=false) then            
                                "|p{"
                            else
                                "p{"
                        )^
                        (string_of_float (colwidth.(col)))^
                        (
                             if (tbnr=false) then
                                "cm}|"^(tb_head (col-1))
                             else
                                "cm}"^(tb_head (col-1))
                        )
                    else
                        ""
                in     
                nl (); out_str "\\vskip 2\\baselineskip"; nl ();
                out_str "\\begin{center}"; ind_incr (); nl ();
                out_str "\\begin{tabular}"; 

                if (tbnr=false) then
                    out_str ( 
                            "{"^
                            (tb_head env.table_col)^
                            "}\\hline"
                    )
                else
                    out_str ( 
                            "{"^
                            (tb_head env.table_col)^
                            "}"
                    );
                    
                ind_incr (); nl ();
                
                (*
                ** Build up the table
                *)

                if (th <> []) then
                begin
                    (*
                    ** The table header
                    *)
                    out_str ( 
                            "\\multicolumn{"^
                            (string_of_int (env.table_col))^
                            (
                                if (tbnr=false) then
                                    "}{|c|}{"
                                else
                                    "}{c}{"
                            )^
                            (if env.doc_color=true then
                                "\\textcolor{red}"
                             else
                                "")^
                            "{\\sc "
                    );
                    List.iter cont_trans (List.hd th).s_childs;
                    if (tbnr=false) then
                        out_str "}}\\\\\\hline\\hline"
                    else
                        out_str "}}\\\\"; 
                    nl ();
                end;

                List.iter (fun r ->
                        let ci = ref 1 in
                        let rl = List.length r in
                        List.iter (fun c ->
                            List.iter cont_trans c.s_childs;
                            if (!ci < rl) then
                                out_str "&";
                            incr ci;
                        ) r;          
                        if (tbnr=false) then
                            out_str "\\\\\\hline"
                        else
                            out_str "\\\\";
                            
                        nl ();
                    ) tr;

                    out_str "\\end{tabular}"; 
                    ind_decr (); nl (); 
                    out_str "\\end{center}"; ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
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
                    if (env.toc.sec_childs <> []) then
                    begin

                        (*
                        ** Print the table of content for the whole
                        ** document.
                        *)

                        print_toc env.toc;

                    end;
                    ind_decr ();

                    nl (); tail ();

                    close_out oc_chan
                end;


            | S_Title -> 
                begin
                    out_str "\\vskip \\baselineskip"; nl ();
                    out_str "\\hrule\\vskip2pt\\hrule}"; nl ();
                    out_str "\\end{center}"; nl ();
                    out_str "\\vfill \\eject"; nl ();
                end;
             

            | S_TOC -> ();

            | S_Program -> 
                begin

                    close_sec env;
                    env.cur_program <- None;
                end;


            | S_S1 -> 
                begin
                    close_sec env;
                    env.cur_s1 <- None;
                end;

            | S_Package -> 
                begin
                    close_sec env;
                    env.cur_package <- None;
                end;

            | S_Module -> 
                begin
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
                    close_sec env;
                    env.cur_s2 <- None ;
                end;


            | S_S3 -> 
                begin

                    close_sec env;
                    env.cur_s3 <- None ;
                end;


            | S_S4 -> 
                begin
                    out_str "}\\end{list}}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    close_sec env;
                    env.cur_s4 <- None;
                end;

            | S_Desc -> 
                begin
                    out_str "}\\end{list}}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                end;
                
            | S_Intro -> ();

            | S_Clib -> 
                begin
                        close_sec env;
                        env.cur_clib <- None;
                end;

            | S_Function -> 
                begin
                        close_sec env;
                        env.cur_function <- None;
                end;

            | S_Type -> 
                begin
                        close_sec env;
                        env.cur_type <- None;
                end;

            | S_Value -> 
                begin
                        close_sec env;
                        env.cur_val <- None;
                end;

            | S_Class -> 
                begin
                        close_sec env;
                        env.cur_class <- None;
                end;
            

            | S_C -> 
                begin
                        close_sec env;
                        env.cur_cint <- None;
                end;

            | S_Mutable -> ();
            | S_Private -> ();
            | S_Virtual -> ();

            | S_Fun_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    if (env.in_class <> Spec_None ||
                        env.in_module = true) then
                    begin
                        out_str (
                            "&\\vbox{"
                        ); 
                        ind_incr (); 
                        nl (); out_str "\\vskip .1\\baselineskip"; 
                    end
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;

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
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str (
                                "\\halign{"^
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt #} & "^
                                "# \\cr"
                            ); nl ();
                        end
                        else
                        begin
                            out_str (
                                "\\halign{"^
                                "\\hskip 20pt # & "^
                                "# \\cr"
                            ); nl ();
                        end;
                        
                        out_str "&\\cr"; nl ();
                        out_str "& ";
                        cont_trans (List.hd comm);
                        out_str "\\cr}"; nl ();
                    end;

                    
                    nl (); out_str "\\halign{"; ind_incr ();
        
                    if (env.in_class = Spec_None &&
                        env.in_module = false) then
                    begin
                        out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end
                    else
                    begin
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end;
                    
                    if (env.in_class = Spec_None &&
                        env.in_module = false &&
                        env.in_interface = true) then
                    begin
                        out_str "&&&& \\cr"; nl ();
                    end;
                    
                    if (rargs <> []) then
                    begin
                                        
                        let n = List.length rargs in
                        let i = ref 1 in
                        

                        List.iter (fun r ->
                                    nl ();

                                    if (!i = 1) then
                                    begin
                                        out_str "$[$ & ";
                                    end 
                                    else 
                                    begin
                                        out_str "& ";
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
                                        out_str "$*$ & & ";
                                    end
                                    else
                                    begin
                                        out_str " & $]=$ & {\\bf ";  
                                        List.iter cont_trans 
                                                (List.hd fname).s_childs;
                                        out_str "}";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "&";
                                            cont_trans com;
                                          end    
                                        | None -> out_str " & ";  
                                    );
                                    
                                    out_str "\\cr"; nl ();
                                    incr i;
                                  ) rargs;

                    end;
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str "& & & \\hskip 10pt ";
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
                                        | true -> out_str "$\\rightarrow$";
                                        | false -> out_str "$*$";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "&";
                                            cont_trans com;
                                          end    
                                        | None -> out_str " & ";  
                                    );


                                    out_str "\\cr"; nl ();
                                
                                    incr i;
                                  ) args;
                    end; 
                    out_str "}"; ind_decr (); 

                    if (env.in_class <> Spec_None  ||
                        env.in_module = true) then
                    begin
                        nl (); out_str "\\vskip .1\\baselineskip"; 
                        out_str "} &&& \\cr \\noalign{\\vglue -1pt}"; 
                        out_str "\\noalign{\\allowbreak}";
                        ind_decr ();nl ();
                    end
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip";                     
                    end;
                end;
                

            | S_Val_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    if (env.in_class = Spec_None &&
                        env.in_module = true) then
                    begin
                        out_str "& \\vbox{";
                        nl (); out_str "\\vskip .1\\baselineskip"
                    end
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"
                    end;

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

                    if (comm <> []) then
                    begin
                        nl (); 
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str (
                                "\\halign{"^
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt #} & "^
                                "# \\cr"
                            ); nl ();
                        end
                        else
                        begin
                            out_str (
                                "\\halign{"^
                                "\\hskip 20pt # & "^
                                "# \\cr"
                            ); nl ();
                        end;
                        
                        out_str "&\\cr"; nl ();
                        out_str "& ";
                        cont_trans (List.hd comm);
                        out_str "\\cr}"; nl ();
                    end;

                        
                    nl (); out_str "\\halign{"; ind_incr ();

                    if (env.in_class = Spec_None &&
                        env.in_module = false) then
                    begin
                        out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill } # & "^
                                "# \\hfil  & \\qquad # \\cr"
                        ); nl ();
                    end
                    else
                    begin
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil  & \\qquad # \\cr"
                        ); nl ();
                    end;

                                
                    if (env.in_class = Spec_None &&
                        env.in_module = false &&
                        env.in_interface = true) then
                    begin
                        out_str "&&\\cr"; nl ();
                    end;


                    out_str "\\textcolor{red}{val} ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "\\textcolor{red}{mutable }"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "{\\bf ";
                                    cont_trans ds;
                                    out_str "}";
                                end;
                              ) (List.hd vname).s_childs;


                    out_str ": "; nl ();
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str "& ";

                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    List.iter 
                                    (fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t false;
                                          | S_Comment -> com := Some na;
                                          | _ ->   cont_trans na;
                                    ) a.s_childs;


                                    if (!i < n) then
                                    begin
                                        match curried with
                                        | true -> out_str " $\\rightarrow$ ";
                                        | false -> out_str " $*$ ";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "&";
                                            cont_trans com;
                                          end    
                                        | None -> out_str " & ";  
                                    );

                                    incr i;
                                    out_str " \\cr"; nl ();
                        ) args;
                    end; 
                    out_str "}"; ind_decr (); 

                    if (env.in_class = Spec_None &&
                        env.in_module = true) then
                    begin
                        nl (); out_str "\\vskip .1\\baselineskip";
                        out_str "} &&& \\cr \\noalign{\\vglue -1pt}"; 
                        out_str "\\noalign{\\allowbreak}";
                        ind_decr ();nl ();
                    end
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip";
                    end;


                end;

            | S_Module_Interface ->
                begin
                    env.in_module <- false;

                    out_str " & end &&& \\cr";

                    if (env.in_interface = true) then
                    begin
                        out_str "&&&& \\cr"; nl ();                    
                    end;
                    
                    out_str "}";ind_decr (); nl ();

                    if (env.in_interface = false) then
                    begin
                        out_str "\\vskip 2\\baselineskip"; nl ();
                    end;            
                    
                end;

            | S_Class_Interface ->
                begin

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

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;

                    if (comm <> []) then
                    begin
                        nl (); 
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str (
                                "\\halign{"^
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt #} & "^
                                "# \\cr"
                            ); nl ();
                        end
                        else
                        begin
                            out_str (
                                "\\halign{"^
                                "\\hskip 20pt # & "^
                                "# \\cr"
                            ); nl ();
                        end;
                        
                        out_str "&\\cr"; nl ();
                        out_str "& ";
                        cont_trans (List.hd comm);
                        out_str "\\cr}"; nl ();
                    end;

                    
                    nl (); out_str "\\halign{"; ind_incr ();
                    out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# \\hfil \\cr"
                    ); nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "&&\\cr"; nl ();
                    end;

                    out_str "\\textcolor{red}{class} & {\\bf ";

                    List.iter cont_trans 
                               (List.hd cname).s_childs;
                    out_str "}: & \\cr"; nl ();

                        

                    if (cargs <> []) then
                    begin
                        let n = List.length cargs in
                        let i = ref 1 in
                        List.iter (fun a ->

                                    out_str " & ";
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
                                        out_str "$\\rightarrow$";
                                    end;
                                
                                    out_str " & \\cr";
                                    nl ();
                                    incr i;
                                  ) cargs;
                    end; 

                    if (oargs <> []) then
                    begin

                        out_str (
                            "& \\textcolor{red}{object} & \\cr"
                        ); nl ();
                        
                        
                        List.iter (fun i ->
                                    out_str "&\\vbox{";
                                    cont_trans i;
                                    out_str "}&\\cr"; nl ();
                                    out_str "&&\\cr";
                                    out_str "\\noalign{\\vglue -1pt}"; nl();
                                  ) oargs;
                        
                        out_str " & \\textcolor{red}{end} & \\cr"; nl ();                        
                        
                    end
                    else
                        syntax_error "class without object"; 
                    
                    out_str "}";

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    
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
                    if  (env.in_class = Spec_None &&
                         env.in_module = true) then
                    begin
                        out_str "&\\vbox{"; ind_incr (); 
                        nl (); out_str "\\vskip .1\\baselineskip";
                    end 
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip";
                    end;

                    let mi = 
                        match env.in_special with
                        | Spec_Method_Int mi -> mi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = mi.meth_name in
                    let args = mi.meth_args in

                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (vname = []) then
                        syntax_error "Method interface without name";


                    nl (); out_str "\\halign{"; ind_incr ();
                    if (env.in_class = Spec_None &&
                        env.in_module = false) then
                    begin
                        out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill \\cr"
                        ); nl ();
                    end else
                    begin
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill \\cr"
                        ); nl ();
                    end;
                                    
                    if (env.in_class = Spec_None &&
                        env.in_module = false &&
                        env.in_interface = true) then
                    begin
                        out_str "&\\cr"; nl ();
                    end;
                    
                    out_str "\\textcolor{red}{method} ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Private -> 
                                begin
                                    out_str (
                                        "\\textcolor{red}{private }"
                                    );
                                end;
                               | S_Virtual -> 
                                begin
                                    out_str (
                                        "\\textcolor{red}{virtual }"
                                    );
                                end;
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "\\textcolor{red}{mutable }"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "{\\bf ";
                                    cont_trans ds;
                                    out_str "}";
                                end;
                              ) (List.hd vname).s_childs;


                    out_str ": "; nl ();
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str "& & &";

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
                                        out_str " $\\rightarrow$ ";
                                    end;
                                    incr i;
                                   out_str "\\cr"; nl ();
                        ) args;
                    end; 
                    out_str "}"; ind_decr (); 


                    if (env.in_class = Spec_None &&
                        env.in_module = true) then
                    begin
                        nl (); out_str "\\vskip .1\\baselineskip"; 
                        out_str "} &&& \\cr \\noalign{\\vglue -1pt}"; 
                        out_str "\\noalign{\\allowbreak}";
                        ind_decr ();nl ();
                    end 
                    else if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;

                end;

            | S_Type_Interface  
            | S_Struc_Interface 
            | S_Exc_Interface ->
                begin
                    if (env.in_module = true) then
                    begin
                        out_str "&\\vbox{"; ind_incr (); 
                    end;

                    let tp = 
                        match env.in_special with
                        | Spec_Type_Int tp -> tp;
                        | _ -> syntax_error "Inavlid in_special";
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

                    if (env.in_interface = false &&
                        env.in_module = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end
                    else if (env.in_module = true) then
                    begin
                        nl (); out_str "\\vskip .1\\baselineskip"; 
                    end;


                    if (comm <> []) then
                    begin
                        nl (); 
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str (
                                "\\halign{"^
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt #} & "^
                                "# \\cr"
                            ); nl ();
                        end
                        else
                        begin
                            out_str (
                                "\\halign{"^
                                "\\hskip 20pt # & "^
                                "# \\cr"
                            ); nl ();
                        end;
                        
                        out_str "&\\cr"; nl ();
                        out_str "& ";
                        cont_trans (List.hd comm);
                        out_str "\\cr}"; nl ();
                    end;

                    
                    nl (); out_str "\\halign{"; ind_incr ();
                    if (env.in_module = false) then
                    begin
                        out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# \\hfil & "^ 
                                "#\\hfill & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end
                    else
                    begin
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "# \\hfill & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end;
                    
                    if (env.in_interface = true) then
                    begin
                        out_str "&&&&\\cr"; nl ();
                    end;
                    

                    if (tp_type <> Type_Exception) then
                        out_str "\\textcolor{red}{type} & {\\bf "
                    else
                        out_str "\\textcolor{red}{exception} & {\\bf ";

                    List.iter cont_trans 
                               (List.hd tname).s_childs;

                    if (tp_type <> Type_Exception) then
                        out_str (
                            "} $=$ "^
                                (
                                    match tp_type with
                                    | Type_List -> "";
                                    | Type_Structure -> " $\\{$ ";
                                    | _ -> 
                                        syntax_error "Invalid Type";
                                )^
                             " & "
                        )
                    else
                       out_str "} & ";
                         
                    nl ();
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str "&& ";

                                        (
                                        match tp_type with
                                        | Type_List      -> out_str " $|$ &";
                                        | Type_Structure -> out_str "&" ;
                                        | _ ->
                                                syntax_error "Invalid Type";
                                        );
                                    end
                                    else
                                    begin
                                            out_str "&";
                                    end;
                                            
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    
                                    List.iter
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t -> tex_text t;
                                          | S_Comment -> com := Some na;  
                                          | S_Mutable -> 
                                            begin
                                                out_str (
                                                    "\\textcolor{red}"^
                                                    "{mutable }"
                                                );
                                            end;
                                          | _ -> cont_trans na;
                                    ) a.s_childs; 
                        

                                    if (!i < n) then
                                    begin
                                        match tp_type with
                                        | Type_List      -> ();
                                        | Type_Structure -> out_str "$;$";
                                        | _ ->
                                            syntax_error "Invalid Type";
                                    end
                                    else
                                    begin
                                        match tp_type with
                                        | Type_List      -> ();
                                        | Type_Structure -> out_str " $\\}$ ";
                                        | _  ->
                                            syntax_error "Invalid Type";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "&";
                                            cont_trans com;
                                          end    
                                        | None -> out_str " & ";  
                                    );
                                    out_str "\\cr"; nl ();
                                    incr i;
                                  ) args;
                    end
                    else if (tp_type <> Type_Exception) then
                        syntax_error "type without args"
                    else
                    begin
                        out_str "\\cr"; 
                        nl ();
                    end;
                    
                    out_str "}"; ind_decr (); 

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    
                    if (env.in_module = true) then
                    begin
                        nl (); out_str "\\vskip .1\\baselineskip"; 
                        out_str "} &&& \\cr \\noalign{\\vglue -1pt}"; 
                        out_str "\\noalign{\\allowbreak}";
                        ind_decr ();nl ();
                    end;


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

            | S_C_Fun_Interface -> 
                begin
                    

                    let fi = 
                        match env.in_special with
                        | Spec_Fun_Int fi -> fi;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let rargs = fi.fun_retargs in
                    let fname = fi.fun_name in
                    let args = fi.fun_args in
                    let curried = fi.fun_curried in

                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (fname = []) then
                        syntax_error "C-Function interface without name";
                    
                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    
                    nl (); out_str "\\halign{"; ind_incr ();
                    out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill & \\qquad # \\hfill \\cr"
                    ); nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "&&&&\\cr"; nl ();
                    end;

                    let rargs_n = List.length rargs in
                    if (rargs_n =1) then     (* only 1 argument *)
                    begin
                        let r = List.hd rargs in

                        (
                            nl ();

                            out_str "&";

                            List.iter
                                    cont_trans
                                    r.s_childs;
                        
                            out_str " & {\\bf ";  
                            List.iter cont_trans 
                                    (List.hd fname).s_childs;
                            out_str "}$($&";

                        );
                    end
                    else
                        syntax_error "Invalid C-Function RetArg";
                         
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i>1) then
                                        out_str "& & &";
                                    
                                    let com = ref None in

                                    List.iter
                                    ( fun na ->
                                        match na.s_content with
                                        | S_Comment -> com := Some na;
                                        | _ -> cont_trans na
                                    )a.s_childs;

                                    if (!i < n) then
                                        out_str ", "
                                    else
                                        out_str "$);$";

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "&";
                                            cont_trans com;
                                          end    
                                        | None -> out_str " & ";  
                                    );
                                        
                                        
                                    out_str "\\cr"; nl ();
                                
                                    incr i;
                                  ) args;
                    end; 
                    out_str "}"; ind_decr (); 

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;

                end;

            | S_C_Hdr_Interface -> 
                begin
                    

                    let hr = 
                        match env.in_special with
                        | Spec_Hdr_Int hr -> hr;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let hname = hr.hdr_name in

                    env.in_special <- Spec_None; 

                    if (hname = []) then
                        syntax_error "C-Hdr interface without name";
                    
                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    
                    nl (); out_str "\\halign{"; ind_incr ();
                    out_str (
                                "\\textcolor{red}{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# & "^ 
                                "#\\hfill \\cr"
                    ); nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "&&\\cr"; nl ();
                    end;

                    out_str "\\textcolor{red}{${\\rm\\#}$include} ";
                    out_str "&$<$ {\\bf ";  

                    List.iter cont_trans 
                              (List.hd hname).s_childs;
                    out_str "}$>$& \\cr";

                    out_str "}"; ind_decr (); 

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    

                end;


            | S_Interface -> 
                begin
                    out_str (
                        "\\halign{"^
                        "\\textcolor{red}{\\strut\\vrule width 5pt"^
                        "#}&#\\cr&\\cr}"
                    ); nl ();
                    out_str "}";ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
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

    try
        cont_trans ds
    with
        Failure s -> syntax_error s
                     
