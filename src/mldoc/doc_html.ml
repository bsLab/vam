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
** HTML 4 transitional backend.
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


type list_type = O_List | U_List | P_List 
type env = {
    mutable doc_single: bool;
    mutable doc_name: string;

    mutable ignore_head: bool;
    mutable ignore_childs: bool;

    (*
    ** Environments needing special treatment.
    *)

    mutable in_special: struc_special;
    mutable special_depth: int;

    mutable in_preform: bool;
    mutable in_interface: bool;

    mutable in_list: list_type list;
    
    mutable in_class: struc_special;
    mutable in_object: struc_special;
    mutable in_module: bool;
            
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
}

(*
** Device options
*)

type html_options = 
    | HTML_single_doc of string
    

(*
** HTML body
*)

let html_start = "
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html;charset=iso-8859-1\">
  <title>Help Document</title>
  <meta name=\"GENERATOR\" content=\"HelpSys\">
</head>
<body style=\"text-align: justify; background-color: #FFFFFF\">
"
and html_end = "
</body>
"

let cur_sb = ref empty_block 

let syntax_error s =
    failwith (
            "html_of_tree: "^
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
** Transform the structure tree to HTML 
**
** Arguments: 
**
**  ds: structure tree
**  options list
**  sections list 
**
*)


let html_of_tree ~ds ~options ~sections =

    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = "main"; sec_type = ""} 
    in  

    let env = {
        doc_single      = false;
        doc_name        = "";
        ignore_head     = false;
        ignore_childs   = false;
        in_special      = Spec_None;
        special_depth   = 0;

        in_preform      = false;
        in_interface    = false;

        in_list         = [];

        in_class        = Spec_None;
        in_object       = Spec_None;
        in_module       = false;
            
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
    } in

    (*
    ** options
    *)

    List.iter (fun o ->
                match o with
                | HTML_single_doc s -> env.doc_single <- true;
                                       env.doc_name <- s;
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



    (*
    ** current output channel
    *)

    let oc_chan = ref None in
    let oc_name = ref "" in
    let oc_list = ref [] in

    let oc_cur () =
        match !oc_chan with
        | Some oc -> oc;
        | None -> failwith "empty out channel";
    in 



     

    (*
    ** Printing
    *)

    let spaces num =
        let fmt = "%"^(string_of_int env.doc_indent)^"s" in
        Printf.sprintf (Obj.magic fmt) ""
    in
    let out_str str =
        let slen = String.length str in
        let oc = oc_cur () in
        if (env.doc_col + slen > env.doc_colwidth &&
            env.in_preform = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        output_string oc str;
        env.doc_col <- env.doc_col + slen;
    in
    let nl () =
        let oc = oc_cur () in
        if (env.doc_col > env.doc_indent && env.in_preform = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
        else if (env.in_preform = true) then
        begin 
            output_string oc ("\n");
            env.doc_col <- 1;
        end;
    in
    let ind_incr () =
        env.doc_indent <- env.doc_indent + 2;
        output_string (oc_cur ()) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in
    let ind_decr () =
        env.doc_indent <- env.doc_indent - 2;
        output_string (oc_cur ()) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in


    (*
    ** Replace special characters in text strings
    *)

    let html_text text =
        let str_of_c = String.create 1 in
        let trans_char c =
            match c with
            | 'a'..'z'  (* Speedup ? *)
            | 'A'..'Z' 
            | '0'..'9' -> str_of_c.[0] <- c; str_of_c;
            | '&'   -> "&amp;";
            | '<'   -> "&lt";
            | '>'   -> "&gt";
            | '"'  -> "&quot;";
            | '@'   -> "&copy;";
            | 'ä'   -> "&auml";
            | 'ö'   -> "&ouml";
            | 'ü'   -> "&uuml";
            | 'Ä'   -> "&Auml";
            | 'Ö'   -> "&Ouml";
            | 'Ü'   -> "&Uuml";
            | 'ß'   -> "&szlig";
            | '°'   -> "&deg"; 
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
            | T_Bold    -> out_str "<B> ";
            | T_Italic  -> out_str "<I> ";
            | T_BoldItalic -> out_str "<B><I> ";
            | T_Type -> out_str "<TT> ";
            | T_Symbol -> ();
            | T_AsIs -> out_str "<TT> "; 
            | T_Superscript -> out_str "<SUP>";
            | T_Subscript -> out_str "<SUB>";
          );
        | [] -> ();
    in
    let rec parse_attr_end attr =
        match attr with
        | a::tl ->
          (
            match a with
            | T_Bold    -> out_str "</B> ";
            | T_Italic  -> out_str "</I> ";
            | T_BoldItalic -> out_str "</I></B> ";
            | T_Type -> out_str "</TT> ";
            | T_Symbol -> ();
            | T_AsIs -> out_str "</TT> "; 
            | T_Superscript -> out_str "</SUP>";
            | T_Subscript -> out_str "</SUB>";
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
            html_text  [!a_str]
        else
        begin
            if (!a_name.[1] <> '~' &&
                !a_name.[1] <> '?') then
            begin

                (*
                ** It's a symbolic name, not a label. Mark it.
                *)
                out_str "<I>";
                html_text  [!a_name];
                out_str "</I>";
                html_text  [":";!a_type];
            end
            else 
            begin
                if (!a_name.[1] = '~') then
                    html_text  [
                        (
                            if (tilde=false) then
                                (String.sub (!a_name) 2 
                                        ((String.length !a_name)-2))
                            else
                                !a_name;
                        );
                        ":";
                        !a_type
                    ]
                else
                    html_text  [
                        (
                            !a_name;
                        );
                        ":";
                        !a_type
                    ]
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
        !str 
    in    

    (*
    ** Prepare a link. The 'ln' argument must be a list of
    ** generic text elements.
    *)

    let link ln =

        let str = str_of_text ln in
        let lname = if (env.doc_single=false) then
                        (file_name str)^".html" 
                    else
                        (file_name str)
        in

        (*
        ** Generate a unique and valid file name. 
        *)

        if (env.doc_single=false) then
            out_str (
                "<a href=\"" ^ lname ^
                "\">"
            )
        else
            out_str (
                "<a href=\""^ 
                "#"^
                lname ^
                "\">"
            );
        
        out_str ("&nbsp; "^ str ^" &nbsp;");
    in

    (*
    ** Open a new sub HTML file
    *)

    let oc_open fname =
        if (env.doc_single = false) then
        begin
            oc_name := fname ;
            oc_chan := Some (open_out !oc_name);
            oc_list := [!oc_chan,!oc_name] @ !oc_list;
            out_str html_start;
            env.doc_indent <- 2; 
        end
        else
        begin
            (*
            ** Single document
            *)

            if (!oc_chan = None) then
            begin
                oc_name := env.doc_name ;
                oc_chan := Some (open_out !oc_name);
                oc_list := [!oc_chan,!oc_name] @ !oc_list;
                out_str html_start;
                env.doc_indent <- 2; 
            end
            else
            begin
                oc_list := [None,fname] @ !oc_list;
            end;
        end
    in


    (*
    ** Close a sub HTML file.
    *)
    
    let oc_close () =
        if (env.doc_single = false ||
            (List.length !oc_list = 1)) then
        begin
            out_str html_end;
            close_out (oc_cur ())
        end;
    in
    
    (*
    ** Remove the current oc from the oc_list and
    ** set the new current oc if any.
    *)
    
    let oc_next () =
        oc_list := List.tl !oc_list;
        match !oc_list with
        | hd::tl -> let oc,on = hd in
                    oc_chan := if (env.doc_single = false) then 
                                oc
                               else
                                !oc_chan;
                    oc_name := on;
        | [] ->     oc_chan := None;
                    oc_name := "";
    in


    (*
    ** Print a table of content starting at a specific section.
    *)
    let print_toc sec =
        let rec iter s =
            if (s.sec_name <> "") then
            begin
                out_str (
                            "<LI><A HREF=\""^
                            (file_name s.sec_name)^".html"^
                            "\">"^(
                                    if (s.sec_type <> "") then
                                        (s.sec_type^": ")
                                    else
                                        ""
                                  )^ 
                            s.sec_name^"</A></LI>"
                ); nl ();
            end;

            if (s.sec_childs <> []) then
            begin
                nl (); out_str "<UL>"; ind_incr (); 
                List.iter iter s.sec_childs;
                ind_decr (); nl (); out_str "</UL>";
            end;
        in
        if (sec.sec_childs <> []) then
        begin
            nl (); out_str "<UL>"; ind_incr ();
            List.iter iter sec.sec_childs;
            ind_decr (); nl (); out_str "</UL>";
        end;
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
        if (env.doc_single = true) then
        begin
            nl ();out_str (
                "<A NAME=\""^
                (file_name sec.sec_name)^
                "\"><P></P></A>"
            ); nl();
        end;
    in
                

    (*
    ** A section was closed. Change to the parent again.
    *)

    let close_sec env =
        let next = env.cur_sec.sec_childs in

        env.cur_sec <- env.cur_sec.sec_parent;
        let s = env.cur_sec in
        if (s.sec_name <> "" &&
            env.doc_single = false) then
        begin
            nl (); out_str (
                            "<br><br><TABLE BORDER=\"0\" "^
                            "WIDTH=\"100%\""^
                            "STYLE=\"background-color: #FFFF00\">"
            ); ind_incr ();
            nl (); out_str "<TBODY>"; ind_incr ();
            nl (); out_str "<TR>"; ind_incr ();
        
            out_str (
                        "<TD ALIGN=\"left\"><A HREF=\""^
                        (file_name s.sec_name)^".html"^
                        "\"><B>UP</B> "^(
                                if (s.sec_type <> "") then
                                    (s.sec_type^": ")
                                else
                                    ""
                            )^"<I>"^s.sec_name^
                        "</I></A></TD>"
            );
                        
            if (next <> []) then
            begin
                let s = List.hd next in
                out_str (
                        "<TD ALIGN=\"right\"><A HREF=\""^
                        (file_name s.sec_name)^".html"^
                        "\"><B>DOWN</B> "^(
                                if (s.sec_type <> "") then
                                    (s.sec_type^": ")
                                else
                                    ""
                            )^"<I>"^s.sec_name^
                        "</I></A></TD>"
                );
            end
            else
                out_str "<TD></TD>";                
 
    
            ind_decr (); nl (); out_str "</TR>";
            ind_decr (); nl (); out_str "</TBODY>";
            ind_decr (); nl (); out_str "</TABLE>";
        end;
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
                        oc_open "main.html";
                end;
            | S_Empty -> ();
            | S_Text t -> html_text  t;

            | S_NL -> out_str "<br>"; nl ();
            | S_TAB -> output_string (oc_cur ()) "    ";

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
                    | Spec_None -> out_str "&nbsp;&nbsp;<I>&nbsp;";
                end;

            | S_Paragraph -> 
                begin
                    nl (); out_str "<br><DL>"; ind_incr ();
                    nl (); out_str "<DT><B>";
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
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD><br>";  
                end;

            | S_Attribute -> parse_attr_start ds.s_attr;

            | S_Example -> 
                begin
                    out_str "<br><br>"; nl ();
                    out_str "<TABLE BORDER=\"1\" ALIGN=\"center\" >";
                    ind_incr ();
                    nl (); out_str "<TBODY>";ind_incr ();
                    nl (); out_str "<TR>";ind_incr ();
                    nl (); out_str (
                                "<TD ALIGN=\"center\" "^
                                "STYLE=\"background-color:#C1FFC1\">"
                    ); 
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
                    out_str "</TD>"; 
                    ind_decr (); nl (); out_str "</TR>"; 
                    nl (); out_str "<TR>"; ind_incr ();  
                    nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    out_str "<PRE>"; 
                    env.in_preform <- true;
                end;

            | S_Preform -> 
                begin
                    out_str "<br>"; nl ();
                    out_str "<br>"; nl ();
                    out_str "<TABLE BORDER=\"0\" ALIGN=\"center\" >";
                    ind_incr ();
                    nl (); out_str "<TBODY>";ind_incr ();
                    nl (); out_str "<TR>";ind_incr ();
                    nl (); out_str (
                                    "<TD "^
                                    "STYLE=\"background-color: #E0E0E0\""^">"
                                    ); 
                    ind_incr ();
                    out_str "<PRE>"; 
                    env.in_preform <- true;
                end;

            | S_Link -> 
                begin
                      nl ();out_str "&nbsp;<span style=\"border: solid 1px\">";
                      link ds.s_childs;
                      env.ignore_childs <- true;
                end;

            | S_OList -> 
                begin
                                nl (); out_str "<OL>";
                                ind_incr (); 
                                env.in_list <- [O_List] @ env.in_list; 
                end;            

            | S_UList -> 
                begin
                                nl (); out_str "<UL>";
                                ind_incr (); 
                                env.in_list <- [U_List] @ env.in_list; 
                end;

            | S_OpList ->
                begin
                    nl (); out_str "<DL>"; ind_incr ();
                    nl (); out_str "<DT><B>";
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
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD><br>";  
                    env.in_list <- [P_List] @ env.in_list; 
                end;
                
            | S_List_Item -> 
                begin
                        match (List.hd env.in_list) with
                        | P_List ->
                        begin
                            nl (); out_str "<DL>"; ind_incr ();
                            nl (); out_str "<DT><B>";
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
                            out_str "</B></DT>"; ind_incr ();
                            nl (); out_str "<DD>";  
                        end;
                        | O_List | U_List ->
                            nl (); out_str "<LI>";
                            ind_incr (); 
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
                                    "<H1 STYLE=\""^
                                    "background-color: #C1FFFF;"^
                                    "border: solid 1px\""^
                                    "align=\"center\">"
                        ); nl (); nl ();
                end;

            | S_TOC -> ();

            | S_Program -> 
                begin
                    let head () = 
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                       "BORDER=\"0\" "^
                                       "ALIGN=\"center\" "^
                                       "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR STYLE=\"background-color: #FFFF00\">"; 
                        ind_incr ();
                    in

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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="Program";
                                        } in

                                        oc_open fname;
                                        
                                
                                        new_sec env sec;
                                        
                                        head ();
                                        out_str (
                                                "<TD ALIGN=\"center\"><B>"^
                                                "Package: "
                                        ); 
                                        cont_trans dna;
                                        out_str "</B></TD>"; ind_decr ();

                                        env.ignore_head <- true;
                                        env.cur_program <- Some sec;
                                    end;
                                | _ -> syntax_error "Package without name"; 
                            end; 
                        | [] -> syntax_error "Package without name";
                    
                    );
                    nl (); out_str "</TR>"; ind_decr ();
                    nl (); out_str "</TBODY>"; ind_decr ();
                    nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;

            | S_Package -> 
                begin
                    let head () = 
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                       "BORDER=\"0\" "^
                                       "ALIGN=\"center\" "^
                                       "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR STYLE=\"background-color: #FFFF00\">"; 
                        ind_incr ();
                    in

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
                                        (*
                                        ** Open a new file
                                        *)

                                        let pname = (str_of_text dna.s_childs)
                                        in
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="Package";
                                        } in

                                        oc_open fname;
                                        
                                
                                        new_sec env sec;
                                        
                                        head ();
                                        out_str (
                                                "<TD ALIGN=\"center\"><B>"^
                                                "Package: "
                                        ); 
                                        cont_trans dna;
                                        out_str "</B></TD>"; ind_decr ();

                                        env.ignore_head <- true;
                                        env.cur_package <- Some sec;
                                    end;
                                | _ -> syntax_error "Package without name"; 
                            end; 
                        | [] -> syntax_error "Package without name";
                    
                    );
                    nl (); out_str "</TR>"; ind_decr ();
                    nl (); out_str "</TBODY>"; ind_decr ();
                    nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;

            | S_Intro -> ();

            | S_S1 -> 
                begin
                    let head () = 
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                       "BORDER=\"0\" "^
                                       "ALIGN=\"center\" "^
                                       "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR STYLE=\"background-color: #FFFF00\">"; 
                        ind_incr ();
                    in

                    (*
                    ** The first child must be the section name.
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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        oc_open fname;
                                
                                        new_sec env sec;
                                        
                                        head ();
                                        out_str (
                                                "<TD ALIGN=\"center\"><B>"
                                        ); 
                                        cont_trans dna;
                                        out_str "</B></TD>"; ind_decr ();

                                        env.ignore_head <- true;
                                        env.cur_s1 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end; 
                        | [] -> syntax_error "Section without name";
                    
                    );
                    nl (); out_str "</TR>"; ind_decr ();
                    nl (); out_str "</TBODY>"; ind_decr ();
                    nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;

            | S_S2 -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in

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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;
                                        
                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #FFA000\">"^
                                            "<B>"
                                        );

                                        cont_trans dna;
                                        out_str "</B></TD>"; 

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
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFFF00\"><B>"
                                        );
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end
                        else if (env.cur_package <> None) then
                        begin
                            match env.cur_package with
                              | Some sec -> 
                                        out_str (
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFFF00\"><B>"
                                        );
                                        out_str (
                                                "Package: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            match env.cur_program with
                              | Some sec -> 
                                        out_str (
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFFF00\"><B>"
                                        );
                                        out_str (
                                                "Program: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end
    
                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();

                end;

            | S_S3 -> 
                begin
                    let head () = 
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; 
                        ind_incr ();
                        nl (); 
                    in


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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        new_sec env sec;


                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

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
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFA000\"><B>"
                                    );
                                    out_str (
                                        "<I>"^
                                        sec.sec_name^
                                        "</I>"
                                    );
                                    out_str "</B></TD>"; 
                              | None -> ();
                        end
                        else if (env.cur_module <> []) then
                        begin
                            out_str (
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFA000\"><B>"
                                    );
                            out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                                    );
                            out_str "</B></TD>"; 
                        end
                        else if (env.cur_clib <> None) then
                        begin
                            match env.cur_clib with
                              | Some sec -> 
                                    out_str (
                                        "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                        "STYLE=\"background-color: #FFA000\"><B>"
                                    );
                                    out_str (
                                        "C-Library: <I>"^
                                        sec.sec_name^
                                        "</I>"
                                    );
                                    out_str "</B></TD>"; 
                              | None -> ();
                        end

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;

            | S_S4 -> 
                begin
                    nl (); out_str "<DL>"; ind_incr ();
                    nl (); out_str "<DT><B>";
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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                        } in

                                        List.iter cont_trans dna.s_childs;
                                        env.ignore_head <- true;    
                                        env.cur_s4 <- Some sec;
                                    end;
                                | _ -> syntax_error "subunit without name"; 
                            end; 
                        | [] -> syntax_error "subunit without name";     
                    
                    );
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD><br>";  
                end;

            | S_Desc -> 
                begin
                    nl (); out_str "<DL>"; ind_incr ();
                    nl (); out_str "<DT><B>Description";
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD><br>";  
                end;


            | S_Clib -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in

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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="C-Library";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;
                                        
                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #FFA000\">"^
                                            "<B>"
                                        );

                                        out_str "C-Library: ";
                                        cont_trans dna;
                                        out_str "</B></TD>"; 

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
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_package with
                            | Some sec -> 
                                    out_str (
                                                "Package: <I>"^
                                                sec.sec_name^
                                                "</I></B></TD>"
                                            );
                            | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_program with
                            | Some sec -> 
                                    out_str (
                                                "Program: <I>"^
                                                sec.sec_name^
                                                "</I></B></TD>"
                                            );
                            | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_s1 with
                            | Some sec -> 
                                    out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I></B></TD>"
                                            );
                            | None -> ();
                        end;
                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;


            | S_Module -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in

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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Module";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;
                                        
                                        head ();
                                        if (env.cur_module <> []) then
                                          out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #FFC1C1\">"^
                                            "<B>"
                                          )
                                        else
                                          out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #FFA000\">"^
                                            "<B>"
                                          );

                                        out_str "ML-Module: ";
                                        cont_trans dna;
                                        out_str "</B></TD>"; 

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
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                           out_str (
                                        "ML-Module: <I>"^
                                        (List.nth env.cur_module 1).sec_name^
                                        "</I>"
                                        );
                           out_str "</B></TD>"; 
                            
                        end
                        else if (env.cur_package <> None) then
                        begin
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_package with
                              | Some sec -> 
                                    out_str (
                                                "Package: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                    );
                                    out_str "</B></TD>"; 
                                            
                              | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_s1 with
                              | Some sec -> 
                                     out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                     );
                                    out_str "</B></TD>"; 
                              | None -> ();
                        end
                        else if (env.cur_program <> None) then
                        begin
                            out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFFF00\"><B>"
                            );
                            match env.cur_program with  (* ??? *)
                              | Some sec -> 
                                    out_str (
                                                "Program: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                    );
                                    out_str "</B></TD>"; 
                                            
                              | None -> ();
                        end

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;


            | S_Function -> 
                begin
                    let head () = 
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; 
                        ind_incr ();
                        nl (); 
                    in


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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Function";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        out_str "ML-Function:";
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

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
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                           out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                                        );
                           out_str "</B></TD>"; 
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                                "C-Library: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end;

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();

                end;


            | S_Type -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in

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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Type";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        out_str "ML-Type:";
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

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
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                            );
                            out_str "</B></TD>"; 
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                                "C-Library: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end;

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>";
                    nl (); out_str "<br>"; nl ();

                end;

            | S_Value -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in


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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Value";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        out_str "ML-Value:";
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

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
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                            );
                            out_str "</B></TD>"; 
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then (* ??? *)
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                                "C-Library: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end;

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;

            | S_Class -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in


                    (*
                    ** The first child must be the class name.
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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Value";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        out_str "Class: ";
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

                                        env.cur_class <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "C without name"; 
                            end; 
                        | [] -> syntax_error "C without name";
                    
                    );

                    nl ();
                    (
                        if (env.cur_module <> []) then  
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                            );
                            out_str "</B></TD>"; 
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then    (* ??? *)
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                                "C-Library: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end;

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
                end;
            

            | S_C -> 
                begin
                    let head () =
                        if (env.doc_single=true) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in


                    (*
                    ** The first child must be the c name.
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
                                        let fname = (file_name pname)^".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="ML-Value";
                                        } in

                                        new_sec env sec;

                                        oc_open fname;

                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        out_str "C: ";
                                        cont_trans dna;    
                                        out_str "</B></TD>"; 

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
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            out_str (
                                        "ML-Module: <I>"^
                                        (List.hd env.cur_module).sec_name^
                                        "</I>"
                            );
                            out_str "</B></TD>"; 
                        end
                        else if (env.cur_s2 <> None) then
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_s2 with
                              | Some sec -> 
                                        out_str (
                                                "<I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 

                              | None -> ();
                        end
                        else if (env.cur_clib <> None) then 
                        begin
                           out_str (
                                "<TD WIDTH=\"50%\" ALIGN=\"right\" "^
                                "STYLE=\"background-color: #FFA000\"><B>"
                            );
                            match env.cur_clib with
                              | Some sec -> 
                                       out_str (
                                                "C-Library: <I>"^
                                                sec.sec_name^
                                                "</I>"
                                        );
                                        out_str "</B></TD>"; 
                              | None -> ();
                        end;

                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    nl (); out_str "<br>"; nl ();
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
                        "<SPAN STYLE=\"color:#FF0000\">"^
                        "mutable&nbsp;</SPAN>"
                    );
                end;
            | S_Virtual -> 
                begin
                    out_str ( 
                        "<SPAN STYLE=\"color:#FF0000\">"^
                        "virtual&nbsp;</SPAN>"
                    );
                end;
            | S_Private -> 
                begin
                    out_str ( 
                        "<SPAN STYLE=\"color:#FF0000\">"^
                        "private&nbsp;</SPAN>"
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
                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;

                    out_str "<DL><DT>"; ind_incr (); nl ();
                    out_str "<SPAN style=\"color: #FF0000\">module</SPAN> ";


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
                                        out_str "<B>";
                                        List.iter cont_trans
                                            dna.s_childs;
                                        out_str "</B>";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Module withou name"; 
                            end; 
                            | [] -> syntax_error "Module withou name";
                    );

                    out_str ": </DT>"; nl ();
                    out_str "<DD>"; nl ();

                    out_str "<DL><DT>"; ind_incr (); nl ();
                    out_str "<SPAN style=\"color: #FF0000\">sig</SPAN> ";

                    out_str "</DT>"; nl ();
                    out_str "<DD>"; nl ();

                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr (); nl ();

    
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
                    env.in_interface <- true;
                    out_str "<br><br>"; nl ();
                    out_str "<TABLE BORDER=\"1\" ALIGN=\"center\" >";
                    ind_incr ();
                    nl (); out_str "<TBODY>";ind_incr ();
                    nl (); out_str "<TR>";ind_incr ();
                    nl (); out_str ("<TD ALIGN=\"center\" "^
                                    "STYLE=\"background-color: #C1FFFF\">"
                    ); 
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
                    out_str "</TD>"; 
                    ind_decr (); nl (); out_str "</TR>"; 
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
                            out_str "<I> ";
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
                            out_str "</I>";
                        end;            
                end;
            
            | S_Paragraph -> 
                begin
                    out_str "</DD>"; ind_decr (); ind_decr (); nl ();
                    out_str "</DL>"; nl ();
                end;

            | S_Attribute -> parse_attr_end ds.s_attr;

            | S_Example -> 
                begin
                            env.in_preform <- false;
                            out_str "</PRE>"; nl ();
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
                            ind_decr (); nl (); out_str "</TBODY>"; 
                            ind_decr (); nl (); out_str "</TABLE>"; nl ();
                            out_str "<br>"; nl ();
                end;

            | S_Preform -> 
                begin
                            env.in_preform <- false;
                            out_str "</PRE>"; nl ();
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
                            ind_decr (); nl (); out_str "</TBODY>"; 
                            ind_decr (); nl (); out_str "</TABLE>"; nl ();
                            out_str "<br>"; nl ();
                end;

            | S_OList ->    
                begin
                            ind_decr (); 
                            out_str "</OL><br>"; nl ();
                            env.in_list <- (List.tl env.in_list);
                end;

            | S_UList ->    
                begin
                            ind_decr ();  
                            out_str "</UL><br>"; nl ();
                            env.in_list <- (List.tl env.in_list);
                end;

            | S_OpList ->    
                begin
                    ind_incr (); nl (); out_str "</DD><br>"; nl ();
                    env.in_list <- (List.tl env.in_list);
                end;

            | S_List_Item ->
                begin
                    match (List.hd env.in_list) with
                    | P_List ->
                        begin
                            ind_incr (); nl (); out_str "</DD>"; nl ();
                        end;
                    | O_List | U_List ->
                        begin
                            ind_decr (); 
                            out_str "</LI>"; nl ();
                        end;
                end;

            | S_Name -> 
                begin
                    let d () = env.special_depth <- env.special_depth - 1 in
                    match env.in_special with
                    | Spec_Fun_Int  fi  -> d ();
                    | Spec_Hdr_Int  hr  -> ();
                    | Spec_Val_Int  vi  -> d (); 
                    | Spec_Type_Int ti  -> d ();
                    | Spec_Class_Int ti  -> d ();
                    | Spec_Object_Int ti  -> d ();
                    | Spec_Method_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_None -> 
                        begin
                            out_str "</I> ";
                        end;            
                end;

            | S_Link -> 
                    out_str "</a></span>&nbsp;";

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

                nl ();
                if (tbnr = false) then
                    out_str (
                            "<br><br><TABLE BORDER=\"1\" "^
                            "ALIGN=\"center\" >"
                    )
                else
                    out_str (
                            "<br><br><TABLE BORDER=\"0\" "^
                            "ALIGN=\"center\" >"
                    );
                ind_incr ();
                nl (); out_str "<TBODY>"; ind_incr ();
    
                (*
                ** Build up the table
                *)

                if (th <> []) then
                begin
                    (*
                    ** The table header
                    *)
                    nl (); out_str "<TR>";
                    nl (); out_str (
                            "<TD STYLE=\"background-color: #C1FFFF\" "^
                            "ALIGN=\"center\""^
                            "COLSPAN="^(string_of_int tcn)^">"
                    );
                    List.iter cont_trans (List.hd th).s_childs;
                    out_str "</TD>"; 
                    ind_decr (); nl (); out_str "</TR>";
                end;
                List.iter (fun r ->
                        nl (); out_str "<TR>"; ind_incr ();

                        List.iter (fun c ->
                            nl (); out_str "<TD>";
                            List.iter cont_trans c.s_childs;
                            out_str "</TD>"; 
                        ) r;          

                        ind_decr (); nl (); out_str "</TR>";
                    ) tr;

                ind_decr (); nl (); out_str "</TBODY>"; 
                ind_decr (); nl (); out_str "</TABLE><br>"; nl ();
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
                        env.doc_single = false ) then
                    begin
                        out_str "<br><br>"; nl ();
                        nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "BORDER=\"1\">"
                        ); ind_incr ();
                        nl ();out_str "<TBODY>"; ind_incr (); nl ();
                        out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Document Content</TD></TR>"
                        ); 
                        nl ();

                        (*
                        ** Print the table of content for the whole
                        ** document.
                        *)

                        out_str "<TR>"; ind_incr (); nl ();
                        out_str "<TD>"; 

                        print_toc env.toc;

                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                        ind_decr (); nl ();
                        out_str "</TBODY>"; 
                        ind_decr (); nl ();
                        out_str "</TABLE>"; nl ();

                    end;
                    ind_decr ();

                    oc_close ();
                    oc_next ();
                end;


            | S_Title -> 
                begin
                    out_str "</H1><br><br>"; nl ();
                end;

            | S_TOC -> ();

            | S_Program -> 
                begin
                    if (env.cur_program <> None) then
                    begin
                        let toc = 
                            match env.cur_program with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false) then
                        begin 
                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                    "<TABLE ALIGN=\"center\" "^
                                    "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 
                            print_toc toc;

                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
    
                            ind_decr (); nl ();
                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_program <- None;
                end;


            | S_S1 -> 
                begin
                    if (env.cur_s1 <> None) then
                    begin
                        let toc = 
                            match env.cur_s1 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false ) then
                        begin 
                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                    "<TABLE ALIGN=\"center\" "^
                                    "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 
                            print_toc toc;

                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
    
                            ind_decr (); nl ();
                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_s1 <- None;
                end;

            | S_Package -> 
                begin
                    if (env.cur_package <> None) then
                    begin
                        let toc = 
                            match env.cur_package with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false ) then
                        begin 
                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                    "<TABLE ALIGN=\"center\" "^
                                    "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 
                            print_toc toc;

                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
    
                            ind_decr (); nl ();
                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_package <- None;
                end;

            | S_Module -> 
                begin
                    if (env.cur_module <> []) then
                    begin
                        let toc = 
                            List.hd env.cur_module 
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_module <- 
                      (
                        match env.cur_module with
                        | hd::tl -> tl;
                        | [] -> [];
                      );
                end;

            | S_S2 -> 
                begin
                    if (env.cur_s2 <> None) then
                    begin
                        let toc = 
                            match env.cur_s2 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_s2 <- None ;
                end;


            | S_S3 -> 
                begin
                    if (env.cur_s3 <> None) then
                    begin
                        let toc = 
                            match env.cur_s3 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_s3 <- None ;
                end;


            | S_S4 -> 
                begin
                    ind_incr (); nl (); out_str "</DD><br>";  nl ();
                    env.cur_s4 <- None;
                end;

            | S_Desc -> 
                begin
                    ind_incr (); nl (); out_str "</DD><br>";  nl ();
                end;
                
            | S_Intro -> ();

            | S_Clib -> 
                begin
                    if (env.cur_clib <> None) then
                    begin
                        let toc = 
                            match env.cur_clib with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            env.doc_single = false) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\""^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC 
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env;
                    oc_close ();
                    oc_next ();
                    env.cur_clib <- None ;
                end;
            

            | S_Function -> 
                begin
                        close_sec env;
                        oc_close ();
                        oc_next ();
                        env.cur_function <- None;
                end;

            | S_Type -> 
                begin
                        close_sec env;
                        oc_close ();
                        oc_next ();
                        env.cur_type <- None;
                end;

            | S_Value -> 
                begin
                        close_sec env;
                        oc_close ();
                        oc_next ();
                        env.cur_val <- None;
                end;

            | S_Class -> 
                begin
                        close_sec env;
                        oc_close ();
                        oc_next ();
                        env.cur_class <- None;
                end;

            | S_C -> 
                begin
                        close_sec env;
                        oc_close ();
                        oc_next ();
                        env.cur_cint <- None;
                end;
    
            | S_Mutable -> ();
            | S_Private -> ();
            | S_Virtual -> ();

            | S_Fun_Interface ->
                if (env.in_object = Spec_None) then 
                begin
                    
                    if (env.in_interface = true ||
                        env.in_class <> Spec_None ||
                        env.in_module = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
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
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CELLSPACING=\"-5\" >"
                    ); ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (fname = []) then
                        syntax_error "Function interface without name";
                    
                    if (rargs <> []) then
                    begin
                        
                        let n = List.length rargs in
                        let i = ref 1 in

                        List.iter (fun r ->
                                    out_str "<TR>";ind_incr ();
                                    if (!i = 1) then
                                    begin
                                        out_str "<TD>[&nbsp;</TD>"; nl();
                                    end 
                                    else
                                    begin
                                        out_str "<TD></TD>"; nl();
                                    end;
                                    out_str "<TD>";
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
                                          | _ -> List.iter
                                                   cont_trans
                                                   r.s_childs;
                        

                                    ) r.s_childs;

                                    if (!i < n) then
                                    begin
                                        out_str " * ";
                                        out_str "</TD>";
                                        out_str "<TD></TD>"; nl();
                                        out_str "<TD></TD>"; nl();
                                        out_str "<TD></TD>"; nl();
                                    end
                                    else
                                    begin
                                        out_str "</TD>";
                                        out_str "<TD>&nbsp;]</TD>"; nl();
                                        out_str "<TD>&nbsp;=&nbsp;</TD>"; nl(); 
                                        out_str "<TD><B>"; 
                                        List.iter cont_trans 
                                                (List.hd fname).s_childs;
                                        out_str "</B></TD>"; nl();
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    );
    
                                    ind_decr ();out_str "</TR>";nl ();
                                    incr i;
                                  ) rargs;

                    end;
                    
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str "<TR>";ind_incr ();
                                    out_str "<TD></TD>";nl ();
                                    out_str "<TD></TD>";nl ();
                                    out_str "<TD></TD>";nl ();
                                    out_str "<TD></TD>";nl ();
                                    out_str "<TD>";

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
                                          | _ -> List.iter
                                                   cont_trans
                                                   a.s_childs;
                        
                                    ) a.s_childs;


                                    if (!i < n) then
                                    begin
                                        match curried with
                                        | true -> out_str " -&gt;";
                                        | false -> out_str " * ";
                                    end;
                                    out_str "</TD>"; 
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    );


                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end; 
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();

                    if (env.in_interface = true ||
                        env.in_class <> Spec_None ||
                        env.in_module = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                end;
                

            | S_Val_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    if (env.in_interface = true ||
                        env.in_module = true ||
                        env.in_class <> Spec_None) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
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
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;


                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (vname = []) then
                        syntax_error "Value interface without name";
                    

                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">val&nbsp;</TD>"; 
                    nl(); out_str "<TD>"; 

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "mutable&nbsp;</SPAN>"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "<B>";
                                    cont_trans ds;
                                    out_str "</B>";
                                end;
                              ) (List.hd vname).s_childs;

                    out_str "&nbsp;</TD>"; nl();
                    out_str "<TD>:&nbsp;</TD>"; nl ();
                    out_str "<TD>";
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD>";
                                    end;
                                    
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    List.iter
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t false;
                                          | S_Comment -> com := Some na;
                                          | _ -> cont_trans na;
                                    ) a.s_childs; 


                                    if (!i < n) then
                                    begin
                                        match curried with
                                        | true -> out_str " -&gt;";
                                        | false -> out_str " * ";
                                    end;

                                    out_str "</TD>"; 
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    );
                                    
                                    ind_decr ();out_str "</TR>"; nl ();

                                    incr i;
                                  ) args;
                    end; 
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();

                    if (env.in_interface = true ||
                        env.in_module = true ||
                        env.in_class <> Spec_None) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                end;

            | S_Module_Interface ->
                begin
                    env.in_module <- false;
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; nl (); 

                    out_str "</DD>"; ind_decr (); nl ();
                    out_str "</DL>"; nl ();
                    out_str "<SPAN STYLE=\"color:#FF0000\">end</SPAN>";
                    out_str "</DD>"; ind_decr (); nl ();
                    out_str "</DL>"; nl ();
                    
                    if (env.in_interface = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                
                end;


            | S_Type_Interface  
            | S_Struc_Interface 
            | S_Exc_Interface ->
                begin
                    if (env.in_interface = true ||
                        env.in_module = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;

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
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (tname = []) then
                        syntax_error "Type interface without name";
                    

                    out_str "<TR>"; ind_incr (); nl (); 
                    if (tp_type <> Type_Exception) then
                        out_str "<TD style=\"color: #FF0000\">type&nbsp;</TD>"
                    else
                        out_str (
                            "<TD style=\"color: #FF0000\">"^
                            "exception&nbsp;</TD>"
                        ); 
                    
                    nl(); out_str "<TD><B>"; 


                    List.iter cont_trans
                              (List.hd tname).s_childs;


                    out_str "&nbsp;</B></TD>"; nl();
                    if (tp_type <> Type_Exception) then
                        out_str (
                            "<TD> &nbsp; = &nbsp; "^
                                (
                                    match tp_type with
                                    | Type_List -> " &nbsp; ";
                                    | Type_Structure -> " { &nbsp; ";
                                    | _ -> 
                                        syntax_error "Invalid Type";
                                )^
                            "</TD>"
                        )
                    else
                        out_str "<TD></TD>";
                         
                    nl ();
                    out_str "<TD>"; 
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD>";
                                    end;
                                    
                                    (*
                                    ** If the child is generic text,
                                    ** do some more formatting.
                                    *)
                                    let com = ref None in
                                    List.iter
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t -> html_text t;
                                          | S_Comment -> com := Some na;
                                          | S_Mutable -> 
                                            begin
                                                out_str (
                                                    "<SPAN STYLE=\""^
                                                    "color:#FF0000\">"^
                                                    "mutable&nbsp;"^
                                                    "</SPAN>"
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
                                
                                    out_str "</TD>"; 
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    );
                                    
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end
                    else if (tp_type <> Type_Exception) then
                        syntax_error "type without args"
                    else
                    begin
                        out_str "</TD>"; 
                        ind_decr ();out_str "</TR>"; nl ();
                    end;
                        
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();


                    if (env.in_interface = true ||
                        env.in_module = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;

                end;

            | S_Class_Interface ->
                begin

                    env.special_depth <- env.special_depth - 1;


                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;
                    
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

                    nl ();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str "<DL><DT>"; ind_incr (); nl ();
                    
                    out_str "<SPAN style=\"color: #FF0000\">class</SPAN> ";

                    if (cname = []) then
                        syntax_error "Class without name";
                        

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Private -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "private </SPAN>"
                                    );
                                end;
                               | S_Virtual -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "virtual </SPAN>"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "<B>";
                                    cont_trans ds;
                                    out_str "</B>";
                                end;
                              ) (List.hd cname).s_childs;

                           
                    out_str ": </DT>"; nl ();
                    out_str "<DD>"; nl ();
                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr (); nl ();

                    if (cargs <> []) then
                    begin
                        let n = List.length cargs in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    out_str "<TR>";ind_incr ();
                                    out_str "<TD>";

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
                                        out_str " -&gt;";
                                    end;
                                
                                    out_str "</TD>"; 
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) cargs;
                    end; 

                    if (oargs <> []) then
                    begin
                        out_str "<TR>";ind_incr ();
                        out_str "<TD>";

                        out_str (
                            "<DL><DT><SPAN style=\"color: #FF0000\">"^
                            "object</SPAN> "
                        );
                        
                        out_str "</DT>"; nl ();
                        out_str "<DD>"; ind_incr (); nl ();

                        out_str "<TABLE ";
                        out_str "BORDER=\"0\" ";
                        out_str "ALIGN=\"left\" ";
                        out_str "CELLSPACING=\"-5\" >";ind_incr ();
                        out_str "<TBODY>"; ind_incr (); nl ();
                        
                        List.iter (fun i ->
                                    cont_trans i
                                  ) oargs;
                        

                        ind_decr (); nl (); out_str "</TBODY>"; 
                        ind_decr (); nl (); out_str "</TABLE>"; nl (); 
                        out_str "</DD>"; ind_decr (); nl ();
                        out_str "</DL>"; nl ();

                        out_str "<SPAN style=\"color: #FF0000\">end</SPAN></TD>"; 
                        ind_decr ();out_str "</TR>"; nl ();
                        
                    end
                    else
                        syntax_error "class without object"; 
                    
                    
                    
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; nl (); 
                    out_str "</DD>"; ind_decr (); nl ();
                    out_str "</DL>"; nl ();
                    
                    if (env.in_interface = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
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
                    if (env.in_interface = true ||
                        env.in_class <> Spec_None) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;

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
                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (vname = []) then
                        syntax_error "Method interface without name";
                    

                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">method&nbsp;</TD>"; nl();
                    out_str "<TD>"; 

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Private -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "private </SPAN>"
                                    );
                                end;
                               | S_Virtual -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "virtual </SPAN>"
                                    );
                                end;
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        "<SPAN STYLE=\"color:#FF0000\">"^
                                        "mutable&nbsp;</SPAN>"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "<B>";
                                    cont_trans ds;
                                    out_str "</B>";
                                end;
                              ) (List.hd vname).s_childs;


                    out_str "&nbsp;</TD>"; nl();
                    out_str "<TD>:&nbsp;</TD>"; nl ();
                    out_str "<TD>";
    

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD>";
                                    end;
                                    
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
                                        out_str " -&gt;";
                                    end;
                                    
                                    out_str "</TD>"; 
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end; 
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();

                    if (env.in_interface = true ||
                        env.in_class <> Spec_None) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                end;

            | S_C_Var_Interface -> 
                begin
                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;

                    if (env.in_interface = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;

                end;

            | S_C_Fun_Interface -> 
                begin
                    
                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
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
                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CELLSPACING=\"-5\" >"
                    ); ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (fname = []) then
                        syntax_error "C-Function interface without name";
                    
                    let rargs_n = List.length rargs in
                    if (rargs_n =1) then        (* only 1 return argument ! *)
                    begin
                        
                        let r = List.hd rargs in
                        (
                            out_str "<TR>";ind_incr ();
                            out_str "<TD>"; 

                            List.iter
                                    cont_trans
                                    r.s_childs;

                            out_str "</TD>"; nl ();
                            out_str "<TD>&nbsp;</TD>"; nl ();
                            out_str "<TD><B>"; 

                            List.iter cont_trans 
                                      (List.hd fname).s_childs;

                            out_str "</B>&nbsp;(</TD>"; nl();
                            out_str "<TD>"; 
                        );
                    end
                    else
                        syntax_error "C-interface without return arg";
                        
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD>";
                                    end;
                                    
                                    let com = ref None in
                                    List.iter
                                    (fun na ->
                                        match na.s_content with
                                        | S_Comment -> com := Some na;
                                        | _ -> cont_trans na;
                                
                                    )a.s_childs;

                                    if (!i < n) then
                                        out_str ", "
                                    else
                                        out_str "&nbsp;);";
                                        
                                    out_str "</TD>"; nl ();
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    );
                                     
                                    ind_decr ();out_str "</TR>"; nl ();

                                    incr i;
                                  ) args;
                    end; 
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                end;

            | S_C_Hdr_Interface -> 
                begin
                    
                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;

                    let hr = 
                        match env.in_special with
                        | Spec_Hdr_Int hr -> hr;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let hname = hr.hdr_name in
                    let comm = hr.hdr_comm in
                    
                    env.in_special <- Spec_None; 

                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CELLSPACING=\"-5\" >"
                    ); ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (hname = []) then
                        syntax_error "C-Hdr interface without name"
                    else
                    begin                    
                            out_str "<TR>";ind_incr ();
                            out_str ("<TD style=\"color: "^
                                     "#FF0000\">#include &nbsp;</TD>"
                            ); nl();

                            out_str "<TD>&lt; <B>"; 

                            List.iter cont_trans 
                                      (List.hd hname).s_childs;

                            out_str "</B>&gt;&nbsp;</TD>"; nl();
                            ind_decr (); out_str "</TR>"; nl ();
                    end;

                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                    end;
                end;


            | S_Interface -> 
                begin
                            env.in_interface <- false;
                            ind_decr (); nl (); out_str "</TBODY>"; 
                            ind_decr (); nl (); out_str "</TABLE>"; nl ();
                            out_str "<br>"; nl ();
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
                     
