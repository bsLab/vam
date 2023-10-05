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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) BSSLAB 2005
**    $CREATED:     23.1.2005
**    $MODIFIED:    
**    $VERSION:     1.27
**
**    $INFO:
**
** HTML 4 transitional backend.
**
**    $ENDOFINFO
**
*)





open Doc_core  
open Doc_toc
open Printf

(*
** Special structures with pre defined layout
*)

type ml_fun_int = {
    mutable ml_fun_name: structure_block list;
    mutable ml_fun_retargs: structure_block list;
    mutable ml_fun_args: structure_block list;
    mutable ml_fun_curried: bool;
    mutable ml_fun_comm: structure_block list;
}
and ml_val_int = {
    mutable ml_val_name: structure_block list;
    mutable ml_val_args: structure_block list;
    mutable ml_val_curried: bool;
    mutable ml_val_comm: structure_block list;
}
and ml_ext_int = {
    mutable ml_ext_name: structure_block list;
    mutable ml_ext_args: structure_block list;
    mutable ml_ext_curried: bool;
    mutable ml_ext_comm: structure_block list;
    mutable ml_ext_fun: structure_block list;
}
and ml_meth_int = {
    mutable ml_meth_name: structure_block list;
    mutable ml_meth_args: structure_block list;
    mutable ml_meth_comm: structure_block list;
}
and ml_obj_int = {
    mutable ml_obj_name: structure_block list;
    mutable ml_obj_args: structure_block list;
    mutable ml_obj_comm: structure_block list;
}
and type_type = Type_List | Type_Structure | Type_Exception | Type_Empty
and ml_type_int = {
    mutable ml_type_name: structure_block list;
    mutable ml_type_args: structure_block list;
    mutable ml_type_type: type_type;
    mutable ml_type_comm: structure_block list;
}
and ml_class_int = {
    mutable ml_class_name: structure_block list;
    mutable ml_class_args: structure_block list;
    mutable ml_class_obj: struc_special;
    mutable ml_class_comm: structure_block list;
}
and ml_mod_int = {
    mutable ml_mod_name: structure_block list;
    mutable ml_mod_args: structure_block list;
    mutable ml_mod_obj: struc_special;
    mutable ml_mod_comm: structure_block list;
}
and c_hdr_int = {
    mutable c_hdr_name: structure_block list;
    mutable c_hdr_comm: structure_block list;
}
and c_fun_int = {
    mutable c_fun_name: structure_block list;
    mutable c_fun_retargs: structure_block list;
    mutable c_fun_args: structure_block list;
    mutable c_fun_comm: structure_block list;
}
and c_var_int = {
    mutable c_var_name: structure_block list;
    mutable c_var_args: structure_block list;
    mutable c_var_comm: structure_block list;
}
and c_type_int = {
    mutable c_type_name: structure_block list;
    mutable c_type_args: structure_block list;
    mutable c_type_type: type_type;
    mutable c_type_comm: structure_block list;
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
and dataformat_int = {
    mutable dataformat_name: structure_block list;
    mutable dataformat_headlist: structure_block list;
    mutable dataformat_rowlist: (structure_block list) list;
    mutable dataformat_currow: structure_block list ref;
    mutable dataformat_comm: structure_block list;
}
and struc_special = 
    | Spec_ML_Fun_Int of ml_fun_int 
    | Spec_ML_Val_Int of ml_val_int
    | Spec_ML_Ext_Int of ml_ext_int
    | Spec_ML_Type_Int of ml_type_int
    | Spec_ML_Method_Int of ml_meth_int
    | Spec_ML_Object_Int of ml_obj_int
    | Spec_ML_Module_Int of ml_mod_int
    | Spec_ML_Class_Int of ml_class_int
    | Spec_C_Hdr_Int of c_hdr_int 
    | Spec_C_Fun_Int of c_fun_int 
    | Spec_C_Var_Int of c_var_int 
    | Spec_C_Type_Int of c_type_int 
    | Spec_Table of table_int 
    | Spec_DataFormat of dataformat_int 
    | Spec_None


(*
** Global document environment: all what the translater must currently
** know about special things. Depends on already translated 
** structure elements. 
*)


type list_type = O_List | U_List | A_List of bool | L_List 



type env = {
    mutable doc_options: doc_options list;
    mutable doc_name: string;

    mutable ignore_head: bool;
    mutable ignore_childs: bool;

    (*
    ** Environments needing special treatment.
    *)

    mutable in_special: struc_special;
    mutable special_depth: int;

    mutable special_name : bool;

    mutable in_preform: bool;
    mutable in_interface: bool;
    mutable in_title: bool;

    mutable in_list: list_type list;
    
    mutable in_class: struc_special;
    mutable in_object: struc_special;
    mutable in_module: bool;
    mutable in_par: bool;       (* paragraphe mode ? *)
    mutable in_ref: bool;
    mutable in_math: bool;

    mutable cur_s1: section option;
    mutable cur_s2: section option;
    mutable cur_s3: section option;
    mutable cur_s4: section option;
    mutable cur_mp: section option;
    mutable cur_mp_paragr : section option;

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
** Check for document options
*)

let doc_single env =
    List.mem Doc_single  env.doc_options 

let doc_sec_break env sec =
    List.mem sec env.doc_options

let doc_toc env =
    List.mem Doc_with_toc env.doc_options

(*
** User customized HTML body text
*)
let doc_head_str env =
    let found = ref "" in
    List.iter (fun o ->
        match o with
        | Doc_head str -> found:=str;
        | _ -> ();
        ) env.doc_options;
    !found


let doc_tail_str env =
    let found = ref "" in
    List.iter (fun o ->
        match o with
        | Doc_tail str -> found:=str;
        | _ -> ();
        ) env.doc_options;
    !found

let doc_image_path_dst env =
    let found = ref "" in
    List.iter (fun o ->
        match o with
        | Doc_image_path_dst str -> found:=str^"/";
        | _ -> ();
        ) env.doc_options;
    !found

let doc_image_path_src env =
    let found = ref "" in
    List.iter (fun o ->
        match o with
        | Doc_image_path_src str -> found:=str^"/";
        | _ -> ();
        ) env.doc_options;
    !found

(*
** HTML body
*)

let html_start title = 
    let keys_l = Str.split (Str.regexp " ") title in
    let keys = ref "" in
    List.iter (fun k -> keys := !keys ^ k ^ ", ") keys_l;
    keys := !keys ^ "MANdoc";
"
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html;charset=iso-8859-1\">
  <title>"^title^"</title>
  <meta name=\"GENERATOR\" content=\"HelpSys\">
  <meta name=\"KEYWORDS\" content=\""^ !keys ^"\">
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

let file_name str secnum =
    let rs = " \|\\\\\|,\|/\|;\|-\|+\|'\|#\|%\|&\|\"\|!\|(\|)\|[\|]\|{\|}"^
             "\|:\|~\|ä\|ö\|ü\|Ö\|Ä\|Ü" in
    let rp = "\1" in
    let name =
        Str.global_replace 
            (Str.regexp rs)
            rp
            str in

    if secnum > 0 
     then name^(string_of_int secnum)
     else name

    



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
    (*
    ** First get the TOC structure
    *)
    let toc = toc_of_tree ds sections in

    let revision = Unix.time () in


    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = ""; sec_type = "Main"; sec_num=0;} 
    in  

    let sec_num = ref 1 in

    let env = {
        doc_options     = [];
        doc_name        = "";
        ignore_head     = false;
        ignore_childs   = false;
        in_special      = Spec_None;
        special_depth   = 0;
        special_name    = false;

        in_preform      = false;
        in_interface    = false;
        in_title        = false;

        in_list         = [];

        in_class        = Spec_None;
        in_object       = Spec_None;
        in_module       = false;
        in_par          = false;
        in_ref          = false;
        in_math         = false;

        cur_s1          = None;
        cur_s2          = None;
        cur_s3          = None;
        cur_s4          = None;
        cur_mp          = None;
        cur_mp_paragr   = None;

        toc             = doc_head;
        cur_sec         = doc_head;
        
        doc_indent      = 1;
        doc_col         = 1;
        doc_colwidth    = 100;

        table_col       = 0;
    } in

    (*
    ** options
    *)
    env.doc_options <- env.doc_options @ options;
    List.iter (fun o ->
                match o with
                | Doc_Main s -> env.doc_name <- s;
                                doc_head.sec_name <- Filename.chop_extension s;
                | _ -> ();
              ) options;  
    if (env.doc_name = "") then env.doc_name <- "main.html";

    (*
    ** External sections
    *)
    if (sections <> []) then
    List.iter (fun s ->
        let sec = 
            match s with
            | Sec_s1 s -> 
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S1";sec_num= !sec_num} in
                         env.cur_s1 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s2 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S2";sec_num= !sec_num} in
                         env.cur_s2 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s3 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S3";sec_num= !sec_num} in
                         env.cur_s3 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s4 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S4";sec_num= !sec_num} in
                         env.cur_s4 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_mp m -> let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= m;sec_type="MP";sec_num= !sec_num} in
                         env.cur_mp <- Some sec;
                         incr sec_num;
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

#ifdef NOTDELAYED

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
#if 0
        env.doc_indent <- env.doc_indent + 2;
#endif
        output_string (oc_cur ()) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in
    let ind_decr () =
#if 0
        env.doc_indent <- env.doc_indent - 2;
#endif
        output_string (oc_cur ()) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in

#else (* !NOTDELAYED *)

    let cur_line = ref "" in

    let out_str str =
        let slen = String.length str in
        if (env.doc_col + slen > env.doc_colwidth &&
            env.in_preform = false) then
        begin
            cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        cur_line := !cur_line ^ str;
        env.doc_col <- env.doc_col + slen;
    in

   let flush_str () =
        let oc = oc_cur () in

        (*
        ** Remove some unnecessary spaces between dot and colons.
        *)
        let subst_list = [
                    " </B>," , "</B>,";
                    " </B>\." , "</B>.";
                    " </B>:" , "</B>:"; 
                    " </B>)" , "</B>)";
                    " </I>:" , "</I>:"; 
                    " </I>\." , "</I>.";
                    " </I>," , "</I>,";
                    " </I>)" , "</I>)";
                    " </TT>:" , "</TT>:"; 
                    " </TT>\." , "</TT>.";
                    " </TT>," , "</TT>,";
                    " </TT>)" , "</TT>)";
                ] in
        let filter = (String.contains !cur_line ',') ||
                     (String.contains !cur_line '.') ||
                     (String.contains !cur_line ':') ||
                     (String.contains !cur_line ')') in
        if filter then
        List.iter (fun (exp,rep) ->
                 cur_line := Str.global_replace
                                (Str.regexp exp) rep !cur_line;
            ) subst_list;

        output_string oc !cur_line;
        cur_line := "";
        in

    let nl () =
        let oc = oc_cur () in
        if (env.doc_col > env.doc_indent && env.in_preform = false) then
        begin
            cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
        else if (env.in_preform = true) then
        begin 
            cur_line := !cur_line ^ ("\n");
            env.doc_col <- 1;
        end;
        flush_str ();
    in


    let ind_incr () =
#if 0
        env.doc_indent <- env.doc_indent + 2;
#endif
        cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in
    let ind_decr () =
#if 0
        env.doc_indent <- env.doc_indent - 2;
#endif
        cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
    in

#endif  (* !NOTDELAYED *)


    let print_copyright () =
                    out_str "<P><BR></P>"; nl ();
                    out_str "<HR><P>"; nl();
                    out_str ("<I>Generated by MANDoc (C) 2005 BSSLAB Dr. Stefan Bosse<BR>");
                    nl();
                    out_str (Printf.sprintf " Revision %12.0f <BR>"
                             revision); nl();
                    out_str "</I></P>"; nl();
        in

    (*
    ** Replace special characters in text string list
    *)
    let trans_char c =
        let str_of_c = String.create 1 in
            match c with
            | 'a'..'z'  (* Speedup ? *)
            | 'A'..'Z' 
            | '0'..'9' -> str_of_c.[0] <- c; str_of_c;
            | '&'   -> "&amp;";
            | '<'   -> "&lt;";
            | '>'   -> "&gt;";
            | '"'  -> "&quot;";
            | '@'   -> "&copy;";
            | 'ä'   -> "&auml;";
            | 'ö'   -> "&ouml;";
            | 'ü'   -> "&uuml;";
            | 'Ä'   -> "&Auml;";
            | 'Ö'   -> "&Ouml;";
            | 'Ü'   -> "&Uuml;";
            | 'ß'   -> "&szlig;";
            | '°'   -> "&deg;"; 
            | '\138' -> "<br>";
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


    let html_text text =
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
            | T_Bold    -> out_str "<B>";
            | T_Italic  -> out_str "<I>";
            | T_BoldItalic -> out_str "<B><I>";
            | T_Type -> out_str "<TT>";
            | T_AsIs -> out_str "<TT>"; 
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
            | T_Bold    -> out_str "</B>";
            | T_Italic  -> out_str "</I>";
            | T_BoldItalic -> out_str "</I></B>";
            | T_Type -> out_str "</TT>";
            | T_AsIs -> out_str "</TT>"; 
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
    ** Return glued text of structure list
    *)
    let str_of_childs ds =
        let str = ref "" in
        List.iter (fun d -> str := !str ^ (str_of_struc d)) ds;
        !str
        in   

    (*
    ** Prepare a link. The 'ln' argument must be a list of
    ** generic text elements.
    *)

    let link ln =

        let str = str_of_text ln in
        let lname = if not (doc_single env) then
                        (file_name str 0)^".html" 
                    else
                        (file_name str 0)
        in

        (*
        ** Generate a unique and valid file name. 
        *)

        if not (doc_single env) then
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
    ** symbol translate
    *)

    let symbol sli =
        let str = ref "" in
        List.iter ( fun d -> str := !str ^ (str_of_struc d)
                ) sli;
        let str' = Str.global_replace (Str.regexp " ") "" !str in
        let str'' = 
            match str' with
            | "rightarrow" -> trans_line " -> ";
            | "Rightarrow" ->  trans_line " => ";
            | "leftarrow" ->  trans_line " <- ";
            | "Leftarrow" ->  trans_line " <= ";
            | "bullet" -> " &#149; ";
            | "mu" -> "u";
            | "oplus" -> " + ";
            | "pm" -> " +- ";
            | "equiv" -> " == ";
            | _ -> trans_line ("<" ^ !str ^ ">");
            in
        out_str str'';
        in

    (*
    ** print formatted text (with attributes, symbols,...) from a
    ** block list (child list of a structure elemenent).
    *)
    let rec format_text tli =
        List.iter (fun ds -> 
            match ds.s_content with
            | S_Text t -> let str = ref "" in
                          List.iter (fun s ->
                            str := !str ^ " " ^ s;
                            ) t;
                          out_str (trans_line !str);
            | S_Attribute -> parse_attr_start ds.s_attr;
                             format_text ds.s_childs;   
                             parse_attr_end ds.s_attr;  
            | S_Symbol -> symbol ds.s_childs;
            
            | _ -> syntax_error
                    "format_text: not a S_Text element";
        ) tli;
        in

    (*
    ** Open a new sub HTML file. Behaviour depends on option settings. 
    *)

    let oc_open fname secname sec =
        if (not (doc_single env) &&
           (doc_sec_break env sec)) then
        begin
            oc_name := fname ;
            oc_chan := Some (open_out !oc_name);
            oc_list := [!oc_chan,!oc_name] @ !oc_list;

            let str = doc_head_str env in
            if str = ""  
                then out_str (html_start secname)
                else out_str str;
 
            env.doc_indent <- 0; 
        end
        else if not (doc_single env) &&
                not (doc_sec_break env sec) then
        begin
            oc_list := [!oc_chan,!oc_name] @ !oc_list; 
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
                let str = doc_head_str env in
                if str = ""  
                    then out_str (html_start secname)
                    else out_str str;
                env.doc_indent <- 0; 
            end
            else
            begin
                oc_list := [!oc_chan,fname] @ !oc_list;
            end;
        end
    in


    (*
    ** Close a sub HTML file. Behaviour depends on option settings.
    *)
    
    let oc_close sec =
        if  (not (doc_single env) &&
            (doc_sec_break env sec)) ||
            (List.length !oc_list = 1) then
        begin
            let str = doc_tail_str env in
            if str = ""  
                then out_str (html_end)
                else out_str str;
            flush_str ();
            close_out (oc_cur ());
        end;
    in
    
    (*
    ** Remove the current oc from the oc_list and
    ** set the new current oc if any.
    *)
    
    let oc_next () =
        oc_list := List.tl !oc_list;
        match !oc_list with
        | hd::tl ->
                    let oc,on = hd in
                    oc_chan := oc;
                    oc_name := on;
        | [] ->     oc_chan := None;
                    oc_name := "";
    in


    (*
    ** Print a table of (sub) content starting at a specified section.
    ** But only print (sub) sections if they open a new file, specified
    ** with option settings.
    *)
    let print_sub_toc sec =
        let rec iter s =
            let stype s = match s.sec_type with
                | "S1" -> Doc_multi_s1;
                | "S2" -> Doc_multi_s2;
                | "S3" -> Doc_multi_s3;
                | "S4" -> Doc_multi_s4;
                | "MP" -> Doc_multi_mp;
                | "Main" -> Doc_Main env.doc_name;
                | _ -> syntax_error "print_sub_toc: invalid type string";
                in
   
            if (s.sec_name <> "" && (doc_sec_break env (stype s))) then
            begin
                out_str (
                            "<LI><A HREF=\""^
                            (file_name s.sec_name s.sec_num)^".html"^
                            "\">"^
                            s.sec_name^"</A></LI>"
                ); nl ();
            end
            else
            begin
                (*
                ** This section doen't force a file break.
                ** Find the parent file instead and build a reference tag
                ** inside this file.
                *)
                let rec search l =
                    if l.sec_type = "Main" then
                        doc_head
                    else
                    match l.sec_parent with
                    | p -> if p.sec_name <> "" && 
                              (doc_sec_break env (stype p))
                                then p
                                else search p;
                    in
                let p = search s in
                out_str (
                            "<LI><A HREF=\""^
                            (file_name p.sec_name p.sec_num)^".html#"^
                            (file_name s.sec_name s.sec_num)^
                            "\">"^
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
    ** Normal text must be put in a paragraph environment.
    ** Set the P tag and antitags if necessary. Not so simple as it
    ** sounds!
    *)
    let tag_set = ref false in
    let leave_p () =
        if env.in_par && !tag_set then 
        begin
            out_str "</P>\n";
            tag_set := false;
        end;
        env.in_par <- false;
        in
    let enter_p () =
        if env.in_par && not !tag_set then 
        begin
            out_str "\n<P>\n";
            tag_set := true;
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

        let up = sec.sec_parent in
        let stype = match sec.sec_type with
                | "S1" -> Doc_multi_s1;
                | "S2" -> Doc_multi_s2;
                | "S3" -> Doc_multi_s3;
                | "S4" -> Doc_multi_s4;
                | "MP" -> Doc_multi_mp;
                | "Main" -> Doc_Main env.doc_name;
                | _ -> syntax_error "new_sec: invalid type string";
                in

        if (doc_single env) then
        begin
            nl ();out_str (
                "<A NAME=\""^
                (file_name sec.sec_name sec.sec_num)^
                "\"><P></P></A>"
            ); nl();
        end 
        else if  ((doc_sec_break env stype) &&
                   up.sec_name <> "") then
        begin
            nl (); out_str (
                            "<br><br><TABLE BORDER=\"0\" "^
                            "WIDTH=\"100%\" "^
                            "CLASS=\"section\" "^
                            "STYLE=\"background-color: #C0C0C0\">"
            ); ind_incr ();
            nl (); out_str "<TBODY>"; ind_incr ();
            nl (); out_str "<TR>"; ind_incr ();
        
            let s = up in
            out_str (
                        "<TD ALIGN=\"left\"><A HREF=\""^
                        (file_name s.sec_name s.sec_num)^".html"^
                        "\"><B>UP</B> "^
                        "<I>"^s.sec_name^
                        "</I></A></TD>"
                );
            (*
            ** The main file should be the TOC index file.
            *)
            if (env.doc_name <> "") then
            begin
                out_str (
                        "<TD ALIGN=\"right\"><A HREF=\""^
                        env.doc_name^
                        "\"><B>INDEX</B> "^
                        "</A></TD>"
                    );

            end;
            ind_decr (); nl (); out_str "</TR>";
            ind_decr (); nl (); out_str "</TBODY>";
            ind_decr (); nl (); out_str "</TABLE>";
        end;

        in
                

    (*
    ** A section was closed. Change to the parent again.
    *)

    let close_sec env sec =

        let cur = env.cur_sec in
        let next = toc_get_next toc cur in
        let prev = toc_get_prev toc cur in

        env.cur_sec <- env.cur_sec.sec_parent;

        let s = env.cur_sec in

        if (not (doc_single env) &&
            (doc_sec_break env sec) &&
            (prev <> None ||
             next <> None)) then
        begin
            nl (); out_str (
                            "<br><br><TABLE BORDER=\"0\" "^
                            "WIDTH=\"100%\" "^
                            "CLASS=\"section\" "^
                            "STYLE=\"background-color: #C0C0C0\">"
            ); ind_incr ();
            nl (); out_str "<TBODY>"; ind_incr ();
            nl (); out_str "<TR>"; ind_incr ();
        
            if (prev <> None) then
            begin
                let s = match prev with | Some s -> s; | None -> failwith "" in
                out_str (
                        "<TD ALIGN=\"left\"><A HREF=\""^
                        (file_name s.sec_name s.sec_num)^".html"^
                        "\"><B>PREVIOUS</B> "^
                        "<I>"^s.sec_name^
                        "</I></A></TD>"
                );
            end
            else
                out_str "<TD></TD>";                 


            if (next <> None) then
            begin
                let s = match next with | Some s -> s; | None -> failwith "" in
                out_str (
                        "<TD ALIGN=\"right\"><A HREF=\""^
                        (file_name s.sec_name s.sec_num)^".html"^
                        "\"><B>NEXT</B> "^
                        "<I>"^s.sec_name^
                        "</I></A></TD>"
                    );
            end
            else
                out_str "<TD></TD>";                
 
    
            ind_decr (); nl (); out_str "</TR>";
            ind_decr (); nl (); out_str "</TBODY>";
            ind_decr (); nl (); out_str "</TABLE>";

            print_copyright ();
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
                        oc_open env.doc_name
                                "MANDoc manual document" 
                                (Doc_Main env.doc_name);
                        env.in_par <- true;
                end;
            | S_Empty -> ();
            | S_Text t -> enter_p ();
                          html_text  t;

            | S_NL -> if not env.in_preform then out_str "<br>"; nl ();
            | S_TAB -> out_str "    "; 

            | S_Comment -> 
                begin
                    match env.in_special with
                    | Spec_ML_Fun_Int fi -> 
                                    fi.ml_fun_comm <- fi.ml_fun_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Val_Int vi -> 
                                    vi.ml_val_comm <- vi.ml_val_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Ext_Int vi -> 
                                    vi.ml_ext_comm <- vi.ml_ext_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Type_Int ti -> 
                                    ti.ml_type_comm <- ti.ml_type_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Method_Int mi -> 
                                    mi.ml_meth_comm <- mi.ml_meth_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Object_Int oi -> 
                                    oi.ml_obj_comm <- oi.ml_obj_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Class_Int ci -> 
                                    ci.ml_class_comm <- ci.ml_class_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Module_Int ci -> 
                                    ci.ml_mod_comm <- ci.ml_mod_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Hdr_Int hr -> 
                                    hr.c_hdr_comm <- hr.c_hdr_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Fun_Int hr -> 
                                    hr.c_fun_comm <- hr.c_fun_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Var_Int hr -> 
                                    hr.c_var_comm <- hr.c_var_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Type_Int hr -> 
                                    hr.c_type_comm <- hr.c_type_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Table tb ->
                                    tb.table_comm <- tb.table_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_DataFormat tb ->
                                    tb.dataformat_comm <- tb.dataformat_comm @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_None -> out_str "&nbsp;&nbsp;<I>&nbsp;";
                end;


            | S_Attribute -> enter_p ();
                             parse_attr_start ds.s_attr;

            | S_Example -> 
                begin
                    leave_p ();
  
                    out_str "<br><br>"; nl ();
                    out_str ("<TABLE BORDER=\"1\" "^
                             "WIDTH=\"80%\" "^
                             "CLASS=\"example\" "^
                             "ALIGN=\"center\" >");
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
                    env.in_preform <- true;
                end;

            | S_Preform -> 
                begin
                    leave_p ();                    
                    out_str "<br>"; nl ();
                    out_str "<br>"; nl ();
                    out_str ("<TABLE BORDER=\"0\" "^
                             "WIDTH=\"80%\" "^
                             "CLASS=\"preform\" "^
                             "ALIGN=\"center\" >");
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
                      enter_p ();
                      nl ();
                      out_str "&nbsp;<span style=\"border: solid 1px\">";
                      link ds.s_childs;
                      env.ignore_childs <- true;
                end;

            | S_Ref -> 
                begin
                      enter_p (); 
                      nl ();out_str "&nbsp;<span style=\"border: solid 1px\">";
                      env.in_ref <- true;
                 end;

            | S_Label ->
                begin
                    (* TODO *)
                    env.ignore_childs <- true;
                end;

            | S_Symbol ->
                begin
                    enter_p ();
                    symbol ds.s_childs;
                    env.ignore_childs <- true;
                end;

            | S_Filename ->
                begin
                    env.ignore_childs <- true;
                end;

            | S_OList -> 
                begin
                                leave_p (); 
                                nl (); out_str "<OL>";
                                ind_incr (); 
                                env.in_list <- [O_List] @ env.in_list; 
                end;            

            | S_UList -> 
                begin
                                leave_p ();
                                nl (); out_str "<UL>";
                                ind_incr (); 
                                env.in_list <- [U_List] @ env.in_list; 
                end;

            | S_ArgList ->
                begin
                    leave_p ();
                    (*
                    ** The first child can be a title name
                    *)

                    let str = ref "" in
                        
                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        let pname = (str_of_text dna.s_childs)
                                        in

                                        str := pname;
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> (); 
                            end; 
                        | [] -> ();     
                    
                    );
                    if (!str <> "" || env.in_list = []) then
                    begin
                        nl (); out_str "<DL>"; ind_incr ();
                        nl (); out_str ("<DT><B>"^ !str ^"</B></DT>");
                        ind_incr ();
                        nl (); out_str "<DD><br>";  
                        env.in_list <- [A_List true] @ env.in_list;             
                    end 
                    else
                        env.in_list <- [A_List false] @ env.in_list; 
                end;
                
            | S_LitList ->
                begin
                    leave_p ();
                    nl (); out_str "<DL>"; ind_incr ();
                    nl (); out_str "<DT><B>";
                    (*
                    ** The first child can be a title name
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
                                | _ -> (); 
                            end; 
                        | [] -> ();     
                    
                    );
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD><br>";  
                    env.in_list <- [L_List] @ env.in_list; 
                end;
                
            | S_List_Item -> 
                begin
                        
                        match (List.hd env.in_list) with
                        | L_List 
                        | A_List _ ->
                        begin
                            nl (); out_str "<DL>"; ind_incr ();
                            nl (); out_str "<DT><B>";
                            (*
                            ** The first child must be the option name in
                            ** the argument list environment.
                            *)

                            (
                                match ds.s_childs with
                                | dna::tl ->
                                begin
                                    match dna.s_content with
                                    | S_Name -> 
                                    begin
                                        format_text dna.s_childs;
                                        env.ignore_head <- true;    
                                    end;
                                    | _ -> syntax_error "argument list item without name";
                                end; 
                                | [] -> syntax_error "argument list item without name";
                    
                            );
                            out_str "</B></DT>"; ind_incr ();
                            nl (); out_str "<DD>";
                            env.in_par <- true;  
                        end;
                        | O_List | U_List ->
                            nl (); out_str "<LI>";
                            ind_incr (); 
                            env.in_par <- true;
                end;
                
            | S_Table -> 
                begin
                        leave_p ();
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
                    env.in_par <- true;
                    match env.in_special with
                    | Spec_Table tb ->
                                    let row = tb.table_currow in
                                    row := !row @ [ds];
                    | _ -> syntax_error "Invalid S_TableCol";            
                end;


            | S_DataFormat ->
                begin
                        leave_p ();

                        let cr = ref [] in
                        env.in_special <- Spec_DataFormat {
                                    dataformat_name = [];  
                                    dataformat_headlist = [];
                                    dataformat_rowlist = []; 
                                    dataformat_currow = cr;  
                                    dataformat_comm = [];    
                        };
                        env.special_depth <- env.special_depth + 1;
                end;

            | S_DataHead ->
                begin
                    match env.in_special with
                    | Spec_DataFormat tb ->  
                                    let cr = ref [] in
                                    tb.dataformat_currow  <- cr;
                    | _ -> syntax_error "Invalid S_DataHead";   
                end;

            | S_DataRow ->
                begin
                    match env.in_special with
                    | Spec_DataFormat tb ->  
                                    let cr = ref [] in
                                    tb.dataformat_currow  <- cr;
                    | _ -> syntax_error "Invalid S_DataRow";    
                end;

            | S_DataEntry ->
                begin
                    (* prevent down stream *)
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_DataFormat tb ->  
                                    let row = tb.dataformat_currow in
                                    row := !row @ [ds];
                    | _ -> syntax_error "Invalid S_DataEntry";
                end;




  


            | S_Title -> 
                begin
                        env.in_title <- true;
                        leave_p ();       
                        out_str (
                                    "<H1 STYLE=\""^
                                    "background-color: #C1FFFF;"^
                                    "border: solid 1px\""^
                                    "align=\"center\">"
                        ); nl (); nl ();
                end;

            | S_TOC -> ();


            | S_S1 -> 
                begin
                    let head () = 
                        if (doc_single env) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                       "BORDER=\"0\" "^
                                       "CLASS=\"section\" "^
                                       "ALIGN=\"center\" "^
                                       "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR STYLE=\"background-color: #FFFF00\">"; 
                        ind_incr ();
                    in

                    leave_p ();                    
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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S1";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        oc_open fname pname Doc_multi_s1;
                                        new_sec env sec;
                                        head ();

                                        out_str (
                                                "<TD ALIGN=\"center\"><B>"
                                        ); 
                                        env.special_name <- true;
                                        cont_trans dna;
                                        env.special_name <- false;
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
                    env.in_par <- true;

                    nl (); out_str "<br>"; nl ();
                end;

            | S_S2 -> 
                begin
                    let head () =
                        if (doc_single env) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "CLASS=\"section\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; ind_incr ();
                        nl (); 
                    in

                    leave_p (); nl();
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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S2";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        oc_open fname pname Doc_multi_s2;
                                        new_sec env sec;
                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #FFA000\">"^
                                            "<B>"
                                        );
                                        env.special_name <- true;
                                        cont_trans dna;
                                        env.special_name <- false;
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
                    );
                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    env.in_par <- true;

                    nl (); out_str "<br>"; nl ();
                end;

            | S_S3 -> 
                begin
                    let head () = 
                        if (doc_single env) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "CLASS=\"section\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; 
                        ind_incr ();
                        nl (); 
                    in

                    leave_p (); nl ();

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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S3";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        oc_open fname pname Doc_multi_s3;
                                        new_sec env sec;
                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        env.special_name <- true;
                                        cont_trans dna;    
                                        env.special_name <- false;
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
                        else if (env.cur_s1 <> None) then
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
                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    env.in_par <- true;

                    nl (); out_str "<br>"; nl ();
                end;

            | S_S4 -> 
                begin
                    
                    leave_p (); 

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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S4";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        List.iter cont_trans dna.s_childs;
                                        env.ignore_head <- true;    
                                        env.cur_s4 <- Some sec;
                                    end;
                                | _ -> syntax_error "subunit without name"; 
                            end; 
                        | [] -> syntax_error "subunit without name";     
                    
                    );
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD>";
                    env.in_par <- true;
                    out_str "<br>";  
                end;





            | S_MP -> 
                begin
                    let head () = 
                        if (doc_single env) then
                            out_str "<br>";
                        nl (); out_str ("<br><TABLE "^
                                   "BORDER=\"0\" "^
                                   "CLASS=\"section\" "^
                                   "ALIGN=\"center\" "^
                                   "WIDTH=\"100%\">"); 
                        ind_incr ();
                        nl (); out_str "<TBODY>"; ind_incr ();
                        nl (); out_str "<TR >"; 
                        ind_incr ();
                        nl (); 
                    in

                    leave_p (); nl();                   

                    (*
                    ** The first child must be the MP name.
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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="MP";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        oc_open fname pname Doc_multi_mp;
                                        new_sec env sec;
                                        head ();

                                        out_str (
                                            "<TD WIDTH=\"50%\" ALIGN=\"left\" "^
                                            "STYLE=\"background-color: #B0FF00\">"^
                                            "<B>"
                                        );
                                        env.special_name <- true;
                                        cont_trans dna;    
                                        env.special_name <- false;
                                        out_str "</B></TD>"; 

                                        env.cur_mp <- Some sec;
                                        env.ignore_head <- true;
                                    end;
                                | _ -> syntax_error "Manual Page without name"; 
                            end; 
                        | [] -> syntax_error "Manual Page without name";
                    
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
                        else if (env.cur_s1 <> None) then
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
                    );

                    ind_decr (); nl (); out_str "</TR>"; 
                    ind_decr (); nl (); out_str "</TBODY>"; 
                    ind_decr (); nl (); out_str "</TABLE>"; 
                    env.in_par <- true;
                    nl (); out_str "<br>"; nl ();
                end;

            | S_MP_paragr -> 
                begin
                    
                    leave_p();

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
                                        let fname = (file_name pname !sec_num)^
                                                    ".html" 
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="MP_paragr";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        List.iter cont_trans dna.s_childs;
                                        env.ignore_head <- true;    
                                        env.cur_mp_paragr <- Some sec;
                                    end;
                                | _ -> syntax_error "manual paragr without name"; 
                            end; 
                        | [] -> syntax_error "manual paragr without name";     
                    
                    );
                    out_str "</B></DT>"; ind_incr ();
                    nl (); out_str "<DD>";
                    env.in_par <- true;

                    out_str "<br>";  
                end;




            | S_ML_Fun_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_ML_Fun_Int {
                                    ml_fun_name = [];
                                    ml_fun_retargs = [];
                                    ml_fun_args = [];
                                    ml_fun_curried = false;                         
                                    ml_fun_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_ML_Object_Int oi -> oi.ml_obj_args <- 
                                                    oi.ml_obj_args @ [ds];
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
            

            | S_ML_Val_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_ML_Val_Int {
                                    ml_val_name = [];
                                    ml_val_args = [];
                                    ml_val_curried = false;                         
                                    ml_val_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_ML_Object_Int oi -> oi.ml_obj_args <- 
                                                    oi.ml_obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;

            | S_ML_Ext_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_ML_Ext_Int {
                                    ml_ext_name = [];
                                    ml_ext_args = [];
                                    ml_ext_curried = false;                         
                                    ml_ext_comm = [];
                                    ml_ext_fun = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_ML_Object_Int oi -> oi.ml_obj_args <- 
                                                    oi.ml_obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;


            | S_ML_Module_Interface ->
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
                                | _ -> syntax_error "Module without name"; 
                            end; 
                            | [] -> syntax_error "Module without name";
                    );

                    out_str ": </DT>"; nl ();
                    out_str "<DD>"; nl ();

                    out_str "<DL><DT>"; ind_incr (); nl ();
                    out_str "<SPAN style=\"color: #FF0000\">sig</SPAN> ";

                    out_str "</DT>"; nl ();
                    out_str "<DD>"; nl ();

                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "CLASS=\"module_interface\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr (); nl ();

    
                end;

            | S_ML_Method_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_ML_Method_Int {
                                    ml_meth_name = [];
                                    ml_meth_args = [];
                                    ml_meth_comm = []; 
                                };
                                env.special_depth <- env.special_depth + 1;
                    end
                    else
                    begin
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_ML_Object_Int oi -> oi.ml_obj_args <- 
                                                    oi.ml_obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;
                
            | S_ML_Object_Interface -> 
                begin
                    env.special_depth <- env.special_depth - 1;
                    
                                (
                                    match env.in_special with
                                    | Spec_ML_Class_Int ci -> ();
                                    | _ -> 
                                      syntax_error "ObjectInt outside class";
                                );

                                let oi = Spec_ML_Object_Int {
                                    ml_obj_name = [];
                                    ml_obj_args = [];
                                    ml_obj_comm = [];
                                } in
                                env.in_object <- oi;
                                env.in_special <- oi;                                
                                
                end;

            | S_ML_Type_Interface -> 
                                env.in_special <- Spec_ML_Type_Int {
                                    ml_type_name = [];
                                    ml_type_args = [];
                                    ml_type_type = Type_List;
                                    ml_type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

            | S_ML_Exc_Interface -> 
                begin
                                env.in_special <- Spec_ML_Type_Int {
                                    ml_type_name = [ds];
                                    ml_type_args = [];
                                    ml_type_type = Type_Exception;
                                    ml_type_comm = [];
                                };
                                env.ignore_childs <- true;
                end;

            | S_ML_Struc_Interface -> 
                                env.in_special <- Spec_ML_Type_Int {
                                    ml_type_name = [];
                                    ml_type_args = [];
                                    ml_type_type = Type_Structure;
                                    ml_type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

            | S_ML_Class_Interface -> 
                begin
                        env.in_special <- Spec_ML_Class_Int {
                                    ml_class_name = [];
                                    ml_class_args = [];
                                    ml_class_obj = Spec_None;                         
                                    ml_class_comm = [];
                                };
                        env.special_depth <- env.special_depth + 1;
                        env.in_class <- env.in_special;
                end;
                
            | S_C_Var_Interface -> 
                begin
                                env.in_special <- Spec_C_Var_Int {
                                    c_var_name = [];
                                    c_var_args = [];
                                    c_var_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                end;

            | S_C_Fun_Interface -> 
                begin
                                env.in_special <- Spec_C_Fun_Int {
                                    c_fun_name = [];
                                    c_fun_retargs = [];
                                    c_fun_args = [];
                                    c_fun_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;
                end;

            | S_C_Hdr_Interface -> 
                begin
                    if (env.in_object = Spec_None) then
                    begin
                                env.in_special <- Spec_C_Hdr_Int {
                                    c_hdr_name = [ds];
                                    c_hdr_comm = [];
                                };
                                env.ignore_childs <- true;
                    end
                    else
                    begin
                        (*
                        ** Class inherit !!! C_Hdr was abused ....
                        *)
                        env.ignore_childs <- true;
                        match env.in_object with
                        | Spec_ML_Object_Int oi -> oi.ml_obj_args <-
                                                    oi.ml_obj_args @ [ds];
                        | _ -> syntax_error "invalid class object";
                    end;
                end;
            | S_C_Type_Interface -> 
                                env.in_special <- Spec_C_Type_Int {
                                    c_type_name = [];
                                    c_type_args = [];
                                    c_type_type = Type_List;
                                    c_type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;


            | S_C_Struc_Interface -> 
                                env.in_special <- Spec_C_Type_Int {
                                    c_type_name = [];
                                    c_type_args = [];
                                    c_type_type = Type_Structure;
                                    c_type_comm = [];
                                };
                                env.special_depth <- env.special_depth + 1;

            | S_Interface -> 
                begin
                    
                    leave_p ();
                    env.in_interface <- true;
                    out_str "<br><br>"; nl ();
                    out_str ("<TABLE BORDER=\"1\" "^
                             "CLASS=\"interface\" "^
                             "ALIGN=\"center\" >");
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
                    enter_p();
                    match env.in_special with
                    | Spec_ML_Fun_Int fi -> 
                                    fi.ml_fun_name <- fi.ml_fun_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Class_Int ci -> 
                                    ci.ml_class_name <- ci.ml_class_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Val_Int vi -> 
                                    vi.ml_val_name <- vi.ml_val_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Ext_Int vi -> 
                                    if vi.ml_ext_name = [] then
                                        vi.ml_ext_name <- vi.ml_ext_name @ [ds]
                                    else if vi.ml_ext_fun = [] then
                                        vi.ml_ext_fun <- vi.ml_ext_fun @ [ds]
                                    else
                                        syntax_error "Spec_ML_Ext_Int";
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Module_Int mi -> 
                                    mi.ml_mod_name <- mi.ml_mod_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Method_Int mi -> 
                                    mi.ml_meth_name <- mi.ml_meth_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Object_Int oi -> 
                                    oi.ml_obj_name <- oi.ml_obj_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_ML_Type_Int ti -> 
                                    ti.ml_type_name <- ti.ml_type_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_Table tb ->
                                    tb.table_name <- tb.table_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_DataFormat tb ->
                                    tb.dataformat_name <- tb.dataformat_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Hdr_Int hdr -> ();
                    | Spec_C_Fun_Int fi -> 
                                    fi.c_fun_name <- fi.c_fun_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Var_Int vi -> 
                                    vi.c_var_name <- vi.c_var_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_C_Type_Int ti -> 
                                    ti.c_type_name <- ti.c_type_name @ [ds];
                                    env.special_depth <- env.special_depth + 1;
                    | Spec_None -> 
                        if not env.special_name then
                        begin
                            out_str "<I> ";
                        end;            
                end;

            | S_Value -> env.ignore_childs <- true;    (* internally handled *)

            | S_CurArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;

                    match env.in_special with
                    | Spec_ML_Fun_Int fi -> fi.ml_fun_args <- fi.ml_fun_args @ [ds];
                                            fi.ml_fun_curried <- true;

                    | Spec_ML_Val_Int vi -> vi.ml_val_args <- vi.ml_val_args @ [ds];
                                            vi.ml_val_curried <- true;
                    | Spec_ML_Ext_Int vi -> vi.ml_ext_args <- vi.ml_ext_args @ [ds];
                                            vi.ml_ext_curried <- true;

                    | Spec_ML_Method_Int mi -> mi.ml_meth_args <- 
                                               mi.ml_meth_args @ [ds];
                    | Spec_ML_Object_Int oi -> oi.ml_obj_args <- 
                                               oi.ml_obj_args @ [ds];

                    | Spec_ML_Type_Int ti -> ti.ml_type_args <- 
                                             ti.ml_type_args @ [ds];

                    | Spec_ML_Class_Int ci -> ci.ml_class_args <- 
                                              ci.ml_class_args @ [ds];

                    | Spec_C_Fun_Int fi -> fi.c_fun_args <- 
                                           fi.c_fun_args @ [ds];
                    | Spec_C_Var_Int vi -> vi.c_var_args <- 
                                           vi.c_var_args @ [ds];
                    | Spec_C_Type_Int ti -> ti.c_type_args <- 
                                            ti.c_type_args @ [ds];

                    | Spec_Table _ -> syntax_error "Invalid S_CurArg in Table";
                    | Spec_None -> syntax_error "Invalid S_CurArg in None";
                    | _ -> syntax_error "Invalid S_CurArg";            
                end;

            | S_UnCurArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_ML_Fun_Int fi -> fi.ml_fun_args <- fi.ml_fun_args @ [ds];
                                         fi.ml_fun_curried <- false;
                    | Spec_ML_Val_Int vi -> vi.ml_val_args <- vi.ml_val_args @ [ds];
                                         vi.ml_val_curried <- false;
                    | Spec_ML_Ext_Int vi -> vi.ml_ext_args <- vi.ml_ext_args @ [ds];
                                         vi.ml_ext_curried <- false;
                    | _ -> syntax_error "Invalid S_UnCurArg";            
                end;

            | S_RetArg -> 
                begin
                    env.special_depth <- env.special_depth + 1;
                    match env.in_special with
                    | Spec_ML_Fun_Int fi -> fi.ml_fun_retargs <- fi.ml_fun_retargs 
                                                           @ [ds];
                    | Spec_C_Fun_Int fi -> fi.c_fun_retargs <- fi.c_fun_retargs 
                                                           @ [ds];
                    | _ -> syntax_error "Invalid S_RetArg";            
                end;

            | S_Math ->
                begin
                    leave_p ();
                    (*
                    ** Math environment will be translated with tex to an image.
                    *)
                    env.in_math <- true;
                    (*
                    ** The first child can be a name element with the 
                    ** equation number. 
                    *)
                    let eqnum =
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        env.ignore_head <- true;
                                        (str_of_text dna.s_childs);
                                    end;
                                | _ -> "";
                           end; 
                           | [] -> "";
                         in
                    
                    (* TODO *)
                    out_str "<DL>"; nl();
                    out_str "<DT></DT>"; nl();
                    out_str "<DD>"; nl();
                end;
            | S_Math_Fun ->
                begin
                    out_str " <B>";
                    (*
                    ** The first child must be the function name
                    *)
                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name ->
                                    begin
                                        env.ignore_head <- true;
                                        (format_text dna.s_childs);
                                    end;
                                | _ -> ();
                           end; 
                           | [] -> ();
                    );
                    out_str "</B>";
                end;


            | S_Figure ->
                begin
                    leave_p ();
                    (* TODO *)
                    out_str "<DL>"; nl();
                    out_str "<DT></DT>"; nl();
                    out_str "<DD>"; nl();
                    env.in_par <- true;
                end;

            | S_Image ->
                begin
                    leave_p ();
                    if env.in_title then
                    begin
                        out_str "<BR>"; nl();
                    end;

                    let str = ref "" in
                    (
                        (*
                        ** The filename must be the first child.
                        *)
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name ->
                                begin
                                    env.ignore_head <- true;
                                    str := !str ^ (str_of_text dna.s_childs);
                                end;
                                | _ -> syntax_error "Image without filename"; 
                            end; 
                            | [] -> syntax_error "Image without filename";
                    );
                    if (!str <> "") then
                    begin
                        let str' = 
                            (doc_image_path_src env)^
                            (Str.global_replace (Str.regexp " ") "" !str) 
                            in

                        (*
                        ** EPS file ? Convert it to PNG format.
                        *)
                        if (Filename.check_suffix str' "eps") then
                        begin
                            let stat = Unix.system ("eps2png "^ str') in
                            (match stat with
                                | Unix.WEXITED s -> if s <> 0 then 
                                                syntax_error "eps2png failed";
                                | _ -> ();
                            );
                            out_str ("<IMG ALT=\"Image\" SRC=\""^
                                     (doc_image_path_dst env)^
                                     (Filename.basename (
                                        Filename.chop_extension str')) ^ 
                                     ".png\">"); 
                            nl();
                        end
                        else
                        begin
                            out_str ("<IMG ALT=\"Image\" SRC=\""^ 
                                     str' ^ "\">"); nl();
                        end;
                    end;
               end;

            | S_Html_Source ->
                begin
                    let str = (str_of_childs ds.s_childs ) in
                    (*
                    ** Inline code ? Paragraph break ?
                    *)
                    let pbr = try
                              begin
                                ignore(Str.search_forward 
                                            (Str.regexp"<P>") str 0);
                                true
                              end
                              with
                              Not_found -> 
                              begin
                                  try
                                    ignore(Str.search_forward 
                                            (Str.regexp"\n") str 0);
                                    true
                                  with Not_found -> false
                              end                                
                        in
                    if pbr then leave_p ();
                    out_str str;
                    env.ignore_childs <- true;
                    if not pbr then out_str " ";
                end;        

            | S_Tex_Source ->
                begin
                    env.ignore_childs <- true;
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
                    | Spec_ML_Fun_Int  fi  -> d ();
                    | Spec_C_Fun_Int  fi  -> d ();
                    | Spec_C_Hdr_Int hr -> d ();
                    | Spec_C_Type_Int ti -> d ();
                    | Spec_ML_Val_Int  vi  -> d (); 
                    | Spec_ML_Ext_Int  vi  -> d (); 
                    | Spec_C_Var_Int  vi  -> d (); 
                    | Spec_ML_Type_Int ti  -> d ();
                    | Spec_ML_Class_Int ti  -> d ();
                    | Spec_ML_Object_Int ti  -> d ();
                    | Spec_ML_Method_Int ti  -> d ();
                    | Spec_ML_Module_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_DataFormat tb -> d ();
                    | Spec_None -> 
                        if not env.special_name then
                        begin
                            out_str "</I>";
                        end;            
                end;
            

            | S_Attribute -> parse_attr_end ds.s_attr;

            | S_Example -> 
                begin
                            
                            env.in_preform <- false;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
                            ind_decr (); nl (); out_str "</TBODY>"; 
                            ind_decr (); nl (); out_str "</TABLE>"; nl ();
                            out_str "<br>"; nl ();
                            env.in_par <- true;
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
                            env.in_par <- true;
                end;

            | S_OList ->    
                begin
                            
                            ind_decr (); 
                            out_str "</OL><br>"; nl ();
                            env.in_list <- (List.tl env.in_list);
                            env.in_par <- true;
                end;

            | S_UList ->    
                begin
                            
                            ind_decr ();  
                            out_str "</UL><br>"; nl ();
                            env.in_list <- (List.tl env.in_list);
                            env.in_par <- true;
                end;

            | S_ArgList ->    
                begin
                    (
                        match (List.hd env.in_list) with
                        | A_List ind ->
                        if ind then
                        begin
                            ind_decr (); nl (); out_str "</DD>"; nl ();
                            ind_decr (); nl (); out_str "</DL>"; nl ();
                        end;
                        | _ -> syntax_error "S_ArgList not in env.in_list!!";
                    );
                    env.in_list <- (List.tl env.in_list);
                    env.in_par <- true;
                end;

            | S_LitList ->    
                begin
                    ind_decr (); nl (); out_str "</DD><br>"; nl ();
                    ind_decr (); nl (); out_str "</DL><br>"; nl ();
                    env.in_list <- (List.tl env.in_list);
                    env.in_par <- true;
                end;

            | S_List_Item ->
                begin
                    leave_p ();
                    match (List.hd env.in_list) with
                    | L_List ->
                        begin
                            ind_decr (); nl (); out_str "</DD>"; nl ();
                            ind_decr (); nl (); out_str "</DL>"; nl ();
                        end;
                    | A_List _ ->
                        begin
                            ind_decr (); nl (); out_str "</DD>"; nl ();
                            ind_decr (); nl (); out_str "</DL>"; nl ();
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
                    | Spec_ML_Fun_Int  fi  -> d ();
                    | Spec_C_Fun_Int  fi  -> d ();
                    | Spec_C_Hdr_Int  hr  -> ();
                    | Spec_ML_Val_Int  vi  -> d (); 
                    | Spec_ML_Ext_Int  vi  -> d (); 
                    | Spec_C_Var_Int  vi  -> d (); 
                    | Spec_C_Type_Int ti  -> d ();
                    | Spec_ML_Type_Int ti  -> d ();
                    | Spec_ML_Class_Int ti  -> d ();
                    | Spec_ML_Object_Int ti  -> d ();
                    | Spec_ML_Method_Int ti  -> d ();
                    | Spec_ML_Module_Int ti  -> d ();
                    | Spec_Table tb -> d ();
                    | Spec_DataFormat tb -> d ();
                    | Spec_None -> 
                        if not env.special_name then
                        begin
                            out_str "</I> ";
                        end;            
                end;

            | S_Value -> env.ignore_childs <- false;    (* internally handled *)

            | S_Link -> 
                    out_str "</a></span>&nbsp;";

            | S_Ref ->     
                    out_str "</span>&nbsp;";

            | S_Label -> ();

            | S_Symbol -> ();

            | S_Filename -> ();

            | S_Table -> 
              begin 
                leave_p ();
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
                            "<br><br>\n"^
                            "<TABLE BORDER=\"1\" "^
                            "CLASS=\"table\" "^
                            "ALIGN=\"center\" WIDTH=\"80%\">"
                    )
                else
                    out_str (
                            "<br><br>\n"^
                            "<TABLE BORDER=\"0\" "^
                            "CLASS=\"table\" "^
                            "ALIGN=\"center\" WIDTH=\"80%\">"
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
                            "ALIGN=\"center\" "^
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
                env.in_par <- true;
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
                    leave_p ();
                    env.special_depth <- env.special_depth - 1;
                      
            | S_DataFormat ->
              begin
                leave_p ();
                let tb =   
                        match env.in_special with
                        | Spec_DataFormat tb -> tb;
                        | _ -> syntax_error "Inavlid in_special";
                in 
                let th = tb.dataformat_headlist in
                let thn = List.length th in
                let tr = tb.dataformat_rowlist in
                let trn = List.length tr in
                let tn = tb.dataformat_name in

                env.special_depth <- env.special_depth - 1;
                env.in_special <- Spec_None;

                (*
                ** Get the cell width in char units
                *)
                let cellwidth = ref 0 in
                List.iter (fun de ->
                    List.iter (fun c ->
                        match c.s_content with
                        | S_Value -> 
                            let str = str_of_childs c.s_childs in
                            let str' = Str.global_replace
                                            (Str.regexp " ") "" str in
                            cellwidth := max !cellwidth (
                                try 
                                    (int_of_string str')
                                with _ -> syntax_error "invalid value specifier";
                                );
                        | _ -> ();
                        ) de.s_childs;
                    ) th;
                if !cellwidth = 0 then 
                    syntax_error "data format without cellwidth specifier";


                out_str (
                            "<br><br>\n"^
                            "<TABLE BORDER=\"1\" "^
                            "CLASS=\"dataformat\" "^
                            "ALIGN=\"center\" >"
                    );
                ind_incr ();
                nl (); out_str "<TBODY>"; ind_incr ();

                if (tn <> []) then 
                begin
                    (*
                    ** The table header
                    *)
                    nl (); out_str "<TR>";
                    nl (); out_str (
                            "<TD STYLE=\"background-color: #C1FFFF\" "^
                            "ALIGN=\"center\" "^
                            "COLSPAN="^(string_of_int thn)^">"
                    );
                    out_str "<B>";   
                    List.iter (fun dn ->
                        List.iter cont_trans dn.s_childs;
                        ) tn;
                    out_str "</B>";
                    out_str "</TD>"; 
                    ind_decr (); nl (); out_str "</TR>";
                end;


                nl (); out_str "<TR>"; ind_incr ();

                let ci = ref 1 in
                let cn = List.length th in
                List.iter (fun de ->
                       nl (); out_str "<TD>";
                       out_str "<B>";
                       List.iter cont_trans de.s_childs;
                       out_str "</B>";
                       out_str "</TD>";
                       incr ci;
                    ) th;

                ind_decr (); nl (); out_str "</TR>";

                List.iter (fun r ->
                        let ci = ref 1 in
                        let rl = List.length r in
                        nl (); out_str "<TR>"; ind_incr ();

                        List.iter (fun c ->
                            (*
                            ** Get the data width in cell units
                            *)
                            let width = ref 0 in
                            List.iter (fun c -> 
                                match c.s_content with
                                | S_Value -> 
                                    let str = str_of_childs c.s_childs in
                                    let str' = Str.global_replace
                                            (Str.regexp " ") "" str in
                                    width := (
                                        try   
                                            (int_of_string str')
                                        with _ -> 
                                        syntax_error "inavlid value specifier";
                                    );
                                | _ -> ();
                                ) c.s_childs;
                            if !width = 0 then
                                syntax_error "data entry without width specifier";

                            nl ();
                            if !width > 1 
                                then out_str (sprintf "<TD colspan=%d>"
                                                      !width)
                                else out_str "<TD>";

                            List.iter cont_trans c.s_childs;

                            out_str "</TD>";  

                            incr ci;
                        ) r;        
                    ind_decr (); nl (); out_str "</TR>";
                    ) tr;

                ind_decr (); nl (); out_str "</TBODY>"; 
                ind_decr (); nl (); out_str "</TABLE><br>"; nl ();

                env.in_par <- true;   
              end;

            | S_DataRow ->
                begin
                    match env.in_special with
                    | Spec_DataFormat tb ->
                        begin
                                    let cr = tb.dataformat_currow in
                                    tb.dataformat_rowlist <- 
                                            tb.dataformat_rowlist
                                            @ [!cr];
                        end;
                    | _ -> syntax_error "Invalid S_DataRow";
                end;

           | S_DataHead ->
                begin
                    match env.in_special with
                    | Spec_DataFormat tb ->  
                        begin
                                    let cr = tb.dataformat_currow in
                                    tb.dataformat_headlist <- !cr;  
                        end;
                    | _ -> syntax_error "Invalid S_DataHead";
                end;

            | S_DataEntry ->
                    env.special_depth <- env.special_depth - 1;



            | S_Body -> 
                begin
                    leave_p ();
                    if (env.toc.sec_childs <> [] &&
                       (not (doc_single env) || 
                        (doc_toc env))) then
                    begin
                        out_str "<br><br>"; nl ();
                        nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "CLASS=\"toc\" "^
                                "WIDTH=\"50%\" BORDER=\"1\">"
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

                        print_sub_toc env.toc;

                        out_str "</TD>"; 
                        ind_decr (); nl (); out_str "</TR>"; 
                        ind_decr (); nl ();
                        out_str "</TBODY>"; 
                        ind_decr (); nl ();
                        out_str "</TABLE>"; nl ();

                    end;

                    ind_decr ();
                    
                    print_copyright ();


                    oc_close (Doc_Main env.doc_name);
                    oc_next ();
                end;


            | S_Title -> 
                begin
                    out_str "</H1><BR><BR>"; nl ();
                    env.in_par <- true;
                    env.in_title <- false;
                end;

            | S_TOC -> ();



            | S_S1 -> 
                begin
                    leave_p ();  
                    if (env.cur_s1 <> None) then
                    begin
                        let toc = 
                            match env.cur_s1 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            not (doc_single env) &&
                            (doc_sec_break env Doc_multi_s2)) then
                        begin 
                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                    "<TABLE ALIGN=\"center\" "^
                                    "CLASS=\"toc\" "^
                                    "WIDTH=\"50%\" BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\" "^
                                    "STYLE=\"background-color: #FFA000\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();


                            (* 
                            ** Print the TOC (subsection of this S1)
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; nl();

                            print_sub_toc toc;

                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 
    
                            ind_decr (); nl ();
                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env Doc_multi_s1;
                    oc_close Doc_multi_s1;
                    oc_next ();
                    env.cur_s1 <- None;
                    env.in_par <- true;
                end;


            | S_S2 -> 
                begin
                    leave_p ();
                    if (env.cur_s2 <> None) then
                    begin
                        let toc = 
                            match env.cur_s2 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            not (doc_single env) &&
                            (doc_sec_break env Doc_multi_s3)) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "CLASS=\"toc\" "^
                                "WIDTH=\"50%\" BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\" "^
                                    "CLASS=\"toc\" "^
                                    "STYLE=\"background-color: #B0FF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC (subsections of this S2)
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_sub_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env Doc_multi_s2;
                    oc_close Doc_multi_s2;
                    oc_next ();
                    env.cur_s2 <- None ;
                    env.in_par <- true;
                end;


            | S_S3 -> 
                begin
                    leave_p ();
                    if (env.cur_s3 <> None) then
                    begin
                        let toc = 
                            match env.cur_s3 with
                            | Some s -> s;
                            | None -> failwith "programming error";
                        in
                        if (toc.sec_childs <> [] &&
                            not (doc_single env) &&
                            (doc_sec_break env Doc_multi_s4)) then
                        begin 

                            out_str "<br><br>"; nl ();
                            nl ();out_str (
                                "<TABLE ALIGN=\"center\" "^
                                "CLASS=\"toc\" "^
                                "WIDTH=\"50%\" BORDER=\"1\">"
                            ); ind_incr ();
                            nl ();out_str "<TBODY>"; ind_incr (); nl ();
                            out_str (
                                    "<TR><TD ALIGN=\"center\" "^
                                    "CLASS=\"toc\" "^
                                    "STYLE=\"background-color: #FFFF00\""^
                                    ">Content</TD></TR>"
                            ); 
                            nl ();

                            (* 
                            ** Print the TOC (subsections of this S3)
                            *)
                            out_str "<TR>"; ind_incr (); nl ();
                            out_str "<TD>"; 

                            print_sub_toc toc;
                            out_str "</TD>"; 
                            ind_decr (); nl (); out_str "</TR>"; 

                            out_str "</TBODY>"; 
                            ind_decr (); nl ();
                            out_str "</TABLE>"; nl ();
                        end;
                    end;

                    close_sec env Doc_multi_s3;
                    oc_close Doc_multi_s3;
                    oc_next ();
                    env.cur_s3 <- None ;
                    env.in_par <- true;
                end;


            | S_S4 -> 
                begin
                    leave_p ();
                    ind_incr (); nl (); out_str "</DD><br>";  nl ();
                    ind_incr (); nl (); out_str "</DL><br>";  nl ();
                    env.cur_s4 <- None;
                    env.in_par <- true;
                end;


            | S_MP -> 
                begin
                    leave_p ();
                    close_sec env Doc_multi_mp;
                    oc_close Doc_multi_mp;
                    oc_next ();
                    env.cur_mp <- None ;
                    env.in_par <- true;
                end;


            | S_MP_paragr -> 
                begin
                    leave_p ();
                    ind_incr (); nl (); out_str "</DD><br>";  nl ();
                    env.cur_mp_paragr <- None;
                    env.in_par <- true;
                end;


            | S_Math -> 
                begin
                    
                    leave_p ();
                    nl(); out_str "</DD>"; nl();
                    out_str "</DL>"; nl ();
                    out_str "<P><BR></P>"; nl ();
                    env.in_math <- false;
                    env.in_par <- true;                    
                end;
            | S_Math_Fun ->
                begin
                    out_str " ";
                end;

            | S_Figure -> 
                begin
                    
                    leave_p ();
                    nl(); out_str "</DD>"; nl();
                    out_str "</DL>"; nl ();
                    out_str "<P><BR></P>"; nl ();
                    env.in_par <- true;                    
                end;

            | S_Image -> 
                begin
                    
                    (* TODO *)
                    env.in_par <- true;                    
                end;
            | S_Html_Source -> env.in_par <- true;
            | S_Tex_Source -> ();



    
            | S_Mutable -> ();
            | S_Private -> ();
            | S_Virtual -> ();

            | S_ML_Fun_Interface ->
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
                        | Spec_ML_Fun_Int fi -> fi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let rargs = fi.ml_fun_retargs in
                    let fname = fi.ml_fun_name in
                    let args = fi.ml_fun_args in
                    let curried = fi.ml_fun_curried in
                    let comm = fi.ml_fun_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CLASS=\"interface_entry\" "^
                                "CELLSPACING=\"-5\" >"
                    ); ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (fname = []) then
                        syntax_error "Function interface without name";
                    
                    let nargs = List.length args in


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
                                        out_str "<TD></TD>"; nl(); 
                                        out_str "<TD></TD>"; nl(); 
                                        ind_decr ();out_str "</TR>";nl ();
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

                                    incr i;
                                  ) rargs;

                    end;
                    
                    if (args <> []) then
                    begin
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                        out_str "<TD></TD>";nl ();
                                    end;
                                    out_str "<TD>&nbsp; ";
                                                           

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


                                    if (!i < nargs) then
                                    begin
                                        match curried with
                                        | true -> out_str " -&gt;";
                                        | false -> out_str " * ";
                                    end;
                                    out_str "</TD>"; 

                                    begin
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>";
                                          end    
                                        | None -> out_str "<TD></TD>";  
                                    end;

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
                

            | S_ML_Val_Interface -> 
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
                        | Spec_ML_Val_Int vi -> vi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = vi.ml_val_name in
                    let args = vi.ml_val_args in
                    let curried = vi.ml_val_curried in
                    let comm = vi.ml_val_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;


                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CLASS=\"interface_entry\" ";
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

            | S_ML_Ext_Interface -> 
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
                        | Spec_ML_Ext_Int vi -> vi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = vi.ml_ext_name in
                    let args = vi.ml_ext_args in
                    let curried = vi.ml_ext_curried in
                    let comm = vi.ml_ext_comm in
                    let extfun = vi.ml_ext_fun in

                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();

                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;


                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CLASS=\"interface_entry\" ";
                    out_str "CELLSPACING=\"-5\" >";ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (vname = []) then
                        syntax_error "External interface without name";
                    

                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">external&nbsp;</TD>"; 
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


                    if extfun = [] then
                        syntax_error "External value without external function";

                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD></TD>"; nl (); 
                    out_str "<TD></TD>"; nl ();
                    out_str "<TD></TD>"; nl ();
                    out_str "<TD>=&nbsp;\""; 

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
                              ) (List.hd extfun).s_childs;

                    out_str "\"</TD>"; nl();
                    out_str "<TD></TD>"; nl ();
                    ind_decr ();out_str "</TR>"; nl ();

                    
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

            | S_ML_Module_Interface ->
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

            | S_ML_Type_Interface  
            | S_ML_Struc_Interface 
            | S_ML_Exc_Interface ->
            begin
                if (env.in_interface = true ||
                    env.in_module = true) then
                begin
                    nl (); out_str "<TR>"; ind_incr ();  
                    nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                end;

                let tp = 
                    match env.in_special with
                    | Spec_ML_Type_Int tp -> tp;
                    | _ -> syntax_error "Invalid in_special";
                in 
                let tname = tp.ml_type_name in
                let args = tp.ml_type_args in
                let tp_type = tp.ml_type_type in
                let comm = tp.ml_type_comm in
                    
                if (tp_type <> Type_Exception) then
                    env.special_depth <- env.special_depth - 1;

                env.in_special <- Spec_None; 
                nl();

                if (comm <> []) then
                begin
                    out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                    cont_trans (List.hd comm);
                    out_str "</TD></TR></TBODY></TABLE>";
                    nl ();
                end;

                out_str "<TABLE ";
                out_str "BORDER=\"0\" ";
                out_str "ALIGN=\"left\" ";
                out_str "CLASS=\"interface_entry\" ";
                out_str "CELLSPACING=\"-5\" >";ind_incr ();
                out_str "<TBODY>"; ind_incr ();
                if (tname = []) then
                    syntax_error "Type interface without name";
                    

                out_str "<TR>"; ind_incr (); nl (); 
                (
                match tp_type with
                | Type_List ->
                begin
                    (*
                    ** 1. Row
                    *)
                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">type&nbsp;</TD>"; nl();
                    out_str "<TD><B>"; 
                    List.iter cont_trans
                              (List.hd tname).s_childs;
                    out_str "&nbsp;</B>&nbsp;=</TD>"; nl();
                    let nargs = List.length args in

                    if nargs > 1 then 
                    begin
                        out_str "<TD></TD>"; nl ();
                        out_str "<TD></TD>"; nl ();
                        out_str "<TD></TD>"; nl ();                 
                        ind_decr(); out_str "</TR>"; nl ();    
                        (*
                        ** 2. Row
                        *)
                        out_str "<TR>";ind_incr ();
                        out_str "<TD></TD>"; nl ();
                        out_str "<TD><B>|</B>&nbsp;&nbsp;";

                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>"; nl ();
                                        out_str "<TD><B>|</B>&nbsp;&nbsp;";
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
                                          | _ -> cont_trans na;
                                    ) a.s_childs; 
                        
                                    out_str "</TD>"; nl();
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>"; nl();
                                          end    
                                        | None -> out_str "<TD></TD>";nl();  
                                    );
                                    out_str "<TD></TD>"; nl();
                                    out_str "<TD></TD>"; nl();                                                                        
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end
                    else if nargs = 1 then
                    begin
                        (*
                        ** Only one tpye argument.
                        *)
                        out_str "<TD>&nbsp;";
                        let arg = List.hd args in

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
                                          | _ -> cont_trans na;
                            ) arg.s_childs; 
                        
                        out_str "</TD>"; nl();
                        (
                            match !com with
                            | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>"; nl();
                                          end    
                            | None -> out_str "<TD></TD>";nl();  
                        );
                        out_str "<TD></TD>"; nl();
                        out_str "<TD></TD>"; nl();                                                                        
                        ind_decr ();out_str "</TR>"; nl ();

                    end 
                    else 
                        syntax_error "type without args";
                end;
                | Type_Structure ->
                begin
                    (*
                    ** 1. Row
                    *)
                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">type&nbsp;</TD>"; nl();
                    out_str "<TD><B>"; 
                    List.iter cont_trans
                              (List.hd tname).s_childs;
                    out_str "&nbsp;</B>&nbsp;=</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    
                    (*
                    ** 2. Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD>";
                    out_str " { &nbsp; ";
                    out_str "</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    

                    (*
                    ** 3. Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD></TD>"; nl ();
                    out_str "<TD>";

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>"; nl ();
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
                        
                                    out_str "&nbsp;;</TD>"; nl();
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>"; nl();
                                          end    
                                        | None -> out_str "<TD></TD>";nl();  
                                    );
                                    out_str "<TD></TD>"; nl();
                                    out_str "<TD></TD>"; nl();                                                                        
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end
                    else 
                        syntax_error "type without args";

                    (*
                    ** Last Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD>";
                    out_str " } &nbsp; ";
                    out_str "</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    

                end;
                | Type_Exception ->
                begin
                    (*
                    ** Only 1 Row
                    *)
                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str (
                            "<TD style=\"color: #FF0000\">"^
                            "exception&nbsp;</TD>"); nl();
                    out_str "<TD><B>"; 

                    let com = ref None in
                    List.iter
                        ( fun na ->
                                match na.s_content with
                                | S_Text t -> html_text t;
                                | S_Comment -> com := Some na;
                                | _ -> env.special_name <- true;
                                       cont_trans na;
                                       env.special_name <- false;
                        ) (List.hd tname).s_childs;

                    out_str "&nbsp;</B></TD>"; nl();

                    (
                        match !com with
                        | Some com -> 
                            begin
                            out_str "<TD>";
                            cont_trans com;
                            out_str "</TD>"; nl();
                                          end    
                        | None -> out_str "<TD></TD>";nl();  
                    );

                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    
                end;
                | Type_Empty -> syntax_error "Type empty in ML type/structure";    

                );               
                ind_decr (); out_str "</TBODY>"; nl ();
                ind_decr (); out_str "</TABLE>"; nl ();


                if (env.in_interface = true ||
                    env.in_module = true) then
                begin
                    out_str "</TD>"; 
                    ind_decr (); nl (); out_str "</TR>"; 
                end;

             end;

            | S_ML_Class_Interface ->
                begin

                    env.special_depth <- env.special_depth - 1;


                    if (env.in_interface = true) then
                    begin
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                    end;
                    
                    let ci =
                        match env.in_special with
                        | Spec_ML_Class_Int ci -> ci;
                        | _ -> syntax_error "Invalid class interface";
                    in
                    let cname = ci.ml_class_name in
                    let cargs = ci.ml_class_args in
                    let comm = ci.ml_class_comm in
                    
                    let co    = 
                        match ci.ml_class_obj with
                        | Spec_ML_Object_Int co -> co;
                        | _ -> syntax_error "Invalid class/object interface";
                    in
                    let oname = co.ml_obj_name in
                    let oargs = co.ml_obj_args in

                    nl ();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
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
                    out_str "CLASS=\"interface_entry\" ";
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
                        out_str "CLASS=\"interface_entry\" ";
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

            | S_ML_Object_Interface ->
                begin
                    env.special_depth <- env.special_depth + 1;

                    match env.in_class with
                    | Spec_ML_Class_Int ci -> 
                            env.in_special <- env.in_class;
                            ci.ml_class_obj <- env.in_object;
                            env.in_object <- Spec_None;
                            
                    | _ -> syntax_error "Invalid class/object interface";
                end;

            | S_ML_Method_Interface ->
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
                        | Spec_ML_Method_Int mi -> mi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = mi.ml_meth_name in
                    let args = mi.ml_meth_args in
                    let comm = mi.ml_meth_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str "<TABLE ";
                    out_str "BORDER=\"0\" ";
                    out_str "ALIGN=\"left\" ";
                    out_str "CLASS=\"interface_entry\" ";
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



                    let vi = 
                        match env.in_special with
                        | Spec_C_Var_Int vi -> vi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let name = vi.c_var_name in
                    let args = vi.c_var_args in
                    let comm = vi.c_var_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CLASS=\"interface_entry\" "^
                                "CELLSPACING=\"-5\" >"
                    ); ind_incr ();
                    out_str "<TBODY>"; ind_incr ();
                    if (name = []) then
                        syntax_error "C-Variable interface without name";
                    if (args = []) then
                        syntax_error "C-Variable interface without type args";

                    let a = List.hd args in
                    out_str "<TR>"; nl();
                    out_str "<TD>";
                    List.iter
                        (fun na ->
                            match na.s_content with
                            | S_Comment -> ();
                            | _ -> cont_trans na;
                         )a.s_childs;

                    out_str "&nbsp;"; 
                    out_str "</TD>"; nl ();
                    out_str "<TD><B>"; 
                    List.iter cont_trans
                              (List.hd name).s_childs;
                    out_str "</B>;</TD>"; nl();    
                    out_str "</TR>"; nl ();
                    
                    ind_decr (); out_str "</TBODY>"; nl ();
                    ind_decr (); out_str "</TABLE>"; nl ();




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
                        | Spec_C_Fun_Int fi -> fi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let rargs = fi.c_fun_retargs in
                    let fname = fi.c_fun_name in
                    let args = fi.c_fun_args in
                    let comm = fi.c_fun_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 
                    nl();
                    if (comm <> []) then
                    begin
                        out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                        cont_trans (List.hd comm);
                        out_str "</TD></TR></TBODY></TABLE>";
                        nl ();
                    end;

                    out_str (   "<TABLE "^
                                "BORDER=\"0\" "^
                                "ALIGN=\"left\" "^
                                "CLASS=\"interface_entry\" "^
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
                if (env.in_object = Spec_None) then
                begin

                    let hr = 
                        match env.in_special with
                        | Spec_C_Hdr_Int hr -> hr;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let hname = hr.c_hdr_name in
                    let comm = hr.c_hdr_comm in
                    
                    env.in_special <- Spec_None; 
                
                    if (env.in_class = Spec_None) then
                    begin
                        if (env.in_interface = true) then
                        begin
                            nl (); out_str "<TR>"; ind_incr ();  
                            nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                        end;
                        nl();
                        if (comm <> []) then
                        begin
                            out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                            cont_trans (List.hd comm);
                            out_str "</TD></TR></TBODY></TABLE>";
                            nl ();
                        end;

                        out_str (   "<TABLE "^
                                    "BORDER=\"0\" "^
                                    "ALIGN=\"left\" "^
                                    "CLASS=\"interface_entry\" "^
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
                    end
                    else
                    begin
                        (*
                        ** Special case: class inherit!!!
                        *)
                        nl (); out_str "<TR>"; ind_incr ();  
                        nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                        if (comm <> []) then
                        begin
                            out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                            cont_trans (List.hd comm);
                            out_str "</TD></TR></TBODY></TABLE>";
                            nl ();
                        end;
                        out_str (   "<TABLE "^
                                    "BORDER=\"0\" "^
                                    "ALIGN=\"left\" "^
                                    "CLASS=\"interface_entry\" "^
                                    "CELLSPACING=\"-5\" >"
                        ); ind_incr ();
                        out_str "<TBODY>"; ind_incr ();
                        if (hname = []) then
                            syntax_error "Class inherit interface without desc."
                        else
                        begin                    
                            out_str "<TR>";ind_incr ();
                            out_str ("<TD style=\"color: "^
                                     "#FF0000\">inherit &nbsp;</TD>"
                            ); nl();

                            out_str "<TD><B>"; 

                            List.iter cont_trans 
                                      (List.hd hname).s_childs;

                            out_str "</B>&nbsp;</TD>"; nl();
                            ind_decr (); out_str "</TR>"; nl ();
                        end;

                        ind_decr (); out_str "</TBODY>"; nl ();
                        ind_decr (); out_str "</TABLE>"; nl ();
            
                    end;
                end;

            | S_C_Type_Interface  
            | S_C_Struc_Interface ->
            begin
                if (env.in_interface = true ||
                    env.in_module = true) then
                begin
                    nl (); out_str "<TR>"; ind_incr ();  
                    nl (); out_str "<TD ALIGN=\"left\" >"; nl (); 
                end;

                let tp = 
                    match env.in_special with
                    | Spec_C_Type_Int tp -> tp;
                    | _ -> syntax_error "Invalid in_special";
                in 
                let tname = tp.c_type_name in
                let args = tp.c_type_args in
                let tp_type = tp.c_type_type in
                let comm = tp.c_type_comm in
                    
                env.special_depth <- env.special_depth - 1;
                env.in_special <- Spec_None; 
                nl();

                if (comm <> []) then
                begin
                    out_str "<TABLE CLASS=\"comment\"><TBODY><TR><TD>";
                    cont_trans (List.hd comm);
                    out_str "</TD></TR></TBODY></TABLE>";
                    nl ();
                end;

                out_str "<TABLE ";
                out_str "BORDER=\"0\" ";
                out_str "ALIGN=\"left\" ";
                out_str "CLASS=\"interface_entry\" ";
                out_str "CELLSPACING=\"-5\" >";ind_incr ();
                out_str "<TBODY>"; ind_incr ();
                if (tname = []) then
                    syntax_error "Type interface without name";
                    

                out_str "<TR>"; ind_incr (); nl (); 
                (
                match tp_type with
                | Type_List ->
                begin
                    (*
                    ** 1. Row
                    *)
                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">typedef&nbsp;</TD>"; nl();
                    out_str "<TD><B>"; 
                    List.iter cont_trans
                              (List.hd tname).s_childs;
                    out_str "&nbsp;</B>&nbsp;</TD>"; nl();
                    let nargs = List.length args in

                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>"; nl ();
                                        out_str "<TD>&nbsp;&nbsp;</TD>"; nl();
                                    end;

                                    out_str "<TD>";
    
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
                                          | _ -> cont_trans na;
                                    ) a.s_childs; 
                        
                                    out_str "</TD>"; nl();

                                    if !i < nargs then
                                    begin
                                        out_str "<TD>,</TD>";nl();
                                    end
                                    else
                                    begin
                                        out_str "<TD>;</TD>";nl();
                                    end;


                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>"; nl();
                                          end    
                                        | None -> out_str "<TD></TD>";nl();  
                                    );

                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end;
                end;
                | Type_Structure ->
                begin
                    (*
                    ** 1. Row
                    *)
                    out_str "<TR>"; ind_incr (); nl (); 
                    out_str "<TD style=\"color: #FF0000\">struct &nbsp;</TD>"; nl();
                    out_str "<TD><B>"; 
                    List.iter cont_trans
                              (List.hd tname).s_childs;
                    out_str "&nbsp;</B>&nbsp;</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    
                    (*
                    ** 2. Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD>";
                    out_str " { &nbsp; ";
                    out_str "</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    

                    (*
                    ** 3. Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD></TD>"; nl ();
                    out_str "<TD>";

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "<TR>";ind_incr ();
                                        out_str "<TD></TD>"; nl ();
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
                        
                                    out_str ";</TD>"; nl();
                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str "<TD>";
                                            cont_trans com;
                                            out_str "</TD>"; nl();
                                          end    
                                        | None -> out_str "<TD></TD>";nl();  
                                    );
                                    out_str "<TD></TD>"; nl();
                                    out_str "<TD></TD>"; nl();                                                                        
                                    ind_decr ();out_str "</TR>"; nl ();
                                    incr i;
                                  ) args;
                    end
                    else 
                        syntax_error "type without args";

                    (*
                    ** Last Row
                    *)
                    out_str "<TR>";ind_incr ();
                    out_str "<TD>";
                    out_str " }; ";
                    out_str "</TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();
                    out_str "<TD></TD>"; nl();                 
                    ind_decr(); out_str "</TR>"; nl ();    

                end;
                | Type_Empty -> syntax_error "Type empty in C type/structure";    
                | Type_Exception -> syntax_error "C-Exception ? ";
                );               
                ind_decr (); out_str "</TBODY>"; nl ();
                ind_decr (); out_str "</TABLE>"; nl ();


                if (env.in_interface = true ||
                    env.in_module = true) then
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
                            env.in_par <- true;
                end;


            | S_CurArg -> 
                    env.special_depth <- env.special_depth - 1;
                    

            | S_UnCurArg -> 
                    env.special_depth <- env.special_depth - 1;

            | S_RetArg -> 
                    env.special_depth <- env.special_depth - 1;


        );
    in
#if 0
    try
#endif
        cont_trans ds
#if 0
    with
        Failure s -> syntax_error s
#endif                     
