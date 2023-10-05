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
**    $CREATED:     7.3.2005
**    $MODIFIED:    
**    $VERSION:     1.20
**
**    $INFO:
**
** LaTeX backend.
**
**    $ENDINFO
*)



open Doc_core 
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
    mutable in_example : bool;
    mutable in_interface: bool;
    mutable in_title: bool;

    (*
    ** ml interfaces
    *)
    mutable in_class: struc_special;
    mutable in_object: struc_special;
    mutable in_module: bool;        

    mutable in_olist: int;
    mutable in_ulist: int;
    mutable in_list: list_type list;

    mutable in_math: bool;

    mutable in_subsup: bool;
    mutable in_par: bool;       (* paragraphe mode ? *)
    mutable in_figure: bool;
    mutable in_ref: bool;
    mutable last_nl : bool;
    
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
    mutable textwidth   : float;

    mutable toc_file    : bool;
}

(*
** Check for special document options
*)

let doc_color env =
    List.mem Doc_color env.doc_options 

let doc_link_ref env =
    List.mem Doc_link_ref  env.doc_options

let doc_toc env =
    List.mem Doc_with_toc env.doc_options

let doc_sec_break env sec =   
    List.mem sec env.doc_options

let doc_pdf env =
     List.mem Doc_pdftex env.doc_options

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

let warn str =
    print_string str;
    print_newline ()



(*
** Generate a valid file name from a string (for example a link).
*)

let file_name str secnum =
    let rs = " \|\\\\\|,\|/\|;\|-\|+\|'\|#\|%\|&\|\"\|!\|(\|)\|[\|]\|{\|}"^
             "\|~\|ä\|ö\|ü\|Ö\|Ä\|Ü" in
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
** Transform the structure tree to Tex 
**
** Argument: 
**
**  ds: structure tree
**
*)


let tex_of_tree ~ds ~options ~sections =


    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = "main"; sec_type = "Main";
                         sec_num = 0;} 
    in  

    let sec_num = ref 1 in

    let env = {
        doc_options     = [];
        doc_name        = "manual.tex";
            
        ignore_head     = false;
        ignore_childs   = false;
        in_special      = Spec_None;
        special_depth   = 0;
        special_name    = false;

        in_preform      = false;
        in_interface    = false;
        in_example      = false;
        in_title        = false;
        in_figure       = false;

        in_class        = Spec_None;
        in_object       = Spec_None;
        in_module       = false;
        
        in_olist        = 0;
        in_ulist        = 0;
        in_list         = [];

        in_math         = false;

        in_subsup       = false;
        in_par          = false;
        in_ref          = false;
        last_nl         = false;
        
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
        doc_colwidth    = 80;

        table_col       = 0;
        textwidth       = 16.5;
    
        toc_file        = false;

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

    let oc_chan = ref (open_out env.doc_name) in
    let oc_name = ref (env.doc_name) in


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
        let oc = !oc_chan in
        if (env.doc_col + slen > env.doc_colwidth &&
            env.in_preform = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent)); 
            env.doc_col <- env.doc_indent;
        end;
        output_string oc str; 
        cur_line := !cur_line ^ str;
        env.doc_col <- env.doc_col + slen;
    in


    let nl () =
        let oc = !oc_chan  in
        if (env.doc_col > env.doc_indent && 
            env.in_preform = false &&
            env.in_example = false) then
        begin
            output_string oc ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
        else if (env.in_example = true &&
                 env.in_preform = true) then
        begin 
            output_string oc ("\\hfil} \\cr\n");
            output_string oc ((spaces env.doc_indent)^
                              "& {\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end
        else if (env.in_preform = true) then
        begin 
            output_string oc ("\\hfil} \\cr\n");
            output_string oc ((spaces env.doc_indent)^
                              "{\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end;
        
    in

    let ind_incr () =
#if 0
        env.doc_indent <- env.doc_indent + 2;
        output_string (!oc_chan) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
#endif 
        ();
    in
    let ind_decr () =
#if 0
        env.doc_indent <- env.doc_indent - 2;
        output_string (!oc_chan) ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
#endif
    ();
    in

#else 

    let cur_line = ref "" in
    let out_str str =
        let slen = String.length str in
        let oc = !oc_chan in
        if (env.doc_col + slen > env.doc_colwidth &&
            env.in_preform = false) then
        begin
            (* output_string oc ("\n"^(spaces env.doc_indent)); *)
            cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        (* output_string oc str; *)
        cur_line := !cur_line ^ str;
        env.doc_col <- env.doc_col + slen;
    in

    let flush_str () =
        (*
        ** Remove some unnecessary spaces between dot and colons.
        *)
        let subst_list = [
                    " }," , "},";
                    " }\." , "}.";
                    " }:" , "}:";
                    " $_{", "$_{";
                    " $^{", "$^{";
                ] in
        List.iter (fun (exp,rep) ->
                 cur_line := Str.global_replace 
                                (Str.regexp exp) rep !cur_line;
            ) subst_list;
        output_string !oc_chan !cur_line;
        cur_line := "";
        in


    let nl () =
        let oc = !oc_chan  in
        if (env.doc_col > env.doc_indent && 
            env.in_preform = false &&
            env.in_example = false) then
        begin
            cur_line := ! cur_line ^ ("\n"^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end
        else if (env.in_example = true &&
                 env.in_preform = true) then
        begin 
            cur_line := !cur_line ^ ("\\hfil} \\cr\n");
            cur_line := !cur_line ^ ((spaces env.doc_indent)^
                              "& {\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end
        else if (env.in_preform = true) then
        begin 
            cur_line := !cur_line ^ ("\\hfil} \\cr\n");
            cur_line := !cur_line ^ ((spaces env.doc_indent)^
                              "{\\tt \\hskip 1em ");
            env.doc_col <- 1;
        end;
        flush_str ();        
    in

    let ind_incr () =
#if 0
        env.doc_indent <- env.doc_indent + 2;
        cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
#endif 
        ();
    in
    let ind_decr () =
#if 0
        env.doc_indent <- env.doc_indent - 2;
        cur_line := !cur_line ^ ("\n"^(spaces env.doc_indent));
        env.doc_col <- env.doc_indent;
#endif
    ();
    in

#endif






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
            | '~' -> if env.in_math then "^\sim" else "$^\sim$";
            | '=' -> if env.in_math then "=" else "$=$";
            | '#' -> "\\#";
            | '$' -> "\\$";
            | '*' -> "$*$";
            | '%' -> "\\%";
            | '&' -> "\\&";
            | '^' -> if env.in_math then "\\hat{}" else "$\\hat{}$";
            | '_' -> "\\_";
            | '{' -> "\\{";
            | '}' -> "\\}";
            | '\\' -> if env.in_math then "\\setminus" else "$\\setminus$";
            | '\138' -> "";
            | '-' -> 
                begin
                     match !next_c with
                     | '>' ->  ignore_next := true;
                               if env.in_math
                                then "\\rightarrow"
                                else "$\\rightarrow$";
                    
                     | _ -> if env.in_math then "-" else "-"; 
                end;
            | '>' -> if env.in_math then ">" else "$>$";
            | '<' -> 
                begin
                     match !next_c with
                     | '-' ->  ignore_next := true;
                               if env.in_math 
                                then "\\leftarrow"
                                else "$\\leftarrow$";
                    
                     | _ -> if env.in_math then "<" else "$<$"; 
                end;
            | _ -> str_of_c.[0] <- c; str_of_c;
    in
            
    let trans_line l =
        begin
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
        end
    in

    let tex_text text =
        List.iter (fun s -> if (env.in_subsup=false) then
                            begin
                                if (s<>"\n") then
                                    out_str ((trans_line s)^" ")
                                else
                                    nl ();
                            end
                            else if not env.in_math then
                            begin
                                out_str ((trans_line s)^" ")
                            end
                            else
                            begin
                                out_str (trans_line s);
                            end;

                   ) text
    in

    (*
    ** Convert a text structure element to a string
    *)

    let str_of_text tln translate =
        let str = ref "" in

        List.iter (fun t ->
            match t.s_content with
            | S_Text t -> List.iter (fun s ->
                                        str := !str ^ " " ^ s;
                          ) t;
            | S_Attribute -> ();
            
            | _ -> syntax_error
                    "str_of_text: not a S_Text element";
        ) tln;
        if translate 
            then trans_line (!str) 
        else
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


    let symbol sli =
        let str = ref "" in
        List.iter ( fun d -> str := !str ^ (str_of_struc d)
               ) sli; 
        let str' = Str.global_replace (Str.regexp " ") "" !str in
        if env.in_math 
            then out_str (" \\"^ str' ^" ")
            else out_str (" $\\"^ str' ^"$ ");
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
            | T_Superscript 
            | T_Subscript ->    out_str "}$ ";
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
                    if (doc_color env) 
                        then out_str ("\\textcolor{red}{mutable} ")
                        else out_str ("{\\bf mutable} ");

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
    ** Prepare a link. The 'ln' argument must be a list of
    ** generic text elements.
    *)

    let link ln =
        let str = ref "" in
        List.iter ( fun d -> str := !str ^ (str_of_struc d)) ln;
        str := trans_line !str;
        out_str ("\\fbox{\\sc\\small "^ !str ^"} ")
    in

    
    (*
    ** Print a table of content starting at a specific section.
    *)
    let print_toc sec =
      if (doc_toc env) = true then
      begin
        let orig_chan = !oc_chan in
        if (env.toc_file) then 
        begin
            (*
            ** Open the TOC file.
            *)
            oc_chan := open_out "toc.tex";
        end;
        
        let depth = ref 1 in
        nl (); out_str "\\vfill \\eject"; nl ();
        out_str "\\sectionmark{Table of Content}"; nl();
        out_str "\\vbox{\\hsize \\textwidth"; ind_incr (); nl ();
        out_str "\\hrule"; nl ();
        out_str "\\vbox{\\rule[-.55em]{0pt}{1.7em}\\strut \\vrule \\quad \\hfill"; 
        ind_incr (); nl ();
                                        
        out_str (
                    (if (doc_color env) =true then
                        "\\textcolor{blue}"
                     else "")^
                     "{\\bf Table of Content "
                ); 

        out_str "} \\hfill"; nl ();
        out_str "\\quad \\vrule}\\hrule}"; 
        ind_decr (); nl (); ind_decr (); nl ();
        out_str "\\vskip \\baselineskip"; nl ();


        let rec iter s =
            if (s.sec_name <> "") then
            begin
                out_str (
                               "\\vbox{\\sc "^
                               s.sec_name^
                               "\\quad\\dotfill\\quad\\pageref{"^
                               (file_name s.sec_name s.sec_num)^
                               "}}"
                           
                ); nl ();
            end;

            if (s.sec_childs <> []) then
            begin
#if 0
                out_str "\\vskip .2\\baselineskip"; nl();
#endif
                out_str (
                            "{\\leftskip"^
                            (string_of_int (!depth*10))^
                            "pt "
                );
                incr depth;
                List.iter iter s.sec_childs;
                out_str "}"; nl ();
#if 0
                out_str "\\vskip .2\\baselineskip"; nl();
#endif
                decr depth;
            end;
            if (s.sec_type = "S1") then
            begin
                out_str "\\vskip .4\\baselineskip"; nl();
            end;
        in
        if (sec.sec_childs <> []) then
        begin
            List.iter iter sec.sec_childs;
        end;
        if (env.toc_file) then 
        begin
            (*
            ** Close the TOC file, restore oc_chan.
            *)
            close_out !oc_chan;
            oc_chan := orig_chan;
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
                (file_name sec.sec_name sec.sec_num)^
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

    let sty () =
        let oc = open_out (env.doc_name^".sty") in
        let out_str = output_string oc in
        out_str "\\def\\ps@headings{\n"; nl();
        out_str "\\def\\@oddhead{}\n"; nl();
        out_str "\\def\\@evenhead{}\n"; nl();
        out_str "\\def\\@oddfoot{\\hss\\setlength{\\fboxrule}{1pt}\n"; nl();
        out_str "\\vbox{\\hsize\\textwidth\\hrule\\vskip 3pt\n"; nl();
        out_str "\\hbox to\\textwidth{\\small\\rightmark\\hfil\\thepage}}\\hss}\n";
        out_str "\\def\\@evenfoot{}}\n"; nl();
        out_str "\\def\\rightmark{}\\def\\leftmark{}\n"; nl();
        out_str "\\def\\bothmark{}\n"; nl();
        out_str "\\def\\sectionmark#1{\\def\\rightmark{\\uppercase{#1}}}\n"; nl();
        out_str "\\pagestyle{headings}\n"; nl();
        close_out oc;
        in
 
    let head () =  
        out_str "\\documentclass{report}"; nl ();
        out_str "\usepackage{newcent}"; nl ();
        out_str "\usepackage{pifont}"; nl ();
        out_str "\usepackage[dvips]{color}\n"; nl (); 
        if not (doc_pdf env) 
            then out_str "\\usepackage{graphics}\n"
            else out_str "\\usepackage[pdftex]{graphicx}\n"; 
        nl (); 

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
        if (doc_color env) = true then
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
        out_str ("\\usepackage{"^(env.doc_name)^"}"); nl();
        out_str "\\begin{document}"; nl ();

    
    in

    let tail () =
            out_str "\\end{document}"; nl ();
    in


    (*
    ** Normal text must be put in a paragraph environment. Really here ?
    *)
    let tag_set = ref false in
    let leave_p () =
        if env.in_par && !tag_set then 
        begin
            out_str "}";
            tag_set := false;
        end;
        env.in_par <- false;
        in
    let enter_p () =
        if env.in_par && not !tag_set then 
        begin
            out_str "{";
            tag_set := true;
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
        if (ds.s_content <> S_NL) then env.last_nl <- false;

        (
          match ds.s_content with
            | S_Body -> 
                begin
                    sty ();
                    head ();
                    env.in_par <- true;
                end;
            | S_Empty -> ();
            | S_Text t -> enter_p ();
                          tex_text  t;

            | S_NL -> if not env.in_preform &&
                         not env.last_nl 
                         then 
                      begin
                        (*
                        ** Never more than one newline in one
                        ** box!
                        *)
                        env.last_nl <- true;
                        out_str "\\hskip1pt\\\\"; 
                        if env.in_title then
                        begin
                            nl (); out_str "\\vskip 1\\baselineskip"; 
                        end;
                      end;
                      nl();
            | S_TAB -> if env.in_preform 
                        then out_str "{\\tt\\hskip 2em}";

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
                    | Spec_None -> out_str 
                                     "{$\\triangleright$\\small\\it\\, ";
                end;

#if 0
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
#endif

            | S_Attribute -> enter_p ();
                             parse_attr_start ds.s_attr;

            | S_Example -> 
                begin
                    leave_p ();

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
                                                (if (doc_color env) = true then
                                                    "\\textcolor{blue}"
                                                 else
                                                    "")^
                                                "{\\rule[-.55em]{0pt}{1.7em}"
                                        );
                                        out_str "{\\sc Example}\\quad";
                                        cont_trans dna;
                                        out_str "}";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> ();
                            end; 
                            | [] -> 
                                    out_str (
                                        (if (doc_color env) = true then
                                            "\\textcolor{blue}"
                                         else
                                            "")^
                                        "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                                        "Example}"
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
                    nl (); 
                    if (doc_color env) then
                        out_str (
                        "\\textcolor[rgb]{.58,.76,1.0}{\\strut\\vrule width 5pt#}"^
                        "\\hskip 2em& "
                        )
                    else
                        out_str (
                        "{\\strut\\vrule width 5pt#}"^
                        "\\hskip 2em& "
                        );

                    nl (); out_str "  #\\hfil  \\cr";
                
                    nl (); out_str " &\\cr "; nl ();
                    env.in_example <- true;
                end;

            | S_Preform -> 
                begin
                    if (not env.in_example) then
                    begin
                        leave_p ();
                        nl (); out_str "\\vskip 1\\baselineskip"; nl ();
                        nl (); out_str "\\halign{"; ind_incr ();
                        nl (); 
                        let skip = ((List.length (env.in_list))+1) * 3 in
                        out_str (sprintf "\\hskip%dem#\\hfil\\cr" skip); 
                        nl ();
                        out_str "{\\tt \\hskip 1em "; 
                    end
                    else
                    begin
                        out_str "& {\\tt \\hskip 1em "; 
                    end;

                    env.in_preform <- true;
                end;

            | S_Link -> 
                begin
                    link ds.s_childs;
                    env.ignore_childs <- true; 
                end;


            | S_Ref -> 
                begin
                    env.in_ref <- true;
                    out_str "\\fbox{\\sc\\small ";
                end;

            | S_Label -> 
                begin
                    env.ignore_childs <- true; 
                    let str = ref "" in
                    List.iter ( fun d -> str := !str ^ (str_of_struc d)
                        ) ds.s_childs; 
                    str := file_name !str 0;
                    if not env.in_ref then
                        out_str ("\\label{"^ !str ^"}")
                    else
                        out_str (" {(p. \\pageref{"^ !str ^"})}");
                end;

            | S_Symbol ->
                begin
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
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
#if 0
                        out_str "\\begin{minipage}{.9\\textwidth}\\vbox{"; 
#endif
                        out_str "{";
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
                    leave_p ();
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
#if 0
                        out_str "\\begin{minipage}{.9\\textwidth}\\vbox{"; 
#endif
                        out_str "{";
                        ind_incr (); 
                    end;

                    out_str (
                        "\\begin{itemize}"
                    ); ind_incr (); nl ();
                    env.in_ulist <- env.in_ulist + 1;
                    env.in_list <- [U_List] @ env.in_list;
                end;

            | S_ArgList ->
                begin
                    leave_p ();
                    nl ();
                    if (env.in_ulist + env.in_olist) = 0 then   
                    begin
                        out_str "\\vskip 1\\baselineskip"; nl ();
                    end
                    else
                    begin
                        out_str "\\vskip 0.4\\baselineskip"; nl ();
                    end;

                    (*
                    ** The first child can be an optional title name.
                    *)
                    let name = ref "" in

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        name := (str_of_text dna.s_childs true);
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> ();
                            end; 
                        | [] -> ();
                    
                    );

                    if !name <> "" && !name <> " " then
                    begin
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



                        out_str ( 
                            "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                            !name^ 
                            " }"
                            );

                        out_str "\\qquad\\vrule}"; 
                        ind_decr (); nl (); 
                        out_str "\\hrule}";
                        ind_decr (); nl ();
                        out_str "}";
                        ind_decr (); nl ();
                                        
                        out_str "\\nobreak"; 
                        out_str "\\vskip \\baselineskip\\nobreak"; nl ();
                    end;

                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  

                    nl (); out_str "\\begin{description}";
                    ind_incr (); nl ();         
                    env.in_list <- [A_List false] @ env.in_list;
                end;

            | S_LitList ->
                begin
                    leave_p ();
                    nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();

                    (*
                    ** The first child can be an optional title name.
                    *)
                    let name = ref "" in

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        name := (str_of_text dna.s_childs true);
                                        env.ignore_head <- true;    
                                    end;
                                | _ -> ();
                            end; 
                        | [] -> ();
                    
                    );

                    if !name <> "" && !name <> " " then
                    begin
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



                        out_str ( 
                            "{\\rule[-.55em]{0pt}{1.7em}\\sc "^
                            !name^ 
                            " }"
                            );

                        out_str "\\qquad\\vrule}"; 
                        ind_decr (); nl (); 
                        out_str "\\hrule}";
                        ind_decr (); nl ();
                        out_str "}";
                        ind_decr (); nl ();
                                        
                        out_str "\\nobreak"; 
                        out_str "\\vskip \\baselineskip\\nobreak"; nl ();
                    end;

                    out_str "\\begin{list}{}{}\\item{";
                    out_str "\\parindent 0pt "; ind_incr (); nl ();  

                    nl (); out_str "\\begin{description}";
                    ind_incr (); nl ();           
                    env.in_list <- [L_List] @ env.in_list;
                end;


            | S_List_Item -> 
                begin
                    match (List.hd env.in_list) with
                    | L_List 
                    | A_List _ ->
                        begin
                            nl (); out_str "\\item[{"; 
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
                                        format_text dna.s_childs;
                                        env.ignore_head <- true;    
                                    end;
                                    | _ -> syntax_error "option list item without name";
                                end; 
                                | [] -> syntax_error "option list item without name";
                    
                            );
                            out_str "}]"; ind_incr ();
                            nl (); out_str "{\\hfill\\\\}"; nl ();   
                            env.in_par <- true;
                        end;
                        
                    | O_List | U_List ->
                        begin
                            out_str ( 
                                "\\item "
                            ); 
                            env.in_par <- true;
                        end;
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
                    leave_p ();

                    out_str "\\begin{center}"; nl ();
                    out_str ( 
                            "\\vbox{\\hrule\\vskip2pt\\hrule\\bf\\Large"^
                            "\\vskip 2\\baselineskip"
                    ); nl ();
                    env.in_title <- true;  
                end;

            | S_TOC -> ();


            | S_S1 -> 
                begin
                    leave_p ();
                    if not (doc_sec_break env Doc_multi_s1)  then
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
                    let sname = ref "" in
                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        sname := pname;
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="S1";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        
                                        out_str (
                                                (if (doc_color env) = true then
                                                    "\\textcolor{blue}"
                                                 else
                                                    "")^
                                                "{\\bf "
                                        ); 
                                        env.special_name <- true;
                                        cont_trans dna;
                                        env.special_name <- false;

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
                    out_str ("\\sectionmark{"^(!sname)^"}"); nl();
                    env.in_par <- true;
                end;

            | S_S2 -> 
                begin
                    leave_p ();
                    if not (doc_sec_break env Doc_multi_s2) then
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
                    let sname = ref "" in

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

                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        sname := pname;
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        new_sec env sec;
                                        
                                        if (doc_color env) = true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\bf ";

                                        env.special_name <- true;
                                        cont_trans dna;
                                        env.special_name <- false;

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
                                            (if (doc_color env) = true then
                                                "\\textcolor[rgb]{.0,.7,.0}"
                                             else
                                                "")^
                                            "{\\it "^
                                            sec.sec_name^
                                            "}"
                                        );
                                        sname := sec.sec_name;
                              | None -> ();
                        end;
                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    if (!sname <> "") then
                    begin
                        out_str ("\\sectionmark{"^(!sname)^"}"); nl();
                    end;
                    env.in_par <- true;
                end;

            | S_S3 -> 
                begin
                    leave_p ();
                    if not (doc_sec_break env Doc_multi_s3) then
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

                    let sname = ref "" in

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
                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        sname := pname;
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        new_sec env sec;

                                        if (doc_color env) = true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\bf ";

                                        env.special_name <- true;
                                        cont_trans dna;    
                                        env.special_name <- false;
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
                                        (if (doc_color env) = true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\it "^
                                        sec.sec_name^
                                        "}"
                                    );
                                    sname := sec.sec_name;
                              | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                    out_str (
                                        (if (doc_color env) =true then
                                            "\\textcolor[rgb]{.7,.7,.0}"
                                         else
                                            "")^
                                        "{\\it "^
                                        sec.sec_name^
                                        "}"
                                    );
                                    sname := sec.sec_name;
                              | None -> ();
                        end;
                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    if (!sname <> "") then
                    begin
                        out_str ("\\sectionmark{"^(!sname)^"}"); nl();
                    end;
                    env.in_par <- true;
                end;

            | S_S4 -> 
                begin
                    leave_p ();

                    out_str "\\vskip .5\\baselineskip "; nl ();
                    out_str "{\\parindent 0pt \\vbox{\\bf ";

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
                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        new_sec env sec;

                                        env.special_name <- true;
                                        List.iter cont_trans dna.s_childs;
                                        env.special_name <- false;

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
                  
                    env.in_par <- true;
                end;

            | S_MP -> 
                begin
                    leave_p ();
                    if not (doc_sec_break env Doc_multi_mp) then
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

                    let sname = ref "" in

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
                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        sname := pname;
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        new_sec env sec;

                                        if (doc_color env) = true then
                                            out_str "\\textcolor{blue}";
                                        out_str "{\\bf ";

                                        env.special_name <- true;
                                        cont_trans dna;    
                                        env.special_name <- false;

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
                                        (if (doc_color env) = true then
                                            "\\textcolor[rgb]{.0,.7,.0}"
                                         else
                                            "")^
                                        "{\\it "^
                                        sec.sec_name^
                                        "}"
                                    );
                                    sname := sec.sec_name;
                              | None -> ();
                        end
                        else if (env.cur_s1 <> None) then
                        begin
                            match env.cur_s1 with
                              | Some sec -> 
                                    out_str (
                                        (if (doc_color env) =true then
                                            "\\textcolor[rgb]{.7,.7,.0}"
                                         else
                                            "")^
                                        "{\\it "^
                                        sec.sec_name^
                                        "}"
                                    );
                                    sname := sec.sec_name;
                              | None -> ();
                        end;
                    );
                    out_str "\\quad \\vrule}\\hrule}"; 
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    if (!sname <> "") then
                    begin
                        out_str ("\\sectionmark{"^(!sname)^"}"); nl();
                    end;
                    env.in_par <- true;
                end;

            | S_MP_paragr -> 
                begin
                    leave_p ();

                    out_str "\\vskip .5\\baselineskip "; nl ();
                    out_str "{\\parindent 0pt \\vbox{\\bf ";

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
                                        let pname = (str_of_text dna.s_childs true)
                                        in
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;

                                        new_sec env sec;

                                        env.special_name <- true;
                                        List.iter cont_trans dna.s_childs;
                                        env.special_name <- false;

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
                    env.in_par <- true;
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
                    if (doc_color env)
                        then out_str "\\textcolor{red}{mutable }"
                        else out_str "{\\bf mutable }";
                end;
            | S_Virtual -> 
                begin
                    if (doc_color env)
                        then out_str "\\textcolor{red}{virtual }"
                        else out_str "{\\bf virtual }";
                end;
            | S_Private -> 
                begin
                    if (doc_color env)
                        then out_str "\\textcolor{red}{private }"
                        else out_str "{\\bf private }";
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

                    if (env.in_interface = false) then
                    begin
                        out_str "\\vskip 0.5\\baselineskip"; nl ();
                    end;
                                        
                    nl (); out_str "\\halign{"; ind_incr ();
                    nl (); 
                    if (doc_color env) then
                        out_str (
                            "\\textcolor{red}{\\strut\\vrule width 5pt"^
                            "\\qquad#}\\tabskip=0pt plus 1fil& "
                            )
                    else
                        out_str (
                            "{\\strut\\vrule width 5pt"^
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
                        | _ -> syntax_error "invalid class method";
                    end;
                end;
                
            | S_ML_Object_Interface -> 
                begin
                    env.special_depth <- env.special_depth - 1;
                    
                                (
                                    match env.in_special with
                                    | Spec_ML_Class_Int ci -> ();
                                    | _ -> 
Db.Pr.ss 0 "spec" 
(
    match env.in_special with
    | Spec_ML_Fun_Int _ -> "Spec_ML_Fun_Int";
    | Spec_ML_Val_Int _ -> "Spec_ML_Val_Int";
    | Spec_ML_Ext_Int _ -> "Spec_ML_Ext_Int";
    | Spec_ML_Type_Int _ -> "Spec_ML_Type_Int";
    | Spec_ML_Method_Int _ -> "Spec_ML_Method_Int";
    | Spec_ML_Object_Int _ -> "Spec_ML_Object_Int";
    | Spec_ML_Module_Int _ -> "Spec_ML_Module_Int";
    | Spec_ML_Class_Int _ -> "Spec_ML_Class_Int";
    | Spec_C_Hdr_Int _ -> "Spec_C_Hdr_Int";
    | Spec_C_Fun_Int _ -> "Spec_C_Fun_Int";
    | Spec_C_Var_Int _ -> "Spec_C_Var_Int";
    | Spec_C_Type_Int _ -> "Spec_C_Type_Int";
    | Spec_Table _ -> "Spec_Table";
    | Spec_DataFormat _ -> "Spec_DataFormat";
    | Spec_None -> "Spec_None";

);
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
                                                (if (doc_color env) = true then
                                                    "\\textcolor{red}"
                                                 else
                                                    "")^
                                                "{\\rule[-.55em]{0pt}{1.7em}"^
                                                "\\sc Programming Interface \\bf"
                                        );
                                        cont_trans dna;
                                        out_str "}";
                                        env.ignore_head <- true;
                                    end;
                                | _ -> 
                                        out_str ( 
                                           (if (doc_color env) = true then
                                                "\\textcolor{red}"
                                            else
                                                "")^
                                            "{\\rule[-.55em]{0pt}{1.7em}"^
                                            "\\sc Programming Interface}"
                                        );
                                    
                            end; 
                            | [] -> 
                                    out_str (
                                        (if (doc_color env) = true then
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
                    nl (); 
                    if (doc_color env) then
                        out_str (
                            "\\textcolor{red}{\\strut\\vrule width 5pt"^
                            "#}\\tabskip=0pt plus 1fil& "
                           )
                    else
                        out_str (
                            "{\\strut\\vrule width 5pt"^
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
                    enter_p ();
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
                            out_str "{\\sl ";
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
                    leave_p (); nl();
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
                                        (str_of_text dna.s_childs false);
                                    end;
                                | _ -> "";
                           end; 
                           | [] -> "";
                         in
                    out_str ("\\def\\eqnum{\\mathbf"^ eqnum ^ "}");
                    out_str "\\vskip \\baselineskip"; nl ();
                    out_str "\\begin{center}\\begin{minipage}{.9\\textwidth}";
                    out_str "\\vbox{$$\\mathrm{";
                    ind_incr();
                end;

            | S_Math_Fun ->
                begin
                    out_str "\\,\\,{\\mathbf{ ";
                    (*
                    ** The first child must be the function name
                    *)
                    let fname = 
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                        env.ignore_head <- true;
                                        (str_of_text dna.s_childs true);
                                    end;
                                | _ -> "";
                           end; 
                           | [] -> "";
                         in
                    out_str (fname^"}{");
                end;

            | S_Figure ->
                begin
                    leave_p ();
                    nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    out_str "\\begin{center}\\begin{minipage}{.9\\textwidth}\\vbox{";
                    ind_incr();
                    env.in_par <- true;  
                    env.in_figure <- true;
                end;

            | S_Image ->
                begin   
                    leave_p ();
                    if not env.in_figure then
                    begin
                        out_str "\\vskip 2\\baselineskip"; nl();
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
                                    str := !str ^ (str_of_text dna.s_childs false);
                                end;
                                | _ -> syntax_error "Image without filename";
                            end; 
                            | [] -> syntax_error "Image without filename";
                    );
                    let str' = Str.global_replace (Str.regexp " ") ""
                                                  !str in

                    (*
                    ** EPS file ? Convert it to PDF format in the
                    ** case of the pdflatex target. If it's not eps,
                    ** replace the name with eps extension!!! Don't know
                    ** to handle other types. Print a message.
                    *)
                    if not (Filename.check_suffix str' "eps") then
                    begin
                        str := (Filename.chop_extension str')^".eps";
                        warn (sprintf "Unknown image type. Name replaced with eps suffix: %s" !str);
                    end
                    else
                        str := str';

                    if (doc_pdf env) &&
                       (Filename.check_suffix !str "eps") then
                    begin
                            let stat = Unix.system ("epstopdf "^ !str) in
                            (match stat with
                                | Unix.WEXITED s -> if s <> 0 then
                                                syntax_error "epstopdf failed";
                                | _ -> ();
                            );
                            let str'' = Filename.chop_extension !str in
                            out_str ("\\vbox{\\includegraphics{" ^ str'' ^"}}");
                    end
                    else
                        out_str ("\\vbox{\\includegraphics{" ^ !str ^"}}"); 

                    nl ();
                    if not env.in_figure then
                    begin
                        out_str "\\vskip 2\\baselineskip"; nl();
                    end;
               end;
            | S_Html_Source -> 
                begin
                    env.ignore_childs <- true;
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
                            out_str "\\,$\\triangleleft$}";
                        end;            
                end;


            | S_Attribute -> parse_attr_end ds.s_attr;

            | S_Example -> 
                begin
                    env.in_example <- false;
                    out_str "}";
                    ind_decr (); nl (); ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                     
                    env.in_par <- true;
                end;

            | S_Preform -> 
                begin
                    env.in_preform <- false;
                    if (not env.in_example) then
                    begin
                        out_str "\\hfil} \\cr "; nl ();
                        out_str "}"; 
                        ind_decr (); nl (); ind_decr (); nl ();
                        out_str "\\vskip 1\\baselineskip"; nl ();
                        env.in_par <- true;
                        env.last_nl <- true;    (* NL after PRE not allowed *)
                    end
                    else
                    begin
                        out_str "\\hfil} \\cr "; nl ();
                        env.in_par <- true;
                        env.last_nl <- true;    (* NL after PRE not allowed *)
                    end;
                end;

            | S_OList ->    
                begin
                    out_str "\\end{enumerate}"; 
                    ind_decr (); nl ();
                    env.in_olist <- env.in_olist - 1;
                    env.in_list <- List.tl env.in_list;

                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
#if 0
                        out_str "}\\end{minipage}"; ind_decr ();
#endif
                        out_str "}"; ind_decr();
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end;
                    env.in_par <- true;
                end;

            | S_UList ->    
                begin
                    out_str "\\end{itemize}"; 
                    ind_decr (); nl ();
                    env.in_ulist <- env.in_ulist - 1;

                    env.in_list <- List.tl env.in_list;
                    
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
#if 0
                        out_str "}\\end{minipage}"; ind_decr ();
#endif
                        out_str "}"; ind_decr();
                        nl (); out_str "\\vskip \\baselineskip"; nl ();
                    end;
                    env.in_par <- true;
                end;

            | S_ArgList ->
                begin
                    nl (); out_str "\\end{description}"; ind_decr (); nl ();
                    env.in_list <- List.tl env.in_list;

                    out_str "}\\end{list}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    nl (); out_str "\\vskip \\baselineskip"; nl ();
                    env.in_par <- true;
                end;

            | S_LitList ->
                begin
                    nl (); out_str "\\end{description}"; ind_decr (); nl ();
                    env.in_list <- List.tl env.in_list;

                    out_str "}\\end{list}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    nl (); out_str "\\vskip \\baselineskip"; nl ();
                    env.in_par <- true;
                end;

            | S_List_Item -> 
                begin
                    leave_p();
                    match (List.hd env.in_list) with
                    | A_List _ 
                    | L_List ->
                        begin
                            ind_decr (); nl ();
                        end;
                    | O_List | U_List ->
                        begin
                            nl ();
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
                    | Spec_DataFormat tb -> d();
                    | Spec_None -> 
                        if not env.special_name then
                        begin
                            out_str "}";
                        end;            
                end;

            | S_Value -> env.ignore_childs <- false;    (* internally handled *)

            | S_Link -> out_str "";


            | S_Ref -> out_str "}";
                       env.in_ref <- false;
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
                        if (List.length r) <> cols then
                            syntax_error "table with different columns";

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
                            (if (doc_color env) = true then
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
                                with _ -> syntax_error "invalid value specifier"
                                );
                        | _ -> (); 
                        ) de.s_childs;
                    ) th;
                if !cellwidth = 0 then syntax_error "data format without cellwidth specifier";

        


                let colwidth = (100 * !cellwidth) / 150 in
                let tablewidth = colwidth * thn in

                let rec tb_head col =
                    if col > 1 then
                        ("|p{"^
                        (string_of_int colwidth)^
                        "em}"^
                        (tb_head (col-1)))
                    else if col > 0 then
                        ("|p{"^
                        (string_of_int colwidth)^
                        "em}|"^
                        (tb_head (col-1)))
                    else
                        ""
                in     

                nl (); out_str "\\vskip 2\\baselineskip"; nl ();
                out_str "\\begin{center}\\vbox{"; ind_incr (); nl ();

                if (tn <> []) then
                begin
                    out_str "{\\bf ";
                    List.iter (fun dn ->
                        List.iter cont_trans dn.s_childs;
                        ) tn;
                    out_str "}\\\\";
                    out_str "\\vskip0.5em"; nl ();
                end;
                
                out_str "\\begin{tabular}"; 

                out_str ( 
                            "{"^
                            (tb_head thn)^
                            "}\\hline"
                    );

                ind_incr (); nl ();

                let ci = ref 1 in
                let cn = List.length th in
                List.iter (fun de ->
                       out_str "{\\bf ";
                       List.iter cont_trans de.s_childs;
                       out_str "}";
                       if (!ci < cn) then
                            out_str "&";
                       incr ci;
                       nl ();
                    ) th;
                out_str "\\\\\\hline\\hline";
                nl ();


                List.iter (fun r ->
                        let ci = ref 1 in
                        let rl = List.length r in
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
                                        syntax_error "inavlid value specifier"
                                    );
                                | _ -> (); 
                                ) c.s_childs;
                            if !width = 0 then 
                                syntax_error "data entry without width specifier";

                            if !width > 1 then
                            begin
                                if !ci = 1 then
                                out_str (sprintf "\\multicolumn{%d}{|l|}{"
                                                      !width)
                                else
                                out_str (sprintf "\\multicolumn{%d}{l|}{"
                                                      !width);
                            end;

                            List.iter cont_trans c.s_childs;

                            if !width > 1
                                then out_str "}";

                            if (!ci < rl) then
                                out_str "&";

                            incr ci;
                        ) r;          

                        out_str "\\\\\\hline";
                        nl ();
                    ) tr;

                out_str "\\end{tabular}}"; 
                ind_decr (); nl (); 
                out_str "\\end{center}"; ind_decr (); nl ();
                out_str "\\vskip 2\\baselineskip"; nl ();


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
                    leave_p();
                    if (doc_toc env) &&
                       (env.toc.sec_childs <> []) then
                    begin

                        (*
                        ** Print the table of content for the whole
                        ** document.
                        *)

                        print_toc env.toc;

                    end;
                    ind_decr ();

                    nl (); tail ();

                    close_out !oc_chan
                end;


            | S_Title -> 
                begin
                    out_str "\\vskip 2\\baselineskip"; nl ();
                    out_str "\\hrule\\vskip2pt\\hrule}"; nl ();
                    out_str "\\end{center}"; nl ();
                    out_str "\\thispagestyle{empty}"; nl ();
                    out_str "\\vfill \\eject"; nl ();

                    if (doc_toc env) then
                    begin
                        (*
                        ** The TOC is written in a separated file.
                        *)
                        out_str "\\pagenumbering{roman}"; nl();
                        out_str "\\setcounter{page}{1}"; nl ();
                        out_str "\\include{toc}";
                        out_str "\\pagenumbering{arabic}"; nl();
                        env.toc_file <- true;
                    end;

                    out_str "\\setcounter{page}{1}"; nl ();
                    env.in_title <- false;


                    env.in_par <- true;
                end;
             

            | S_TOC -> ();


            | S_S1 -> 
                begin
                    leave_p ();
                    close_sec env;
                    env.cur_s1 <- None;
                end;


            | S_S2 -> 
                begin
                    leave_p ();
                    close_sec env;
                    env.cur_s2 <- None ;
                end;


            | S_S3 -> 
                begin
                    leave_p ();
                    close_sec env;
                    env.cur_s3 <- None ;
                end;


            | S_S4 -> 
                begin
                    leave_p ();
                    out_str "}\\end{list}}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    close_sec env;
                    env.cur_s4 <- None;
                end;

            | S_MP -> 
                begin
                    leave_p ();
                    close_sec env;
                    env.cur_mp <- None ;
                end;

            | S_MP_paragr -> 
                begin
                    leave_p ();
                    out_str "}\\end{list}}"; ind_decr (); nl ();
                    out_str "\\vskip .5\\baselineskip"; nl ();
                    close_sec env;
                    env.cur_mp_paragr <- None;
                end;

            | S_Math ->
                begin
                    ind_decr ();
                    out_str "}\\quad\\quad\\eqnum$$}\\end{minipage}\\end{center}"; nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    env.in_par <- true;
                    env.in_math <- false;
                end;
            | S_Math_Fun ->
                begin
                    out_str "\\,\\,}}";
                end;

            | S_Figure -> 
                begin
                    
                    leave_p ();
                    ind_decr();
                    out_str "}\\end{minipage}\\end{center}"; nl ();
                    out_str "\\vskip \\baselineskip"; nl ();
                    env.in_par <- true;                    
                    env.in_figure <- false;
                end;

            | S_Image -> 
                begin
                    env.in_par <- true;                    
                end;

            | S_Html_Source -> ();
            | S_Tex_Source -> ();


            | S_Mutable -> ();
            | S_Private -> ();
            | S_Virtual -> ();

            | S_ML_Fun_Interface -> 
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
                                (if (doc_color env)
                                        then "\\textcolor{red}" 
                                        else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                        then "\\textcolor{red}" 
                                        else "")
                                ^"{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill & # \\hfil & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end
                    else
                    begin
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill & # \\hfil & \\qquad # \\hfill \\cr"
                        ); nl ();
                    end;
                    
                    if (env.in_class = Spec_None &&
                        env.in_module = false &&
                        env.in_interface = true) then
                    begin
                        out_str "&&&&& \\cr"; nl ();
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

                                    List.iter 
                                    ( fun na ->
                                          match na.s_content with
                                          | S_Text t -> format_arg t true;
                                          | S_Comment -> ();
                                          | _ -> cont_trans na;
                        
                                    ) r.s_childs;

                                    if (!i < n) then
                                    begin
                                        out_str "$*$ & & & ";
                                    end
                                    else
                                    begin
                                        out_str " & $]=$ & {\\bf ";  
                                        List.iter cont_trans 
                                                (List.hd fname).s_childs;
                                        out_str "} & ";
                                    end;

                                    
                                    if (!i < n) 
                                        then out_str "& \\cr"; nl ();
                                    
                                    incr i;
                                  ) rargs;

                    end;

                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if !i > 1 then
                                    begin
                                        out_str "& & & & ";
                                    end;
                            
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
                

            | S_ML_Val_Interface -> 
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
                        | Spec_ML_Val_Int vi -> vi;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let vname = vi.ml_val_name in
                    let args = vi.ml_val_args in
                    let curried = vi.ml_val_curried in
                    let comm = vi.ml_val_comm in
                    
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
                                (if (doc_color env)
                                        then "\\textcolor{red}" 
                                        else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                        then "\\textcolor{red}" 
                                        else "")
                                ^"{\\strut\\vrule width 5pt"^
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


                    if (doc_color env) 
                        then out_str "\\textcolor{red}{val} "
                        else out_str "{\\bf val} ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"mutable }"
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

            | S_ML_Ext_Interface -> 
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

                   if (vname = []) then
                        syntax_error "External interface without name";

                    if (comm <> []) then
                    begin
                        nl (); 
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str (
                                "\\halign{"^
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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


                    if (doc_color env) 
                        then out_str "\\textcolor{red}{external} "
                        else out_str "{\\bf external} ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"mutable }"
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

                    out_str " & "; nl ();
                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"mutable }"
                                    );
                                end;
                               | _ -> 
                                begin
                                    out_str "{\\bf ";
                                    cont_trans ds;
                                    out_str "}";
                                end;
                              ) (List.hd extfun).s_childs;
                    
                    out_str " & \\cr"; nl ();
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

            | S_ML_Module_Interface ->
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

            | S_ML_Class_Interface ->
                begin

                    env.special_depth <- env.special_depth - 1;

                    
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
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# \\hfil & "^
                                "# \\hfil \\cr"
                    ); nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "&&\\cr"; nl ();
                    end;

                    if (doc_color env) 
                        then out_str "\\textcolor{red}{class} & {\\bf "
                        else out_str "{\\bf class} & {\\bf ";

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
                            "& "^
                            (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                            ^"object} & \\cr"
                        ); nl ();
                        
                        
                        List.iter (fun i ->
                                    out_str "&\\vbox{";
                                    cont_trans i;
                                    out_str "}&\\cr"; nl ();
                                    out_str "&&\\cr";
                                    out_str "\\noalign{\\vglue -1pt}"; nl();
                                  ) oargs;
                        
                        if (doc_color env) 
                            then out_str " & \\textcolor{red}{end} & \\cr"
                            else out_str " & {\\bf end} & \\cr";
                        nl ();                        
                        
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
                        | Spec_ML_Method_Int mi -> mi;
                        | _ -> syntax_error "Invalid in_special";
                    in 
                    let vname = mi.ml_meth_name in
                    let args = mi.ml_meth_args in

                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (vname = []) then
                        syntax_error "Method interface without name";


                    nl (); out_str "\\halign{"; ind_incr ();
                    if (env.in_class = Spec_None &&
                        env.in_module = false) then
                    begin
                        out_str (
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                    
                    if (doc_color env) 
                        then out_str "\\textcolor{red}{method} "
                        else out_str "{\\bf method} ";

                    List.iter (fun ds ->
                               match ds.s_content with
                               | S_Private -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"private }"
                                    );
                                end;
                               | S_Virtual -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"virtual }"
                                    );
                                end;
                               | S_Mutable -> 
                                begin
                                    out_str (
                                        (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                        ^"mutable }"
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

            | S_ML_Type_Interface  
            | S_ML_Struc_Interface 
            | S_ML_Exc_Interface ->
                begin
                    if (env.in_module = true) then
                    begin
                        out_str "&\\vbox{"; ind_incr (); 
                    end;

                    let tp = 
                        match env.in_special with
                        | Spec_ML_Type_Int tp -> tp;
                        | _ -> syntax_error "Inavlid in_special";
                    in 
                    let tname = tp.ml_type_name in
                    let args = tp.ml_type_args in
                    let tp_type = tp.ml_type_type in
                    let comm = tp.ml_type_comm in
                    
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
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                                then "\\textcolor{red}" 
                                                else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                        out_str (
                                 (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                ^"type} & {\\bf ")
                    else
                        out_str (
                                 (if (doc_color env)
                                                then "\\textcolor{red}{" 
                                                else "{\\bf ")
                                 ^"exception} & {\\bf ");

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
                                                (if (doc_color env)
                                                    then "\\textcolor{red}{" 
                                                    else "{\\bf ")^
                                                    "mutable }"
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
                    let vi = 
                        match env.in_special with
                        | Spec_C_Var_Int vi -> vi;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let name = vi.c_var_name in
                    let args = vi.c_var_args in
                    let comm = vi.c_var_comm in

                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (name = []) then
                        syntax_error "C-Variable interface without name";
                    if (args = []) then
                        syntax_error "C-Variable interface without name";
                    
                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;
                    
                    nl (); out_str "\\halign{"; ind_incr ();
                    out_str (
                                (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                                ^"{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & # & #\\cr"
                    ); nl ();

                    if (env.in_interface = true) then
                    begin
                        out_str "&&\\cr"; nl ();
                    end;

                    out_str "&";

                    let a = List.hd args in
                    List.iter
                        ( fun na ->
                            match na.s_content with
                            | S_Comment -> ();
                            | _ -> cont_trans na
                        )a.s_childs;
                    out_str " & {\\bf "; 

                    List.iter cont_trans  
                              (List.hd name).s_childs;

                    out_str "};\\cr"; nl();
                    out_str "}"; ind_decr ();

                    if (env.in_interface = false) then
                    begin
                        nl (); out_str "\\vskip .5\\baselineskip"; 
                    end;

                end;


            | S_C_Fun_Interface -> 
                begin

                    let fi = 
                        match env.in_special with
                        | Spec_C_Fun_Int fi -> fi;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let rargs = fi.c_fun_retargs in
                    let fname = fi.c_fun_name in
                    let args = fi.c_fun_args in
                    let comm = fi.c_fun_comm in

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
                                (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                                ^"{\\strut\\vrule width 5pt"^
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

                    if (hname = []) then
                        syntax_error "C-Hdr interface without name";
                    
                    
                    if (env.in_class = Spec_None) then
                    begin
                        if (env.in_interface = false) then
                        begin
                            nl (); out_str "\\vskip .5\\baselineskip"; 
                        end;
                        nl (); out_str "\\halign{"; ind_incr ();
                        out_str (
                                (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                                ^"{\\strut\\vrule width 5pt"^
                                "\\hskip 20pt \\hfill #} & "^
                                "# & "^ 
                                "#\\hfill \\cr"
                        ); nl ();
                        if (env.in_interface = true) then
                        begin
                            out_str "&&\\cr"; nl ();
                        end;

                        if (doc_color env) 
                            then out_str "\\textcolor{red}{${\\rm\\#}$include} "
                            else out_str "{\\bf${\\rm\\#}$include} ";

                        out_str "&$<$ {\\bf ";  

                        List.iter cont_trans 
                                  (List.hd hname).s_childs;
                        out_str "}$>$& \\cr";
                        out_str "}"; ind_decr (); 
                        if (env.in_interface = false) then
                        begin
                            nl (); out_str "\\vskip .5\\baselineskip"; 
                        end;
                    end
                    else
                    begin
                        (*
                        ** Special case: class inherit!!!
                        *)
                        nl (); out_str "\\halign{"; ind_incr ();
                        out_str (
                                "\\hskip 20pt \\hfill # & "^
                                "# \\hfil & "^
                                "# & "^ 
                                "#\\hfill \\cr"
                        ); nl ();
                    
                        if (doc_color env) 
                            then out_str "\\textcolor{red}{inherit} "
                            else out_str "{\\bf inherit} ";

                        out_str "& {\\bf ";  

                        List.iter cont_trans 
                                  (List.hd hname).s_childs;
                        out_str "}& \\cr";
                        out_str "}"; ind_decr (); 
                    end;

                    

                end;

            | S_C_Type_Interface 
            | S_C_Struc_Interface ->
                begin
                    if (env.in_module = true) then
                    begin
                        out_str "&\\vbox{"; ind_incr (); 
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

                    if (tname = []) then
                        syntax_error "C type/structure interface without name";

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
                                (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                                (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                                ^"{\\strut\\vrule width 5pt"^
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
                    

                    if (doc_color env) then
                    (match tp_type with
                        | Type_List ->
                            out_str "\\textcolor{red}{typedef} & {\\bf ";
                        | Type_Structure ->
                            out_str "\\textcolor{red}{struct} & {\\bf ";
                        | _ -> syntax_error "invalid C struct/type";
                    ) else
                    (match tp_type with
                        | Type_List ->
                            out_str "{\\bf typedef} & {\\bf ";
                        | Type_Structure ->
                            out_str "{\\bf struct} & {\\bf ";
                        | _ -> syntax_error "invalid C struct/type";
                    );

                    
                    List.iter cont_trans 
                               (List.hd tname).s_childs;

                    (match tp_type with
                        | Type_List ->
                            out_str "} & ";
                        | Type_Structure ->
                            out_str "} $\\{$ & ";
                        | _ -> syntax_error "invalid C struct/type";
                    );
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
                                        | Type_List      -> out_str " $,$ &";
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
                    else 
                        syntax_error "type without args";

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

            | S_Interface -> 
                begin
                    out_str (
                        "\\halign{"^
                        (if (doc_color env)
                                    then "\\textcolor{red}" 
                                    else "")
                        ^"{\\strut\\vrule width 5pt"^
                        "#}&#\\cr&\\cr}"
                    ); nl ();
                    out_str "}";ind_decr (); nl ();
                    out_str "\\vskip 2\\baselineskip"; nl ();
                    env.in_interface <- false;
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

    try
        cont_trans ds
    with
        Failure s -> syntax_error s
                     
