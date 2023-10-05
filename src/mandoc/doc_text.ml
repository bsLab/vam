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
**    $CREATED:     26.12.2005
**    $MODIFIED:    
**    $VERSION:     1.05
**
**    $INFO:
**
** ASCII Text backend.
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
    mutable dataformat_cols: int;
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
    mutable in_example: bool;
    mutable in_interface: bool;
    mutable in_title: bool;

    (*
    ** ml interfaces
    *)
    mutable in_class: struc_special;
    mutable in_object: struc_special;
    mutable in_module: bool;         
        

    mutable in_olist: int;
    mutable olist_cnt: int ref list;
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
    mutable doc_row     : int;
    mutable doc_rowhight: int;
    

    mutable table_col   : int;
    mutable textwidth   : int;
}

let doc_toc env =
    List.mem Doc_with_toc env.doc_options

let doc_term env =
    List.mem Doc_terminal env.doc_options

let doc_manfile env =
    List.mem Doc_manfile env.doc_options


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
** Transform the structure tree to Text output.
**
** Argument: 
**
**  ds: structure tree
**
*)


let text_of_tree ~ds ~options ~sections =

    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = "main"; sec_type = "Main";
                         sec_num = 0;} 
    in  

    let sec_num = ref 1 in

    let env = {
        doc_options     = [];
        doc_name        = "manual.txt";

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
        olist_cnt       = [];
        in_ulist        = 0;
        in_list        = [];


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
        
        doc_indent      = 0;
        doc_rowhight    = 20;
        doc_colwidth    = 80;
        doc_col         = 0;
        doc_row         = 0;

        table_col       = 0;
        textwidth       = 80;
    } in

    (*
    ** options
    *)
    env.doc_options <- env.doc_options @ options;
    let copyright = ref "" in
    List.iter (fun o ->
                match o with   
                | Doc_Main s -> env.doc_name <- s;
                                doc_head.sec_name <- Filename.chop_extension s;
                | Doc_copyright s -> copyright := s;
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
    
    let fill num c =
        let str = String.create num in
        for i = 0 to num-1
        do
            str.[i] <- c;
        done;
        str
    in

    let head =  
        ""
    in

    let tail =
        let tm = Unix.gmtime (Unix.time ()) in
        "\n\n"^
        (fill (env.doc_colwidth-1) '-')^"\n"^
        (Str.global_replace (Str.regexp "\\\\n") "\n" !copyright)^
        "Created with MANdoc, (C) 2005-2006 BSSLAB Dr. Stefan Bosse\n"^
        (sprintf "%d.%d.%d %d:%d\n" tm.Unix.tm_mday
                                      (tm.Unix.tm_mon+1) 
                                      (tm.Unix.tm_year + 1900)
                                      tm.Unix.tm_hour 
                                      tm.Unix.tm_min)^
        (fill (env.doc_colwidth-1) '-')^"\n"
    in
    


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
    let oc_chan = ref None in
    let oc_name = ref "" in
    let oc_list = ref [] in




    (*
    ** Open a new file. Behaviour depends on option settings. 
    *)

    let oc_open fname sec =
        if (doc_term env) then
        begin
            oc_name := "stdout";
            oc_chan := Some Pervasives.stdout;
            env.doc_indent <- 0; 
            env.doc_col <- 0;
            output_string Pervasives.stdout head;
        end
        else if (doc_manfile env) then
        begin
            oc_name := fname ;
            let oc = open_out !oc_name in
            oc_chan := Some oc;
            oc_list := [!oc_chan,!oc_name,sec] @ !oc_list;
            env.doc_indent <- 0; 
            env.doc_col <- 0;
            output_string oc head; 
        end
        else
        begin
            (*
            ** Single document
            *)

            if (!oc_chan = None) then
            begin
                oc_name := env.doc_name ;
                let oc = open_out !oc_name in
                oc_chan := Some oc;
                oc_list := [!oc_chan,!oc_name,sec] @ !oc_list;
                env.doc_indent <- 0; 
                env.doc_col <- 0;
                output_string oc head;
            end
            else
            begin
                oc_list := [!oc_chan,fname,sec] @ !oc_list;
            end;
        end
    in

    (*
    ** Close a file only with appropiate section specifier
    ** (for example S_Body, S_S1,...).
    *)
    
    let oc_close sec =
        match !oc_list with
        | hd::tl -> 
        begin
            let oc',_,sec' = hd in
            if sec = sec' then
            begin 
                (
                    match oc' with  
                    | Some oc'' -> 
                        output_string oc'' tail;
                        close_out oc'';
                    | None -> ();
                );
                oc_list := List.tl !oc_list;
                oc_chan := match !oc_list with
                           | hd::tl -> let oc',_,_ = hd in
                                       oc';
                           | [] -> None;
            end;
        end;
        | [] -> ();
    in

    let oc_close_all () =
        oc_chan := None;
        List.iter (fun (oc,_,_) ->
            match oc with
            | Some c -> close_out c;
            | None -> ();
            ) !oc_list in        
    
    


    (*
    ** Printing
    *)

    let spaces num =
        let fmt = "%"^(string_of_int num)^"s" in
        Printf.sprintf (Obj.magic fmt) ""
    in


    (*
    ** Text newline and row counter 
    *)
    let nl' () =
        match !oc_chan with
        | Some oc ->
        if (doc_term env) = true then
        begin
                env.doc_row <- env.doc_row + 1;
                if (env.doc_row > env.doc_rowhight) then
                begin
                    output_string oc "\n  --Continue-- ? [yY,nN] ";
                    let answer = read_line () in
                    (
                        match answer with
                        | "n" 
                        | "N" -> raise Exit;
                        | _ -> ();
                    );        
                    Terminfo.backup 1;
                    output_string oc (spaces (env.doc_colwidth-1));
                    Terminfo.backup 1;

                    env.doc_row <- 1;
                end;
        end;
        "\n";
        | None -> "\n";
    in
    
    let out_str str =
        let slen = String.length str in
        match !oc_chan with
        | Some oc ->
        if (env.doc_col + slen > env.doc_colwidth-1) then
        begin
            output_string oc ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        output_string oc str;
        env.doc_col <- env.doc_col + slen;
        | None -> ();
    in

    (*
    ** Fuzzy text newline
    *)
    
    let nl () =
        match !oc_chan with
        | Some oc ->
        if (env.doc_col > env.doc_indent) then
        begin
            output_string oc ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        end;
        | None -> ();
    in

    let ind_incr () =
        match !oc_chan with
        | Some oc ->
            env.doc_indent <- env.doc_indent + 2;
            output_string oc ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        | None -> ();
    in
    let ind_decr () =
        match !oc_chan with
        | Some oc ->
            env.doc_indent <- env.doc_indent - 2;
            output_string oc ((nl'())^(spaces env.doc_indent));
            env.doc_col <- env.doc_indent;
        | None -> ();
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
            | T_BoldItalic -> ();
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
                | S_Value -> ();
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
    ** Convert a text structure element to a string
    *)

    let str_of_text tln =
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
    ** symbol translate
    *)

    let symbol_replace str =    
        match str with
        | "rightarrow" -> " -> ";
        | "Rightarrow" -> " => ";
        | "leftarrow" ->  " <- ";
        | "Leftarrow" ->  " <= ";
        | "bullet" -> " * ";
        | "mu" -> "u";
        | "oplus" -> " + ";
        | "pm" -> " +- ";
        | "equiv" -> " == ";
        | _ -> ("<" ^ str ^ ">");
        in

    let symbol sli =
        let str = ref "" in
        List.iter ( fun d -> str := !str ^ (str_of_struc d)
                ) sli;
        let str' = Str.global_replace (Str.regexp " ") "" !str in
        out_str (symbol_replace str');
        in

    let symbol_str sli =
        let str = ref "" in
        List.iter ( fun d -> str := !str ^ (str_of_struc d)
                ) sli;
        let str' = Str.global_replace (Str.regexp " ") "" !str in
        (symbol_replace str');
        in

    let trans_line str = str in

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

    let rec format_text_str tli =
        let str = ref "" in
        List.iter (fun ds -> 
            match ds.s_content with
            | S_Text t -> 
                          List.iter (fun s ->
                            str := !str ^ " " ^ s;
                            ) t;
            | S_Attribute -> 
                             str := !str ^ (format_text_str ds.s_childs);   
            | S_Symbol -> str := !str ^ (symbol_str ds.s_childs);
            
            | _ -> syntax_error
                    "format_text_str: not a S_Text element";
        ) tli;
        !str
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
                if not (doc_manfile env) then
                begin
                    oc_open env.doc_name S_Body;
                    ind_incr ();
                end;
            | S_Empty -> ();
            | S_Text t -> tex_text  t;

            | S_NL -> nl ();
            | S_TAB -> 
            begin
                match !oc_chan with
                | Some oc -> output_string oc "  ";
                | None -> ();
            end;

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
                                     " < ";
                end;

#if 0            
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
#endif

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

            | S_Ref -> 
                begin
                      env.in_ref <- true;
                 end;
            | S_Label ->
                begin   
                    (* TODO *)
                    env.ignore_childs <- true;
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
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str (nl'());
                    end;
            
                    let ol_cnt = ref 0 in
                    env.in_olist <- env.in_olist + 1; 
                    out_str (nl'()); ind_incr (); 
                    env.in_list <- [O_List] @
                                    env.in_list;
                    let ol_cnt = ref 0 in
                    env.olist_cnt <- [ol_cnt] @ env.olist_cnt;
                end;
                                
            | S_UList -> 
                begin
                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str (nl'()); 
                    end;

                    env.in_ulist <- env.in_ulist + 1;
                    out_str (nl'()); ind_incr (); 
                    env.in_list <- [U_List] @ 
                                    env.in_list;
                end;

            | S_ArgList -> 
                begin
                    if (env.in_ulist + env.in_olist) = 0 then
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
                                | _ -> (); 
                            end; 
                        | [] -> ();     
                    
                    );

                    env.doc_indent <- env.doc_indent + 2;
                    out_str (nl'()^(spaces env.doc_indent)); 
                    env.in_list <- [A_List false] @ 
                                    env.in_list;
                end;

            | S_LitList -> 
                begin
                    nl ();
                    out_str (nl'()^(spaces env.doc_indent));

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
                                | _ -> out_str "References"; 
                            end; 
                        | [] -> out_str "References";     
                    
                    );

                    env.doc_indent <- env.doc_indent + 2;
                    out_str (nl'()^(spaces env.doc_indent)); 
                    env.in_list <- [L_List] @ env.in_list;
                end;

            | S_List_Item -> 
                begin
                    
                    nl (); 
                    match (List.hd env.in_list) with
                        | U_List ->
                        begin
                            out_str ( 
                                (ulist_mark env.in_ulist)
                            ); 
                            env.doc_indent <- env.doc_indent + 4;
                            out_str (spaces 3);
                        end
                        | O_List ->
                        begin
                            incr (List.hd env.olist_cnt);
                            let str = "("^(list_onum env.olist_cnt)^")"
                            in
                            out_str ( str );
                            env.doc_indent <- env.doc_indent + 4;
                            out_str (spaces 
                                    (4 - (String.length str)));
                        end
                        | A_List _
                        | L_List -> 
                        begin

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
                                        env.ignore_head <- true;    
                                        (format_text dna.s_childs);
                                    end;
                                    | _ -> syntax_error "option list item without name";
                                end; 
                                | [] -> syntax_error "option list item without name";
                            );
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

            | S_DataFormat -> 
                begin

                        let cr = ref [] in
                        env.in_special <- Spec_DataFormat {
                                    dataformat_name = [];
                                    dataformat_headlist = [];
                                    dataformat_rowlist = [];
                                    dataformat_currow = cr;
                                    dataformat_comm = [];
                                    dataformat_cols = 0;
                        };
                        env.special_depth <- env.special_depth + 1;
                end; 

            | S_DataHead -> 
                begin
                    env.table_col <- 0;
                    match env.in_special with
                    | Spec_DataFormat tb ->
                                    let cr = ref [] in
                                    tb.dataformat_currow  <- cr;        
                    | _ -> syntax_error "Invalid S_DataHead";            
                end;

            | S_DataRow -> 
                begin
                    env.table_col <- 0;
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
                    env.table_col <- env.table_col + 1;
                    match env.in_special with
                    | Spec_DataFormat tb ->
                                    let row = tb.dataformat_currow in
                                    row := !row @ [ds];
                    | _ -> syntax_error "Invalid S_DataEntry";            
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


            | S_S1 -> 
                begin


                    (*
                    ** The first child must be the section s1 name.
                    *)
                    let i = ref 0 in
                    let pname = ref "" in
                    let fname = ref "" in
                    List.iter (fun dna ->
                        match dna.s_content with
                        | S_Name -> 
                            if !pname = "" then
                            begin
                                incr i; 
                                if !i <> 1 then 
                                    syntax_error "Section without name";
                                        
                                pname := (str_of_text dna.s_childs);

                            end;
                        | S_Filename -> 
                            if !fname = "" then
                            begin
                                fname := file_name (str_of_text dna.s_childs) 0;
                            end;
                        | _ -> (); 
                    ) ds.s_childs;

                    if !pname = "" then
                        syntax_error "Section without name";
                    
                    

                    let sec = { sec_parent=env.cur_sec;
                                sec_childs=[];
                                sec_name= !pname;
                                sec_type="";
                                sec_num= !sec_num;
                        } in

                    incr sec_num;
                    if (doc_manfile env) && !fname <> "" then
                    begin
                        oc_open !fname S_S1;
                        ind_incr ();
                    end;

                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    new_sec env sec;
                                        
                    out_str (c_box
                             !pname
                             (env.doc_colwidth-1)
                             ); 

                                         
                    env.ignore_head <- true;
                    env.cur_s1 <- Some sec;


                    out_str (
                        (fill (env.doc_colwidth-1) '=')^
                        (nl'())
                    );
                    ind_incr ();
                end;

            | S_S2 -> 
                begin

                    (*
                    ** The first child must be the section s1 name.
                    *)
                    let i = ref 0 in
                    let pname = ref "" in
                    let fname = ref "" in
                    List.iter (fun dna ->
                        match dna.s_content with
                        | S_Name -> 
                            if !pname = "" then
                            begin
                                incr i; 
                                if !i <> 1 then 
                                    syntax_error "Section without name";
                                        
                                pname := (str_of_text dna.s_childs);

                            end;
                        | S_Filename -> 
                            if !fname = "" then
                            begin
                                fname := file_name (str_of_text dna.s_childs) 0;
                            end;
                        | _ -> (); 
                    ) ds.s_childs;

                    if !pname = "" then
                        syntax_error "Section without name";
                    
                    if (doc_manfile env) && !fname <> "" then
                    begin
                        oc_open !fname S_S2;
                        ind_incr ();
                     end;

                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();


                    let lbox = !pname in

                    let sec = { sec_parent=env.cur_sec;
                                sec_childs=[];
                                sec_name= !pname;
                                sec_type="";
                                sec_num = !sec_num;
                                        } in
                    incr sec_num;
                    new_sec env sec;
                                
                    env.ignore_head <- true;
                    env.cur_s2 <- Some sec;

                    let rbox =
                        if (env.cur_s1 <> None) then
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

            | S_S3 -> 
                begin

                    (*
                    ** The first child must be the section s1 name.
                    *)
                    let i = ref 0 in
                    let pname = ref "" in
                    let fname = ref "" in
                    List.iter (fun dna ->
                        match dna.s_content with
                        | S_Name -> 
                            if !pname = "" then
                            begin
                                incr i; 
                                if !i <> 1 then 
                                    syntax_error "Section without name";
                                        
                                pname := (str_of_text dna.s_childs);

                            end;
                        | S_Filename -> 
                            if !fname = "" then
                            begin
                                fname := file_name (str_of_text dna.s_childs) 0;
                            end;
                        | _ -> (); 
                    ) ds.s_childs;

                    if !pname = "" then
                        syntax_error "Section without name";
                    
                    if (doc_manfile env) && !fname <> "" then
                    begin
                        oc_open !fname S_S3;
                        ind_incr ();
                     end;

                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    let lbox = !pname in

                    (*
                    ** Open a new file
                    *)
                    let sec = { sec_parent=env.cur_sec;
                                sec_childs=[];
                                sec_name= !pname;
                                sec_type="";
                                sec_num= !sec_num;
                              } in

                    incr sec_num;
                    new_sec env sec;

                    env.cur_s3 <- Some sec;
                    env.ignore_head <- true;

                    let rbox = 
                        if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
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
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in

                                        incr sec_num;
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



            | S_MP -> 
                begin

                    (*
                    ** The first child must be the section s1 name.
                    *)
                    let i = ref 0 in
                    let pname = ref "" in
                    let fname = ref "" in
                    List.iter (fun dna ->
                        match dna.s_content with
                        | S_Name -> 
                            if !pname = "" then
                            begin
                                incr i; 
                                if !i <> 1 then 
                                    syntax_error "Section without name";
                                        
                                pname := (str_of_text dna.s_childs);

                            end;
                        | S_Filename -> 
                            if !fname = "" then
                            begin
                                fname := file_name (str_of_text dna.s_childs) 0;
                            end;
                        | _ -> (); 
                    ) ds.s_childs;

                    if !pname = "" then
                        syntax_error "Section without name";
                    
                    if (doc_manfile env) && !fname <> "" then
                    begin
                        oc_open !fname S_MP;
                        ind_incr ();
                     end;

                    out_str (
                        (nl'())^
                        (fill (env.doc_colwidth-1) '=')
                    );
                    ind_decr ();

                    let lbox = !pname in
                    let sec = { sec_parent=env.cur_sec;
                                sec_childs=[];
                                sec_name= !pname;
                                sec_type="";
                                sec_num= !sec_num;
                              } in

                    incr sec_num;
                    new_sec env sec;

                    env.cur_s3 <- Some sec;
                    env.ignore_head <- true;

                    let rbox = 
                        if (env.cur_s2 <> None) then
                        begin
                            match env.cur_s2 with
                              | Some sec -> 
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

            | S_MP_paragr -> 
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
                                        let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name=pname;
                                                    sec_type="";
                                                    sec_num= !sec_num;
                                        } in

                                        incr sec_num;
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
                                | _ -> syntax_error "ML Module without name"; 
                            end; 
                            | [] -> syntax_error "ML Module without name";
                    );

                    env.doc_indent <- env.doc_indent + 2;
                    nl ();
                    
                    out_str "sig";

                    env.doc_indent <- env.doc_indent + 2;
                    nl ();
                
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
                            if (doc_term env) then
                                Terminfo.standout true;
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
                    nl ();
                    env.in_par <- true;  
                    env.in_figure <- true;
                end;

            | S_Image ->
                begin   
                    if not env.in_figure then
                    begin
                        nl();
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
                    let str' = Str.global_replace (Str.regexp " ") ""
                                                  !str in
                    out_str (sprintf "[Image %s]" str');
                    if not env.in_figure then
                    begin
                        nl ();
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
                            out_str ">";
                        end;            
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
                    env.olist_cnt <- match env.olist_cnt with
                                    | hd::tl -> tl;
                                    | [] -> syntax_error "Unbalanced olist";
                    ;
                    env.in_list <- List.tl env.in_list;
                    env.in_olist <- env.in_olist - 1;

                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str (nl'()); 
                    end;
                    ind_decr ();
                end;

            | S_UList ->    
                begin
                    env.in_ulist <- env.in_ulist - 1;
                    env.in_list <- List.tl env.in_list;

                    if (env.in_ulist + env.in_olist) = 0 then
                    begin
                        out_str (nl'()); 
                    end;
                    ind_decr (); 
                end;

            | S_ArgList ->    
                begin
                    env.in_list <- List.tl env.in_list;
                    env.doc_indent <- env.doc_indent - 2;
            
                    if (env.in_list = []) then
                    begin
                        nl ();
                    end;
                end;

            | S_LitList ->    
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
                    | O_List
                    | U_List ->
                        env.doc_indent <- env.doc_indent - 4;
                    | A_List _
                    | L_List ->
                        env.doc_indent <- env.doc_indent - 2;
                        out_str (nl'()^(spaces env.doc_indent));
                  );  
                  nl ();
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
                        begin
                            if (doc_term env) then
                                Terminfo.standout false;
                        end;            
                end;


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
                    let str =  format_text_str ds.s_childs in 
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
                        let sl = 
                            let sl' = Str.split (Str.regexp " ") str in
                            let sl'' = ref [] in
                            List.iter (fun s ->
                                let slen = String.length s in
                                if slen >= width then
                                begin
                                    (*
                                    ** Brutal word cut...
                                    *)
                                    let frags = slen / width in
                                    let pos = ref 0 in
                                    for i = 0 to frags
                                    do
                                        let size = min (width-1)
                                                       (slen - !pos) in
                                        sl'' := !sl'' @ [
                                            String.sub s !pos size;
                                            ];
                                        pos := !pos + size;
                                    done;
                                end
                                else sl'' := !sl'' @ [s];
                                ) sl';
                                !sl''
                            in
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

            | S_DataFormat -> 
              begin
                let tb = 
                        match env.in_special with
                        | Spec_DataFormat tb -> tb;
                        | _ -> syntax_error "Invalid in_special";
                in 

                let th = tb.dataformat_headlist in
                let thn = List.length th in
                let tr = tb.dataformat_rowlist in
                let trn = List.length tr in
                let tn = tb.dataformat_name in
                let tcols = tb.dataformat_cols in


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
                cellwidth := min (!cellwidth+2)
                                 ((env.textwidth-env.doc_indent)/tcols) ;

                let tablewidth = tcols * !cellwidth in
                
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
                        let sl = 
                            let sl' = Str.split (Str.regexp " ") str in
                            let sl'' = ref [] in
                            List.iter (fun s ->
                                let slen = String.length s in
                                if slen >= width then
                                begin
                                    (*
                                    ** Brutal word cut...
                                    *)
                                    let frags = slen / width in
                                    let pos = ref 0 in
                                    for i = 0 to frags
                                    do
                                        let size = min (width-1)
                                                       (slen - !pos) in
                                        sl'' := !sl'' @ [
                                            String.sub s !pos size;
                                            ];
                                        pos := !pos + size;
                                    done;
                                end
                                else sl'' := !sl'' @ [s];
                                ) sl';
                                !sl''
                            in

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
                if (tn <> []) then 
                begin
                   List.iter (fun dn ->
                        List.iter cont_trans dn.s_childs;
                        ) tn;                    
                end;

                out_str (nl'());ind_incr ();
                out_str (fill (tablewidth) '-'); 
                nl ();


                if th <> [] then
                begin
                    let ci = ref 1 in
                    let rl = List.length th in
    
                    let cl = ref [] in
                    let jmax = ref 0 in

                        List.iter (fun c ->
                            let cle = (colbox c !cellwidth false) in
                            let clel = Array.length cle in
                            cl := !cl @ [cle];
                            if (clel > !jmax) then
                                jmax := clel;
                            incr ci;
                        ) th;   
                        let ca = Array.of_list !cl in
                        for j = 0 to (!jmax)-1
                        do
                            for i = 0 to rl - 1
                            do
                                let cle = ca.(i) in
                                let cll = Array.length cle in
                                if (cll>j) then
                                    out_str (cle.(j))
                                else
                                    out_str (fill !cellwidth ' ');
                            done;
                            nl ();
                        done;

                    out_str (fill (tablewidth) '-'); 
                    nl ();
                end;


                List.iter (fun r ->
                    let ci = ref 1 in
                    let rl = List.length r in
                    let cl = ref [] in
                    let jmax = ref 0 in


                    List.iter (fun c ->
                        (*
                        ** Get the data width in cell units
                        *)
                        let width = ref 0 in
                        List.iter (fun ch -> 
                                match ch.s_content with
                                | S_Value -> 
                                    let str = str_of_childs ch.s_childs in
                                    let str' = Str.global_replace
                                            (Str.regexp " ") "" str in
                                    width := (
                                        try   
                                            (int_of_string str')
                                        with _ -> 
                                        syntax_error "invalid value specifier";
                                    );
                                | _ -> ();
                                ) c.s_childs;
                        if !width = 0 then
                            syntax_error "data entry without width specifier";
                        let cle = colbox c (!cellwidth * !width) false in
                        let clel = Array.length cle in
                        cl := !cl @ [cle];
                        if (clel > !jmax) then
                                jmax := clel;
                        incr ci;
                        ) r;   
                    let ca = Array.of_list !cl in
                    for j = 0 to (!jmax)-1
                    do
                            for i = 0 to rl - 1
                            do
                                let cle = ca.(i) in
                                let cll = Array.length cle in
                                if (cll>j) then
                                    out_str (cle.(j))
                                else
                                    out_str (fill !cellwidth ' ');
                            done;
                            nl ();
                    done;
                ) tr;
                out_str (fill (tablewidth) '-'); 
                nl ();

                env.in_par <- true;   

                ind_decr (); nl (); 
              end;


           | S_DataHead ->
                begin
                    match env.in_special with
                    | Spec_DataFormat tb ->
                        begin
                                    let cr = tb.dataformat_currow in
                                    tb.dataformat_headlist <- !cr;
                                    tb.dataformat_cols <- max
                                                          tb.dataformat_cols 
                                                          env.table_col;
                        end;
                    | _ -> syntax_error "Invalid S_DataHead";
                end;

            | S_DataRow -> 
                begin
                    
                    match env.in_special with
                    | Spec_DataFormat tb ->
                        begin
                                    let cr = tb.dataformat_currow in
                                    tb.dataformat_rowlist <- tb.dataformat_rowlist
                                                        @ [!cr];
                                    tb.dataformat_cols <- max
                                                          tb.dataformat_cols 
                                                          env.table_col;
                        end;
                    | _ -> syntax_error "Invalid S_DataRow";            
                end;

            | S_DataEntry ->
                    env.special_depth <- env.special_depth - 1;

                      

            | S_Body -> 
                begin
                    if env.toc.sec_childs <> [] &&
                       not (doc_toc env) then
                    begin

                        (*
                        ** Print the table of content for the whole
                        ** document.
                        *)

                        print_toc env.toc;

                    end;
                    out_str (nl'()); ind_decr ();
                    oc_close S_Body;
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

            | S_S1 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s1 <- None;
                    oc_close S_S1;
                end;


            | S_S2 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s2 <- None ;
                    oc_close S_S2;
                end;


            | S_S3 -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s3 <- None ;
                    oc_close S_S3;
                end;


            | S_S4 -> 
                begin
                    out_str (nl'());
                    nl (); 
                    close_sec env;
                    env.cur_s4 <- None;
                end;

            | S_MP -> 
                begin
                    out_str (nl'()); nl ();
                    close_sec env;
                    env.cur_s3 <- None ;
                    oc_close S_MP;
                end;


            | S_MP_paragr -> 
                begin
                    out_str (nl'());
                    nl (); 
                    close_sec env;
                    env.cur_s4 <- None;
                end;

            | S_Math ->
                begin  
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


            | S_ML_Fun_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    

                    let fi = 
                        match env.in_special with
                        | Spec_ML_Fun_Int fi -> fi;
                        | _ -> syntax_error "Invalid in_special";
                    in 

                    let rargs = fi.ml_fun_retargs in
                    let fname = fi.ml_fun_name in   
                    let args = fi.ml_fun_args in    
                    let curried = fi.ml_fun_curried in
                    let comm = fi.ml_fun_comm in
                    
                    env.special_depth <- env.special_depth - 1;
                    env.in_special <- Spec_None; 

                    if (fname = []) then
                        syntax_error "ML function interface without name";
                    

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
                                    
            | S_ML_Val_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    
                    
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

                    if not env.in_module &&            
                       env.in_class = Spec_None then   
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
                    if not env.in_module &&
                       env.in_class = Spec_None then                    
                        out_str (nl'()^(spaces env.doc_indent));

                end;

            | S_ML_Ext_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    
                    
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

                    if (vname = []) then
                        syntax_error "External interface without name";

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
                    out_str (spaces !ind);
                    List.iter (fun ds ->
                               match ds.s_content with
                               | _ -> 
                                begin
                                    cont_trans ds;
                                end;
                              ) (List.hd extfun).s_childs;
                    out_str (nl'()^(spaces env.doc_indent));
                end;


            | S_ML_Module_Interface ->
                begin
                    env.doc_indent <- env.doc_indent - 2;
                    nl ();
                    out_str "end";
                    env.doc_indent <- env.doc_indent - 2;
                    nl ();
                    
                end;

            | S_ML_Class_Interface ->
                begin
                    let ind = ref env.doc_col in                    
                    env.special_depth <- env.special_depth - 1;

                    let ci =
                        match env.in_special with
                        | Spec_ML_Class_Int ci -> ci;
                        | _ -> syntax_error "Invalid ML class interface";
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
                    let ind = ref env.doc_col in                    
                    
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

                    if (vname = []) then
                        syntax_error "Method interface without name";
                    

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
                end;
            
            | S_ML_Type_Interface  
            | S_ML_Struc_Interface 
            | S_ML_Exc_Interface ->
                begin
                    let ind = ref env.doc_col in                    
                    
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
                        nl (); 
                    end;
                    
                    

                    let a = List.hd args in
                    List.iter
                        ( fun na ->
                            match na.s_content with
                            | S_Comment -> ();
                            | _ -> cont_trans na
                        )a.s_childs;

                    List.iter cont_trans  
                              (List.hd name).s_childs;

                    out_str ";";
                    if (env.in_interface = false) then
                    begin
                        nl (); 
                    end;
                end;

            | S_C_Hdr_Interface -> 
                if (env.in_object = Spec_None) then
                begin
                    let ind = ref env.doc_col in                    

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
                    
                        nl ();
                        if (comm <> []) then
                        begin
                            cont_trans (List.hd comm);
                            nl ();
                        end;

                        out_str "#include < ";

                        List.iter cont_trans
                                (List.hd hname).s_childs;
                     
                        out_str ">";                    
                        out_str (nl'()^(spaces env.doc_indent));
                    end
                    else
                    begin
                        (*
                        ** Special case: class inherit!!!
                        *)
                    
                        nl ();
                        if (comm <> []) then
                        begin
                            cont_trans (List.hd comm);
                            nl ();
                        end;

                        out_str "inherit ";

                        List.iter cont_trans
                                (List.hd hname).s_childs;
                     
                        out_str "";                    
                        nl ();
                    end;
                end;
        

            | S_C_Fun_Interface -> 
                begin
                    let ind = ref env.doc_col in                    

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

            | S_C_Type_Interface 
            | S_C_Struc_Interface ->
                begin
                    if (env.in_module = true) then
                    begin
                        out_str ""; ind_incr (); 
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
                        nl (); out_str ""; 
                    end
                    else if (env.in_module = true) then
                    begin
                        nl (); out_str ""; 
                    end;

                    if (comm <> []) then
                    begin
                        if (env.in_class = Spec_None &&
                            env.in_module = false) then
                        begin
                            out_str "";
                        end
                        else
                        begin
                            out_str "";
                        end;
                        
                        out_str "<";
                        cont_trans (List.hd comm);
                        out_str ">"; 
                    end;


                    let ind = ref env.doc_col in
                    
                    if (env.in_module = false) then
                    begin
                        out_str "";
                    end
                    else
                    begin
                        out_str "";
                    end;


                    (match tp_type with
                        | Type_List ->
                            out_str "typedef ";
                        | Type_Structure ->
                            out_str "struct ";
                        | _ -> syntax_error "invalid C struct/type";
                    );

                    
                    List.iter cont_trans 
                               (List.hd tname).s_childs;


                    (match tp_type with
                        | Type_List ->
                            out_str " ";
                        | Type_Structure ->
                            out_str " { "; 
                        | _ -> syntax_error "invalid C struct/type";
                    );

                    ind := env.doc_col - !ind;
                    if (args <> []) then
                    begin
                        let n = List.length args in
                        let i = ref 1 in
                        List.iter (fun a ->
                                    if (!i > 1) then
                                    begin
                                        out_str (spaces !ind);
                                    end
                                    else
                                    begin
                                            out_str "";
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
                                        | Type_List      -> out_str ",";
                                        | Type_Structure -> out_str ";";
                                        | _ ->
                                            syntax_error "Invalid Type";
                                    end
                                    else
                                    begin
                                        match tp_type with
                                        | Type_List      -> out_str ";";
                                        | Type_Structure -> out_str ";};";
                                        | _  ->
                                            syntax_error "Invalid Type";
                                    end;

                                    (
                                        match !com with
                                        | Some com -> 
                                          begin
                                            out_str " ";
                                            cont_trans com;
                                          end    
                                        | None -> out_str "  ";  
                                    );
                                    nl ();
                                    incr i;
                                  ) args;
                    end
                    else 
                        syntax_error "type without args";


                    if (env.in_interface = false) then
                    begin
                        nl (); 
                    end;
                    
                    if (env.in_module = true) then
                    begin
                        nl (); 
                    end;
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

            | S_Value -> env.ignore_childs <- false;    (* internally handled *)

            | S_Link -> out_str "";

            | S_Ref -> env.in_ref <- false;
            | S_Label -> ();

            | S_Symbol -> ();

            | S_Filename -> ();

            | S_Figure ->
                begin
                    nl();
                    env.in_par <- true;  
                end;

            | S_Image ->
                begin
                    
                    (* TODO *)
                    env.in_par <- true;
                end;
            | S_Html_Source -> env.in_par <- true;
            | S_Tex_Source -> ();



        );
    in
    if (doc_term env) then
    begin  
        match !oc_chan with
        | Some oc ->
        begin
            ignore(Terminfo.setup oc);
            try
                cont_trans ds
            with    
                | Exit -> ();
                | Failure s -> syntax_error s
        end;
        | None -> ();
    end
    else
    begin
(*
        try
            cont_trans ds
        with
            Failure s -> syntax_error s;
            | _ -> syntax_error "F";
*)
cont_trans ds
    end;

    if not (doc_term env) then
        oc_close_all ();
    
