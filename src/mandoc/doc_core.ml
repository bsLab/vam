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
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     
**    $VERSION:     1.19
**
**    $INFO:
**
**  Manual Doc Core module
**
**    $ENDOFINFO
**
*)


(*
** Document formatting mode options (used by the backend)
** H: HTML backend only
** L: Latex backend only
*)

type doc_options =
    | Doc_single            (* One single output file [H] *)
    | Doc_multi_s1          (* One for each section S1 [H] *)
                            (* Page break on new S1 [L] *) 
    | Doc_multi_s2          (* One for S2 [H/L] *)
    | Doc_multi_s3          (* [H/L] *)
    | Doc_multi_s4          (* [H/L] *)
    | Doc_multi_mp          (* Manual page [H/L] *)
    | Doc_with_toc          (* With global table of content [HL] *)
    | Doc_link_ref          (* Linked references ? [L] *)
    | Doc_Main of string    (* Main file without extension! *)
    | Doc_pdftex            (* latex with pdf target *)
    | Doc_color

    | Doc_terminal          (* output to stdout *)
    | Doc_manfile           (* produce only specified manual page files 
                            ** (with FileName command)
                            *)
    | Doc_copyright of string

    (*
    ** User customized header and trailer of each output file
    *)
    | Doc_head of string    (* Output text before doc content [H] *)
    | Doc_tail of string    (* Output text after doc content *)
    | Doc_image_path_src of string (* image source directory [H] *)
    | Doc_image_path_dst of string (* image target directory [H] *)

open StrLabels

let in_text = ref false 
let in_preform = ref false
let in_asis = ref false


(*
** Structure attributes
*)

type structure_attr = 
    (*
    ** Text styles
    *)
    | T_Bold        (* .B .R *)
    | T_Italic      (* .I .R *)
    | T_BoldItalic  (* .BI .R *)
    | T_Type        (* .T .R *)
    | T_AsIs        (* .[ .] *)
    | T_Subscript   (* .SB .BS *)
    | T_Superscript (* .SP .PS *)


(*
** Document contents
*)

type structure_content =
    (*
    ** Heavily used 
    *)
    | S_Empty                   (* Empty element                *)
    | S_Text of (string list)   (* Generic text: basic element  *)

    (*
    ** Document head
    *)
    | S_Body                    (* Document root                        *)
    | S_Title                   (* Document title                       *)
    | S_TOC                     (* Table of contents                    *)

    (*
    ** Section and unit heads 
    *)

    (*
    ** Sections
    *)

    | S_S1                      (* Generic named section .S1 .1S        *)
    | S_S2                      (* Generic named section .S2 .2S        *)
    | S_S3                      (* Generic named section .S3 .3S        *)
    | S_S4                      (* Generic named section .S4 .4S        *)
    | S_MP                      (* Manual Page           .MP .PM        *)

        | S_MP_paragr           (* .MA .AM *)

      (*
      ** Sub sections
      *)
          (*
          ** Blocks
          *)
          | S_Figure              (* A picture block .FG .GF            *)
          | S_Table               (* A table block .TB .BT              *)
          | S_DataFormat          (* A data format table .DF .FD        *)
          | S_Interface           (* Generic Interface .IN .NI          *)
          | S_Example             (* Example paragraph .EX .XE          *)
                                  (* Needs nested S_Preform !!!!        *)
          | S_Preform             (* .{ .} *)
          | S_Math                (* Math environment .ME .EM *)

            (*
            ** Special layout blocks, commonly packed in 'S_Interface'.
            *)
            | S_ML_Fun_Interface       (* Function interface .IF .FI *)
            | S_ML_Val_Interface       (* Value interface    .IV .VI *) 
            | S_ML_Ext_Interface       (* External interface .IE .EI *) 
            | S_ML_Type_Interface      (* A type definition  .IT .TI *)
            | S_ML_Exc_Interface       (* Exception          .IX .XI *)
            | S_ML_Struc_Interface     (* A type definition  .IS .SI *)
            | S_ML_Method_Interface    (* A ML method of a class .MT .TM *)
            | S_ML_Object_Interface    (* A ML object of a class .OB .BO *)
            | S_ML_Module_Interface    (* A ML module interface  .IM .MI *)  
            | S_ML_Class_Interface     (* A ML-Class             .MC .CM *)
            | S_C_Hdr_Interface        (* C header interface     .CH .HC *)
            | S_C_Fun_Interface        (* C function interface   .CF .FC *)
            | S_C_Var_Interface        (* C variable interface   .CV .VC *)
            | S_C_Type_Interface       (* C type interface       .CY .YC *)
            | S_C_Struc_Interface      (* C structure interface  .CS .SC *)

            (*  
            ** Math entries
            *)

            | S_Math_Frac              (* .F0 
                                       ** .F1 <nominator>   .1F 
                                       ** .F2 <denominator> .2F 
                                       ** .0F
                                       *)
            | S_Math_Frac1             
            | S_Math_Frac2

            | S_Math_Int               (* .I1 
                                       ** .L1 <limit1> .1L
                                       ** .L2 <limit2> .2L
                                       ** .1I
                                       ** Limits are optional     
                                       *)
            | S_Math_Sum               (* .I2 .2I *)
            | S_Math_Prod              (* .I3 .3I *)

            | S_Math_Limit1            
            | S_Math_Limit2  
            
            | S_Math_Fun                (* .FN .NF *)
        
    (*
    ** Generic sub elements
    *)

    | S_Attribute       (* Text or other attributes changes     *)

    (*
    ** Paragraphe elements
    *)

    | S_OList           (* Ordered List .OL .LO *)
    | S_UList           (* Unordered List .UL .LU *)
    | S_ArgList         (* Argument List  .AL .LA *)
    | S_LitList         (* Literature List .RL .LR *)
    | S_List_Item       (* List item .LI .IL *)

    (*
    ** Simple Tables with borders
    *)

    | S_TableHead       (* Table header     .TH .HT *)
    | S_TableRow        (* A new table row  .TR .RT *)       
    | S_TableCol        (* Column data      .TC .CT *)
    | S_TableNoRulers   (* Don't put boxes around the table cells .NR *)

    (*
    ** Data format table (with column spawn) for graphical representations
    ** of structures in table format.
    **
    ** .DF
    **      .DH
    **          .DE .VE <cell width in char units> .EV .NA ... .AN .ED
    **          ...
    **      .HD
    **      .DR
    **          .DE .VE <data width in cell units> .EV .NA ... .AN .ED
    **          ...
    **      .RD
    **
    ** .FD
    *)

    | S_DataHead        (* Table header     .DH .HD *)
    | S_DataRow         (* A new table row  .DR .RD *)       
    | S_DataEntry       (* Column data      .DE .ED *)
 

    | S_Value   (* A value specifier .VE .EV  *)


    (*
    ** Special backend sources (handled as is...)
    *)
    | S_Html_Source     (* source for the HTML backend .HS .SH *)
    | S_Tex_Source      (* source for the Tex backend .TS .ST  *)

    

    (*
    ** Special names (functions,....)
    *)    
    | S_Name            (* Function, Type,... name  .NA .AN  *)
    | S_Mutable
    | S_Private
    | S_Virtual
    
    (*
    ** Arguments of functions, values,...
    *)

    | S_CurArg          (* Argument of a curried list           *)
    | S_UnCurArg        (* Argument of an uncurried list:tuple  *)
    | S_RetArg          (* Return argument of a function        *)

    | S_NL              (* Newline                              *)
    | S_TAB             (* Text tabulation                      *)


    | S_Link            (* link to another document .LK .KL     *)

    | S_Ref             (* Reference to an object  .RF .FR      *)
    | S_Label           (* Label mark of this object .LB .BL    *)
                        (*
                        ** Usage:
                        ** .S1 .NA <text> .AN .LB S1013 .BL  ....
                        ** ... .RF .LB S1013 .BL <text> .FR ...
                        *)

    | S_Comment         (* A comment                            *)
    
    | S_Image           (* image file .PI .IP *)
    | S_Symbol          (* .SX <tex symbol name> .XS *)

    | S_Filename        (* .MF <output file name for a section> .FM *)


type structure_block =
{
    mutable s_parent:     structure_block option;
    mutable s_childs:     structure_block list;
    mutable s_content:    structure_content;
    mutable s_attr:       structure_attr list;   
    mutable s_line:       int;
    mutable s_name:       string ref;
}

(*
** Section names
*)

type section = {
    mutable sec_parent: section;
    mutable sec_childs: section list;
    mutable sec_name: string;
    mutable sec_type: string;   (* private type string *)
    mutable sec_num: int;   (* unique section number *)
    }
                

type section_names = 
    | Sec_s1 of string
    | Sec_s2 of string
    | Sec_s3 of string
    | Sec_s4 of string
    | Sec_mp of string 
    


let empty_str = ref "" 
let empty_block = {
    s_parent = None;
    s_childs=[];
    s_content=S_Empty;
    s_attr=[];
    s_line=0;
    s_name=empty_str;
}  

let space_translate = ref false 

(*
** util function: replace all spaces (# > 1) between words with
** tabulators in preformatted text -> optional translation
*)
let spaces_to_tab line =
    (*
    ** First find out if we're in special preformatted mode
    *)
    let atoms = (split ~sep:(regexp "[ ]+") line) in
    let still_translate = !space_translate in
    List.iter (fun a ->
        match a with
        | ".{" -> space_translate := true;
        | ".}" -> space_translate := false;
        | _ -> ();
        ) atoms;

    if !space_translate || still_translate then
    begin
        let line' = ref "" in
        let s = " " in
        let space_count = ref 0 in
        String.iter (fun c ->
            if c = ' ' then
            begin
                incr space_count;
            end
            else
            begin
                if !space_count > 1 then
                begin
                    let tn = max (!space_count / 4) 1 in
                    for i = 1 to tn 
                    do
                        line' := !line' ^ " .#";
                    done;
                    line' := !line' ^ " ";
                end
                else if !space_count = 1 then
                    line' := !line' ^ " ";

                space_count := 0;
                s.[0] <- c;
                line' := !line' ^ s;
            end;
        ) line;
        !line'
    end
    else
        line

(*
** Return a list of all lines from 'text', an each list member is a
** string list of text atoms [delimited by spaces].
*)

let atoms_of_text ~text =
        
    (*
    ** Split the content text in lines
    *)

    let lines = split ~sep:( regexp "\n" ) text in

    (*
    ** and the lines in sub items delimited by spaces
    *)

    let atoms = ref [] in
    if lines <> [] then
        List.iter ( fun l ->
                let l = spaces_to_tab l in
                atoms := !atoms @ (split ~sep:(regexp "[ ]+") l) @ ["\n"];
              ) lines
    else
        atoms := !atoms @ ["\n"];


    !atoms

(*
** Read the text from a file and convert it to atoms.
*)


let doc_path = ref []

let atoms_of_file ~fname =
    space_translate := false;

    let ic = try 
                open_in fname 
             with
                Sys_error _ -> failwith (
                                "atoms_of_file: can't open "^
                                fname
                                );
    in

    let dp = Filename.dirname fname in
    if (dp <> ".") then
        doc_path := [dp] @ !doc_path;

    let rec convert () = 
        try
            let str = input_line ic in
            let atoms = atoms_of_text str in
            atoms@(convert ());                        
        with
            End_of_file -> []
    in
                
    let nas = convert () in
    close_in ic; 
    nas

(*
** The language parser:
** Parse atoms and store them into structure blocks.
*)

let line_num = ref 1 
let file_name = ref "<stdin>"

let line_list = ref []
let file_list = ref []

let syntax_error () =
    failwith ("doc_core: syntax error in "^
              (!file_name)^
              " line "^
              (string_of_int !line_num)
    )

(*
** Create a new block element
*)

let new_block par cont attr =
    {
        s_parent = Some par;
        s_childs = [];
        s_content = cont;
        s_attr = attr;
        s_line = !line_num;
        s_name = file_name;
    }    

(*
** Generic text: The basic atom.
*)


let rec parse al cur =
    (*
    ** There is one special case: generic text has no dot
    ** environment, it 'comes as is'. All the time, the
    ** parser must check if the current structure
    ** element is generic text before a new command can be
    ** parsed ('check_text').
    *)

    let cur = ref cur in

    (*
    ** Add a new child structure element to the current block.
    *)
    let add_child cur chi =
        cur.s_childs <- cur.s_childs @ [chi]
    in

    (*
    ** The current element is closed. Go up to the parent of 
    ** the current element. 
    *)

    let up cur =
            let parent = 
                match cur.s_parent with
                | Some p -> p;
                | None -> syntax_error ();
            in
            parent
    in

    (*
    ** Check if the current block is a generic text block without
    ** a dot environment.
    *)

    let check_text cur' =
        if (!in_text = true) then
        begin
            in_text := false;
            cur := match cur'.s_parent with
                   | Some p -> p
                   | None -> syntax_error ();
        end
    in
    let check_cont cur tp =
        if (cur.s_content <> tp) then
            syntax_error ();
    in


    match al with
    | a::tl ->
    begin


        let sl = String.length a in

        if (!in_asis = true) then
        begin
            (*
            ** Ignore all dot commands except '.]'.
            *)
            if (sl = 2 && a = ".]") then
            begin
                in_asis := false;
                if (!in_preform = true) then 
                    parse tl !cur
                else
                begin
                    check_text !cur;
                    parse tl (up !cur);
                end;
            end
            else
            begin
                match !cur.s_content with
                | S_Text l -> 
                    begin
                        !cur.s_content <- S_Text (l@[a]);
                        parse tl !cur;
                    end;
                | _ ->  
                    begin 
                        (*
                        ** Create a new S_Text child
                        *) 
                        let b = new_block !cur (S_Text [a]) !cur.s_attr in
                        add_child !cur b;
                        in_text := true;
                        parse tl b;
                    end;
            end;
        end 
        else if (sl > 0 && a.[0] <> '.' || sl > 3) then
        begin
          
          if (a.[0] = '\n') then
          begin
            incr line_num;
            if (!in_preform = true || !in_asis = true) then
            begin
                match !cur.s_content with
                | S_Text l -> 
                    begin
                        !cur.s_content <- S_Text (l@[a]);
                        parse tl !cur;
                    end;
                | _ ->  
                    parse tl !cur;
            end
            else
                parse tl !cur;
          end
          else
          begin
            (*
            ** It must be generic text. If the current element
            ** is already a text element, then append this atom
            ** to the list of text atoms.
            *)
            match !cur.s_content with
            | S_Text l -> 
                begin
                    !cur.s_content <- S_Text (l@[a]);
                    parse tl !cur;
                end;
            | _ ->  
                begin 
                    (*
                    ** Create a new S_Text child
                    *) 
                    let b = new_block !cur (S_Text [a]) !cur.s_attr in
                    add_child !cur b;
                    in_text := true;
                    parse tl b;
                end;
          end;
        end  
        else match a with

        (*
        ** Text styles
        *)

        | ".B" | ".BI" | ".I" | ".T" | ".SB" | ".SP" -> 
          begin
            check_text !cur;
            let attr = 
                match a with
                    | ".B" -> T_Bold;
                    | ".I" -> T_Italic;
                    | ".BI" -> T_BoldItalic;
                    | ".T" -> T_Type;
                    | ".SB" -> T_Subscript;
                    | ".SP" -> T_Superscript;
                    | _ -> syntax_error ();
            in
            let b = new_block !cur S_Attribute (!cur.s_attr @ [attr]) in
            add_child !cur b;
            parse tl b;
          end;
        | ".[" -> 
            begin
                in_asis := true;
                if (!in_preform = false) then
                begin
                    check_text !cur;
                    let b = new_block !cur S_Attribute (!cur.s_attr @ [T_AsIs]) 
                    in
                    add_child !cur b;
                    parse tl b;
                end
                else
                    parse tl !cur;
            end;
        | ".R"  | ".BS" | ".PS" ->
          begin
            check_text !cur;
            parse tl (up !cur);
          end; 

        (*
        ** Text formatting
        *)

        | ".NL" ->
          begin
            check_text !cur;
            let b = new_block !cur S_NL !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;

        | ".MU" ->
          begin
            check_text !cur;
            let b = new_block !cur S_Mutable !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;

        | ".PV" ->
          begin
            check_text !cur;
            let b = new_block !cur S_Private !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;

        | ".VT" ->
          begin
            check_text !cur;
            let b = new_block !cur S_Virtual !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;
    
        | ".NR" ->
          begin
            check_text !cur;
            let b = new_block !cur S_TableNoRulers !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;

        | ".#" ->
          begin
            check_text !cur;
            let b = new_block !cur S_TAB !cur.s_attr in
            add_child !cur b;
            parse tl !cur;
          end;

        | "\n" 
        | "" ->
          begin
            incr line_num;
            if (!in_preform = true) then
            begin
                match !cur.s_content with
                | S_Text l -> 
                    begin
                        !cur.s_content <- S_Text (l@[a]);
                        parse tl !cur;
                    end;
                | _ ->  
                    parse tl !cur;
            end
            else
                parse tl !cur;
          end;
   
        | ".<<" ->
          begin
            (*
            ** Include a mldoc file 
            *)
            
            check_text !cur;
            
            let tl' = ref tl in
            let rec getname s =
                match s with
                | a::tl ->
                    if (a <> ".>>") then
                        a^(getname tl)
                    else
                    begin
                        tl' := tl;
                        ""
                    end;
                | [] -> syntax_error ();
            in

            let dpl = List.length !doc_path in
            let dp  = if (dpl > 0) then
                      begin
                        let rec iter l =
                            match l with
                            | hd::tl -> if (tl <> []) then
                                            (iter tl)^"/"^hd
                                        else
                                            hd;
                            | [] -> ""
                        in
                        iter !doc_path;
                      end
                      else
                        ""
            in

                        
            let fname = ref (getname tl) in

            let dp' = Filename.dirname !fname in

            if (dp <> "") then
                fname := dp^"/"^(!fname);
                
            if (dp' <> ".") then
                doc_path := [dp'] @ !doc_path;
               
            Printf.printf "Include file %s...\n" (!fname);            
                 
            let ic = try 
                        open_in !fname 
                     with
                        Sys_error _ -> syntax_error ()
            in


            let rec convert () = 
                    try
                        let str = input_line ic in
                        (atoms_of_text str)@(convert ());                        
                    with
                        End_of_file -> []
            in
                
            let nas = convert () in
            close_in ic; 
        
            file_list := [file_name] @ !file_list;
            file_name := !fname;
            
            line_list := [line_num]  @ !line_list;
            line_num  := 1;
              

            parse (nas) !cur;

            if (dp' <> ".") then
                doc_path := List.tl !doc_path;

            parse (!tl') !cur;
    
            file_name := !(List.hd !file_list);
            line_num  := !(List.hd !line_list);
            
            file_list := List.tl !file_list;
            line_list := List.tl !line_list;
            


          end;

        | _ ->
        begin

        (*
        ** Generic dot commands
        *)

        check_text !cur;

        let s_type,antidot =
            match a with

            (*
            ** Lists
            *)

            | ".OL" -> S_OList, false;
            | ".LO" -> S_OList, true;
            | ".UL" -> S_UList, false;
            | ".LU" -> S_UList, true;
            | ".AL" -> S_ArgList, false;
            | ".LA" -> S_ArgList, true;
            | ".RL" -> S_LitList, false;
            | ".LR" -> S_LitList, true;
            
            | ".LI" -> S_List_Item, false;
            | ".IL" -> S_List_Item, true;

            (*
            ** Tables
            *)
            | ".TB" -> S_Table, false;
            | ".BT" -> S_Table, true;
            (*
            ** Table head
            *)
            | ".TH" -> S_TableHead, false;
            | ".HT" -> S_TableHead, true;
            (*
            ** Table Row
            *)
            | ".TR" -> S_TableRow, false;
            | ".RT" -> S_TableRow, true;
            (*
            ** Table Column Data
            *)
            | ".TC" -> S_TableCol, false;
            | ".CT" -> S_TableCol, true;

            (*
            ** Data format table
            *)
            | ".DF" -> S_DataFormat, false;
            | ".FD" -> S_DataFormat, true;
            (*
            ** Table head
            *)
            | ".DH" -> S_DataHead, false;
            | ".HD" -> S_DataHead, true;
            (*
            ** Table Row
            *)
            | ".DR" -> S_DataRow, false;
            | ".RD" -> S_DataRow, true;
            (*
            ** Table Column Data
            *)
            | ".DE" -> S_DataEntry, false;
            | ".ED" -> S_DataEntry, true;


            | ".(*" -> S_Comment, false;
            | ".*)" -> S_Comment, true;

            (*
            ** Manual page sub units
            *)

            (*
            ** Special interface blocks, commonly packed in 'S_Interface'.
            *)
            (*
            ** ML function
            *)
            | ".IF" -> S_ML_Fun_Interface, false;
            | ".FI" -> S_ML_Fun_Interface, true;
            (*
            ** ML Value 
            *)
            | ".IV" -> S_ML_Val_Interface, false;
            | ".VI" -> S_ML_Val_Interface, true;
            (*
            ** ML External 
            *)
            | ".IE" -> S_ML_Ext_Interface, false;
            | ".EI" -> S_ML_Ext_Interface, true;
            (*
            ** Type interface (type = | | | )
            *)
            | ".IT" ->  S_ML_Type_Interface, false;
            | ".TI" ->  S_ML_Type_Interface, true;

            (*
            ** Exception
            *)
            | ".IX" ->  S_ML_Exc_Interface,false;
            | ".XI" ->  S_ML_Exc_Interface,true;
            
            (*
            ** Type structure (type = { })
            *)
            | ".IS" ->  S_ML_Struc_Interface, false;
            | ".SI" ->  S_ML_Struc_Interface, true;


            (*
            ** Module interface 
            *)
            
            | ".IM"   ->  S_ML_Module_Interface,false;
            | ".MI"   ->  S_ML_Module_Interface,true;

            (*
            ** C function and variable interface 
            *)
            | ".CH" ->  S_C_Hdr_Interface, false;
            | ".HC" ->  S_C_Hdr_Interface, true;
            | ".CF" ->  S_C_Fun_Interface, false;
            | ".FC" ->  S_C_Fun_Interface, true;
            | ".CV" ->  S_C_Var_Interface, false;
            | ".VC" ->  S_C_Var_Interface, true;
            | ".CY" ->  S_C_Type_Interface, false;
            | ".YC" ->  S_C_Type_Interface, true;
            | ".CS" ->  S_C_Struc_Interface, false;
            | ".SC" ->  S_C_Struc_Interface, true;
        
            (*
            ** ML-Class
            *)
            | ".MC" -> S_ML_Class_Interface, false;
            | ".CM" -> S_ML_Class_Interface, true;
            | ".OB" -> S_ML_Object_Interface, false;
            | ".BO" -> S_ML_Object_Interface, true;
            | ".MT" -> S_ML_Method_Interface, false;
            | ".TM" -> S_ML_Method_Interface, true;
            
            (*
            **  Common interface dots
            **  The interface function/value/type name 
            *)
            | ".NA" ->  S_Name, false;
            | ".AN" ->  S_Name, true;
            (*
            ** arguments [curried form]
            *)
            | ".AR" ->  S_CurArg, false;
            | ".RA" ->  S_CurArg, true;
            (*
            ** uncurried form [tuple] 
            *)
            | ".AV" ->  S_UnCurArg, false;
            | ".VA" ->  S_UnCurArg, true;
            (*
            ** Return arguments from a function
            *)
            | ".RV" ->  S_RetArg, false;
            | ".VR" ->  S_RetArg, true;

            (*
            ** Links
            *)
            | ".LK" ->  S_Link, false;
            | ".KL" ->  S_Link, true;

            (*
            ** References and Labels
            *)
            | ".RF" -> S_Ref, false;
            | ".FR" -> S_Ref, true;
            | ".LB" -> S_Label, false;
            | ".BL" -> S_Label, true;


            (*
            ** Symbols (latex symbol names)
            *)
            | ".SX" -> S_Symbol, false;
            | ".XS" -> S_Symbol, true;


            (*
            ** Filename
            *)
            | ".MF" -> S_Filename, false;
            | ".FM" -> S_Filename, true;

            
            (*
            ** Sub units
            *)

            (*
            ** Interface
            *)
            | ".IN" -> S_Interface, false;
            | ".NI" -> S_Interface, true;

            (*
            ** Preformatted and Example paragraph
            *)
            | ".EX" ->
              begin
                in_preform := true;
                S_Example, false;
              end;
            | ".XE" ->
              begin
                in_preform := false;
                S_Example, true;
              end;

            | ".{" ->
              begin
                in_preform := true;
                S_Preform, false;
              end;    
            | ".}" ->
              begin
                in_preform := false;
                S_Preform, true;
              end;
    
            (*
            ** A figure environement with picture and text
            *)
            | ".FG" -> S_Figure, false;
            | ".GF" -> S_Figure, true;


            (*
            ** A math environement 
            *)
            | ".ME" -> S_Math, false;
            | ".EM" -> S_Math, true;

            | ".F0" -> S_Math_Frac, false;
            | ".0F" -> S_Math_Frac, true;

            | ".F1" -> S_Math_Frac1, false;
            | ".1F" -> S_Math_Frac1, true;

            | ".F2" -> S_Math_Frac2, false;
            | ".2F" -> S_Math_Frac2, true;


            | ".I1" -> S_Math_Int, false;
            | ".1I" -> S_Math_Int, true;
            | ".I2" -> S_Math_Sum, false;
            | ".2I" -> S_Math_Sum, true;
            | ".I3" -> S_Math_Prod, false;
            | ".3I" -> S_Math_Prod, true;

            | ".L1" -> S_Math_Limit1, false;
            | ".1L" -> S_Math_Limit1, true;

            | ".L2" -> S_Math_Limit2, false;
            | ".2L" -> S_Math_Limit2, true;

            | ".FN" -> S_Math_Fun, false;
            | ".NF" -> S_Math_Fun, true;


            (*
            ** Document head
            *)

            (*
            ** Title page of the document
            *)

            | ".TL" -> S_Title, false;
            | ".LT" -> S_Title, true;


            (*
            ** Sections
            *)


            (*
            ** Generic sections and units
            *)

            | ".S1" -> S_S1, false;
            | ".1S" -> S_S1, true;
            | ".S2" -> S_S2, false;
            | ".2S" -> S_S2, true;
            | ".S3" -> S_S3, false;
            | ".3S" -> S_S3, true;
            | ".S4" -> S_S4, false;
            | ".4S" -> S_S4, true;
            | ".MP" -> S_MP, false;
            | ".PM" -> S_MP, true;            
            | ".MA" -> S_MP_paragr, false;
            | ".AM" -> S_MP_paragr, true;            

            | ".PI" -> S_Image,false;
            | ".IP" -> S_Image,true;

            (*
            ** A value (in text format)
            *)

            | ".VE" -> S_Value,false;
            | ".EV" -> S_Value,true;

            (*
            ** Backend sources
            *)
            | ".HS" -> in_preform := true; S_Html_Source,false;
            | ".SH" -> in_preform := false; S_Html_Source,true;
            | ".TS" -> in_preform := true; S_Tex_Source,false;
            | ".ST" -> in_preform := false; S_Tex_Source,true;

            (*
            ** Generic Text
            *)

            | text -> 
              begin

                (*
                ** It must be generic text. If the current element
                ** is already a text element, then append this atom
                ** to the list of text atoms.
                *)
                match !cur.s_content with
                | S_Text l -> 
                    begin
                        !cur.s_content <- S_Text (l@[text]);
                        parse tl !cur;
                        S_Empty, false; (* already done *)
                    end;
                | _ ->  
                    begin 
                        (*
                        ** Create a new S_Text child
                        *) 
                        let b = new_block !cur (S_Text [text]) !cur.s_attr in
                        add_child !cur b;
                        in_text := true;
                        parse tl b;
                        S_Empty, false; (* already done *)
                    end;
              end
        in
        if (s_type <> S_Empty) then
        begin
            if (antidot = false) then
            begin
                let b = new_block !cur s_type !cur.s_attr in
                add_child !cur b;
                parse tl b;
            end
            else
            begin
                check_cont !cur s_type;
                parse tl (up !cur); 
            end;
        end;
    end;
    end;
    | [] -> ()


(*
** Build a structure tree from the atom list of the text.
*)

let tree_of_atoms ~atoms =
    (*
    ** Init the parser
    *)

    in_text := false;
    in_asis := false;
    in_preform := false;

    line_num := 1;

    (*
    ** The document body (root)
    *)

    let body_block = 
    {
        s_parent = None;
        s_childs = [];
        s_content = S_Body; 
        s_attr = [];
        s_line = 0;
        s_name = file_name;
    } in                         


    (*
    ** work through all the atoms of the text
    *)
    parse atoms body_block;
    body_block



(*
** Print a parsed structure tree
*)

open Printf

let print_tree ds =

    let indent = ref 2 in

    let cont_str ds =
          match ds.s_content with
            | S_Body -> "Body";
            | S_TOC -> "TableOfCont";
            | S_Empty -> "Empty";
            | S_Text t -> "Text";
            | S_Comment -> "Comment";
            | S_Title -> "Title";
            | S_Interface -> "Interface";
            | S_Figure -> "Figure";
            | S_Image -> "Image";

            | S_C_Var_Interface -> "C-Var";
            | S_C_Hdr_Interface -> "C-Hdr";
            | S_C_Fun_Interface -> "C-Fun";
            | S_C_Type_Interface -> "C-Type";
            | S_C_Struc_Interface -> "C-Struc";


            | S_ML_Fun_Interface -> "ML-Fun_Int";
            | S_ML_Val_Interface -> "ML-Val_Int";
            | S_ML_Type_Interface -> "ML-Type_Int";
            | S_ML_Exc_Interface -> "ML-Exc_Int";
            | S_ML_Ext_Interface -> "ML-Ext_Int";
            | S_ML_Struc_Interface -> "ML-Type_Int";
            | S_ML_Class_Interface -> "ML-Class_Int";
            | S_ML_Module_Interface -> "ML-Module_Int";
            | S_ML_Object_Interface -> "ML-ObjectInt";
            | S_ML_Method_Interface -> "ML-MethodInt";
            
            | S_Attribute -> "Attr";
            | S_Example -> "Example";
            | S_Preform -> "Preformat";
            | S_OList -> "OList";
            | S_UList -> "UList";
            | S_LitList -> "LitList";
            | S_ArgList -> "ArgList";
            | S_List_Item -> "ListItem";
            | S_Name -> "Name";

            | S_CurArg -> "CurArg";
            | S_UnCurArg -> "UnCurArg";
            | S_RetArg -> "RetArg";
            
            
            
            | S_NL -> "NL";
            | S_Mutable -> "Mutable";
            | S_Virtual -> "Virtual";
            | S_Private -> "Private";
            
            | S_TAB -> "TAB";

            | S_Link -> "Link";
            | S_Ref -> "Ref";
            | S_Label -> "Label";
            | S_Symbol -> "Symbol";
            | S_Filename -> "Filename";
    
            | S_Table -> "Table";
            | S_TableHead -> "TableHead";
            | S_TableRow  -> "TableRow";
            | S_TableCol  -> "TableCol";
            | S_TableNoRulers -> "TableNoRulers";

            | S_DataFormat -> "DataFormatTable";
            | S_DataHead -> "DataHead";
            | S_DataRow  -> "DataRow";
            | S_DataEntry  -> "DataEntry";

            | S_Math -> "Math";
            | S_Math_Frac -> "Math_Frac";
            | S_Math_Frac1 -> "Math_Frac1";
            | S_Math_Frac2 -> "Math_Frac2";
            | S_Math_Int -> "Math_Int";
            | S_Math_Sum -> "Math_Sum";
            | S_Math_Prod -> "Math_Prod";
            | S_Math_Limit1 -> "Math_Limit1";
            | S_Math_Limit2 -> "Math_Limit2";
            | S_Math_Fun -> "Math_Fun";            

            | S_S1 -> "S1";
            | S_S2 -> "S2";
            | S_S3 -> "S3";
            | S_S4 -> "S4";
            
            | S_Value -> "Value";
            | S_Html_Source -> "HTML_source";
            | S_Tex_Source -> "TEX_source";

            | S_MP -> "ManualPage";
            | S_MP_paragr -> "ManualPage Paragraphe";
    in

                    

    let rec dump_tree ds =

        let rec dump_childs dsl = 
            match dsl with
            | ds::tl -> dump_tree ds;
                        dump_childs tl;
            | [] -> ()
        in
        
        let str = cont_str ds in
        let spaces = !indent + String.length str in
        let istr = "%"^(string_of_int spaces)^"s\n" in
        printf (Obj.magic istr) str; 
        if (ds.s_childs <> []) then
        begin
            indent := !indent + 2;
            dump_childs ds.s_childs;
            indent := !indent - 2;
        end;
    in

    dump_tree ds;
    print_newline ();
;;

