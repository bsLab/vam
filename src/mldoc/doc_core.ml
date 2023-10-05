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

open StdLabels
open StrLabels

(*
**
**  Document structure:
**
**  =========================================================
**  Document/|  Section |  Subsection |  Unit    |  Subunits   
**  Head     |          |             |          |
**  =========================================================
**
**  Manual     Package     ML-Module    ML-Func     Paragraph
**  Title      Intro                    ML-Type     Interface
**                                      ML-Value    Example
**  Intro
**                         C-Lib        C-Int          |      
**  TOC                    Intro                      \|/
**             Program                                 '
**                                      ML-Module
**                                      ML-Class
**
**                                      Intro
**               S1          S2           S3            S4
**
**
** Subunits and Textelements can be used everywhere. S1-S4 are
** generic sections and units.
**
** Textelements:
**
**  Text 
**  Text attributes
**  Preformatted Text
**
**  Ordered Lists
**  Unordered Lists
**  Option Lists
**  List Item
**  Tables
**  
**  Generic Name
**  Links 
**
**  NewLine
**  Spaces
**
**
**  Special Interface blocks:
**      
**      
**      ML Function Interface
**      ML Value    Interface
**      ML Type     Interface
**      ML Structure Interface
**
**      ML Class Interface
**      
**      C-Function  Interface
**      C-Variable  Interface
**
**  Subblock interface elements:
**
**      Name, Argument Value, Return Argument
**  
**
** and many more ....
**
*)


(*
** Structure attributes
*)

type structure_attr = 
    (*
    ** Text styles
    *)
    | T_Bold
    | T_Italic
    | T_BoldItalic
    | T_Type
    | T_Symbol
    | T_AsIs
    | T_Subscript
    | T_Superscript


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
    | S_Intro                   (* An intro paragraph                   *)

    (*
    ** Sections
    *)

    | S_Package                 (* Package=Library                      *)
    | S_Program                 (* A program description                *)
    | S_S1                      (* Generic named section                *)

      (*
      ** Sub sections
      *)
      | S_Module                  (* A ML-module            *)
      | S_Clib                    (* A C-Library            *)
      | S_S2                      (* Generic subsection     *)
        (*
        ** Units
        *)

        | S_Function              (* A function subsection                *)
        | S_Type                  (* A type subsection                    *)
        | S_Value                 (* A value subsection                   *)
        | S_Class                 (* A class subsection                   *)
        | S_C                     (* C functions and variables            *)
        | S_S3                    (* Generic unit                         *)
          (*
          ** Sub units
          *)
          | S_Paragraph           (* A named paragraph                    *)
          | S_Interface           (* Generic Interface                    *)
          | S_Example             (* Example preformatted paragraph       *)
          | S_Preform             (* Preformatted paragraph               *)
          | S_S4                  (* Generic subunit                      *)
          | S_Desc                (* Description paragraph                *)

            (*
            ** Special layout blocks, commonly packed in 'S_Interface'.
            *)
            | S_Fun_Interface       (* Function interface               *)
            | S_Val_Interface       (* Value interface                  *) 
            | S_Type_Interface      (* A type definition                *)
            | S_Exc_Interface       (* Exception                        *)
            | S_Struc_Interface     (* A type definition                *)
            | S_C_Hdr_Interface     (* C header interface               *)
            | S_C_Fun_Interface     (* C function interface             *)
            | S_C_Var_Interface     (* C variable interface             *)
            | S_Class_Interface     (* A ML-Class                       *)
            | S_Method_Interface    (* A ML method of a class           *)
            | S_Object_Interface    (* A ML object of a class           *)
            | S_Module_Interface    (* A ML module interface            *)  

    (*
    ** Generic sub elements
    *)

    | S_Attribute       (* Text or other attributes changes     *)

    | S_OList           (* Ordered List                         *)
    | S_UList           (* Unordered List                       *)
    | S_OpList          (* Definition List                      *)
    | S_List_Item       (* Unordered List item                  *)


    (*
    ** Simple Tables with borders
    *)

    | S_Table           (* Table body       *)
    | S_TableHead       (* Table header     *)
    | S_TableRow        (* A new table row  *)       
    | S_TableCol        (* Column data      *)
    | S_TableNoRulers   (* Don't put boxes around the table cells *)



    (*
    ** Special names (functions,....)
    *)    
    | S_Name            (* Function, Type,... name              *)
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
    | S_Link            (* link                                 *)
    | S_Comment         (* A comment                            *)
    

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
    mutable sec_type: string;
    }
                

type section_names = 
    | Sec_s1 of string
    | Sec_s2 of string
    | Sec_s3 of string
    | Sec_s4 of string
    | Sec_package of string
    | Sec_program of string
    | Sec_module of string 
    | Sec_function of string
    | Sec_type of string
    | Sec_val of string
    | Sec_class of string
    | Sec_cint of string
    

let empty_str = ref "" 
let empty_block = {
    s_parent = None;
    s_childs=[];
    s_content=S_Empty;
    s_attr=[];
    s_line=0;
    s_name=empty_str;
}  

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
    List.iter ( fun l ->
                atoms := !atoms @ (split ~sep:(regexp "[ ]+") l) @ ["\n"];
              ) lines;
   
    !atoms

(*
** Read the text from a file and convert it to atoms.
*)


let doc_path = ref []

let atoms_of_file ~fname =
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
            (atoms_of_text str)@(convert ());                        
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

let in_text = ref false 
let in_preform = ref false
let in_asis = ref false

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

        | ".B" | ".BI" | ".I" | ".T" | ".S" | ".SB" | ".SP" -> 
          begin
            check_text !cur;
            let attr = 
                match a with
                    | ".B" -> T_Bold;
                    | ".I" -> T_Italic;
                    | ".BI" -> T_BoldItalic;
                    | ".T" -> T_Type;
                    | ".S" -> T_Symbol;
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

        | "\n" ->
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
            ** Unordered and ordered lists
            *)

            | ".OL" -> S_OList, false;
            | ".LO" -> S_OList, true;
            | ".UL" -> S_UList, false;
            | ".LU" -> S_UList, true;
            | ".PL" -> S_OpList, false;
            | ".LP" -> S_OpList, true;
            
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

            | ".(*" -> S_Comment, false;
            | ".*)" -> S_Comment, true;

            (*
            ** Manual page sub units
            *)

            (*
            ** Special interface blocks, commonly packed in 'S_Interface'.
            *)
            | ".IF" -> S_Fun_Interface, false;
            | ".FI" -> S_Fun_Interface, true;
            (*
            ** Value interface
            *)
            | ".IV" -> S_Val_Interface, false;
            | ".VI" -> S_Val_Interface, true;
            (*
            ** Type interface (type = | | | )
            *)
            | ".IT" ->  S_Type_Interface, false;
            | ".TI" ->  S_Type_Interface, true;

            | ".IX" ->  S_Exc_Interface,false;
            | ".XI" ->  S_Exc_Interface,true;
            
            (*
            ** Type structure (type = { })
            *)
            | ".IS" ->  S_Struc_Interface, false;
            | ".SI" ->  S_Struc_Interface, true;


            (*
            ** Module interface 
            *)
            
            | ".IM"   ->  S_Module_Interface,false;
            | ".MI"   ->  S_Module_Interface,true;

            (*
            ** C function and variable interface 
            *)
            | ".CH" ->  S_C_Hdr_Interface, false;
            | ".HC" ->  S_C_Hdr_Interface, true;
            | ".CF" ->  S_C_Fun_Interface, false;
            | ".FC" ->  S_C_Fun_Interface, true;
            | ".CV" ->  S_C_Var_Interface, false;
            | ".VC" ->  S_C_Var_Interface, true;
        
            (*
            ** ML-Class
            *)
            | ".CS" -> S_Class_Interface, false;
            | ".SC" -> S_Class_Interface, true;
            | ".OB" -> S_Object_Interface, false;
            | ".BO" -> S_Object_Interface, true;
            | ".MT" -> S_Method_Interface, false;
            | ".TM" -> S_Method_Interface, true;
            
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
            ** Units of a section (= manual page)
            *)

            (*
            ** ML Function
            *)

            | ".FU" -> S_Function, false;
            | ".UF" -> S_Function, true;
            (*
            ** ML Type
            *)
            | ".TP" -> S_Type, false;
            | ".PT" -> S_Type, true; 
            (*
            ** ML Value
            *)
            | ".MV" -> S_Value, false;
            | ".VM" -> S_Value, true;
            (*
            ** ML Class
            *)
            | ".CL" -> S_Class, false;
            | ".LC" -> S_Class, true;
            (*
            ** C 
            *)
            | ".CI" -> S_C, false;
            | ".IC" -> S_C, true;

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
            ** A named paragraph (needs S_Name child).
            *)
            | ".PA" -> S_Paragraph, false;
            | ".AP" -> S_Paragraph, true;

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
            ** Introduction section/text
            *)

            | ".IO" -> S_Intro, false;
            | ".OI" -> S_Intro, true;

            (*
            ** A program 
            *)

            | ".PR" -> S_Program, false;
            | ".RP" -> S_Program, true;

            (*
            ** A package (library) section
            *)

            | ".PK" -> S_Package, false;
            | ".KP" -> S_Package, true;

            (*
            ** Sub Sections
            *)

            (*
            ** A ML-Module 
            *)

            | ".MD" -> S_Module, false;
            | ".DM" -> S_Module, true;
    
            (*
            ** C-Library
            *)

            | ".CY" -> S_Clib, false;
            | ".YC" -> S_Clib, true;

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
            | ".DE" -> S_Desc, false;
            | ".ED" -> S_Desc,true;            

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
            | S_Intro -> "Intro";
            | S_Module -> "Module";
            | S_Clib    -> "CLib";
            | S_Package -> "Package";
            | S_Program -> "Program";
            | S_Title -> "Title";
            | S_C -> "C";
            | S_Interface -> "Interface";

            | S_C_Var_Interface -> "C-Var";
            | S_C_Hdr_Interface -> "C-Hdr";
            | S_C_Fun_Interface -> "C-Fun";

            | S_Function -> "Function" ;
            | S_Type -> "Type" ;
            | S_Value -> "Value";
            | S_Class -> "Class";
            | S_Paragraph -> "Paragraph";

            | S_Fun_Interface -> "Fun_Int";
            | S_Val_Interface -> "Val_Int";
            | S_Type_Interface -> "Type_Int";
            | S_Exc_Interface -> "Exc_Int";
            | S_Struc_Interface -> "Type_Int";
            | S_Class_Interface -> "Class_Int";
            | S_Module_Interface -> "Module_Int";
            
            | S_Attribute -> "Attr";
            | S_Example -> "Example";
            | S_Preform -> "Preformat";
            | S_OList -> "OList";
            | S_UList -> "UList";
            | S_OpList -> "OpList";
            | S_List_Item -> "ListItem";
            | S_Name -> "Name";

            | S_CurArg -> "CurArg";
            | S_UnCurArg -> "UnCurArg";
            | S_RetArg -> "RetArg";
            | S_Object_Interface -> "ObjectInt";
            | S_Method_Interface -> "MethodInt";
            
            
            | S_NL -> "NL";
            | S_Mutable -> "Mutable";
            | S_Virtual -> "Virtual";
            | S_Private -> "Private";
            
            | S_TAB -> "TAB";
            | S_Link -> "Link";
            | S_Table -> "Table";
            | S_TableHead -> "TableHead";
            | S_TableRow  -> "TableRow";
            | S_TableCol  -> "TableCol";
            | S_TableNoRulers -> "TableNoRulers";
            
            | S_S1 -> "GenSec";
            | S_S2 -> "GenSubSec";
            | S_S3 -> "GenUnit";
            | S_S4 -> "GenSubUnit";
            | S_Desc -> "Desc";
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

