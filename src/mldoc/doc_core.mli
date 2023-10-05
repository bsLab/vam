type structure_attr =
    T_Bold
  | T_Italic
  | T_BoldItalic
  | T_Type
  | T_Symbol
  | T_AsIs
  | T_Subscript
  | T_Superscript
and structure_content =
    S_Empty
  | S_Text of string list
  | S_Body
  | S_Title
  | S_TOC
  | S_Intro
  | S_Package
  | S_Program
  | S_S1
  | S_Module
  | S_Clib
  | S_S2
  | S_Function
  | S_Type
  | S_Value
  | S_Class
  | S_C
  | S_S3
  | S_Paragraph
  | S_Interface
  | S_Example
  | S_Preform
  | S_S4
  | S_Desc
  | S_Fun_Interface
  | S_Val_Interface
  | S_Type_Interface
  | S_Exc_Interface
  | S_Struc_Interface
  | S_C_Hdr_Interface
  | S_C_Fun_Interface
  | S_C_Var_Interface
  | S_Class_Interface
  | S_Method_Interface
  | S_Object_Interface
  | S_Module_Interface
  | S_Attribute
  | S_OList
  | S_UList
  | S_OpList
  | S_List_Item
  | S_Table
  | S_TableHead
  | S_TableRow
  | S_TableCol
  | S_TableNoRulers
  | S_Name
  | S_Mutable
  | S_Private
  | S_Virtual
  | S_CurArg
  | S_UnCurArg
  | S_RetArg
  | S_NL
  | S_TAB
  | S_Link
  | S_Comment
and structure_block = {
  mutable s_parent : structure_block option;
  mutable s_childs : structure_block list;
  mutable s_content : structure_content;
  mutable s_attr : structure_attr list;
  mutable s_line : int;
  mutable s_name : string ref;
} 
and section = {
  mutable sec_parent : section;
  mutable sec_childs : section list;
  mutable sec_name : string;
  mutable sec_type : string;
} 
and section_names =
    Sec_s1 of string
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
val empty_str : string ref
val empty_block : structure_block
val atoms_of_text : text:string -> string list
val atoms_of_file : fname:string -> string list
val line_num : int ref
val file_name : string ref
val line_list : int ref list ref
val file_list : string ref list ref
val syntax_error : unit -> 'a
val new_block :
  structure_block ->
  structure_content -> structure_attr list -> structure_block
val in_text : bool ref
val in_preform : bool ref
val in_asis : bool ref
val parse : string list -> structure_block -> unit
val tree_of_atoms : atoms:string list -> structure_block
val print_tree : structure_block -> unit
