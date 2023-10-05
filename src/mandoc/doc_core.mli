type doc_options =
    Doc_single
  | Doc_multi_s1
  | Doc_multi_s2
  | Doc_multi_s3
  | Doc_multi_s4
  | Doc_multi_mp
  | Doc_with_toc
  | Doc_link_ref
  | Doc_Main of string
  | Doc_pdftex
  | Doc_color
  | Doc_terminal          (* output to stdout *)
  | Doc_manfile           (* produce only specified manual page files  
                          ** (with FileName command)
                          *)
  | Doc_copyright of string
  | Doc_head of string 
  | Doc_tail of string 
  | Doc_image_path_src of string (* image source directory [H] *) 
  | Doc_image_path_dst of string (* image target directory [H] *)

val in_text : bool ref
val in_preform : bool ref
val in_asis : bool ref
type structure_attr =
    T_Bold
  | T_Italic
  | T_BoldItalic
  | T_Type
  | T_AsIs
  | T_Subscript
  | T_Superscript
and structure_content =
    S_Empty
  | S_Text of string list
  | S_Body
  | S_Title
  | S_TOC
  | S_S1
  | S_S2
  | S_S3
  | S_S4
  | S_MP
  | S_MP_paragr
  | S_Figure
  | S_Table
  | S_DataFormat
  | S_Interface
  | S_Example
  | S_Preform
  | S_Math
  | S_ML_Fun_Interface
  | S_ML_Val_Interface
  | S_ML_Ext_Interface
  | S_ML_Type_Interface
  | S_ML_Exc_Interface
  | S_ML_Struc_Interface
  | S_ML_Method_Interface
  | S_ML_Object_Interface
  | S_ML_Module_Interface
  | S_ML_Class_Interface
  | S_C_Hdr_Interface
  | S_C_Fun_Interface
  | S_C_Var_Interface
  | S_C_Type_Interface
  | S_C_Struc_Interface
  | S_Math_Frac              
  | S_Math_Frac1 
  | S_Math_Frac2
  | S_Math_Int         
  | S_Math_Sum         
  | S_Math_Prod           
  | S_Math_Limit1            
  | S_Math_Limit2            
  | S_Math_Fun
  | S_Attribute
  | S_OList
  | S_UList
  | S_ArgList
  | S_LitList
  | S_List_Item
  | S_TableHead
  | S_TableRow
  | S_TableCol
  | S_TableNoRulers
  | S_DataHead
  | S_DataRow
  | S_DataEntry
  | S_Value
  | S_Html_Source
  | S_Tex_Source
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
  | S_Ref
  | S_Label
  | S_Comment
  | S_Image
  | S_Symbol
  | S_Filename
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
  mutable sec_num : int;
} 
and section_names =
    Sec_s1 of string
  | Sec_s2 of string
  | Sec_s3 of string
  | Sec_s4 of string
  | Sec_mp of string
val empty_str : string ref
val empty_block : structure_block
val space_translate : bool ref
val spaces_to_tab : string -> string
val atoms_of_text : text:string -> string list
val doc_path : string list ref
val atoms_of_file : fname:string -> string list
val line_num : int ref
val file_name : string ref
val line_list : int ref list ref
val file_list : string ref list ref
val syntax_error : unit -> 'a
val new_block :
  structure_block ->
  structure_content -> structure_attr list -> structure_block
val parse : string list -> structure_block -> unit
val tree_of_atoms : atoms:string list -> structure_block
val print_tree : structure_block -> unit
