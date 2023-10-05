type h_sec = {
  mutable h_sec_name : string;
  mutable h_sec_keywords : string list;
  mutable h_sec_ds : Doc_core.structure_block ref;
  mutable h_sec_env : Doc_core.section_names list;
  mutable h_type : string;
} 
and s_help = {
  mutable h_sections : h_sec list;
  mutable h_subsections : h_sec list;
  mutable h_units : h_sec list;
  mutable h_main : Doc_core.structure_block;
} 
val file_name : string -> string
val cur_help : s_help option ref
val syntax_error : Doc_core.structure_block -> string -> 'a
val help_of_doc : ds:Doc_core.structure_block -> s_help
val help_load : ds:Doc_core.structure_block -> unit
val help_write: fname:string -> unit
val help_read: fname:string -> unit
val help_file : fname:string -> unit
type help_device = Help_TTY | Help_ASCII | Help_HTML | Help_LATEX
val cur_dev : help_device ref
val help_dev : dev:help_device -> unit
exception Found
val help : name:string -> unit
