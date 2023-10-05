
type text_options = TEXT_doc_name of string | TEXT_terminal | TEXT_notoc

val text_of_tree :
  ds:Doc_core.structure_block ->
  options:text_options list -> sections:Doc_core.section_names list -> unit
