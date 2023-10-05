
type html_options = HTML_single_doc of string

val html_of_tree :
  ds:Doc_core.structure_block ->
  options:html_options list -> sections:Doc_core.section_names list -> unit
