type tex_options =
    TEX_doc_name of string
  | TEX_head_inline
  | TEX_color
  | TEX_no_toc
  | TEX_link_ref

val tex_of_tree :
  ds:Doc_core.structure_block ->
  options:tex_options list -> sections:Doc_core.section_names list -> unit
