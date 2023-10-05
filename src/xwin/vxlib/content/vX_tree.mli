val warn : string -> unit
val def_size : int
val no_refresh : bool ref
type tree = Node of node | Leaf of leaf
and node = {
  mutable b_opened : bool;
  mutable b_prev : tree option;
  mutable b_next : tree option;
  mutable b_child : tree option;
  mutable b_parent : tree option;
  mutable b_but : VX_types.frame;
  mutable b_label : string;
  mutable b_frame : VX_types.frame;
  mutable b_symbol : VX_types.symbol;
  mutable b_selected : bool;
} 
and leaf = {
  mutable l_label : string;
  mutable l_frame : VX_types.frame;
  mutable l_prev : tree option;
  mutable l_next : tree option;
  mutable l_parent : tree option;
  mutable l_selected : bool;
} 
class orig :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method close_dir : string -> unit
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
    method deselect_all : unit
    method destroy : unit
    method display : Xtypes.display
    method draw_frame : unit
    method focus : unit
    method font_make :
      VX_types.text_font ->
      VX_types.text_style -> VX_types.text_size -> bool -> VX_types.font
    method foreground : VX_types.color
    method geometry : Xtypes.geometry
    method getHilite : VX_types.color -> VX_types.color
    method getShadow : VX_types.color -> VX_types.color
    method get_node : string -> node
    method get_path : tree -> string
    method get_root : node
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method new_leaf : string -> leaf
    method new_node : string -> node
    method normal : unit
    method open_dir : string -> unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_action : VX_types.widget_attributes list -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method tree_add : node -> tree -> unit
    method tree_add_s : node -> tree list -> unit
    method tree_insert : tree -> tree -> unit
    method update : unit
    method update_clipping : unit
    method update_elem : tree -> unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable but_size : int
    val mutable close_fun : string -> unit
    val id : int
    val mutable last_size : int * int
    val mutable mutual : bool
    val mutable open_fun : string -> unit
    val mutable pad_x : int
    val mutable pad_y : int
    val mutable parent : VX_types.container
    val mutable path_sep : string
    val s : VX_types.screen_struct
    val mutable select : bool
    val mutable select_fun : string -> unit
    val mutable selected : tree list
    val mutable size_modified : bool
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable tree : tree
    val w : VX_types.window
  end
class t : VX_types.container -> VX_types.widget_attributes list -> orig
