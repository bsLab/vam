val warn : string -> unit
val def_size : int
class orig :
  VX_types.box_desc ->
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable view_size : int
    val w : VX_types.window
  end
class t :
  VX_types.box_desc ->
  VX_types.container -> VX_types.widget_attributes list -> orig
class v :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable view_size : int
    val w : VX_types.window
  end
class h :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable view_size : int
    val w : VX_types.window
  end
class view_v :
  VX_types.container ->
  int ->
  int ->
  int ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_params : int -> int -> int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable view_size : int
    val w : VX_types.window
  end
class view_h :
  VX_types.container ->
  int ->
  int ->
  int ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_params : int -> int -> int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable view_size : int
    val w : VX_types.window
  end
class val_v :
  VX_types.container ->
  int ->
  int ->
  int ->
  int ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable val_large_step : float
    val mutable val_max : float
    val mutable val_min : float
    val mutable val_small_step : float
    val mutable view_size : int
    val w : VX_types.window
  end
class val_h :
  VX_types.container ->
  int ->
  int ->
  int ->
  int ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
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
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable val_large_step : float
    val mutable val_max : float
    val mutable val_min : float
    val mutable val_small_step : float
    val mutable view_size : int
    val w : VX_types.window
  end
class val_hf :
  VX_types.container ->
  float ->
  float ->
  float ->
  float ->
  VX_types.widget_attributes list ->
  object
    method action : unit
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method cursor_make : VX_types.cursor_desc -> bool -> VX_types.cursor
    method default_font : VX_types.font
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
    method get_pos : float
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
    method normal : unit
    method page_down : unit
    method page_up : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_but : VX_types.ps -> int -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_name : string -> unit
    method set_pagepos : int -> unit
    method set_parent : VX_types.container -> unit
    method set_pos : float -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_but : int -> bool -> unit
    method update_clipping : unit
    method update_size : unit
    method update_top_size : unit
    (*
    ** Reconfigure slider during runtime
    ** valmin valmax valsmall_step vallarge_step
    *)
    method val_config : float -> float -> float -> float -> unit
    
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable action_done : unit -> unit
    val mutable action_f : float -> unit
    val mutable action_i : int -> unit
    val mutable but_down : VX_types.frame
    val mutable but_drag : VX_types.frame
    val mutable but_size : int
    val mutable but_up : VX_types.frame
    val mutable down_active : bool
    val mutable drag_active : bool
    val id : int
    val mutable last_pos : int * int
    val mutable lastpos : float
    val mutable object_offset : int
    val mutable object_size : int
    val mutable page_size : int
    val mutable parent : VX_types.container
    val mutable pos : float
    val s : VX_types.screen_struct
    val mutable sym_down : VX_types.symbol
    val mutable sym_up : VX_types.symbol
    val mutable szhints : VX_types.szhints
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable up_active : bool
    val mutable val_large_step : float
    val mutable val_max : float
    val mutable val_min : float
    val mutable val_small_step : float
    val mutable view_size : int
    val w : VX_types.window
  end
