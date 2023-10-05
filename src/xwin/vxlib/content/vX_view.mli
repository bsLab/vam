class t :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
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
    method get_wob : VX_types.contained option
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
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_allocate_childs : unit
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit
    method update_size : unit
    method update_top_size : unit
    method update_x : int -> unit
    method update_y : int -> unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable child : VX_types.contained option
    val id : int
    val mutable init : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val view : Xtypes.geometry
    val w : VX_types.window
  end
class v :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method adjust_y : int -> unit
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method container_add_s : VX_types.contained list -> unit
    method container_exchange : int -> VX_types.contained -> unit
    method container_insert : int -> VX_types.contained -> unit
    method container_remove : int -> unit
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
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method items : VX_types.contained array
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method nitems : int
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_items : VX_types.contained array -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit
    method update_size : unit
    method update_slider_y : unit
    method update_top_size : unit
    method update_y : int -> unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val id : int
    val mutable init : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable slider_size : int
    val mutable slider_y : VX_slider.view_v option
    val mutable szhints : VX_types.szhints
    val mutable view_wob : VX_types.contained option
    val mutable viewport : t option
    val w : VX_types.window
  end
class h :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method adjust_x : int -> unit
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method container_add_s : VX_types.contained list -> unit
    method container_exchange : int -> VX_types.contained -> unit
    method container_insert : int -> VX_types.contained -> unit
    method container_remove : int -> unit
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
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method items : VX_types.contained array
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method nitems : int
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_items : VX_types.contained array -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit
    method update_size : unit
    method update_slider_x : unit
    method update_top_size : unit
    method update_x : int -> unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val id : int
    val mutable init : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable slider_size : int
    val mutable slider_x : VX_slider.view_h option
    val mutable szhints : VX_types.szhints
    val mutable view_wob : VX_types.contained option
    val mutable viewport : t option
    val w : VX_types.window
  end
class hbox :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method container_add_s : VX_types.contained list -> unit
    method container_exchange : int -> VX_types.contained -> unit
    method container_insert : int -> VX_types.contained -> unit
    method container_remove : int -> unit
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
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method items : VX_types.contained array
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method nitems : int
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_items : VX_types.contained array -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
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
    val id : int
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable szhints : VX_types.szhints
    val w : VX_types.window
  end
class hv :
  VX_types.container ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method adjust_x : int -> unit
    method adjust_y : int -> unit
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method contained : VX_types.contained
    method container : VX_types.container
    method container_add : VX_types.contained -> unit
    method container_add_s : VX_types.contained list -> unit
    method container_exchange : int -> VX_types.contained -> unit
    method container_insert : int -> VX_types.contained -> unit
    method container_remove : int -> unit
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
    method global_color_make : VX_types.color_desc -> bool -> VX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method image_get : string -> Ximage.t
    method image_make : string * VX_types.image_desc -> Ximage.t
    method inverse : unit
    method items : VX_types.contained array
    method iter : (VX_types.contained -> unit) -> unit
    method iter_visible : (VX_types.contained -> unit) -> unit
    method name : string
    method nitems : int
    method normal : unit
    method parent : VX_types.container
    method pixmap_get : string -> Xpm.pixmap
    method pixmap_make : string * VX_types.pixmap_desc -> Xpm.pixmap
    method print : VX_types.ps -> int -> int -> unit
    method print_frame : VX_types.ps -> int -> int -> unit
    method print_info : int -> unit
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : VX_types.screen_struct
    method set_items : VX_types.contained array -> unit
    method set_name : string -> unit
    method set_parent : VX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> bool
    method size_request : VX_types.szhints
    method to_refresh : VX_types.refresh_widget
    method to_resize : VX_types.resize_widget
    method update : unit
    method update_clipping : unit
    method update_size : unit
    method update_slider_x : unit
    method update_slider_y : unit
    method update_top_size : unit
    method update_x : int -> unit
    method update_y : int -> unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method win : VX_types.window
    method win_request : VX_types.window
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
    val mutable edge_fill : VX_box.h option
    val id : int
    val mutable init : bool
    val mutable parent : VX_types.container
    val s : VX_types.screen_struct
    val mutable slider_size : int
    val mutable slider_x : VX_slider.view_h option
    val mutable slider_y : VX_slider.view_v option
    val mutable szhints : VX_types.szhints
    val mutable view_wob : VX_types.contained option
    val mutable viewport : t option
    val w : VX_types.window
  end
