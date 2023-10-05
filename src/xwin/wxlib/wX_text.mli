
(*
** Changes:
**              + pixmap_get 
**              + image_make/get
**
*)


module type Make_sig =
  sig
    type text
    and line
    and 'a tree_desc = {
      mutable tree_nlines : int;
      mutable tree_width : int;
      mutable tree_height : int;
      mutable tree_parts : 'a array;
      mutable tree_up : tree tree_desc;
      mutable tree_pos : int;
      mutable tree_modified : bool;
      mutable line_height : int;
      mutable line_width : int;
      mutable tree_text : text;
    } 
    and tree = Parts of tree tree_desc | Lines of line tree_desc
    and redraw = NoRedraw | Redraw of int * int * int * int | TotalRedraw
    val modify : 'a tree_desc -> unit
    class t :
      WX_types.container ->
      tree tree_desc ->
      WX_types.base_attributes list ->
      object
        val mutable font : WX_types.font
        val id : int
        val mutable parent : WX_types.container
        val mutable redraw_area : redraw
        val s : WX_types.screen_struct
        val mutable szhints : WX_types.szhints
        val mutable text : tree tree_desc
        val w : WX_types.window
        val mutable widgets : WX_types.contained list
        method actions : (WX_types.event_desc * WX_types.handler) list
        method background : WX_types.color
        method click_type : WX_types.click
        method color_make : string -> bool -> WX_types.color
        method configure : WX_types.base_attributes list -> unit
        method contained : WX_types.contained
        method container : WX_types.container
        method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
        method default_font : WX_types.font
        method destroy : unit
        method display : Xtypes.display
        method draw_relief : unit
        method focus : unit
        method font_make : string -> bool -> WX_types.font
        method foreground : WX_types.color
        method geometry : Xtypes.geometry
        method getHilite : WX_types.color -> WX_types.color
        method getShadow : WX_types.color -> WX_types.color
        method global_color_make :
          WX_types.color_desc -> bool -> WX_types.color
        method handle_button : unit -> unit
        method handle_key : unit -> unit
        method height : int
        method hide : unit
        method id : string
        method inverse : unit
        method iter : (WX_types.contained -> unit) -> unit
        method iter_visible : (WX_types.contained -> unit) -> unit
        method name : string
        method normal : unit
        method parent : WX_types.container
        method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
        method pixmap_get : string -> Xpm.pixmap
        method image_make : string * WX_types.image_desc -> Ximage.t
        method image_get : string -> Ximage.t
        method realize : unit
        method refresh : unit
        method reverse : bool
        method root_coordinates : int * int
        method screen : WX_types.screen_struct
        method set_parent : WX_types.container -> unit
        method set_text : tree tree_desc -> unit
        method set_font : WX_types.font -> unit 
        method show : unit
        method size_allocate : int -> int -> int -> int -> unit
        method size_request : WX_types.szhints
        method to_refresh : WX_types.refresh_widget
        method to_resize : WX_types.resize_widget
        method update : unit
        method update_size : unit
        method update_top_size : unit
        method wait_refresh :
          bool ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
        method wait_resize : unit
        method width : int
        method window : Xtypes.window
        method xevents : Xtypes.xevent -> unit
      end
    val make_text : text -> line array -> tree tree_desc
  end
type item =
    String of item_attr list * int
  | RealString of item_attr list * string
  | Widget of WX_types.contained array
and item_attr =
    Font of WX_types.font
  | Foreground of WX_types.color
  | Background of WX_types.color
module type Text =
  sig
    type line
    and text
    val representation : text -> line -> string * int
    val items : text -> line -> item array
  end
module Make :
  functor (Text : Text) ->
    sig
      type text = Text.text
      and line = Text.line
      and 'a tree_desc = {
        mutable tree_nlines : int;
        mutable tree_width : int;
        mutable tree_height : int;
        mutable tree_parts : 'a array;
        mutable tree_up : tree tree_desc;
        mutable tree_pos : int;
        mutable tree_modified : bool;
        mutable line_height : int;
        mutable line_width : int;
        mutable tree_text : text;
      } 
      and tree = Parts of tree tree_desc | Lines of line tree_desc
      and redraw = NoRedraw | Redraw of int * int * int * int | TotalRedraw
      val get_font : WX_types.font -> item_attr list -> WX_types.font
      val get_attrs :
        Xtypes.pixel * Xtypes.pixel * WX_types.font ->
        item_attr list -> Xtypes.pixel * Xtypes.pixel * WX_types.font
      val modify : 'a tree_desc -> unit
      class t :
        WX_types.container ->
        tree tree_desc ->
        WX_types.base_attributes list ->
        object
          val mutable font : WX_types.font
          val id : int
          val mutable parent : WX_types.container
          val mutable redraw_area : redraw
          val s : WX_types.screen_struct
          val mutable szhints : WX_types.szhints
          val mutable text : tree tree_desc
          val w : WX_types.window
          val mutable widgets : WX_types.contained list
          method actions : (WX_types.event_desc * WX_types.handler) list
          method background : WX_types.color
          method click_type : WX_types.click
          method color_make : string -> bool -> WX_types.color
          method configure : WX_types.base_attributes list -> unit
          method contained : WX_types.contained
          method container : WX_types.container
          method cursor_make :
            WX_types.cursor_desc -> bool -> WX_types.cursor
          method default_font : WX_types.font
          method destroy : unit
          method display : Xtypes.display
          method draw_relief : unit
          method focus : unit
          method font_make : string -> bool -> WX_types.font
          method foreground : WX_types.color
          method geometry : Xtypes.geometry
          method getHilite : WX_types.color -> WX_types.color
          method getShadow : WX_types.color -> WX_types.color
          method global_color_make :
            WX_types.color_desc -> bool -> WX_types.color
          method handle_button : unit -> unit
          method handle_key : unit -> unit
          method height : int
          method hide : unit
          method id : string
          method inverse : unit
          method iter : (WX_types.contained -> unit) -> unit
          method iter_visible : (WX_types.contained -> unit) -> unit
          method name : string
          method normal : unit
          method parent : WX_types.container
          method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
          method pixmap_get : string -> Xpm.pixmap
        method image_make : string * WX_types.image_desc -> Ximage.t
        method image_get : string -> Ximage.t
          method realize : unit
          method refresh : unit
          method reverse : bool
          method root_coordinates : int * int
          method screen : WX_types.screen_struct
          method set_parent : WX_types.container -> unit
          method set_text : tree tree_desc -> unit
          method set_font : WX_types.font -> unit 
          method show : unit
          method size_allocate : int -> int -> int -> int -> unit
          method size_request : WX_types.szhints
          method to_refresh : WX_types.refresh_widget
          method to_resize : WX_types.resize_widget
          method update : unit
          method update_size : unit
          method update_top_size : unit
          method wait_refresh :
            bool ->
            Xtypes.coord ->
            Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
          method wait_resize : unit
          method width : int
          method window : Xtypes.window
          method xevents : Xtypes.xevent -> unit
        end
      val make_text : text -> line array -> tree tree_desc
    end
module WidgetText :
  sig
    type text = unit
    and line = item array
    val representation : 'a -> 'b -> string * int
    val items : 'a -> 'b -> 'b
  end
module WidgetTree :
  sig
    type text = WidgetText.text
    and line = WidgetText.line
    and 'a tree_desc =
      'a Make(WidgetText).tree_desc = {
      mutable tree_nlines : int;
      mutable tree_width : int;
      mutable tree_height : int;
      mutable tree_parts : 'a array;
      mutable tree_up : tree tree_desc;
      mutable tree_pos : int;
      mutable tree_modified : bool;
      mutable line_height : int;
      mutable line_width : int;
      mutable tree_text : text;
    } 
    and tree =
      Make(WidgetText).tree =
        Parts of tree tree_desc
      | Lines of line tree_desc
    and redraw =
      Make(WidgetText).redraw =
        NoRedraw
      | Redraw of int * int * int * int
      | TotalRedraw
    val get_font : WX_types.font -> item_attr list -> WX_types.font
    val get_attrs :
      Xtypes.pixel * Xtypes.pixel * WX_types.font ->
      item_attr list -> Xtypes.pixel * Xtypes.pixel * WX_types.font
    val modify : 'a tree_desc -> unit
    class t :
      WX_types.container ->
      tree tree_desc ->
      WX_types.base_attributes list ->
      object
        val mutable font : WX_types.font
        val id : int
        val mutable parent : WX_types.container
        val mutable redraw_area : redraw
        val s : WX_types.screen_struct
        val mutable szhints : WX_types.szhints
        val mutable text : tree tree_desc
        val w : WX_types.window
        val mutable widgets : WX_types.contained list
        method actions : (WX_types.event_desc * WX_types.handler) list
        method background : WX_types.color
        method click_type : WX_types.click
        method color_make : string -> bool -> WX_types.color
        method configure : WX_types.base_attributes list -> unit
        method contained : WX_types.contained
        method container : WX_types.container
        method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
        method default_font : WX_types.font
        method destroy : unit
        method display : Xtypes.display
        method draw_relief : unit
        method focus : unit
        method font_make : string -> bool -> WX_types.font
        method foreground : WX_types.color
        method geometry : Xtypes.geometry
        method getHilite : WX_types.color -> WX_types.color
        method getShadow : WX_types.color -> WX_types.color
        method global_color_make :
          WX_types.color_desc -> bool -> WX_types.color
        method handle_button : unit -> unit
        method handle_key : unit -> unit
        method height : int
        method hide : unit
        method id : string
        method inverse : unit
        method iter : (WX_types.contained -> unit) -> unit
        method iter_visible : (WX_types.contained -> unit) -> unit
        method name : string
        method normal : unit
        method parent : WX_types.container
        method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
        method pixmap_get : string -> Xpm.pixmap
        method image_make : string * WX_types.image_desc -> Ximage.t
        method image_get : string -> Ximage.t
        method realize : unit
        method refresh : unit
        method reverse : bool
        method root_coordinates : int * int
        method screen : WX_types.screen_struct
        method set_parent : WX_types.container -> unit
        method set_text : tree tree_desc -> unit
        method set_font : WX_types.font -> unit 
        method show : unit
        method size_allocate : int -> int -> int -> int -> unit
        method size_request : WX_types.szhints
        method to_refresh : WX_types.refresh_widget
        method to_resize : WX_types.resize_widget
        method update : unit
        method update_size : unit
        method update_top_size : unit
        method wait_refresh :
          bool ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
        method wait_resize : unit
        method width : int
        method window : Xtypes.window
        method xevents : Xtypes.xevent -> unit
      end
    val make_text : text -> line array -> tree tree_desc
  end
class with_widgets :
  WX_types.container ->
  WidgetTree.line array ->
  WX_types.base_attributes list ->
  object
    val mutable font : WX_types.font
    val id : int
    val mutable parent : WX_types.container
    val mutable redraw_area : WidgetTree.redraw
    val s : WX_types.screen_struct
    val mutable szhints : WX_types.szhints
    val mutable text : WidgetTree.tree WidgetTree.tree_desc
    val w : WX_types.window
    val mutable widgets : WX_types.contained list
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method pixmap_get : string -> Xpm.pixmap
    method image_make : string * WX_types.image_desc -> Ximage.t
    method image_get : string -> Ximage.t
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_parent : WX_types.container -> unit
    method set_text : WidgetTree.tree WidgetTree.tree_desc -> unit
    method set_font : WX_types.font -> unit 
    method set_widgets : WidgetTree.line array -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
module SimpleText :
  sig
    type text = string
    and line = int * item array
    val representation : 'a -> 'b * 'c -> 'a * 'b
    val items : 'a -> 'b * 'c -> 'c
  end
module SimpleTree :
  sig
    type text = SimpleText.text
    and line = SimpleText.line
    and 'a tree_desc =
      'a Make(SimpleText).tree_desc = {
      mutable tree_nlines : int;
      mutable tree_width : int;
      mutable tree_height : int;
      mutable tree_parts : 'a array;
      mutable tree_up : tree tree_desc;
      mutable tree_pos : int;
      mutable tree_modified : bool;
      mutable line_height : int;
      mutable line_width : int;
      mutable tree_text : text;
    } 
    and tree =
      Make(SimpleText).tree =
        Parts of tree tree_desc
      | Lines of line tree_desc
    and redraw =
      Make(SimpleText).redraw =
        NoRedraw
      | Redraw of int * int * int * int
      | TotalRedraw
    val get_font : WX_types.font -> item_attr list -> WX_types.font
    val get_attrs :
      Xtypes.pixel * Xtypes.pixel * WX_types.font ->
      item_attr list -> Xtypes.pixel * Xtypes.pixel * WX_types.font
    val modify : 'a tree_desc -> unit
    class t :
      WX_types.container ->
      tree tree_desc ->
      WX_types.base_attributes list ->
      object
        val mutable font : WX_types.font
        val id : int
        val mutable parent : WX_types.container
        val mutable redraw_area : redraw
        val s : WX_types.screen_struct
        val mutable szhints : WX_types.szhints
        val mutable text : tree tree_desc
        val w : WX_types.window
        val mutable widgets : WX_types.contained list
        method actions : (WX_types.event_desc * WX_types.handler) list
        method background : WX_types.color
        method click_type : WX_types.click
        method color_make : string -> bool -> WX_types.color
        method configure : WX_types.base_attributes list -> unit
        method contained : WX_types.contained
        method container : WX_types.container
        method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
        method default_font : WX_types.font
        method destroy : unit
        method display : Xtypes.display
        method draw_relief : unit
        method focus : unit
        method font_make : string -> bool -> WX_types.font
        method foreground : WX_types.color
        method geometry : Xtypes.geometry
        method getHilite : WX_types.color -> WX_types.color
        method getShadow : WX_types.color -> WX_types.color
        method global_color_make :
          WX_types.color_desc -> bool -> WX_types.color
        method handle_button : unit -> unit
        method handle_key : unit -> unit
        method height : int
        method hide : unit
        method id : string
        method inverse : unit
        method iter : (WX_types.contained -> unit) -> unit
        method iter_visible : (WX_types.contained -> unit) -> unit
        method name : string
        method normal : unit
        method parent : WX_types.container
        method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
        method pixmap_get : string -> Xpm.pixmap
        method image_make : string * WX_types.image_desc -> Ximage.t
        method image_get : string -> Ximage.t
        method realize : unit
        method refresh : unit
        method reverse : bool
        method root_coordinates : int * int
        method screen : WX_types.screen_struct
        method set_parent : WX_types.container -> unit
        method set_text : tree tree_desc -> unit
        method set_font : WX_types.font -> unit 
        method show : unit
        method size_allocate : int -> int -> int -> int -> unit
        method size_request : WX_types.szhints
        method to_refresh : WX_types.refresh_widget
        method to_resize : WX_types.resize_widget
        method update : unit
        method update_size : unit
        method update_top_size : unit
        method wait_refresh :
          bool ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
        method wait_resize : unit
        method width : int
        method window : Xtypes.window
        method xevents : Xtypes.xevent -> unit
      end
    val make_text : text -> line array -> tree tree_desc
  end
val make_simple_text :
  SimpleTree.text -> SimpleTree.tree SimpleTree.tree_desc
val file_to_stext : string -> SimpleTree.tree SimpleTree.tree_desc
class t :
  WX_types.container ->
  SimpleTree.tree SimpleTree.tree_desc ->
  WX_types.base_attributes list ->
  object
    val mutable font : WX_types.font
    val id : int
    val mutable parent : WX_types.container
    val mutable redraw_area : SimpleTree.redraw
    val s : WX_types.screen_struct
    val mutable szhints : WX_types.szhints
    val mutable text : SimpleTree.tree SimpleTree.tree_desc
    val w : WX_types.window
    val mutable widgets : WX_types.contained list
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method pixmap_get : string -> Xpm.pixmap
    method image_make : string * WX_types.image_desc -> Ximage.t
    method image_get : string -> Ximage.t
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_parent : WX_types.container -> unit
    method set_text : SimpleTree.tree SimpleTree.tree_desc -> unit
    method set_font : WX_types.font -> unit 
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class of_file :
  WX_types.container ->
  string ->
  WX_types.base_attributes list ->
  object
    val mutable font : WX_types.font
    val id : int
    val mutable parent : WX_types.container
    val mutable redraw_area : SimpleTree.redraw
    val s : WX_types.screen_struct
    val mutable szhints : WX_types.szhints
    val mutable text : SimpleTree.tree SimpleTree.tree_desc
    val w : WX_types.window
    val mutable widgets : WX_types.contained list
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method pixmap_get : string -> Xpm.pixmap
    method image_make : string * WX_types.image_desc -> Ximage.t
    method image_get : string -> Ximage.t
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_file : string -> unit
    method set_parent : WX_types.container -> unit
    method set_text : SimpleTree.tree SimpleTree.tree_desc -> unit
    method set_font : WX_types.font -> unit 
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class of_string :
  WX_types.container ->
  SimpleTree.text ->
  WX_types.base_attributes list ->
  object
    val mutable font : WX_types.font
    val id : int
    val mutable parent : WX_types.container
    val mutable redraw_area : SimpleTree.redraw
    val s : WX_types.screen_struct
    val mutable szhints : WX_types.szhints
    val mutable text : SimpleTree.tree SimpleTree.tree_desc
    val w : WX_types.window
    val mutable widgets : WX_types.contained list
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method pixmap_get : string -> Xpm.pixmap
    method image_make : string * WX_types.image_desc -> Ximage.t
    method image_get : string -> Ximage.t
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_lines : SimpleTree.text -> unit
    method set_string : SimpleTree.text -> unit
    method set_parent : WX_types.container -> unit
    method set_text : SimpleTree.tree SimpleTree.tree_desc -> unit
    method set_font : WX_types.font -> unit 
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
