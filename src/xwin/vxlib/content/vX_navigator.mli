(*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ==================================
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF:
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     10.7.2005
**    $VERSION:     1.21
**
**    $INFO:
**
**  Frame based highlevel widget navigator for rapid prototyping.
**
**  
**  The navigator is handled like a book with chapters, sections and 
**  subsections. The chapter names are displayed on the top, the sections
**  on the left side. Subsections are used for partitioning the frame
**  content:
**
**      S1 S1 S1 S1
**  S2  +------------+
**  S2  | S3.        |
**  S2  | Content    |
**      |            |
**      +------------+
**
**
**  The user can navigate by clicking the section buttons. If there are
**  no S2 sections, the left button side is not displayed.
**
**  Three kinds of navigator widget classes exist:
**
**      s1      : only chapters, no s2/s3 sections
**      s12     : chapters and sections
**      s123    : three section depths - really a book
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common
open VX_view
open VX_slider

open Amoeba
open Stderr
open Stdcom
open Vtty_server
open Shell_exec
open Thread
open Myenv 
open Bytebuf 
open Printf   

type navigator_kind =
    | N_s1
    | N_s12
    | N_s123
    (*
    ** With additional content box view and a vertical scroll slider
    *)
    | N_s1_view
    | N_s12_view
    | N_s123_view
(*
** Public part
*)

(*
** Public part
*)
type attribute = VX_types.widget_attributes

type text = {
    text_str : string;
    text_attr : attribute list;
    mutable text_vx : VX_text.t option;
}

type input = {
    (*
    ** input label name and optional fixed label width for multiinput
    ** widget alignment ( if > 0 then fixed width in pixel, 
    ** else auto width)
    *)
    inp_desc : int * string;  
    inp_attr : attribute list;
    (*
    ** The input widget can contain one ore more buttons of
    ** type ActionSS (only one, inside editable line) or 
    ** ActionUU (several, just after the editable line). The
    ** string specifies the button labels (except in the ActionSS case,
    ** just an empty string).
    *)
    inp_buts  : (string * attribute) list; 
    (*
    ** The editable string - can be initialized
    *)
    mutable inp_str  : string;
    
    mutable inp_vx : VX_texttable.t option;
}

type file = {
    file_desc : string;
    mutable file_attr : attribute list;
    mutable file_action  : (string -> unit);
    mutable file_path  : Myenv.path_arg;
    file_edit : bool;       (* with editable line ? *)
    file_top : bool;        (* toplevel or child window ? *)
    mutable file_vx : VX_popup.file_select_edit_orig option; 
}

type select = {
    sel_desc : string;              (* Checkbox label name              *)
    mutable sel_attr : attribute list;
    sel_choices : string array;     (* all possible choices             *)
    sel_mutual : bool;              (* one or multiple choices          *)

    mutable sel_sel : int list;     (* user selected items              *)
    mutable sel_action : (int -> unit);     (* action handler called each time a 
                                       select occured *)
    mutable sel_vx : VX_checkbox.v option;
}

type buttons = {
    mutable but_attr : attribute list; 
    (*
    ** Buttons organzied in rows and columns.
    *)
    mutable but_cols : ((string * (unit -> unit)) array) array;
    mutable but_group : bool;
    mutable but_vx : VX_button.table option;
}

type log = {
    mutable log_attr : attribute list;
    (*
    ** Size of log srceen line array - not necessary the visible part
    *)
    mutable log_rows : int;
    (*
    ** Add lines to the log screen
    *)
    mutable log_add  : (string list -> unit) option;
    (*
    ** Clear screen
    *)
    mutable log_clear : (unit -> unit) option;
    
    mutable log_vx : VX_log.view_v option;
}

type logedit = {
    mutable logedit_attr : attribute list;
    (*
    ** Size of log srceen line array - not necessary the visible part
    *)
    mutable logedit_cols : int;
    mutable logedit_rows : int;
    (*
    ** Add lines to the log screen
    *)
    mutable logedit_add  : (string list -> unit) option;
    (*
    ** Clear screen
    *)
    mutable logedit_clear : (unit -> unit) option;

    
    mutable logedit_vx : VX_log.editview_v option;
}
                                                            
(*
** Process control. The exec_def structure can be found in the
** Shell_exec module (similar to Vamboot, but only for a single 
** process object).
*)
type process = {
    mutable pro_desc : string;
    mutable pro_attr : attribute list;
    pro_def : exec_def;
    mutable pro_log : (string list -> unit) option;
    mutable pro_obj : exec_obj option;
}

(*
** Text table origanized in rows and columns (cells). The cells can be
** editable.
*)
type table = {
    mutable tab_desc : string;
    mutable tab_attr : attribute list;          (* main attributes *)
    mutable tab_rows : (attribute list) array;  (* row attributes  *)
    (*
    ** cell content and cell attributes 
    *)
    mutable tab_cols : (string * (attribute list)) array array; 

    (*
    ** Function to change a cell and to get the actual content of
    ** the cell. The row and column must be specified (start index 0).
    ** Available after class creation!
    *)
    mutable tab_set : (int -> int -> (string * (attribute list)) -> unit)
                      option;
    mutable tab_get : (int -> int -> string) option;
    mutable tab_vx : VX_texttable.t option;
}

type drawing = {
    mutable draw_attr : attribute list;
    (*
    ** Add a drawing path
    *)
    mutable draw_add : (VX_draw.draw list -> unit) option;
    (*
    ** Delete (erase) a drawing path
    *)
    mutable draw_del : (int -> unit) option;

    (*
    ** Print the drawing to an eps file
    *)
    mutable draw_print : (string -> unit) option;

    mutable draw_vx : VX_draw.t option;
}


(*
** Slider for value adjustment
*)
type value = {
    mutable val_desc : string;
    (*
    ** Value unit (for example seconds) - displayed after actual value
    *)
    mutable val_unit : string;

    mutable val_attr : attribute list;
    (*
    ** Min/Max, smallest step and large step value settings
    *)
    mutable val_min  : float;
    mutable val_max  : float;
    mutable val_res  : float;
    mutable val_step : float;

    (*
    ** User supplied formatted value string
    *) 
    mutable val_print : (float -> string);
    (*
    ** Action handler with actual value
    *)
    mutable val_action : (float -> unit);

    (*
    ** Reconfigure parameters
    *)
    mutable val_config : (unit -> unit) option;

    (*
    ** Set new value between min and max value
    *)
    mutable val_set : (float -> unit) option;

    mutable val_vx : VX_slider.val_hf option;
}

type descriptor =
    | S1 of (string * descriptor list)      (* chapter              *)
    | S2 of (string * descriptor list)      (* section              *)
    | S3 of (string * descriptor list)      (* subsection           *)

    | Text of text                          (* text paragraph       *)
    | Input of input                        (* something to edit    *)
    | File of file                          (* file selector        *)
    | Select of select                      (* select boxes         *)
    | Buttons of buttons                    (* button box           *)
    | Log of  log                           (* log window           *)
    | Logedit of logedit                    (* editable log win.    *)
    | Proc of process                       (* process manager      *)
    | Table of table                        (* text tables          *)
    | Draw of drawing                       (* drawing              *)
    | Value of value                        (* Value slider         *)

    (*
    ** This action handler is called if a section was opened (selected).
    *)
    | Action of (unit -> unit)             


    | Space of int
    | Ruler of int


val text : string -> descriptor

(*
** Private part
*)

type section_depth =
    | Sec_1
    | Sec_2
    | Sec_3


type section = {
    sec_hbox : VX_box.h option;
    sec_vbox : VX_box.v option;
    (*
    ** Section content box 
    *)
    mutable sec_cbox : VX_box.v option;
    (*
    ** Clipped view of content box if any
    *)
    mutable sec_sbox : VX_view.v option;

    sec_button : VX_button.t option;
    sec_name   : string;
    sec_depth  : section_depth;
    mutable sec_child : section option;
    mutable sec_parent : section option;
    mutable sec_next : section option;  
}


class orig :
  navigator_kind ->
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    (*
    ** Iterate the descriptor list with all section and content
    ** informations and generate internal navigator tree.
    ** Create and add widgets, too.
    *) 
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    (*
    ** Actual useable width in frame vbox (excluding border and padding)
    *)
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option

    (*
    ** Holds S2 buttons and fillbox.
    *)
    val mutable left_vbox : VX_box.v option
    val mutable s2_vbox : VX_box.v option
    val mutable fill_vbox : VX_box.v option

    val mutable parent : VX_types.container
    (*
    ** Holds S1 buttons and content frame.
    *)
    val mutable right_vbox : VX_box.v option
    val mutable s1_hbox : VX_box.h option

    val root : VX_root.t
    val s : VX_types.screen_struct
    (*
    ** All the widgets we're handling
    *)
    val mutable sections : section list

    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s1 :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s12 :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s123 :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s1_view :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s12_view :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end

class s123_view :
  VX_root.t ->
  VX_types.container ->
  descriptor list ->
  VX_types.widget_attributes list ->
  object
    method actions : (VX_types.event_desc * VX_types.handler) list
    method background : VX_types.color
    method clear_items : unit
    method click_type : VX_types.click
    method color_make : string -> bool -> VX_types.color
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method configure_sections : descriptor list -> unit
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
    (*
    ** Get content container for a section S1 (first section has index 0)
    *)    
    method get_s1 : int -> VX_box.v
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
    method set_s1 : section -> unit
    method set_s2 : section -> unit
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
    val mutable fill_vbox : VX_box.v option
    val mutable frame_height : int
    val mutable frame_vbox : VX_box.v option
    val mutable frame_width : int
    val id : int
    val mutable init : bool
    val mutable last_s1 : section option
    val mutable last_s2 : section option
    val mutable left_vbox : VX_box.v option
    val mutable parent : VX_types.container
    val mutable right_vbox : VX_box.v option
    val root : VX_root.t
    val s : VX_types.screen_struct
    val mutable s1_hbox : VX_box.h option
    val mutable s2_vbox : VX_box.v option
    val mutable sections : section list
    val mutable szhints : VX_types.szhints
    val mutable updates : < update : unit; .. > list
    val w : VX_types.window
  end
