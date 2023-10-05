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
**    $CREATED:     29.7.2005
**    $VERSION:     1.06
**
**    $INFO:
**
** VX_log.t:
**
** Log window with fixed number of rows. The text is organized in
** lines. The displayed text window supports scrolling. That means,
** if the log screen is full, and another line is added, the screen
** content is shifted one line up, and the previously first line is
** removed. Attribute Rown 0 enables automatic height expansion (if
** log array is actually not filled up)!
**
**  method add_lines sl -> 
**    adds new lines to log window
**
**  method set_line rownum s -> 
**    modifies 'rownum' line (Top line is # 0)
**
**  method get_lines -> 
**    returns (text_lines,head,tail)
**
**  method get_line rownum -> 
**    returns 'rownum' line
**
**  method last_row -> 
**    last line number actuall used
**
**  method clear_log -> 
**    clears log line array
**
**
** VX_log.view_v:
**
** Hybrid widget consisting of VX_log.t log window contained in vertical 
** VX_view.v viewport with scrollbar.
** 
**   method hide -> 
**     hides window
**
**   method destroy -> 
**     destroys window
** 
** VX_log.edit:
**
** Same as class t, but content can be edited. Each line is treated
** independently! The cursor can be moved both in x- and y-direction.
** But if a line is filled up or a NL was entered (cols #), 
** no line jump is performed!
** This must be implemented by the client application using the
** action handlers.
** 
**   method update_cursor -> 
**     refresh cursor
**
**   method set_cursor x y -> 
**     set cursor to new position (col/row)
**
**   method set_editrange x0 y0 y1 -> 
**     sets editable are (col/rows)
**     (Default: full window area)
**
**   method set_action_nl (fun row -> ()) -> 
**     set NL action handler 
**
**   method set_action_left (fun row -> ()) -> 
**     action handler is called each time the cursor is tried to
**     move left, but actual position is already 0 
**
**   method set_action_right (fun col row -> ()) -> 
**     action handler is called each time the cursor is tried to
**     move right, but actual position is already end of line 
**
**   method set_action_up (fun row -> ()) -> 
**     action handler is called each time the cursor is tried to
**     move up, but actual position is already at the top of the 
**     editable area
**
**   method set_action_down (fun col row -> ()) -> 
**     action handler is called each time the cursor is tried to
**     move right, but actual position is already at the bottom of
**     the editable area
**                                           
**   method set_edit on ->
**     Enables or disables line editing. If disabled, the cursor 
**     disapears.
**
** VX_log.editview.v
**
** Same as edit, but in viewport frame.
**
**    $ENDOFINFO
**
*)

class orig :
  VX_types.container ->
  int ->
  VX_types.widget_attributes list ->
  object
    inherit VX_object.t
    
    method add_lines : string list -> unit
    method clear_log : unit
    method configure :
      VX_types.widget_attributes list -> VX_types.widget_attributes list
    method get_line : int -> string
    method get_lines : string array * int * int
    method last_row : int 
    method set_line : int -> string -> unit
    method print : VX_types.ps -> int -> int -> unit
    method refresh : unit
    method size_request : VX_types.szhints
    method text_width : int
    method update : unit
    (*
    **  The cursor got a new position. Restore old position,
    **  draw new position.
    *)
    method update_cursor : unit
    method update_screen : int -> int -> unit
    val mutable fixed : bool
    val mutable full : bool
    val mutable head : int
    val mutable last_text_width : int
    val mutable line_spacing : int
    val mutable tail : int
    val mutable text_font : VX_types.font
    val mutable text_font_symbol : VX_types.font
    val mutable text_lines : string array
    val mutable text_rows : int
    val mutable used : int
    val mutable edit : bool
    val mutable cursor_x : int
    val mutable cursor_y : int
    val mutable display_cursor : bool
    val mutable last_cursor : (int * int * char) option    
    val cursor : string
    
  end

class t :
  VX_types.container -> int -> VX_types.widget_attributes list -> orig

class view_v :
  VX_types.container ->
  int ->
  VX_types.widget_attributes list ->
  object
    inherit VX_view.v
    method add_lines : string list -> unit
    method clear_log : unit
    method destroy : unit
    method get_line : int -> string
    method get_lines : string array * int * int
    method hide : unit
    method last_row : int 
    method set_line : int -> string -> unit
    method refresh : unit
    val mutable log : t option
  end

class edit :
  VX_types.container -> int -> int -> VX_types.widget_attributes list ->
  object
    inherit orig

    method set_action_nl : (int -> unit) -> unit
    method set_action_left : (int -> unit) -> unit
    method set_action_right : (int -> int -> unit) -> unit
    method set_action_up : (int -> unit) -> unit
    method set_action_down : (int -> unit) -> unit
    method set_edit : bool -> unit

    method cursor : bool -> unit    
    method set_cursor : int -> int -> unit
    (*
    ** Set editable area : x0 -> y0 -> y1 (col/row units)
    *)
    method set_editrange : int -> int -> int -> unit
    
                    
    val mutable text_cols : int
    val mutable edit_x0 : int
    val mutable edit_y0 : int
    val mutable edit_y0 : int
  end

class editview_v :
  VX_types.container ->
  int -> int ->
  VX_types.widget_attributes list ->
  object
    inherit VX_view.v
    method add_lines : string list -> unit
    method clear_log : unit
    method destroy : unit
    method get_line : int -> string
    method get_lines : string array * int * int
    method hide : unit
    method last_row : int 
    method set_line : int -> string -> unit
    method refresh : unit
    val mutable log : edit option

    (*
    ** Action handlers
    *)
    method set_action_nl : (int -> unit) -> unit
    method set_action_left : (int -> unit) -> unit
    method set_action_right : (int -> int -> unit) -> unit
    method set_action_up : (int -> unit) -> unit
    method set_action_down : (int -> unit) -> unit
    method set_edit : bool -> unit

    method set_cursor : int -> int -> unit
    (*
    ** Set editable area : x0 -> y0 -> y1 (col/row units)
    *)
    method set_editrange : int -> int -> int -> unit
    
    (*
    **  The cursor got a new position. Restore old position,
    **  draw new position.
    *)
    method update_cursor : unit
    method cursor : bool -> unit

  end
