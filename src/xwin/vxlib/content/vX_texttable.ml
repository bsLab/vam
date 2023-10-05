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
**    $CREATED:     24.5.2005
**    $VERSION:     1.35
**
**    $INFO:
**
**  Text table widget. This widget supports text cells either editable
**  or not. The font, the border lines and the width of each cell can
**  be specified. Furthermore, a user supplied callback function can 
**  be specified. Each cell can include a button. The button behaviour
**  depends on the attribute settings:
**
**  1. A status button. The current status of the (editable) text entry
**     is displayed (Modified, Error, Ok...). If the text was modified,
**     the status is updated (Modified). If the user presses teh button,
**     an installed callback function (ActionSSS) is called with the current
**     status and string. This function returns a new status, which is
**     displayed.
**
**
**  2. The button can also be used (for non editable entries) to popup other
**     widgets (another texttable, for example), specified with the
**     Widget attribute and optional ActionUP and ActionDOWN callback
**     functions.  
**
**  3. Generic button with label and action handler (ActionUU).
**
**  4. Generic action handler without a button (ActionSU) called each time
**     the cell content was modified.
**
**  The height of a row can be specified. If this specification is missing,
**  the height is determined automatically from the padding, border and
**  font specification. The maximal value from all column entries of one
**  row is used.
**
**
**  Regular tables: all rows have same number of columns, and
**                  all entries of one column have the same width.
**
**
**  The width of each column entry can be specified with the Width
**  attribute inside the Cols attribute. Either all entries
**  from one column have a specified width (that means the width is fixed),
**  or all entries have width 0 (not specified). All entries from one
**  column must have the same width (regular table), except all 
**  entries have a specified width (irregular table)!
**
**  Column entries can be expanded, specified with the ExpandX attribute
**  inside the Cols attribute. 
**  Editable columns entries have either a specified fixed width or will be
**  expanded according the window size (they consume all available space).
**
**  Both padding of the table inside widget container AND padding of
**  cell content of each entry is specified with IpadX and IpadY attributes
**  in main attribute list (not inside Cols/Rows). IpadY inside Rows
**  attributes specify additional space between rows!!!  
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common
open VX_text
open Printf
open GCCache

let warn str =
    print_string ("Warning: "^str);
    print_newline ()



(*
** Status of an editable column entry. Displayed in the right
** side of the text field. Each status (Submitted, Modified,...)
** corresponds to a symbol. The symbols displayed must be specifed. 
** Empty symbol list = blank button field.
*)
type status_button = {
    (*
    ** Callback function called if button was pressed.
    *)
    mutable sb_fun : (string -> status -> status);

    (*
    ** The status symbols displayed. If a status symbol is not
    ** contained in the list, the button keeps blank.
    *)
    mutable sb_syms : symbol_type list;    

    (*
    ** Frame box of button
    *)
    mutable sb_frame: frame;
    mutable sb_color: color;
    mutable sb_active: bool;
}

(*
** Generic labeled button with action handler.
*)
type label_button = {
    (*
    ** Callback function called if button was pressed.
    *)
    mutable lb_fun : (unit -> unit);

    (*
    ** Label string and fonts
    *)
    mutable lb_label : string;
    mutable lb_font: font;
    mutable lb_font_symbol : font;
    (*  
    ** Label padding. Default: tb_col_pad_X
    *)
    mutable lb_pad_x : int;
    mutable lb_pad_y : int;
    (*
    ** Bounding box of button
    *)
    mutable lb_frame: frame;
    mutable lb_color: color;
    mutable lb_active: bool;

}

(*
** Popup widget button 
*)
type popup_button = {
    (*
    ** Callback function called if widget window is opened and closed.
    *)
    mutable pb_fun_up : (unit -> unit);
    mutable pb_fun_down : (unit -> unit);

    (*
    ** Bounding box of button
    *)
    mutable pb_frame: frame;
    mutable pb_color: color;
    mutable pb_active: bool;

    (*
    ** Widget window opened (popuped?)
    *)
    mutable pb_opened : bool;
    
    (*
    ** The widget contained
    *)
    mutable pb_widget : VX_types.contained option;
    mutable pb_realization : VX_top.t option;
}


type button = 
    | Status_but of status_button
    | Label_but of label_button
    | Popup_but of popup_button

(*
** One column entry of a row
*)
type col_desc = {
    (*
    ** Content
    *)
    (*
    ** The content string can contain embedded symbols if not editable:
    **  "The distance in \\mu meters" 
    ** The symbols are displayed with the symbol_font instead.
    ** The symbols are specified in TeX notation.
    *)
    mutable col_str     : string;
    mutable col_lines   : string list;  (* multiline text   *)
    mutable col_multi_line : bool;

    (*
    ** Single or multiline texts:
    **  Number of text rows  = 0 -> auto height calculated multiline text
    **                       = 1 -> single line
    **                       > 1 -> fixed multiline height
    *)
    mutable col_rows    : int;

    mutable col_font        : font;         (* normal text font *)
    mutable col_symbol_font : font;     (* embedded symbols font *)
    mutable col_align_x     : align;
    mutable col_align_y     : align;
    mutable col_fg          : color;
    mutable col_bg          : color;

    (*
    ** Border descriptor
    *)
    mutable col_frame: frame;

    (*
    ** Requested width in pixel including 
    ** border and column padding. Can be zero. Either the column
    ** entry width is fixed or expandable.
    *)
    mutable col_width : int;
    mutable col_expand : bool;
    mutable col_fixed : bool;

    mutable col_edit : bool;
    mutable col_baseline : text_baseline option;

    (*
    ** An optional button
    *)
    mutable col_but: button option;


    (*
    ** Informations at runtime
    *)

    (*
    ** Cursor text position
    *)
    mutable col_cursor: int;
    mutable col_cursor_display: bool;
    
    mutable col_lastpad : int;

    (*
    ** Current status of entry (modified ?)
    *)
    mutable col_status: status;

    (*
    ** generic action handler called eacht time the cell content
    ** was modified
    *)
    mutable col_action : string -> unit;    

}

type row_desc = {
    (*
    ** each row contains 1..n columns. 
    *)
    mutable row_cols : col_desc array;

    (*
    ** Requested height in pixel including 
    ** border and column padding
    *)
    mutable row_height : int;
    
    mutable row_width : int;

    (*
    ** Pad space after this row
    *)
    mutable row_pad : int;
}

type table_desc = {
    mutable tb_rows : row_desc array;
    mutable tb_colpad_x : int;
    mutable tb_colpad_y : int;
    (*
    ** Only in case of multiline text with non fixed height the
    ** table widget must be resized if the height of a column entry
    ** changed.
    *)
    mutable tb_height_expand : bool;
}


let def_but_size = 18

(*
** The table widget.
*)

class orig parent root (table : table_desc) attributes =
    object (self)

    inherit VX_object.t parent attributes as super

    (*
    ** Popup button implementation needs the root widget!
    *)
    val root = root

    (*
    ** Cursor and user interaction utilities. Only one table entry
    ** can get the input focus (cursor set).
    *)
    val mutable display_cursor = false  
    val cursor = String.create 1

    (*
    ** Last active (row,col) editable entry if any.
    *)
    val mutable last_active = None


    (*
    ** For multiline text fields
    *)
    val mutable line_spacing = 1

    (*
    ** Initialize this widget. The column entry widths can be either static 
    ** specified and already be calculated, or determined automatically. 
    ** The row height can be specified and already calculated or 
    ** adjusted automatically, too.
    *)

    initializer
        self#set_name "text_table";
        self#col_iter (fun row_i col_j row col ->
            if col.col_rows = 0 || col.col_rows > 1 then
            begin
                (*
                ** Format multiline text
                *)
                col.col_multi_line <- true;
                let lines,newstr,last_pad = 
                        format_lines col.col_font
                                     (atoms_of_str col.col_str)
                                     (self#text_width row_i col_j) in
                if col.col_edit then
                    col.col_lastpad <- if col.col_rows = 0 ||
                                       (List.length lines) <= col.col_rows
                                        then last_pad
                                        else 0;
                col.col_lines <- lines;
                col.col_str <- newstr;

            end;
            (*
            ** Update cell and button frame colors
            *)
            
            if col.col_fg = noColor then
                col.col_fg <- w.w_foreground;
            if col.col_bg = noColor then
                col.col_bg <- w.w_background;

            if col.col_but <> None then
            begin
                let but = self#but_frame row_i col_j in 
                let color = 
                    match (get_some col.col_but) with
                    | Status_but but -> but.sb_color;
                    | Label_but but -> but.lb_color;
                    | Popup_but but -> but.pb_color;
                    in
                match but.f_type with
                | ReliefRaised
                | ReliefSunken ->
                        but.f_foreground <- 
                            parent#getShadow 
                                (if color <> noColor 
                                    then color 
                                    else col.col_bg);

                        but.f_auxiliary <- 
                            parent#getHilite 
                                (if color <> noColor 
                                    then color 
                                    else col.col_bg);

                        but.f_fillground <- 
                            if color <> noColor then color else col.col_bg;
                        but.f_background <- 
                            col.col_bg;
                | _ ->
 
                        but.f_foreground <- 
                            col.col_fg;
                        but.f_fillground <- 
                            if color <> noColor then color else col.col_bg;
                        but.f_background <- 
                            col.col_bg;
            end;
            (*
            ** Update button frames
            *)

            if col.col_but <> None then
            begin
                match (get_some col.col_but) with
                | Label_but but ->
                (*
                ** Recalculate width and height
                *)
                let tw = string_width_S but.lb_font
                                        but.lb_font_symbol
                                        but.lb_label in
                let th = but.lb_font.font_height in
                            
                let bframe = frame_size but.lb_frame in
                let bframe_off = frame_offset but.lb_frame in
                           
                let bw = tw + 2 * table.tb_colpad_x + bframe in
                let bh = th + 2 * table.tb_colpad_y + bframe in

                but.lb_frame.f_bbox <- bbox_of_xywh 0 0 bw bh;
                | _ -> ();
            end;
            );
        (*
        ** User interaction: mouse & keyboard
        *)
        __(self#configure [Bindings [
          PointerMotion,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                self#update_cursor x y 0;
            );
          EnterWindow,(fun _ ->
                display_cursor <- true;
                self#update_cursor (-1) (-1) 0;
            );
          LeaveWindow,(fun _ ->
                display_cursor <- false;
                self#update_cursor (-1) (-1) 0;
            );
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                protect (
                    self#col_iter (fun row_i col_j row col ->
                    let col = self#get_col row_i col_j in
                    if col.col_but <> None then
                    begin
                        match (get_some col.col_but) with
                        | Status_but but ->
                        let within = within_bbox but.sb_frame.f_bbox x y in
                        if within && not but.sb_active then
                        begin
                            but.sb_active <- true;
                            self#update_but row_i col_j;
                            col.col_status <- but.sb_fun col.col_str 
                                                         col.col_status;
                            self#update_but row_i col_j;
                            raise Exit;
                        end;
                        | Popup_but but ->
                        let within = within_bbox but.pb_frame.f_bbox x y in
                        if within && not but.pb_active then
                        begin
                            but.pb_active <- true;
                            but.pb_opened <- not but.pb_opened;                                
                            self#update_but row_i col_j;
                            if but.pb_opened then
                            begin
                                let x0,y0 = self#root_coordinates in
                                
                                self#popup row_i col_j 
                                           (x0+but.pb_frame.f_bbox.x2)
                                           (y0+but.pb_frame.f_bbox.y1)
                            end
                            else
                            begin
                                self#popdown row_i col_j;
                            end;
                            raise Exit;
                        end;
                        | Label_but but ->
                        let within = within_bbox but.lb_frame.f_bbox x y in
                        if within && not but.lb_active then
                        begin
                            but.lb_active <- true;
                            self#update_but row_i col_j;
                            but.lb_fun ();
                            raise Exit;
                        end;
                    end;
                    (*
                    ** Cursor positioning ?
                    *)
                    if col.col_edit then
                    begin
                        let within = within_bbox col.col_frame.f_bbox x y in
                        if within then
                        begin
                            let x0,y0,width,height = bbox_to_xywh
                                                     col.col_frame.f_bbox in
                            let frame,frame_off = 
                                    frame_size col.col_frame,
                                    frame_offset col.col_frame in
                            let text_str = col.col_str^" " in
                            let text_align = col.col_align_x in
                            let text_font = col.col_font in
                            let pad_x = table.tb_colpad_x in
                            if not col.col_multi_line then
                            begin
                                let tw = string_width text_font 
                                                      (text_str^" ") in
                                let tx0 = 
                                    match text_align with
                                    | Left -> x0 + frame_off + pad_x
                                    | Center -> x0 + (width - frame - tw)/2
                                    | Right -> x0 + width - tw - 2 - 
                                                    frame_off - pad_x
                                    | _ ->  x0 + frame_off + pad_x
                                    in
                               if (x >= tx0 && x <= (tx0+tw)) then
                               begin
                                    (*
                                    ** Restore text under old position
                                    *)
                                    display_cursor <- false;
                                    self#update_cursor x y 0;

                                    protect (     
                                    (*
                                    ** Find new position in text string...
                                    *)
                                    for i = 1 to (String.length text_str)   
                                    do
                                        let str' = String.sub text_str 0 i in
                                        let tw' = string_width text_font     
                                                               str' in       
                                        if x <= (tx0 + tw') then
                                        begin
                                            col.col_cursor <- i-1;
                                            raise Exit;
                                        end;
                                    done);
                                    display_cursor <- true;
                                    self#update_cursor x y 0;
                                end;
                            end;
                            raise Exit;
                        end;
                    end;
            )));

          ButtonReleased,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                protect (
                    self#col_iter (fun row_i col_j row col ->
                    let col = self#get_col row_i col_j in

                    if col.col_but <> None then
                    begin
                        match (get_some col.col_but) with
                        | Status_but but ->
                        let within = within_bbox but.sb_frame.f_bbox x y in
                        if within then
                        begin
                            but.sb_active <- false;
                            self#update_but row_i col_j;
                        end;
                        | Popup_but but ->
                        let within = within_bbox but.pb_frame.f_bbox x y in
                        if within then
                        begin
                            but.pb_active <- false;
                            self#update_but row_i col_j;
                        end;
                        | Label_but but ->
                        let within = within_bbox but.lb_frame.f_bbox x y in
                        if within then
                        begin
                            but.lb_active <- false;
                            self#update_but row_i col_j;
                        end;
                    end;

            )));
          Key (XK.xk_Left, 0), (fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                self#update_cursor x y (-1);  
            );
          Key (XK.xk_Right, 0), (fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                self#update_cursor x y 1;   
            );
          Key (XK.xk_BackSpace, 0), (fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                match last_active with
                | Some (row_i,col_j) ->
                begin
                    let col = self#get_col row_i col_j in

                    let text_font = col.col_font in
                    let cursor_x = col.col_cursor in
                    let text_lastpad = col.col_lastpad in
                    if cursor_x > 0 then
                    begin
                        (*
                        ** First restore text under cursor
                        *)
                        display_cursor <- false;
                        self#update_cursor x y 0;
                        (*
                        ** Delete character before current cursor
                        ** position.
                        *)
                        self#set_text row_i col_j  (
                            let s = col.col_str in
                            let len = String.length s in
                            (String.sub s 0 (cursor_x-1)) ^ 
                            (String.sub s (cursor_x) (len - cursor_x))
                            );
                        col.col_cursor <- col.col_cursor - 1;
                        let update = col.col_status <> St_Modified in
                        col.col_status <- St_Modified;

                        (*
                        ** The content of this column entry was changed.
                        ** Redraw the column entry.
                        *)
                        self#update_col row_i col_j;
                        (*
                        ** Redraw cursor at new position
                        *)
                        display_cursor <- true;
                        self#update_cursor x y 0;
                        (*
                        ** Update button if any and necessary
                        *)    
                        if update then
                            self#update_but row_i col_j;
                        (*
                        ** Call external action handler if any
                        *)
                        col.col_action col.col_str;
                    end;
                end;
                | None -> ();
            );

          Key (XK.xk_Delete, 0), (fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                match last_active with
                | Some (row_i,col_j) ->
                begin
                    let col = self#get_col row_i col_j in
            
                    let text_font = col.col_font in
                    let cursor_x = col.col_cursor in
                    let text_lastpad = col.col_lastpad in
                    if cursor_x < (String.length col.col_str) then
                    begin
                        (*
                        ** First restore text under cursor
                        *)
                        display_cursor <- false;
                        self#update_cursor x y 0;
                        (*
                        ** Delete character under current cursor
                        ** position.
                        *)
                        self#set_text row_i col_j (
                            let s = col.col_str in
                            let len = String.length s in
                            (String.sub s 0 cursor_x) ^ 
                            (String.sub s (cursor_x+1) (len - 1 - cursor_x))
                            );

                        col.col_status <- St_Modified;
                        (*
                        ** The content of this column entry was changed.
                        ** Redraw the column entry.
                        *)
                        self#update_col row_i col_j;
                        (*
                        ** Redraw cursor at new position
                        *)
                        display_cursor <- true;
                        self#update_cursor x y 0;
                        (*
                        ** Call external action handler if any
                        *)
                        col.col_action col.col_str;
                    end;
                end;
                | None -> ();
            );

          Key (anyKey, anyModifier), (fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                (*
                ** Enough space for a new character?
                *)
                if !key_string <> "" then
                match last_active with
                | Some (row_i,col_j) ->
                begin
                    let col = self#get_col row_i col_j in
                    
                    let text_font = col.col_font in
                    let cursor_x = col.col_cursor in
                    let text_lastpad = col.col_lastpad in

                    if text_lastpad > text_font.font_width then
                    begin
                        (*
                        ** First restore text under cursor
                        *)    
                        display_cursor <- false;
                        self#update_cursor x y 0;
                        (*
                        ** Insert new text
                        *)
                        self#set_text row_i col_j (
                            let s = col.col_str in
                            let len = String.length s in
                            (String.sub s 0 cursor_x) ^ !key_string ^ 
                            (String.sub s cursor_x (len - cursor_x))
                            );
                        col.col_cursor <- col.col_cursor + 
                                          (String.length !key_string);

                        let update = col.col_status <> St_Modified in
                        col.col_status <- St_Modified;

                        (*
                        ** The content of this column entry was changed.
                        ** Redraw the column entry.
                        *)
                        self#update_col row_i col_j;
                        (*
                        ** Redraw cursor at new position
                        *)
                        display_cursor <- true;
                        self#update_cursor x y 0;
                        (*
                        ** Update button if any and necessary
                        *)    
                        if update then
                            self#update_but row_i col_j;
                        (*
                        ** Call external action handler if any
                        *)
                        col.col_action col.col_str;
                    end;
                end;
                | None -> warn "no active";    
            );

          ]]);


    val table = table



    method col_iter f = 
        let rows = table.tb_rows in
        let r = ref 0 in
        Array.iter (fun row ->
                let c = ref 0 in
                Array.iter (fun col -> f !r !c row col; incr c) row.row_cols;
                incr r;
            ) rows

    (*
    ** Get a column entry
    *)

    method get_col row_i col_j =
        table.tb_rows.(row_i).row_cols.(col_j)


    method but_bbox row_i col_j =
        let col = self#get_col row_i col_j in
        if (col.col_but <> None) then
        begin
            match (get_some col.col_but) with
            | Status_but but ->
                but.sb_frame.f_bbox
            | Popup_but but ->
                but.pb_frame.f_bbox                
            | Label_but but ->
                but.lb_frame.f_bbox 
        end
        else   
            bbox_of_xywh 0 0 0 0

    method but_frame row_i col_j =
        let col = self#get_col row_i col_j in
        if (col.col_but <> None) then
        begin
            match (get_some col.col_but) with
            | Status_but but ->
                but.sb_frame
            | Popup_but but ->
                but.pb_frame                
            | Label_but but ->
                but.lb_frame
        end
        else   
            empty_frame

    (*
    ** Optional button size (width) including extra
    ** padding space.
    *)


    method but_width row_i col_j =
        let _,_,bw,bh=bbox_to_xywh (self#but_bbox row_i col_j) in
        if bw > 0 
            then (bw + table.tb_colpad_x)
            else 0


    method but_height row_i col_j =
        let _,_,bw,bh=bbox_to_xywh (self#but_bbox row_i col_j) in
        if bh > 0 
            then (bh + 2*table.tb_colpad_y)
            else 0

   
    (*
    ** The pixel width actually available for the text content of a 
    ** column entry.
    *)
    method text_width row_i col_j =
        let col = self#get_col row_i col_j in
        let frame = frame_size col.col_frame in
        col.col_width - 2 * table.tb_colpad_x 
                      - frame - (self#but_width row_i col_j)
  


    (*
    ** Configure one table cell. Only a few attributes can be set at
    ** runtime.
    *)
    method configure_col row_i col_j attrs =
        let update_all = ref false in
        let update_this = ref false in
        let col = self#get_col row_i col_j in
        let syms = ref [] in
        List.iter (fun attr ->
            match attr with
            | Border attr -> 
                        col.col_frame <- 
                            create_frame parent#win 
                                         (fun c->parent#color_make c true)
                                         attr;
                        update_all := true;
            | Foreground c -> col.col_fg <- parent#color_make c true;
                              update_this := true;
            | Background c -> col.col_bg <- parent#color_make c true;
                              update_this := true;
            | ActionSU f ->
            begin
                let old = col.col_action in
                col.col_action <- (fun str -> old str; f str);
            end;
            | But attrs' ->
                if col.col_but <> None then
                List.iter (fun a ->
                    match a with                
                    | Sym s -> 
                        syms := !syms @ [s];
                    | Label l ->
                    begin
                        match (get_some col.col_but) with
                        | Label_but but ->
                            update_all := true;
                            but.lb_label <- l; 
                        | _ -> warn_attr 
                               "VX_texttable: button type conflict" a self#name;
                    end;
                    | ActionUU f ->
                    begin
                        match (get_some col.col_but) with
                        | Label_but but -> but.lb_fun <- f;
                        | _ -> warn_attr 
                               "VX_texttable: invalid button type" a
                               self#name;
                    end;
                    | ActionSSS f ->
                    begin
                        match (get_some col.col_but) with
                        | Status_but but -> but.sb_fun <- f;
                        | _ -> warn_attr 
                               "VX_texttable: invalid button type" a
                               self#name;
                    end;
                            
                    | _ -> warn_attr "VX_texttable: configure_col" a self#name;
                    ) attrs'
                else
                    warn_attr "VX_texttable: no button specified" attr self#name;

            | _ -> warn_attr "VX_texttable: configure_col" attr self#name;
            ) attrs;
        (*
        ** Post config
        *)
        if !syms <> [] then
        begin    
            match (get_some col.col_but) with
            | Status_but but ->
                   but.sb_syms <- !syms;
                   update_this := true;
            | _ -> warn_attr 
                   "VX_texttable: button type conflict" (Sym S_ERR)
                   self#name;
        end;
        if !update_this then
        begin
            self#update_col row_i col_j;
            self#update_but row_i col_j;
        end
        else if !update_all then
        begin
            self#update;
        end;
    (*
    ** Set the text content of one specific column entry.
    *)
    method set_text row_i col_j str =
        let col = self#get_col row_i col_j in
        col.col_str <- str;

        if col.col_rows = 0 || col.col_rows > 1 then
        begin
                (*
                ** Format multiline text
                *)
                col.col_multi_line <- true;
                let lines,newstr,last_pad = 
                        format_lines col.col_font
                                     (atoms_of_str col.col_str)
                                     (self#text_width row_i col_j) in

                if col.col_edit then
                    col.col_lastpad <- if col.col_rows = 0 ||
                                       (List.length lines) <= col.col_rows
                                        then last_pad
                                        else 0;
                (*
                ** Adjustable line height?
                *)
                let resize = col.col_rows = 0 &&
                             ((List.length col.col_lines) <>
                              (List.length lines)) in
                col.col_lines <- lines;
                col.col_str <- newstr;
                if resize then 
                begin
                    (*
                    ** Recalculate full table!!
                    ** Maybe reduced line height, so reset the
                    ** current row height
                    *)
                    table.tb_rows.(row_i).row_height <- 0;
                    parent#wait_resize;
                end;
        end
        else
        begin
            if col.col_edit then
                col.col_lastpad <- (self#text_width row_i col_j) - 
                                   (string_width col.col_font 
                                           (str^" "));
        end;
        self#update_col row_i col_j;


    (*
    ** Get the actual content of the specified column entry.
    *)
    method get_text row_i col_j  =
        let col = self#get_col row_i col_j in
        col.col_str




    (*
    ** Calculate the size of this text widget. Expandable and unsized
    ** columns entries are recalculated. If the height of a row is not 
    ** specified, calculate it from the entries of the row, too.
    *)
    method size_request =
Db.Pr.sd 10 "VX_texttable.size_request geo_width" w.w_geometry.width;    
        let sz = szhints in
        (*
        ** This border is externally handled!
        *)
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            (*
            *)
            let rows = table.tb_rows in
            let nrows = Array.length rows in
            let ncols = Array.length rows.(0).row_cols in
            let col_widths = Array.create ncols 0 in
            
            for row_i = 0 to nrows-1
            do
                let row = rows.(row_i) in
                for col_j = 0 to ncols-1
                do
                    let col = row.row_cols.(col_j) in
                    let text_str = col.col_str in
                    let text_font = col.col_font in
                    let text_font_symbol = col.col_symbol_font in

                    let cframe = frame_size col.col_frame in

                    if col.col_expand = true then
                        (*
                        ** Recalculate width!
                        *)
                        col.col_width <- 0
                    else if not col.col_fixed then
                    begin
                        if not col.col_multi_line then
                            col.col_width <- (string_width_S 
                                                text_font 
                                                text_font_symbol
                                                text_str) + 
                                             2 * table.tb_colpad_x +
                                             (self#but_width row_i col_j) + 
                                             cframe
                        else
                            col.col_width <- (string_width_max_S 
                                                text_font
                                                text_font_symbol
                                                col.col_lines) + 
                                             2 * table.tb_colpad_x +
                                             (self#but_width row_i col_j) +
                                             cframe; 
                   end;
                   col_widths.(col_j) <- max col_widths.(col_j) 
                                             col.col_width;
                done;
            done;
            (*
            ** how many columns must be expanded or sized ->
            ** all columns with current width 0
            *)
            let ncols_exp = ref 0 in
            (*
            ** total width already consumed
            *)
            let roww_used = ref 0 in
            let required_width = ref 0 in
            Array.iter (fun cw -> 
                        if cw = 0 then 
                            incr ncols_exp      
                        else
                            roww_used := !roww_used + cw;
                    ) col_widths;

            (*
            ** Add some default space for expandable columns TODO
            *)
            required_width := !roww_used + !ncols_exp * 50;

            (*
            ** What remains for column expansion
            *)
            let roww_exp = max 0 
                                ((max sz.min_width w.w_geometry.width) - 
                                 (frame + 2 * w.w_ipad_x) -
                                 !roww_used) in

            for row_i = 0 to nrows-1
            do
                let row = rows.(row_i) in
                for col_j = 0 to ncols-1
                do
                    let col = row.row_cols.(col_j) in
                    if !ncols_exp > 0 && col.col_expand then
                        col.col_width <- roww_exp / !ncols_exp
                    else if not col.col_fixed then
                        col.col_width <- col_widths.(col_j);
                done;
            done;
            
            let width = ref 0 in
            let height = ref 0 in
            (*
            ** Reduced height due to overlapping bottom and top 
            ** borders if any
            *)
            let rcol_height = ref 0 in
            let nrows = Array.length table.tb_rows in
            for row_i = 0 to nrows-1
            do
                let row = table.tb_rows.(row_i) in
                let cols = row.row_cols in
                let ncols = Array.length cols in
                let row_width = ref 0 in
                for col_j = 0 to ncols - 1
                do
                    let col = cols.(col_j) in
                    let cframe = frame_size col.col_frame in
                    let cframe_off = frame_offset col.col_frame in
                    let base_line = match col.col_baseline with
                                     | Some tb -> tb.tb_width + 2;
                                     | None -> 0; in
                    row_width := !row_width + col.col_width;

                    let text_font = col.col_font in
                    let h = if not col.col_multi_line then
                                text_font.font_height +
                                table.tb_colpad_y * 2 + 
                                cframe  +       
                                base_line 
                            else
                            begin
                                let rows = max col.col_rows 
                                               (List.length 
                                                col.col_lines) in
                                text_font.font_height * rows +
                                line_spacing * (rows-1) + 
                                table.tb_colpad_y * 2 +   
                                cframe  +       
                                base_line  
                            end
                    in
                    (*
                    ** Row height: maximum of text and optional button
                    *)
                    row.row_height <- max row.row_height 
                                          (max h (self#but_height row_i
                                                                  col_j));
                    rcol_height := max !rcol_height cframe_off;
                done;
                row.row_width <- !row_width;
                width := max !width !row_width;            
                height := !height + row.row_height -
                          !rcol_height + row.row_pad;
            done;
            height := !height + !rcol_height;
            
            required_width := !required_width + frame + 2 * w.w_ipad_x;

            sz.requested_width <- min sz.max_width
                                      (max (!width + frame + 
                                            2 * w.w_ipad_x)
                                           (sz.min_width + frame +
                                            2 * w.w_ipad_x));
            sz.requested_height <- min sz.max_height
                                      (max (!height + frame +
                                            2 * w.w_ipad_y)
                                            (sz.min_height + frame +
                                            2 * w.w_ipad_y));
            sz.min_width <- max sz.min_width !required_width;
            sz
        end

    (*
    **
    ** (Re)Size this object and (re)size the X window associated
    ** with this object widget.
    **     
    **  Args:
    **      x y dx dy -> Maximal available area!
    **
    *)

    method size_allocate x y dx dy =
Db.Pr.sdd 10 "VX_texttable#size_allocate w/h" dx dy;

        __(super#size_allocate x y dx dy);

        w.w_size_modified <- true;    
        let wg = w.w_geometry in
        let s = self#screen in
        let sz = szhints in

        if sz.expand_x && dx <> sz.requested_width then
        begin
            (*
            ** expand to desired width - update column widths just
            ** by calling size_request with initial requested_width
            ** value set to the new window width
            *)
            sz.requested_width <- dx;
            __(self#size_request);
        end;
        true

        

    (*
    ** A column entry can have an optional action button.
    ** Set the state of the button (and redraw content).
    *)
    method update_but row_i col_j = 
        let col = self#get_col row_i col_j in

        (*
        ** Draw status button box if any
        *)
        if col.col_but <> None then
        begin
            let sz = szhints in
            let dpy = s.s_display in
            let gcs = s.s_gcs in
            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let frame = frame_size col.col_frame in
            let frame_off = frame_offset col.col_frame in            

            let bbox = col.col_frame.f_bbox in
            let cx0,cy0,cw,ch = bbox_to_xywh bbox in

            match (get_some col.col_but) with
            | Status_but but ->
                let active = but.sb_active in
                let bbox = but.sb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.sb_frame.f_type <- (
                    match but.sb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.sb_frame.f_type);
    
                let bframe,bframe_off = 
                    frame_size but.sb_frame,
                    frame_offset but.sb_frame in

                let sym = match col.col_status with
                        | St_Failed -> S_ERR;
                        | St_Unknown -> S_QUEST;
                        | St_No_status -> S_QUEST;
                        | St_Modified -> S_ENTER;
                        | St_Submitted -> S_OK;
                        | St_Locked -> S_OK;
                        | St_Busy -> S_BUSY;
                    in


                drawFrame s.s_display w s.s_gcs but.sb_frame true;

                (*
                ** Show symbol status only if specified in col_but list.
                *)
                if (List.mem sym but.sb_syms) then
                begin
                    let symbol = {
                    sym_type = if sym = S_WARN then S_ERR else sym;
                    sym_bbox = {x1=bx0+bframe_off;
                                y1=by0+bframe_off;
                                x2=bx1-bframe+bframe_off;
                                y2=by1-bframe+bframe_off};
                    sym_col = if sym <> S_ERR then noColor
                                              else (parent#color_make "red"
                                                           true);
                    sym_width = 1;
                    } in
                    drawSymbol s.s_display w gcs symbol; 
                end;


            | Popup_but but ->
                let active = but.pb_active in
                let bbox = but.pb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.pb_frame.f_type <- (
                    match but.pb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.pb_frame.f_type);
    
                let bframe,bframe_off = 
                    frame_size but.pb_frame,
                    frame_offset but.pb_frame in

                let sym = if but.pb_opened 
                            then S_UP
                            else S_DOWN in


                drawFrame s.s_display w s.s_gcs but.pb_frame true;

                let symbol = {
                    sym_type = sym;
                    sym_bbox = {x1=bx0+bframe_off;
                                y1=by0+bframe_off;
                                x2=bx1-bframe+bframe_off;
                                y2=by1-bframe+bframe_off};
                    sym_col = noColor;
                    sym_width = 1;
                    } in
                drawSymbol s.s_display w gcs symbol; 

            | Label_but but ->
                let active = but.lb_active in
                let text_font = but.lb_font in
                let text_font_symbol = but.lb_font_symbol in
                let text_str = but.lb_label in

                let bbox = but.lb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.lb_frame.f_type <- (
                    match but.lb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.lb_frame.f_type);

                drawFrame s.s_display w s.s_gcs but.lb_frame true;

                let frame,frame_off = 
                    frame_size but.lb_frame,
                    frame_offset but.lb_frame in

                let th =  text_font.font_height in
                let tw = string_width_S text_font 
                                        text_font_symbol
                                        text_str in
                let tyoff = max 0 (bh - th - 
                                   frame_off - 2 * but.lb_pad_y - 
                                   frame)/2
                            in

                let tx,ty = bx0+(bw - tw)/2,
                            by0+ frame_off + but.lb_pad_y + 
                            text_font.font_ascent + tyoff in
                (*
                ** draw text label
                *)
                draw_string_S s.s_display w.w_window gcs
                                           tx ty fg bg
                                           text_font
                                           text_font_symbol
                                           text_str;
                
        end;

    (*
    **  The cursor got a new position or the display state
    **  changed. In the first case, restore old position, draw new position.
    ** The third argument specifies an optional cursor position change.
    *)
    method update_cursor x y change =
        let sz = szhints in
        let dpy = s.s_display in                      
        let gcs = s.s_gcs in

        last_active <- None;
        self#col_iter (fun row_i col_j row col ->
                if col.col_edit then
                begin
                    let within = within_bbox col.col_frame.f_bbox x y in
                    let all = x = (-1) && y = (-1) in

                    (*
                    ** Change cursor state: state=true -> on , else off.
                    *)
                    let cursor_set state =
                        let bbox = col.col_frame.f_bbox in                        
                        let cx0,cy0,cw,ch=bbox_to_xywh bbox in

                        let text_font = col.col_font in
                        let text_align = col.col_align_x in
                        let cframe = frame_size col.col_frame in
                        let cframe_off = frame_offset col.col_frame in

                        let fg = if col.col_fg <> noColor then
                                    col.col_fg.c_pixel
                                 else
                                    w.w_foreground.c_pixel in
                        let bg = if col.col_bg <> noColor then
                                    col.col_bg.c_pixel
                                 else
                                    w.w_background.c_pixel in
                        
                        let gc = if state then
                                    GCCache.get_fg_bg_font gcs bg fg 
                                                           text_font.font_id 
                                 else
                                    GCCache.get_fg_bg_font gcs fg bg 
                                                           text_font.font_id 
                            in

                        let th =  text_font.font_height in

                        if not col.col_multi_line then
                        begin
                            let text_str = col.col_str in
                            let tyoff = 
                                match col.col_align_y with
                                | Middle -> max 0 (ch - th - 
                                                   2 * table.tb_colpad_y - 
                                                   cframe)/2;
                                | Top -> 0;
                                | Bottom -> ch - th - 2 * table.tb_colpad_y -
                                            cframe;
                                | _ -> progerr "VX_texttable";
                                in
                            let ty = cy0 + table.tb_colpad_y + 
                                     cframe_off + text_font.font_ascent + 
                                     tyoff in

                            let tlen = String.length text_str in
                            let tw = string_width text_font text_str in
    
                            let tx = 
                                match text_align with
                                | Left -> cx0 + cframe_off + 
                                          table.tb_colpad_x 
                                | Center -> cx0 + (cw - tw)/2
                                | Right -> cx0 + cw - tw - 
                                           2 - cframe_off - 
                                           table.tb_colpad_x -
                                           (string_width text_font " ");
                                | _ -> progerr "VX_texttable";
                                in
                            let cursor_x = col.col_cursor in
                            cursor.[0] <-   
                                if cursor_x > (String.length text_str) - 1 
                                    then ' ' 
                                    else text_str.[cursor_x];
                            let xc,yc = (tx+(string_width text_font 
                                            (String.sub text_str 0 cursor_x))),
                                        ty in
                            Xlib.imageSubString s.s_display w.w_window gc  
                                                xc yc cursor 0 1;

                        end
                        else
                        begin
                            (*
                            ** Multiline text is quite more complicated...
                            *)
                            let text_lines = col.col_lines in
                            let text_rows = col.col_rows in
                            let nlines = max col.col_rows 
                                             (List.length text_lines) in
                            let cursor_pos = ref (0,0,' ') in
                            let tot_len = ref 0 in
                            let last_y = ref 0 in
                            let last_x = ref 0 in
                            let last_width = ref 0 in
                            let n = ref 0 in
                            let ty = ref (cy0 + text_font.font_ascent + 
                                          cframe_off + 
                                          table.tb_colpad_y) in

                            List.iter (fun line ->
                              if text_rows = 0 ||
                                 !n < text_rows then
                              begin
                                incr n;
                                let tlen = String.length line in
                                let tw = string_width text_font 
                                                         line in
                                let tx = 
                                match text_align with
                                | Left -> cx0 + cframe_off + table.tb_colpad_x
                                | Center -> cx0 + (cw - tw)/2
                                | Right -> cx0 + cw - tw - 2 - cframe_off - 
                                           table.tb_colpad_x
                                | _ -> progerr "VX_texttable";
                                in

                                last_x := tx;
                                last_y := !ty;
                                last_width := tw;
                                if col.col_cursor >= !tot_len &&
                                   col.col_cursor < (!tot_len + tlen) then
                                begin
                                    let pos' = col.col_cursor - !tot_len in
                                    cursor_pos := (tx + 
                                                    (string_width text_font
                                                     (String.sub line 0 pos')),
                                                  !ty,
                                                  line.[pos']);
                                end; 

                                tot_len := !tot_len + tlen;
                                ty := !ty + th + line_spacing;
    
                              end;
                              ) text_lines;

                            let x',y',c = !cursor_pos in
                            cursor.[0] <- if col.col_cursor > !tot_len - 1
                                        then ' ' 
                                        else c;
                                   

                            let x'',y'' = 
                                if col.col_cursor > !tot_len - 1 then
                                    (!last_x + !last_width), !last_y
                                else
                                    x',y' in
                
                            Xlib.imageSubString s.s_display w.w_window gc  
                                                x'' y'' cursor 0 1;
                
                        end;
                        col.col_cursor_display <- state;
                        in

                    (*
                    ** Cursor state machine
                    *)
                    if within && 
                       display_cursor &&
                       not col.col_cursor_display then
                    begin
                        col.col_cursor <- max 0 (col.col_cursor + change);
                        col.col_cursor <- min col.col_cursor
                                            (String.length col.col_str);
                        cursor_set true;
                    end
                    else if within &&
                            not display_cursor &&
                            col.col_cursor_display then
                    begin
                        cursor_set false;
                    end
                    else if not within &&
                            not display_cursor && 
                            col.col_cursor_display then
                    begin
                        cursor_set false;
                    end 
                    else if not within && 
                            col.col_cursor_display then
                    begin
                        cursor_set false;
                    end
                    else if within &&
                            display_cursor &&
                            col.col_cursor_display &&
                            change <> 0 then
                    begin
                        cursor_set false;
                        col.col_cursor <- max 0 (col.col_cursor + change);
                        col.col_cursor <- min col.col_cursor
                                            (String.length col.col_str);
                        cursor_set true;                        
                    end;
                    if col.col_cursor_display then
                        last_active <- Some (row_i,col_j);

                    
                end;
            )
    

    (*
    ** Update and redraw one specific column entry.
    *)
    method update_col row_i col_j =
        if not (w.w_window == noWindow) then
        begin
            (*
            ** get the table info entry
            *)
            let col = self#get_col row_i col_j in

            let sz = szhints in
            let dpy = s.s_display in
            let gcs = s.s_gcs in
            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in

            let max_text = self#text_width row_i col_j in

            let bbox = col.col_frame.f_bbox in
            let cx0,cy0,cw,ch=bbox_to_xywh bbox in

            (*
            ** Draw text
            *)
            let text_font = col.col_font in
            let text_font_symbol = col.col_symbol_font in
            let text_align = col.col_align_x in

            let cframe = frame_size col.col_frame in
            let cframe_off = frame_offset col.col_frame in

            let fg = if col.col_fg <> noColor then
                                col.col_fg.c_pixel
                             else
                                fg in
            let bg = if col.col_bg <> noColor then
                                col.col_bg.c_pixel
                             else
                                bg in
                        
            let gc = GCCache.get_fg_bg_font gcs fg bg
                                            text_font.font_id in
      
            if not col.col_multi_line then
            begin
                let text_str = col.col_str in  

                let th =  text_font.font_height in
                let tyoff = 
                    match col.col_align_y with
                    | Middle -> max 0 (ch - th - 
                                       2 * table.tb_colpad_y - 
                                       cframe)/2;
                    | Top -> 0;
                    | Bottom -> ch - th - 2 * table.tb_colpad_y - cframe;
                    | _ -> progerr "VX_texttable";
                    in

                let ty = cy0 + table.tb_colpad_y + cframe_off +
                               text_font.font_ascent + tyoff in
    
                let tlen = String.length text_str in
                let tw = string_width_S text_font 
                                        text_font_symbol
                                        text_str in
    
                let tx = 
                    match text_align with
                    | Left -> cx0 + cframe_off + table.tb_colpad_x 
                    | Center -> cx0 + (cw - tw) / 2
                    | Right -> cx0 + cw - tw - 2 - cframe_off - 
                               table.tb_colpad_x -
                               (if col.col_edit then
                                    (string_width text_font " ")
                                else
                                    0);
                    | _ -> progerr "VX_texttable";
                    in
                (*
                ** Only clear content!!! Keep borders...
                *)


                let cc = [
                        (cx0+cframe_off),
                        (cy0+cframe_off),
                        (cw - cframe - (self#but_width row_i col_j)),
                        (ty + (th -
                               text_font.font_ascent) - cy0)] in

                let gc_clear = GCCache.get_fg gcs
                               col.col_frame.f_fillground.c_pixel in
                X.polyFillRectangle s.s_display w.w_window gc_clear cc;

                if col.col_edit then
                    Xlib.drawSubString s.s_display w.w_window gc  
                                       tx ty text_str 0 tlen
                else
                    (*
                    ** String may contain embedded symbols!
                    ** The draw_string function resolves the embedded symbols.
                    *)
                    draw_string_S s.s_display w.w_window gcs 
                                       tx ty fg bg 
                                       text_font
                                       text_font_symbol
                                       text_str;

                (*
                ** Draw baseline if any.    
                *)
                (
                match col.col_baseline with
                | Some tb ->
                begin
                    let blw = tb.tb_width in
                    let fg = if tb.tb_color <> noColor then
                                    tb.tb_color.c_pixel
                                 else
                                    fg in
                            
                    let gc' = 
                        match tb.tb_linetype with
                        | L_Dotted ->
                                get_gc s.s_gcs 
                                       (Fg_bg_ls 
                                          (fg,bg,blw,
                                           LineOnOffDash,
                                           blw))   
                        | L_Dashed ->
                                get_gc s.s_gcs 
                                       (Fg_bg_ls 
                                           (fg,bg,blw,
                                            LineOnOffDash,
                                            4*blw))   
                        | L_Solid ->
                                GCCache.get_fg_bg_lw s.s_gcs fg bg blw
                            in

                    let x0' = cx0 + table.tb_colpad_x + cframe_off in
                    let x1' = cx0 + max_text in
                    let y'  = ty + (th -
                                    text_font.font_ascent) + 
                                    (blw/2) + 1 in

                    X.polyLine s.s_display w.w_window gc' Origin
                               [x0',y';x1',y'];

                end;
                | None -> ()
                );
                if col.col_edit then 
                    col.col_lastpad <- (self#text_width row_i col_j) - 
                                       (string_width text_font (text_str^" "));
            end
            else
            begin
                (*
                ** Mulitline text
                *) 
                let text_lines = col.col_lines in
                let text_rows = col.col_rows in
                let text_edit = col.col_edit in
                let cursor_x = col.col_cursor in

                let nlines = max col.col_rows (List.length text_lines) in
                let th = text_font.font_height in

                let ty = ref (cy0 + text_font.font_ascent + 
                              cframe_off + table.tb_colpad_y) in



                let n = ref 0 in

                (*
                ** If text_rows <> 0 then draw only fixed text_rows number  
                ** of lines!!! 
                *)

                List.iter (fun line ->
                  if text_rows = 0 ||
                     !n < text_rows then
                  begin
                    incr n;
                    let tlen = String.length line in
                    let tw = if not text_edit then
                                string_width_S text_font 
                                               text_font_symbol
                                               line
                             else
                                string_width text_font 
                                             line in
                    let tx = 
                        match text_align with
                        | Left -> cx0 + cframe_off + table.tb_colpad_x
                        | Center -> cx0 + (cw - tw)/2
                        | Right -> cx0 + cw - tw - 2 - cframe_off - 
                                   table.tb_colpad_x
                        | _ -> progerr "VX_texttable";
                        in

                    (*
                    ** Only clear content!!! Keep borders...
                    *)


                    X.clearArea s.s_display w.w_window
                            (cx0 + cframe_off + table.tb_colpad_x) 
                            (!ty - text_font.font_ascent)
                            (cw - cframe - 2 * table.tb_colpad_x)
                            th false;


                    if text_edit then
                        Xlib.drawSubString s.s_display w.w_window gc  
                                       tx !ty
                                       line 0 tlen
                    else
                        draw_string_S s.s_display w.w_window gcs
                                       tx !ty fg bg
                                       text_font
                                       text_font_symbol
                                       line;
                    (*
                    ** Draw baseline if any.     TODO
                    *)
                    ty := !ty + th + line_spacing;
    
                  end;
                  ) text_lines;

            end;
        end;


    (*
    ** Draw the graphics content of the full table widget: 
    **  text, border, cursor...
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            if w.w_clipped then w.w_clear <- false;
            super#refresh;

            let sz = szhints in
            let dpy = s.s_display in
            let gcs = s.s_gcs in
            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fc = w.w_foreground in
            let bc = w.w_background in
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in 

            let wx0 = (frame_off + w.w_ipad_x) in
            let wy0 = ref (frame_off + w.w_ipad_y) in

            let nrows = Array.length table.tb_rows in

            for row_i = 0 to nrows-1
            do
                let row = table.tb_rows.(row_i) in
                let ncols = Array.length row.row_cols in
                let rx = ref wx0 in
                let rh = row.row_height in

                let row_border = ref 0 in

                (*
                ** The right border of the previous and the left border
                ** of this cell should overlap if both right and
                ** left side borders are selected.
                *)
                let last_frame = ref 0 in
Db.Pr.sdd 10 "w_clipping x/y" w.w_clipping.x w.w_clipping.y;
Db.Pr.sdd 10 "w_clipping w/h" w.w_clipping.width w.w_clipping.height;

                let row_visible = 
                    if w.w_clipped then
                        clip_visible w.w_clipping 
                                     (bbox_of_xywh 0 !wy0 
                                                   row.row_width
                                                   row.row_height) 
                    else
                        true in
Db.Pr.ss 10 "VX_texttable#refresh: row visible" (if row_visible then "yes"
                                               else "no");


                for col_j = 0 to ncols -1 
                do
                  let col = row.row_cols.(col_j) in
                  let cw = col.col_width in
                  let text_font = col.col_font in
                  let text_str = col.col_str in
                  let cframe = frame_size col.col_frame in
                  let cframe_off = frame_offset col.col_frame in

                  row_border := max !row_border cframe_off;

                  if row_visible then
                  begin
                    (*
                    ** Save sub window geometry of this column entry.
                    *)
                    let shift = if (List.mem B_left col.col_frame.f_sides) ||
                                   (col.col_frame.f_sides = [] &&
                                    col.col_frame.f_type <> Plain)
                                    then !last_frame
                                    else 0 in

                    let cx0,cy0 = !rx - shift,
                                  !wy0 in

                    last_frame := if (List.mem B_right col.col_frame.f_sides) ||
                                     (col.col_frame.f_sides = [] &&
                                      col.col_frame.f_type <> Plain)
                                    then cframe/2
                                    else 0;

                    col.col_frame.f_bbox <- bbox_of_xywh cx0 cy0 cw rh;
                    (*
                    ** Update frame colors
                    *)
                    col.col_frame.f_foreground <- if col.col_fg <> noColor
                                                    then col.col_fg
                                                    else fc;
                    col.col_frame.f_fillground <- if col.col_bg <> noColor
                                                    then col.col_bg
                                                    else bc;

                    (*
                    ** Update button bounding box and colors if any.
                    *)
                    if col.col_but <> None then
                    begin
                        match (get_some col.col_but) with
                        | Status_but but ->
                            (*
                            ** Keep width and height
                            *)
                            let _,_,bw,bh = bbox_to_xywh but.sb_frame.f_bbox in
                            let yoff = rh/2 - bh/2 in
                            let bx0 = cx0 + cw - cframe_off - bw -
                                             table.tb_colpad_x in
                            let by0 = cy0 + yoff in
    
                            but.sb_frame.f_bbox <- bbox_of_xywh bx0 by0 bw bh;

                        | Popup_but but ->
                            (*
                            ** Keep width and height
                            *)
                            let _,_,bw,bh = bbox_to_xywh but.pb_frame.f_bbox in
                            let yoff = rh/2 - bh/2 in
                            let bx0 = cx0 + cw - cframe_off - bw -
                                             table.tb_colpad_x in
                            let by0 = cy0 + yoff in
    
                            but.pb_frame.f_bbox <- bbox_of_xywh bx0 by0 bw bh;

                        | Label_but but ->
                            (*
                            ** Recalculate width and height
                            *)
                            let tw = string_width_S but.lb_font
                                                    but.lb_font_symbol
                                                    but.lb_label in
                            let th = but.lb_font.font_height in

                            
                            let bframe = frame_size but.lb_frame in
                            let bframe_off = frame_offset but.lb_frame in
                           
                            let bw = tw + 2 * but.lb_pad_x + bframe in
                            let bh = th + 2 * but.lb_pad_y + bframe in
                            let yoff = rh/2 - bh/2 in
                            let bx0 = cx0 + cw - cframe_off - bw -
                                            but.lb_pad_x in
                            let by0 = cy0 + yoff in

                            but.lb_frame.f_bbox <- bbox_of_xywh bx0 by0 bw bh;
                    end;

                    if col.col_but <> None then
                    begin
                        let but = self#but_frame row_i col_j in 
                        let color = 
                            match (get_some col.col_but) with
                            | Status_but but -> but.sb_color;
                            | Label_but but -> but.lb_color;
                            | Popup_but but -> but.pb_color;
                            in
                        match but.f_type with
                        | ReliefRaised
                        | ReliefSunken ->
                            but.f_foreground <- 
                                parent#getShadow 
                                (if color <> noColor 
                                    then color 
                                    else col.col_bg);

                            but.f_auxiliary <- 
                                parent#getHilite 
                                        (if color <> noColor 
                                            then color 
                                            else col.col_bg);
                            but.f_fillground <- 
                                if color <> noColor then color else col.col_bg;
                            but.f_background <- 
                                col.col_bg;
                        | _ ->
    
                            but.f_foreground <- 
                                col.col_fg;
                            but.f_fillground <- 
                                if color <> noColor then color else col.col_bg;
                            but.f_background <- 
                                col.col_bg;
                    end;
                    
                    if not col.col_multi_line then
                    begin
                    
                        if col.col_edit then
                            col.col_lastpad <- (self#text_width row_i col_j)- 
                                               (string_width text_font 
                                                             (text_str^" "))
                    end
                    else
                    begin
                        (*
                        ** Maybe cell size has changed. Reformat text...
                        *)
                        let lines,newstr,last_pad = 
                            format_lines col.col_font
                                     (atoms_of_str col.col_str)
                                     (self#text_width row_i col_j) in

                        col.col_lines <- lines;
                        col.col_str <- newstr;
                        if col.col_edit then
                            col.col_lastpad <- 
                                if col.col_rows = 0 ||
                                   (List.length lines) <= col.col_rows
                                    then last_pad
                                    else 0;

                    end;
                    
                    (*
                    ** Redraw frame. Fill background with fillground color...
                    *)
                    drawFrame dpy w gcs col.col_frame true;
                    self#update_col row_i col_j;
                    (*
                    ** Draw button in column entry if there is a 
                    ** button action specified.
                    *)
                    if col.col_but <> None then 
                        self#update_but row_i col_j;


                    rx := !rx + cw;
                  end;
                done;
                (*
                ** Note: we want to have overlap of bottom
                **       and top border lines of succeeding rows:
                **       - row_border!
                *)
                wy0 := !wy0 + rh - !row_border + row.row_pad;
            done;
        end

    method update =
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;

    (*
    ** Popup button support
    *)

    method popup row_i col_j x y =
        let col = self#get_col row_i col_j in
        if col.col_but <> None then
        begin
          let top = 
          match (get_some col.col_but) with
          | Popup_but but ->
          begin
            match but.pb_realization with
            | Some (top : VX_top.t) -> 
                __(top#configure [Background "white"; Position (x,y)]);
                top
            | None -> 
                if root = None then 
                    vx_error "VX_texttable with popup buttons needs root!";
                let top = new VX_top.t (get_some root) (Some (x,y)) [] in
                but.pb_realization <- Some top;
                let vbar = new VX_box.v top#container 
                        [
                            Background "white";
                            Border [];
                            IpadX 5; IpadY 5;
                            Cursor (FontCursor XC.xc_hand1);
                        ] in
                top#container_add vbar#contained;
                (
                    match but.pb_widget with
                    | Some wob -> vbar#container_add wob;
                    | None -> ();
                );
                __(top#configure [Bindings 
                    [
                        EnterWindow, (fun _ ->
                            top#focus;
                            );
                    ]]);
                top
          end
          | _ -> vx_error "VX_texttable: popup: invalid button type";
                 progerr "";
          in
          top#show;
          top#focus
        end

    method popdown row_i col_j =
        let col = self#get_col row_i col_j in
        if col.col_but <> None then
        match (get_some col.col_but) with
        | Popup_but but ->
        begin
            match but.pb_realization with
            | Some (top : VX_top.t) -> 
                    top#hide;
                    but.pb_opened <- false;
            | None -> ()
        end;
        | _ -> vx_error "VX_textable: popdown: invalid button type";
        
    (*
    ** Print button
    *)

    method print_but ps wx0 wy0 row_i col_j = 
        let col = self#get_col row_i col_j in

        (*
        ** Print status button box if any
        *)
        if col.col_but <> None then
        begin
            let sz = szhints in
            let dpy = s.s_display in

            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fc = w.w_foreground in
            let bc =  w.w_background in
            let frame = frame_size col.col_frame in
            let frame_off = frame_offset col.col_frame in            

            let bbox = col.col_frame.f_bbox in
            let cx0,cy0,cw,ch = bbox_to_xywh bbox in

            match (get_some col.col_but) with
            | Status_but but ->
                let active = but.sb_active in
                let bbox = but.sb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.sb_frame.f_type <- (
                    match but.sb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.sb_frame.f_type);
    
                let bframe,bframe_off = 
                    frame_size but.sb_frame,
                    frame_offset but.sb_frame in

                let sym = match col.col_status with
                        | St_Failed -> S_ERR;
                        | St_Unknown -> S_QUEST;
                        | St_No_status -> S_QUEST;
                        | St_Modified -> S_ENTER;
                        | St_Submitted -> S_OK;
                        | St_Locked -> S_OK;
                        | St_Busy -> S_BUSY;
                    in

                printFrame ps w wx0 wy0 but.sb_frame true;

                (*
                ** Show symbol status only if specified in col_but list.
                *)
                if (List.mem sym but.sb_syms) then
                begin
                    let symbol = {
                    sym_type = if sym = S_WARN then S_ERR else sym;
                    sym_bbox = {x1=bx0+bframe_off;
                                y1=by0+bframe_off;
                                x2=bx1-bframe+bframe_off;
                                y2=by1-bframe+bframe_off};
                    sym_col = if sym <> S_ERR then noColor
                                              else (parent#color_make "red"
                                                           true);
                    sym_width = 1;
                    } in
                    printSymbol ps s.s_display w wx0 wy0 symbol; 
                end;


            | Popup_but but ->
                let active = but.pb_active in
                let bbox = but.pb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.pb_frame.f_type <- (
                    match but.pb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.pb_frame.f_type);
    
                let bframe,bframe_off = 
                    frame_size but.pb_frame,
                    frame_offset but.pb_frame in

                let sym = if but.pb_opened 
                            then S_UP
                            else S_DOWN 
                    in

                printFrame ps w wx0 wy0 but.pb_frame true;

                let symbol = {
                    sym_type = sym;
                    sym_bbox = {x1=bx0+bframe_off;
                                y1=by0+bframe_off;
                                x2=bx1-bframe+bframe_off;
                                y2=by1-bframe+bframe_off};
                    sym_col = noColor;
                    sym_width = 1;
                    } in
                printSymbol ps s.s_display w wx0 wy0 symbol; 

            | Label_but but ->
                let active = but.lb_active in
                let text_font = but.lb_font in
                let text_font_symbol = but.lb_font_symbol in
                let text_str = but.lb_label in

                let bbox = but.lb_frame.f_bbox in
                let bx0,by0,bw,bh=bbox_to_xywh bbox in
                let bx1,by1 = bbox.x2,bbox.y2 in

                but.lb_frame.f_type <- (
                    match but.lb_frame.f_type with
                    | ShadowRaised
                    | ShadowSunken -> 
                        if active 
                            then ShadowSunken
                            else ShadowRaised;
                    | ReliefRaised
                    | ReliefSunken -> 
                        if active 
                            then ReliefSunken
                            else ReliefRaised;
                    | _ -> 
                        but.lb_frame.f_type);
    
                printFrame ps w wx0 wy0 but.lb_frame true;

                let frame,frame_off = 
                    frame_size but.lb_frame,
                    frame_offset but.lb_frame in

                let th =  text_font.font_height in
                let tw = string_width_S text_font 
                                        text_font_symbol
                                        text_str in
                let tyoff = max 0 (bh - th - 
                                   frame_off - 2 * but.lb_pad_y - 
                                   frame)/2
                            in

                let tx,ty = bx0+(bw - tw)/2,
                            by0+ frame_off + but.lb_pad_y + 
                            text_font.font_ascent + tyoff in
                (*
                ** print text label
                *)
                print_string_S ps wx0 wy0 
                               (i2f tx) (i2f ty)
                               fc bc
                               text_font
                               text_font_symbol
                               text_str;
                
        end;



    (*
    ** Print one column entry
    *)
    method print_col ps wx0 wy0 row_i col_j  =
            (*
            ** get the table info entry
            *)
            let col = self#get_col row_i col_j in

            (*
            ** Draw border lines and fill with fillground.
            *)
            printFrame ps w wx0 wy0 col.col_frame true;

            let sz = szhints in
            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fc = w.w_foreground in
            let bc =  w.w_background in

            let max_text = self#text_width row_i col_j in

            let bbox = col.col_frame.f_bbox in
            let x0 = bbox.x1 in
            let y0 = bbox.y1 in
            let w' = col.col_width in
            let h' = bbox.y2 - bbox.y1 + 1 in
            (*
            ** Draw text
            *)
            let text_font = col.col_font in
            let text_font_symbol = col.col_symbol_font in
            let text_str = col.col_str in  
            let text_align = col.col_align_x in

            let frame = frame_size col.col_frame in
            let frame_off = frame_size col.col_frame in

            let fc = if col.col_fg <> noColor then
                                col.col_fg
                             else
                                fc in
            let bc = if col.col_bg <> noColor then
                                col.col_bg
                             else
                                bc in
                        
      
            let h'' =  text_font.font_height in

     
            if not col.col_multi_line then
            begin
                let text_str = col.col_str in  
            
                let offset = 
                    match col.col_align_y with
                    | Middle -> max 0 (h' - h'' - 
                                       2 * table.tb_colpad_y - 
                                       frame)/2;
                    | Top -> 0;
                    | Bottom -> h' - 2 * table.tb_colpad_y - h'' - frame;
                    | _ -> progerr "VX_texttable";
                    in

                let ty = y0 + table.tb_colpad_y + frame_off +
                              text_font.font_ascent + offset in

                let len = String.length text_str in
                let width = string_width_S text_font 
                                           text_font_symbol
                                           text_str in
    
                let tx = 
                    match text_align with
                    | Left -> x0 + frame_off + table.tb_colpad_x 
                    | Center -> x0 + (w' - width) / 2
                    | Right -> x0 + w' - width - 2 - frame_off - 
                               table.tb_colpad_x -
                               (if col.col_edit then
                                    (string_width text_font " ")
                                else
                                    0);
                    | _ -> progerr "VX_texttable";
                    in


                print_string_S ps wx0 wy0 
                               (i2f tx) (i2f ty) fc bc 
                               text_font
                               text_font_symbol
                               text_str;

                (*
                ** Draw baseline if any.    
                *)
                (
                    match col.col_baseline with
                    | Some tb ->
                    begin
                        let blw = tb.tb_width in
                        let fc = if tb.tb_color <> noColor then
                                        tb.tb_color
                                     else
                                        fc in
                            
                        (*
                        ** TODO: linetype...
                        *)

                        let x0' = x0 + table.tb_colpad_x + frame_off in
                        let x1' = x0 + max_text in
                        let y'  = ty + (text_font.font_height -
                                        text_font.font_ascent) + 
                                        (blw/2) + 1 in

                        VX_ps.polyLine ps wx0 wy0 (i2f blw) fc
                                   [i2f x0', i2f y';
                                    i2f x1', i2f y'];
    
                    end;
                    | None -> ()
                );
            end
            else
            begin
                (*
                ** Mulitline text
                *) 
                let text_lines = col.col_lines in
                let text_rows = col.col_rows in
                let text_edit = col.col_edit in
                let cursor_x = col.col_cursor in

                let nlines = max col.col_rows (List.length text_lines) in
                let h'' = text_font.font_height in

                let ty = ref (y0 + text_font.font_ascent + 
                              frame_off + w.w_ipad_y) in

                let n = ref 0 in

                (*
                ** If text_rows <> 0 then draw only fixed text_rows number  
                ** of lines!!! 
                *)

                List.iter (fun line ->
                  if text_rows = 0 ||
                     !n < text_rows then
                  begin
                    incr n;
                    let len = String.length line in
                    let width = if not text_edit then
                                    string_width_S text_font 
                                                   text_font_symbol
                                                   line
                                else
                                    string_width text_font 
                                                 line in
                    let tx = 
                        match text_align with
                        | Left -> x0 + frame_off + table.tb_colpad_x
                        | Center -> x0 + (w' - width)/2
                        | Right -> x0 + w' - width - 2 - frame_off - 
                                   table.tb_colpad_x
                        | _ -> progerr "VX_texttable";
                        in
                    print_string_S ps wx0 wy0
                                   (i2f tx) (i2f !ty) fc bc
                                   text_font
                                   text_font_symbol
                                       line;

                    ty := !ty + h'' + line_spacing;
    
                  end;
                  ) text_lines;
            end;


            (*
            ** Draw button in column entry if there is a 
            ** button action specified.
            *)
            if col.col_but <> None then 
                self#print_but ps wx0 wy0 row_i col_j;




    method print (ps : ps) (x0 : int) (y0 : int) =
        super#print ps x0 y0;
        self#col_iter (fun row_i col_j row col ->
                self#print_col ps x0 y0 row_i col_j;
                    );

end



(*
** Generates a table descriptor. Not specified parameters are filled
** with default values. Sizes are computed in size_request if necessary.
*)
let table_gen parent table_cont attributes =
    let table_attr = ref [] in
    let row_attr = ref [||] in
    let col_attr = ref [||] in

    let tb = {
                tb_rows = [||];
                tb_colpad_x = 0;
                tb_colpad_y = 0;
                tb_height_expand = false;
             } in


    let nrows = Array.length table_cont in
    let rows = Array.init nrows (fun _ -> {row_cols=[||];
                                           row_height=0;
                                           row_width=0;
                                           row_pad=0;}) in

    let default_font = parent#font_make Helvetica Roman 12 true in

    let font = font_desc default_font in
    let font_changed = ref false in
    List.iter (fun ta ->
        match ta with
        | IpadX x -> tb.tb_colpad_x <- x;
        | IpadY y -> tb.tb_colpad_y <- y;
        | Rows r -> row_attr := r;
        | Cols c -> col_attr := c;
        | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
        | Text_style style -> font_changed := true;
                                  font.text_style <- style;
        | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
        | _ -> ();
        ) attributes;

    let default_font = 
        if !font_changed then
            parent#font_make font.text_font
                             font.text_style 
                             font.text_size true

        else
            default_font in            

    let default_symbol_font = parent#font_make Symbol
                                               default_font.font_style 
                                               default_font.font_size true in

    (*
    ** Some sanity checks
    *)
    if !row_attr <> [||] && (Array.length !row_attr <> nrows) then
        vx_error "VX_texttable: invalid length of Rows array";
    if !col_attr <> [||] && (Array.length !col_attr <> nrows) then
        vx_error "VX_texttable: invalid length of Cols array";

    let max_cols = ref 0 in

    for row_i = 0 to nrows - 1
    do
        let row = rows.(row_i) in
        let row_str = table_cont.(row_i) in
        let ncols = Array.length row_str in
        if !max_cols > 0 && ncols <> !max_cols then
            vx_error (sprintf 
            "VX_texttable: invalid number of column entries (got %d, expected %d)" 
            ncols !max_cols);

        max_cols := max !max_cols ncols;
        let cols = Array.init ncols (fun _ -> 
                        {
                            col_str = "";
                            col_lines = [];
                            col_multi_line = false;
                            col_rows = 1;
                            col_font = default_font;
                            col_symbol_font = default_symbol_font;
                            col_align_x = Left;
                            col_align_y = Middle;
                            col_fg = noColor;
                            col_bg = noColor;
                            col_frame = {(default_frame 0) with 
                                         f_type=Plain};
                            col_width = 0;
                            col_baseline = None;
                            col_edit = false;
                            col_but = None;
                            col_expand = false;
                            col_fixed = false;
                            col_cursor = 0;
                            col_cursor_display = false;
                            col_lastpad = 0;
                            col_status = St_Submitted;
                            col_action = (fun _ -> ());
                        }) in
        row.row_cols <- cols;
        for col_j = 0 to ncols-1
        do
            let col = cols.(col_j) in
            col.col_str <- table_cont.(row_i).(col_j);

            let font_changed = ref false in
            let font = font_desc col.col_font in

            if !col_attr <> [||] then
            begin
                if (Array.length !col_attr.(row_i) <> ncols) then
                    vx_error "VX_texttable: invalid length of Cols array";
                List.iter (fun ca ->
                    match ca with
                    | Width  w -> col.col_width <- w;
                                  col.col_fixed <- true;
                    | Rown n -> col.col_rows <- n;
                                if col.col_rows <> 1 then
                                    col.col_multi_line <- true;
                                if col.col_rows = 0 then
                                    tb.tb_height_expand <- true;
                    | Border attr -> 
                        col.col_frame <- 
                            create_frame parent#win 
                                         (fun c->parent#color_make c true)
                                         attr;
                    | Text_baseline attr -> 
                        col.col_baseline <- Some ( 
                            create_baseline parent#win 
                                         (fun c->parent#color_make c true)
                                         attr);
                                            
                    | Foreground c -> col.col_fg <- parent#color_make c true;
                    | Background c -> col.col_bg <- parent#color_make c true;
                    | Mutable m -> col.col_edit <- m;
                    | Align al -> if (List.mem al [Left;Right;Center])
                                    then col.col_align_x <- al
                                    else col.col_align_y <- al;
                    | ActionSU f ->
                    begin
                        let old = col.col_action in
                        col.col_action <- (fun str -> old str; f str);
                    end;
                    | But attr ->
                        List.iter (fun a ->
                            match a with                
                            | Sym s -> 
                            begin
                                (*
                                ** This attribute specifies a 
                                ** status button!
                                *)
                                if col.col_but = None then
                                    col.col_but <- Some (Status_but {
                                        sb_fun=(fun _ _ -> St_Submitted);
                                        sb_syms=[s];
                                        sb_frame={(default_frame 1) with
                                                    f_bbox = 
                                                        bbox_of_xywh 0 0
                                                                def_but_size
                                                                def_but_size;
                                                    f_type=ShadowRaised;
                                                };
                                        sb_color=noColor;
                                        sb_active=false;
                                        })
                                else
                                match (get_some col.col_but) with
                                | Status_but but ->
                                    but.sb_syms <- but.sb_syms @ [s];
                                | _ -> 
                                    warn_attr 
                                    "VX_texttable: button type conflict" a
                                    "";
                            end;
                            | Widget w -> 
                            begin
                                (*
                                ** This attribute specifies a 
                                ** widget popup button!
                                *)
                                if col.col_but = None then
                                    col.col_but <- Some (Popup_but {
                                        pb_fun_up=(fun ()-> ());
                                        pb_fun_down=(fun ()-> ());
                                        pb_frame={(default_frame 1) with
                                                    f_bbox = 
                                                        bbox_of_xywh 0 0
                                                                def_but_size
                                                                def_but_size;
                                                    f_type=ShadowRaised;
                                                };
                                        pb_color=noColor;
                                        pb_active=false;
                                        pb_opened=false;    
                                        pb_widget = Some w;
                                        pb_realization=None;
                                        })
                                else
                                    warn_attr 
                                    "VX_texttable: button type conflict" a
                                    "";

                            end;
                            | Label l -> 
                            begin
                                (*
                                ** This attribute specifies a 
                                ** label button!
                                *)
                                if col.col_but = None then
                                    col.col_but <- Some (Label_but {
                                        lb_label=l;
                                        lb_font=default_font;
                                        lb_font_symbol=default_symbol_font;
                                        lb_pad_x = tb.tb_colpad_x;
                                        lb_pad_y = tb.tb_colpad_y;
                                        lb_fun=(fun () -> ());
                                        lb_frame={(default_frame 1) with
                                                  f_type=ShadowRaised};
                                        lb_color=noColor;
                                        lb_active=false;
                                        })
                                else
                                match (get_some col.col_but) with
                                | Label_but but ->
                                    but.lb_label <- l;
                                | _ -> 
                                    warn_attr 
                                    "VX_texttable: button type conflict" a
                                    "";
                            end;
                            (*
                            ** All the action handlers with different
                            ** meanings
                            *)
                            | ActionSSS f -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but ->
                                    but.sb_fun <- f;
                                | _ -> warn_attr 
                                       "VX_texttable: invalid button type" a
                                       "";
                            end;
                            | ActionUU f -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Label_but but ->
                                    but.lb_fun <- f;
                                | _ -> warn_attr 
                                       "VX_texttable: invalid button type" a
                                       "";
                            end;
                            | IpadX x -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but -> ();
                                | Popup_but but -> ();
                                | Label_but but ->
                                    but.lb_pad_x <- x;
                            end;
                            | IpadY y -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but -> ();
                                | Popup_but but -> ();
                                | Label_but but ->
                                    but.lb_pad_y <- y;
                            end;
                            | Frame s  -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but -> 
                                    but.sb_frame.f_type <- s;
                                | Popup_but but -> 
                                    but.pb_frame.f_type <- s;
                                | Label_but but -> 
                                    but.lb_frame.f_type <- s;
                            end;
                            | Size s  -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but -> 
                                    but.sb_frame.f_bbox <-
                                        bbox_of_xywh 0 0 s s;
                                | Popup_but but -> 
                                    but.pb_frame.f_bbox <-
                                        bbox_of_xywh 0 0 s s;
                                | Label_but but -> 
                                    warn_attr 
                                    "VX_texttable: button type conflict" a
                                    ""
                            end;
                            | Color c  -> 
                            begin
                                if col.col_but = None then
                                    warn_attr 
                                    "VX_texttable: button type not set" a
                                    ""
                                else
                                match (get_some col.col_but) with
                                | Status_but but -> 
                                    but.sb_color <- 
                                        parent#color_make c true;
                                | Popup_but but -> 
                                    but.pb_color <- 
                                        parent#color_make c true;
                                | Label_but but -> 
                                    but.lb_color <- 
                                        parent#color_make c true;
                            end;
    
                            | _ -> warn_attr "VX_texttable" a "";
                          ) attr;

                    | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
                    | Text_style style -> font_changed := true;
                                  font.text_style <- style;
                    | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
                    | ExpandX b -> col.col_expand <- b;
                                   if col.col_expand then
                                   begin
                                        col.col_fixed <- false;
                                        col.col_width <- 0;
                                   end;
                    | _ -> warn_attr "VX_texttable" ca "";
                ) !col_attr.(row_i).(col_j);
                (*
                ** Update frame colors
                *)
                if col.col_fg <> noColor then
                    col.col_frame.f_foreground <- col.col_fg;
                if col.col_bg <> noColor then
                    col.col_frame.f_fillground <- col.col_bg;
 
            end;

            if !font_changed then 
            begin
                col.col_font <- parent#font_make font.text_font
                                          font.text_style 
                                          font.text_size true;
                col.col_symbol_font <- parent#font_make Symbol
                                          font.text_style 
                                          font.text_size true;
            end;

        done;
        if !row_attr <> [||] then
        List.iter (fun ra ->
                match ra with
                | IpadY p -> row.row_pad <- p;
                | _ -> ();
            ) !row_attr.(row_i);
    done;

    tb.tb_rows <- rows;

    (*
    ** Some sanity checks of column widths. Expandable or currently unsized 
    ** column netries will be sized in size_request.
    *)

    let col_widths = Array.create !max_cols (max_int,0) in
    (*
    ** First calculate column width...
    *)
    for row_i = 0 to nrows-1
    do
        let row = rows.(row_i) in
        let ncols = Array.length row.row_cols in
        for col_j = 0 to ncols-1
        do
            let col = row.row_cols.(col_j) in
            let last_min,last_max = col_widths.(col_j) in
            col_widths.(col_j) <-   min last_min col.col_width,
                                    max last_max col.col_width;
        done;
    done;

    (*
    ** Check columns widths
    *)

    let irregular = ref false in
    Array.iter ( fun (min_w,max_w) ->
            (*
            ** we tolerate different widths of entries from
            ** one column -> irregular table! -> but all widths must
            ** be specified!!!
            *)
            if min_w <> max_w then
                irregular := true;

            if min_w = 0 && min_w <> max_w then 
                vx_error (sprintf
                    "VX_texttable: different widths of column entries (min=%d max=%d)"
                    min_w max_w);
            if !irregular && min_w = 0 && max_w = 0 then
                vx_error "VX_texttable: irregular table, but not all widths specified!";
        ) col_widths; 

    tb

(*
** Texttable main class.
*)
class t parent table_cont attributes =
    object 
    inherit orig parent None (table_gen parent table_cont 
                                        attributes) attributes
end

(*
** With additional root window needed for popup button implementation.
*)
class with_root parent root table_cont attributes =
    object 
    inherit orig parent (Some root) (table_gen parent table_cont 
                                               attributes) attributes

end
