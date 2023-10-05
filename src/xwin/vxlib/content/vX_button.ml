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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     31.5.2005
**    $VERSION:     1.23
**
**    $INFO:
**
**  VXlib button widgets.
**
**  class t: one button with text label
**
**  class table : buttons with label organized in rows and columns
**  
**  Main attributes: IpadX,IpadY are applied both inside the button
**  (space between text label and button border), and between buttons
**  and rows!
** 
**  Either the width of a button (border to border width) is specified
**  with the Width attribute inside the Cols attribute, or the
**  size of each button is either calculated from the label width and
**  inside padding, or the buttons will be expanded to the extents
**  of the window (with specified padding). This requires the ExpandX
**  attribute.     
**
**  Together with the ExpandX attribute and fixed button width autopadding
**  can be enabled with teh AdjustX attribute. The button will be aligned
**  automatically inside the window widget. 
**
**  Single buttons can be grouped using the Group attribute inside the
**  Cols attribute. A unique group number in each button entry specifies
**  all buttons belonging to the same group.
**
**
**    $ENDOFINFO
**
*)


open Xtypes
open VX_types
open VX_ps
open VX_common
open VX_text
open Printf
open GCCache


(*
** Text button
*)
class orig parent (text : string) attributes =
    object (self)
    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font = parent#font_make Times Roman 12 true; 
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;


    inherit VX_object.t parent ([IpadX 5]@attributes) as super

    val mutable action = (fun () -> ())
    val mutable active = false

    val mutable inited = false
    initializer 
        self#set_name "button";
        __(self#configure [Cursor (FontCursor XC.xc_hand2)]@attributes);
        __(self#configure [Bindings [
          (*
          ** Keyboard and mouse user interaction.
          *)
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                if not active then
                begin
                    active <- true;
                    self#update;
                    action ();
                    self#update;
                end;  
            );
          ButtonReleased,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                active <- false;
                self#update;
            );
        ]]);


    val mutable text_str = text


    method set_state b = active <- b
    method get_state   = active 
    method set_action f = action <- f

    (*
    ** Set text attributes
    *)
    method configure attrs = 
        if not inited then
        begin
            w.w_frame <- default_frame 1;
            inited <- true;
        end;
        let remains = super#configure attrs in

        let font_changed = ref false in
        let font = font_desc text_font in
        let changed = ref false in


        List.iter (fun attr ->
            match attr with
            | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
                                  changed := true;
            | Text_style style -> font_changed := true;
                                  font.text_style <- style;
                                  changed := true;
            | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
                                  changed := true;
            | ActionUU f -> action <- f;
            | Shape s -> w.w_frame.f_shape <- s;
            (*
            ** TODO
            *)
            | Frame s -> w.w_frame.f_type <- s;
            | Border attr -> w.w_frame <- {(create_frame w 
                                                   (fun c ->
                                                    parent#color_make c true)
                                                    attr) with
                                           f_bbox = w.w_frame.f_bbox};
                             
            | _ -> warn_attr "VX_button" attr self#name;
            ) remains;
        
        if !font_changed then 
        begin
            text_font <- parent#font_make font.text_font
                                          font.text_style 
                                          font.text_size true;
            text_font_symbol <- parent#font_make Symbol
                                          font.text_style 
                                          font.text_size true;
        end;
        [] (* leaf widget *)

        
    (*
    ** Get the size of this button widget. Text string area + 
    ** shadow and padding. Widget border is not very usefull here!
    *)
    method size_request =
        let sz = szhints in
        (*
        ** Not very usefull here...
        *)
        let frame = frame_size w.w_frame in

        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            sz.comp_timestamp <- s.s_timestamp;

            let required_width = (string_width_S text_font
                                                 text_font_symbol
                                                 text_str) +
                                 frame + 2 * w.w_ipad_x in    

            sz.requested_width <- 
                        min sz.max_width
                            (max required_width sz.min_width);

            sz.requested_height <- 
                    min sz.max_height
                        (max (text_font.font_height + 
                              frame + 2 * w.w_ipad_y) 
                             sz.min_height);
            sz.min_width <- max sz.min_width required_width;
            sz
        end

    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            let sz = szhints in
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let gcs = s.s_gcs in

            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            (*
            ** Update state of button
            *)
            (*
            ** TODO
            *)
            w.w_frame.f_type <- (
                match w.w_frame.f_type with
                | ShadowRaised  
                | ShadowSunken -> if active then ShadowSunken
                                            else ShadowRaised;
                | ReliefRaised 
                | ReliefSunken -> if active then ReliefSunken
                                            else ReliefRaised;
                | _ -> w.w_frame.f_type);

            (*
            ** Update frame bbox
            *)
            w.w_frame.f_bbox <- bbox_of_xywh 0 0 width height;

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let th =  text_font.font_height in
            let tyoff = max 0 (height - th - 
                               frame_off - 2 * w.w_ipad_y - frame)/2
                            in

            let tw = string_width_S text_font 
                                    text_font_symbol
                                    text_str in

            (*
            ** Centered text
            *)
            let tx = (width - tw)/2 in
            let ty = w.w_ipad_y + frame_off + 
                     text_font.font_ascent + tyoff in

            
            (*
            ** Draw text and button frame
            *)

            drawFrame s.s_display w s.s_gcs w.w_frame true; 
            (*
            ** draw text label
            *)
            draw_string_S s.s_display w.w_window gcs
                                       tx ty fg bg
                                       text_font
                                       text_font_symbol
                                       text_str;

        end;

    method update =
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;

    method print (ps : ps) (wx0 : int) (wy0 : int) =
        Db.Pr.sdd 0 "VX_button.print x0 y0" wx0 wy0;
        let sz = szhints in

        let fc = w.w_foreground in
        let bc =  w.w_background in
        let gcs = s.s_gcs in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        let th =  text_font.font_height in
        let tyoff = max 0 (height - th - 
                           frame_off - 2 * w.w_ipad_y - frame)/2
                           in

        let tw = string_width_S text_font 
                                text_font_symbol
                                text_str in
        (*
        ** Centered text
        *)
        let tx = (width - tw)/2 in
        let ty = w.w_ipad_y + frame_off + 
                 text_font.font_ascent + tyoff - 1 in

            
        printFrame ps w wx0 wy0 w.w_frame true;

        print_string_S ps wx0 wy0 
                       (i2f tx) (i2f ty) 
                       fc bc
                       text_font
                       text_font_symbol
                       text_str;

    
end

class t = orig

(*
** Button tables
*)

(*
** One column entry of a row
*)
type col_desc = {
    (*
    ** Content
    *)
    mutable col_str     : string;
    mutable col_font    : font;         (* normal text font *)
    mutable col_symbol_font : font;     (* embedded symbols font *)
    mutable col_fg      : color;
    mutable col_bg      : color;

    (*
    ** Button frame
    *)
    mutable col_shadow  : frame;

   

    (*
    ** Requested width in pixel including
    ** shape and inside label padding. Can be zero. Either the column
    ** entry width is fixed or expandable.
    *)
    mutable col_width : int;
    mutable col_expand : bool;
    mutable col_fixed : bool;


    (*
    ** Button action
    *)
    mutable col_action : (unit -> unit) option;
    (*
    ** Is button sunken or raised ?
    *)
    mutable col_active : bool;
    (*
    ** Grouping of buttons (row,col). Only one button
    ** of the list can be activated (sunken).
    *)
    mutable col_group : int option;
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
    
    (*
    ** Pad space after this row
    *)
    mutable row_pad : int;
}

type table_desc = {
    mutable tb_rows : row_desc array;
    mutable tb_colpad_x : int;
    mutable tb_colpad_y : int;
    mutable tb_regular : bool;
    (*
    ** Dynamic auto adjustment (space padding between buttons).
    *)
    mutable tb_autopad_x : bool;
}



class orig2 parent (table : table_desc) attributes =
    object (self)

    inherit VX_object.t parent attributes as super



    (*
    ** Initialize this widget. The column entry widths are static and
    ** must be already calculated. The row height can be adjusted auto-
    ** matically.
    *)

    initializer
        self#set_name "button_table";

        __(self#configure attributes);
        __(self#configure [Bindings [
          (*
          ** Keyboard and mouse user interaction.
          *)
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                protect (
                    self#col_iter (fun row_i col_j row col ->    
                        let within = within_bbox col.col_shadow.f_bbox x y in
                        if within then
                        begin
                            if not col.col_active then
                            begin
                                col.col_active <- true;
                                self#update_but row_i col_j;
                                let get_fun = match col.col_action with
                                          | Some f -> f;
                                          | None -> (fun () -> ())
                                            in 
                                    
                                get_fun ();
                                self#update_but row_i col_j;
                            end;
                            (
                              match col.col_group with
                              | Some g ->
                              begin
                                (*
                                ** Deactivate other group members.
                                *)
                                self#col_iter (fun row_i' col_j' row' col' ->
                                    match col'.col_group with
                                    | Some g' when (row_i <> row_i') ||
                                                   (col_j <> col_j') ->
                                        if g' = g && col'.col_active then
                                        begin
                                            col'.col_active <- false;
                                            self#update_but 
                                                    row_i' col_j';
                                        end;
                                    | _ -> ();
                                );
                              end;
                              | None -> ();
                            );
                            raise Exit;
                        end;
                    );
                );
            );
          ButtonReleased,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                protect (
                    self#col_iter (fun row_i col_j row col ->    
                        let within = within_bbox col.col_shadow.f_bbox x y in

                        if within then
                        begin
                            if col.col_group = None then
                            begin
                                col.col_active <- false;
                                self#update_but row_i col_j;
                            end;
                            raise Exit;                            
                        end;
                    );
                );
            );
        ]]);


    val table = table
    method col_iter (f: int -> int -> row_desc -> col_desc -> unit) = 
        let rows = table.tb_rows in
        let r = ref 0 in
        Array.iter (fun row ->
                let c = ref 0 in
                Array.iter (fun col -> 
                    (f !r !c row col); 
                    incr c;
                  ) row.row_cols;
                incr r;
            ) rows


    method get_col row_i col_j =
        table.tb_rows.(row_i).row_cols.(col_j)




    method set_state row_i col_j b = 
        let col = self#get_col row_i col_j in
        col.col_active <- b;

    method get_state row_i col_j  = 
        let col = self#get_col row_i col_j in
        col.col_active;

    method set_action row_i col_j f = 
        let col = self#get_col row_i col_j in
        col.col_action <- f;


    (*
    ** Calculate the size of this text widget. Expandable and unsized
    ** columns entries are recalculated. If the height of a row is not 
    ** specified, calculate it from the entries of the row, too.
    *)
    method size_request =
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


                    if col.col_expand = true then
                        (*
                        ** Recalculate width!
                        *)
                        col.col_width <- 0
                    else if not col.col_fixed then
                        col.col_width <- (string_width_S text_font 
                                           text_font_symbol
                                           text_str) + 
                                         2 * table.tb_colpad_x;

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
            let roww_used = ref table.tb_colpad_x in
            let required_width = ref 0 in
            Array.iter (fun cw -> 
                        if cw = 0 then 
                            incr ncols_exp      
                        else
                            roww_used := !roww_used + cw;
                    ) col_widths;

            (*
            ** Ass some default space for expandable columns TODO
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
            let height = ref w.w_ipad_y in

            let nrows = Array.length table.tb_rows in
            for row_i = 0 to nrows-1
            do
                let row = table.tb_rows.(row_i) in
                let cols = row.row_cols in
                let ncols = Array.length cols in
                let row_width = ref w.w_ipad_x in
                for col_j = 0 to ncols - 1
                do
                    let col = cols.(col_j) in
                    let frame' = frame_size col.col_shadow in
                    let frame_off = frame_offset col.col_shadow in
                    (*
                    ** Note 1: Padding is applied both inside
                    ** and outside of each button
                    *)
                    row_width := !row_width + col.col_width +
                                 table.tb_colpad_x;

                    let text_font = col.col_font in
                    let h = text_font.font_height +
                            table.tb_colpad_y * 2 + 
                            table.tb_colpad_y * 2 +     (* Note 1 *)
                            frame' in
                    row.row_height <- max row.row_height h;
                done;
                width := max !width !row_width;
                height := !height + row.row_height + row.row_pad;
            done;
            required_width := !required_width + frame + 2 * w.w_ipad_x;

            sz.requested_width <- min sz.max_width
                                      (max (!width + frame)
                                           (sz.min_width + frame -
                                            2 * w.w_ipad_x));
            sz.requested_height <- min sz.max_height
                                      (max (!height + frame - 
                                            w.w_ipad_y)
                                            (sz.min_height + frame -
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


    method update =
        super#update;
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;

    (*
    ** Update one button
    *)
    method update_but row_i col_j = 
        let col = self#get_col row_i col_j in
        let active = col.col_active in
        let sz = szhints in
        let dpy = s.s_display in
        let gcs = s.s_gcs in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        (*
        ** Default fore- and background color. Can be overwritten.
        *)
        let fg = w.w_foreground.c_pixel in
        let bg = w.w_background.c_pixel in
        let text_font = col.col_font in
        let text_font_symbol = col.col_symbol_font in

        let bbox = col.col_shadow.f_bbox in
        let bx0,by0,bw,bh = bbox_to_xywh bbox in

        X.clearArea s.s_display w.w_window 
                    bx0 by0 bw bh false;


        (*
        ** TODO 
        *)
        col.col_shadow.f_type <- 
            (match col.col_shadow.f_type with
             | ShadowRaised
             | ShadowSunken -> if col.col_active 
                                    then ShadowSunken
                                    else ShadowRaised;
             | ReliefRaised
             | ReliefSunken -> if col.col_active 
                                    then ReliefSunken
                                    else ReliefRaised;
             | _ -> col.col_shadow.f_type);



        drawFrame s.s_display w s.s_gcs col.col_shadow true; 

        let text_str = col.col_str in
        let active = col.col_active in
        (*
        ** Shadows are asymetric!
        *)
        let shadow_box = col.col_shadow in
        let bframe = frame_size shadow_box in
        let bframe_off = frame_offset shadow_box in

        let th =  text_font.font_height in
        let tyoff = max 0 (bh - th - bframe)/2 in

        let tw = string_width_S text_font 
                                text_font_symbol
                                text_str in
        let tx = bx0 + (bw - tw - bframe) / 2 + bframe_off in
        let ty = by0 + text_font.font_ascent + tyoff + bframe_off in


        (*
        ** Activated group button is filled with fg and text
        ** is drawn with bg !!
        *)
        if col.col_group <> None && col.col_active &&
           not (col.col_shadow.f_type = ReliefSunken) then
        begin
            match col.col_shadow.f_shape with
            | S_Rect ->
            begin
                let x1,y1,x2,y2 = 
                              bx0+bframe_off+2,
                              by0+bframe_off+2,
                              bx0+bw-1-bframe_off-2,
                              by0+bh-1-bframe_off-2 in
                let gc = GCCache.get_fg_bg_lw gcs fg fg 1 in
                X.fillPoly  s.s_display w.w_window gc Origin Complex
                                     [x1,y1;
                                      x2,y1;
                                      x2,y2;
                                      x1,y2;
                                      x1,y1;  
                                     ];
                X.polyLine  s.s_display w.w_window gc Origin 
                                     [x1,y1;
                                      x2,y1;
                                      x2,y2;
                                      x1,y2;
                                      x1,y1;  
                                     ];
                draw_string_S s.s_display w.w_window gcs
                                       tx ty bg fg
                                       text_font
                                       text_font_symbol
                                       text_str
            end;
            | S_Oval ->
            begin          
                let cr = min (bh/3) 8 in
                let x1,y1,x2,y2 = 
                              bx0+bframe_off+2+1,
                              by0+bframe_off+2+1,
                              bx0+bframe_off+(bw-bframe-1)-2,
                              by0+bframe_off+(bh-bframe-1)-2 in
                let gc = GCCache.get_fg_bg_lw gcs fg fg 1 in

                VX_common.fillRoundedRectangle s.s_display 
                                               w.w_window gc 
                                               x1 y1 
                                               (x2-x1+1)
                                               (y2-y1+1)
                                               cr cr;

                draw_string_S s.s_display w.w_window gcs
                                       tx ty bg fg
                                       text_font
                                       text_font_symbol
                                       text_str
            end;
            | S_Circ ->
                draw_string_S s.s_display w.w_window gcs
                                       tx ty bg fg
                                       text_font
                                       text_font_symbol
                                       text_str

        end
        else
            draw_string_S s.s_display w.w_window gcs
                                       tx ty fg bg
                                       text_font
                                       text_font_symbol
                                       text_str;

                
    (*
    ** Redraw full button table widget. Recalculate bounding boxes.
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            super#refresh;
            let sz = szhints in
            let dpy = s.s_display in
            let gcs = s.s_gcs in
            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            (*
            ** Default fore- and background color. Can be overwritten.
            *)
            let fg = w.w_foreground.c_pixel in
            let bg = w.w_background.c_pixel in
            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let x0 = (frame_off + w.w_ipad_x) in
            let y0 = ref (frame_off + w.w_ipad_y) in

            (*
            ** Auto padding ?
            *)
            let offered_x = width - frame in
            
            let nrows = Array.length table.tb_rows in

            for row_i = 0 to nrows-1
            do
                let row = table.tb_rows.(row_i) in
                let ncols = Array.length row.row_cols in
                let x = ref x0 in
                let rh = row.row_height in

                (*
                ** Size we need
                *)
                let required_x = ref x0 in
                for col_j = 0 to ncols-1
                do
                    let col = row.row_cols.(col_j) in
                    required_x := !required_x + col.col_width + 
                                  table.tb_colpad_x;
                done;
                required_x := !required_x + table.tb_colpad_x;

                let extra_pad_x = 
                    if table.tb_autopad_x then
                        (offered_x - !required_x) / (ncols+1)
                    else
                        0 in

                x := !x + extra_pad_x;

                for col_j = 0 to ncols -1 
                do
                    let col = row.row_cols.(col_j) in
                    let cw = col.col_width in
                    (*
                    ** Padding is applied around button.
                    *)
                    let ch = rh - 2 * table.tb_colpad_y in
                    (*
                    ** Save sub window geometry of this column entry.
                    *)
                    col.col_shadow.f_bbox <- bbox_of_xywh !x !y0 cw ch;

                    (*
                    ** Update button colors.
                    *)
                    (
                     match col.col_shadow.f_type with
                     | ReliefRaised
                     | ReliefSunken ->
                        let bg = if col.col_bg = noColor
                                then w.w_background
                                else col.col_bg in

                        col.col_shadow.f_foreground <- 
                            parent#getShadow bg;
                        col.col_shadow.f_auxiliary <- 
                            parent#getHilite bg;
                        col.col_shadow.f_fillground <- 
                            bg;
                        col.col_shadow.f_background <- 
                                w.w_background;
                     | _ ->
                        col.col_shadow.f_foreground <- 
                            if col.col_fg = noColor
                                then w.w_foreground
                                else col.col_fg;
                        col.col_shadow.f_fillground <- 
                            if col.col_bg = noColor
                                then w.w_background
                                else col.col_bg;
                        col.col_shadow.f_background <- 
                                w.w_background;
                    );
                    self#update_but row_i col_j;
                    x := !x + cw + table.tb_colpad_x + extra_pad_x;
                done;
                y0 := !y0 + rh + row.row_pad;
            done;
        end

    method print_but (ps : ps) (wx0 : int) (wy0 : int) 
                      row_i col_j =
        let col = self#get_col row_i col_j in
        let active = col.col_active in
        let sz = szhints in
        let dpy = s.s_display in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        (*
        ** Default fore- and background color. Can be overwritten.
        *)
        let fc = w.w_foreground in
        let bc = w.w_background in
        let text_font = col.col_font in
        let text_font_symbol = col.col_symbol_font in

        let bbox = col.col_shadow.f_bbox in
        let bx0,by0,bw,bh = bbox_to_xywh bbox in


        printFrame ps w wx0 wy0 col.col_shadow true; 

        let text_str = col.col_str in
        let active = col.col_active in
        (*
        ** Shadows are asymetric!
        *)
        let shadow_box = col.col_shadow in
        let bframe = frame_size shadow_box in
        let bframe_off = frame_offset shadow_box in

        let th =  text_font.font_height in
        let tyoff = max 0 (bh - th - bframe)/2 in

        let tw = string_width_S text_font 
                                text_font_symbol
                                text_str in
        let tx = bx0 + (bw - tw) / 2 + bframe_off in
        let ty = by0 + text_font.font_ascent + tyoff + bframe_off in


        (*
        ** Activated group button is filled with fg and text
        ** is drawn with bg !!
        *)
        if col.col_group <> None && col.col_active &&
           not (col.col_shadow.f_type = ReliefSunken) then
        begin
            match col.col_shadow.f_shape with
            | S_Rect ->
            begin
                let x1,y1,x2,y2 = 
                              bx0+bframe_off+2,
                              by0+bframe_off+2,
                              bx0+bw-1-bframe_off-2,
                              by0+bh-1-bframe_off-2 in
                VX_ps.fillPoly  ps wx0 wy0 fc
                                     [i2f x1,i2f y1;
                                      i2f x2,i2f y1;
                                      i2f x2,i2f y2;
                                      i2f x1,i2f y2;
                                      i2f x1,i2f y1;  
                                     ];
                VX_ps.polyLine  ps wx0 wy0 1.0 fc
                                     [i2f x1,i2f y1;
                                      i2f x2,i2f y1;
                                      i2f x2,i2f y2;
                                      i2f x1,i2f y2;
                                      i2f x1,i2f y1;  
                                     ];
                print_string_S ps wx0 wy0
                               (i2f tx) (i2f ty) bc fc
                               text_font
                               text_font_symbol
                               text_str
            end;
            | S_Oval ->
            begin          
                let cr = min (bh/3) 8 in
                let x1,y1,x2,y2 = 
                              bx0+bframe_off+2+1,
                              by0+bframe_off+2+1,
                              bx0+bframe_off+(bw-bframe-1)-2,
                              by0+bframe_off+(bh-bframe-1)-2 in

                VX_common.printFilledRoundedRectangle 
                          ps wx0 wy0 fc 
                          (i2f x1) (i2f y1) 
                          (i2f (x2-x1+1))
                          (i2f (y2-y1+1))
                          (i2f cr) (i2f cr);

                print_string_S ps wx0 wy0
                               (i2f tx) (i2f ty) bc fc
                               text_font
                               text_font_symbol
                               text_str
            end;
            | S_Circ ->
                print_string_S ps wx0 wy0
                               (i2f tx) (i2f ty) bc fc
                               text_font
                               text_font_symbol
                               text_str

        end
        else
            print_string_S ps wx0 wy0
                           (i2f tx) (i2f ty) fc bc
                           text_font
                           text_font_symbol
                           text_str;

    method print (ps : ps) (wx0 : int) (wy0 : int) =
            super#print ps wx0 wy0;
            self#col_iter (fun row_i col_j row col ->
                    self#print_but ps wx0 wy0 row_i col_j;
                );

end

(*
** Generates a table descriptor. Not specified parameters are filled
** with default values.
*)
let table_gen parent table_cont attributes =
    let table_attr = ref [] in
    let row_attr = ref [||] in
    let col_attr = ref [||] in

    let tb = {
                tb_rows = [||];
                tb_colpad_x = 0;
                tb_colpad_y = 0;
                tb_regular = true;
                tb_autopad_x = false;
             } in


    let nrows = Array.length table_cont in
    let rows = Array.init nrows (fun _ -> {row_cols=[||];
                                           row_height=0;
                                           row_pad=0;}) in

    let default_font = parent#font_make Helvetica Bold 12 true in
    let default_symbol_font = parent#font_make Symbol Bold 12 true in

    let font_changed = ref false in
    let font = font_desc default_font in

    let default_frame = default_frame 1 in
    let default_color = ref noColor in

    let default_width = ref 0 in

    List.iter (fun ta ->
        match ta with
        | IpadX x -> tb.tb_colpad_x <- x;
        | IpadY y -> tb.tb_colpad_y <- y;
        | Rows r -> row_attr := r;
        | Cols c -> col_attr := c;
        | AdjustX b -> tb.tb_autopad_x <- b;
        | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
        | Text_style style -> font_changed := true;
                                  font.text_style <- style;
        | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
        | But attr -> 
                List.iter (fun a ->
                    match a with
                    | Shape s -> default_frame.f_shape <- s;
                    | Frame s -> default_frame.f_type <- s;
                    | Background c -> default_color := 
                            parent#color_make c true;
                    | Color c -> default_color := 
                            parent#color_make c true;
                    | Width w -> default_width := w;
                    | _ -> ();
                    ) attr;
        (*
        ** TODO
        *)
        | Frame s -> default_frame.f_type <- s;
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
                                               default_font.font_size true
        in


    (*
    ** Some sanity checks
    *)
    if !row_attr <> [||] && (Array.length !row_attr <> nrows) then
        vx_error "VX_button.table: invalid length of Rows array";
    if !col_attr <> [||] && (Array.length !col_attr <> nrows) then
        vx_error "VX_button.table: invalid length of Cols array";

    let max_cols = ref 0 in

    for row_i = 0 to nrows - 1
    do
        let row = rows.(row_i) in
        let row_str = table_cont.(row_i) in
        let ncols = Array.length row_str in
        if !max_cols > 0 && ncols <> !max_cols then
            vx_error (sprintf 
            "VX_button.table: invalid number of column entries (got %d, expected %d)" 
            ncols !max_cols);

        max_cols := max !max_cols ncols;
        let cols = Array.init ncols (fun _ -> 
                        {
                            col_str = "";
                            col_font = default_font;
                            col_symbol_font = default_symbol_font;
                            col_fg = noColor;
                            col_bg = !default_color;
                            col_shadow = {default_frame with
                                          f_bbox=bbox_of_xywh 0 0 0 0};
                            col_action = None;
                            col_active = false;
                            col_group = None;
                            col_width = !default_width;
                            col_fixed =  !default_width > 0;
                            col_expand = false;
                        }) in
        row.row_cols <- cols;
        for col_j = 0 to ncols-1
        do
            let font = ref [] in
            let col = cols.(col_j) in
            col.col_str <- table_cont.(row_i).(col_j);

            let font_changed = ref false in
            let font = font_desc col.col_font in

            if !col_attr <> [||] then
            begin
                if (Array.length !col_attr.(row_i) <> ncols) then
                    vx_error "VX_button.table: invalid length of Cols array";
                List.iter (fun ca ->
                    match ca with
                    | Width  w -> col.col_width <- w;
                                  col.col_fixed <- true;
                    (*
                    ** TODO
                    *)
                    | Frame s -> col.col_shadow.f_type <- s;
                    | Shape s -> col.col_shadow.f_shape <- s;
                    | Foreground c -> col.col_fg <- parent#color_make c true;
                    | Background c -> col.col_bg <- parent#color_make c true;
                    | Color c -> col.col_bg <- parent#color_make c true;
                    | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
                    | Text_style style -> font_changed := true;
                                  font.text_style <- style;
                    | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
                    | ActionUU f -> col.col_action <- Some f;
                    | ExpandX b -> col.col_expand <- b;
                                   if col.col_expand then
                                   begin
                                        col.col_fixed <- false;
                                        col.col_width <- 0;
                                   end;

                    | Group g -> col.col_group <- Some g;
                    | Active b -> col.col_active <- b;
                    | _ -> warn_attr "VX_button.table" ca "";
                ) !col_attr.(row_i).(col_j);
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

class table parent table_cont attributes =
    object 
    inherit orig2 parent (table_gen parent table_cont 
                                    attributes) attributes
end
