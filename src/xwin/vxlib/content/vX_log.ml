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

open Xtypes
open VX_types
open VX_common
open VX_view
open VX_slider

(*
** Main widget class
*)

class orig parent rows attributes =
    object (self)
    

    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font = parent#font_make Times Roman 12 true;
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;

    inherit VX_object.t parent attributes as super

    initializer 
        self#set_name "log";
        __(self#configure attributes)

    (*
    ** Text is organized in lines. This array is circular with
    ** a head and tail pointer. Head points to next free line. Tail
    ** is the current first line of the screen. If head = tail then full.
    *)

    val mutable text_lines = Array.create rows ""
    val mutable head = 0
    val mutable tail = 0
    val mutable full = false
    val mutable used = 0

    (*
    ** Part of the log array visisble
    *)
    val mutable visible = rows

    (*
    ** Fixed text size?
    *)
    val mutable fixed = false

    (*
    ** Number of log text rows
    *)
    val mutable text_rows = rows


    (*
    ** Space between text lines in pixel
    *)
    val mutable line_spacing = 1


    (*
    ** Editable area?
    *)
    val mutable edit = false

    (*
    ** Cursor and focus management 
    *)
        
    val mutable cursor_x = 0
    val mutable cursor_y = 0
    val mutable display_cursor = false
    val mutable last_cursor = None
    val cursor = String.create 1

    (*
    ** Pixel width of text content available. Derived from our widget size.
    *)
    method text_width = 
        let sz = szhints in
        let frame = frame_size w.w_frame in
        let width = max sz.min_width w.w_geometry.width in
        width - 2 * w.w_ipad_x - frame 
        
    val mutable last_text_width = 0

    (*
    ** Set text attributes
    *)
    method configure attrs = 
        let sz = szhints in
        (*
        ** First the widget gloabl attributes
        *)
        List.iter (fun a -> match a with 
                            | Width _ -> fixed <- true;
                            | _ -> () )
                  attrs;
        let remains = super#configure attrs in
        (*
        ** Now the special ones...
        *)
        let font = font_desc text_font  in
        let font_changed = ref false in
        let changed = ref false in


        List.iter (fun attr ->
            match attr with
            | Rown n -> visible <- n;     (* TODO *)
                        changed := true;
            | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
                                  changed := true;
            | Text_style style -> font_changed := true;
                                  font.text_style <- style;
                                  changed := true;
            | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
                                  changed := true;

            | _ -> warn_attr "VX_text.log" attr self#name;
        
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
        []      (* leaf widget! *)


        
    (*
    ** Add new lines to the log screen. 
    *) 
    method add_lines sl =
        let head',tail',full' = head,tail,full in

        let nl = List.length sl in
        List.iter (fun s ->
                used <- min (used + 1) text_rows;
                (*
                ** Head always points to next (free) array slot
                *)
                text_lines.(head) <- s;
                head <- head + 1;
                if head = text_rows then
                    head <- 0;

                if full then
                    tail <- head;

                if tail = head then
                begin
                    full <- true;
                end
                else
                    full <- false;


            ) sl;
        let wrapped = head < tail || 
                      (full && not (head = 0)) in

        let scroll = full' || wrapped in

        let first = if not scroll then
                        head'
                    else
                        -1 in
        let last =  if first = (-1) then
                        (-1)
                    else if head <> 0 then
                        (head-1)
                    else
                        text_rows-1 in
                    
        if not (w.w_window == noWindow) && visible > 0 then
        begin
            let is_cursor = display_cursor in
            if edit then
            begin
              display_cursor <- false;
              self#update_cursor;
              last_cursor <- None;
            end;
            self#update_screen first last;
            if edit && is_cursor then
            begin
              cursor_y <- min (used-1) (cursor_y+1);
              display_cursor <- true;
              self#update_cursor;
            end;
        end
        else
        begin
            let is_cursor = display_cursor in
            if edit then
            begin
              display_cursor <- false;
              self#update_cursor;
              last_cursor <- None;
            end;
            self#update;
            if edit && is_cursor then
            begin
              cursor_y <- min (used-1) (cursor_y+1);
              display_cursor <- true;
              self#update_cursor;
            end;
        end;
    (*
    ** Modify a line. Top line has row number 0 independent of head
    ** and tail pointers.
    *)
    method set_line rownum s =
      if rownum >= 0 && rownum < used then
      begin
        let linenum =
             let line' = tail + rownum in
             if line' < used then
                line'
             else
                line' - used 
             in
            
        text_lines.(linenum) <- s;
        if not (w.w_window == noWindow) then
        begin
          let is_cursor = display_cursor in
          if edit && is_cursor && cursor_y = rownum then
          begin
              display_cursor <- false;
              self#update_cursor;
              last_cursor <- None;
          end;
          self#update_screen linenum linenum;
          if edit && is_cursor && cursor_y = rownum then
          begin
              display_cursor <- true;
              self#update_cursor;
          end;
        end
        else
          self#update;
      end
      
    method update =
Db.Pr.s 10 "VX_log#update";
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;
        parent#update    


    (*
    ** Get the text_lines array with actual head and tail pointer.
    *)
    method get_lines = text_lines,head,tail

    method get_line rownum =
      if rownum >= 0 && rownum < used then
      begin
        let linenum =
             let line' = tail + rownum in
             if line' < used then
                line'
             else
                line' - used 
             in
        text_lines.(linenum)
      end
      else ""
      

    (*
    ** Last line actually used 
    *)
    
    method last_row = if used > 0 then used - 1 else 0
    
    (*
    ** Clear the entire log screen
    *)
    method clear_log =
        head <- 0;
        tail <- 0;
        full <- false;
        used <- 0;
        if edit then
        begin
            display_cursor <- false;
            self#update_cursor;
        end;
        if not (w.w_window == noWindow) then
            self#update;

    (*
    ** Return required sizes
    *)

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
Db.Pr.s 10 "VX_text.log#size_request";

            sz.comp_timestamp <- s.s_timestamp;
            let width = max sz.min_width w.w_geometry.width in
            
            sz.requested_width <- min width sz.max_width;
                
            let n = if visible = 0 then 
                        min (max 1 used) text_rows
                    else visible in

            sz.requested_height <- 
                    min sz.max_height
                        (max (text_font.font_height * n +
                              (n-1) * line_spacing + 
                              frame + 2 * w.w_ipad_y )
                             sz.min_height);

Db.Pr.sdd 10 "VX_text.log#size_request w/h" sz.requested_width
                                              sz.requested_height;
            sz                    
        end;

    (*
    ** Do a full or partial redraw of text content.
    *)
    method update_screen first last =
        let sz = szhints in

Db.Pr.sdd 10 "VX_text.log#update_screen first,last" first last;
Db.Pr.sddd 10 "VX_text.log#update_screen head,tail,full" head tail 
             (if full then 1 else 0);

        let fg = w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in
        let font = text_font.font_id in
        let gcs = s.s_gcs in
        let gc = GCCache.get_fg_bg_font gcs fg bg font in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        (*
        ** Draw all text starting at tail upto head-1
        *)
        if not (w.w_window == noWindow) &&
           (head <> tail || full) then
        begin    
            (*
            ** Draw multiline text. Text was broken up into fragments
            ** and inserted in text_lines list.
            *)

            let th =  text_font.font_height in
            let ty =  ref (text_font.font_ascent + frame_off + 
                               w.w_ipad_y) in


            let wrapped = head < tail || 
                              (full && not (head = 0)) in

            let parts = if not wrapped 
                                then [tail,(if head = 0 
                                                then text_rows-1
                                                else head-1)]
                                else
                                    [tail,text_rows-1;
                                     0,head-1] in
            List.iter (fun (part_start,part_end) ->
                  
            (*
            ** We can speed up drawing considerably if only
            ** visible parts of a tree are drawn (inside
            ** viewport!).
            *)
            let visible bbox = clip_visible w.w_clipping bbox in

            (*
            ** Text is always left aligned.
            *)
            let tx = frame_off + w.w_ipad_x in
    
            let text_width = self#text_width in

            for i = part_start to part_end
            do
                let do_refresh = 
                    first = (-1) ||
                    last = (-1) ||
                    (last >= first && i >= first && i <= last) ||
                    (last < first && i >= first) ||
                    (last < first && i <= last) in

                    
                if do_refresh && (if w.w_clipped then
                                    visible (bbox_of_xywh tx !ty 
                                                          text_width
                                                          th)
                                  else
                                    true) then
                begin
Db.Pr.sd 10 "draw" i;                    
                    let line = text_lines.(i) in
                    let tlen = String.length line in
                    let tw   = string_width_S text_font 
                                              text_font_symbol
                                              line in



                    (*
                    ** Only clear content!!! Keep borders...
                    *)


                    X.clearArea s.s_display w.w_window
                            (frame_off + w.w_ipad_x)
                            (!ty - text_font.font_ascent)
                            (width - frame - 2 * w.w_ipad_x)
                            th false;


                    draw_string_S s.s_display w.w_window gcs
                                       tx !ty fg bg
                                       text_font
                                       text_font_symbol
                                       line;

                end;
                ty := !ty + th + line_spacing;
            done;
            ) parts;
        end;
        self#update_cursor
        

    (*
    ** Draw the log screen
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
Db.Pr.s 10 "VX_log.t#refresh";
            super#refresh;
            self#update_screen (-1) (-1);
        end; 


    (*
    **  Cursor support for editable window.
    **  The cursor got a new position. Restore old position,
    **  draw new position.
    *)
    method update_cursor =
      if not (w.w_window == noWindow) then
      begin
Db.Pr.sddd 10 "VX_log.orig#update_cursor" cursor_x cursor_y 
            (if display_cursor then 1 else 0);
        let sz = szhints in
        let fg = w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in
        let font = text_font.font_id in
        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        if (last_cursor <> None) then
        begin
            (*
            ** Restore old position...
            *)
            let gc = GCCache.get_fg_bg_font s.s_gcs fg bg 
                                            text_font.font_id in
            let xc,yc,c = match last_cursor with 
                          | Some lc -> lc;
                          | None -> assert false;
                in
            cursor.[0] <- c;

            Xlib.imageSubString s.s_display w.w_window gc  
                            xc yc cursor 0 1;
        end;

        if display_cursor then
        begin
            (*
            ** Editable text. Draw new cursor position...
            *)

            let th = text_font.font_height in
            let ty = ref (text_font.font_ascent + frame_off + 
                          w.w_ipad_y) in


            let cursor_pos = ref (0,0,' ') in

            let maxrow = used-1 in

            protect (for n = 0 to maxrow
            do
                let index = 
                    let i = n + tail in
                    if i < used then i else (i-used) in

                let line = text_lines.(index) in
                let tlen = String.length line in
                let tx = frame_off + w.w_ipad_x in

                (*
                ** Check cursor position.
                *)
                if cursor_x < tlen &&
                   cursor_y = n then
                begin
                    let pos' = cursor_x in
                    cursor_pos := (tx + (string_width text_font
                                            (String.sub line 0 pos')),
                                   !ty,
                                   line.[pos']);
                    raise Exit;
                end
                else if cursor_y = n then
                begin
                    cursor_pos := (tx + (string_width text_font line),
                                   !ty,
                                   ' ');
                    raise Exit;
                end;  

                ty := !ty + th + line_spacing;

            done);

            let gc = GCCache.get_fg_bg_font s.s_gcs bg fg 
                                            text_font.font_id in
            let x',y',c = !cursor_pos in
            cursor.[0] <- c;
            last_cursor <- Some (x',y', cursor.[0]);
            Xlib.imageSubString s.s_display w.w_window gc  
                                        x' y' cursor 0 1;
        end   
      end

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0 =
        super#print ps wx0 wy0;

end

class t = orig

(*
** Hybrid widget consisting of VX_log.t log window contained in vertical 
** VX_view.v viewport with scrollbar.
*)
 
class view_v parent rows attributes =
    object (self)
    inherit VX_view.v parent ([But [Frame ReliefRaised;Color "grey80"]]@
                              attributes) as super

    val mutable log = None
    initializer
        let lw = new t self#container rows ([Rown 0; ExpandX true;
                                            ]@
                                            (List.filter (fun a ->
                                             match a with
                                             | Text_font _ -> true;
                                             | Text_style _ -> true;
                                             | Text_size _ -> true;
                                             | Background _ -> true;
                                             | _ -> false) attributes)) in
        self#container_add lw#contained;
        log <- Some lw;

    method hide =
        (match log with
        | Some w -> w#hide;
        | None -> ();
        );
        super#hide;

    method destroy =
        (match log with
        | Some w -> w#destroy;
        | None -> ();
        );
        super#destroy; 
    
    method add_lines ls =
        match log with
        | Some w -> w#add_lines ls;
        | None -> ();

    method set_line rownum s =
        match log with
        | Some w -> w#set_line rownum s;
        | None -> ();

      
    method clear_log =
        match log with
        | Some w -> w#clear_log;
        | None -> ();

    method last_row =
        match log with
        | Some w -> w#last_row;
        | None -> 0;

    method get_line rownum =
        match log with
        | Some w -> w#get_line rownum;
        | None -> "";

    method get_lines =
        match log with
        | Some w -> w#get_lines;
        | None -> [||],0,0;

    method refresh =
Db.Pr.s 10 "VX_log.view_v#refresh";
        super#refresh;
end

(*
** Editable log window widget
*)
class edit parent cols rows attributes =
    object (self)
    inherit orig parent rows ([Cursor (FontCursor XC.xc_hand1)]@attributes)
            as super

    (*
    ** Action handler called any time a newline was hit.
    *)
    val mutable action_nl = None

    (*
    ** Action handler called each time the boundary of the
    ** editable area was reached.
    *)
    val mutable action_left = None
    val mutable action_right = None
    val mutable action_up = None
    val mutable action_down = None

    val mutable text_cols = (cols:int)
    
    (*
    ** Editable area of window (in col/row units)
    *)
    val mutable edit_x0 = 0
    val mutable edit_y0 = 0
    val mutable edit_y1 = rows
    
    
    initializer
        self#set_name "log_edit";
        display_cursor <- false;
        cursor_x <- 0;
        cursor_y <- 0;
        edit <- true;
        
        __(self#configure [Bindings [
          (*
          ** Keyboard and mouse user interaction.
          *)
          EnterWindow,(fun _ ->
                display_cursor <- true;
                self#update_cursor;
            );
          LeaveWindow,(fun _ ->
                display_cursor <- false;
                self#update_cursor;
            );
          ButtonPress,(fun _ ->
              if edit && display_cursor then
              begin

                let x,y= !mouse_x_event,
                         !mouse_y_event in
Db.Pr.sdd 10 "mouse" x y;
                let frame = frame_size w.w_frame in
                let frame_off = frame_offset w.w_frame in
                let g = w.w_geometry in
                let width = g.width in

                let maxrow = used-1 in
                let newpos = ref (0,0) in
                let th = text_font.font_height in
                let ty = ref (frame_off + w.w_ipad_y) in
                let th' = th + line_spacing in
                
                protect (for n = 0 to maxrow
                do
                  let index = 
                    let i = n + tail in
                    if i < used then i else (i-used) in

                  if (y <= (!ty + th') && y > !ty) then
                  begin
                    let line = text_lines.(index) in
                    let tlen = String.length line in
                    let tx0 = frame_off + w.w_ipad_x in
                    let tw = string_width text_font 
                                          line in
                    if (x >= tx0 && x <= (tx0+tw)) then
                    begin
                        (*
                        ** Find position in text string...
                        *)
                        protect ( for i = 1 to tlen 
                        do
                            let str' = String.sub line 0 i in
                            let tw' = string_width text_font
                                                   str' in
                            if x <= (tx0 + tw') then
                            begin
                                newpos := (i-1),n;
                                raise Exit;
                            end;
                        done);
                        raise Exit;
                    end
                    else
                    begin
                      (*
                      ** Put cursor at end of line
                      *)
                      newpos := tlen,n;
                    end;
                  end;
                  
                  ty := !ty + th';
                done);
                let x',y' = !newpos in
                if x' >= edit_x0 && y' >= edit_y0 && y' <= edit_y1 then
                  self#set_cursor x' y';
              end;   
            );
          Key (XK.xk_Left, 0), (fun _ ->
              if edit then
              begin
                let c' = max 0 (cursor_x - 1) in
                if c' >= edit_x0 then
                begin
                  cursor_x <- c';
                  self#update_cursor;  
                end
                else
                begin
                  match action_left with
                  | Some f -> f cursor_y;
                  | None -> ();
                end;
              end;
            );
          Key (XK.xk_Right, 0), (fun _ ->
              if edit then
              begin
                let line = self#get_line cursor_y in
                let tlen = String.length line in
                if (cursor_x < tlen) then
                begin
                  cursor_x <- cursor_x + 1;
                  self#update_cursor;  
                end
                else
                begin
                  match action_right with
                  | Some f -> f cursor_x cursor_y;
                  | None -> ();
                end;                
              end;
            );
          Key (XK.xk_Up, 0), (fun _ ->
              if edit then
              begin
                let c' = max 0 (cursor_y-1) in
                if c' >= edit_y0 && c' <= edit_y1 then
                begin
                  cursor_y <- c';
                  self#update_cursor;              
                end
                else
                begin
                  match action_up with
                  | Some f -> f cursor_y;
                  | None -> ();
                end;                
              end;
            );
          Key (XK.xk_Down, 0), (fun _ ->
              if edit then
              begin
                let c' = cursor_y+1 in
                if c' >= edit_y0 && c' <= edit_y1 then
                begin
                  cursor_y <- c';
                  self#update_cursor;              
                end
                else
                begin
                  match action_down with
                  | Some f -> f cursor_y;
                  | None -> ();
                end;                
              end;
            );
          Key (XK.xk_Return, 0), (fun _ ->
            if edit then match action_nl with
            | Some f -> display_cursor <- false;
                        self#update_cursor;
                        last_cursor <- None;
                        let last = cursor_y = (used-1) in

                        f cursor_y;

                        if last && cursor_y < (used-1) then
                          cursor_y <- used-1;
                          
                        display_cursor <- true;
                        self#update_cursor;
            | None -> ();
            );
            
          Key (XK.xk_BackSpace, 0), (fun _ ->
            if edit then 
            begin
              let line = self#get_line cursor_y in
              let tlen = String.length line in
              if cursor_x > edit_x0 then
              begin
                let newline = 
                  (String.sub line 0 (cursor_x-1))^
                  (String.sub line (cursor_x) (tlen-cursor_x)) in

                display_cursor <- false;
                self#update_cursor;
                last_cursor <- None;                
                self#set_line cursor_y newline;

                cursor_x <- cursor_x - 1;

                display_cursor <- true;
                self#update_cursor;
              end
              else
              begin
                  match action_left with
                  | Some f -> f cursor_y;
                  | None -> ();
              end;
            end;
            );

          Key (anyKey, anyModifier), (fun _ ->
              if edit && !key_string <> "" then
              begin
                (*
                ** Enough space for a new character in current line?
                *)
                let line = self#get_line cursor_y in
                let tlen = String.length line in
                if (tlen < text_cols) then
                begin
                  let newline = 
                    (String.sub line 0 cursor_x)^
                    !key_string^
                    (if cursor_x < tlen then
                      String.sub line cursor_x (tlen-cursor_x)
                     else
                       "") in
                       
                  display_cursor <- false;
                  self#update_cursor;
                  last_cursor <- None;                
                  self#set_line cursor_y newline;

                  cursor_x <- cursor_x + 1;

                  display_cursor <- true;
                  self#update_cursor;
                end
                else 
                begin
                  match action_right with
                  | Some f -> f cursor_x cursor_y;
                  | None -> ();
                end;
              end
            );                                              
          FocusIn ,(fun _ -> 
Db.Pr.s 10 "fi";
              display_cursor <- true; 
              self#wait_refresh true 0 0 0 0;
              self#update_cursor;
              );
          FocusOut ,(fun _ -> 
Db.Pr.s 10 "fo";
              display_cursor <- false; 
              self#wait_refresh true 0 0 0 0
              );
          ]]);

      
    method set_cursor x y =
        cursor_x <- min x text_cols;
        cursor_y <- min y (used-1);
        self#update_cursor

    method cursor en =
        display_cursor <- en;
        self#update_cursor
        

    (*
    ** Specify editable col and row range. Default: full window area.
    ** (Col/row units).
    *)
    method set_editrange col_start row_start row_end =
        edit_x0 <- col_start;
        edit_y0 <- row_start;
        edit_y1 <- row_end
        

    method add_lines sl =
        cursor_x <- edit_x0;
        super#add_lines sl;
        self#update_cursor
        
    (*
    ** (fun rownum -> unit)
    *)
    method set_action_nl (f: (int -> unit)) =
        action_nl <- Some f

    (*
    ** (fun rownum -> unit)
    *)
    method set_action_left (f: (int -> unit)) =
        action_left <- Some f

    (*
    ** (fun colnum rownum -> unit)
    *)
    method set_action_right (f: (int -> int -> unit)) =
        action_right <- Some f

    (*
    ** (fun rownum -> unit)
    *)
    method set_action_up (f: (int -> unit)) =
        action_up <- Some f
    method set_action_down (f: (int -> unit)) =
        action_down <- Some f
        
    (*
    ** Enable/disable editing
    *)
    method set_edit en =
      if en && not edit then
      begin
          display_cursor <- true;
          self#update_cursor;
          edit <- true;
      end
      else if not en && edit then
      begin
          display_cursor <- false;
          self#update_cursor;
          last_cursor <- None;
          edit <- false;
      end 

end

                                      
(*
** Hybrid widget consisting of VX_log.edit log window contained in vertical 
** VX_view.v viewport with scrollbar.
*)
 
class editview_v parent cols rows  attributes =
    object (self)
    inherit VX_view.v parent ([But [Frame ReliefRaised;Color "grey80"]]@
                              attributes) as super

    val mutable log = None
    initializer
        let lw = new edit self#container cols rows 
                            ([Rown 0; ExpandX true;
                             ]@
                             (List.filter (fun a ->
                                             match a with
                                             | Text_font _ -> true;
                                             | Text_style _ -> true;
                                             | Text_size _ -> true;
                                             | Background _ -> true;
                                             | _ -> false) attributes)) in
        self#container_add lw#contained;
        log <- Some lw;

    method hide =
        (match log with
        | Some w -> w#hide;
        | None -> ();
        );
        super#hide;

    method destroy =
        (match log with
        | Some w -> w#destroy;
        | None -> ();
        );
        super#destroy; 
    
    method add_lines ls =
        match log with
        | Some w -> w#add_lines ls;
        | None -> ();

    method set_line rownum s =
        match log with
        | Some w -> w#set_line rownum s;
        | None -> ();

      
    method clear_log =
        match log with
        | Some w -> w#clear_log;
        | None -> ();

    method last_row =
        match log with
        | Some w -> w#last_row;
        | None -> 0;

    method get_line rownum =
        match log with
        | Some w -> w#get_line rownum;
        | None -> "";

    method get_lines =
        match log with
        | Some w -> w#get_lines;
        | None -> [||],0,0;

    method set_action_nl f =
        match log with
        | Some w -> w#set_action_nl f;
        | None -> ();

    method set_action_left f =
        match log with
        | Some w -> w#set_action_left f;
        | None -> ();

    method set_action_right f =
        match log with
        | Some w -> w#set_action_right f;
        | None -> ();

    method set_action_up f =
        match log with
        | Some w -> w#set_action_up f;
        | None -> ();
    
    method set_action_down f =
        match log with
        | Some w -> w#set_action_down f;
        | None -> ();

    method set_edit e =
        match log with
        | Some w -> w#set_edit e;
        | None -> ();

    method set_editrange x0 y0 y1 =
        match log with
        | Some w -> w#set_editrange x0 y0 y1;
        | None -> ();

    method set_cursor x y =
        match log with
        | Some w -> w#set_cursor x y;
        | None -> ();

    method cursor en =
        match log with
        | Some w -> w#cursor en;
        | None -> ();

    method update_cursor=
        match log with
        | Some w -> w#update_cursor;
        | None -> ();

        
    method refresh =
Db.Pr.s 10 "VX_log.editview_v#refresh";
        super#refresh;

            
end
