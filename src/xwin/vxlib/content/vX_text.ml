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
**    $CREATED:     16.5.2005
**    $VERSION:     1.27
**
**    $INFO:
**
**  VXlib text widget.
**
**  1. Simple text label (single line)
**  2. Multiline text 
**
**  3. Editable (mutable) text (single line)
**  4. Multliline editable (mutable) text
**
**
**  Simple text: just a string (auto formatted and line broken).
**  The width of the text widget can be calculated from the
**  current text string or explicitly by attributes (MinWidth,
**  Width). 
**  The number of text lines is either determined automatically
**  or fixed with Rown attribute. If the Rown value is 0, the
**  row number is determined automatically, either if > 1 it's a fixed
**  value.
**
**  class t,text : non editable text.
**                  Rown 0      -> multiline text, auto heigth scale
**                  Rown > 1    -> multiline text, fixed number of rows
**                  
**
**  class edit : same as above, but editable
**          
**  class label: simple, single line text widget. A subset of class text.
**  
**
**  Non editable text strings can contain embedded symbols in TeX style
**  format:
**      "This is greek \\alpha and \\mu."
**
**  The appropiate symbol font is derived (in size and style) from the
**  specified generic text font.
**
**
**    $ENDOFINFO
**
*)


open Xtypes
open VX_types
open VX_common
open Printf
open GCCache

let warn str =
    print_string ("Warning: "^str);
    print_newline ()





(*
** Break up string into space separated atoms.
*)
let atoms_of_str str =
    Str.split (Str.regexp " ") str 

(*
** Return list of formatted text lines from atom text list. 
** The maximal line_width in pixel units may not be exceeded. 
** In the case a text atom string is wider than line_width, this atom 
** must be broken!
** Also, a new text string concatenated from the atoms and the remaining
** pixel space in the last line is returned.
*)

let format_lines font atoms line_width =
    let find_nl str = 
        let i = ref 0 in 
        let cnt = ref 0 in
        String.iter (fun c -> if c = '\n' then 
                              begin
                                incr cnt;
                                str.[!i] <- ' ';  (* delete NL *)
                              end;
                              incr i) str;
        !cnt
        in

    let lines = ref [] in
    let curline = ref "" in
    let curlen = ref 0 in
    let newstr = ref "" in

    let last_pad = ref 0 in

    List.iter (fun a ->
            let a' = a ^ " " in
            let is_symbol = (a <> "" && a.[0] = '\\') in
 
            let len' = if not is_symbol then
                        string_width font a'
                       else
                        string_width_Xn font 2 in

            let nl = find_nl a' in


            begin
                if (!curlen + len') > line_width then
                begin
                    if !curline <> "" then
                    begin
                        lines := !lines @ [!curline];
                        newstr := !newstr ^ !curline;
                        last_pad := line_width - !curlen;
                    end;

                    if len' <= line_width then
                    begin    
                        curlen := len';
                        curline := a';                    
                    end
                    else
                    begin
                        (*
                        ** Break up word into pieces...
                        *)
                        let c_tmp = " " in
                        let cur = ref "" in
                        String.iter (fun c ->
                                c_tmp.[0] <- c;
                                let len'' = string_width font (!cur^c_tmp) in
                                if len'' > line_width then
                                begin
                                    lines := !lines @ [!cur];
                                    newstr := !newstr ^ !cur;                                   
                                    cur := String.copy c_tmp;
                                end
                                else
                                    cur := !cur ^ c_tmp;
                            ) a';
                        curline := !cur;
                        curlen := string_width font !cur;                    
                        last_pad := line_width - !curlen;
                    end;
                end
                else
                begin
                    curlen := !curlen + len';
                    curline := !curline ^ a';
                end;
            end;

            if nl > 0 then
            begin
                (*
                ** Insert empty lines...
                ** Expecting "end.\n"
                *)
                lines := !lines @ [!curline];
                let slen = String.length !curline in
                if slen > 0 then
                begin
                    let str' = String.copy !curline in
                    str'.[slen-2] <- '\n';
                    newstr := !newstr ^ str';
                end;
                last_pad := line_width;
                curlen := 0;
                curline := "";
            end;

        ) atoms;
    if !curline <> "" then 
    begin
        newstr := !newstr ^ !curline;
        lines := !lines @ [!curline];
        last_pad := line_width - !curlen;
    end;
    !lines, !newstr, !last_pad


    
(*
** Basic text class shared by text and exit class. Single and multiline text 
** display is provided. Embedded symbols with TeX syntax are
** supported in non editable text widgets. A slightly complex dragon... 
*)

class orig parent (text : string) attributes =
    object (self)
    

    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font = parent#font_make Times Roman 12 true;
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;

    inherit VX_object.t parent attributes as super

    initializer 
        self#set_name "text";
        __(self#configure attributes)
            



    (*
    ** Text content: 
    **  text_str: one text string (single and multiline)
    **  text_lines: splitted into lines for multiline texts
    *)
    val mutable text_str = text
    val mutable text_lines = []

    (*
    ** Fixed text size?
    *)
    val mutable fixed = false

    (*
    ** Single or multiline texts:
    **  Number of text rows  = 0 -> auto height calculated multiline text
    **                       = 1 -> single line
    **                       > 1 -> fixed multiline height
    *)
    val mutable text_rows = 1


    (*
    ** Space between text lines in pixel
    *)
    val mutable line_spacing = 1

    (*
    ** Text alignment and font settings
    *)
    val mutable text_align = Left

    (*
    ** Content editable ? Set by derived edit widget class.
    *)
    val mutable text_edit = false

    (*
    ** Remaining pixel space of (last) text line.
    *)
    val mutable text_lastpad = 0;

    (*
    ** Some attributes
    *)
    val mutable multi_line = false
    val mutable base_line = None
    
    
    (*
    ** Cursor and focus management if any
    *)
    val cursor = String.create 1
    val mutable cursor_x = -1
    val mutable display_cursor = false
    val mutable last_cursor = None

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
            | Rown n -> text_rows <- n;
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

            | Align align      -> text_align <- align;
                                  changed := true;
            | Text_baseline attr  -> 
                    base_line <- Some (create_baseline w 
                                        (fun c -> parent#color_make c true)
                                        attr);
                    changed := true;
            | Mutable m -> text_edit <- m;
                    changed := true
            | _ -> warn_attr "VX_text" attr self#name;
        
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

        if !changed = true then
        begin
            if text_rows = 0 || text_rows > 1 then 
            begin
                multi_line <- true;
                last_text_width <- 0;   (* force reformat *)
                self#update_multiline;
            end
            else
            begin
                multi_line  <- false;
                text_rows <- 1;
                text_lines <- [];
                text_lastpad <- self#text_width -
                                (if not text_edit then
                                    string_width_S text_font
                                                   text_font_symbol 
                                                   text_str
                                 else
                                    string_width text_font 
                                                 text_str);
            end;

            self#wait_resize;
            self#wait_refresh true 0 0 0 0;            
        end;
        []      (* leaf widget! *)

    (*
    ** Reformat multiline text
    *)
    method update_multiline =
        let tw = self#text_width in
        if tw <> last_text_width then
        begin
Db.Pr.s (10) "VX_text#update_multiline really";
            let lines,newstr,last_pad = 
                        format_lines text_font
                                     (atoms_of_str text_str)
                                     self#text_width in

            text_lastpad <- if text_rows = 0 ||
                                   (List.length lines) <= text_rows
                                    then last_pad
                                    else 0;
            text_lines <- lines;
            text_str <- newstr;     (* must be kept consistent *)
            last_text_width <- tw;
        end;

    method update =
Db.Pr.s (10) "VX_text#update";
        if multi_line then self#update_multiline;
        super#update;

    method size_allocate x y dx dy =
        __(super#size_allocate x y dx dy);
Db.Pr.sdd (10) "VX_text#size_allocate: w/h" dx dy; 
        if multi_line && dx <> last_text_width then
        begin
Db.Pr.sd (10) "VX_text#size_allocate: update_multiline last" last_text_width; 
            self#update_multiline;
            w.w_size_modified <- true;  (* force recalc. of size *)
            szhints.comp_timestamp <- (-1);
            false
        end
        else
            true
        
    (*
    ** Set (replace) and get text string.
    *) 
    method set_text s =
        text_str <- s;
        if multi_line then 
        begin
            last_cursor <- None;
            last_text_width <- 0;   (* force reformat *)
            self#update_multiline;
        end
        else
            last_cursor <- None;
            text_lastpad <- self#text_width - 
                            (if not text_edit then
                                string_width_S text_font text_font_symbol s
                             else
                                string_width text_font s);

        self#wait_resize;
        self#wait_refresh true 0 0 0 0;

    method get_text = text_str 


    (*
    ** Calculate the size of this text widget:
    **
    **      1. from current string width and text font height ( mult.
    **         with  lines)
    **      2. from min width, min height
    **      3. from max width, max height
    **
    **
    *)

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            sz.comp_timestamp <- s.s_timestamp;
            let width = max sz.min_width w.w_geometry.width in

            let required = (if multi_line then
                            begin
                                if not text_edit then
                                    string_width_max_S text_font
                                                       text_font_symbol
                                                       text_lines
                                else
                                    string_width_max text_font
                                                     text_lines;
                            end
                            else
                                string_width_S text_font
                                               text_font_symbol
                                               text_str
                            ) + frame + 2 * w.w_ipad_x in

            if not fixed then
                sz.requested_width <-
                            min sz.max_width
                                (max required sz.min_width)
            else
                sz.requested_width <-
                            min sz.max_width sz.min_width;


            let text_height' = 
                if multi_line && text_rows = 0 then
                    (List.length text_lines)        (* auto height scale *)
                else 
                    text_rows
                in

            let base_line' = match base_line with
                             | Some b -> b.tb_width;
                             | None -> 0; in
                
            sz.requested_height <- 
                    min sz.max_height
                        (max (text_font.font_height * text_height' +
                              (text_height' - 1 ) * line_spacing + 
                              frame + 2 * w.w_ipad_y + 
                              text_height' * base_line')
                             sz.min_height);
Db.Pr.sdd (10) "VX_text#size_request w/h" sz.requested_width
                                          sz.requested_height;
            sz                    
        end;

    (*
    ** Draw the graphics content: text, basline, cursor...
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            super#refresh;
            let sz = szhints in

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

            if not multi_line then
            begin
                (*
                ** Draw one single line.
                *)
                let th     =  text_font.font_height in
                let tyoff  = max 0 (height - th - 
                                    2 * w.w_ipad_y - frame)/2
                            in
                let tlen = String.length text_str in
                let tw   = if not text_edit then
                                string_width_S text_font 
                                               text_font_symbol
                                               text_str
                            else
                                string_width text_font 
                                             text_str in

                let tx = 
                    match text_align with
                    | Left -> frame_off + w.w_ipad_x 
                    | Center -> (width - tw)/2
                    | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                    | _ -> warn_attr "VX_text" (Align text_align) self#name; 
                           frame_off + w.w_ipad_x
                    in
                let ty =  w.w_ipad_y + frame_off +
                          text_font.font_ascent + tyoff in
                (*
                ** Draw text to window
                *)
                X.clearArea s.s_display w.w_window 
                                        frame_off frame_off  
                                        (width - frame)
                                        (height - frame) false;
                if text_edit then
                    Xlib.drawSubString s.s_display w.w_window gc  
                                   tx ty 
                                   text_str 0 tlen
                else
                    draw_string_S s.s_display w.w_window gcs
                                       tx ty fg bg
                                       text_font
                                       text_font_symbol
                                       text_str;
                
                (*
                ** Draw baseline if any.    
                *)
                (
                    match base_line with
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

                        let x0 = w.w_ipad_x in
                        let x1 = width - w.w_ipad_x in

                        let y' = ty + (text_font.font_height -
                                       text_font.font_ascent) + 
                                       (blw/2) + 1 in

                        X.polyLine s.s_display w.w_window gc' Origin
                                   [x0,y';x1,y'];
                    end;
                    | None -> ();
                );

                if display_cursor then 
                begin
                    (*
                    ** editable text widget!
                    *)
                    let gc = GCCache.get_fg_bg_font s.s_gcs bg fg 
                                                    text_font.font_id in
                    cursor.[0] <- if cursor_x > (String.length text_str) - 1 
                                    then ' ' 
                                    else text_str.[cursor_x];
                    let xc,yc = (tx+(string_width text_font 
                                      (String.sub text_str 0 cursor_x))),
                                ty in

                    Xlib.imageSubString s.s_display w.w_window gc  
                                        xc yc cursor 0 1;

                    last_cursor <- Some (xc,yc,cursor.[0]);
                end;
            end
            else 
            begin    
                (*
                ** Draw multiline text. Text was broken up into fragments
                ** and inserted in text_lines list.
                ** Intial text may contain newline characters or not.
                *)
                let nlines = if text_rows = 0 then
                                List.length text_lines  (* auto height scale *)
                             else
                                text_rows in

                let th =  text_font.font_height in
                let ty = ref (text_font.font_ascent + frame_off + 
                              w.w_ipad_y) in


                let cursor_pos = ref (0,0,' ') in
                let tot_len = ref 0 in
                let last_y = ref 0 in
                let last_x = ref 0 in
                let last_width = ref 0 in
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
                    let tw   = if not text_edit then
                                    string_width_S text_font 
                                                   text_font_symbol
                                                   line
                                else
                                    string_width text_font 
                                                 line in

                    let tx = 
                        match text_align with
                        | Left -> frame_off + w.w_ipad_x 
                        | Center -> (width - tw - frame)/2
                        | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                        | _ -> warn_attr "VX_text" (Align text_align) self#name;
                               frame_off + w.w_ipad_x
                        in
                    last_x := tx;
                    last_y := !ty;
                    last_width := width;

                    (*
                    ** Only clear content!!! Keep borders...
                    *)


                    X.clearArea s.s_display w.w_window
                            (frame_off + w.w_ipad_x)
                            (!ty - text_font.font_ascent)
                            (width - frame - 2 * w.w_ipad_x)
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
                    ** Check cursor position.
                    *)
                    if display_cursor then
                    begin
                        (*
                        ** editable text widget
                        *)
                        if cursor_x >= !tot_len &&
                           cursor_x < (!tot_len + tlen) then
                        begin
                            let pos' = cursor_x - !tot_len in
                            cursor_pos := (tx + (string_width text_font
                                                  (String.sub line 0 pos')),
                                          !ty,
                                          line.[pos']);
                        end; 
                    end;

                    (*
                    ** Draw baseline if any.    
                    *)
                    (
                      match base_line with
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

                        let x0 = frame_off + w.w_ipad_x in
                        let x1 = width - frame_off - w.w_ipad_x in
                        let y' = !ty - line_spacing + 
                                 text_font.font_ascent / 3 + 1  in

                        let y'  = !ty + (th -
                                         text_font.font_ascent) + 
                                        (blw/2) + 1 in

                        X.polyLine s.s_display w.w_window gc' Origin
                                   [x0,y';x1,y'];
                        ty := !ty + blw;
                      end;
                      | None -> ();
                    );


                    tot_len := !tot_len + tlen;
                    ty := !ty + th + line_spacing;

                  end;
                  ) text_lines;

                if display_cursor then 
                begin
                    let gc = GCCache.get_fg_bg_font s.s_gcs bg fg 
                                                    text_font.font_id in
                    let x',y',c = !cursor_pos in
                    cursor.[0] <- if cursor_x > !tot_len - 1
                                        then ' ' 
                                        else c;
                                   

                    let x'',y'' = 
                        if cursor_x > !tot_len - 1 then
                            (!last_x + !last_width), !last_y
                        else
                            x',y' in
                
                    last_cursor <- Some (x'',y'', cursor.[0]);
                    Xlib.imageSubString s.s_display w.w_window gc  
                                        x'' y'' cursor 0 1;
                end;
                        
            end;

        end; 

    (*
    **  The cursor got a new position. Restore old position,
    **  draw new position.
    *)
    method update_cursor =
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
            if not multi_line  then
            begin
                (*
                ** Single line text.
                *)
                let th    =  text_font.font_height in
                let tyoff = max 0 (height - th - 
                                   2 * w.w_ipad_y - frame)/2
                            in
                let ty = text_font.font_ascent + tyoff in
                let tlen = String.length text_str in
                let tw = string_width text_font 
                                      text_str in

                let tx = 
                        match text_align with
                        | Left -> frame_off + w.w_ipad_x 
                        | Center -> (width - frame - tw)/2
                        | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                        | _ -> warn_attr "VX_text" (Align text_align) self#name;
                               frame_off + w.w_ipad_x
                        in
                let gc = GCCache.get_fg_bg_font s.s_gcs bg fg 
                                                text_font.font_id in
                cursor.[0] <- if cursor_x > (String.length text_str) - 1 
                                    then ' ' 
                                    else text_str.[cursor_x];
                let xc,yc = (tx+(string_width text_font 
                                      (String.sub text_str 0 cursor_x))),
                            (w.w_ipad_y + frame_off + ty) in

                Xlib.imageSubString s.s_display w.w_window gc  
                                        xc yc cursor 0 1;

                last_cursor <- Some (xc,yc,cursor.[0]);
            end
            else
            begin
                (*
                ** Multiline text is quite more complicated...
                *)
                let nlines = if text_rows = 0 then
                                List.length text_lines  (* auto height scale *)
                             else
                                text_rows in

                let th = text_font.font_height in
                let ty = ref (text_font.font_ascent + frame_off + 
                              w.w_ipad_y) in


                let cursor_pos = ref (0,0,' ') in
                let tot_len = ref 0 in
                let last_y = ref 0 in
                let last_x = ref 0 in
                let last_width = ref 0 in
                let n = ref 0 in

                List.iter (fun line ->
                  if text_rows = 0 ||
                     !n < text_rows then
                  begin
                    incr n;
                    let tlen = String.length line in
                    let tw   = string_width text_font line in

                    let tx = 
                        match text_align with
                        | Left -> frame_off + w.w_ipad_x 
                        | Center -> (width - tw - frame)/2
                        | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                        | _ -> warn_attr "VX_text" (Align text_align) self#name;
                               frame_off + w.w_ipad_x
                        in
                    last_x := tx;
                    last_y := !ty;
                    last_width := tw;

                    (*
                    ** Check cursor position.
                    *)
                    if display_cursor then
                    begin
                        if cursor_x >= !tot_len &&
                           cursor_x < (!tot_len + tlen) then
                        begin
                            let pos' = cursor_x - !tot_len in
                            cursor_pos := (tx + (string_width text_font
                                                 (String.sub line 0 pos')),
                                          !ty,
                                          line.[pos']);
                        end; 
                    end;

                    (
                      match base_line with
                      | Some tb ->
                      begin
                        ty := !ty + tb.tb_width;
                      end;
                      | None -> ();
                    );

                    tot_len := !tot_len + tlen;
                    ty := !ty + th + line_spacing;

                  end;
                  ) text_lines;

                let gc = GCCache.get_fg_bg_font s.s_gcs bg fg 
                                                text_font.font_id in
                let x',y',c = !cursor_pos in
                cursor.[0] <- if cursor_x > !tot_len - 1
                                        then ' ' 
                                        else c;
                                   

                let x'',y'' = 
                        if cursor_x > !tot_len - 1 then
                            (!last_x + !last_width), !last_y
                        else
                            x',y' in
                
                last_cursor <- Some (x'',y'', cursor.[0]);
                Xlib.imageSubString s.s_display w.w_window gc  
                                        x'' y'' cursor 0 1;
            end;
        end   

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0 =
        super#print ps wx0 wy0;
        let sz = szhints in
        let fc = w.w_foreground in
        let bc =  w.w_background in
        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in
        let cache = s.s_gcs in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in

        if not multi_line then
        begin
                (*
                ** Draw one single line.
                *)
                let th =  text_font.font_height in
                let tyoff = max 0 (height - th - 
                                   2 * w.w_ipad_y - frame)/2
                            in
                let ty = text_font.font_ascent + tyoff in
                let tlen = String.length text_str in
                let tw   = if not text_edit then
                                string_width_S text_font 
                                               text_font_symbol
                                               text_str
                            else
                                string_width text_font 
                                             text_str in

                let tx = 
                    match text_align with
                    | Left -> frame_off + w.w_ipad_x 
                    | Center -> (width - frame - tw)/2
                    | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                    | _ -> warn_attr "VX_text" (Align text_align) self#name; 
                           frame_off + w.w_ipad_x
                    in
                let ty = (w.w_ipad_y + frame_off + ty) in

                print_string_S ps wx0 wy0 
                             (i2f tx) (i2f ty) fc bc
                             text_font
                             text_font_symbol
                             text_str;
                
                (*
                ** Draw baseline if any.    
                *)
                (
                    match base_line with
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
                        let x0 = w.w_ipad_x in
                        let x1 = width - w.w_ipad_x in

                        let y' = ty + (text_font.font_height -
                                       text_font.font_ascent) + 
                                       (blw/2) + 1 in

                        VX_ps.polyLine ps wx0 wy0 (i2f blw) fc
                                           [i2f x0, i2f y';
                                            i2f x1, i2f y'];
                    end;
                    | None -> ();
                );
            end
            else 
            begin    
                (*
                ** Draw multiline text. Text was broken up into fragments
                ** and inserted in text_lines list.
                ** Intial text may contain newline characters or not.
                *)
                let nlines = if text_rows = 0 then
                                List.length text_lines  (* auto height scale *)
                             else
                                text_rows in

                let th =  text_font.font_height in
                let ty = ref (text_font.font_ascent + frame_off + 
                              w.w_ipad_y) in


                let cursor_pos = ref (0,0,' ') in
                let tot_len = ref 0 in
                let last_y = ref 0 in
                let last_x = ref 0 in
                let last_width = ref 0 in
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
                    let tw   = if not text_edit then
                                    string_width_S text_font 
                                                   text_font_symbol
                                                   line
                               else
                                    string_width text_font 
                                                 line in

                    let tx = 
                        match text_align with
                        | Left -> frame_off + w.w_ipad_x 
                        | Center -> (width - tw - frame)/2
                        | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                        | _ -> warn_attr "VX_text" (Align text_align) self#name;
                               frame_off + w.w_ipad_x
                        in
                    last_x := tx;
                    last_y := !ty;
                    last_width := tw;

                    print_string_S ps wx0 wy0 
                                 (i2f tx) (i2f !ty) 
                                 fc bc
                                 text_font
                                 text_font_symbol
                                 line;
                        
                    (*
                    ** Draw baseline if any.    
                    *)
                    (
                      match base_line with
                      | Some tb ->
                      begin
                        let blw = tb.tb_width in
                        let fc = if tb.tb_color <> noColor then
                                    tb.tb_color
                                 else
                                    fc in
                        (*
                        ** TODO: linetype..
                        *)

                        let x0 = frame_off + w.w_ipad_x in
                        let x1 = width - frame_off - w.w_ipad_x in
                        let y' = !ty - line_spacing + 
                                 text_font.font_ascent / 3 + 1  in

                        let y'  = !ty + (th -
                                         text_font.font_ascent) + 
                                        (blw/2) + 1 in

                        VX_ps.polyLine ps wx0 wy0 (i2f blw) fc 
                                       [i2f x0, i2f y';
                                        i2f x1, i2f y'];
                        ty := !ty + blw;
                      end;
                      | None -> ();
                    );


                    tot_len := !tot_len + tlen;
                    ty := !ty + th + line_spacing;

                  end;
                  ) text_lines;

            end;





    method print_info ind =
        let spaces n =
            let str = String.create n in
            for i = 0 to n-1 do str.[i] <- ' '; done; 
            str in

        let g = w.w_geometry in
        let sz = szhints in
        printf "%s%s: s.req=%d,%d s.min=%d,%d s.max=%d,%d s.pad=%d,%d s.exp=%b,%b\n" 
               (spaces ind)
               (self#name)
               sz.requested_width sz.requested_height
               sz.min_width sz.min_height
               sz.max_width sz.max_height
               sz.pad_x sz.pad_y 
               sz.expand_x sz.expand_y;
        printf "%s      w.pad=%d,%d g.win=%d,%d g.pos=%d,%d w.adjust=%b,%b" 
              (spaces ind)
               w.w_ipad_x w.w_ipad_y
               g.width g.height
               g.x g.y
               w.w_adjust_x w.w_adjust_y;
        print_newline ();
end

(*
** None editable simple text. Both single and multiline texts are supported.
*)

class t = orig
class text = orig

    
(*
** Editable text widget. Both single and multiline texts are supported. 
*)

class edit parent (text : string) attributes =
    object (self)
    inherit orig parent text ([Cursor (FontCursor XC.xc_hand1)]@attributes) 
            as super

    val mutable last_width = 0
    (*
    ** Optional action handler called any time the string was modified
    *)
    val mutable action = (fun _ -> ())

    initializer
        self#set_name "text_edit";
        text_edit <- true;
        display_cursor <- false;
        cursor_x <- 0;

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
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                let frame = frame_size w.w_frame in
                let frame_off = frame_offset w.w_frame in
                let g = w.w_geometry in
                let width = g.width in

                (*
                ** Set cursor
                *)
                if not multi_line then
                begin
                    let tw = string_width text_font 
                                          text_str in
                    let tx0 = match text_align with
                        | Left -> frame_off + w.w_ipad_x 
                        | Center -> (width - frame - tw)/2
                        | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                        | _ -> warn_attr "VX_text" (Align text_align) self#name;
                               frame_off + w.w_ipad_x
                        in
                    if (x >= tx0 && x <= (tx0+tw)) then
                    begin
                        (*
                        ** Find position in text string...
                        *)
                        protect ( for i = 1 to (String.length text_str) 
                        do
                            let str' = String.sub text_str 0 i in
                            let tw' = string_width text_font
                                                   str' in
                            if x <= (tx0 + tw') then
                            begin
                                cursor_x <- i-1;
                                raise Exit;
                            end;
                        done);
                        display_cursor <- true;
                        self#update_cursor;
                    end;
                end;
            );

          Key (XK.xk_Return, 0), (fun _ -> ());
          Key (XK.xk_BackSpace, 0), (fun _ ->
              if cursor_x > 0 then
              begin
                  cursor_x <- cursor_x - 1;
                    self#set_text (
                        let s = self#get_text in
                        let len = String.length s in
                        (String.sub s 0 cursor_x) ^ 
                        (String.sub s (cursor_x+1) (len - 1 - cursor_x))
                        );
                    self#update;
                    action self#get_text;
              end;
            );
          Key (XK.xk_Delete, 0), (fun _ ->
              if cursor_x < (String.length self#get_text) then
              begin
                    self#set_text (
                        let s = self#get_text in
                        let len = String.length s in
                        (String.sub s 0 cursor_x) ^ 
                        (String.sub s (cursor_x+1) (len - 1 - cursor_x))
                        );
                    self#update;
                    action self#get_text;
              end;
            );
          Key (XK.xk_Left, 0), (fun _ ->
                cursor_x <- max 0 (cursor_x - 1);
                self#update_cursor;  
            );
          Key (XK.xk_Right, 0), (fun _ ->
                cursor_x <- min (String.length self#get_text) 
                                  (cursor_x + 1);
                self#update_cursor;  
            );
          Key (XK.xk_Up, 0), (fun _ ->
              if multi_line then
              begin
                (*
                ** Multiline text. TODO
                *)
                ()
              end;
            );
          Key (XK.xk_Down, 0), (fun _ ->
              if multi_line then
              begin
                (*
                ** Multiline text. TODO
                *)
                ()
              end;
            );
          Key (anyKey, anyModifier), (fun _ ->
            (*
            ** Enough space for a new character?
            *)
            if !key_string <> "" &&
               (text_lastpad > text_font.font_width ||
                text_rows = 0 ||
                (multi_line && 
                 (List.length text_lines) < text_rows)) then
            begin
                self#set_text (
                    let s = self#get_text in
                    let len = String.length s in
                    (String.sub s 0 cursor_x) ^ !key_string ^ 
                    (String.sub s cursor_x (len - cursor_x))
                    );
                cursor_x <- cursor_x + (String.length !key_string);
                self#update;
                action self#get_text;
            end;
            );
          FocusIn ,(fun _ -> 
              display_cursor <- true; self#wait_refresh true 0 0 0 0);
          FocusOut ,(fun _ -> 
              display_cursor <- false; self#wait_refresh true 0 0 0 0);
          ]]);

    val mutable input_focus = false
    
    method size_allocate x y dx dy =
        let accepted = super#size_allocate x y dx dy in
        if dx <> last_width && not multi_line then
        begin
            (*
            ** Update single line text.
            *)
            text_lastpad <- self#text_width - 
                            (string_width text_font  text_str);
            last_width <- dx;
        end;
        accepted

    method set_action f =
        action <- f;

    method set_cursor n =
        cursor_x <- n;
        self#update_cursor

end



(*
** Very simple label text widget. For performance reasons outsourced from
** original class above, though orig class contains the label methods, too.
*)

class label parent (text : string) attributes =
    object (self)
    
    (*
    ** Default text font: generic and symbol of same size
    *)
    val mutable text_font = parent#font_make Times Roman 12 true; 
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;

    inherit VX_object.t parent attributes as super

    initializer 
        self#set_name "label";
        __(self#configure attributes)


    val mutable text_str = text

    (*
    ** Text alignment and font settings
    *)
    val mutable text_align = Left


    method configure attrs = 
        let remains = super#configure attrs in
        let font = font_desc text_font in
        let font_changed = ref false in
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
            | Align align       -> text_align <- align;
                                   changed := true;
            | _ -> warn_attr "VX_text.label" attr self#name;
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

        if !changed = true then
        begin
            self#wait_resize;
            self#wait_refresh true 0 0 0 0;            
        end;
        []

    (*
    ** Set (replace) and get text string.
    *) 
    method set_text s =
        text_str <- s;
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;


    (*
    ** Calculate the size of this text widget:
    **
    ** 1. from current string width and text_height
    ** 2. from fixed text_width with current font and text_height
    ** 3. from inherited object settings max_width and/or max_height
    *)

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            sz.comp_timestamp <- s.s_timestamp;
            sz.requested_width <- 
                min sz.max_width
                    (max ((string_width_S text_font 
                                          text_font_symbol 
                                          text_str) + 
                          frame + 2 * w.w_ipad_x) 
                         sz.min_width);
                
            sz.requested_height <- 
                    min sz.max_height
                        (max (text_font.font_height +
                              frame + 2 * w.w_ipad_y) 
                             sz.min_height);
            sz                    
        end;

    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            super#refresh;
            let sz = szhints in

            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let font = text_font.font_id in
            let gcs = s.s_gcs in
            let gc = GCCache.get_fg_bg_font gcs fg bg font in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let th    =  text_font.font_height in
            let tyoff = max 0 (height - th - 
                               2 * w.w_ipad_y - frame)/2
                            in
            let ty = text_font.font_ascent + tyoff in
            let tlen = String.length text_str in
            let tw = string_width_S text_font 
                                    text_font_symbol
                                    text_str in

            let x = 
                    match text_align with
                    | Left -> frame_off + w.w_ipad_x 
                    | Center -> (width - frame - tw)/2
                    | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                    | _ -> warn_attr "VX_text" (Align text_align) self#name;
                           frame_off + w.w_ipad_x
                    in
            (*
            ** Draw text to window
            *)
            X.clearArea s.s_display w.w_window 
                                        frame_off frame_off
                                        (width - frame)
                                        (height - frame) false;
            let tx = x in
            let ty = w.w_ipad_y + ty in
            draw_string_S s.s_display w.w_window gcs
                                       tx ty fg bg
                                       text_font
                                       text_font_symbol
                                       text_str;

        end; 


    method print_info ind =
        let spaces n =
            let str = String.create n in
            for i = 0 to n-1 do str.[i] <- ' '; done; 
            str in

        let g = w.w_geometry in
        let sz = szhints in
        printf "%s%s: s.req=%d,%d s.min=%d,%d s.max=%d,%d s.pad=%d,%d s.exp=%b,%b\n" 
               (spaces ind)
               (self#name)
               sz.requested_width sz.requested_height
               sz.min_width sz.min_height
               sz.max_width sz.max_height
               sz.pad_x sz.pad_y 
               sz.expand_x sz.expand_y;
        printf "%s      w.pad=%d,%d g.win=%d,%d g.pos=%d,%d w.adjust=%b,%b" 
              (spaces ind)
               w.w_ipad_x w.w_ipad_y
               g.width g.height
               g.x g.y
               w.w_adjust_x w.w_adjust_y;
        print_newline ();

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0 =
        super#print ps wx0 wy0;
        let sz = szhints in
        let fc = w.w_foreground in
        let bc =  w.w_background in
        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in
        let cache = s.s_gcs in

        let g = w.w_geometry in
        let width = g.width in
        let height = g.height in
        (*
        ** Draw one single line.
        *)
        let th =  text_font.font_height in
        let tyoff = max 0 (height - th - 
                           2 * w.w_ipad_y - frame)/2
            in
        let ty = text_font.font_ascent + tyoff in
        let tlen = String.length text_str in
        let tw   = string_width_S text_font 
                                  text_font_symbol
                                  text_str
            in

        let tx = 
                    match text_align with
                    | Left -> frame_off + w.w_ipad_x 
                    | Center -> (width - frame - tw)/2
                    | Right -> width - tw - 2 - frame_off - w.w_ipad_x
                    | _ -> warn_attr "VX_text" (Align text_align) self#name; 
                           frame_off + w.w_ipad_x
                    in
        let ty = (w.w_ipad_y + frame_off + ty) in

        print_string_S ps wx0 wy0 
                             (i2f tx) (i2f ty) fc bc
                             text_font
                             text_font_symbol
                             text_str;
                
        
end
