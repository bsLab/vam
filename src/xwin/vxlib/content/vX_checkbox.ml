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
**    $CREATED:     1.6.2005
**    $VERSION:     1.13
**
**    $INFO:
**
**  Check box selector with buttons.
**
**  Horizontal:     B1 Label1 B2 Label2 ...
**  Vertical:       B1 Label1
**                  B2 Label2 
**                  ...
**             
**  Each time the button is pressed, the selected checkbox entry 
**  changes his state. 
**  Mutual checkboxes (Mutual attribute) allow only 1 entry activated with 
**  state S_1. All other entries have state S_0.
**
**  The space between checkbox entries is specified with IpadX (H) and 
**  IpadY (V) attributes of the widget, or automatically calculated if
**  AdjustX (H) or AdjustY (V) is specified. 
**
**  All buttons and labels can be configured individually using the
**  Rows attribute: Each row specifies one button+label pair. 
**  The space between the button and the label can be specified there
**  with the IpadX (H/V) attribute.
**
**  If AdjustX & ExpandX is specified, the total width of
**  all button+label pairs are expanded to the offered space by the
**  parent widget, in contrast to the behaviour of boxes! Fixed
**  padding IpadX and IpadY is required, too, and is applied only
**  (IpadX) on the left of the most left, and on the right of the most
**  right entry field.
** 
**    $ENDOFINFO
**
*)


open Xtypes
open VX_types
open VX_common
open VX_text
open VX_button
open Printf
open GCCache


type kind =
    | Horizontal
    | Vertical

type box_desc = {
    mutable cb_label : string;
    mutable cb_active : bool;           (* button pressed ?     *)
    mutable cb_cur_state  : bool;       (* checkbox entry state *)
    mutable cb_but : symbol_type list;
    mutable cb_frame : frame;
    mutable cb_label_width : int;   
    mutable cb_but_size : int;      (* size = width = height *)
}

(*
** Button and labels are configured either with global attributes
** or locally with the Cols array: Each row containes two column entries:
** The button and the label, nevertheless the orintation is!
*)

class orig kind parent labels attributes =
    object (self)
    val mutable text_font = parent#font_make Helvetica Roman 12 true;
    val mutable text_font_symbol = parent#font_make Symbol Roman 12 true;


    inherit VX_object.t parent attributes as super

    (*
    ** Internal checkbox info array with informations about the
    ** state of each checkbox entry.
    *)

    val mutable mutual = false
    val mutable check_info = [||]
    val mutable check_fun = (fun _ -> ())

    (*
    ** Padding space between button and label text. Padding between
    ** checkbox entries is specified with IpadX and IpadY.
    *)
    val mutable pad_x = 5 

    
    initializer
        self#set_name "checkbox";
        self#init;
        __(self#configure [Cursor (FontCursor XC.xc_hand1)]@attributes);
        (*   
        ** User interaction: mouse & keyboard
        *)
        __(self#configure [Bindings [
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                let i = ref 0 in
                protect (Array.iter (fun cb ->
                    let within = within_bbox cb.cb_frame.f_bbox x y in
                    if within & not cb.cb_active then
                    begin
                        cb.cb_active <- true;
                        self#update_but !i;
                        raise Exit;
                    end;
                    incr i;
                    ) check_info);
            );
          ButtonReleased,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                let i = ref 0 in
                protect (Array.iter (fun cb ->
                    let within = within_bbox cb.cb_frame.f_bbox x y in
                    if within then
                    begin
                        cb.cb_active <- false; 
                        if not mutual then
                        begin
                            cb.cb_cur_state <- not cb.cb_cur_state;
                            self#update_but !i;
                        end
                        else
                        begin
                            (*
                            ** Only one entry can be selected!
                            *)
                            let i' = ref 0 in
                            Array.iter (fun cb' ->
                                    if !i = !i' then
                                        cb'.cb_cur_state <- true
                                    else
                                        cb'.cb_cur_state <- false;
                                    self#update_but !i';
                                    incr i';
                                ) check_info;                                                                
                        end;
                        check_fun !i;
                        raise Exit;
                    end;
                    incr i;
                    ) check_info);
            )
            ]]);

    val mutable inited = false
    method init =
      if inited = false then
      begin
        (*
        ** Create checkbox entry array.
        ** Will be later configured...
        *)
        let nentr = Array.length labels in 
        let i = ref 0 in
        check_info <- Array.init nentr (fun _ ->
                        let width = (string_width_S text_font
                                                    text_font_symbol
                                                    labels.(!i)) in
                        (*
                        ** Default button size
                        *)
                        let size = 17 in
                        let cb = {
                                cb_label=labels.(!i);
                                cb_active=false;
                                cb_cur_state=false;
                                cb_but = [S_BLANK;S_R 2];
                                cb_frame= {(default_frame 1) with
                                           f_background = w.w_background;
                                           f_fillground = 
                                            parent#color_make "white" true;
                                           f_type = ShadowRaised;
                                            };
                                cb_label_width= width;
                                cb_but_size= size; 
                            } in
                        incr i;
                        cb
                );
        inited <- true;
      end

    method configure attr =
        let remains = super#configure attr in
        if (Array.length check_info = 0) then
            self#init;

        (*
        ** Get global attributes
        *)
        let font_changed = ref false in
        let font = font_desc text_font in
        let row_attr = ref [||] in

        List.iter (fun a ->
            match a with
            | ActionIU f -> check_fun <- f;
            | Text_font  kind  -> font_changed := true;
                                  font.text_font <- kind;
            | Text_style style -> font_changed := true;
                                  font.text_style <- style;
            | Text_size  size  -> font_changed := true;
                                  font.text_size <- size;
            | Mutual m -> mutual <- m;   
            | Rows r -> row_attr := r;
            | _ -> ();
            ) remains;

        
        let n = Array.length check_info in
        if !row_attr <> [||] &&
           n <> (Array.length !row_attr) then
            vx_error "VX_checkbox: invalid Rows attribute row length";

        if !font_changed then 
        begin
            text_font <- parent#font_make font.text_font
                                          font.text_style 
                                          font.text_size true;
            text_font_symbol <- parent#font_make Symbol
                                          font.text_style 
                                          font.text_size true;
        end;


        (*
        ** Configure buttons and labels
        *)
        if !row_attr <> [||] then
        for i = 0 to n-1
        do
            let ch = check_info.(i) in

            let syms = ref [] in
            List.iter (fun a ->
                match a with
                | Size s -> ch.cb_but_size <- s;
                | Active b -> ch.cb_cur_state <- b;
                | Sym s -> syms := !syms @ [s];
                | IpadX x -> pad_x <- x;
                | Frame a -> ch.cb_frame.f_type <- a;
                | _ -> warn_attr "VX_checkbox" a self#name;
                ) !row_attr.(i);
            if !syms <> [] && (List.length !syms) <> 2 then
                vx_error "VX_checkbox: invalid Sym attributes"
            else if !syms <> [] then
                ch.cb_but <- !syms;

        done; 
        [] (* leaf widget *)

    (*
    ** Calculate required width and height of this widget.
    *)

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            sz.comp_timestamp <- s.s_timestamp;
            match kind with
            | Horizontal ->
                let width = ref (frame + w.w_ipad_x) in
                let height = ref 0 in
                let nentr = Array.length check_info in

                (*
                ** The minimal required sizes
                *)
                Array.iter (fun cb ->
                    height := max !height (max (cb.cb_but_size + 
                                                2 * w.w_ipad_y +
                                                frame)
                                               (text_font.font_height + 
                                                2 * w.w_ipad_y +
                                                frame));
                    width := !width + cb.cb_label_width + 
                             cb.cb_but_size + 
                             pad_x + 
                             w.w_ipad_x;
                  ) check_info;
                width := !width + w.w_ipad_x;
                (*
                ** Auto padding adjustment?
                *)
                if w.w_adjust_x then
                begin
                    width := max !width sz.min_width;
                end;                
                sz.requested_width <- min sz.max_width !width;
                sz.requested_height <- min sz.max_height !height;
                sz

            | Vertical ->
                let width = ref 0 in
                let height = ref (frame + w.w_ipad_y) in

                Array.iter (fun cb ->
                    height := !height + (max cb.cb_but_size
                                             text_font.font_height) + 
                                        w.w_ipad_y;
                    width := max !width (frame + cb.cb_label_width +
                                         cb.cb_but_size + 2 * w.w_ipad_x +
                                         pad_x);  
                  ) check_info;
                (*
                ** Auto padding adjustment?
                *)
                if w.w_adjust_y then
                begin
                    height := max !height sz.min_height;
                end;                

                sz.requested_width <- min sz.max_width !width;
                sz.requested_height <- min sz.max_height !height;
                sz

        end

    (*
    ** Update the one specified button
    *)
    method update_but i =
        let sz = szhints in
        let fg = w.w_foreground.c_pixel in
        let bg =  w.w_background.c_pixel in
        let gcs = s.s_gcs in

        let cb = check_info.(i) in
        let x,y,wd,ht = bbox_to_xywh cb.cb_frame.f_bbox in
        (*
        ** Clear button area
        *)
        X.clearArea s.s_display w.w_window x y wd ht false;

        cb.cb_frame.f_type <- (
            match cb.cb_frame.f_type with
            | ShadowSunken
            | ShadowRaised ->
                if cb.cb_active then
                    ShadowSunken
                else
                    ShadowRaised;
            | ReliefSunken
            | ReliefRaised ->
                if cb.cb_active then
                    ReliefSunken
                else
                    ReliefRaised;
            | _ -> cb.cb_frame.f_type);

        drawFrame s.s_display w s.s_gcs cb.cb_frame true;

        let frame' = frame_size cb.cb_frame in
        let frame_off' = frame_offset cb.cb_frame in

        let bbox = cb.cb_frame.f_bbox in
        let x1' = bbox.x1 + frame_off' in
        let x2' = bbox.x2 + frame_off' - frame' in
        let y1' = bbox.y1 + frame_off' in
        let y2' = bbox.y2 + frame_off' - frame' in

        let symbol = {
                        sym_type = if cb.cb_cur_state then 
                                        (List.nth cb.cb_but 1)
                                   else
                                        (List.nth cb.cb_but 0);
                        sym_bbox = {x1=x1';y1=y1';
                                    x2=x2';y2=y2';};
                        sym_col = noColor;
                        sym_width = 1;
                        } in

        drawSymbol s.s_display w gcs symbol;

    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp &&
           not (w.w_window == noWindow) then
        begin
            super#refresh;
            let sz = szhints in
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let gcs = s.s_gcs in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            match kind with
            | Horizontal ->
                let g = w.w_geometry in
                let x = ref (frame_off + w.w_ipad_x) in
                let y0 = frame_off + w.w_ipad_y in
                let nentr = Array.length check_info in
                let offered = g.width - frame in
                (*
                ** In the case of auto padding adjustment here
                ** all entry fields get the same size, not the same
                ** padding. Padding IpadX must also be specified. This
                ** space inserted left from the most left, and right from
                ** the most right entry.
                *)
                let eq_width = (offered - 2 * w.w_ipad_x)/nentr in
                
                Array.iter (fun cb ->
                    (*
                    ** First the select button
                    *)
                    let bbox = cb.cb_frame.f_bbox in
                    bbox.x1 <- !x;
                    bbox.x2 <- !x + cb.cb_but_size - 1;
                    bbox.y1 <- y0;
                    bbox.y2 <- y0 + cb.cb_but_size - 1;


                    cb.cb_frame.f_type <- (
                        match cb.cb_frame.f_type with
                        | ShadowSunken
                        | ShadowRaised ->
                            if cb.cb_active then
                                ShadowSunken
                            else
                                ShadowRaised;
                        | ReliefSunken
                        | ReliefRaised ->
                            if cb.cb_active then
                                ReliefSunken
                            else
                                ReliefRaised;
                        | _ -> cb.cb_frame.f_type);
                    drawFrame s.s_display w s.s_gcs 
                               cb.cb_frame true;

                    let frame' = frame_size cb.cb_frame in
                    let frame_off' = frame_offset cb.cb_frame in

                    let x1' = bbox.x1 + frame_off' in
                    let x2' = bbox.x2 + frame_off' - frame'  in
                    let y1' = bbox.y1 + frame_off' in
                    let y2' = bbox.y2 + frame_off' - frame'  in

                    let symbol = {
                        sym_type = if cb.cb_cur_state then 
                                        (List.nth cb.cb_but 1)
                                   else
                                        (List.nth cb.cb_but 0);
                        sym_bbox = {x1=x1';y1=y1';
                                    x2=x2';y2=y2'}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in
                    drawSymbol s.s_display w gcs symbol;
                    
                    (*
                    ** Left aligned text
                    *)
                    let tx = !x + cb.cb_but_size + pad_x in
                    let height =  text_font.font_height in
                    let offset = max 0 (w.w_geometry.height - height -
                                        2 * w.w_ipad_y - frame)/2
                             in
                    let y = text_font.font_ascent + offset in
                    let ty = (w.w_ipad_y + frame_off + y) in
                    draw_string_S s.s_display w.w_window gcs
                                  tx ty fg bg
                                  text_font
                                  text_font_symbol
                                  cb.cb_label;

                    let cur_width = cb.cb_label_width + cb.cb_but_size +
                                    pad_x + w.w_ipad_x in
                    
                    x := !x + (if w.w_adjust_x then
                                eq_width
                               else
                                cur_width);
                  ) check_info;

            | Vertical ->
                let g = w.w_geometry in
                let x0 = frame_off + w.w_ipad_x in
                let y = ref (frame_off + w.w_ipad_y) in
                let nentr = Array.length check_info in
                let offered = g.height - frame in
                (*
                ** In the case of auto padding adjustment here
                ** all entry fields get the same height, not the same
                ** padding. Padding IpadY must also be specified. This
                ** space inserted top from the most top, and bottom from
                ** the most bottom entry.
                *)
                let eq_height = (offered - 2 * w.w_ipad_y)/nentr in
                
                Array.iter (fun cb ->
                    (*
                    ** First the select button
                    *)
                    let frame' = frame_size cb.cb_frame in
                    let frame_off' = frame_offset cb.cb_frame in
                    let bbox = cb.cb_frame.f_bbox in
                    bbox.x1 <- x0;
                    bbox.x2 <- x0 + cb.cb_but_size - 1;
                    bbox.y1 <- !y;
                    bbox.y2 <- !y + cb.cb_but_size - 1;
                    drawFrame s.s_display w s.s_gcs 
                              cb.cb_frame true;

                    let x1' = bbox.x1 + frame_off' in
                    let x2' = bbox.x2 + frame_off' - frame' in
                    let y1' = bbox.y1 + frame_off' in
                    let y2' = bbox.y2 + frame_off' - frame'  in
                    let symbol = {
                        sym_type = if cb.cb_cur_state then 
                                        (List.nth cb.cb_but 1)
                                   else
                                        (List.nth cb.cb_but 0);
                        sym_bbox = {x1=x1';y1=y1';
                                    x2=x2';y2=y2'}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in
                    drawSymbol s.s_display w gcs symbol;
                    
                    (*
                    ** Left aligned text
                    *)
                    let tx = x0 + cb.cb_but_size + pad_x in
                    let height =  text_font.font_height in
                    let offset = ((max height cb.cb_but_size) - height) / 2 in
                    let ty = !y + text_font.font_ascent + offset in
                    draw_string_S s.s_display w.w_window gcs
                                  tx ty fg bg
                                  text_font
                                  text_font_symbol
                                  cb.cb_label;

                    
                    let cur_height = (max height cb.cb_but_size) +
                                     w.w_ipad_y in
                    y := !y + (if w.w_adjust_y then
                                eq_height
                               else
                                cur_height);
                  ) check_info;
        end              

    (*
    ** Is a checkbox entry selected ?
    *)
    method selected n = 
        if n >= 0 && n < (Array.length check_info) then
            check_info.(n).cb_cur_state
        else
        begin
            vx_error "VX_checkbox: activated: index not valid";
            false
        end;

    (*
    ** Set action handler (called when a selection was done). The action
    ** handler is called with the selected checkbox index.
    *)
    method set_action f = check_fun <- f;

    method print (ps : ps) (wx0 : int) (wy0 : int) =
        Db.Pr.sdd 0 "VX_checkbox.print x0 y0" wx0 wy0;
        super#print ps wx0 wy0;

        let sz = szhints in
        let fc = w.w_foreground in
        let bc =  w.w_background in

        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        match kind with
        | Horizontal ->
                let g = w.w_geometry in
                let x = ref (frame_off + w.w_ipad_x) in
                let y0 = frame_off + w.w_ipad_y in
                let nentr = Array.length check_info in
                let offered = g.width - frame in
                (*
                ** In the case of auto padding adjustment here
                ** all entry fields get the same size, not the same
                ** padding. Padding IpadX must also be specified. This
                ** space inserted left from the most left, and right from
                ** the most right entry.
                *)
                let eq_width = (offered - 2 * w.w_ipad_x)/nentr in
                
                Array.iter (fun cb ->
                    (*
                    ** First the select button
                    *)
                    let bbox = cb.cb_frame.f_bbox in
                    bbox.x1 <- !x;
                    bbox.x2 <- !x + cb.cb_but_size - 1;
                    bbox.y1 <- y0;
                    bbox.y2 <- y0 + cb.cb_but_size - 1;


                    cb.cb_frame.f_type <- (
                        match cb.cb_frame.f_type with
                        | ShadowSunken
                        | ShadowRaised ->
                            if cb.cb_active then
                                ShadowSunken
                            else
                                ShadowRaised;
                        | ReliefSunken
                        | ReliefRaised ->
                            if cb.cb_active then
                                ReliefSunken
                            else
                                ReliefRaised;
                        | _ -> cb.cb_frame.f_type);

                    printFrame ps w wx0 wy0  cb.cb_frame true;

                    let frame' = frame_size cb.cb_frame in
                    let frame_off' = frame_offset cb.cb_frame in

                    let x1' = bbox.x1 + frame_off' in
                    let x2' = bbox.x2 + frame_off' - frame'  in
                    let y1' = bbox.y1 + frame_off' in
                    let y2' = bbox.y2 + frame_off' - frame'  in

                    let symbol = {
                        sym_type = if cb.cb_cur_state then 
                                        (List.nth cb.cb_but 1)
                                   else
                                        (List.nth cb.cb_but 0);
                        sym_bbox = {x1=x1';y1=y1';
                                    x2=x2';y2=y2'}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in

                    printSymbol ps s.s_display w wx0 wy0 symbol; 
                    
                    (*
                    ** Left aligned text
                    *)
                    let tx = !x + cb.cb_but_size + pad_x in
                    let height =  text_font.font_height in
                    let offset = max 0 (w.w_geometry.height - height -
                                        2 * w.w_ipad_y - frame)/2
                             in
                    let y = text_font.font_ascent + offset in
                    let ty = (w.w_ipad_y + frame_off + y) in
                    print_string_S ps wx0 wy0
                                  (i2f tx) (i2f ty) fc bc
                                  text_font
                                  text_font_symbol
                                  cb.cb_label;

                    let cur_width = cb.cb_label_width + cb.cb_but_size +
                                    pad_x + w.w_ipad_x in
                    
                    x := !x + (if w.w_adjust_x then
                                eq_width
                               else
                                cur_width);
                  ) check_info;

        | Vertical ->
                let g = w.w_geometry in
                let x0 = frame_off + w.w_ipad_x in
                let y = ref (frame_off + w.w_ipad_y) in
                let nentr = Array.length check_info in
                let offered = g.height - frame in
                (*
                ** In the case of auto padding adjustment here
                ** all entry fields get the same height, not the same
                ** padding. Padding IpadY must also be specified. This
                ** space inserted top from the most top, and bottom from
                ** the most bottom entry.
                *)
                let eq_height = (offered - 2 * w.w_ipad_y)/nentr in
                
                Array.iter (fun cb ->
                    (*
                    ** First the select button
                    *)
                    let frame' = frame_size cb.cb_frame in
                    let frame_off' = frame_offset cb.cb_frame in
                    let bbox = cb.cb_frame.f_bbox in
                    bbox.x1 <- x0;
                    bbox.x2 <- x0 + cb.cb_but_size - 1;
                    bbox.y1 <- !y;
                    bbox.y2 <- !y + cb.cb_but_size - 1;
                    printFrame ps w wx0 wy0  
                              cb.cb_frame true;

                    let x1' = bbox.x1 + frame_off' in
                    let x2' = bbox.x2 - frame' - 1 in
                    let y1' = bbox.y1 + frame_off' in
                    let y2' = bbox.y2 - frame' - 1 in
                    let symbol = {
                        sym_type = if cb.cb_cur_state then 
                                        (List.nth cb.cb_but 1)
                                   else
                                        (List.nth cb.cb_but 0);
                        sym_bbox = {x1=x1';y1=y1';
                                    x2=x2';y2=y2'}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in
                    printSymbol ps s.s_display w wx0 wy0 symbol;
                    
                    (*
                    ** Left aligned text
                    *)
                    let tx = x0 + cb.cb_but_size + pad_x in
                    let height =  text_font.font_height in
                    let offset = ((max height cb.cb_but_size) - height) / 2 in
                    let ty = !y + text_font.font_ascent + offset in
                    print_string_S ps wx0 wy0
                                  (i2f tx) (i2f ty) fc bc
                                  text_font
                                  text_font_symbol
                                  cb.cb_label;

                    
                    let cur_height = (max height cb.cb_but_size) +
                                     w.w_ipad_y in
                    y := !y + (if w.w_adjust_y then
                                eq_height
                               else
                                cur_height);
                  ) check_info;

end

class h parent labels attributes =
    object
    inherit orig Horizontal parent labels 
                                   attributes
    method name = "horizontal_checkbox"
end

class v parent labels attributes =
    object
    inherit orig Vertical parent labels 
                                 attributes
    method name = "vertical_checkbox"
end


