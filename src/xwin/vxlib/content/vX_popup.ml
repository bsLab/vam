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
**    $INITIAL:     (C) 2005-2006 BSSLAB
**    $CREATED:     7.6.2005
**    $VERSION:     1.24
**
**    $INFO:
**
**  Popup button with label. 
**
**  class t (generic):
**
**  This button controls a child
**  widget. If the button is activated (up), the child
**  is displayed in a new windown beneath the button. This window
**  can be closed simply pressing the button again (down).
**
**  The button is placed right aligned, the text label left aligned.
**  The box gets a default border. 
**
**  class select :
**
**  Same as above, but with predefined child widget and an additional
**  choice label on the right side of the button. The child widget is
**  a simple choice (text) list: only one item can be selected. The
**  choice is displayed.  
**
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
open Myenv
open VX_file
open Amoeba
open Stderr
open VX_tree
open VX_view
open VX_slider

type kind =
    | Sticky
    | Window

(*
** Popup window button with label text.
*)
class orig kind root parent (text : string) attributes =
    object (self)
    val root = root

    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable bold_font = parent#font_make Helvetica Bold 12 true; 
    val mutable normal_font = parent#font_make Helvetica Roman 12 true;
    val mutable symbol_font = parent#font_make Symbol Bold 12 true;



    inherit VX_object.t parent ([IpadX 5;
                                 Border []]@attributes) as super

    (*
    ** Optional event handler
    *)
    val mutable action_up = (fun () -> ())
    val mutable action_down = (fun () -> ())

    (*
    ** Button pressed ?
    *)
    val mutable but_active = false

    (*
    ** Window opened?
    *)
    val mutable win_active = false

    (*
    ** The button
    *)
    val but_box = {(default_frame 1) with
                      f_shape=S_Rect;
                      f_type=ShadowRaised} 


    (*
    ** Special select popup ?
    *)
    val mutable is_select = false
    val mutable select_str = ""
    val mutable max_select_width = 0

    initializer 
        self#set_name "popup_button";
        __(self#configure [Cursor (FontCursor XC.xc_hand2)]@attributes);
        __(self#configure [Bindings [
          (*
          ** Keyboard and mouse user interaction.
          *)
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                let within = within_bbox but_box.f_bbox x y in
                if within && not but_active then
                begin
                    but_active <- true;
                    self#update;
                end;  
            );
          ButtonReleased,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in
                let g = w.w_geometry in

                let within = within_bbox but_box.f_bbox x y in
                if within then
                begin
                    let x0,y0 = self#root_coordinates in 
                    but_active <- false;
                    win_active <- not win_active;
                    if win_active then
                    begin
                        self#popup  (x0+g.width) y0; 
                        action_up ();
                    end
                    else
                    begin
                        self#popdown;
                        action_down ();
                    end;
                    self#update;
                end;
            );
        ]]);


    val mutable text_str = text

    (*
    ** Button size
    *)
    val mutable but_size = 19


    method set_state b = but_active <- b
    method get_state   = but_active 
    method set_action f = 
        let old = action_down in
        action_down <- (fun _ -> old (); f());

    (*
    ** Set text attributes
    *)
    method configure attrs = 
        let remains = super#configure attrs in

        let font_changed = ref false in
        let font = font_desc bold_font in
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
            | But attr ->
                List.iter (fun a ->
                    match a with
                    | Shape s -> but_box.f_shape <- s;
                    | Frame a -> but_box.f_type <- a;
                    | Color c -> but_box.f_fillground <- 
                                    parent#color_make c true;
                    | Background c -> but_box.f_fillground <- 
                                    parent#color_make c true;
                    | _ -> warn_attr "VX_popup: But attribute list" a
                                     self#name;
                    ) attr;
            | ActionUU f -> self#set_action f;
            | _ -> warn_attr "VX_popup" attr self#name;
            ) remains;
        

        if !font_changed then 
        begin
            bold_font <- parent#font_make font.text_font
                                          font.text_style 
                                          font.text_size true;
            normal_font <- parent#font_make font.text_font
                                          Roman 
                                          font.text_size true;
            symbol_font <- parent#font_make Symbol
                                          font.text_style 
                                          font.text_size true;
        end;
        (*
        ** Configure color of button
        *)
        (
            match  but_box.f_type with
            | ShadowRaised
            | ShadowSunken ->
                let back = w.w_background in
                but_box.f_background <- back;
                but_box.f_fillground <- parent#color_make "white" true;
            | ReliefRaised
            | ReliefSunken ->
                let back = w.w_background in
                but_box.f_foreground <- parent#getShadow back;
                but_box.f_auxiliary <- parent#getHilite back;
                if but_box.f_fillground = noColor then
                    but_box.f_fillground <- back;
            | _ -> but_box.f_background <- w.w_background;
                   but_box.f_fillground <- parent#color_make "white" true; 
        );
        [] (* leaf widget *)

    (*
    ** Get the size of this button widget. Text string area + 
    ** button and padding. 
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
                            (max ((string_width_S bold_font 
                                                  symbol_font
                                                  text_str) +
                                  frame + 
                                  but_size + 
                                  3 * w.w_ipad_x + (
                                    if is_select then
                                        (max_select_width + w.w_ipad_x)
                                    else
                                        0  
                                  )) 
                                 sz.min_width);
            sz.requested_height <- 
                    min sz.max_height
                        (max (max (bold_font.font_height + 
                                   frame + 2 * w.w_ipad_y) 
                                  (but_size + 
                                   frame + 2 * w.w_ipad_y))
                             sz.min_height);
            sz
        end

    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
            super#refresh;
            let sz = szhints in
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in
            let gcs = s.s_gcs in
            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let x0,y0 = frame_off + w.w_ipad_x,
                        frame_off + w.w_ipad_y in
            (*
            ** First the Label
            *)

            let th =  bold_font.font_height in
            let tyoff = max 0 (height - th - 
                               2 * w.w_ipad_y - frame)/2
                               in

            let ty = y0 + bold_font.font_ascent + tyoff in
            let tw = string_width_S bold_font 
                                    symbol_font
                                    text_str in



            let tx = x0 in

            (*
            ** draw text label
            *)
            draw_string_S s.s_display w.w_window gcs
                          tx ty fg bg
                          bold_font
                          symbol_font
                          text_str;

            (*
            ** Now the button
            *)
            let bx0,by0 = tx + tw + w.w_ipad_x,
                          y0 in 

            but_box.f_bbox <- bbox_of_xywh bx0 by0 but_size but_size; 

            but_box.f_type <- (
                match but_box.f_type with
                | ShadowSunken
                | ShadowRaised ->
                    if but_active then
                        ShadowSunken
                    else
                        ShadowRaised;
                | ReliefSunken
                | ReliefRaised ->
                    if but_active then
                        ReliefSunken
                    else
                        ReliefRaised;
                | _ -> but_box.f_type);

            drawFrame s.s_display w s.s_gcs but_box true; 

            let bframe = frame_size but_box in
            let bframe_off = frame_offset but_box in

            let symbol = {
                        sym_type = if win_active then 
                                        S_UP
                                   else
                                        S_DOWN;
                        sym_bbox = {x1=bx0+bframe_off;
                                    y1=by0+bframe_off;
                                    x2=bx0+bframe_off+but_size-1-bframe;
                                    y2=by0+bframe_off+but_size-1-bframe}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in
            drawSymbol s.s_display w gcs symbol;

            if is_select then
            begin
                (*
                ** Draw optional selected string.
                *)

                let th =  normal_font.font_height in
                let tyoff = max 0 (height - th - 
                                    2 * w.w_ipad_y - frame)/2
                                in


                let tx = bx0 + but_size + w.w_ipad_x in
                let ty = y0 + bold_font.font_ascent + tyoff in

                (*
                ** If the string doesn't fit into the window,
                ** show an abbriviated version
                *)
                let offered = width - tx - w.w_ipad_x in
                let tw = string_width_S normal_font
                                        symbol_font
                                        select_str in
                if tw <= offered then
                    draw_string_S s.s_display w.w_window gcs
                                           tx ty fg bg
                                           normal_font
                                           symbol_font
                                           select_str
                else
                begin
                    let len = String.length select_str in
                    (*
                    ** String may contain embedded symbols!
                    *)
                    let in_sym = ref false in
                    protect (
                        for i = 0 to len-1
                        do
                            if select_str.[i] = '\\' then
                                in_sym := true
                            else if !in_sym && select_str.[i] = ' ' then
                                in_sym := false
                            else if not !in_sym then
                            begin
                                let str' =  String.sub select_str
                                                       i (len-i) in
                                let tw' = string_width_S normal_font
                                                         symbol_font
                                                         str' in

                                if tw' <= offered then
                                begin
                                    draw_string_S s.s_display w.w_window gcs
                                                  tx ty fg bg
                                                  normal_font
                                                  symbol_font
                                                  str';
                                    raise Exit;
                                end;
                            end;
                        done
                    );
                end;
            end;

        end;

    method update =
        super#update;
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;

    val mutable popup_wob = None
    val mutable realization1 = None
    val mutable realization2 = None

    method container_set wob =
        match popup_wob with
        | None -> popup_wob <- Some wob;
        | Some wob' -> ();

    method container_add (wob:VX_types.contained) = 
        wob#set_parent self#container;
        self#container_set wob;

    method popup x y =
        match kind with
        | Sticky ->
        begin
          let top = 
            match realization1 with
            | Some (top : VX_top.t) -> 
                __(top#configure [Background "white"; Position (x,y)]);
                top
            | None -> 
                let top = new VX_top.t root (Some (x,y))  [] in 
                realization1 <- Some top;
                let vbar = new VX_box.v top#container 
                        [
                            Background "white";
                            Border [];
                            IpadX 5; IpadY 5;
                            Cursor (FontCursor XC.xc_hand1);
                        ] in
                top#container_add vbar#contained;
                (
                    match popup_wob with
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
            in
          top#show;
          top#focus;
        end;
        | Window ->
        begin
          let top = 
            match realization2 with
            | Some (top : VX_wmtop.t) -> 
                __(top#configure [Background "white";]);
                top
            | None -> 
                let top = new VX_wmtop.t root [] in 
                realization2 <- Some top;
                let vbar = new VX_box.v top#container 
                        [
                            Background "white";
                            Border [];
                            IpadX 5; IpadY 5;
                            Cursor (FontCursor XC.xc_hand1);
                        ] in
                top#container_add vbar#contained;
                (
                    match popup_wob with
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
            in
          top#show;
(*          top#focus; *)
        end;

    method popdown =
      match kind with
      | Sticky ->
      begin
        match realization1 with
        | Some (top : VX_top.t) -> top#hide;
                                   win_active <- false;
        | None -> ()
      end;
      | Window ->
      begin
        match realization2 with
        | Some (top : VX_wmtop.t) -> top#hide;
                                     win_active <- false;
        | None -> ()
      end;

    method print (ps : ps) (wx0 : int) (wy0 : int) =
            super#print ps wx0 wy0;
            let sz = szhints in
            let fc = w.w_foreground in
            let bc =  w.w_background in



            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let x0,y0 = frame_off + w.w_ipad_x,
                        frame_off + w.w_ipad_y in
            (*
            ** First the Label
            *)

            let th =  bold_font.font_height in
            let tyoff = max 0 (height - th - 
                               2 * w.w_ipad_y - frame)/2
                               in

            let ty = y0 + bold_font.font_ascent + tyoff in
            let tw = string_width_S bold_font 
                                    symbol_font
                                    text_str in



            let tx = x0 in

            (*
            ** draw text label
            *)
            print_string_S ps wx0 wy0 
                          (i2f tx) (i2f ty) fc bc
                          bold_font
                          symbol_font
                          text_str;

            (*
            ** Now the button
            *)
            let bx0,by0 = tx + tw + w.w_ipad_x,
                          y0 in 

            but_box.f_bbox <- bbox_of_xywh bx0 by0 but_size but_size; 

            but_box.f_type <- (
                match but_box.f_type with
                | ShadowSunken
                | ShadowRaised ->
                    if but_active then
                        ShadowSunken
                    else
                        ShadowRaised;
                | ReliefSunken
                | ReliefRaised ->
                    if but_active then
                        ReliefSunken
                    else
                        ReliefRaised;
                | _ -> but_box.f_type);

            printFrame ps w wx0 wy0 but_box true; 

            let bframe = frame_size but_box in
            let bframe_off = frame_offset but_box in

            let symbol = {
                        sym_type = if win_active then 
                                        S_UP
                                   else
                                        S_DOWN;
                        sym_bbox = {x1=bx0+bframe_off;
                                    y1=by0+bframe_off;
                                    x2=bx0+bframe_off+but_size-1-bframe;
                                    y2=by0+bframe_off+but_size-1-bframe}; 
                        sym_col = noColor;
                        sym_width = 1;
                        } in
            printSymbol ps s.s_display w wx0 wy0 symbol;

            if is_select then
            begin
                (*
                ** Draw optional selected string.
                *)

                let th =  normal_font.font_height in
                let tyoff = max 0 (height - th - 
                                    2 * w.w_ipad_y - frame)/2
                                in


                let tx = bx0 + but_size + w.w_ipad_x in
                let ty = y0 + bold_font.font_ascent + tyoff in

                print_string_S ps wx0 wy0
                               (i2f tx) (i2f ty) fc bc
                               normal_font
                               symbol_font
                               select_str;

                (*
                ** If the string doesn't fit into the window,
                ** show an abbriviated version
                *)
                let offered = width - tx - w.w_ipad_x in
                let tw = string_width_S normal_font
                                        symbol_font
                                        select_str in
                if tw <= offered then
                    print_string_S ps wx0 wy0
                                   (i2f tx) (i2f ty) fc bc
                                   normal_font
                                   symbol_font
                                   select_str
                else
                begin
                    let len = String.length select_str in
                    (*
                    ** String may contain embedded symbols!
                    *)
                    let in_sym = ref false in
                    protect (
                        for i = 0 to len-1
                        do
                            if select_str.[i] = '\\' then
                                in_sym := true
                            else if !in_sym && select_str.[i] = ' ' then
                                in_sym := false
                            else if not !in_sym then
                            begin
                                let str' =  String.sub select_str
                                                       i (len-i) in
                                let tw' = string_width_S normal_font
                                                         symbol_font
                                                         str' in

                                if tw' <= offered then
                                begin
                                    print_string_S ps wx0 wy0
                                               (i2f tx) (i2f ty) fc bc
                                               normal_font
                                               symbol_font
                                               str';

                                    raise Exit;
                                end;
                            end;
                        done
                    );
                end;
                
            end;

    
end

(*
** Generic popup widget. Any child widget can be displayed.
**
**  +--------------+
**  | Label |  But |
**  +--------------+
*)
class t root parent label attributes =
    object
    inherit orig Sticky root parent label attributes
end

(*
** Modified version: a select list widget is displayed on popup 
** request, and the selection is permanently displayed.
**
**  +----------------------+
**  | Label | But | Choice |
**  +----------------------+
**
** The 'choices' array contains text strings of all possible
** choices. The initial number specifies the intial selected
** item.
*)

class select_orig kind root parent label choices initial attributes =
    object (self)

    inherit orig kind root parent label attributes
    initializer
        self#set_name "popup_select";
        is_select <- true;
        select_str <- choices.(initial);
        let width = ref 0 in
        Array.iter (fun s ->
                width := max !width (string_width_S normal_font
                                                    symbol_font s);
            ) choices;
        max_select_width <- !width;
        let rows = Array.init (Array.length choices) (fun i ->
                    if i = initial then [Active true] else []) in
        let cb = new VX_checkbox.v parent choices [IpadX 3; IpadY 5;
                                                   Mutual true; 
                                                   Rows rows] in 
        self#container_add cb#contained;

        (*
        ** Update select label after popup window was closed.
        *)
        let update () =
            let i = ref 0 in
            protect(Array.iter (fun c ->
                    if (cb#selected !i) then
                    begin
                        select_str <- c;
                        raise Exit;
                    end;
                    incr i;
                ) choices); 
            in
        self#set_action update;

        cb#set_action (fun i ->
                self#popdown;
                select_str <- choices.(i);
                self#update;
            );

end

class select root parent label choices initial attributes =
    object (self)
    inherit select_orig Sticky root parent label choices initial attributes
end

class selectW root parent label choices initial attributes =
    object (self)
    inherit select_orig Window root parent label choices initial attributes
end

let path_str fp =
    match fp with
    | Amoeba_path p -> p;
    | Unix_path p -> p

let base p =
    Filename.basename p 

class file_select_orig kind root parent label initial attributes =
    object (self)

    inherit orig kind root parent label attributes as super

    val mutable path = path_str initial
    val mutable tree_wob = None
    
    initializer        
        self#set_name "popup_file_select";
        is_select <- true;
        select_str <- base (path_str initial);

        let vb = new VX_view.hv parent [
                                Width 300;
                                Height 300;
                                Border [Size 2];
                                But [Frame ReliefRaised;
                                     Color "grey80";
                                     Size 14;
                                    ];
                                IpadX 2;
                                IpadY 2;
                                Background "white"; 
                                ] in
        let vt = new VX_tree.t vb#container  
                            [
                                IpadX 2;
                                IpadY 2;
                                Mutual true;
                            ] in

        self#container_add vb#contained;
        vb#container_add vt#contained;

        let tree = vt#get_root in
        tree.b_label <- "/";

        
        (*
        ** configure tree dependening on filesyetm type
        *)
        (
            match initial with
            | Amoeba_path _ ->
                ()
            | Unix_path _ ->
               vt#set_action [
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Opening " str;
                        let node = vt#get_node str in
                        let stat = unix_file_tree vt str node in
                        if stat <> std_OK then
                          Db.Pr.ss 10 "failed " (err_why stat)
                        else
                          Db.Pr.s 10 "Ok."
                        );
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Closing " str;
                        let node = vt#get_node str in
                        node.b_child <- None;
                        );
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Selected " str;
                        select_str <- str;
                        path <- str;
                        );
                    ];
        );
        vt#update;
        tree_wob <- Some vt;
        vt#open_dir select_str;
        
        (*
        ** Update select label after popup window was closed.
        *)
        let update = (fun () -> ()) in
        self#set_action update;  
    
    method get_path = path;

    (*
    ** Open and close tree branches
    *)
    method open_dir path =
      match tree_wob with
      | Some v -> v#open_dir path;
      | None -> ()

    method close_dir path =
      match tree_wob with
      | Some v -> v#close_dir path;
      | None -> ()


end


class file_select root parent label initial attributes =
    object (self)
    inherit file_select_orig Sticky root parent label initial attributes 
end

class file_selectW root parent label initial attributes =
    object (self)
    inherit file_select_orig Window root parent label initial attributes 
end


class file_select_edit_orig kind root parent label initial attributes =
    object (self)

    inherit orig kind root parent label attributes as super

    val mutable path = path_str initial
    val mutable tree_wob = None

    initializer        
        self#set_name "popup_file_select_edit";
        is_select <- true;
        select_str <- (path_str initial);

        let vbox = new VX_box.v parent [
                                Width 300;
                            ] in

        let vb = new VX_view.hv vbox#container [
                                ExpandX true;
                                Height 300;
                                Border [Size 2];
                                But [Frame ReliefRaised;
                                     Color "grey80";
                                     Size 14;
                                    ];
                                IpadX 2;
                                IpadY 2;
                                Background "white"; 
                                ] in

        let vt = new VX_tree.t vb#container  
                            [
                                IpadX 2;
                                IpadY 2;
                                Mutual true;
                            ] in
        tree_wob <- Some vt;

        let tb = new VX_view.h vbox#container [
                                ExpandX true;
                                ExpandY false;
                                Border [];
                                But [Frame ReliefRaised;
                                     Color "grey80";
                                     Size 14;
                                    ];
                                IpadX 2;
                                IpadY 2;
                                Background "white"; 
                                ] in
 
        let edit = new VX_text.edit tb#container
                                    "<Edit selection>" [
                                    Background "grey90";
                                    Width 600;
                                    IpadX 5;
                                    IpadY 2;
                                    Text_font Fixed;
                                    Text_style Bold;
                                    Border [];
                                ] in

        edit#set_action (fun _ -> vt#deselect_all);

        self#container_add vbox#contained;
        vb#container_add vt#contained;
        tb#container_add edit#contained;
        vbox#container_add_s [
                                tb#contained;
                                vb#contained;
                              ];

        let tree = vt#get_root in
        tree.b_label <- "/";

        
        (*
        ** configure tree dependening on filesystem type
        *)
        (
            match initial with
            | Amoeba_path _ ->
                ()
            | Unix_path _ ->
               vt#set_action [
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Opening " str;
                        let node = vt#get_node str in
                        let stat = unix_file_tree vt str node in
                        if stat <> std_OK then
                            Db.Pr.ss 10 "failed: " (err_why stat)
                        else
                            Db.Pr.s 10  "Ok.";
                        );
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Closing " str;
                        let node = vt#get_node str in
                        node.b_child <- None;
                        );
                    ActionSU (fun str ->
                        Db.Pr.ss 10 "Selected " str;
                        select_str <- str;
                        edit#set_text str;
                        path <- str;
                        );
                    ];
        );
        vt#open_dir select_str;
        vt#update;
        
        (*
        ** Update select label after popup window was closed.
        *)
        let update () = 
            let str = edit#get_text in
            if str <> "<Edit selection>" then
            begin
                path <- str;
                select_str <- path;
            end;
            in

        self#set_action update;
        action_up <- (fun () ->
          edit#set_cursor 0;
          );

    method get_path = path;

    (*
    ** Open and close tree branches
    *)
    method open_dir path =
      match tree_wob with
      | Some v -> v#open_dir path;
      | None -> ()

    method close_dir path =
      match tree_wob with
      | Some v -> v#close_dir path;
      | None -> ()

    
end

class file_select_edit root parent label initial attributes =
    object (self)
    inherit file_select_edit_orig Sticky root parent label initial attributes
end

class file_select_editW root parent label initial attributes =
    object (self)
    inherit file_select_edit_orig Window root parent label initial attributes
end

