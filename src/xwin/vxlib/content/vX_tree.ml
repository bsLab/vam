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
**    $CREATED:     27.6.2005
**    $VERSION:     1.14
**
**    $INFO:
**
**  Text trees. A tree consists of tree nodes (aka. branches) and leafs.
**  Branches can be opened (showing childs) or not. One plane of the tree
**  can contain a list of tree elements (leafs and nodes).
**
**  +-Branch1
**  | 
**  +-Branch2
**  |
**  --Branch3
**    | 
**    - Leaf 1
**    |
**    - Leaf 2
**
**
**  There must be one initial root node containing the (child) tree! This
**  root node has no parent, and has no previous or next neighbour!. The
**  label of this root node can be for example '/'.
**
**  An action handler can be installed. If a branch is opened, the action
**  handler is called with a path argument:
**
**      /<branch1 label>/<branch2 label>/<leaf1 label>
**
**  The '/' char is defined with 'path_sel'.
**
**  New tree elements (nodes or leafs) can be added with the
**  'tree_add' method specifying the node to which a new child element should
**  be added, or the tree_insert' method with an element specified after
**  which the new element should be inserted. Elements can be deleted
**  using the 'tree_delete' method.
** 
**
**  A But attributes list can contain three action handlers ActionSU:
**  1. Branch opening, 2. closing and 3. label selecttion.
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common
open Printf
open GCCache
open Math 
open Thread 

let warn str =
    print_string ("Warning: "^str);
    print_newline ()

let def_size = 13

let no_refresh = ref false

(*
** The tree structures
*)
type tree =
    | Node of node
    | Leaf of leaf
and node = {
    mutable b_opened : bool;
    mutable b_prev : tree option;   (* previous in tree list                *)
    mutable b_next : tree option;   (* next in tree list                    *)
    mutable b_child : tree option;  (* childs if any                        *)
    mutable b_parent : tree option; (* parent node                          *)
    mutable b_but : frame;          (* branch button                        *)
    mutable b_label : string;       (* the branch name                      *)
    mutable b_frame : frame;        (* the frame around the label string    *)
    mutable b_symbol : symbol;
    mutable b_selected : bool;
}
and leaf = {
    mutable l_label : string;
    mutable l_frame : frame;
    mutable l_prev : tree option;
    mutable l_next : tree option;
    mutable l_parent : tree option;
    mutable l_selected : bool;
}

class orig parent attributes =
    object (self)

    (*
    ** Default text font: generic text and symbol of same size
    *)
    val mutable text_font = parent#font_make Helvetica Bold 12 true;
    val mutable text_font_symbol = parent#font_make Symbol Bold 12 true;


    inherit VX_object.t parent attributes as super



    (*
    ** Default settings
    *)
    val mutable but_size = def_size
    (*
    ** Pad between 1.) button and label, and 2.) indent padding of childs
    ** relative to parent label origin
    *)
    val mutable pad_x = 3
    (*
    ** Space between lines
    *)
    val mutable pad_y = 3

    val mutable path_sep = "/"

    val mutable size_modified = true
    val mutable last_size = (0,0)

    (*
    ** root node
    *)
    val mutable tree = Node {
        b_opened = false;
        b_prev = None;
        b_next = None;
        b_child = None;
        b_parent = None;
        b_but = {(default_frame 1) with
                 f_bbox = bbox_of_xywh 0 0 def_size def_size;
                 f_type = Flat};
        b_label = "Root";
        b_frame = {(default_frame 1) with
                   f_type = Plain};
        b_symbol = default_symbol S_PLUS;
        b_selected = false;
        }


    (*
    ** Action handler called during branch opening and closing.
    *)    
    val mutable open_fun = (fun _ -> ())
    val mutable close_fun = (fun _ -> ())    
    val mutable select_fun = (fun _ -> ())

    (*
    ** Can labels be selected ? (Set with Mutable flag!!)
    *)
    val mutable select = true
    val mutable selected = []

    (*
    ** Only one actual selected item allowed ?
    *)
    val mutable mutual = false
 
    initializer 
        self#set_name "tree";
        __(self#configure [Cursor (FontCursor XC.xc_hand1)]@attributes);
        __(self#configure [Bindings [


          (*
          ** Keyboard and mouse user interaction.
          *)
          ButtonPress,(fun _ ->
                let x,y= !mouse_x_event,
                         !mouse_y_event in

                (*
                ** Search for a branch button
                *)
                let rec iter_tree tree =
                match tree with
                | Node node ->
                    if within_bbox node.b_but.f_bbox x y then
                    begin
                        node.b_opened <- not node.b_opened;
                        (*
                        ** Call action handlers. Branch has changed...
                        *)
                        if node.b_opened then
                            open_fun (self#get_path tree)
                        else
                        begin
                            close_fun (self#get_path tree);
                        end;
                        self#update;
                        raise Exit;
                    end;     

                    if select && (within_bbox node.b_frame.f_bbox x y) then
                    begin
                        let path = self#get_path tree in
                        if not mutual then
                        begin
                            if node.b_selected then
                            begin
                                let selected' = ref [] in
                                List.iter (fun tree' ->
                                    match tree' with
                                    | Node node' ->
                                        if not (node == node') then
                                            selected' := !selected' @ [tree'];
                                    | _ -> selected' := !selected' @ [tree'];
                                    ) selected;
                                selected <- !selected';
                            end
                            else
                                selected <- selected @ [tree];
                            node.b_selected <- not node.b_selected;
                            select_fun path;
                            self#update_elem tree;                        
                        end
                        else if not node.b_selected then
                        begin
                            (*
                            ** Only one entry can be selected.
                            ** Deselect former entry if any.
                            *)
                            (    
                              match selected with
                              | tree'::tl -> 
                              begin
                                match tree' with
                                | Node node' ->
                                        node'.b_selected <- false;                            
                                        self#update_elem tree';
                                        selected <- [tree];
                                | Leaf leaf' ->
                                        leaf'.l_selected <- false;                            
                                        self#update_elem tree';
                                        selected <- [tree];
                              end;
                              | [] -> selected <- [tree];
                            );
                            node.b_selected <- true;
                            select_fun path;
                            self#update_elem tree;                        
                        end;

                        raise Exit;
                    end;
                    if node.b_opened && node.b_child <> None then
                        iter_tree (get_some node.b_child);
                    if node.b_next <> None then
                        iter_tree (get_some node.b_next);

                | Leaf leaf ->
                    if select && (within_bbox leaf.l_frame.f_bbox x y) then
                    begin

                        let path = self#get_path tree in
                        if not mutual then
                        begin
                            if leaf.l_selected then
                            begin
                                let selected' = ref [] in
                                List.iter (fun tree' ->
                                    match tree' with
                                    | Leaf leaf' ->
                                        if not (leaf == leaf') then
                                            selected' := !selected' @ [tree'];
                                    | _ -> selected' := !selected' @ [tree'];
                                    ) selected;
                                selected <- !selected';
                            end
                            else
                                selected <- selected @ [tree];
                            leaf.l_selected <- not leaf.l_selected;
                            select_fun path;
                            self#update_elem tree;                        
                        end
                        else if not leaf.l_selected then
                        begin
                            (*
                            ** Only one entry can be selected.
                            ** Deselect former entry if any.
                            *)
                            (    
                              match selected with
                              | tree'::tl -> 
                              begin
                                match tree' with
                                | Node node' ->
                                        node'.b_selected <- false;                            
                                        self#update_elem tree';
                                        selected <- [tree];
                                | Leaf leaf' ->
                                        leaf'.l_selected <- false;                            
                                        self#update_elem tree';
                                        selected <- [tree];
                              end;
                              | [] -> selected <- [tree];

                            );
                            leaf.l_selected <- true;
                            selected <- [tree];
                            select_fun path;
                            self#update_elem tree;                        
                        end;
                        raise Exit;
                    end;
                    if leaf.l_next <> None then
                        iter_tree (get_some leaf.l_next);
                in

                protect(iter_tree tree);


            );
        ]])

    (*
    ** Configure this widget
    *)

    method configure attrs =
        let remains = super#configure attrs in
        List.iter (fun attr ->
            match attr with
            | Mutable b -> select <- b;
            | Mutual b -> mutual <- b;
            | But attrs ->
            begin
                (*
                ** This list can contain three action handlers ActionSU:
                ** 1. Branch opening, 2. closing and 3. label selecttion.
                *)
                let i = ref 0 in
                List.iter (fun attr ->
                    match attr with
                    | ActionSU f -> 
                    begin
                        incr i;
                        match !i with
                        | 1 -> open_fun <- f;
                        | 2 -> close_fun <- f;
                        | 3 -> select_fun <- f;
                        | _ -> warn_attr "VX_tree: too much handlers" attr
                                         self#name;
                    end;
                    | _ -> warn_attr "VX_tree" attr self#name;
                    ) attrs;
            end;
            | _ -> warn_attr "VX_tree" attr self#name;
            ) remains;
        []

    method set_action attrs =
        (*
        ** This list can contain three action handlers ActionSU:
        ** 1. Branch opening, 2. closing and 3. label selecttion.
        *)
        let i = ref 0 in
        List.iter (fun attr ->
            match attr with
            | ActionSU f -> 
            begin
                incr i;
                match !i with
                | 1 -> open_fun <- f;
                | 2 -> close_fun <- f;
                | 3 -> select_fun <- f;
                | _ -> warn_attr "VX_tree: too much handlers" attr
                                 self#name;
            end;
            | _ -> warn_attr "VX_tree" attr self#name;
          ) attrs;

    (*
    ** Get the size of the tree widget. Calculated from tree elements.
    ** The bounding boxes of all nodes and leafes are calculated only
    ** if size_modified = true. The required size of a tree doesn't depend
    ** on outside window sizes! 
    *)
    method size_request =
Db.Pr.s 10 "VX_tree#size_request";
        let sz = szhints in
        let frame = frame_size w.w_frame in
        let frame_off = frame_offset w.w_frame in

        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
          sz.comp_timestamp <- s.s_timestamp;

          if size_modified then
          begin   
            (*
            ** Calculate required width and height from bounding boxes
            ** of each tree element only if size_modified = true.
            *)

            let x0 = (frame_off+w.w_ipad_x) in
            let y0 = (frame_off+w.w_ipad_y) in

            let x,y = ref 0, ref 0 in

            let rec iter_tree x0 y0 tree =
                match tree with
                | Node node ->
                    let bw,bh = but_size,but_size in
                    let lh = text_font.font_height in
                    let lw = string_width_S text_font
                                            text_font_symbol
                                            node.b_label in

                    let bx0,by0 = x0,
                                  (if lh > bh 
                                    then (y0 + (lh-bh)/2)
                                    else y0)
                                 in
                    node.b_but.f_bbox <- bbox_of_xywh bx0 by0 bw bh;

                    y := max !y node.b_but.f_bbox.y2;        

                    let bframe,bframe_off =
                        frame_size node.b_but,
                        frame_offset node.b_but in

                    node.b_symbol.sym_bbox <- bbox_of_xywh 
                                                (bx0 + bframe_off)  
                                                (by0 + bframe_off)
                                                (bw - bframe)
                                                (bh - bframe);

                    let bx1,by1 = node.b_but.f_bbox.x2,
                                  node.b_but.f_bbox.y2 in
                    let lx0 = bx1 + pad_x in
                    let ly0 = if bh > lh
                                then (y0 + (bh-lh)/2)
                                else y0 in

                    node.b_frame.f_bbox <- bbox_of_xywh lx0
                                                        ly0
                                                        lw
                                                        lh;

                    x := max !x node.b_frame.f_bbox.x2;
                    y := max !y node.b_frame.f_bbox.y2;
                    
                    let tx,ty = lx0,
                                ly0 + text_font.font_ascent in


                    let x0' = node.b_frame.f_bbox.x1 in
                    let y0' = (max node.b_frame.f_bbox.y2
                                   node.b_but.f_bbox.y2) + pad_y in
                    let _,y0'' =
                        if node.b_opened && node.b_child <> None then
                        begin
                            iter_tree x0' y0' (get_some node.b_child)
                        end
                        else
                            x0,y0' in

                    if node.b_next <> None then
                    begin
                        iter_tree x0 y0'' (get_some node.b_next);
                    end
                    else
                        x0,y0'';


                | Leaf leaf ->
                    let bw = but_size in
                    let lh = text_font.font_height in
                    let lw = string_width_S text_font
                                            text_font_symbol
                                            leaf.l_label in

                    leaf.l_frame.f_bbox <- bbox_of_xywh (x0+bw+pad_x)
                                                        y0
                                                        lw
                                                        lh;

                    x := max !x leaf.l_frame.f_bbox.x2;
                    y := max !y leaf.l_frame.f_bbox.y2;

                    let tx,ty = x0+bw+pad_x,
                                y0 + text_font.font_ascent in

                    let y0' = leaf.l_frame.f_bbox.y2 + pad_y in


                    if leaf.l_next <> None then
                    begin
                        iter_tree x0 y0' (get_some leaf.l_next)
                    end
                    else
                        x0,y0';

                in

            __(iter_tree x0 y0 tree);

            let width = !x + 1 + w.w_ipad_x - frame_off + frame in
            let height = !y + 1 + w.w_ipad_y - frame_off + frame in
            last_size <- width,height;
            
            sz.requested_width <- 
                    min sz.max_width
                        (max width
                             sz.min_width);
                
            sz.requested_height <- 
                        min sz.max_height
                            (max height
                                 sz.min_height);
          end
          else
          begin
            (*
            ** Tree was not modfied. Use last saved size.
            *)
            let width,height = last_size in
            
            sz.requested_width <- 
                    min sz.max_width
                        (max width
                             sz.min_width);
                
            sz.requested_height <- 
                        min sz.max_height
                            (max height
                                 sz.min_height);

          end;
          sz
        end;

    (*
    ** Update only one tree element. No bounding box calculations
    ** are performed here!
    *)
    method update_elem tree =
            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in


            let fc = w.w_foreground in
            let bc =  w.w_background in
            let font = text_font.font_id in
            let gcs = s.s_gcs in
            let dpy = s.s_display in
            let win = w.w_window in
            
            let lw = 1 in
            let gc = GCCache.get_fg_bg_lw gcs fg bg lw in


            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            match tree with
            | Node node ->
                    (*
                    ** Update colors
                    *)
                    (
                        match node.b_but.f_type with
                        | ReliefRaised
                        | ReliefSunken ->
                            let fc = parent#getShadow bc in
                            let hc = parent#getHilite bc in
                            let ic = bc in
                            node.b_but.f_background <- bc;
                            node.b_but.f_fillground <- ic;
                            node.b_but.f_foreground <- fc;
                            node.b_but.f_auxiliary <- hc;
                        | _ -> 
                            let ic = bc in
                            node.b_but.f_background <- bc;
                            node.b_but.f_fillground <- ic;
                            node.b_but.f_foreground <- fc;
                    );
                    let tfg,tbg =
                        if node.b_selected then
                        begin
                            node.b_frame.f_background <- fc;
                            node.b_frame.f_fillground <- fc;
                            node.b_frame.f_foreground <- bc;
                            bc.c_pixel,fc.c_pixel
                        end
                        else
                        begin
                            node.b_frame.f_background <- bc;
                            node.b_frame.f_fillground <- bc;
                            node.b_frame.f_foreground <- fc;
                            fc.c_pixel,bc.c_pixel
                        end;
                        in
                    drawFrame s.s_display w gcs node.b_but true;
                    node.b_symbol.sym_type <-
                            if node.b_opened 
                                then S_MINUS
                                else S_PLUS;
                    drawSymbol s.s_display w gcs node.b_symbol; 
                    drawFrame s.s_display w gcs node.b_frame true;
                    (*
                    ** Draw label text
                    *)

                    let bx0,by0,bw,bh = bbox_to_xywh node.b_but.f_bbox in
                    let lx0,ly0,lw,lh = bbox_to_xywh node.b_frame.f_bbox in
                    
                    let tx,ty = lx0,
                                ly0 + text_font.font_ascent in
                                       

                    draw_string_S s.s_display w.w_window gcs
                                  tx ty tfg tbg
                                  text_font
                                  text_font_symbol
                                  node.b_label;


                | Leaf leaf ->
                    (*
                    ** Update colors
                    *)

                    let tfg,tbg =
                        if leaf.l_selected then
                        begin
                            leaf.l_frame.f_background <- fc;
                            leaf.l_frame.f_fillground <- fc;
                            leaf.l_frame.f_foreground <- bc;
                            bc.c_pixel,fc.c_pixel
                        end
                        else
                        begin
                            leaf.l_frame.f_background <- bc;
                            leaf.l_frame.f_fillground <- bc;
                            leaf.l_frame.f_foreground <- fc;
                            fc.c_pixel,bc.c_pixel
                        end;
                        in

                    drawFrame s.s_display w gcs leaf.l_frame true;

                    let bw = but_size in
                    let lx0,ly0,lw,lh = bbox_to_xywh leaf.l_frame.f_bbox in
                    let tx,ty = lx0,
                                ly0 + text_font.font_ascent in


                    draw_string_S s.s_display w.w_window gcs
                                  tx ty tfg tbg
                                  text_font
                                  text_font_symbol
                                  leaf.l_label;



    (*
    ** Calculate all bounding boxes of the tree dependening on the
    ** status of branches (opened).
    ** Finally draw the content.
    *)
    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin

Db.Pr.sd 10 "VX_tree#refresh timestamp" w.w_refresh_timestamp;
            if w.w_clipped then
                w.w_clear <- false;
Db.Pr.sdddd 10 "VX_tree#refresh: clipping box xywh" w.w_clipping.x
                                                   w.w_clipping.y
                                                   w.w_clipping.width
                                                   w.w_clipping.height;
            super#refresh;  
            
            let sz = szhints in

            let fg = w.w_foreground.c_pixel in
            let bg =  w.w_background.c_pixel in


            let fc = w.w_foreground in
            let bc =  w.w_background in
            let font = text_font.font_id in
            let gcs = s.s_gcs in
            let dpy = s.s_display in
            let win = w.w_window in
            
            let lw = 1 in
            let gc = GCCache.get_fg_bg_lw gcs fg bg lw in


            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let x0 = (frame_off+w.w_ipad_x) in
            let y0 = (frame_off+w.w_ipad_y) in

            (*
            ** We can speed up drawing considerably if only
            ** visible parts of a tree are drawn (inside
            ** viewport!).
            *)
            let visible bbox = 
                if w.w_clipped then
                    clip_visible w.w_clipping bbox
                else
                    true in

            let rec iter_tree x0 y0 tree =
                match tree with
                | Node node ->

                    (*
                    ** Update colors
                    *)
                    (
                        match node.b_but.f_type with
                        | ReliefRaised
                        | ReliefSunken ->
                            let fc = parent#getShadow bc in
                            let hc = parent#getHilite bc in
                            let ic = bc in
                            node.b_but.f_background <- bc;
                            node.b_but.f_fillground <- ic;
                            node.b_but.f_foreground <- fc;
                            node.b_but.f_auxiliary <- hc;
                        | _ -> 
                            let ic = bc in
                            node.b_but.f_background <- bc;
                            node.b_but.f_fillground <- ic;
                            node.b_but.f_foreground <- fc;
                    );
                    let tfg,tbg =
                        if node.b_selected then
                        begin
                            node.b_frame.f_background <- fc;
                            node.b_frame.f_fillground <- fc;
                            node.b_frame.f_foreground <- bc;
                            bc.c_pixel,fc.c_pixel
                        end
                        else
                        begin
                            node.b_frame.f_background <- bc;
                            node.b_frame.f_fillground <- bc;
                            node.b_frame.f_foreground <- fc;
                            fc.c_pixel,bc.c_pixel
                        end;
                        in

                    let but_visible = visible node.b_but.f_bbox in
                    if but_visible then
                        drawFrame s.s_display w gcs node.b_but false;

                    (*
                    ** Symbol inside button showing state of branch.
                    ** Opened: -
                    ** Closed: +
                    *)
                                                
                    node.b_symbol.sym_type <-
                            if node.b_opened 
                                then S_MINUS
                                else S_PLUS;
                    if but_visible then 
                        drawSymbol s.s_display w gcs node.b_symbol; 

                    let text_visible = visible node.b_frame.f_bbox in
                    if text_visible && node.b_selected then
                        drawFrame s.s_display w gcs node.b_frame true;

                    (*
                    ** Draw label text
                    *)

                    let tx,ty = node.b_frame.f_bbox.x1,
                                node.b_frame.f_bbox.y1 + 
                                text_font.font_ascent in
                                       

                    if text_visible then
                        draw_string_S s.s_display w.w_window gcs
                                      tx ty tfg tbg
                                      text_font
                                      text_font_symbol
                                      node.b_label;

                    let x0' = node.b_frame.f_bbox.x1 in
                    let y0' = (max node.b_frame.f_bbox.y2
                                   node.b_but.f_bbox.y2) + pad_y in
                    let _,y0'' =
                        if node.b_opened && node.b_child <> None then
                        begin
                            (*
                            ** Draw |
                            *)
                            let xl,yl1,yl2 =
                                tx + but_size/2,
                                y0'-pad_y,
                                y0' in
                            if text_visible then
                                X.polyLine dpy win gc Origin [xl,yl1;xl,yl2];
                            iter_tree x0' y0' (get_some node.b_child)
                        end
                        else
                            x0,y0' in

                    if node.b_next <> None then
                    begin
                        (*
                        ** Draw |
                        **      |
                        *)
                        let xl,yl1,yl2 =
                                x0 + but_size/2,
                                y0'-pad_y,
                                y0'' in
                        X.polyLine dpy win gc Origin [xl,yl1;xl,yl2];
                        iter_tree x0 y0'' (get_some node.b_next);
                    end
                    else
                        x0,y0'';

                | Leaf leaf ->
                    (*
                    ** Update colors
                    *)

                    let tfg,tbg =
                        if leaf.l_selected then
                        begin
                            leaf.l_frame.f_background <- fc;
                            leaf.l_frame.f_fillground <- fc;
                            leaf.l_frame.f_foreground <- bc;
                            bc.c_pixel,fc.c_pixel
                        end
                        else
                        begin
                            leaf.l_frame.f_background <- bc;
                            leaf.l_frame.f_fillground <- bc;
                            leaf.l_frame.f_foreground <- fc;
                            fc.c_pixel,bc.c_pixel
                        end;
                        in

                    let text_visible = visible leaf.l_frame.f_bbox in

                    if text_visible && leaf.l_selected then
                        drawFrame s.s_display w gcs leaf.l_frame true;

                    (*
                    ** Draw label text
                    *)

                    let tx,ty = x0+but_size+pad_x,
                                y0 + text_font.font_ascent in
                                       


                    if text_visible then
                        draw_string_S s.s_display w.w_window gcs
                                      tx ty tfg tbg
                                      text_font
                                      text_font_symbol
                                      leaf.l_label;

                    let y0' = leaf.l_frame.f_bbox.y2 + pad_y in

                    (*
                    ** Draw |_
                    *)
                    let xl1,xl2,yl1,yl2 =
                        x0+but_size/2,
                        x0+but_size+pad_x-1,
                        y0,
                        y0+text_font.font_height/2 in
        
                    if text_visible then
                        X.polyLine dpy win gc Origin [xl1,yl1;xl1,yl2;
                                                      xl2,yl2];

                    if leaf.l_next <> None then
                    begin
                        (*
                        ** Draw |
                        *)
                        let xl,yl1,yl2 =
                                x0+but_size/2,
                                y0+text_font.font_height/2,
                                y0+text_font.font_height+pad_y in
                        if text_visible then
                            X.polyLine dpy win gc Origin [xl,yl1;xl,yl2];
                        iter_tree x0 y0' (get_some leaf.l_next)
                    end
                    else
                        x0,y0';
                in

            __(iter_tree x0 y0 tree)
        end

    method update =
Db.Pr.s 10 "VX_tree#update";
        size_modified <- true;
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0; 
        parent#update;


    (*
    ** Deselect all current selected elements
    *)
    method deselect_all =
        List.iter (fun sel ->
                match sel with
                | Node node ->
                        node.b_selected <- false;                        
                        self#update_elem sel;
                | Leaf leaf ->
                        leaf.l_selected <- false;
                        self#update_elem sel;
            ) selected;
        selected <- [];


    (*
    ** Get the tree root node
    *)

    method get_root = match tree with
                      | Node node -> node;
                      | Leaf _ -> progerr "root must be node";

                      
    (*
    ** Return full path string of given tree element.
    *)
    method get_path tree =
        let path = ref "" in
        
        let rec iter_tree tree =
            match tree with
            | Node node ->
            begin
                    path := (if node.b_label <> path_sep &&
                                node.b_parent <> None
                                then path_sep else "") ^ 
                            (if node.b_label <> path_sep ||
                                !path = "" 
                                then node.b_label else "") ^ !path;
                    match node.b_parent with
                    | Some p -> 
                    begin
                        iter_tree p;
                    end;
                    | None -> ();
            end;
            | Leaf leaf ->
            begin
                    path := (if leaf.l_label <> path_sep 
                                then path_sep else "")^ 
                            leaf.l_label ^ !path;
                    match leaf.l_parent with
                    | Some p -> iter_tree p;
                    | None -> ();
            end;
            in
        iter_tree tree;

        !path

    (*
    ** Return node associated with path (last path element may not a leaf!).
    *)
    method get_node path =
        (*
        ** Split path into components and add root label
        ** if label = path_sep.
        *)
        let pathl = (
                        match tree with
                        | Node node -> 
                            if node.b_label = path_sep 
                                then [node.b_label]
                                else [];
                        | Leaf _ -> progerr "";
                    ) 
                    @ (Str.split (Str.regexp path_sep) path) in
        
        let rec iter_tree tree pathl =
            match tree with
            | Node node ->
            begin    
                match pathl with
                | path'::tl ->
                    (*
                    ** Last component of path ? 
                    *)
                    if node.b_label = path' && tl = [] then
                    begin
                        (*
                        ** All done. Path is resolved.
                        *)
                        node       
                    end
                    else if node.b_label = path' && tl <> [] then
                    begin
                        (*
                        ** Go one step deeper in the tree...
                        *)
                        if node.b_child <> None then
                            iter_tree (get_some node.b_child) tl
                        else
                            failwith "node not found!!!";
                    end
                    else 
                    begin
                        (*
                        ** Try next tree element...
                        *)
                        if node.b_next <> None then
                            iter_tree (get_some node.b_next) pathl
                        else
                            failwith "node not found!!!";
                    end
                | [] -> 
                    (*
                    ** Root node '/' ?
                    *)
                    if path = node.b_label then
                        node
                    else
                        failwith "node not found!!!";
            end;
            | Leaf leaf ->
                (*
                ** A path component may not be a leaf here!!!
                *)
                if leaf.l_next <> None then
                    iter_tree (get_some leaf.l_next) pathl
                else
                    failwith "node not found!!!";
            in
        iter_tree tree pathl;

    method new_node name = 
        let root = match tree with | Node node -> node; | _ -> progerr"" in
        let but = root.b_but in
        let _,_,bw,bh=bbox_to_xywh but.f_bbox in
        {
            b_opened = false;
            b_prev = None;
            b_next = None;
            b_child = None;
            b_parent = None;
            b_but = {but with
                     f_bbox = bbox_of_xywh 0 0 bw bh};
            b_label = name;
            b_frame = {(default_frame 1) with
                       f_type = Plain};
            b_symbol = default_symbol S_PLUS;
            b_selected = false;
        }
    method new_leaf name = 
        {
            l_prev = None;
            l_next = None;
            l_parent = None;
            l_label = name;
            l_frame = {(default_frame 1) with
                       f_type = Plain};
            l_selected = false;
        }


    (*
    ** Add a new tree (element) at the end of the child list
    *)
    method tree_add root_node new_tree =
        let rec add tree =
            match tree with
            | Node node ->
                if node.b_next = None then
                begin
                    node.b_next <- Some new_tree;
                    match new_tree with
                    | Node node' ->
                        node'.b_prev <- Some tree;
                        node'.b_parent <- Some (Node root_node);
                    | Leaf leaf' ->
                        leaf'.l_prev <- Some tree;
                        leaf'.l_parent <- Some (Node root_node);
                end
                else
                    add (get_some node.b_next);
            | Leaf leaf ->
                if leaf.l_next = None then
                begin
                    leaf.l_next <- Some new_tree;
                    match new_tree with
                    | Node node' ->
                        node'.b_prev <- Some tree;
                        node'.b_parent <- Some (Node root_node);
                    | Leaf leaf' ->
                        leaf'.l_prev <- Some tree;
                        leaf'.l_parent <- Some (Node root_node);
                end
                else
                    add (get_some leaf.l_next);
            in
        if root_node.b_child <> None then
            add (get_some root_node.b_child)
        else
        begin
            root_node.b_child <- Some new_tree;
            match new_tree with
            | Node node' ->
                node'.b_prev <- None;
                node'.b_parent <- Some (Node root_node);
            | Leaf leaf' ->
                leaf'.l_prev <- None;
                leaf'.l_parent <- Some (Node root_node);
            
        end;

    method tree_add_s node newl =
        List.iter (fun new_tree ->
            self#tree_add node new_tree) newl

    (*
    ** Insert a new tree (element) after the specified tree element 'this'
    *)
    method tree_insert this new_tree =
        match this with
        | Node node ->
            if node.b_next = None then
            begin
                node.b_next <- Some new_tree;
                match new_tree with
                | Node node' ->
                    node'.b_prev <- Some this;
                    node'.b_parent <- node.b_parent;
                | Leaf leaf' ->
                    leaf'.l_prev <- Some this;
                    leaf'.l_parent <- node.b_parent;
            end
            else
            begin
                let old_next = node.b_next in
                node.b_next <- Some new_tree;

                match new_tree with
                | Node node' ->
                begin
                    node'.b_prev <- Some this;
                    node'.b_parent <- node.b_parent;
                    node'.b_next <- old_next;
                    match (get_some old_next) with
                    | Node node'' ->
                        node''.b_prev <- Some new_tree;
                    | Leaf leaf'' ->
                        leaf''.l_prev <- Some new_tree;
                end;    
                | Leaf leaf' ->
                    leaf'.l_prev <- Some this;
                    leaf'.l_parent <- node.b_parent;
                    leaf'.l_next <- old_next;
            end;
        | Leaf leaf ->
            if leaf.l_next = None then
            begin
                leaf.l_next <- Some new_tree;
                match new_tree with
                | Node node' ->
                    node'.b_prev <- Some this;
                    node'.b_parent <- leaf.l_parent;
                | Leaf leaf' ->
                    leaf'.l_prev <- Some this;
                    leaf'.l_parent <- leaf.l_parent;
            end
            else
            begin
                let old_next = leaf.l_next in
                leaf.l_next <- Some new_tree;

                match new_tree with
                | Node node' ->
                begin
                    node'.b_prev <- Some this;
                    node'.b_parent <- leaf.l_parent;
                    node'.b_next <- old_next;
                    match (get_some old_next) with
                    | Node node'' ->
                        node''.b_prev <- Some new_tree;
                    | Leaf leaf'' ->
                        leaf''.l_prev <- Some new_tree;
                end;    
                | Leaf leaf' ->
                    leaf'.l_prev <- Some this;
                    leaf'.l_parent <- leaf.l_parent;
                    leaf'.l_next <- old_next;
            end

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0  =
            super#print ps wx0 wy0;

            let sz = szhints in

            let fc = w.w_foreground in
            let bc =  w.w_background in

            let gcs = s.s_gcs in
            let dpy = s.s_display in
            let win = w.w_window in
            
            let flw = 1.0 in

            let g = w.w_geometry in
            let width = g.width in
            let height = g.height in

            let frame = frame_size w.w_frame in
            let frame_off = frame_offset w.w_frame in

            let x0 = (frame_off+w.w_ipad_x) in
            let y0 = (frame_off+w.w_ipad_y) in

            let rec iter_tree x0 y0 tree =
                match tree with
                | Node node ->
                    let tfg,tbg =
                        if node.b_selected then
                        begin
                            bc,fc
                        end
                        else
                        begin
                            fc,bc
                        end;
                        in

                    let lx0,ly0,lw,lh = bbox_to_xywh node.b_frame.f_bbox in
                    let bx0,by0,bw,bh = bbox_to_xywh node.b_but.f_bbox in

                    printFrame ps w wx0 wy0 node.b_but true;
                    printSymbol ps dpy w wx0 wy0 node.b_symbol; 
                    printFrame ps w wx0 wy0 node.b_frame true;
                    (*
                    ** Draw label text
                    *)

                    let tx,ty = lx0,
                                ly0 + text_font.font_ascent in
                                       

                    print_string_S ps wx0 wy0 
                                  (i2f tx) (i2f ty) 
                                  tfg tbg
                                  text_font
                                  text_font_symbol
                                  node.b_label;

                    let x0' = node.b_frame.f_bbox.x1 in
                    let y0' = (max node.b_frame.f_bbox.y2
                                   node.b_but.f_bbox.y2) + pad_y in
                    let _,y0'' =
                        if node.b_opened && node.b_child <> None then
                        begin
                            (*
                            ** Draw |
                            *)
                            let xl,yl1,yl2 =
                                (i2f tx) + (i2f bw)/2.0,
                                i2f (y0'-pad_y),
                                i2f (y0') in
                            VX_ps.polyLine ps wx0 wy0 flw fc
                                           [xl,yl1;xl,yl2];
                            iter_tree x0' y0' (get_some node.b_child)
                        end
                        else
                            x0,y0' in

                    if node.b_next <> None then
                    begin
                        (*
                        ** Draw |
                        **      |
                        *)
                        let xl,yl1,yl2 =
                                (i2f x0) + (i2f bw)/2.0,
                                i2f (y0'-pad_y),
                                i2f (y0'') in
                        VX_ps.polyLine ps wx0 wy0 flw fc [xl,yl1;xl,yl2];
                        iter_tree x0 y0'' (get_some node.b_next);
                    end
                    else
                        x0,y0'';

                | Leaf leaf ->
                    (*
                    ** Update colors
                    *)

                    let tfg,tbg =
                        if leaf.l_selected then
                        begin
                            bc,fc
                        end
                        else
                        begin
                            fc,bc
                        end;
                        in

                    let lx0,ly0,lw,lh = bbox_to_xywh leaf.l_frame.f_bbox in
                    let bw = but_size in
                    printFrame ps w wx0 wy0 leaf.l_frame true;
                    (*
                    ** Draw label text
                    *)

                    let tx,ty = x0+bw+pad_x,
                                y0 + text_font.font_ascent in
                                       


                    print_string_S ps wx0 wy0
                                  (i2f tx) (i2f ty) 
                                  tfg tbg
                                  text_font
                                  text_font_symbol
                                  leaf.l_label;

                    let y0' = leaf.l_frame.f_bbox.y2 + pad_y in

                    (*
                    ** Draw |_
                    *)
                    let xl1,xl2,yl1,yl2 =
                        (i2f x0)+(i2f bw)/2.0,
                        i2f (x0+bw+pad_x-1),
                        i2f y0,
                        (i2f y0)+(i2f lh)/2.0 in
        
                    VX_ps.polyLine ps wx0 wy0 flw fc [xl1,yl1;xl1,yl2;
                                                     xl2,yl2];

                    if leaf.l_next <> None then
                    begin
                        (*
                        ** Draw |
                        *)
                        let xl,yl1,yl2 =
                                (i2f x0)+(i2f bw)/2.0,
                                (i2f y0)+(i2f lh)/2.0,
                                i2f (y0+lh+pad_y) in
                        VX_ps.polyLine ps wx0 wy0 flw fc [xl,yl1;xl,yl2];
                        iter_tree x0 y0' (get_some leaf.l_next)
                    end
                    else
                        x0,y0';
                in

            __(iter_tree x0 y0 tree)

    method open_dir path =
      protect (
        let node = self#get_node "/" in
        if not node.b_opened then
        begin
            node.b_opened <- true;
            open_fun "/";
        end;
        let pathl = Str.split (Str.regexp "/") path in
        let path' = ref "" in
        List.iter (fun p ->
          path' := !path' ^ (if p <> "/" then "/" ^ p else ""); 
          let node = self#get_node !path' in
          if not node.b_opened then
          begin
            node.b_opened <- true;
            open_fun !path';
          end;
          ) pathl;
      ); self#update;
  
    method close_dir path =
      protect (
        let node = self#get_node path in
        if node.b_opened then
        begin
          node.b_opened <- false;
          close_fun path;
          self#update;
        end
      ) 
end 

class t = orig
