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
**    $CREATED:     17.7.2005
**    $VERSION:     1.07
**
**    $INFO:
**
**  Generic draw widget. The geometry of the actual widget is hidden.
**  Instead, normalized coordinates in the range from 0.0 .. 1.0 are
**  used. The origin is natural like in a x-y plot:
**
**  ^y
**  +--------+ (1.0,1.0)  
**  |        |
**  |        |
**  +--------+-> x
**  (0.0,0.0)
**
**
**  Coordinates can be transformred: scaling, translation, rotation
**
**  Drawing elements are collected in so called paths. Transformations
**  are applied to these paths in the order they appear.
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


(*
** Various draw and transformation descriptors
*)
type point = {
    mutable xp : float;
    mutable yp : float;
}

let pt x y = {xp=x;yp=y} 

type circle = {
    mutable cp : point;
    mutable rx : float;
    mutable ry : float;    
}


type text = {
    mutable t_rp : point;       (* reference point              *)
    mutable t_an : float;       (* clock direction angle preset *)
}

type frame = {
    mutable f_bc : string;      (* background (fill) color  *)
    mutable f_fc : string;      (* border color             *)
    mutable f_bw : float;       (* border width             *)
    mutable f_bx : bounding_box option; (* initial None     *)
}

type translation = {
    mutable dx : float;
    mutable dy : float;
}

type rotation = {
    mutable rc : point;         (* rotation center          *)
    mutable phi : float;        (* clock direction angle !  *)
}

type scale = {
    mutable sc : point;         (* scaling center           *)
    mutable sx : float;         (* scaling factor in x-dir. *)
    mutable sy : float;         (* scaling factor in y-dir. *)
}

type transformation = 
    | T of translation
    | R of rotation
    | S of scale
    | M         (* mirror y = 1.0 - y *)

type style = 
    (*
    ** Color style
    *)
    | PC of string          (* pen color                *)
    | FC of string          (* background fill color    *)
    (*
    ** Text style
    *)
    | Font_name of text_font
    | Font_style of text_style
    | Font_size of float    (* normalized to 1.0 ! *)

    | AL of (align list)        (* alignment *)

    | Up
    | Down

    (*
    ** Line style
    *)
    | Line of line_type
    | LW of float       (* normalited to 1.0 ! *)

    | FILL                (* fill with FC mode *)

(*
** The main draw type
*)
type draw = 
    (*
    ** Transformation of a path
    *)
    | X of (transformation * draw list)

    (*
    ** Line path
    *)
    | LP of (point list)
    (*
    ** Circle
    *)
    | CR of circle

    (*
    ** A Framebox around the content
    *)
    | F of (frame * draw list)

    (*
    ** Drawing style
    *)
    | Y of (style list * draw list)

    (*
    ** Text box
    *)
    | TX of (text * string) 


let pi = 3.14159265359 
let d2r deg = (deg/360.0)*(2.0*pi)
let fr2i f =
    let fi = i2f (f2i f) in
    let df = f - fi in
    if df > 0.5 then (f2i (ceil f)) else (f2i (floor f))

(*
** Iterate a path descriptor. Apply all transformations in 
** specified order (from right to left!). A new path descriptor
** is returned with removed transformation descriptors.
** Example:
**
**  [ R (r, [T (t, [LP [p1;p2;p3]])]) ] -> [ LP [p1';p2';p3'] ]
*)


let transform_path path =
    (*
    ** Apply all current transformation to a point list.
    *)
    let transform_pl transl lp =
        List.iter (fun trans ->
          List.iter (fun p ->
            match trans with
            | T  t ->
                p.xp <- p.xp + t.dx;
                p.yp <- p.yp + t.dy;
            | R  r ->          
                let phi = -r.phi in
                let x,y = p.xp - r.rc.xp,
                          p.yp - r.rc.yp in
                p.xp <- (x * (cos (d2r phi)) - y * (sin (d2r phi))) + 
                        r.rc.xp;
                p.yp <- (x * (sin (d2r phi)) + y * (cos (d2r phi))) + 
                        r.rc.yp;
            | S  s ->          
                let x,y = p.xp - s.sc.xp,
                          p.yp - s.sc.yp in
                p.xp <- (x * s.sx) + s.sc.xp;
                p.yp <- (y * s.sy) + s.sc.yp;
            | M ->
                p.yp <- 1.0 - p.yp;
            ) lp;
          ) transl;
        lp in

    (*
    ** Transform a circle
    *)
    let transform_cr transl circ =
        let cp = circ.cp in
        List.iter (fun trans ->
            match trans with
            | T  t ->
                cp.xp <- cp.xp + t.dx;
                cp.yp <- cp.yp + t.dy;
            | R  r ->          
                let phi = -r.phi in
                let x,y = cp.xp - r.rc.xp,
                          cp.yp - r.rc.yp in
                cp.xp <- (x * (cos (d2r phi)) - y * (sin (d2r phi))) 
                            + r.rc.xp;
                cp.yp <- (x * (sin (d2r phi)) + y * (cos (d2r phi))) 
                            + r.rc.yp;
                let rx  = abs_float (circ.rx * (cos (d2r phi)) - 
                                      circ.ry * (sin (d2r phi))) in
                let ry  = abs_float (circ.rx * (sin (d2r phi)) + 
                                      circ.ry * (cos (d2r phi))) in
                circ.rx <- rx;
                circ.ry <- ry;
            | S  s ->          
                let x,y = cp.xp - s.sc.xp,
                          cp.yp - s.sc.yp in
                cp.xp <- (x * s.sx) + s.sc.xp;
                cp.yp <- (y * s.sy) + s.sc.yp;
                circ.rx <-  circ.rx * s.sx;    
                circ.ry <-  circ.ry * s.sy;    
            | M ->
                cp.yp <- 1.0 - cp.yp;
          ) transl;
        circ in

    let transform_sl transl sl =
      let sl' = ref [] in
      List.iter (fun style ->
        match style with  
        | LW w ->
            let w' = ref w in 
            List.iter (fun trans ->
                match trans with
                | S  s ->          
                    let s = fmax s.sx s.sy in
                    w' := !w' * s;
                | _ -> ();
                ) transl;
            sl' := !sl' @ [LW !w'];
        | Font_size f ->
            let f' = ref f in 
            List.iter (fun trans ->
                match trans with
                | S  s ->          
                    let s = fmax s.sx s.sy in
                    f' := !f' * s;
                | _ -> ();
                ) transl;
            sl' := !sl' @ [Font_size !f'];
        | _ -> sl' := !sl' @ [style];
        ) sl; !sl' in

    let transform_fr transl fr =
        List.iter (fun trans ->
            match trans with
            | S  s ->          
                    let s = fmax s.sx s.sy in
                    fr.f_bw <- fr.f_bw * s;
            | _ -> ();
            ) transl; fr
        in
    let transform_tx transl tx =
        let cp = tx.t_rp in
        List.iter (fun trans ->
            match trans with
            | T  t ->
                cp.xp <- cp.xp + t.dx;
                cp.yp <- cp.yp + t.dy;
            | R  r ->          
                let phi = -r.phi in
                let x,y = cp.xp - r.rc.xp,
                          cp.yp - r.rc.yp in
                cp.xp <- (x * (cos (d2r phi)) - y * (sin (d2r phi))) 
                            + r.rc.xp;
                cp.yp <- (x * (sin (d2r phi)) + y * (cos (d2r phi))) 
                            + r.rc.yp;
                tx.t_an <- tx.t_an + (d2r phi);
            | S  s ->          
                let x,y = cp.xp - s.sc.xp,
                          cp.yp - s.sc.yp in
                cp.xp <- (x * s.sx) + s.sc.xp;
                cp.yp <- (y * s.sy) + s.sc.yp;
            | M ->
                cp.yp <- 1.0 - cp.yp;
          ) transl;        
        tx in

    let topath = ref [] in
    let transforms = ref [] in

    (*
    ** Transformations are applied and not present in the 
    ** 'topath' destination list anymore.
    *)
    let rec iter_path path topath =
        match path with
        | elem::tl ->
        begin
            match elem with
            | X (t,apply) -> 
                transforms := t :: !transforms; 
                iter_path apply topath;
                transforms := List.tl !transforms;
                iter_path tl topath;
            | LP thispath -> 
                topath := !topath @ [LP (transform_pl !transforms thispath)];
                iter_path tl topath;                
            | CR circ ->
                topath := !topath @ [CR (transform_cr !transforms circ)];
                iter_path tl topath;                
            | TX (t,apply) ->
                topath := !topath @ [TX ((transform_tx !transforms t),apply)];
                iter_path tl topath;                
            | F (f,apply) ->
            (*
            ** Frames must be resolved just before drawing because 
            ** the exact size of text boxes are only known at the
            ** end of the transformation pipeline.
            *)
                let topath' = ref [] in
                iter_path apply topath';
                topath := !topath @ [F (transform_fr !transforms f,!topath')];
                iter_path tl topath;
            | Y (sl,apply) ->
                
                let topath' = ref [] in
                iter_path apply topath';
                topath := !topath @ [Y (transform_sl !transforms sl,!topath')];
                iter_path tl topath;
        end;
        | [] -> ();
        in

    iter_path path topath;
    !topath   

(*
** Get an unmodified copy of the specified path.
*)
let path_copy path =
    let copy_x t apply =
        match t with
        | T t -> (T {dx=t.dx;dy=t.dy}),apply;
        | R r -> (R {rc={xp=r.rc.xp;yp=r.rc.yp};phi=r.phi}),apply;
        | S s -> (S {sc={xp=s.sc.xp;yp=s.sc.yp};sx=s.sx;sy=s.sy}),apply;
        | M -> M,apply;
        in

    let copy_f f apply =
        {f_bc=f.f_bc;
         f_fc=f.f_fc;
         f_bw=f.f_bw;
         f_bx=f.f_bx},apply in

    let copy_tx t apply =
        {t_rp={xp=t.t_rp.xp;yp=t.t_rp.yp};
         t_an=t.t_an},apply in

    let copy_pl pl =
        List.map (fun p -> {xp=p.xp;yp=p.yp} ) pl in

    let copy_cr circ =
            let cp = circ.cp in
            {rx=circ.rx;
             ry=circ.ry;
             cp={xp=cp.xp;yp=cp.yp}} in

    let rec iter_path path =
        match path with
        | elem::tl ->
        begin
            match elem with
            | X (t,apply) -> 
                [X (copy_x t (iter_path apply))]@(iter_path tl);
            | LP thispath -> 
                [LP (copy_pl thispath)]@(iter_path tl);  
            | CR circ ->
                [CR (copy_cr circ)] @ (iter_path tl);
            | F (f,apply) ->
                [F (copy_f f (iter_path apply))]@(iter_path tl);
            | TX (t,apply) ->
                [TX (copy_tx t apply)]@(iter_path tl);
            | Y (sl,apply) ->
                [Y ((List.map (fun s -> s) sl),(iter_path apply))]@
                        (iter_path tl);
        end;
        | [] -> [];
        in
    iter_path path

(*
** only for internal usage
*)
type draw_style = { 
    mutable y_text_font : font;
    mutable y_text_font_symbol : font;
    mutable y_font_desc : font_desc;
    mutable y_align_h : align;
    mutable y_align_v : align;
    mutable y_foreground : color;
    mutable y_background : color;    
    mutable y_linewidth : int;
}

(*
** Calculate frame boundaries (if any). Must be done after paths_t were 
** calculated (due some uncertainy of text bounding boxes).
*)
let expand_frames parent pathsl =
        let style = {
        y_text_font = parent#font_make Helvetica Roman 12 true;
        y_text_font_symbol = parent#font_make Symbol Roman 12 true;
        y_font_desc = font_desc (parent#font_make Helvetica Roman 12 true);
        y_align_h = Center;
        y_align_v = Middle;
        y_foreground = parent#color_make "black" true;
        y_background = parent#color_make "white" true;
        y_linewidth = 1;
        } in

        let bbox = {x1=max_int;y1=max_int;x2=min_int;y2=min_int} in
        let init_bbox () = bbox.x1 <- max_int;
                           bbox.y1 <- max_int;
                           bbox.x2 <- min_int;
                           bbox.y2 <- min_int; in
        List.iter (fun pathl ->
          let rec path_iter pathl =
                List.iter (fun path ->
                match path with
                | LP points ->
                    List.iter (fun p ->
                            let x,y = (fr2i p.xp),(fr2i p.yp) in
                            expand_bbox bbox x y;
                        ) points;
                | CR circle ->
                    let cp = circle.cp in
                    let x,y=(fr2i cp.xp),(fr2i cp.yp) in
                    let rx,ry = (fr2i circle.rx),(fr2i circle.ry) in
                    let ac = [(x-rx),(y-ry),2*rx,2*ry,0,(360*64)] in
                    expand_bbox bbox (x-rx) (y-ry);
                    expand_bbox bbox (x+rx) (y+ry);
                | TX (tx,apply) ->

                    let align_h,align_v = 
                                    style.y_align_h,
                                    style.y_align_v  in
                    let x0,y0 = (fr2i tx.t_rp.xp),
                                (fr2i tx.t_rp.yp) in

                    let text_str = apply in
                    (*
                    ** Horizontal text. Can be drawn directly.
                    *)
                    let tw = string_width_S style.y_text_font
                                            style.y_text_font_symbol
                                            text_str in
                    let th = style.y_text_font.font_height in
                    let tx = 
                            match align_h with
                            | Left -> x0;
                            | Right -> x0 - tw;
                            | Center -> x0 - tw/2;
                            | _ -> progerr "";
                            in
                    let asc = style.y_text_font.font_ascent in
                    let ty = 
                            match align_v with
                            | Top -> y0 + asc;
                            | Bottom -> y0 - th +  asc;
                            | Middle -> y0 - th/2 + asc;
                            | _ -> progerr "";
                            in

                    expand_bbox bbox (tx)       (y0-th/2);
                    expand_bbox bbox (tx+tw)    (y0+th/2);
                | F (f,apply) ->
                    init_bbox ();
                    path_iter apply;
                    let pad = 5 + (fr2i f.f_bw) in
                    f.f_bx <- Some {bbox with x1=bbox.x1 - pad;
                                              y1=bbox.y1 - pad;
                                              x2=bbox.x2 + pad;
                                              y2=bbox.y2 + pad;};
                | Y (sl,apply) -> 
                    let text = ref false in
                    List.iter (fun s ->
                        match s with
                        | Font_style n -> style.y_font_desc.text_style <- n;
                                          text := true;
                        | Font_size f -> style.y_font_desc.text_size <- (fr2i f);
                                         text := true;
                        | Font_name n -> style.y_font_desc.text_font <- n;
                                         text := true;
                        | AL al ->
                            List.iter (fun a ->
                                match a with
                                | Left | Right | Center -> style.y_align_h <- a;
                                | Top | Bottom | Middle -> style.y_align_v <- a;
                                ) al;
                        | _ -> ()) sl;
                    if !text then
                    begin
                        style.y_text_font <- parent#font_make  
                                          style.y_font_desc.text_font 
                                          style.y_font_desc.text_style 
                                          style.y_font_desc.text_size true;
                        style.y_text_font_symbol <- parent#font_make  
                                          Symbol
                                          style.y_font_desc.text_style
                                          style.y_font_desc.text_size true;

                    end;
                    path_iter apply;
                | _ -> ();
                ) pathl;
            in
            path_iter pathl;
         ) pathsl



class t parent attributes =
    object (self)
    
    (*
    ** Current style defintions
    ** May change during path evaluation!
    *)

    val mutable style = {
        y_text_font = parent#font_make Helvetica Roman 12 true;
        y_text_font_symbol = parent#font_make Symbol Roman 12 true;
        y_font_desc = font_desc (parent#font_make Helvetica Roman 12 true);
        y_align_h = Center;
        y_align_v = Middle;
        y_foreground = parent#color_make "black" true;
        y_background = parent#color_make "white" true;
        y_linewidth = 1;
        };

    (*
    ** During path evaluation different style can be used in a stacked
    ** fashion. Remember style history. Top of list always our initial
    ** =  default settings.
    *)
    val mutable styles = []

    inherit VX_object.t parent attributes as super


    initializer 
        self#set_name "draw";

    (*
    ** Original normalized paths
    *)
    val mutable paths = []

    (*
    ** Copy of last transformed copy of original path.
    ** Stay actual as long as the draw geometry stays unchanged.
    *)
    val mutable paths_t = []    

    val mutable last_geom =  {x=0;y=0;width=0;height=0;border=0}
    val mutable path_transform = true

    method size_request =
        let sz = szhints in
        let frame = frame_size w.w_frame in
        if not w.w_size_modified || 
           sz.comp_timestamp = s.s_timestamp then sz
        else 
        begin
            sz.requested_width <- 
                    min sz.max_width sz.min_width;
                
            sz.requested_height <- 
                    min sz.max_height sz.min_height;
    
            sz
        end

    method size_allocate x y dx dy =
        let g = w.w_geometry in
        let size_changed =
            last_geom.x <> g.x ||
            last_geom.y <> g.y ||
            last_geom.width <> g.width ||
            last_geom.height <> g.height in
        if size_changed then
        begin
            last_geom.x <- g.x;
            last_geom.y <- g.y;
            last_geom.width <- g.width;
            last_geom.height <- g.height;
            path_transform <- true;
        end;
        super#size_allocate x y dx dy

    (*
    ** Return frame box of draw area 
    *)
    method draw_bbox =
        let g = w.w_geometry in
        let frame,frame_off =
            frame_size w.w_frame,
            frame_offset w.w_frame in

        let x0,y0,dx,dy =
            frame_off + w.w_ipad_x,
            frame_off + w.w_ipad_y,
            g.width - frame - w.w_ipad_x,
            g.height - frame - w.w_ipad_y in
        bbox_of_xywh x0 y0 dx dy

    (*
    ** Clear draw area
    *)
    method clear =
        let x,y,wi,he = bbox_to_xywh self#draw_bbox in
        let cache = s.s_gcs in
        let dpy = s.s_display in
        let win = w.w_window in
        let bg = w.w_background.c_pixel in
        let gc = GCCache.get_fg_bg_lw cache bg bg 1 in
        X.polyFillRectangle dpy win gc [x,y,wi,he];

    (*
    ** Transform all paths to actual window geometry.
    *)
    method transform_paths =
        if path_transform then
        begin
            paths_t <- [];
            let paths' = ref [] in
            (*
            ** Transformations are done in place. Therefore, a
            ** copy of the original paths are required.
            *)
            List.iter (fun path -> 
                        paths' := !paths' @ [(path_copy path)];
                ) paths;

            (*
            ** Scale and translate normalized path to actual
            ** drawing coordinates.
            *)
            let x,y,wi,he = bbox_to_xywh self#draw_bbox in
            let s = {sc={xp=0.0;yp=0.0};sx=i2f wi;sy=i2f he} in
            let t = {dx=i2f x;dy = i2f y} in
            List.iter (fun pathl ->
                paths_t <- paths_t @ 
                   [(transform_path [X (T t,[X (S s,[X (M,pathl)])])])];
                ) !paths';
            expand_frames parent paths_t;
            path_transform <- false;
        end;


    method private new_draw_style =
        {
            style with
            y_text_font = style.y_text_font;
        }

    (*
    ** Save current style settings.
    *)
    method private save_style  =
            styles <- style :: styles;
            style <- self#new_draw_style;

    (*
    ** Restore previous style settings
    *)
    method private restore_style =
            let old = List.hd styles in
            styles <- List.tl styles;
            style <- old 

    (*
    ** Return style of parant environment from actual style list 
    *)
    method parent_style =
        match styles with
        | actual :: prev ->
        begin
            match prev with
            | parent :: tl -> parent;
            | [] -> actual;
        end;
        | [] -> style

    (*
    ** Draw one path. With 'ersae' flag set the background color of the widget 
    ** is used for drawing. Needed by the delete_path method.
    *)

    method draw_path path erase =
Db.Pr.ss 10 "VX_draw#draw_path erase" (if erase then "yes" else "no");

        let cache = s.s_gcs in
        let dpy = s.s_display in
        let win = w.w_window in
        let fg = if not erase
                    then style.y_foreground.c_pixel
                    else style.y_background.c_pixel in
        let bg = style.y_background.c_pixel in
        let lw = style.y_linewidth in
        let gc = GCCache.get_fg_bg_lw cache fg bg lw in
        let gc_inv = GCCache.get_fg_bg_lw cache bg fg lw in
        let gcl = ref [gc,gc_inv] in
        (*
        ** Get current normal graphics context
        *)
        let get_gc () = 
            match !gcl with
            | gct::tl -> let gc,gc_inv = gct in gc;
            | [] -> vx_error "VX_draw#draw_paths: no gc!!!";
                    assert false;
            in
        (*
        ** Get current fore- and background inverted (bg,fg) graphics context
        *)
        let get_gc_inv () = 
            match !gcl with
            | gct::tl -> let gc,gc_inv = gct in gc_inv;
            | [] -> vx_error "VX_draw#draw_paths: no gc!!!";
                    assert false;
            in
        (*
        ** Make a new graphics context pair (normal,inverted) from
        ** style list. Either it's a text or geometry context.
        ** Update our style variables. Save current style context.
        *)

        let make_gc sl =
            let text = ref false in
            List.iter (fun s ->
                match s with
                | LW w -> style.y_linewidth <- (fr2i w);
                | PC c -> let fg = parent#color_make c true in
                          style.y_foreground <- fg;
                | FC c -> let bg = parent#color_make c true in
                          style.y_background <- bg;
                | Font_name n -> style.y_font_desc.text_font <- n;
                                 text := true;
                | Font_style n -> style.y_font_desc.text_style <- n;
                                  text := true;
                | Font_size f -> style.y_font_desc.text_size <- (fr2i f);
                                 text := true;
                | AL al ->
                    List.iter (fun a ->
                        match a with
                        | Left | Right | Center -> style.y_align_h <- a;
                        | Top | Bottom | Middle -> style.y_align_v <- a;
                        ) al;
                | _ -> ();
                ) sl;
            (*
            ** Either geometry or text style
            *)
            if (not !text) then
            begin
                let ps = self#parent_style in
                let fg = if not erase 
                            then style.y_foreground.c_pixel
                            else ps.y_background.c_pixel in
                let bg = if not erase 
                            then style.y_background.c_pixel 
                            else ps.y_background.c_pixel in
                GCCache.get_fg_bg_lw cache fg bg style.y_linewidth,
                GCCache.get_fg_bg_lw cache bg fg style.y_linewidth
            end
            else
            begin
                style.y_text_font <- parent#font_make  
                                          style.y_font_desc.text_font 
                                          style.y_font_desc.text_style 
                                          style.y_font_desc.text_size true;
                style.y_text_font_symbol <- parent#font_make  
                                          Symbol
                                          style.y_font_desc.text_style
                                          style.y_font_desc.text_size true;

                let ps = self#parent_style in
                let fg = if not erase 
                            then style.y_foreground.c_pixel
                            else ps.y_background.c_pixel in
                let bg = if not erase 
                            then style.y_background.c_pixel 
                            else ps.y_background.c_pixel in

                (*
                ** Not really needed!! Text is drawn outside...
                *)
                GCCache.get_fg_bg_font cache fg bg style.y_text_font.font_id,
                GCCache.get_fg_bg_font cache bg fg style.y_text_font.font_id
            end;          
            in
        (*
        ** Current Style with type style list - only locally used
        *)
        let cur_style = ref None in
        let fill_mode () =
            let fill = ref false in
            match !cur_style with
            | Some sl ->
                List.iter (fun s -> if s = FILL then fill := true;) sl;
                !fill 
            | None -> false;
            in

        let rec path_iter pathl =
                List.iter (fun path ->
                match path with
                | LP points ->
                    let points' = 
                        List.map (fun p ->
                            let x,y = (fr2i p.xp),(fr2i p.yp) in
                            x,y) points in
                    if (fill_mode ()) then
                        X.fillPoly dpy win (get_gc_inv ()) Origin 
                                           Complex points';
                    X.polyLine dpy win (get_gc ()) Origin points';                    
                | CR circle ->
                    let cp = circle.cp in
                    let x,y=(fr2i cp.xp),(fr2i cp.yp) in
                    let rx,ry = (fr2i circle.rx),(fr2i circle.ry) in
                    let ac = [(x-rx),(y-ry),2*rx,2*ry,0,(360*64)] in
                    X.polyArc dpy win (get_gc ()) ac;
                | F (f,apply) ->
                    let ps = self#parent_style in
                    let frame = default_frame 1 in
                    frame.f_bbox <- get_some f.f_bx;
                    frame.f_type <- Flat;
                    frame.f_size <- fr2i f.f_bw;
                    frame.f_fillground <- 
                        if f.f_bc <> "" then
                            parent#color_make f.f_bc true
                        else
                            style.y_background;
                    frame.f_background <- ps.y_background;
                    frame.f_foreground <- 
                            if not erase && f.f_fc <> "" then
                                parent#color_make f.f_fc true
                            else if not erase then
                                style.y_foreground
                            else ps.y_background;
                    if erase then
                        frame.f_black <- ps.y_background;

                    drawFrame dpy w cache frame false;
                    path_iter apply;

                | TX (tx,apply) ->
                    if tx.t_an = 0.0 then
                    begin
                        let align_h,align_v = 
                                    style.y_align_h,
                                    style.y_align_v  in
                        let x0,y0 = (fr2i tx.t_rp.xp),
                                    (fr2i tx.t_rp.yp) in

                        let text_str = apply in
                        (*
                        ** Horizontal text. Can be drawn directly.
                        *)
                        let tw = string_width_S style.y_text_font
                                                style.y_text_font_symbol
                                                text_str in
                        let th = style.y_text_font.font_height in
                        let tx = 
                            match align_h with
                            | Left -> x0;
                            | Right -> x0 - tw;
                            | Center -> x0 - tw/2;
                            | _ -> progerr "";
                            in
                        let asc = style.y_text_font.font_ascent in
                        let ty = 
                            match align_v with
                            | Top -> y0 + asc;
                            | Bottom -> y0 - th +  asc;
                            | Middle -> y0 - th/2 + asc;
                            | _ -> progerr "";
                            in
                        let fc,bc = if not erase then
                                        style.y_foreground.c_pixel,
                                        style.y_background.c_pixel
                                    else                        
                                        style.y_background.c_pixel,
                                        style.y_background.c_pixel in
                        draw_string_S dpy w.w_window cache
                                       tx ty 
                                       fc bc
                                       style.y_text_font
                                       style.y_text_font_symbol
                                       text_str; 
                    end
                    else
                    begin
                        vx_error "TX with ang <> 0.0 not supported";
                    end;
                | Y (sl,apply) ->
                    cur_style := Some sl;
                    self#save_style;
                    gcl := (make_gc sl) :: !gcl;
                    path_iter apply;
                    gcl := List.tl !gcl;
                    cur_style := None;
                    self#restore_style;
                | _ -> ();
                ) pathl;
            in
            path_iter path;
      
    (*
    ** Draw all (already transformed) drawing paths.
    *)
    method draw_paths =
        List.iter (fun path ->
            self#draw_path path false;
         ) paths_t;


    (*
    ** Postscript printing
    *)
    method print_paths ps wx0 wy0 =
        let fg = style.y_foreground in
        let bg = style.y_background in

        let gc = fg,bg,1.0 in
        let gc_inv = bg,fg,1.0 in
        let gcl = ref [gc,gc_inv] in
        (*
        ** Get current normal graphics context
        *)
        let get_gc () = 
            match !gcl with
            | gct::tl -> let gc,gc_inv = gct in gc;
            | [] -> vx_error "VX_draw#print_paths: no gc!!!";
                    assert false;
            in
        (*
        ** Get current fore- and background inverted (bg,fg) graphics context
        *)
        let get_gc_inv () = 
            match !gcl with
            | gct::tl -> let gc,gc_inv = gct in gc_inv;
            | [] -> vx_error "VX_draw#print_paths: no gc!!!";
                    assert false;
            in

        (*
        ** Make a new graphics context pair (normal,inverted) from
        ** style list. Either it's a text or geometry context.
        ** Update our style variables. Save current style context.
        ** Returns: (fg,bg,lw),(bg,fg,lw).
        *)

        let make_gc sl =
            let text = ref false in
            List.iter (fun s ->
                match s with
                | LW w -> style.y_linewidth <- (fr2i w);
                | PC c -> let fg = parent#color_make c true in
                          style.y_foreground <- fg;
                | FC c -> let bg = parent#color_make c true in
                          style.y_background <- bg;
                | Font_name n -> style.y_font_desc.text_font <- n;
                                 text := true;
                | Font_style n -> style.y_font_desc.text_style <- n;
                                  text := true;
                | Font_size f -> style.y_font_desc.text_size <- (fr2i f);
                                 text := true;
                | AL al ->
                    List.iter (fun a ->
                        match a with
                        | Left | Right | Center -> style.y_align_h <- a;
                        | Top | Bottom | Middle -> style.y_align_v <- a;
                        ) al;
                | _ -> ();
                ) sl;

            (*
            ** Either geometry or text style
            *)
            if (not !text) then
            begin
                let fg = style.y_foreground in
                let bg = style.y_background in
                let lw = style.y_linewidth in
                (fg,bg,(i2f lw)),
                (bg,fg,(i2f lw))
            end
            else
            begin
                style.y_text_font <- parent#font_make  
                                          style.y_font_desc.text_font 
                                          style.y_font_desc.text_style 
                                          style.y_font_desc.text_size true;
                style.y_text_font_symbol <- parent#font_make  
                                          Symbol
                                          style.y_font_desc.text_style
                                          style.y_font_desc.text_size true;

                let fg = style.y_foreground in
                let bg = style.y_background in
                (fg,bg,1.0),
                (bg,fg,1.0)
            end;          
            in
        (*
        ** Current Style with type style list - only locally used
        *)
        let cur_style = ref None in
        let fill_mode () =
            let fill = ref false in
            match !cur_style with
            | Some sl ->
                List.iter (fun s -> if s = FILL then fill := true;) sl;
                !fill 
            | None -> false;
            in

        List.iter (fun pathl ->
          let rec path_iter pathl =
                List.iter (fun path ->
                match path with
                | LP points ->
                    let fc,bc,lw = get_gc () in
                    let points' = 
                        List.map (fun p ->
                            let x,y = p.xp,p.yp in
                            x,y) points in
                    if (fill_mode ()) then
                        VX_ps.fillPoly ps wx0 wy0 bc
                                       points';
                    VX_ps.polyLine ps wx0 wy0 lw fc points';                    
                | CR circle ->
                    let fc,bc,lw = get_gc () in
                    let cp = circle.cp in
                    let x,y=cp.xp,cp.yp in
                    let rx,ry = circle.rx,circle.ry in
                    let ac = [(x-rx),(y-ry),2.0*rx,2.0*ry,0.0,-360.0] in
                    VX_ps.polyCircle ps wx0 wy0 lw fc ac;

                | F (f,apply) ->
                    let fc,bc,lw = get_gc () in
                    let frame = default_frame 1 in
                    frame.f_bbox <- get_some f.f_bx;
                    frame.f_type <- Flat;
                    frame.f_size <- fr2i f.f_bw;

                    frame.f_fillground <- 
                        if f.f_bc <> "" then
                            parent#color_make f.f_bc true
                        else
                            bc;
                    frame.f_background <- bc;
                    frame.f_foreground <- 
                            if f.f_fc <> "" then
                                parent#color_make f.f_fc true
                            else fc;


                    printFrame ps w wx0 wy0 frame false;
                    path_iter apply;

                | TX (tx,apply) ->
                    if tx.t_an = 0.0 then
                    begin
                        let align_h,align_v = 
                                    style.y_align_h,
                                    style.y_align_v  in
                        let x0,y0 = tx.t_rp.xp,
                                    tx.t_rp.yp in

                        let text_str = apply in
                        (*
                        ** Horizontal text. Can be drawn directly.
                        *)
                        let tw =  i2f (string_width_S style.y_text_font
                                                style.y_text_font_symbol
                                                text_str) in
                        let th =  i2f (style.y_text_font.font_height) in
                        let tx = 
                            match align_h with
                            | Left -> x0;
                            | Right -> x0 - tw;
                            | Center -> x0 - tw/2.0;
                            | _ -> progerr "";
                            in
                        let asc = i2f (style.y_text_font.font_ascent) in
                        let ty = 
                            match align_v with
                            | Top -> y0 + asc;
                            | Bottom -> y0 - th +  asc;
                            | Middle -> y0 - th/2.0 + asc;
                            | _ -> progerr "";
                            in
                        print_string_S ps wx0 wy0
                                       tx ty
                                       style.y_foreground
                                       style.y_background
                                       style.y_text_font
                                       style.y_text_font_symbol
                                       text_str; 
                    end
                    else
                    begin
                        vx_error "TX with ang <> 0.0 not supported";
                    end;

                | Y (sl,apply) ->
                    cur_style := Some sl;
                    self#save_style;
                    gcl := (make_gc sl) :: !gcl;
                    path_iter apply;
                    gcl := List.tl !gcl;
                    cur_style := None;
                    self#restore_style;
                | _ -> ();
                ) pathl;
            in
            path_iter pathl;
         ) paths_t;
      

    (*
    ** Add a new drawing path.
    *)
    method add_path path =
        paths <- paths @ [path];
        path_transform <- true;
        self#update

    method clear_paths = 
        paths <- [];
        paths_t <- [];
        path_transform <- true;

    (*
    ** Remove one path - restore drawing region below this path
    *)    
    method delete_path n =
        let paths' = ref [] in
        let paths_t' = ref [] in
        let i = ref 0 in
        List.iter (fun path ->
                if !i = n then
                begin
                    ()
                end
                else
                begin
                    paths' := !paths' @ [path];
                end;
                incr i;
            ) paths;  
        i := 0;
        List.iter (fun path_t ->
                if !i = n then
                begin
                    (*
                    ** Erase to background
                    *)
                    self#draw_path path_t true;
                end
                else
                begin
                    paths_t' := !paths_t' @ [path_t];
                end;
                incr i;
            ) paths_t;  
        paths <- !paths';
        paths_t <- !paths_t';

    (*
    ** Exchange one path - restore drawing region below this path
    *)    
    method exchange_path n newpath =
        let paths' = ref [] in
        let paths_t' = ref [] in
        let i = ref 0 in
        List.iter (fun path ->
                if !i = n then
                begin
                    paths' := !paths' @ [newpath];
                end
                else
                begin
                    paths' := !paths' @ [path];
                end;
                incr i;
            ) paths;  
        i := 0;
        List.iter (fun path_t ->
                if !i = n then
                begin
                    (*
                    ** Erase to background
                    *)
                    self#draw_path path_t true;
                end
                else
                begin
                    paths_t' := !paths_t' @ [path_t];
                end;
                incr i;
            ) paths_t;  
        paths <- !paths';
        paths_t <- !paths_t';
        path_transform <- true;
        self#update


    method update =
        super#update;
        self#transform_paths;
        self#draw_paths;

    method refresh =
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
Db.Pr.s 10 "VX_draw#refresh";
            super#refresh;
            self#transform_paths;
            self#draw_paths;
        end;

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0 =
        super#print ps wx0 wy0;
        self#print_paths ps wx0 wy0
end

