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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     3.6.2005
**    $VERSION:     1.11
**
**    $INFO:
**
**  Main Postscript output module.
**
**  We must distinguish two different coordinate systems:
**
**  X --x      y 
**  |          |
**  y          P--x
**
**
**  All draw routines will emit coordinates with the following
**  transformation between X and PS coordinates:
**
**      X(x,y) -> P(x,-y)
**
**  That means, the content must be translated and scaled finally!
**
**    $ENDOFINFO
**
*)

open VX_types
open Printf
open Math

let max_bbox = bbox_of_xywh (min_int/2) (min_int/2) (max_int) 
                                                    (max_int)
let clip_bbox = ref [max_bbox]

let vx_error str =
    print_string ("Fatal Error: "^str);
    print_newline ();
    exit 1

let i2f = float_of_int
let f2i = int_of_float
let fmax a b = if a > b then a else b

type paper_format =
    | A4

let paper_size format =
    match format with
    | A4 -> 210,298


(*
** Coordinate and unit transformation.
**
** Normalized scaling factor (X pixel -> PS): 1.0 !!!
** The image scaled at the end to desired size.
*)
module C = struct
    (*
    ** x and y coordinate transformation
    *)
    let x2p x = x
    let y2p y = -.y 

    (*
    ** Size (width,height) conversion (X -> PS)
    *)
    let s2p s = s

    (*
    ** Millimeters to PS 1/72 inches
    *)
    let m2p fm = (fm *. 72.0) /. 25.4 
end


(*
** Create a polyline. x0,y0: current X coordinates.
** Line coordinates are relative to x0,y0.
*)
let polyLine (ps:ps) (x0:int) (y0:int) (lw:float) (color:color) flines =
    let out str = ps.ps_cont <- ps.ps_cont @  [str]  in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
    out "newpath";
    out (sprintf "%.1f setlinewidth" lw);
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in
    let fx0,fy0 = i2f x0, i2f y0 in
    List.iter (fun (fx,fy) ->
        let fx,fy = fx0 +. fx, fy0 +. fy in

        if !first then
        begin
            out (sprintf "%.1f %.1f moveto" (C.x2p fx) (C.y2p fy));
            first := false;
        end
        else
            out (sprintf "%.1f %.1f lineto" (C.x2p fx) (C.y2p fy));
        if within_bbox (List.hd !clip_bbox) (f2i fx) (f2i fy) then 
            expand_bbox ps.ps_bbox (f2i fx) (f2i fy);
      ) flines;
    out "stroke";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"

(*
** Fill a polyline area
** x0,y0: current X coordinates.
** Line coordinates are relative to x0,y0.
*)

let fillPoly (ps:ps) (x0:int) (y0:int) (color:color) flines =
    let out str = ps.ps_cont <- ps.ps_cont @ [str]  in
    let lw = 1.0 in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
                   
    out "newpath";
    (*
    ** Set linewidth and color state
    *)
    out (sprintf "%.1f setlinewidth" lw);
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in
    let fx0,fy0 = i2f x0, i2f y0 in
    List.iter (fun (fx,fy) ->
        let fx,fy = fx0 +. fx,
                    fy0 +. fy in 

        if !first then
        begin
            out (sprintf "%.1f %.1f moveto" (C.x2p fx) (C.y2p fy));
            first := false;
        end
        else
            out (sprintf "%.1f %.1f lineto" (C.x2p fx) (C.y2p fy));
        if within_bbox (List.hd !clip_bbox) (f2i fx) (f2i fy) then
            expand_bbox ps.ps_bbox (f2i fx) (f2i fy);
      ) flines;
    out "closepath";
    out "fill";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"

(*
** Fill a rectangular area
** x0,y0: current X coordinates.
** Line coordinates are relative to x0,y0.
*)

let fillRectangle (ps:ps) (x0:int) (y0:int) (color:color) x y w h =
    let out str = ps.ps_cont <- ps.ps_cont @  [str] in
    let lw = 1.0 in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
                   
    out "newpath";
    (*
    ** Set linewidth and color state
    *)
    out (sprintf "%.1f setlinewidth" lw);
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in
    let fx0,fy0 = i2f x0, i2f y0 in
    
    
    let flines = [
            x,y; x+w-1.0,y;
            x+w-1.0,y+h-1.0;
            x,y+h-1.0;
            x,y;    
        ] in 
    List.iter (fun (fx,fy) ->
        let fx,fy = fx0 +. fx,
                    fy0 +. fy in 

        if !first then
        begin
            out (sprintf "%.1f %.1f moveto" (C.x2p fx) (C.y2p fy));
            first := false;
        end
        else
            out (sprintf "%.1f %.1f lineto" (C.x2p fx) (C.y2p fy));
        if within_bbox (List.hd !clip_bbox) (f2i fx) (f2i fy) then
            expand_bbox ps.ps_bbox (f2i fx) (f2i fy);
      ) flines;
    out "closepath";
    out "fill";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"


let polyFillRectangle (ps:ps) (x0:int) (y0:int) (color:color) rects =
        List.iter (fun a ->
            let x,y,w,h = a in
            fillRectangle ps x0 y0 color
                          x y w h;
        ) rects

(*
** Create an Arc.
** x0,y0: current X coordinates.
** x1,y1: upper left corner relative to (x0,y0)
** a,b : start and rotation angle 
*)
let arc (ps:ps) (x0:int) (y0:int) (lw:float) (color:color) 
            (x1:float) (y1:float) (a:float) (b:float) (d:float) =

    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
    out "newpath";
    
    out (sprintf "%.1f setlinewidth" lw);
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in

    let fx0,fy0=(i2f x0),(i2f y0) in
    let fr = d/2.0 in
    let fa = a in
    let fb = fa + b in
    let xc,yc = x1+fr,y1+fr in
    let fxc,fyc = (xc+fx0), (yc+fy0) in
    out (sprintf "%.1f %.1f %.1f %.1f %.1f arcn" 
            (C.x2p fxc) (C.y2p fyc)
            (C.s2p (fr-lw/2.0))  
            (C.s2p fa) (C.s2p fb)
        );
    if within_bbox (List.hd !clip_bbox) (f2i x1) (f2i y1) then
        expand_bbox ps.ps_bbox (f2i x1) (f2i y1);
    if within_bbox (List.hd !clip_bbox) (f2i (x1+2.0*fr)) (f2i (y1+2.0*fr)) then
        expand_bbox ps.ps_bbox (f2i (x1+2.0*fr)) (f2i (y1+2.0*fr));
    out "stroke";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"

(*
** Create a filled Arc.
** x0,y0: current X coordinates.
** x1,y1: upper left corner relative to (x0,y0)
** a,b : start and rotation angle 
*)

let fillArc (ps:ps) (x0:int) (y0:int) (color:color) 
            (x1:float) (y1:float) (a:float) (b:float) (d:float) =

    let out str = ps.ps_cont <- ps.ps_cont @ [str]  in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
    let lw = 1.0 in
    out "newpath";
    out (sprintf "%.1f setlinewidth" lw);
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in

    let fx0,fy0=(i2f x0),(i2f y0) in
    let fr = d/2.0 in
    let fa = a in
    let fb = fa + b in
    let xc,yc = x1+fr,y1+fr in
    let fxc,fyc = (xc+fx0), (yc+fy0) in
    out (sprintf "%.1f %.1f %.1f %.1f %.1f arcn" 
            (C.x2p fxc) (C.y2p fyc)
            (C.s2p (fr))  
            (C.s2p fa) (C.s2p fb)
        );
    out (sprintf "%.1f %1.f lineto" (C.x2p fxc) (C.y2p fyc));

    if within_bbox (List.hd !clip_bbox) (f2i x1) (f2i y1) then
        expand_bbox ps.ps_bbox (f2i x1) (f2i y1);
    if within_bbox (List.hd !clip_bbox) (f2i (x1+2.0*fr)) (f2i (y1+2.0*fr)) then
        expand_bbox ps.ps_bbox (f2i (x1+2.0*fr)) (f2i (y1+2.0*fr));

    out "closepath";
    out "fill";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"



let fmin a b = if a < b then a else b

let polyArc (ps:ps) (x0:int) (y0:int) (lw:float) (color:color) 
                    arcs =
    List.iter (fun a ->
            let x1,y1,d1,d2,a,b = a in
            arc ps x0 y0 lw color 
                x1 y1 a b (fmin d1 d2);
        ) arcs

let polyFillArc (ps:ps) (x0:int) (y0:int) (color:color) 
                arcs =
    List.iter (fun a ->
            let x1,y1,d1,d2,a,b = a in
            fillArc ps x0 y0 color 
                x1 y1 a b (fmin d1 d2);
        ) arcs


(*
** Above Arc version can only handle non elliptic circles, but with
** arbitary start and end angles. The following functions can
** handle elliptic full circles.
*)

(*
** Create a Circle.
** x0,y0: current X coordinates.
** x1,y1: upper left corner relative to (x0,y0)
** d1,d2: width and height
*)
let circle (ps:ps) (x0:int) (y0:int) (lw:float) (color:color) 
            (x1:float) (y1:float) (d1:float) (d2:float) =


    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
    out "newpath";
    let scale = fmax d1 d2 in
    let lw' = lw / scale in
    out (sprintf "%.1f setlinewidth" lw');
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in

    let fx0,fy0=(i2f x0),(i2f y0) in
    let frx = d1/2.0 in
    let fry = d2/2.0 in
    let xc,yc = x1+frx,y1+fry in
    let fxc,fyc = (xc+fx0), (yc+fy0) in

    out (sprintf "%.1f %.1f translate" 
            (C.x2p fxc) (C.y2p fyc));
    out (sprintf "%.1f %.1f scale"
            (C.s2p (frx)) (C.s2p (fry))
            );
    out (sprintf "%.1f %.1f %.1f %.1f %.1f arcn" 
            0.0 0.0
            1.0  
            0.0 (-360.0)
        );
    if within_bbox (List.hd !clip_bbox) (f2i x1) (f2i y1) then
        expand_bbox ps.ps_bbox (f2i x1) (f2i y1);
    if within_bbox (List.hd !clip_bbox) (f2i (x1+2.0*frx)) (f2i (y1+2.0*fry)) then
        expand_bbox ps.ps_bbox (f2i (x1+2.0*frx)) (f2i (y1+2.0*fry));
    out "stroke";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"

(*
** Create a filled Arc.
** x0,y0: current X coordinates.
** x1,y1: upper left corner relative to (x0,y0)
** d1,d2: width and height
*)

let fillCircle (ps:ps) (x0:int) (y0:int) (color:color) 
            (x1:float) (y1:float) (d1:float) (d2:float) =


    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f color.c_red) /. (i2f div),
                   (i2f color.c_green) /. (i2f div),
                   (i2f color.c_blue) /. (i2f div) in
    out "newpath";
    let scale = fmax d1 d2 in
    let lw' = 1.0 / scale in
    out (sprintf "%.1f setlinewidth" lw');
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);
    let first = ref true in

    let fx0,fy0=(i2f x0),(i2f y0) in
    let frx = d1/2.0 in
    let fry = d2/2.0 in
    let xc,yc = x1+frx,y1+fry in
    let fxc,fyc = (xc+fx0), (yc+fy0) in

    out (sprintf "%.1f %.1f translate" 
            (C.x2p fxc) (C.y2p fyc));
    out (sprintf "%.1f %.1f scale"
            (C.s2p (frx)) (C.s2p (fry))
            );
    out (sprintf "%.1f %.1f %.1f %.1f %.1f arcn" 
            0.0 0.0
            1.0  
            0.0 (-360.0)
        );
    if within_bbox (List.hd !clip_bbox) (f2i x1) (f2i y1) then
        expand_bbox ps.ps_bbox (f2i x1) (f2i y1);
    if within_bbox (List.hd !clip_bbox) (f2i (x1+2.0*frx)) (f2i (y1+2.0*fry)) then
        expand_bbox ps.ps_bbox (f2i (x1+2.0*frx)) (f2i (y1+2.0*fry));
    out "fill";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"



let polyCircle (ps:ps) (x0:int) (y0:int) (lw:float) (color:color) 
                    arcs =
    List.iter (fun a ->
            let x1,y1,d1,d2,a,b = a in
            let da = a - b in
            if (abs_float da) <> 360.0 then
                vx_error "VX_ps:polyCircle: invalid angles";
            circle ps x0 y0 lw color 
                x1 y1 d1 d2;
        ) arcs

let polyFillCircle (ps:ps) (x0:int) (y0:int) (color:color) 
                arcs =
    List.iter (fun a ->
            let x1,y1,d1,d2,a,b = a in
            let da = a - b in
            if (abs_float da) <> 360.0 then
                vx_error "VX_ps:polyFillCircle: invalid angles";

            fillCircle ps x0 y0 color 
                x1 y1 d1 d2;
        ) arcs


(*
** Print a text string.
*)

(*   
** Return nearest avaiable size (must be kept consistent with font_name sizing)
** with fine tuned scaling.
*)
let font_size size =
    match size with
    | a when a>=0 && a<=8 -> 8.25;
    | a when a>=9 && a<=10 -> 10.3;
    | 11 -> 10.3;       (* ? *)
    | 12 -> 11.7;
    | a when a>=13 && a<=14 -> 14.0;
    | a when a>=15 && a<=18 -> 18.5;
    | a when a>=19 && a<=20 -> 19.0;
    | a when a>=21 && a<=24 -> 25.0;
    | _ -> 25.0


let print_text     (ps:ps)  (x0:int) (y0:int) 
                   (tx:float) (ty:float) 
                   (fg:color) (bg:color) 
                   (text_font:font) 
                   (text_str:string) =
    let div = 256*256 - 1 in
    let rf,gf,bf = (i2f fg.c_red) /. (i2f div),
                   (i2f fg.c_green) /. (i2f div),
                   (i2f fg.c_blue) /. (i2f div) in
    (*
    ** fine tuning
    *)

    (*
    ** Create postscript font name
    *)
    let ps_font name style =
        match name with
        | Times ->
        begin
            match style with
            | Roman
            | NoStyle -> "Times-Roman";
            | Bold -> "Times-Bold";
            | Italic -> "Times-Italic";
        end
        | NoFont
        | Helvetica ->
        begin
            match style with
            | Roman
            | NoStyle -> "Helvetica-Roman";
            | Bold -> "Helvetica-Bold";
            | Italic -> "Helvetica-Italic";
        end
        | Fixed 
        | Courier ->
        begin
            match style with
            | Roman
            | NoStyle -> "Courier-Roman";
            | Bold -> "Courier-Bold";
            | Italic -> "Courier-Italic";
        end
        | Symbol ->
        begin
            match style with
            | Roman
            | NoStyle -> "Symbol-Roman";
            | Bold -> "Symbol-Bold";
            | Italic -> "Symbol-Italic";
        end
        in
    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    out (sprintf "/%s findfont %.1f scalefont setfont"
                 (ps_font text_font.font_class text_font.font_style)    
                 (C.s2p (font_size text_font.font_size)));
    let fx0,fy0 = i2f x0,i2f y0 in
    
    out "newpath";
    out (sprintf "%.2f %.2f %.2f setrgbcolor" rf gf bf);


    let ftx,fty = fx0 + tx,
                  fy0 + ty in

    out (sprintf "%.1f %.1f moveto"
                 (C.x2p ftx) (C.y2p fty));
    out (sprintf "(%s)" text_str);
    out "show";
    (*
    ** restore color state
    *)
    out "1 1 1 setrgbcolor"


(*
** Enable and disable clipping boxes.
*)
let enable_clipping (ps:ps)  
                    (x0:int) (y0:int) 
                    (tx:float) (ty:float) 
                    (dx:float) (dy:float) =

    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    out "gsave";
    out "newpath";
    let fx0,fy0 = i2f x0,i2f y0 in
    let ftx,fty = fx0 + tx,fy0 + ty in
    out (sprintf "%.1f %.1f moveto"
                 (C.x2p ftx) (C.y2p fty));
    let lines = [tx+dx,ty;
                 tx+dx,ty+dy;
                 tx,ty+dy;
                 tx,ty] in
    List.iter (fun (x,y) ->
        let fx,fy = fx0 + x,
                    fy0 + y in 
        out (sprintf "%.1f %.1f lineto" (C.x2p fx) (C.y2p fy));
        ) lines;
    let cb = bbox_of_xywh (f2i (fx0+tx)) (f2i (fy0+ty)) 
                          (f2i dx) (f2i dy) in
    let cb_cur = List.hd !clip_bbox in
    if within_bbox cb_cur cb.x1 cb.y1 && 
       within_bbox cb_cur cb.x2 cb.y2 then
        clip_bbox := cb :: !clip_bbox
    else
        clip_bbox := cb_cur :: !clip_bbox;

    out "gsave stroke grestore clip"


let disable_clipping (ps:ps)  
                    (x0:int) (y0:int) =

    let out str = ps.ps_cont <- ps.ps_cont @ [str] in
    clip_bbox := List.tl !clip_bbox;
    out "grestore"


(*
** Print content of specified container and all his childs to the
** file specified. The image is scaled to desired paper size (including
** reasonable margins). Width and height in millimeters.
**
** Default scaling: 1 X pixel unit = 1/72 inch = 1 PS pixel unit
**
** If the image is larger than the page size, the image is scaled to
** maximal boundings.
*)

let print_ps filename format contained =
    let width,height = paper_size format in
    let ps = {
            ps_bbox = bbox_of_xywh 0 0 0 0;
            ps_cont = [];
            ps_file_name=filename;
            ps_print_bbox = bbox_of_xywh 0 0 0 0;
            ps_scale = 1.0;
        } in
    contained#print ps 0 0;
    (*
    ** Center the image
    *)
    let x0,y0,w,h = bbox_to_xywh ps.ps_bbox in

    let fw = i2f w in
    let fh = i2f h in
    let fwidth = i2f width in
    let fheight = i2f height in
    if fw > (C.m2p fwidth) ||
       fh > (C.m2p fheight) then
    begin
        (*
        ** Change scaling
        *)
        let scale_x = (C.m2p fwidth) /. fw in
        let scale_y = (C.m2p fheight) /. fh in
        ps.ps_scale <- fmax scale_x scale_y;
    end;

    let fxt1 = (C.m2p (i2f width)) /. 2.0 in
    let fyt1 = (C.m2p (i2f height)) /. 2.0 in

    let fxt2 = -. (C.s2p (i2f x0)) -. 
              (C.s2p (i2f w)) /. 2.0 in
    let fyt2 = -. (C.y2p (i2f (y0 + h))) -. 
              (C.s2p (i2f h)) /. 2.0  in

    let oc = open_out filename in 
    let out str = output_string oc (str ^ "\n") in

    out "%!PS-Adobe-2.0";
    out "%%Creator: VAM (C) 2005 BSSLAB Dr. Stefan Bosse";
    out "%%EndComments";
    (*
    ** There is only one page!
    *)
    out "%%Page: 1 1";
    out (sprintf "%.1f %.1f translate" fxt1 fyt1);
    out (sprintf "%.1f %.1f scale" ps.ps_scale ps.ps_scale);
    out (sprintf "%.1f %.1f translate" fxt2 fyt2);
    List.iter (fun str -> output_string oc str;
                          output_string oc "\n";
        ) ps.ps_cont;
    out "showpage";
    out "%%Trailer";
    out "%%EOF";
    close_out oc
    
(*
** Print instead an encapsulated PS file. The scaling factor must be
** provided. Default: 1.0
*)

let print_eps filename scale contained =
    let ps = {
            ps_bbox = bbox_of_xywh 0 0 0 0;
            ps_cont = [];
            ps_file_name=filename;
            ps_print_bbox = bbox_of_xywh 0 0 0 0;
            ps_scale = scale;
        } in
    contained#print ps 0 0;

    let x0,y0,w,h = bbox_to_xywh ps.ps_bbox in

    (*
    ** Give some additonal pad space around the image (outline border 
    ** not linewidth adjusted inside the ps.ps_bbox!).
    *)

    let fxt = -. (C.s2p (i2f x0)) +. 5.0 in
    let fyt = -. (C.y2p (i2f (y0 + h))) +. 5.0 in

    let bx0,by0,bw,bh = 0.0,0.0,
                        ps.ps_scale *. ((C.s2p (i2f w))+.10.0),
                        ps.ps_scale *. ((C.s2p (i2f h))+.10.0) in

    let oc = open_out filename in 
    let out str = output_string oc (str ^ "\n") in

    out "%!PS-Adobe-2.0";
    out "%%Creator: VAM (C) 2005 BSSLAB Dr. Stefan Bosse";
    out (sprintf "%%%%BoundingBox: %d %d %d %d" (f2i bx0) (f2i by0) 
                                                (f2i bw) (f2i bh));
    out "%%EndComments";
    (*
    ** There is only one page!
    *)
    out (sprintf "%.1f %.1f scale" ps.ps_scale ps.ps_scale);
    out (sprintf "%.1f %.1f translate" fxt fyt);
    List.iter (fun str -> output_string oc str;
                          output_string oc "\n";
        ) ps.ps_cont;
    out "%%Trailer";
    out "%%EOF";
    close_out oc
    
