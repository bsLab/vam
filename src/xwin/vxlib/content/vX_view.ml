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
**    $AUTHORS:     Stefan Bosse, based on work from Frabice Le Fessant (Inria)
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     1.7.2005
**    $VERSION:     1.15
**
**    $INFO:
**  
**  Viewport container: display only a part of a child widget (clipped
**  window region).
**  The visbile part can be controlled with sliders, for example.
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common

(*
** The basic viewport doing clipping and hiding the size of the
** viewport wob.
*)

class t parent attributes = 
  object (self)
  inherit VX_object.t parent attributes as super

  initializer
        self#set_name "viewport";

  (*
  ** Current origin and size of child wob
  *)
  val view = { x= 0; y = 0; width = 1; height = 1; border=0;}
  val mutable child = None
  val mutable init = false

  (*
  ** Kick the child widget to display
  *)
  method realize =
    super#realize;
    match child with 
    | None -> ()
    | Some wob -> wob#realize

  (*
  ** Calculate size of the view window. 
  *)
  method size_request = 
Db.Pr.s 10 "VX_view:size_request";

    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz
      else
      begin

        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_width <- max sz.requested_width sz.min_width;
        sz.requested_height <- max sz.requested_height sz.min_height;
        (
            match child with 
            | None -> ()
            | Some wob -> 
                let csz = wob#size_request in
                let wg = w.w_geometry in
                let frame = frame_size w.w_frame in
                let width =  (max csz.requested_width csz.min_width) + 
                              frame + 2 * w.w_ipad_x in
                let height = (max csz.requested_height csz.min_height) + 
                              frame + 2 * w.w_ipad_y in
                if not sz.expand_x then
                    sz.requested_width <- max sz.requested_width width;
                if not sz.expand_y then
                    sz.requested_height <- max sz.requested_height height;
        );
        sz.requested_height <- min sz.requested_height sz.max_height;
        sz.requested_width <- min sz.requested_width sz.max_width;

Db.Pr.sdd 10 "VX_view#size_request" sz.requested_width sz.requested_height;
        sz
      end

  method show =
    super#show;
    match child with None -> ()
    | Some wob -> wob#show

  method get_wob =
    child

  method hide =
        (match child with
        | Some w -> w#hide;
        | None -> ();
        );
        super#hide;

  method destroy =
        (match child with
        | Some w -> w#destroy;
        | None -> ();
        );
        super#destroy; 
    

  method size_allocate x y dx dy =
Db.Pr.sdd 10 "VX_view#size_allocate" dx dy;
    let g = w.w_geometry in    
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    w.w_size_modified <- false;
    __(super#size_allocate x y dx dy);
    if modified then
        self#update_top_size;
    true

  method container_add (wob : contained) =
Db.Pr.s 10 "VX_view#container_add" ;
    wob#set_parent self#container;
    let win' = wob#win_request in
    win'.w_clipped <- true; 
    child <- Some (if not (w.w_window == noWindow) 
                    then (wob#realize; wob) 
                    else  wob);
    self#update_top_size

  (*
  ** Calculate size of view field and propagate it to the wob.
  *)
  method update_top_size =
Db.Pr.s 10 "VX_view#update_top_size";
    match child with 
    | None -> ()
    | Some wob -> 
        let sz = szhints in
        let csz = wob#size_request in
        let g = w.w_geometry in
        let frame,frame_off = frame_size w.w_frame,
                              frame_offset w.w_frame in

        let width = (max (max csz.requested_width csz.min_width)
                         (if  csz.expand_x 
                            then g.width - frame - 2 * w.w_ipad_x 
                            else 0))
                    + frame in
        let height = (max (max csz.requested_height csz.min_height) 
                           (if csz.expand_y 
                            then g.height - frame - 2 * w.w_ipad_y 
                            else 0))
                    + frame in
        view.width <- width;
        view.height <- height;
        self#size_allocate_childs


  (*
  ** Reconfigure the visible part of the child widget. The child
  ** gets his new origin and the new size.
  *)
  method size_allocate_childs =
Db.Pr.s 10 "VX_view#size_allocate_childs";
    let accepted = ref true in
    let g = w.w_geometry in
    match child with 
    | None -> ()
    | Some wob -> 
        let frame,frame_off = frame_size w.w_frame,
                              frame_offset w.w_frame in
        let sz = wob#size_request in
        accepted := wob#size_allocate
          (-view.x + w.w_ipad_x + frame_off)
          (-view.y + w.w_ipad_y + frame_off)
          (view.width  - 2 * w.w_ipad_x - frame) 
          (view.height - 2 * w.w_ipad_y - frame);
        (*
        ** Update wobs clipping region, too
        *)
        let wc = g.width - 2 * w.w_ipad_x - frame in
        let hc = g.height - 2 * w.w_ipad_y - frame in
        let win' = wob#win_request in
        let v = win'.w_clipping in
        v.x <- view.x;
        v.y <- view.y;
        v.width <- wc;
        v.height <- hc;
Db.Pr.sdd 10 "VX_view#size_allocate_childs w_clipping x/y" v.x v.y;
Db.Pr.sdd 10 "VX_view#size_allocate_childs w_clipping w/h" v.width v.height;

        (*
        ** Maybe clipping must be inherited to child widgets...
        *)
        wob#update_clipping;

  (*
  ** Change origin of child widget inside view container.
  *)
  method update_x x =
    view.x <- x;
    self#size_allocate_childs

  method update_y y =
    view.y <- y;
    self#size_allocate_childs

  method refresh = 
        if s.s_timestamp > w.w_refresh_timestamp && 
           not (w.w_window == noWindow) then
        begin
Db.Pr.s 10 "VX_view#refresh";
            if not init then
            begin
                self#size_allocate_childs;
                init <- true;
            end;
        end

  (*
  ** Propagate update request up to parent!
  *)
  method update =
Db.Pr.s (10) "VX_view#update";
        super#update; 
        self#wait_resize;
        self#wait_refresh true 0 0 0 0;
        parent#update  

  method configure attrs =
        let remains = super#configure attrs in
        List.iter (fun attr ->
            match attr with
            | Widget w -> self#container_add w;
            | _ -> warn_attr "VX_view.t" attr self#name;
            ) remains;
        []

    (*
    ** Postscript printing
    *)

    method print ps wx0 wy0 =  
        let g = w.w_geometry in
        let frame,frame_off =
            frame_size w.w_frame,
            frame_offset w.w_frame in
        let vx0 = frame_off + w.w_ipad_x in
        let vy0 = frame_off + w.w_ipad_y in
        let vw = g.width - frame - 2 * w.w_ipad_x in
        let vh = g.height - frame - 2 * w.w_ipad_y in
        super#print ps wx0 wy0;
        match child with
        | Some v -> 
            let wx0',wy0' =
                wx0 - view.x,
                wy0 - view.y in
            Db.Pr.sdd 0 "print view_wob" wx0' wy0';
            VX_ps.enable_clipping ps wx0 wy0 
                                  (i2f vx0) (i2f vy0)
                                  (i2f vw)  (i2f vh);
                                  
            v#print ps wx0' wy0'; 
            VX_ps.disable_clipping ps wx0 wy0;
        | None -> ()


end

(*
** Viewports with sliders. This code is slightly confused due to the
** necessity of slider updates. The simple case: the user moves a
** slider, and the viewport must be updated. The more complicated case:
** the content of the viewport was resized and the slider parameters
** must be updated.
*)

(*
** Viewport with one vertical slider
*)
class v parent attributes =
    object (self)

    inherit VX_box.h parent attributes as super

    val mutable slider_y = None
    val mutable viewport = None
    val mutable view_wob = None
    val mutable slider_size = 12

    val mutable init = false
    initializer 
        let slattr = ref [] in
        List.iter (fun attr ->
            match attr with
            | But battrs -> slattr := !slattr @ [attr];
                       List.iter (fun attr ->
                        match attr with
                        | Size n -> slider_size <- n;
                        | _ -> ();
                        ) battrs; 
            | _ -> ();
            ) attributes;
        let vattr = ref [] in
        let frame = frame_size w.w_frame in
        let w' x = x-slider_size-
                   frame - 
                   3 * w.w_ipad_x in
        let h' y = y-
                   frame - 
                   2 * w.w_ipad_y in

        (*
        ** Adjust size settings for viewport
        *)
        List.iter (fun attr ->
            match attr with
            | Width wd -> vattr := !vattr @ [Width (w' wd)];
            | Height hg -> vattr := !vattr @ [Height (h' hg)];
            | MinWidth wd -> vattr := !vattr @ [MinWidth (w' wd)];
            | MinHeight hg -> vattr := !vattr @ [MinHeight (h' hg)];
            | MaxWidth wd -> vattr := !vattr @ [MaxWidth (w' wd)];
            | MaxHeight hg -> vattr := !vattr @ [MaxHeight (h' hg)];
            | _ -> ()
            ) attributes;

        let vp = new t self#container ([ExpandX true;
                                       ExpandY true]@ !vattr)  in
        self#container_add vp#contained;
        viewport <- Some vp;
        let objsize = 100 in
        let viewsize = 100 in
        let pagesize = 100 in
        let sl = new VX_slider.view_v self#container
                                      objsize
                                      viewsize
                                      pagesize 
                                      ([Width slider_size;
                                        ExpandY true;
                                        ActionIU (fun newpos ->
                                             self#update_y newpos)]@
                                       !slattr) in
        slider_y <- Some sl;
        self#container_add sl#contained;

        init <- true

    method update_y newpos =
        match viewport with
        | Some v ->
        begin
Db.Pr.sd 10 "update_y pagepos" newpos;
            v#update_y newpos;
        end;
        | None -> ()

    (*
    ** Update viewport and slider positions
    *)
    method adjust_y newpos =
        self#update_y newpos;
        match slider_y with
        | Some sl ->
        begin
Db.Pr.sd 10 "adjust_y pagepos" newpos;
            sl#set_pagepos newpos;
        end;
        | None -> ()


    method update_slider_y =
        if viewport <> None && view_wob <> None then
        begin
            let v = get_some viewport in
            let wob = get_some view_wob in
            (*
            ** Adjust slider
            *)
            let vw = v#win_request in
            let csz = wob#size_request in
            match slider_y with
            | Some sl ->
                    let oldpos = sl#get_pos in
                    let viewsize = vw.w_geometry.height in
                    let objsize = max viewsize csz.requested_height in
                    let pagesize = ((viewsize * 9)/10) in

                    sl#set_params objsize
                                  viewsize
                                  pagesize;
                    (*
                    ** In the case the slider was previously at the
                    ** end of the old viewport, set it again to the
                    ** end.
                    *)
                    if oldpos > 0.99 then
                    begin
                        self#update_y (objsize-viewsize);      
                        sl#set_pos 1.0;
                    end;
            | None -> ();
        end;            

    method container_add wob =
        if not init then
            super#container_add wob
        else
        begin
            match viewport with
            | Some v -> 
            begin
                v#container_add wob;
                view_wob <- Some wob;
                self#update_slider_y;
            end;            
            | None  -> ()
        end    

    (*
    ** Update slider, too.
    *)
    method update =
        super#update;
        self#update_slider_y

    method size_allocate  x y dx dy =
        let g = w.w_geometry in
        let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
        __(super#size_allocate x y dx dy);
        if modified then
        begin
            self#update_slider_y;
        end;    
        true

    method name = "viewport.slider_y"

end

class h parent attributes =
    object (self)

    inherit VX_box.v parent attributes as super

    val mutable slider_x = None
    val mutable viewport = None
    val mutable view_wob = None
    val mutable slider_size = 12

    val mutable init = false
    initializer 
        let slattr = ref [] in
        List.iter (fun attr ->
            match attr with
            | But battrs -> slattr := !slattr @ [attr];
                       List.iter (fun attr ->
                        match attr with
                        | Size n -> slider_size <- n;
                        | _ -> ();
                        ) battrs; 
            | _ -> ();
            ) attributes;
        let vattr = ref [] in
        let frame = frame_size w.w_frame in
        let w' x = x-
                   frame - 
                   2 * w.w_ipad_x in
        let h' y = y-slider_size-
                   frame - 
                   3 * w.w_ipad_y in

        (*
        ** Adjust size settings for viewport
        *)
        List.iter (fun attr ->
            match attr with
            | ExpandX _ -> vattr := !vattr @ [attr];
            | ExpandY _ -> vattr := !vattr @ [attr];
            | Width wd -> vattr := !vattr @ [Width (w' wd)];
            | Height hg -> vattr := !vattr @ [Height (h' hg)];
            | MinWidth wd -> vattr := !vattr @ [MinWidth (w' wd)];
            | MinHeight hg -> vattr := !vattr @ [MinHeight (h' hg)];
            | MaxWidth wd -> vattr := !vattr @ [MaxWidth (w' wd)];
            | MaxHeight hg -> vattr := !vattr @ [MaxHeight (h' hg)];
            | _ -> ()
            ) attributes;

        let vp = new t self#container ([ExpandX true; 
                                        ExpandY true]@ !vattr)  in
        self#container_add vp#contained;
        viewport <- Some vp;
        let objsize = 100 in
        let viewsize = 100 in
        let pagesize = 100 in
        let sl = new VX_slider.view_h self#container
                                      objsize
                                      viewsize
                                      pagesize 
                                      ([Height slider_size;
                                        ExpandX true;
                                        ActionIU (fun newpos ->   
                                             self#update_x newpos)]@
                                        !slattr) in
        slider_x <- Some sl;
        self#container_add sl#contained;

        init <- true

    method container_add wob =
        if not init then
            super#container_add wob
        else
        begin
            match viewport with
            | Some v -> 
            begin
                v#container_add wob;
                view_wob <- Some wob;
                self#update_slider_x;
            end;            
            | None  -> ()
        end    

    (*
    ** Update viewport and slider positions
    *)

    method update_x newpos =
        match viewport with
        | Some v ->
        begin
            v#update_x newpos;
        end;
        | None -> ()

    (*
    ** Update viewport and slider positions
    *)
    method adjust_x newpos =
        self#update_x newpos;
        match slider_x with
        | Some sl ->
        begin
            sl#set_pagepos newpos;
        end;
        | None -> ()


    method update_slider_x =
        if viewport <> None && view_wob <> None then
        begin
            let v = get_some viewport in
            let wob = get_some view_wob in
            (*
            ** Adjust slider
            *)
            let vw = v#win_request in
            let csz = wob#size_request in
            match slider_x with
            | Some sl ->
                    let viewsize = vw.w_geometry.width in
                    let objsize = max viewsize csz.requested_width in
                    let pagesize = ((viewsize * 9)/10) in
                    sl#set_params objsize
                                  viewsize
                                  pagesize;
                          
            | None -> ();
        end;            

    (*
    ** Update slider, too.
    *)
    method update =
        super#update;
        self#update_slider_x

    method size_allocate  x y dx dy =
        let g = w.w_geometry in
        let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
        __(super#size_allocate x y dx dy);
        if modified then
        begin
            self#update_slider_x;
        end;    
        true

    method name = "viewport.slider_x"
end

(*
** Modified hbox class with upward propagation of update events.
*)
class hbox parent attributes =
    object (self)
    inherit VX_box.h parent attributes as super

    method update =
        super#update;
        parent#update;
end

class hv parent attributes =
    object (self)

    inherit VX_box.v parent (attributes @ [IpadX 2; IpadY 2]) as super

    val mutable slider_x = None
    val mutable slider_y = None
    val mutable edge_fill = None
    val mutable viewport = None
    val mutable view_wob = None
    val mutable slider_size = 12

    val mutable init = false
    initializer 
        let pad_x,pad_y = ref 2, ref 2 in
        (*
        ** Filter attributes for each sub widget
        *)
        let hattr = ref [] in
        let vattr = ref [] in
        let slattr = ref [] in

        List.iter (fun attr ->
            match attr with
            | ExpandX _ -> hattr := !hattr @ [attr];
            | ExpandY _ -> hattr := !hattr @ [attr];
            | IpadX x -> pad_x := x; hattr := !hattr @ [attr];
            | IpadY y -> pad_y := y; hattr := !hattr @ [attr];
            | But battrs -> slattr := !slattr @ [attr];
                       List.iter (fun attr ->
                        match attr with
                        | Size n -> slider_size <- n;
                        | _ -> ();
                        ) battrs; 
            | _ -> ();
            ) attributes;

        let frame = frame_size w.w_frame in
        let w' x = x-slider_size-
                   frame - 
                   2 * w.w_ipad_x - 
                   3 * !pad_x in
        let h' y = y-slider_size-
                   frame - 
                   3 * w.w_ipad_y -
                   3 * !pad_y in

        
        List.iter (fun attr ->
            match attr with
            | ExpandY _ -> vattr := !vattr @ [attr];
            | Width wd -> vattr := !vattr @ [Width (w' wd)];
            | Height hg -> vattr := !vattr @ [Height (h' hg)];
            | MinWidth wd -> vattr := !vattr @ [MinWidth (w' wd)];
            | MinHeight hg -> vattr := !vattr @ [MinHeight (h' hg)];
            | MaxWidth wd -> vattr := !vattr @ [MaxWidth (w' wd)];
            | MaxHeight hg -> vattr := !vattr @ [MaxHeight (h' hg)];
(*            | Border _ -> vattr := !vattr @ [attr]; *)
            | _ -> ()
            ) attributes;

        let hbox1 = new hbox self#container ([(* Background "red" *)
                                             ]@ !hattr @
                                                 []) in

        let vp = new t hbox1#container ([ExpandX true; 
                                         ExpandY true
                                        ]@ !vattr @
                                        [(* Background "green" *)])  in
        hbox1#container_add vp#contained;
        viewport <- Some vp;
        let objsize = 100 in
        let viewsize = 100 in
        let pagesize = 100 in
        let sl = new VX_slider.view_v hbox1#container
                                      objsize
                                      viewsize
                                      pagesize 
                                      ([Width slider_size;
                                        ExpandX false;
                                        ExpandY true;
                                        ActionIU (fun newpos ->
                                             self#update_y newpos);
                                       (* Background "green" *)]@ 
                                        !slattr)
                in
        slider_y <- Some sl;
        hbox1#container_add sl#contained;

        let hbox2 = new hbox self#container ([(* Background "yellow" *)
                                              ExpandX true] @
                                              [IpadY 0;
                                               IpadX !pad_x]) in
        (*
        ** Only initial setting
        *)
        let objsize = 100 in
        let viewsize = 100 in
        let pagesize = 100 in
        let sl = new VX_slider.view_h hbox2#container
                                      objsize
                                      viewsize
                                      pagesize 
                                      ([Height slider_size;
                                        ExpandX true;
                                        ExpandY true;
                                        ActionIU (fun newpos ->
                                             self#update_x newpos);
                                       (* Background "green" *)]@
                                        !slattr) in
        slider_x <- Some sl;
        let ef = new VX_box.h hbox2#container [Width (slider_size- !pad_x);
                                               Height slider_size;
                                               ] in
        edge_fill <- Some ef;
        hbox2#container_add_s [sl#contained;ef#contained];

        (*
        ** final filling after h slider
        *)
        let hf = new VX_box.h self#container [Height !pad_y;IpadY 0] in

        self#container_add_s [hbox1#contained;
                              hbox2#contained;
                              hf#contained];
        init <- true

    method container_add wob =
        if not init then
            super#container_add wob
        else
        begin
            match viewport with
            | Some v -> 
            begin
                v#container_add wob;
                view_wob <- Some wob;
                self#update_slider_x;
                self#update_slider_y;
            end;            
            | None  -> ()
        end    

    method update_x newpos =
        match viewport with
        | Some v ->
        begin
            v#update_x newpos;
        end;
        | None -> ()

    method update_y newpos =
        match viewport with
        | Some v ->
        begin
            v#update_y newpos;
        end;
        | None -> ()


    (*
    ** Update viewport and slider positions
    *)
    method adjust_x newpos =
        self#update_x newpos;
        match slider_x with
        | Some sl ->
        begin
            sl#set_pagepos newpos;
        end;
        | None -> ()

    method adjust_y newpos =
        self#update_y newpos;
        match slider_y with
        | Some sl ->
        begin
            sl#set_pagepos newpos;
        end;
        | None -> ()


    method update_slider_x =
        if viewport <> None && view_wob <> None then
        begin
            let v = get_some viewport in
            let wob = get_some view_wob in
            (*
            ** Adjust slider
            *)
            let vw = v#win_request in
            let csz = wob#size_request in
            match slider_x with
            | Some sl ->
                    let viewsize = vw.w_geometry.width in
                    let objsize = max viewsize csz.requested_width in
                    let pagesize = ((viewsize * 9)/10) in

                    sl#set_params objsize
                                  viewsize
                                  pagesize;
                          
            | None -> ();
        end;            

    method update_slider_y =
        if viewport <> None && view_wob <> None then
        begin
            let v = get_some viewport in
            let wob = get_some view_wob in
            (*
            ** Adjust slider
            *)
            let vw = v#win_request in
            let csz = wob#size_request in
            match slider_y with
            | Some sl ->
                    let oldpos = sl#get_pos in
                    let viewsize = vw.w_geometry.height in
                    let objsize = max viewsize csz.requested_height in
                    let pagesize = ((viewsize * 9)/10) in
                    sl#set_params objsize
                                  viewsize
                                  pagesize;
                          
                    (*
                    ** In the case the slider was previously at the
                    ** end of the old viewport, set it again to the
                    ** end.
                    *)
                    if oldpos > 0.99 then
                    begin
                        self#update_y (objsize-viewsize);      
                        sl#set_pos 1.0;
                    end;
            | None -> ();
        end;            

    (*
    ** Update slider, too.
    *)
    method update =
        super#update;
        self#update_slider_x;
        self#update_slider_y;

    method size_allocate  x y dx dy =
        let g = w.w_geometry in
        let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
        __(super#size_allocate x y dx dy);
        if modified then
        begin
            self#update_slider_x;
            self#update_slider_y;
        end;    
        true

    (*
    ** Postscript printing
    *)
    method print ps wx0 wy0 =  
        super#print ps wx0 wy0;

    method name = "viewport.slider_hv"
end

 