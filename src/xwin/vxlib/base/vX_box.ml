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
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Fabrice Le Fessant, 
**                  Stefan Bosse
**    $INITIAL:     Derived from wXlib / INRIA, 2005 BSSLAB
**    $CREATED:     11.5.2005
**    $VERSION:     1.19
**
**    $INFO:
**
**  VXlib box container management. All wideget are arranged in
**  box containers of either horizontal or vertial alignment. Both types
**  of box containers can be mixed. Similar to TeX boxes.
**
**  +------------------+ <- frame
**  |                  |
**  +------------------+
**  
**  The usable area inside of each box is reduced due to frame border
**  (Flat line, Shadow, Relief) and pad space. The frame border grows 
**  inside!
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common
open Printf
open Debug

let dl = 0

let badness = 5

let warn str name =
    if !vx_warn then
    begin
        print_string ("Warning ["^name^"]: "^str);
        print_newline ()
    end

(*
** Common box class
*)

class orig sens parent attributes =
    object (self)
    
    inherit VX_object.t parent ([IpadX 2;IpadY 2]@attributes) as super
    
    (*
    ** All the widget objects packed in the box container.
    *)
    val mutable wobs = [||]

    val mutable sens = sens
    
    method iter f = Array.iter f wobs
    
    method size_request =
        let tsz = szhints in
        if not w.w_size_modified || 
           tsz.comp_timestamp = s.s_timestamp 
            then 
        begin
            debug dl "VX_box.size_request: %s dx=%d dy=%d\n"
                     self#name
                     tsz.requested_width tsz.requested_height;

            tsz 
        end
        else
        begin
            tsz.comp_timestamp <- s.s_timestamp;
            Array.iter (fun wob -> let _ = wob#size_request in ()) wobs;
            let frame = frame_size w.w_frame in

            tsz.requested_width <- frame + w.w_ipad_x;
            tsz.requested_height <- frame + w.w_ipad_y;

            let wob_requested_width  = ref 0 in
            let wob_requested_height = ref 0 in

            Array.iter (fun wob ->
                let sz = wob#size_request in
                let width = sz.requested_width + 
                            2 * sz.pad_x + 
                            w.w_ipad_x in
                let height = sz.requested_height + 
                             2 * sz.pad_y + 
                             w.w_ipad_y in

                match sens with
                | Vertical -> 
                    wob_requested_width := max width !wob_requested_width;
                    wob_requested_height := !wob_requested_height + height
                | Horizontal ->
                    wob_requested_height := max height !wob_requested_height;
                    wob_requested_width := !wob_requested_width + width
              ) wobs;


            tsz.requested_width <-  tsz.requested_width +
                                    !wob_requested_width;
            tsz.requested_height <- tsz.requested_height +
                                    !wob_requested_height;

            tsz.requested_width <- min tsz.max_width 
                                       (max tsz.requested_width 
                                            tsz.min_width);
            tsz.requested_height <- min tsz.max_height 
                                        (max tsz.requested_height 
                                             tsz.min_height);

            debug dl "VX_box.size_request: %s modified dx=%d dy=%d\n"
                     self#name
                     tsz.requested_width tsz.requested_height;


            tsz
        end;
    (*
    ** Inherit clipping to the wobs
    *)
    method update_clipping =
Db.Pr.ss 10 "VX_box#update_clipping" self#name;
        if w.w_clipped then
        begin
            let nwobs = Array.length wobs in
            for i = 0 to nwobs - 1 
            do            
                let cb = w.w_clipping in
                let win' = wobs.(i)#win_request in
                let g' = win'.w_geometry in
                win'.w_clipped <- true;
                let cb' = win'.w_clipping in

Db.Pr.sdddd 10 "VX_box#update_clipping box(x y) wob.g(x,y)" cb.x
                                       cb.y
                                       g'.x
                                       g'.y;
Db.Pr.sdd 10 "VX_box#update_clipping clip(w,h)" cb.width cb.height;
                cb'.x <- cb.x - g'.x;
                cb'.y <- cb.y - g'.y;
                cb'.width <- cb.width;
                cb'.height <- cb.height;
                wobs.(i)#update_clipping;
            done;
        end;
                 
    (*
    **
    **  (Re)size and position the wobs.
    **  Args:
    **      x y dx dy -> Available area!
    **
    *)

    method size_allocate x y dx dy =

        let g = w.w_geometry in
        let modified = ref (
                            w.w_size_modified || 
                            not (g.width = dx && g.height = dy)) in
        (*
        ** This object (re)size.
        *)
        let accepted = ref (super#size_allocate x y dx dy) in

        Db.Pr.sdddd (10) "VX_box#size_allocate w/h g.w g.h" dx dy 
                                                          g.width
                                                          g.height;

        let loop = ref 0 in
        while !modified && !loop < 2
        do
            incr loop;
            modified := false;
            if Array.length wobs > 0 then
            begin
                let tsz = szhints in
                let frame = frame_size w.w_frame in
                let frame_off = frame_offset w.w_frame in
                let holes = ref 0 in
                let retractables = ref 0 in
                let required = ref 0 in
                let g = w.w_geometry in
                begin
                  match sens with
                  | Vertical ->
                    let required = ref 0 in
                    let requested = ref 0 in
                    let holes = ref 0 in
                    let nwobs = Array.length wobs in
                    for i = 0 to nwobs - 1 do
                            let sz = wobs.(i)#size_request in
                            required := sz.min_height + !required;
                            requested := sz.requested_height + !requested;
                            if sz.expand_y then incr holes
                    done;
                    (*
                    ** Available height for the wobs.
                    *)
                    let offered = g.height - frame in

                    if offered >= !requested then 
                    begin 
                        (*
                        ** We have more place than wanted 
                        *)
                    
                        let supplement = offered - !requested in
                        let pad = if not w.w_adjust_y then
                                     w.w_ipad_y 
                                  else
                                    (supplement / (nwobs + 1)) in
                        let padding = pad * (nwobs + 1) in
                        let expand = ref (supplement - padding) in
                        let y = ref (pad+frame_off) in
                        for i = 0 to nwobs - 1 do
                            let sz = wobs.(i)#size_request in
                            let pad' = sz.pad_y in

                            (*
                            ** Widget outside padding reduces
                            ** precalculated height expansion and
                            ** the start y point of the widget!
                            *)
                            expand := max 0 (!expand - 2 * pad');
                            y := !y + pad';

                            (*
                            ** Height expansion of WOB?
                            *)

                            let height = sz.requested_height in
                            let height = if sz.expand_y then
                                                height + !expand / !holes 
                                             else 
                                                height
                                in
                            (*
                            ** Now the other direction
                            *)
                            let x, width = 
                                let offered = g.width - frame in
                                let requested = sz.requested_width in
                                let required = sz.min_width in

                                let supplement = offered - requested in

                                let pad = if supplement > 0 
                                            then min (w.w_ipad_x + sz.pad_x)
                                                     (supplement/2) 
                                            else (w.w_ipad_x + sz.pad_x) in

                                if offered >= requested then
                                begin
                                    (pad,
                                     (if sz.expand_x 
                                        then (offered - 2 * pad)
                                        else requested))
                                end
                                else if offered >= required then
                                begin
                                    (pad,
                                     (if sz.expand_x 
                                        then (offered - 2 * pad)
                                        else required))
                                end
                                else
                                begin
                                    warn (sprintf
                                    "VX_box: overfull vbox [h] (%d px too large)"
                                        (required - offered)) self#name;
                                    (0, g.width)
                                end;
                                in
                      
                            (*
                            ** Resize and reposition the WOB. 
                            ** Regard user position constrains.
                            *)
                            let accepted' =
                            wobs.(i)#size_allocate ((if sz.pos_x = -1 
                                                        then x
                                                        else sz.pos_x)
                                                    + frame_off) 
                                                   (if sz.pos_y = -1 
                                                        then !y
                                                        else sz.pos_y)
                                                   width 
                                                   height in

                            y := !y + height + pad + pad';
                            modified := !modified or (not accepted');
                        done;

                        if !y > (offered+badness+frame_off) then
                            warn (sprintf
                                  "VX_box: overfull vbox [v] due to padding (%d px too large)"
                                  (!y - offered)) self#name;
                        if !y < (offered-badness) then
                            warn (sprintf
                                  "VX_box: underfull vbox [v] (%d px too small)"
                                  (offered - !y)) self#name;
                    end
                    else if offered > !required then
                    begin
                        let supplement = ref (offered - !required) in
                    
                        let y = ref frame_off in
                        for i = 0 to nwobs - 1 do
                            let sz = wobs.(i)#size_request in
                            let sup = max 0 (min !supplement 
                                              (sz.requested_height - 
                                               sz.min_height)) in
                            let height = sz.min_height + sup in
                      
                            (*
                            ** Now the other direction
                            *)
                            let x, width = 
                                let offered = g.width - frame in
                                let requested = sz.requested_width in
                                let required = sz.min_width in
                                let supplement = offered - requested in

                                let pad = min (w.w_ipad_x + sz.pad_x)
                                              (supplement/2) in
                                if offered >= requested then
                                begin
                                    (pad,
                                     (if sz.expand_x 
                                        then (offered - 2 * pad)
                                        else requested))
                                end
                                else if offered >= required then
                                begin
                                    (pad,
                                     (if sz.expand_x 
                                        then (offered - 2 * pad)
                                        else required))
                                end
                                else
                                begin
                                    warn (sprintf
                                    "VX_box: overfull vbox [h] (%d px too large)"
                                        (required - offered)) self#name;
                                    (0, g.width)
                                end;
                                in
                            (*
                            ** Resize and reposition the WOB. 
                            ** Regard user position constrains.
                            *)
                            let accepted' =
                            wobs.(i)#size_allocate ((if sz.pos_x = -1  
                                                        then x
                                                        else sz.pos_x)
                                                    + frame_off) 
                                                   (if sz.pos_y = -1 
                                                        then !y
                                                        else sz.pos_y)                                                   
                                                   width 
                                                   height in
                            y := !y + height;
                            supplement := !supplement - sup;
                            modified := !modified or (not accepted');
                        done;
                        if !y > (offered+badness+frame_off) then
                            warn (sprintf
                                  "VX_box: overfull vbox [v] due to padding (%d px too large)"
                                  (!y - offered)) self#name;
                        if !y < (offered-badness) then
                            warn (sprintf
                                  "VX_box: underfull vbox [v] (%d px too small)"
                                  (offered - !y)) self#name;
                    end
                    else
                    begin
                        warn (sprintf "VX_box: overfull vbox [v] (%d px too large)!"
                              (!required - offered)) self#name;
                    end;

                  |  Horizontal ->
                    let required = ref 0 in
                    let requested = ref 0 in
                    let holes = ref 0 in
                    let nwobs = Array.length wobs in
                    for i = 0 to nwobs - 1 do
                        let sz = wobs.(i)#size_request in
                        required := !required + sz.min_width;
                        requested := !requested + sz.requested_width;
                        if sz.expand_x then incr holes
                    done;

                    (*
                    ** The width we can use for the wobs.
                    *)
                    let offered = g.width - frame in

Db.Pr.sddd 10 "VX_box.h#size_allocate required requested offered"
              !required !requested offered;

                    if offered > !requested then 
                    begin 
                        (*
                        ** We have more place than wanted 
                        *)
                        let supplement = offered - !requested in
                        let pad = if not w.w_adjust_x
                                    then w.w_ipad_x 
                                    else (supplement / (nwobs + 1)) in

                        let padding = pad * (nwobs + 1) in
                        let expand = ref (supplement - padding) in
                    
                        let x = ref (pad+frame_off) in
                        for i = 0 to nwobs - 1 do
                            let sz = wobs.(i)#size_request in
                            let width = sz.requested_width in
                            let pad' = sz.pad_x in

                            (*
                            ** Widget outside padding reduces
                            ** precalculated widht expansion and
                            ** the start x point of the widget!
                            *)
                            expand := max 0 (!expand - 2 * pad');
                            x := !x + pad';

                            (*
                            ** Width expansion?
                            *)
                            let width = if sz.expand_x then
                                            width + !expand / !holes 
                                        else 
                                            width in

                            (*
                            ** Now the other direction
                            *)
                            let y,  height = 
                                let offered = g.height - frame in
                                let requested = sz.requested_height in
                                let required = sz.min_height in
                                let supplement = offered - requested in

                                let pad = if (supplement > 0)
                                            then min (w.w_ipad_y + sz.pad_y)
                                                     (supplement/2) 
                                            else (w.w_ipad_y + sz.pad_y) in

                                if offered >= requested then
                                begin
                                    (pad,
                                     (if sz.expand_y 
                                        then (offered - 2 * pad)
                                        else requested))
                                end
                                else if offered >= required then
                                begin
                                    (pad,
                                     (if sz.expand_y 
                                        then (offered - 2 * pad)
                                        else required))
                                end
                                else
                                begin
                                    warn (sprintf
                                    "VX_box: overfull hbox [v] (%d px too large)"
                                        (required - offered)) self#name;
                                    (0, g.height)
                                end;
                                in

                            (*
                            ** Resize and reposition the WOB. 
                            ** Regard user position constrains.
                            *)
                            let accepted' =
                            wobs.(i)#size_allocate  (if sz.pos_x = -1 
                                                        then !x
                                                        else sz.pos_x)
                                                    ((if sz.pos_y = -1  
                                                        then y
                                                        else sz.pos_y)
                                                      + frame_off) 
                                                    width 
                                                    height in

                            x := !x + width + pad + pad';
                            modified := !modified or (not accepted');
                        done;
                        if !x > (offered+badness+frame_off) then
                            warn (sprintf
                                  "VX_box: overfull hbox [h] due to padding (%d px too large)"
                                  (!x - offered)) self#name;
                        if !x < (offered-badness) then
                            warn (sprintf
                                  "VX_box: underfull hbox [h] (%d px too small)"
                                  (offered - !x)) self#name;
                    end
                    else if offered > !required then
                    begin
                        let supplement = ref (offered - !required) in
                    
                        let x = ref frame_off in
                        for i = 0 to nwobs - 1 do
                            let sz = wobs.(i)#size_request in
                            let sup = max 0 (min !supplement 
                                                 (sz.requested_width - 
                                                  sz.min_width)) in
                            let width = sz.min_width + sup in

                            (*
                            ** Now the other direction
                            *)
                            let y,  height = 
                                let offered = g.height - frame in
                                let requested = sz.requested_height in
                                let required = sz.min_height in
                                let supplement = offered - requested in

                                let pad = min (w.w_ipad_y + sz.pad_y)
                                              (supplement/2) in
                                if offered >= requested then
                                begin
                                    (pad,
                                     (if sz.expand_y 
                                        then (offered - 2 * pad)
                                        else requested))
                                end
                                else if offered >= required then
                                begin
                                    (pad,
                                     (if sz.expand_y 
                                        then (offered - 2 * pad)
                                        else required))
                                end
                                else
                                begin
                                    warn (sprintf
                                    "VX_box: overfull hbox [v] (%d px too large)"
                                        (required - offered)) self#name;
                                    (0, g.height)
                                end;
                                in
                            (*
                            ** Resize and reposition the WOB. 
                            ** Regard user position constrains.
                            *)
                            let accepted' =
                            wobs.(i)#size_allocate  (if sz.pos_x = -1 
                                                        then !x
                                                        else sz.pos_x)
                                                    ((if sz.pos_y = -1
                                                        then y
                                                        else sz.pos_y)
                                                      + frame_off) 
                                                    width 
                                                    height in


                            x := !x + width;
                            supplement := !supplement - sup;
                            modified := !modified or (not accepted');
                        done;
                        if !x > (offered+badness+frame_off) then
                            warn (sprintf
                                  "VX_box: overfull hbox [h] due to padding (%d px too large)"
                                  (!x - offered)) self#name;
                        if !x < (offered-badness) then
                            warn (sprintf
                                  "VX_box: underfull hbox [h] (%d px too small)"
                                  (offered - !x)) self#name;
                    end
                    else
                    begin
                        warn (sprintf "VX_box: overfull hbox [h] (%d px too large)!"
                              (!required - offered)) self#name;
                    end;
                end;
            end;
        if !modified then
            Db.Pr.s (10) "VX_box#size_allocate:  recalculate wobs";
        done;

        self#update_clipping;
        !accepted 


(*            
**  method destroy =
**    Array.iter (fun wob -> wob#destroy) wobs;
**    super#destroy
*)
        
    method container_insert i (wob : contained) =
        let size = Array.length wobs in
        if i > size then failwith "VX_box: Bar: insert_item after end of box";
        wobs <- Array.init (size+1) (fun j ->
            if i=j then
            begin
                if not (w.w_window == noWindow) then 
                    (wob#realize; wob) 
                else 
                    wob
            end
            else if j < i then 
                wobs.(j) 
            else 
                wobs.(j-1));
        self#wait_resize
  
    method container_remove i =
        let size = Array.length wobs in
        if i >= size then failwith "VX_box: Bar: remove_item after end of box";
        wobs <- Array.init (size-1) (fun j ->
            if i = j then wobs.(i)#destroy;
            if j >= i then wobs.(j+1) else wobs.(j));
        self#wait_resize
  
    (*
    ** Add a widget object to the box container.
    *)
    method container_add wob = 
        wob#set_parent self#container;
        self#container_insert (self#nitems) wob;

    (*
    ** Exchange a widget object (# i). 
    *)
    method container_exchange i newwob =
        newwob#set_parent self#container;
        let size = Array.length wobs in
        if i >= size then failwith "VX_box: Bar: exchange_item after end of box";
        wobs.(i)#hide;
        wobs.(i)#destroy;
        wobs.(i) <- newwob;
        self#wait_resize


    method container_add_s wobs = 
        match wobs with
        | [] -> () 
        | wob :: wobs -> 
            self#container_add wob; self#container_add_s wobs;

    method clear_items =
        Array.iter (fun w -> w#destroy) wobs;
        wobs <- [||];
        self#wait_resize
    
    method set_items ws =
        self#clear_items;
        wobs <- ws;
        if not (w.w_window == noWindow) then 
            Array.iter (fun w -> w#realize) wobs;
        if w.w_shown then  
            Array.iter (fun w -> w#show) wobs;
        self#wait_resize
        
    method nitems = Array.length wobs
    method items = wobs    
  

    (*
    ** Output postscript. The (x0,y0) origin is absolute relative to
    ** X origin (upper left corner) and in pixel units.
    *)
    method print (ps : ps) (x0:int) (y0:int) =
        Db.Pr.sdd 10 "VX_box: print: x0,y0" x0 y0;
        super#print ps x0 y0;
        self#iter (fun obj -> 
            let w = obj#win_request in
            let g = w.w_geometry in
            obj#print ps (x0+g.x) (y0+g.y);
            );


    (*
    ** Print box informations (sizes...) in tree format with
    ** childs
    *)
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
        printf "%sContained objects:" (spaces (ind+2));
        print_newline ();
        self#iter (fun o -> 
            let sz' = o#size_request in
            printf "%ss.req=%d,%d s.min=%d,%d s.max=%d,%d s.pad=%d,%d s.exp=%b,%b"
                   (spaces (ind+2))
                   sz'.requested_width sz'.requested_height
                   sz'.min_width sz'.min_height
                   sz'.min_width sz'.min_height
                   sz'.pad_x sz'.pad_y 
                   sz'.expand_x sz'.expand_y;
            print_newline ();
            );
end

class t = orig

(*
** Vertical aligned box
*)

class v parent attributes  =
    object (self)
    inherit t Vertical parent attributes

    initializer
        self#set_name "vbox"  

end

(*
** Horizontal aligned box
*)
class h parent attributes =
    object (self)
    inherit t Horizontal parent attributes
    
    initializer
        self#set_name "hbox"  
end

