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
**    $AUTHORS:     Fabrice Le Fessant, Stefan Bosse
**    $INITIAL:     (C) 1999 INRIA
**    $CREATED:     Part II: 2003.11.28
**    $VERSION:     1.06
**
**    $INFO:
**
**  class t:
**
**  Generic table widget.
**  
**  NOT COMPLETELY IMPLEMENTED YET.
**  
**  The WX_table.t widget will allow to set widgets positions in a table.
**  
**  Clearly, for all containers, there is a need to separate size hints in two 
**  different kinds:
**  those which are user defined, and those which are computed from
**  contained widgets.
**
**
**  class text:
**
**  Text Table Widget.
**
**  Text strings can be placed within a table with # rows and # columns.
**  The height and width of each cell (in pixles) must be specified.
**  Value 0 forces auto adjusting of cell widths and heights.
**  The text can be set individual with the set_text method or with
**  the set_table expecting row array of col array string data or
**  inital with the new method (but can also be an empty array here).
**
**  Example:
**
**  let rows = 3 
**  let cols = 3
**
**  let vals = [|
**      [|"Nummer";"Datum";"Name"|];
**      [|"0001";"23.10.2003-042171717";"Target"|];
**      [|"0002";"23.11.2003";"Task"|];|]
**  let tab = new wX_table.text vbar#container rows cols 150 50 vals [] in
**  
**  tab#set_table vals;
**  tab#set_text 1 1 "Changed";
**
**  vbar#container_add_s  [ 
**                tab#contained; ...
**
**  Attributes of individual cells can be set with the set_style_cell, and
**  the below text widget with the set_style_text method.
**
**
**
**  class button:
**
**  Table of button widgets. Here, the text can be specified at
**  creation time, too, or later with the set_string and
**  set_font, set_style methods. 
**  After, the individual button actions can be set with
**  the set#action method.
**  Button groups (with only one button activated each time) can be 
**  created with the set_group method. The [(col,row)...] list and the initial 
**  (col,row) activated button must be specified. The set-group method
**  must be called before the set_action method!
**
**    $ENDOFINFO
**
*)

open Xtypes
open WX_types
  

class t parent attributes dx dy homogeneous =
  let _ = assert (dx > 0); assert (dy > 0) in
  object (self)
  
  val wobs = Array.init dy (fun i -> 
        Array.init dx (fun i -> WX_dummy.contained parent,0,0,1,1)
    )
  
  val hints = Array.init (if homogeneous then 1 else
        max dx dy) WX_object.new_szhints
      
      inherit WX_object.t parent attributes as super
  
  method iter f = Array.iter (fun t -> 
        Array.iter (fun (o,x,y,dx,dy) -> f o) t) wobs
  
  method size_request =
    let tsz = szhints in
    if not w.w_size_modified || tsz.comp_timestamp = s.s_timestamp then tsz
    else 
    let _ = () in
    tsz.comp_timestamp <- s.s_timestamp;
        (* First, initialize everything *)
    for i = 1 to if homogeneous then 1 else max dx dy do
      hints.(i-1) <- WX_object.new_szhints ()
    done;
        (* second, computes rows and cols sizes *)
    for i = 0 to dx - 1 do
      for j = 0 to dy - 1 do
        let wob,_,_,ddx,ddy = wobs.(j).(i) in
        let sz = wob#size_request in
        let borders = sz.border_width * 2 in
        let width = sz.requested_width + borders in
        let height = sz.requested_height + borders in
        let szx, szy =
          if homogeneous then hints.(0), hints.(0) else
            hints.(i), hints.(j) in
        szx.requested_width <- max szx.requested_width (
          (sz.requested_width + 2 * sz.border_width) / ddx);
        szx.expand_x <- sz.expand_x && szx.expand_x;
        szx.min_width <- max szx.min_width sz.min_width;
        szy.requested_height <- max szx.requested_height (
          (sz.requested_height + 2 * sz.border_width) / ddy);
        szy.expand_y <- sz.expand_y && szx.expand_y;
        szy.min_height <- max szy.min_height sz.min_height;
      done
    done;
        (* third, compute the complete size *)
    if homogeneous then
      let sz = hints.(0) in
      tsz.requested_width <- 2 * w.w_ipad_x + dx * sz.requested_width;
      tsz.requested_height <- 2 * w.w_ipad_y + dy * sz.requested_height;
      tsz.expand_x <- sz.expand_x;
      tsz.expand_y <- sz.expand_y;
      tsz
    else
    let expand_x = ref true in
    let expand_y = ref true in
    tsz.requested_width <- 2 * w.w_ipad_x;
    tsz.requested_height <- 2 * w.w_ipad_y;
    for i = 0 to dx - 1 do
      tsz.requested_width <- hints.(i).requested_width + tsz.requested_width;
      expand_x := !expand_x && hints.(i).expand_x
    done;
    for i = 0 to dy - 1 do
      tsz.requested_height <- hints.(i).requested_height + tsz.requested_height;
      expand_y := !expand_y && hints.(i).expand_y      
    done;
    tsz.expand_x <- tsz.expand_x || !expand_x;
    tsz.expand_y <- tsz.expand_y || !expand_y;
    tsz
  
  method size_allocate x y width height =
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (
        g.width = width && g.height = height) in
    super#size_allocate x y width height;
    if not modified || Array.length hints < 1 then () else
    let tsz = szhints in
    let relief = match w.w_relief with
        ReliefFlat -> 0
      | ReliefRaised -> 1
      | ReliefSunken -> 1
      | ReliefRaisedN n -> n
      | ReliefSunkenN n -> n
      | ReliefInRaised -> 1
      | ReliefInSunken -> 1
      | ReliefInRaisedN n -> n
      | ReliefInSunkenN n -> n          
      | _ -> 2
    in      
    if homogeneous then
      let sz = hints.(0) in
      let width = g.width - 2 * w.w_ipad_x in
      let height = g.height - 2 * w.w_ipad_y in
      let width = if sz.expand_x then width else
          min width (sz.requested_width * dx) in
      let height = if sz.expand_y then height else
          min height (sz.requested_height * dy) in
      let width = width / dx in
      let height = height / dy in
      for i = 0 to dx - 1 do
        for j = 0 to dy - 1 do
          let wob, x,y, ddx, ddy = wobs.(j).(i) in
          let sz = wob#size_request in
          if x = 0 && y = 0 then
            let borders = sz.border_width * 2 in
            wob#size_allocate (i * width + sz.border_width + w.w_ipad_x) (
              j * height + sz.border_width + w.w_ipad_y) (
              width * ddx - borders) (height * ddy - borders)
        done
      done
    else
    let g = w.w_geometry in
    let width = g.width - 2 * w.w_ipad_x in
    let height = g.height - 2 * w.w_ipad_y in
    let x_holes = ref 0 in
    let y_holes = ref 0 in
    let requested_x = ref 1 in
    let requested_y = ref 1 in
    for i = 0 to dy - 1 do
      let sz = hints.(i) in
      requested_y := sz.requested_height + !requested_y;
      if sz.expand_y then incr y_holes
    done;
    for i = 0 to dx - 1 do
      let sz = hints.(i) in
      requested_x := sz.requested_width + !requested_x;
      if sz.expand_x then incr x_holes
    done;        
    let supplement_x = max 0 (width - !requested_x) in
    let supplement_y = max 0 (height - !requested_y) in
    let x = ref w.w_ipad_x in
    for i = 0 to dx - 1 do
      let y = ref w.w_ipad_y in
      let szx = hints.(i) in
      let width = szx.requested_width + (
          if szx.expand_x then supplement_x / !x_holes else 0) in
      for j = 0 to dy - 1 do
        let wob,xx,yy,dxx,dyy = wobs.(j).(i) in
        let szy = hints.(j) in
        let height = szy.requested_height + (
            if szx.expand_y then supplement_y / !y_holes else 0) in
        begin
          if xx=0 && yy=0 then
            let sz = wob#size_request in
            let borders = 2 * sz.border_width in
            let ww = ref width in
            let hh = ref height in
            for ii = 1 to dxx -1 do
              let szx = hints.(i+ii) in
              let width = szx.requested_width + (
                  if szx.expand_x then supplement_x / !x_holes else 0) in
              ww := !ww + width
            done;
            for jj = 1 to dyy -1 do
              let szy = hints.(j+jj) in
              let height = szy.requested_height + (
                  if szx.expand_y then supplement_y / !y_holes else 0) in
              hh := !hh + height
            done;
            let width = !ww in
            let height = !hh in
            wob#size_allocate (!x + sz.border_width) (!y + sz.border_width) (
              (if sz.expand_x then 
                  width else min width (sz.requested_width + borders))
              - borders) (
              (if sz.expand_y then 
                  height else min height (sz.requested_height + borders))
              - borders);
        end;
        y := !y + height
      done;
      x := !x + width
    done
    
          
  method container_add wob x y dx dy =
    wob#set_parent self#container;
    for i = 0 to dx-1 do
      for j = 0 to dy-1 do
        wobs.(y+j).(x+i) <- (wob, i, j, dx, dy)
      done
    done

  method container = (self :> container)
end



class text parent cols rows width height vals attr =
object (self)
    val tab_obj = new t parent attr
                    cols rows false 
    val tabs = Array.create rows [||]
    val tabo = Array.create rows [||]

    inherit WX_object.t parent 
            ([
             ] @ (if width > 0 then [MinWidth width] else [])
               @ (if height > 0 then [MinHeight height] else [])
               @ attr)
    as super

    initializer
        for row = 0 to rows-1
        do
            for col = 0 to cols-1
            do
                let obj = new WX_bar.h tab_obj#container 
                                   (
                                    [ 
                                        Background "white";
                                        BorderWidth 1;
                                        BorderColor "grey";
                                    ]
                           @ (if width > 0 then [MinWidth width] else 
                                                [ExpandX true])
                           @ (if height > 0 then [MinHeight height] else 
                                                [ExpandY true])
                                    ) in 
                tabo.(row) <- Array.append (tabo.(row)) [|obj|];

                let s = if (vals <> [||]) then vals.(row).(col) else "" in
                let str = new WX_text.of_string obj#container s
                                    [
                                        Background "white"; 
                                    ] in
                tabs.(row) <- Array.append (tabs.(row)) [|str|];
                obj#container_add str#contained; 
                tab_obj#container_add obj#contained col row 1 1; 
            done;
        done;
            
    (*
    ** Set one cell
    *)
    method set_text col row str =
        (tabs.(row)).(col)#set_string str;
        (tabs.(row)).(col)#update;

    method set_style_text col row attr =
        (tabs.(row)).(col)#configure attr;
        (tabs.(row)).(col)#update;

    method set_style_cell col row attr =
        (tabo.(row)).(col)#configure attr;
        (tabo.(row)).(col)#update;

    (*
    ** apply attributes to all cells = configure !
    *)
    method set_style_all attr =
        for row = 0 to rows-1
        do
            for col = 0 to cols-1
            do
                (tabs.(row)).(col)#configure attr;
                (tabs.(row)).(col)#update;
                (tabo.(row)).(col)#configure attr;
                (tabo.(row)).(col)#update;
            done;
        done;

    (*
    ** Set all cells of the table.
    ** vals is a string array array, organized by row arrays.
    *)

    method set_table vals =
        for i = 0 to rows-1
        do
            for j = 0 to cols-1
            do
                self#set_text j i vals.(i).(j);
            done;
        done


    method set_font col row f = 
        (tabs.(row)).(col)#set_font (parent#font_make f true);
        (tabs.(row)).(col)#update;
        tab_obj#update

    method contained = tab_obj#contained        
    method name = "texttable"
end

class button parent cols rows width height names attr =
object (self)
    val tab_obj = new t parent attr
                    cols rows false 

    (*
    ** Arrays of all button (tabb) and cell (tabo) widgets,
    ** and of all action handlers (taba) (if any) with button states
    ** (tabs), if button is handled in this class.
    ** Additionally, the kind of the button is stored (tabm).
    *)

    val tabb = Array.create rows [||]
    val tabo = Array.create rows [||]
    val taba = Array.create rows [||]
    val tabs = Array.create rows [||]
    val tabm = Array.create rows [||]

    val mutable groups = []

    inherit WX_object.t parent 
            ([
             ] @ (if width > 0 then [MinWidth width] else [])
               @ (if height > 0 then [MinHeight height] else [])
               @ attr)
    as super

    initializer
        for row = 0 to rows-1
        do
            for col = 0 to cols-1
            do
                let obj = new WX_bar.h tab_obj#container 
                                   (
                                    [
                                    ]
                           @ (if width > 0 then [MinWidth width] else 
                                                [ExpandX true])
                           @ (if height > 0 then [MinHeight height] else 
                                                [ExpandY true])
                                    ) in 
                tabo.(row) <- Array.append (tabo.(row)) [|obj|];
                let but = new WX_button.with_label obj#container 
                                    names.(row).(col)
                                    [
                                        ExpandX true; 
                                        ExpandY true;
                                    ] in
                tabb.(row) <- Array.append (tabb.(row)) [|but|];
                obj#container_add but#contained; 
                tab_obj#container_add obj#contained col row 1 1; 
            done;
            (*
            ** Initialize various arrays
            *)
            taba.(row) <- Array.append (taba.(row)) 
                                (Array.create cols (fun () -> ()));
            tabs.(row) <- Array.append (tabs.(row)) 
                                (Array.create cols false);
            tabm.(row) <- Array.append (tabm.(row)) 
                                (Array.create cols false);
        done

    (*
    ** Set style for one cell
    *)

    method set_style_cell col row attr =
        (tabo.(row)).(col)#configure attr;
        (tabo.(row)).(col)#update;

    (*
    ** and for the button (label)
    *)

    method set_style_but col row attr =
        (tabb.(row)).(col)#label#configure attr;
        (tabb.(row)).(col)#label#update;
        (tabb.(row)).(col)#configure attr;
        (tabb.(row)).(col)#update;

    (*
    ** apply attributes to all cells = configure !
    *)
    method set_style_all attr =
        for row = 0 to rows-1
        do
            for col = 0 to cols-1
            do
                (tabb.(row)).(col)#configure attr;
                (tabb.(row)).(col)#update;

                (tabo.(row)).(col)#configure attr;
                (tabo.(row)).(col)#update;
            done;
        done;
    

    method set_action col row fu =
        (*
        ** Call wrapper function to resolve possible button groups
        ** with only one activated button or control bistable button
        ** states.
        *)
        let f () = 
            let state  = tabs.(row).(col) in
            let action = taba.(row).(col) in
            let doaction = ref true in
            (*
            ** Is it a bistable button ?
            *)
            if (tabm.(row).(col) = true) then
            begin
                    tabs.(row).(col) <- not tabs.(row).(col);
                    match tabs.(row).(col) with
                    | true -> tabb.(row).(col)#activate;
                    | false -> tabb.(row).(col)#desactivate;
            end
            (*
            ** Belongs button to any group ?
            *)
            else if (groups <> []) then
            List.iter (fun grp ->
                    for i = 0 to (Array.length grp)-1
                    do
                        if grp.(i) = (col,row) then
                        begin
                            (*
                            ** found in group. activated or not ?
                            *)
                            if (state = false) then
                            begin
                                (*
                                ** not activeated. perform switch 
                                ** before switch other activated button off
                                *)
                                for j = 0 to (Array.length grp)-1
                                do
                                    let col',row' = grp.(j) in
                                    let state' = tabs.(row').(col') in
                                    if state' = true then
                                    begin
                                        tabb.(row').(col')#desactivate;
                                        tabs.(row').(col') <- false;
                                    end;
                                done;

                                tabb.(row).(col)#activate;
                                tabs.(row).(col) <- true;
                            end
                            else
                                doaction := false;  (* nothing todo *)
                        end;
                    done;
                ) groups;

            if (!doaction) then 
                action ();
            in
        taba.(row).(col) <- fu;
        (tabb.(row)).(col)#set_action f


    method set_font col row f = 
        (tabb.(row)).(col)#label#set_font f;
        (tabb.(row)).(col)#label#refresh;
        (tabb.(row)).(col)#refresh;
        tab_obj#update

    method set_text col row f = 
        (tabb.(row)).(col)#label#set_string f;
        (tabb.(row)).(col)#label#refresh;
        (tabb.(row)).(col)#refresh;
        tab_obj#update
   
    (*
    ** Set a button group (list of (col,row) values).
    ** Switch activities are handled in THIS class.
    ** Initial sets the initial activated button 
    *)

    method set_group grp initial =
        groups <- groups @ [(Array.of_list grp)];
        List.iter (fun b ->
                let col,row = b in
                (tabb.(row)).(col)#set_external_switch true;
            
                (*
                ** The action handler will control the button state.
                *)
                self#set_action col row (fun () -> ());
            ) grp;
        let col,row = initial in
        tabb.(row).(col)#activate;
        tabs.(row).(col) <- true

    (*
    ** Mono- (wait_for_release) or bistable button (one action for
    ** Button press- and release)! 
    ** Default: f=false -> Monoswitch
    *) 
    method set_bistable col row f =
        tabm.(row).(col) <- f;
        if (f=true) then
        begin
            (*
            ** The action handler will control the button state.
            *)
            (tabb.(row)).(col)#set_external_switch true;
            self#set_action col row (fun () -> ());
        end
        

    method set_state col row s =
        (tabs.(row)).(col) <- s;
        match s with
        | true -> (tabb.(row)).(col)#activate; 
        | false -> (tabb.(row)).(col)#desactivate;


    method contained = tab_obj#contained        
    method name = "buttontable"
end

