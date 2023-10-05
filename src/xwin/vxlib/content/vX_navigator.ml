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
**    $CREATED:     10.7.2005
**    $VERSION:     1.22
**
**    $INFO:
**
**  Frame based highlevel widget navigator for rapid prototyping.
**
**  
**  The navigator is handled like a book with chapters, sections and 
**  subsections. The chapter names are displayed on the top, the sections
**  on the left side. Subsections are used for partitioning the frame
**  content:
**
**      S1 S1 S1 S1
**  S2  +------------+
**  S2  | S3.        |
**  S2  | Content    |
**      |            |
**      +------------+
**
**
**  The user can navigate by clicking the section buttons. If there are
**  no S2 sections, the left button side is not displayed.
**
**  Three kinds of navigator widget classes exist:
**
**      s1      : only chapters, no s2/s3 sections
**      s12     : chapters and sections
**      s123    : three section depths - really a book
**
**    $ENDOFINFO
**
*)

open Xtypes
open VX_types
open VX_common
open VX_view
open VX_slider

open Amoeba
open Stderr
open Stdcom
open Vtty_server
open Shell_exec
open Thread
open Myenv
open Bytebuf
open Printf
open VX_draw

type navigator_kind =
    | N_s1
    | N_s12
    | N_s123
    (*
    ** With additional content box view and a vertical scroll slider
    *)
    | N_s1_view
    | N_s12_view
    | N_s123_view


(*
** Public part
*)
type attribute = VX_types.widget_attributes

type text = {
    text_str : string;
    text_attr : attribute list;
    mutable text_vx : VX_text.t option;
}


type input = {
    (*
    ** input label name and optional fixed label width for multiinput
    ** widget alignment ( if > 0 then fixed width in pixel, 
    ** else auto width)
    *)
    inp_desc : int * string;  
    inp_attr : attribute list;
    (*
    ** The input widget can contain one ore more buttons of
    ** type ActionSS (only one, inside editable line) or 
    ** ActionUU (several, just after the editable line). The
    ** string specifies the button labels (except in the ActionSS case,
    ** just an empty string).
    *)
    inp_buts  : (string * attribute) list; 
    (*
    ** The editable string - can be initialized
    *)
    mutable inp_str  : string;
    
    mutable inp_vx : VX_texttable.t option;
}

type file = {
    file_desc : string;
    mutable file_attr : attribute list;
    mutable file_action  : (string -> unit);
    mutable file_path  : Myenv.path_arg;
    file_edit : bool;       (* with editable line ? *)
    file_top : bool;        (* toplevel or child window *)
    mutable file_vx : VX_popup.file_select_edit_orig option; 
}

type select = {
    sel_desc : string;              (* Checkbox label name              *)
    mutable sel_attr : attribute list;
    sel_choices : string array;     (* all possible choices             *)
    sel_mutual : bool;              (* one or multiple choices          *)

    mutable sel_sel : int list;     (* user selected items              *)
    mutable sel_action : (int -> unit);     (* action handler called each time a 
                                       select occured *)
    mutable sel_vx : VX_checkbox.v option;
}

type buttons = {
    mutable but_attr : attribute list; 
    (*
    ** Buttons organzied in rows and columns.
    *)
    mutable but_cols : ((string * (unit -> unit)) array) array;
    mutable but_group : bool;
    mutable but_vx : VX_button.table option;
}

type log = {
    mutable log_attr : attribute list;
    (*
    ** Size of log srceen line array - not necessary the visible part
    *)
    mutable log_rows : int;
    (*
    ** Add lines to the log screen
    *)
    mutable log_add  : (string list -> unit) option;
    (*
    ** Clear screen
    *)
    mutable log_clear : (unit -> unit) option;
    
    mutable log_vx : VX_log.view_v option;
}

type logedit = {
    mutable logedit_attr : attribute list;
    (*
    ** Size of log srceen line array - not necessary the visible part
    *)
    mutable logedit_cols : int;
    mutable logedit_rows : int;
    (*
    ** Add lines to the log screen
    *)
    mutable logedit_add  : (string list -> unit) option;
    (*
    ** Clear screen
    *)
    mutable logedit_clear : (unit -> unit) option;
    
    mutable logedit_vx : VX_log.editview_v option;
}


(*
** Process control. The exec_def structure can be found in the
** Shell_exec module (similar to Vamboot, but only for a single 
** process object).
*)
type process = {
    mutable pro_desc : string;
    mutable pro_attr : attribute list;
    pro_def : exec_def;
    mutable pro_log : (string list -> unit) option;
    mutable pro_obj : exec_obj option;
}

(*
** Text table origanized in rows and columns (cells). The cells can be
** editable.
*)
type table = {
    mutable tab_desc : string;
    mutable tab_attr : attribute list;          (* main attributes *)
    mutable tab_rows : (attribute list) array;  (* row attributes  *)
    (*
    ** cell content and cell attributes 
    *)
    mutable tab_cols : (string * (attribute list)) array array; 

    (*
    ** Function to change a cell and to get the actual content of
    ** the cell. The row and column must be specified (start index 0).
    ** Available after class creation!
    *)
    mutable tab_set : (int -> int -> (string * (attribute list)) -> unit)
                      option;
    mutable tab_get : (int -> int -> string) option;
    mutable tab_vx : VX_texttable.t option;
}

type drawing = {
    mutable draw_attr : attribute list;
    (*
    ** Add a drawing path
    *)
    mutable draw_add : (draw list -> unit) option;
    (*
    ** Delete (erase) a drawing path
    *)
    mutable draw_del : (int -> unit) option;

    (*
    ** Print the drawing to an eps file
    *)
    mutable draw_print : (string -> unit) option;

    mutable draw_vx : VX_draw.t option;
}


(*
** Slider for value adjustment
*)
type value = {
    mutable val_desc : string;
    (*
    ** Value unit (for example seconds) - displayed after actual value
    *)
    mutable val_unit : string;

    mutable val_attr : attribute list;
    (*
    ** Min/Max, smallest step and large step value settings
    *)
    mutable val_min  : float;
    mutable val_max  : float;
    mutable val_res  : float;
    mutable val_step : float;

    (*
    ** User supplied formatted value string
    *) 
    mutable val_print : (float -> string);
    (*
    ** Action handler with actual value
    *)
    mutable val_action : (float -> unit);

    (*
    ** Reconfigure parameters
    *)
    mutable val_config : (unit -> unit) option;

    (*
    ** Set new value between min and max value
    *)
    mutable val_set : (float -> unit) option;
    mutable val_vx : VX_slider.val_hf option;
}

type descriptor =
    | S1 of (string * descriptor list)      (* chapter              *)
    | S2 of (string * descriptor list)      (* section              *)
    | S3 of (string * descriptor list)      (* subsection           *)

    | Text of text                          (* text paragraph       *)
    | Input of input                        (* something to edit    *)
    | File of file                          (* file selector        *)
    | Select of select                      (* select boxes         *)
    | Buttons of buttons                    (* button box           *)
    | Log of  log                           (* log window           *)
    | Logedit of logedit                    (* editable log win.    *)
    | Proc of process                       (* process manager      *)
    | Table of table                        (* text tables          *)
    | Draw of drawing                       (* drawing              *)
    | Value of value                        (* Value slider         *)

    (*
    ** This action handler is called if a section was opened (selected).
    *)
    | Action of (unit -> unit)             


    | Space of int
    | Ruler of int


let text str = Text {
    text_str = no_nl str;
    text_attr = [];
    text_vx = None;
}

(*
** Private part
*)

type section_depth =
    | Sec_1
    | Sec_2
    | Sec_3


type section = {
    sec_hbox : VX_box.h option;
    sec_vbox : VX_box.v option;
    (*
    ** Section content box 
    *)
    mutable sec_cbox : VX_box.v option;
    (*
    ** Clipped view of content box if any
    *)
    mutable sec_sbox : VX_view.v option;

    sec_button : VX_button.t option;
    sec_name   : string;
    sec_depth  : section_depth;
    mutable sec_child : section option;
    mutable sec_parent : section option;
    mutable sec_next : section option;  
}



class orig kind root parent (desc: descriptor list)  attributes =
    object (self)
    val root = root
    inherit VX_box.h parent ([IpadX 5; IpadY 5;
                              Background "lightblue";]@attributes) as super


    (*
    ** All the widgets we're handling
    *)
    val mutable sections = []
    (*
    ** Holds S1 buttons and content frame.
    *)
    val mutable right_vbox = None
    val mutable s1_hbox = None
    val mutable frame_vbox = None

    (*
    ** Actual useable width in frame vbox (excluding border and padding)
    *)
    val mutable frame_width = 1
    val mutable frame_height = 1


    (*
    ** Holds S2 buttons and fillbox.
    *)
    val mutable left_vbox = None
    val mutable s2_vbox = None
    val mutable fill_vbox = None

    val mutable init = true
    val mutable last_s1 = None
    val mutable last_s2 = None


    val mutable updates = []
    
    initializer
        match kind with
        | N_s1 
        | N_s1_view ->
        begin
            (*
            ** There is no left vertical button bar.
            *)
            let rightbox = new VX_box.v self#container   
                                 [IpadX 2; IpadY 5;
                                  ExpandX true;
                                  ExpandY true;
                                  ] in
            right_vbox <- Some rightbox;
 
            let s1box = new VX_box.h rightbox#container
                                 [IpadY 5; 
                                  ExpandX true; AdjustX true;
                                  Background "white";
                                  MinHeight 10;
                                  Border []] in
            s1_hbox <- Some s1box;

            let framebox = new VX_box.v rightbox#container
                                 [IpadX 2; IpadY 2;
                                  ExpandX true;
                                  ExpandY true;
                                  Background "white";
                                  Border []] in
            frame_vbox <- Some framebox;

        
        
            self#container_add_s [rightbox#contained];
            rightbox#container_add_s [s1box#contained;
                                    framebox#contained;
                                 ];

            self#configure_sections desc;
            if sections <> [] then
                self#set_s1 (List.hd sections);

            init <- false;
        end;
        | N_s12 | N_s123 
        | N_s12_view | N_s123_view ->
        begin
            let leftbox = new VX_box.v self#container   
                                 [IpadX 2; IpadY 5;
                                  ExpandY true;
                                  ] in
            left_vbox <- Some leftbox;
            let rightbox = new VX_box.v self#container   
                                 [IpadX 2; IpadY 5;
                                  ExpandX true;
                                  ExpandY true;
                                  ] in
            right_vbox <- Some rightbox;
 
            let s1box = new VX_box.h rightbox#container
                                 [IpadY 5; 
                                  ExpandX true; AdjustX true;
                                  Background "white";
                                  MinHeight 10;
                                  Border []] in
            s1_hbox <- Some s1box;
            let s2box = new VX_box.v leftbox#container
                                 [IpadX 2; IpadY 2; 
                                  ExpandY true; 
                                  Background "white";
                                  MinWidth 20;
                                  Border []] in
            s2_vbox <- Some s2box;

            let framebox = new VX_box.v rightbox#container
                                 [IpadX 2; IpadY 2;
                                  ExpandX true;
                                  ExpandY true;
                                  Background "white";
                                  Border []] in
            frame_vbox <- Some framebox;

        
            let fillbox = new VX_box.v leftbox#container
                                 [IpadX 2; IpadY 2; 
                                  Background "lightblue";
                                  MinHeight 20;
                                  ExpandX true] in
            fill_vbox <- Some fillbox;
        
            self#container_add_s [leftbox#contained;
                              rightbox#contained];
            rightbox#container_add_s [s1box#contained;
                                  framebox#contained;
                                 ];

            leftbox#container_add_s [fillbox#contained;
                                 s2box#contained;
                                ];

            self#configure_sections desc;
            if sections <> [] then
                self#set_s1 (List.hd sections);

            (*
            ** Calculate S2 button box width. Should be fixed for
            ** all S1s.
            *)

            let width = ref 20 in
            List.iter (fun s1 ->
                match s1.sec_vbox with
                | Some v -> let sz = v#size_request in
                        width := max !width sz.requested_width;
                        __(v#configure [ExpandX true]);
                | None -> ();
                ) sections;
            width := !width + 4;        (* 2 * ipad_x *)
            __(s2box#configure [Width !width]);
            s2box#update;

            init <- false;
        end; 

    (*
    ** Iterate the descriptor list with all section and content 
    ** informations and generate internal navigator tree.
    ** Create and add widgets, too.
    *) 
    method configure_sections desc =
        let s1box = get_some s1_hbox in
        let framebox = get_some frame_vbox in

        let cur_s1 = ref None in
        let cur_s2 = ref None in
        let cur_s3 = ref None in

        let err str =
            print_string ("VX_navigator: syntax error: "^str);
            print_newline ();
            exit 1;
            in
        let warn str =
            print_string ("VX_navigator: warning: "^str);
            print_newline ();
            in

        let add_s1 s =
            sections <- sections @ [s] in

        let add_s2 parent s =
            let sl = parent.sec_child in 
            match sl with
            | Some sc ->
            begin
                let rec append sc =
                    match sc.sec_next with
                    | Some sc' -> append sc';
                    | None -> 
                        sc.sec_next <- Some s;
                    in
                append sc;
            end;
            | None -> parent.sec_child <- Some s;
            in

        let add_cont parent s widget =
            let sl = parent.sec_child in 
            match sl with
            | Some sc ->
            begin
                let rec append sc =
                    match sc.sec_next with
                    | Some sc' -> append sc';
                    | None -> 
                        sc.sec_next <- Some s;
                        let cbox = get_some parent.sec_cbox in
                        cbox#container_add widget#contained;
                    in
                append sc;
            end;
            | None -> 
                parent.sec_child <- Some s;
                let cbox = get_some parent.sec_cbox in
                cbox#container_add widget#contained; 
            in
    
        let get_cbox s =
            match s.sec_cbox with
            | Some c -> c;
            | None -> 
            begin
                match kind with
                | N_s1
                | N_s12
                | N_s123 ->
                    let cbox = new VX_box.v framebox#container
                                    [IpadX 5; IpadY 5;
                                      ExpandX true;
                                      ExpandY true; 
                                      Background "white";
                                     ] in
                    s.sec_cbox <- Some cbox;
                    cbox
                | N_s1_view
                | N_s12_view
                | N_s123_view ->
                    let sbox = new VX_view.v framebox#container
                                            [
                                                IpadX 5;
                                                ExpandX true;
                                                ExpandY true;
                                                But [Frame ReliefRaised;
                                                     Color "grey80"];
                                            ] in
                    s.sec_sbox <- Some sbox;
                    let cbox = new VX_box.v framebox#container
                                    [IpadX 5; IpadY 5;
                                      ExpandX true;
                                      ExpandY true; 
                                      Background "white";
                                     ] in
                    s.sec_cbox <- Some cbox;
                    sbox#container_add cbox#contained;
                    cbox
            end in

        sections <- [];

        let rec iter_desc descl =
          List.iter (fun desc ->
            match desc with
            | S1 (label,seclist) ->
            begin
                let butbox2 = new VX_box.v s1box#container
                                     [IpadX 4; IpadY 10;
                                      ExpandY true; 
                                      Background "white";
                                      MinWidth 20;
                                     ] in
                butbox2#set_name "Button box S2";                    
                let but = new VX_button.t s1box#container 
                                          label
                                          [
                                            IpadX 10;
                                            Text_style Bold;
                                            Text_font Helvetica;
                                            Text_size 14;
                                            Border [
                                                Frame ReliefRaised;
                                            ];
                                            Background "grey80"; 
                                          ]  in
                let ss =
                    {
                    sec_button = Some but;
                    sec_hbox = Some s1box;
                    sec_vbox = Some butbox2;
                    sec_cbox = None;
                    sec_sbox = None;
                    sec_name=label;
                    sec_depth=Sec_1;
                    sec_parent=None;
                    sec_child=None;
                    sec_next=None;
                    } in
                
                add_s1 ss;
                cur_s1 := Some ss;
                s1box#container_add but#contained;
                but#set_action (fun () ->
                        self#set_s1 ss;
                    );
                iter_desc seclist;
                cur_s1 := None;   
            end;
            | S2 (label,seclist) ->
            begin
                if kind = N_s1 then vx_error "VX_navigator.s1: S2 not expected";
                let butbox2 = if (!cur_s1 <> None) &&
                                 ((get_some !cur_s1).sec_vbox <> None) then
                                (get_some (get_some !cur_s1).sec_vbox)
                              else
                                err "S2 but no S1" in

                let but = new VX_button.t butbox2#container 
                                          label
                                          [
                                            ExpandX true;
                                            Text_style Bold;
                                            Text_font Helvetica;
                                            Text_size 14;
                                            Border [
                                                Frame ReliefRaised;
                                            ];
                                            Background "grey80"; 
                                          ]  in
                let ss =
                    {
                    sec_button = Some but;
                    sec_hbox = None;
                    sec_vbox = None;
                    sec_cbox = None;
                    sec_sbox = None;
                    sec_name=label;
                    sec_depth=Sec_2;
                    sec_parent= !cur_s1;
                    sec_child=None;
                    sec_next=None;
                    } in
                
                add_s2 (get_some !cur_s1) ss;
                cur_s2 := Some ss;
                butbox2#container_add but#contained;
                but#set_action (fun () ->
                        self#set_s2 ss;
                    );
                iter_desc seclist;
                cur_s2 := None;   
            end;

            | Action f ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Action: no section found";
                    in
                let s1 = get_some !cur_s1 in
                let but = get_some s1.sec_button in
                __(but#configure [ActionUU f]);
            end;
            | Text tx ->
            begin
                let t = tx.text_str in


                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Text: no section found";
                    in
                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let text = new VX_text.t cbox#container 
                                             t
                                             [
                                            ExpandX true;
                                            Rown 0;
                                            MinWidth 200;
                                             ]
                                             in
                tx.text_vx <- Some text;
                cbox#container_add text#contained;
            end;

            | Input inp ->
            begin
                let tab_cont = [|
                        [| snd inp.inp_desc;inp.inp_str;|];
                        |] in

                (*
                ** Filter generic (ActionUU) or embedded buttons (ActionSS)
                *)
                let buttons = ref [] in
                let action = ref None in
                List.iter (fun (label,attr) ->
                    match attr with
                    | ActionUU _ -> buttons := !buttons @ [(label,attr)];
                    | ActionSS _ -> action := Some attr;
                    | _ -> ();
                ) inp.inp_buts;

                let tab_attr = [
                        Background "white";
                        IpadX 5;
                        IpadY 2;
                        Text_font Times;
                        Text_style Bold;
                        Text_size 12;
                        ExpandX true] @
                        [
                        Cols [|
                            [| 
                                [(if (fst inp.inp_desc) = 0 then
                                    Align Right else Align Left);
                                 ]  @ (let w = fst inp.inp_desc in
                                       if w > 0 then [Width w] else []);
                                [Text_font Courier; 
                                 Align Left; 
                                 Mutable true;
                                 ExpandX true;
                                 Text_baseline [Color "grey60"];
                                ] @ 
                                (
                                 if !action <> None then
                                    [But [
                                        Sym S_OK;Sym S_ENTER;Sym S_ERR;
                                        ActionSSS (fun str st -> 
                                            let stat = 
                                                (match (get_some !action) with
                                                | ActionSS f -> f;
                                                | _ -> progerr "") str in
                                            (*
                                            ** Update structure entry string
                                            *)
                                            inp.inp_str <- str;
                                            if stat = std_OK 
                                                then St_Submitted
                                                else St_Failed
                                             );
                                        Frame ReliefRaised;
                                        Color "grey80";
                                        Size 20;
                                        ]]
                                    else
                                        [ActionSU (fun str ->
                                            (*
                                            ** Update structure entry string
                                            *)
                                            inp.inp_str <- str;
                                            );
                                        ]
                                  
                                        
                                );
                            |];
                        |]] in


                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Input: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                if !buttons = [] then
                begin
                        let hbox = new VX_box.h cbox#container
                                                ([
                                                 ExpandX true; IpadX 5]@
                                                 inp.inp_attr) in
                        let tab = new VX_texttable.t hbox#container 
                                                     tab_cont 
                                                     tab_attr in
    
                        inp.inp_vx <- Some tab;
                        cbox#container_add hbox#contained;
                        hbox#container_add tab#contained;
                end
                else
                begin
                        (*
                        ** Put the table and the buttons in one hbox
                        *)
                        let hbox = new VX_box.h cbox#container
                                                ([
                                                 ExpandX true; IpadX 5]@
                                                 inp.inp_attr) in

                        let tab = new VX_texttable.t hbox#container 
                                                     tab_cont 
                                                     tab_attr in
    
                        inp.inp_vx <- Some tab;

                        let labels = Array.create (List.length !buttons) "" in
                        let cols = Array.create (List.length !buttons) [] in

                        let i = ref 0 in
                        List.iter (fun (label,attr) ->
                                labels.(!i) <- label;
                                cols.(!i) <- [attr];
                                incr i;
                            ) !buttons;
                        let butattr = 
                                [
                                 IpadY 2; IpadX 10;
                                 ExpandX false;
                                 But [
                                    Frame ReliefRaised;
                                    Color "grey80";
                                 ];
                                 Cols [|cols|]] in

                        let buts = new VX_button.table hbox#container 
                                                       [| labels |]
                                                       butattr in
                                
                        hbox#container_add_s [tab#contained;buts#contained];
                        cbox#container_add hbox#contained;
                end;
            end;
            | File file ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: File: no section found";
                    in
                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let fs = new VX_popup.file_select_edit_orig 
                                          (if file.file_top then 
                                            VX_popup.Window else
                                            VX_popup.Sticky) root 
                                          cbox#container 
                                          file.file_desc
                                          file.file_path
                                          ([ExpandX true;
                                            IpadX 10;
                                            IpadY 5;
                                            But [Frame ReliefRaised;
                                                 Color "grey80"];
                                           ]@
                                           file.file_attr)
                                          in
                fs#set_action (fun () ->
                                file.file_path <- 
                                    match file.file_path with
                                    | Unix_path _ -> Unix_path 
                                                     fs#get_path;
                                    | Amoeba_path _ -> Amoeba_path
                                                       fs#get_path;
                                );
                file.file_vx <- Some fs;
                cbox#container_add fs#contained;
            end;
            | Buttons but ->
            begin
                let tab_cont = Array.map (fun row ->
                                    Array.map (fun (label,action) ->
                                               label) row) 
                               but.but_cols in
                let tab_attr = [Cols (Array.map (fun row ->
                                        Array.map (fun (label,action) ->
                                                   ([ActionUU action]@
                                                    (if but.but_group then
                                                        [Group 1] else
                                                        []))) row) 
                                      but.but_cols)] in


                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Buttons: no section found";
                    in
                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let bs = new VX_button.table
                                          cbox#container 
                                          tab_cont
                                          ([ExpandX true;
                                            AdjustX true;
                                            IpadX 10;
                                            IpadY 2;
                                            But [Frame ReliefRaised;
                                                 Color "grey80"];
                                           ]@
                                           tab_attr@
                                           but.but_attr)
                                          in
                                          

                but.but_vx <- Some bs;

                cbox#container_add bs#contained;
            end;
            | Select sel ->
            begin
                let action i = 
                    if not (List.mem i sel.sel_sel) then
                    begin
                        if not sel.sel_mutual then
                            sel.sel_sel <- sel.sel_sel @ [i]
                        else
                            sel.sel_sel <- [i];    
                        sel.sel_action i;
                    end
                    else if not sel.sel_mutual then
                        sel.sel_sel <- List.filter (fun e ->
                                        not (e = i)) sel.sel_sel;
                    begin

                    end;
                    in
                let labels = sel.sel_choices in
                let attr = 
                            [
                                ActionIU action;
                                IpadX 10;
                                IpadY 3;
                            ] @
                            (if sel.sel_mutual then [Mutual true] else []) @
                            (List.filter (fun a ->
                                            match a with
                                            | Rows _ -> true;
                                            | _ -> false) sel.sel_attr)
                    in

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Select: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let vbox = new VX_box.v cbox#container 
                                            ([IpadX 10;
                                             ] @ sel.sel_attr) in
                let label = new VX_text.t vbox#container
                                              sel.sel_desc
                                              [Text_style Bold] in
                let sl = new VX_checkbox.v
                                            vbox#container 
                                            labels
                                            attr
                                          in
                                          

                vbox#container_add_s [label#contained;
                                          sl#contained];
                sel.sel_vx <- Some sl;

                cbox#container_add vbox#contained;
            end;
            | Log log ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Log: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let lw = new VX_log.view_v
                                          cbox#container 
                                          log.log_rows
                                          ([ExpandX true;
                                            Text_font Fixed;
                                            Text_size 12;
                                           ]@
                                           log.log_attr)
                                          in
                                          

                log.log_vx <- Some lw;
                log.log_add <- Some lw#add_lines;
                log.log_clear <- Some (fun () -> lw#clear_log);
                cbox#container_add lw#contained;
            end;

            | Logedit log ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Logedit: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let lw = new VX_log.editview_v
                                          cbox#container 
                                          log.logedit_cols
                                          log.logedit_rows
                                          ([ExpandX true;
                                            Text_font Fixed;
                                            Text_size 12;
                                           ]@
                                           log.logedit_attr)
                                          in
                                          

                log.logedit_vx <- Some lw;
                log.logedit_add <- Some lw#add_lines;
                log.logedit_clear <- Some (fun () -> lw#clear_log);
                cbox#container_add lw#contained;
            end;

            | Proc ps ->
            begin
                (*
                ** Process control. Some more complex tasks...
                *)
                let status_box = ref None in

                let buts_cont = [|[|"Start";"Stop";"Status";"Clear"|]|] in

                (*
                ** Start TTY server and redirect output to log window
                ** on first start request.
                *)
                let ttyinited = ref false in
                let ttysrv = ref None in
                let ttycap = ref nilcap in


                let info str =
                    __(Bstream.stream_write !ttycap str);
                    in
                let input_event = thread_create_event () in
                let start () = 
                  let success = protects (
                    if not !ttyinited then
                    begin
                        ttyinited := true;

                        (*
                        ** Redirect output (and input) to log widget
                        *)
                        let line = ref "" in
                        
                        let tty_output =
                            (fun buf len ->
                                let str = buf_tostring buf 0 len in
                                (*
                                ** Only transfer complete lines...
                                *)
                                let tmp = ref (!line ^ str) in
                                line := "";
                                while protects (
                                    let len = String.length !tmp in
                                    let pos = String.index !tmp '\n' in
                                    let str' = String.sub !tmp 0 pos in
                                    (get_some ps.pro_log) [str'];
                                    if pos = len then
                                        tmp := "" 
                                    else
                                        tmp := String.sub !tmp (pos+1) 
                                                               (len-pos-1);
                                    ) do () done;
                                if !tmp <> "" then
                                    line := !tmp;
                            ) in
                        let tty_input = 
                            (fun buf len -> 
                                while(true) 
                                do
                                    __(thread_await input_event 0);
                                done;
                                0
                                ) in       

                        let stat,ts = tty_init 30000 2 (Some tty_input)
                                                       (Some tty_output) in
                        ttysrv := Some ts;
                        ttycap := tty_cap ts;
                        (*
                        ** Start status label box thread showing the
                        ** status of the process.
                        *)
                        __(thread_create (fun () ->
                          let last_stat = ref Exec_unknown in
                          while true
                          do
                            __(thread_delay 1 SEC);
                            match !status_box with
                            | Some sb ->
                            begin
                              match ps.pro_obj with
                              | Some obj ->
                                if obj.exec_stat <> !last_stat then
                                begin
                                  let str,color = 
                                    match obj.exec_stat with
                                    | Exec_cold -> "Cold.","grey60";
                                    | Exec_starting -> "Starting...","red";
                                    | Exec_killing -> "Killing...","red";
                                    | Exec_up -> "Up.","blue";
                                    | Exec_down -> "Down.","blue";
                                    | Exec_executed -> "Executed.","blue";
                                    | Exec_restarting -> "Restarting...","red";
                                    | Exec_unknown -> "?","green" in
                                  sb#set_text ("Status \\rightarrow  "^str);
                                  sb#configure [Foreground color];
                                  sb#update;
                                  last_stat := obj.exec_stat;
                                end;
                              | None -> 
                                if !last_stat <> Exec_cold then
                                begin
                                    sb#set_text ("Status \\rightarrow  Cold.");
                                    sb#configure [Foreground "grey50"];
                                    sb#update;
                                    last_stat := Exec_cold;
                                end;
                            end;
                            | None -> ();
                          done;         
                        ) ());  

                    end;
                    match ps.pro_obj with
                    | Some obj ->
                        if obj.exec_stat <> Exec_down then
                            info "Warning: Process already running!\n"
                        else
                        begin
                            info "Restarting process...\n";
                            ps.pro_def.exec_env <- ps.pro_def.exec_env @  
                                            [Env_cap ("TTY",!ttycap)];
                            let obj = exec_control_obj ps.pro_def info
                                                       in
                            ps.pro_obj <- Some obj;                            
                        end;
                    | None ->
                        info "Starting process...\n";
                        ps.pro_def.exec_env <- ps.pro_def.exec_env @ 
                                            [Env_cap ("TTY",!ttycap)];
                        let obj = exec_control_obj ps.pro_def info
                                                   in
                        ps.pro_obj <- Some obj;
                    ) in
                  if not success then
                    info "Process start failed!\n";
                  in 
                let stop () = 
                    match ps.pro_obj with
                    | Some obj -> 
                            obj.exec_op <- Exec_stop;
                    | None -> (); 
                    in 
                let status () = 
                    match ps.pro_obj with
                    | Some obj -> 
                        let stat,str = std_status obj.exec_proc_cap 1000 in
                        if stat <> std_OK then
                            info (sprintf "Status: failed: %s\n" (err_why stat))
                        else
                            info str;
                            
                    | None -> (); 
                    in 
            
                let buts_attr = [Cols [|[|[ActionUU start;Group 1];
                                          [ActionUU stop;Group 1];
                                          [ActionUU status];
                                          [ActionUU (fun () ->
                                                (get_some ps.pro_log) []
                                            )];
                                      |]|];
                                ] in

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Table: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in
                let vb = new VX_box.v cbox#container
                                          ([Height 160;
                                            ExpandX true]
                                           @ps.pro_attr) in
                let hb = new VX_box.h vb#container
                                          [] in
                let lb = new VX_text.label hb#container
                                               ps.pro_desc
                                               [
                                                Text_style Bold;
                                               ] in
                let sb = new VX_text.label hb#container
                                               "Status \\rightarrow  Cold."
                                               [
                                                ExpandY true;
                                                Foreground "grey50";
                                               ] in
                let lw = new VX_log.view_v
                                          vb#container 
                                          100
                                          [ExpandX true;
                                           Text_font Fixed;
                                           Text_size 12;
                                           Border [];
                                           Height 100;
                                          ]
                                          in
                                          
                let bs = new VX_button.table
                                          vb#container 
                                          buts_cont
                                          ([ExpandX false;
                                            IpadX 10;
                                            IpadY 2;
                                            But [Frame ReliefRaised;
                                                 Color "grey80"];
                                           ]@
                                           buts_attr)
                                          in
                                          

                ps.pro_log <- Some (fun sl ->
                                    if sl <> [] then lw#add_lines sl 
                                                else lw#clear_log);
                cbox#container_add vb#contained;
                hb#container_add_s [bs#contained;
                                        sb#contained;];
                vb#container_add_s [lb#contained;
                                        hb#contained;
                                        lw#contained];
                status_box := Some sb;                
            end;

            | Table tab ->
            begin
                
                let tab_attr = List.filter (fun a ->
                                match a with
                                | IpadX _ -> true;
                                | IpadY _ -> true;
                                | Background _ -> true;
                                | Foreground _ -> true;
                                | But _ -> true;
                                | Color _ -> true;
                                | _ -> false) tab.tab_attr in
                let main_attr = List.filter (fun a ->
                                match a with
                                | IpadX _ -> true;
                                | IpadY _ -> true;
                                | OpadX _ -> true;
                                | OpadY _ -> true;
                                | _ -> false) tab.tab_attr in

                let row_attr = tab.tab_rows in
                let col_attr = Array.map (fun row ->
                                Array.map (fun (str,attr) ->
                                        attr) row ) tab.tab_cols in
                let col_text = Array.map (fun row ->
                                Array.map (fun (str,attr) ->
                                        str) row ) tab.tab_cols in
                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Table: no section found";
                    in

                let vb  = new VX_box.v cbox#container main_attr in
                let lb  = new VX_text.label vb#container tab.tab_desc 
                                    [Text_style Bold] in 
                let tb  = new VX_texttable.t
                                          vb#container 
                                          col_text
                                          (
                                            tab_attr @
                                            [OpadX 10] @
                                            [Rows row_attr] @
                                            [Cols col_attr]
                                          )
                                          in
                                          
                    
                tab.tab_vx <- Some tb;
                tab.tab_set <- Some (fun row_i col_j (str,al) ->
                            tb#configure_col row_i col_j al;
                            tb#set_text row_i col_j str;
                            );
                tab.tab_get <- Some (fun row_i col_j ->
                            tb#get_text row_i col_j
                            );

                cbox#container_add vb#contained;
                vb#container_add_s [lb#contained;tb#contained];
            end;

            | Draw draw ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Table: no section found";
                    in

                let dw = new VX_draw.t cbox#container 
                                          ([
                                           ]@
                                           draw.draw_attr)
                                          in
                                          

                draw.draw_vx <- Some dw;
                draw.draw_add <- Some (dw#add_path);
                draw.draw_del <- Some (dw#delete_path);
                draw.draw_print <- Some (fun filename ->
                                            VX_ps.print_eps 
                                            filename
                                            1.0
                                            dw#contained);
                cbox#container_add dw#contained;
            end;
            | Value value ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Table: no section found";
                    in

                let s1 = get_some !cur_s1 in
                let cbox = get_cbox s1 in

                (*
                ** Filter some special attributes
                *)
                let hb_attr = ref [] in
                let lb_attr = ref [] in
                List.iter (fun attr ->
                        match attr with
                        | Size v -> lb_attr := !lb_attr @ [Width v];
                        | _  -> hb_attr := !hb_attr @ [attr]; 
                    ) value.val_attr; 

                let hbox = new VX_box.h cbox#container
                                        ([
                                            ExpandX true;
                                            IpadX 10;
                                         ] @ 
                                         !hb_attr) in

                let lb = new VX_text.label hbox#container value.val_desc 
                                       ([
                                        Text_style Bold;
                                        ExpandY true;
                                        ]@
                                        !lb_attr) in
                let vb = new VX_text.label hbox#container 
                                       ((value.val_print value.val_min)^
                                        value.val_unit
                                       )
                                       [
                                        Text_style Roman;
                                        Text_font Fixed;
                                        ExpandY true;
                                       ] in
                                    
                let valmin = value.val_min in
                let valmax = value.val_max in
                let valsmall_step = value.val_res in
                let vallarge_step = value.val_step in
                let valattr = [
                                ExpandX true;
                                But [Frame ReliefRaised;
                                     Color "grey80"];
                                ActionFU (fun v ->
                                        let str = value.val_print v in
                                        vb#set_text (str^value.val_unit);
                                        value.val_action v;
                                    );
                              ] in
                let vl = new VX_slider.val_hf
                                          hbox#container 
                                          valmin
                                          valmax
                                          valsmall_step
                                          vallarge_step
                                          valattr
                                          in
                                                          
                value.val_config <- Some (fun () -> 
                                        vl#val_config
                                        value.val_min
                                        value.val_max
                                        value.val_res
                                        value.val_step);
                                        
                value.val_set <- Some (fun v ->
                                let vmin = value.val_min in
                                let vmax = value.val_max in
                                let v' = (v -. vmin )/. (vmax -. vmin) in
                                vl#set_pos v';
                                vl#update;
                                let str = value.val_print v in
                                vb#set_text (str^value.val_unit);
                                );
                value.val_vx <- Some vl;
                cbox#container_add hbox#contained;
                hbox#container_add_s [
                                        lb#contained;
                                        vl#contained;
                                        vb#contained;
                                     ];
            end;
            | Space sp ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Space: no section found";
                    in

                let hb = new VX_box.h cbox#container 
                                      [ExpandX true;
                                       Height sp]
                                          in                                          
                cbox#container_add hb#contained;
            end;
            | Ruler sp ->
            begin

                let cbox = 
                    if !cur_s2 <> None then
                    begin
                        let s2 = get_some !cur_s2 in
                        get_cbox s2
                    end
                    else if !cur_s1 <> None then
                    begin
                        let s1 = get_some !cur_s1 in
                        get_cbox s1 
                    end
                    else
                        vx_error "VX_navigator: Ruler: no section found";
                    in

                let hb = new VX_box.h cbox#container 
                                      [ExpandX true;
                                       Background "black";
                                       Height sp]
                                          in                                          
                cbox#container_add hb#contained;
            end;

            | _ -> warn "descriptor ignored";
            ) descl in
        iter_desc desc;

        if (kind <> N_s1 && kind <> N_s1_view) then
        begin
            (*
            ** Update button bar alignment
            *)
            let fillbox = get_some fill_vbox in
            let sz' = s1box#size_request in
            __(fillbox#configure [Height sz'.requested_height]);
        end
 
    (*
    ** Load a new S1 section. The first S2 of S1 (if any) is activated
    ** and loaded.
    *)
    method set_s1 ss =
        let ignore = 
            if last_s1 <> None then
            begin
                let last_ss = get_some last_s1 in
                if not (last_ss == ss) then
                begin
                    let but = get_some last_ss.sec_button in
                    __(but#configure [Foreground "black"]);
                    but#update;
                    false;
                end
                else
                    true
            end
            else
                false in

        last_s1 <- Some ss;

        if not ignore then
        begin
            let but = get_some ss.sec_button in
            __(but#configure [Foreground "red"]);
            but#update;

            match kind with
            | N_s1 ->
            begin
                (*
                ** Update content frame
                *)
                let framebox = get_some frame_vbox in
                if init then
                begin
                    if (ss.sec_cbox <> None) then
                    begin
                        let cbox = get_some ss.sec_cbox in
                        framebox#container_add cbox#contained;
                    end;
                end
                else
                begin
                    if (ss.sec_cbox <> None) then
                    begin
                        let cbox = get_some ss.sec_cbox in
                        framebox#container_exchange 0 cbox#contained;
                        cbox#update;
                        cbox#realize;
                        cbox#show;
                    end;
                end;
                framebox#update;
            end;
            | N_s1_view ->
            begin
                (*
                ** Update content frame
                *)
                let framebox = get_some frame_vbox in
                if init then
                begin
                    if (ss.sec_sbox <> None) then
                    begin
                        let sbox = get_some ss.sec_sbox in
                        framebox#container_add sbox#contained;
                    end;
                end
                else
                begin
                    if (ss.sec_sbox <> None) then
                    begin
                        let sbox = get_some ss.sec_sbox in
                        framebox#container_exchange 0 sbox#contained;
                        sbox#update;
                        sbox#realize;
                        sbox#show;
                    end;
                end;
                framebox#update;
            end;
            | N_s12 | N_s123 
            | N_s12_view | N_s123_view ->
            begin
                let butbox2 = get_some ss.sec_vbox in
                let s2box = get_some s2_vbox in
                if init then
                begin
                    s2box#container_add butbox2#contained;
                end 
                else
                begin
                    s2box#container_exchange 0 butbox2#contained;
                    butbox2#update;
                    butbox2#realize;
                    butbox2#show;
                end;
                s2box#update;
                (*
                ** Update S2 and content frame.
                *)
                if ss.sec_child <> None then
                begin
                    self#set_s2 (get_some ss.sec_child);
                end;
            end;
        end;

    (*
    ** Load new S2 section.
    *)

    method set_s2 ss =
        let ignore = 
            if last_s2 <> None then
            begin
                let last_ss = get_some last_s2 in
                if not (last_ss == ss) then
                begin
                    let but = get_some last_ss.sec_button in
                    __(but#configure [Foreground "black"]);
                    but#update;
                    false
                end
                else
                    true
            end
            else
                false in

        if not ignore then
        begin
            let but = get_some ss.sec_button in
            __(but#configure [Foreground "red"]);
            but#update;
            (*
            ** Update content frame
            *)
            let framebox = get_some frame_vbox in
            if init then
            begin
                match kind with
                | N_s12 | N_s123 ->
                    if (ss.sec_cbox <> None) then
                    begin
                        let cbox = get_some ss.sec_cbox in
                        framebox#container_add cbox#contained;
                    end;
                | N_s12_view | N_s123_view ->
                    if (ss.sec_cbox <> None) then
                    begin
                        let sbox = get_some ss.sec_sbox in
                        framebox#container_add sbox#contained;
                    end;
                | _ -> progerr "";
            end
            else
            begin
                match kind with
                | N_s12 | N_s123 ->
                    if (ss.sec_cbox <> None) then
                    begin
                        let cbox = get_some ss.sec_cbox in
                        framebox#container_exchange 0 cbox#contained;
                        cbox#update;
                        cbox#realize;
                        cbox#show;
                    end;
                | N_s12_view | N_s123_view ->
                    if (ss.sec_cbox <> None) then
                    begin
                        let sbox = get_some ss.sec_sbox in
                        framebox#container_exchange 0 sbox#contained;
                        sbox#update;
                        sbox#realize;
                        sbox#show;
                    end;
                | _ -> progerr "";
            end;
            framebox#update;
            last_s2 <- Some ss;
        end;


    (*
    ** Get content container for a section S1 (first section has index 0)
    *)

    method get_s1 num =
        let s1 = List.nth sections num in
        (get_some s1.sec_cbox)

    method size_allocate a b c d =
        __(super#size_allocate a b c d);
        List.iter (fun upd -> upd#update) updates;
        true

end     

class s1 root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s1 root parent desc attributes as super
    initializer
        self#set_name "navigator_s1"  


end

class s12 root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s12 root parent desc attributes as super
    initializer
        self#set_name "navigator_s12"  

end

class s123 root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s123 root parent desc attributes 
    initializer
        self#set_name "navigator_s123"  
end

class s1_view root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s1_view root parent desc attributes as super
    initializer
        self#set_name "navigator_s1_view"  


end

class s12_view root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s12_view root parent desc attributes as super
    initializer
        self#set_name "navigator_s12_view"  

end

class s123_view root parent (desc: descriptor list)  attributes =
    object (self)
    inherit orig N_s123_view root parent desc attributes 
    initializer
        self#set_name "navigator_s123_view"  
end

