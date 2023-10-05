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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     2003.0.0
**    $VERSION:     1.07
**
**    $INFO:
**
**  Amoeba-Unix filesystem transfer util (xafs)
**
**
**  TODO:
**      copy_from_afs: file target
**
**    $ENDOFINFO
**
*)




open Xtypes
open WX_types
open WX_tree
open Unix


open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Cap_env
open Thread
open Printf
open Sema

open Xafs_file
open Xafs_widget



(*
** Select mode flag
*)

let sel_add = ref false



(*
** Check for select mode (add to = false), and unselect already
** selected objects, both files and directory lists.
*)
let unsel_files fl =
    List.iter (fun file ->
        file.file_label#configure [Background "lightblue"];
        file.file_selected <- false;
        ) !fl;  
    fl := []
let unsel_dirs dl =
    List.iter (fun dir ->
        dir.dir_label#configure [Background "lightblue"];
        dir.dir_selected <- false;
        ) !dl;
    dl := []

let sel_file_check fl =
    if (!sel_add = false) then
    begin
        unsel_files fl;
        (*
        ** and the dirs list, too
        *)
        if (fl == sel_objs_afs) then unsel_dirs sel_dirs_afs;
        if (fl == sel_files_unix) then unsel_dirs sel_dirs_unix;
    end

let sel_dir_check dl =
    if (!sel_add = false) then
    begin
        unsel_dirs dl;
        (*
        ** and the file list, too
        *)
        if (dl == sel_dirs_afs) then unsel_files sel_objs_afs;
        if (dl == sel_dirs_unix) then unsel_files sel_files_unix;
    end

let check_input_sel () =
    if (!sel_objs_afs = [] &&
        !sel_dirs_afs = []) then
    begin
        cur_path := "";
        cur_sel  := "";
        input_path#set_string "";
        input_sel#set_string "";
    end
(*
** A file was selected. Mark it.
*)

let select_file file fl =
    if (file.file_selected = false ) then
    begin
        sel_file_check fl;
        file.file_label#configure [Background "steelblue"];
        file.file_selected <- true;
        let tpath = if file.file_path = "/"
                        then
                            "/"^file.file_name
                        else
                            file.file_path^"/"^file.file_name
                    in

        (*
        ** Add the file to the selection list
        *)
        if (List.mem file !fl) = false then
            fl := !fl @ [file];
        cur_sel := tpath;
        input_sel#set_string !cur_sel;
    end else
    begin
        file.file_label#configure [Background "lightblue"];
        file.file_selected <- false;
        (*
        ** Remove file from the selected list
        *)
        fl := List.filter 
                    ( fun e ->
                        if e <> file then 
                            true
                        else
                            false;       
                    ) !fl ;
        check_input_sel ();
    end
  
(*
** A directory was selected. Mark it.
*)

let select_dir dir dl =
#if 0
    if (dir.dir_name <> "") then
#endif
    if (dir.dir_selected = false ) then
    begin
        sel_dir_check dl;
        dir.dir_label#configure [Background "steelblue"];
        dir.dir_selected <- true;

        (*
        ** Add the dir to the selection list
        *)
        if (List.mem dir !dl) = false then
            dl := !dl @ [dir];
        if (dl == sel_dirs_afs) then
        begin
            let tpath = if dir.dir_path = "/"
                        then
                            "/"^dir.dir_name
                        else
                            dir.dir_path^"/"^dir.dir_name
                    in

            cur_path := tpath;
            cur_sel := tpath;
            input_path#set_string !cur_path;
            input_sel#set_string !cur_sel;
        end;
    end else
    begin
        dir.dir_label#configure [Background "lightblue"];
        dir.dir_selected <- false;
        (*
        ** Remove dir from the selected list
        *)
        dl := List.filter 
                    ( fun e ->
                        if e <> dir then 
                            true
                        else
                            false;       
                    ) !dl ;
        check_input_sel ();
    end




(*
** Load a directory and build the subtree. Returns descriptor list
** for branches (sub directories). 
*)

let make_sub_dir dir tree f =
    let (dirs,files) = f dir in

    (*
    ** Determine the filesystem type and the selection lists.
    *)

    let fs_type = if (f == load_afs_dir) then
                    AFS
                  else
                    UNIX 
    in
    let fl = match fs_type with
                        | AFS   -> sel_objs_afs ;
                        | UNIX  -> sel_files_unix;
    in
    let dl = match fs_type with
                        | AFS   -> sel_dirs_afs ;
                        | UNIX  -> sel_dirs_unix;
    in
 
    let dir_map =
        List.map ( fun se ->
                    let s,e = se in
                    let tree' = (new WX_tree.t tree#container []) in
                    let label' = (new WX_label.t tree#container s 
                                    (if e <> std_OK 
                                        then [Foreground "blue"] else [])) in
                    let dd =
                    {
                        dir_name = s;
                        dir_path = dir;
                        dir_tree = tree';
                        dir_label = label';
                        dir_branch = (branch true 
                                        label'#contained 
                                        tree'#contained
                                     );
                        dir_selected = false;                         
                        dir_open = false;
                    } in
                    label'#configure [ Bindings [
                                     ButtonPress, (fun _ -> select_dir dd dl);
                                     ]];

                    dd
                 ) dirs 
    in

    let file_map =
        List.map ( fun se ->
                    let s,e = se in
                    let label' = (new WX_label.t tree#container s 
                                    (if e <> std_OK then [Foreground "blue"]
                                        else [])) in
                    let fd =
                    {
                        file_name = s;
                        file_path = dir;
                        file_label = label';
                        file_leaf = (leaf 0 label'#contained);
                        file_selected = false;                         
                    } in
                    label'#configure [ Bindings [
                                     ButtonPress, (fun _ -> select_file fd fl);
                                     ]];

                    fd
                 ) files
    in
    let map = 
            ( List.map (fun d -> 
                            d.dir_branch
                        ) dir_map
            ) @
            ( List.map (fun f -> 
                            f.file_leaf
                        ) file_map
            ) 
    in
    tree#set_desc map;
    dir_map

(*
** Open/Close event for directory branches
*)
  
let rec set_notify tree treelist f =

    (*
    ** Determine the filesystem type and the selection lists.
    *)

    let fs_type = if (f == load_afs_dir) then
                    AFS
                  else
                    UNIX
    in
    let fl = match fs_type with
                        | AFS   -> sel_objs_afs ;
                        | UNIX  -> sel_files_unix;
    in
    let dl = match fs_type with
                        | AFS   -> sel_dirs_afs ;
                        | UNIX  -> sel_dirs_unix;
    in
    
    (*
    ** Is path s a subdir of p ?
    ** F.e. s="/tmp/dir1"
    **      p="/tmp"  -> true
    **
    *)

    let check_path p s =
        let pl = Str.split (Str.regexp "/") p in
        let sl = Str.split (Str.regexp "/") s in
        let pn = List.length pl in
        let eq = ref true in
        for i = 0 to pn-1
        do
            if (List.nth pl i) <> (List.nth sl i) then
                eq := false;
        done;                     
        !eq                                        
    in                


    let notify leaf = 

        List.iter ( fun d ->
                    let b = d.dir_branch in
                    if (leaf = b) then
                    begin
                        let path = if d.dir_path = "/" then 
                                        "/"^d.dir_name 
                                   else
                                     d.dir_path^"/"^d.dir_name 
                        in 

                        if (d.dir_open = false) then
                        begin
                            d.dir_label#configure [Foreground "red"];
                            let dirmap = make_sub_dir 
                                                 path
                                                 d.dir_tree f in
                            d.dir_open <- true;
                            d.dir_label#configure [Foreground "black"];

                            set_notify d.dir_tree dirmap f;                    
                        end
                        else
                        begin

                            d.dir_tree#destroy_desc;   
                            d.dir_open <- false;


                            (*
                            ** Remove all selection entries from and
                            ** below this directory.
                            *)

                            dl := List.filter 
                                        ( fun d ->
                                        
                                          let dpath = if d.dir_path = "/" then 
                                                       "/"^d.dir_name 
                                                      else
                                                       d.dir_path^"/"^d.dir_name 
                                          in 

                                          if (check_path path dpath) = false
                                          then
                                                true
                                          else
                                                false;
                                        ) !dl;
                            fl := List.filter 
                                  ( fun f ->
                                        
                                    let fpath = if f.file_path = "/" then 
                                                    "/"^f.file_name 
                                                else 
                                                    f.file_path^"/"^f.file_name 
                                    in 

                                    if (check_path path fpath) = false
                                    then
                                        true
                                    else
                                        false;
                                   ) !fl;
                        end;
                    end;
                  ) treelist
    in
    tree#set_notify (fun a -> 
                service_fun := (fun () ->
                                    notify a);
                sema_up service_sema
        )


let _ =
  (*
  ** Start service threads
  *)
  for i = 1 to 4
  do
    ignore(thread_create service_thr ());
  done;
 
  (*
  ** Build up the main window and the widgets
  *)

  let closewindow () = 
        Display.closeDisplay (top#display);
  in

  (*
  ** Set the action handler for the buttons.
  *)

  but_tab#set_action 0 0 ( fun () -> closewindow () );


  
  but_tab#set_action 2 2 show_info;
  but_tab#set_action 3 2 show_status;
  but_tab#set_action 1 0 (fun () -> Gc.compact ();
                                    log_msg := "";
                                    print "Ready.\n");

  but_tab#set_action 0 1 delfile;
  but_tab#set_action 1 1 deldir;
  but_tab#set_action 2 1 delobj;
  but_tab#set_bistable 3 1 true;
  but_tab#set_action 3 1 (fun () ->
            match del_flags.del_destroy with
            | false -> del_flags.del_destroy <- true;
                       but_tab#set_state 3 1 true;
                       doing "Setting delete and destroy flag";
            | true -> del_flags.del_destroy <- false;
                       but_tab#set_state 3 1 false;
                       doing "Resetting delete and destroy flag";
    );
  but_tab#set_bistable 3 0 true;
  but_tab#set_action 3 0 (fun () ->
            match !verbose with
            | false -> verbose := true;
                       but_tab#set_state 3 0 true;
                       doing "Setting verbose flag";
            | true ->  verbose := false;
                       but_tab#set_state 3 0 false;
                       doing "Resetting verbose flag";
    );
  but_tab#set_action 0 2 copy;
  but_tab#set_action 1 2 paste;

  (*
  ** Copy buttons
  *)
  cop_tab#set_action 0 0 copy_from_afs;
  cop_tab#set_action 0 3 copy_from_unix;

  cop_tab#set_bistable 0 1 true;
  cop_tab#set_bistable 0 2 true;
  
  cop_tab#set_action 0 1 (fun () ->
            match copy_flags.copy_overwrite with
            | false -> copy_flags.copy_overwrite <- true;
                       cop_tab#set_state 0 1 true;
                       doing "Setting copy overwrite flag";
            | true -> copy_flags.copy_overwrite <- false;
                       cop_tab#set_state 0 1 false;
                       doing "Resetting copy overwrite flag";
    );
  cop_tab#set_action 0 2 (fun () ->
            match copy_flags.copy_destroy with
            | false -> copy_flags.copy_destroy <- true;
                       cop_tab#set_state 0 2 true;
                       doing "Setting copy destroy on overwrite flag";
            | true -> copy_flags.copy_destroy <- false;
                       cop_tab#set_state 0 2 false;
                       doing "Resetting copy destroy on overwrite flag";
    );

  (*
  ** Setup root directory 
  *)



  let tree_root_left = (new WX_tree.t tree_left#container []) in 
  let label_root_left = (new WX_label.t tree_left#container "AFS" []) in

  let dir_dns_root =
                    {
                        dir_name = "";
                        dir_path = "/";
                        dir_tree = tree_root_left;
                        dir_label = label_root_left;
                        dir_branch = (branch true 
                                        label_root_left#contained 
                                        tree_root_left#contained
                                     );
                        dir_selected = false;                         
                        dir_open = false;
                    } 
  in
  label_root_left#configure [ Bindings [
                              ButtonPress, (fun _ -> select_dir dir_dns_root
                                                            sel_dirs_afs);
                                     ]];
  tree_left#set_desc [dir_dns_root.dir_branch];
  set_notify tree_left [dir_dns_root] load_afs_dir;

  let tree_root_right = (new WX_tree.t tree_right#container []) in 
  let label_root_right = (new WX_label.t tree_right#container "UNIX" []) in

  let dir_unix_root =
                    {
                        dir_name = "";
                        dir_path = "/";
                        dir_tree = tree_root_right;
                        dir_label = label_root_right;
                        dir_branch = (branch true 
                                        label_root_right#contained 
                                        tree_root_right#contained
                                     );
                        dir_selected = false;                         
                        dir_open = false;
                    } 
  in
  label_root_right#configure [ Bindings [
                              ButtonPress, (fun _ -> select_dir dir_unix_root
                                                            sel_dirs_unix);
                                     ]];
  tree_right#set_desc [dir_unix_root.dir_branch];
  set_notify tree_right [dir_unix_root] load_unix_dir;


#if 0
  let dirmap_left = make_sub_dir "/" tree_left load_afs_dir in
  set_notify tree_left dirmap_left load_afs_dir; 

  let dirmap_right = make_sub_dir "/" tree_right load_unix_dir in
  set_notify tree_right dirmap_right load_unix_dir; 
#endif

  vbar1#container_add_s  [
                            hbar_comm1#contained;
                            hbar_path#contained;
                            hbar_sel#contained;
                            hbar3#contained;
                            hbar4#contained;
                            hbar2#contained;
                         ];

  hbar_comm1#container_add_s [
                            but_tab#contained;
                            logo_hbar#contained;
                        ];

  hbar_path#container_add_s [
                            input_path#contained;
                            make_newdir#contained;
                        ];

  hbar_sel#container_add_s [
                            input_sel#contained;
                            make_rename#contained;
                        ];

  hbar2#container_add_s [
                            hbar_trees#contained;
                        ];
    
  hbar_trees#container_add_s [
                            vbar_left#contained;
                            cop_tab#contained;
                            vbar_right#contained
                        ];

  hbar_left#container_add_s [
                            viewport_left#contained;
                            scrollbar_left_y#contained
                        ];
  hbar_right#container_add_s [
                            viewport_right#contained;
                            scrollbar_right_y#contained
                        ];

  vbar_left#container_add_s [
                            hbar_left#contained;
                            scrollbar_left_x#contained;
                        ];
  vbar_right#container_add_s [
                            hbar_right#contained;
                            scrollbar_right_x#contained;
                        ];


  hbar3#container_add_s [viewport#contained; vscroll#contained];
  viewport#container_add (log_text#contained);

  hbar4#container_add_s [do_text#contained];
  

  make_newdir#set_action Xafs_file.mkdir;
  make_rename#set_action Xafs_file.rename;

  (*
  ** Configure the scrollbar container. Scrolling with arrow keys
  ** and Page Up/Down keys.
  ** The pressed shift key enable select add to mode.
  *)

  hbar_left#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady_left#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady_left#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady_left#down);
      Key(XK.xk_Up,0),(fun _ -> ady_left#up);
      KeyPress,(fun _ ->  
            if (!WX_types.key_sym = XK.xk_Shift_L) then
                sel_add := true;
        );
      KeyRelease,(fun _ ->  
            if (!WX_types.key_sym = XK.xk_Shift_L) then
                sel_add := false;
        );
    ]];

  hbar_right#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady_right#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady_right#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady_right#down);
      Key(XK.xk_Up,0),(fun _ -> ady_right#up);
      KeyPress,(fun _ ->  
            if (!WX_types.key_sym = XK.xk_Shift_L) then
                sel_add := true;
        );
      KeyRelease,(fun _ ->  
            if (!WX_types.key_sym = XK.xk_Shift_L) then
                sel_add := false;
        );
    ]];

  hbar3#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady#down);
      Key(XK.xk_Up,0),(fun _ -> ady#up);
    ]];

  viewport_left#container_add tree_left#contained;
  viewport_right#container_add tree_right#contained;

  (*
  ** The logo
  *)
  logo_hbar#container_add logo#contained;
  logo#set_text 0 0 "BSS";
  logo#set_text 1 0 "LAB";
  logo#set_style_cell 0 0 [Background "blue";Foreground "white";
                            BorderWidth 0];
  logo#set_style_text 0 0 [Background "blue";
                             Foreground "white";];
  logo#set_style_cell 1 0 [Foreground "blue";
                        BorderWidth 0];
  logo#set_style_text 1 0 [Foreground "blue";
                        BorderWidth 0];

  logo#set_font 0 0 Fonts.Helvetica.Bold.s24;
  logo#set_font 1 0 Fonts.Helvetica.Bold.s24;


  (*
  ** Configure focus actions
  *)
  input_path#configure [ 
                        Bindings 
                        [
                            ButtonPress, (fun _ -> 
                                input_path#focus);
                        ]
        ];

  input_sel#configure [ 
                        Bindings 
                        [
                            ButtonPress, (fun _ -> 
                                input_sel#focus);
                        ]
        ];

  hbar_trees#configure [ 
                        Bindings 
                        [
                            ButtonPress, (fun _ -> 
                                top#focus);
                            EnterWindow, (fun _ -> 
                                top#focus);
                        ]
        ];

  top#configure [ 
                        Bindings 
                        [
                          FocusIn,(fun _ ->
                                    if (X.getInputFocus top#display).gif_win 
                                         == top#window then
                                    begin
                                        top#focus
                                    end;
                            );

                          ButtonPress, (fun _ -> 
                                        top#focus);
                        ]
        ];

  top#container_add vbar1#contained;


  top#show;

  top#setWM_NAME "Amoeba File System tool";
  try 
      loop ()
  with 
      Exit -> ()

