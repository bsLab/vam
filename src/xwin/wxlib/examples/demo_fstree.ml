
open Xtypes
open WX_types
open WX_tree
  
open Unix


let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root [MinWidth 100; MinHeight 100; MaxHeight 400]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container []
let hbar1 = new WX_bar.h vbar1#container [] 
let hbar2 = new WX_bar.h vbar1#container [Relief ReliefSunken] 

(* An exit button *)
let exitbut = new WX_button.with_label hbar1#container "Exit" []

(* 
** Because the file tree can be larger than the widget, we need
** a scrollbar together wit a viewport widget controlled by 
** adjust widgets.
*)

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()

let viewport1 = new WX_viewport.t hbar2#container adx ady []
let scrollbar1 = new WX_scrollbar.v hbar2#container ady []


(*
** main entry directory tree
*)

let tree1 = new WX_tree.t viewport1#container [ExpandX true]

(*
** Directory entry (branches)
*)

type dir_desc = {
    dir_name : string;
    dir_path : string;
    dir_tree : WX_tree.t;
    dir_label : WX_label.t;
    dir_branch : WX_tree.node;
    mutable dir_selected : bool;
    mutable dir_open : bool;
}

(*
** File entry
*)

type file_desc = {
    file_name : string;
    file_path : string;
    file_label : WX_label.t;
    file_leaf : WX_tree.node;
    mutable file_selected : bool;
}

(*
** A file was selected. Mark it.
*)

let select_file file =
    if (file.file_selected = false ) then
    begin
        file.file_label#configure [Background "steelblue"];
        file.file_selected <- true
    end else
    begin
        file.file_label#configure [Background "lightblue"];
        file.file_selected <- false
    end

(*
** A directory was selected. Mark it.
*)

let select_dir dir =
    if (dir.dir_selected = false ) then
    begin
        dir.dir_label#configure [Background "steelblue"];
        dir.dir_selected <- true
    end else
    begin
        dir.dir_label#configure [Background "lightblue"];
        dir.dir_selected <- false
    end

(*
** Load a directory. Return sub dir and file list.
*)

let load_dir dirname =
    let filenames' = Sort.list (<=) (Utils.list_dir dirname) in
    let subdirs = List.fold_left (fun files filename ->
          if filename <> "." && filename <> ".." then
            let fullname = Filename.concat dirname filename in
            let stats = Unix.lstat fullname in
            if stats.st_kind = S_DIR then filename::files else
              files
          else files
      ) [] filenames' in
    let filenames = List.filter ( fun filename ->
                    if filename <> "." && filename <> ".."  &&
                    ( 
                        let fullname = Filename.concat dirname filename in
                        (Unix.lstat fullname).st_kind <> S_DIR
                    )
                    then
                        true
                    else
                        false
                    ) filenames' in

    (subdirs,filenames)


(*
** Load a directory and build the subtree. Returns descriptor list
** for branches (sub directories). 
*)

let make_sub_dir dir tree =
    let (dirs,files) = load_dir dir in

    let dir_map =
        List.map ( fun s ->
                    let tree' = (new WX_tree.t tree#container []) in
                    let label' = (new WX_label.t tree#container s []) in
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
                                     ButtonPress, (fun _ -> select_dir dd);
                                     ]];

                    dd
                 ) dirs 
    in

    let file_map =
        List.map ( fun s ->
                    let label' = (new WX_label.t tree#container s []) in
                    let fd =
                    {
                        file_name = s;
                        file_path = dir;
                        file_label = label';
                        file_leaf = (leaf 0 label'#contained);
                        file_selected = false;                         
                    } in
                    label'#configure [ Bindings [
                                     ButtonPress, (fun _ -> select_file fd);
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
  
let rec set_notify tree treelist =
    let notify leaf = 
        List.iter ( fun d ->
                    let b = d.dir_branch in
                    if (leaf = b) then
                    begin
                        Printf.printf "notify %s" (d.dir_path^"/"^d.dir_name);
                        print_newline ();

                        if (d.dir_open = false) then
                        begin
                            let dirmap = make_sub_dir 
                                                (d.dir_path^"/"^d.dir_name)
                                                 d.dir_tree in
                            set_notify d.dir_tree dirmap;                    
                            d.dir_open <- true;
                        end
                        else
                        begin
                            d.dir_tree#destroy_desc;  
                            d.dir_open <- false;
                        end;
                    end;
                  ) treelist
    in
    tree#set_notify notify
    

let _ =
  let closewindow () = 
        Display.closeDisplay (top#display);
        raise Exit;
  in

  exitbut#set_action ( fun () -> closewindow () );

  (*
  ** Load entry directory 
  *)
  let dirmap = make_sub_dir "/home/sbosse" tree1 in
  set_notify tree1 dirmap;

  vbar1#container_add_s  [hbar1#contained;hbar2#contained];

  hbar1#container_add_s [exitbut#contained];
  hbar2#container_add_s [
                            viewport1#contained;
                            scrollbar1#contained
                        ];

  (*
  ** Configure the scrollbar container. Scrolling with arrow keys
  ** and Page Up/Down keys.
  *)

  hbar2#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady#down);
      Key(XK.xk_Up,0),(fun _ -> ady#up);
    ]];


  viewport1#container_add tree1#contained;

  top#container_add vbar1#contained;
  top#show;

  try 
      loop ()
  with 
      Exit -> ()

