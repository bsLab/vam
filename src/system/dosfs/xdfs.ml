(*
** Amoeba DOS Filesystem transfer util (dfs)
*)

let version = "1.01"

open Xtypes
open WX_types
open WX_tree
open Unix

open Fat
open Dosfs

open Amoeba
open Ar
open Dir
open Name
open Stderr
open Cap_env


(*************************************** TEST *******************************)



let _ =
    Db.set_level 1

let _ = get_env_cap "ROOT"

let home_path = "/contrib/sbosse"


let hostcap = 
    let stat,cap = name_lookup "/hosts/flash01" in
    if (stat <> std_OK) then
        failwith ("Can't lookup host flash01: "^(err_why stat));
    cap
              

(*
let (err,doscap) = dir_lookup ~root:hostcap ~name:"/ldisk@dos:01" 
*)

let (err,doscap) = dir_lookup ~root:hostcap ~name:"/ldisk@dos:00" 


let dos_vol = vol_init doscap 


(******************************* TEST END **********************************)




type file_system = Unix | Dos

let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root [MinWidth 100; MinHeight 100; MaxHeight 400]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container []
let hbar1 = new WX_bar.h vbar1#container [] 
let hbar2 = new WX_bar.h vbar1#container [] 
let hbar_left = new WX_bar.h vbar1#container [Relief ReliefSunken] 
let hbar_right = new WX_bar.h vbar1#container [Relief ReliefSunken] 


(*
** The buttons 
*)
let exitbut = new WX_button.with_label hbar1#container "Exit" []
let showselbut = new WX_button.with_label hbar1#container "Show Selection" []
let copyfromdosbut = new WX_button.with_label hbar1#container "Copy from DOS" []

(* 
** Because the file tree can be larger than the widget, we need
** a scrollbar together wit a viewport widget controlled by 
** adjust widgets.
*)

let adx_left = new WX_adjust.t ()
let ady_left = new WX_adjust.t ()
let adx_right = new WX_adjust.t ()
let ady_right = new WX_adjust.t ()

let viewport_left = new WX_viewport.t hbar_left#container 
                                      adx_left ady_left []
let scrollbar_left = new WX_scrollbar.v hbar_left#container 
                                      ady_left []

let viewport_right = new WX_viewport.t hbar_right#container 
                                      adx_right ady_right []
let scrollbar_right = new WX_scrollbar.v hbar_right#container 
                                      ady_right []


(*
** main entry directory tree
*)

let tree_left = new WX_tree.t viewport_left#container [ExpandX true]
let tree_right = new WX_tree.t viewport_right#container [ExpandX true]

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
** List of all selected dos files and directories
*)

let sel_files_dos = ref []
let sel_dirs_dos = ref []

(*
** List of all selected unix files and directories
*)

let sel_files_unix = ref []
let sel_dirs_unix = ref []


(*
** A file was selected. Mark it.
*)

let select_file file fl =
    if (file.file_selected = false ) then
    begin
        file.file_label#configure [Background "steelblue"];
        file.file_selected <- true;

        (*
        ** Add the file to the selection list
        *)
        if (List.mem file !fl) = false then
            fl := !fl @ [file];
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
    end

(*
** A directory was selected. Mark it.
*)

let select_dir dir dl =
    if (dir.dir_selected = false ) then
    begin
        dir.dir_label#configure [Background "steelblue"];
        dir.dir_selected <- true;

        (*
        ** Add the dir to the selection list
        *)
        if (List.mem dir !dl) = false then
            dl := !dl @ [dir];
        
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
    end


(*
** Load a unix directory. Return sub dir and file list.
*)

let load_unix_dir dirname =
    Db.Pr.ss 1 "load_unix_dir" dirname;

    let filenames' = Utils.list_dir dirname in
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
    (Sort.list (<=) subdirs,Sort.list (<=) filenames)

(*
** Load a dos directory. Return sub dir and file list.
*)

let load_dos_dir dirname =
    Db.Pr.ss 1 "load_dos_dir" dirname;

    let dire = read_dir dos_vol dirname in
    let dirl = list_dir dire in
 
    let subdirs = 
        List.fold_left (fun l d ->
            let lnam = d.dir_long_name in 
            if (lnam <> "." && lnam <> "..") then
            begin
                if (d.dir_attr land attr_DIRECTORY = attr_DIRECTORY) then
                    l@[lnam]
                else
                    l
            end
            else
                l
        ) [] dirl 
    in
    let filenames = 
        List.fold_left (fun l d ->
            let lnam = d.dir_long_name in 
            if (lnam <> "." && lnam <> "..") then
            begin
                if (d.dir_attr land attr_DIRECTORY <> attr_DIRECTORY) then
                    l@[lnam]
                else
                    l
            end
            else
                l
        ) [] dirl 
    in
    (Sort.list (<=) subdirs,Sort.list (<=) filenames)



(*
** Load a directory and build the subtree. Returns descriptor list
** for branches (sub directories). 
*)

let make_sub_dir dir tree f =
    let (dirs,files) = f dir in

    (*
    ** Determine the filesystem type and the selection lists.
    *)

    let fs_type = if (f == load_dos_dir) then
                    Dos
                  else
                    Unix 
    in
    let fl = match fs_type with
                        | Dos   -> sel_files_dos ;
                        | Unix  -> sel_files_unix;
    in
    let dl = match fs_type with
                        | Dos   -> sel_dirs_dos ;
                        | Unix  -> sel_dirs_unix;
    in
 
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
                                     ButtonPress, (fun _ -> select_dir dd dl);
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

    let fs_type = if (f == load_dos_dir) then
                    Dos
                  else
                    Unix 
    in
    let fl = match fs_type with
                        | Dos   -> sel_files_dos ;
                        | Unix  -> sel_files_unix;
    in
    let dl = match fs_type with
                        | Dos   -> sel_dirs_dos ;
                        | Unix  -> sel_dirs_unix;
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
                            Printf.printf "open %s" path;
                            print_newline ();
                            let dirmap = make_sub_dir 
                                                 path
                                                 d.dir_tree f in
                            d.dir_open <- true;
                            set_notify d.dir_tree dirmap f;                    
                        end
                        else
                        begin

                            Printf.printf "close %s" path;
                            print_newline ();

                            d.dir_tree#destroy_desc;   
                            d.dir_open <- false;

                            if (fs_type = Dos) then
                            begin
                                invalidate_dir dos_vol path;                            
                            end;

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
    tree#set_notify notify 

(*
** Copy selected files and directories to selected Unix directory.
*)

let copy_from_dos () =

    if (!sel_dirs_unix = [] || !sel_files_unix <> [] ||
        (List.length !sel_dirs_unix) <> 1 
       ) then
    begin
        print_string "One target directory must be selected!";
        print_newline ();
    end
    else if (!sel_dirs_dos = [] && !sel_files_dos = []) then
    begin
        print_string "No sources selected!";
        print_newline ();
    end
    else
    begin
        (*
        ** Copy files first.
        *)
        let t = List.nth !sel_dirs_unix 0 in
        List.iter ( fun f ->
                    let spath = if f.file_path = "/" 
                                then 
                                    "/"^f.file_name 
                                else
                                    f.file_path^"/"^f.file_name 
                    in
                    let tpath = if t.dir_path = "/" 
                                then 
                                    "/"^t.dir_name^f.file_name 
                                else
                                    t.dir_path^"/"^t.dir_name^"/"^f.file_name 
                    in

                    Printf.printf "Copying '%s' to '%s'..."
                                  spath tpath;
                    print_newline ();

                    copy_fromdos ~dosvol:dos_vol
                                 ~dosfile:spath
                                 ~homefile:tpath;
               ) !sel_files_dos;
        
        (*
        ** Now the DOS directories. Currently without
        ** subdirectories.
        *)
        
        List.iter ( fun d ->
            let dirname = if d.dir_path = "/" then
                            "/"^d.dir_name 
                          else
                            d.dir_path^"/"^d.dir_name
            in
            
            let subdirs,files = load_dos_dir dirname in

            Printf.printf "DOS directory: %s\n" dirname;
            List.iter ( fun f ->        
                    let spath = if dirname = "/" 
                                then 
                                    "/"^f 
                                else
                                    dirname^"/"^f 
                    in
                    let tpath = if t.dir_path = "/" 
                                then 
                                    "/"^t.dir_name^f 
                                else
                                    t.dir_path^"/"^t.dir_name^"/"^f 
                    in

                    Printf.printf "Copying '%s' to '%s'..."
                                  spath tpath;
                    print_newline ();

                    copy_fromdos ~dosvol:dos_vol
                                 ~dosfile:spath
                                 ~homefile:tpath;
            ) files;
        ) !sel_dirs_dos;
        
                        
    end
    

let _ =
  let closewindow () = 
        Display.closeDisplay (top#display);
  in

  (*
  ** Set the action handler for the buttons.
  *)

  exitbut#set_action ( fun () -> closewindow () );

  showselbut#set_action ( fun () ->
            Printf.printf "Unix dir selection:\n";
            List.iter ( fun d ->
                        let dpath = if d.dir_path = "/" then 
                                        "/"^d.dir_name 
                                    else
                                        d.dir_path^"/"^d.dir_name 
                        in
                        Printf.printf "%s\n" dpath; 
                    ) !sel_dirs_unix;
            Printf.printf "Unix file selection:\n";
            List.iter ( fun f ->
                        let dpath = if f.file_path = "/" then 
                                        "/"^f.file_name 
                                    else
                                        f.file_path^"/"^f.file_name 
                        in
                        Printf.printf "%s\n" dpath; 
                    ) !sel_files_unix;
            Printf.printf "Dos dir selection:\n";
            List.iter ( fun d ->
                        let dpath = if d.dir_path = "/" then 
                                        "/"^d.dir_name 
                                    else
                                        d.dir_path^"/"^d.dir_name 
                        in
                        Printf.printf "%s\n" dpath; 
                    ) !sel_dirs_dos;
            Printf.printf "Dos file selection:\n";
            List.iter ( fun f ->
                        let dpath = if f.file_path = "/" then 
                                        "/"^f.file_name 
                                    else
                                        f.file_path^"/"^f.file_name 
                        in
                        Printf.printf "%s\n" dpath; 
                    ) !sel_files_dos;
            print_newline ();
    );

  copyfromdosbut#set_action copy_from_dos;

  (*
  ** Load entry directory 
  *)

  let dirmap_left = make_sub_dir "/" tree_left load_dos_dir in
  set_notify tree_left dirmap_left load_dos_dir; 

  let dirmap_right = make_sub_dir home_path tree_right load_unix_dir in
  set_notify tree_right dirmap_right load_unix_dir; 

  vbar1#container_add_s  [
                            hbar1#contained;
                            hbar2#contained;
                         ];

  hbar1#container_add_s [
                            exitbut#contained;
                            showselbut#contained;
                            copyfromdosbut#contained
                        ];
  hbar2#container_add_s [
                            hbar_left#contained;
                            hbar_right#contained
                        ];
                                   
  hbar_left#container_add_s [
                            viewport_left#contained;
                            scrollbar_left#contained
                        ];
  hbar_right#container_add_s [
                            viewport_right#contained;
                            scrollbar_right#contained
                        ];

  (*
  ** Configure the scrollbar container. Scrolling with arrow keys
  ** and Page Up/Down keys.
  *)

  hbar_left#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady_left#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady_left#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady_left#down);
      Key(XK.xk_Up,0),(fun _ -> ady_left#up);
    ]];

  hbar_right#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady_right#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady_right#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady_right#down);
      Key(XK.xk_Up,0),(fun _ -> ady_right#up);
    ]];


  viewport_left#container_add tree_left#contained;
  viewport_right#container_add tree_right#contained;

  top#container_add vbar1#contained;
  top#show;

  try 
      loop ()
  with 
      Exit -> ()

