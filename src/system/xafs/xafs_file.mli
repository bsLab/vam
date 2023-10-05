exception AmError of Amoeba.status
type dir_desc = {
  dir_name : string;
  dir_path : string;
  dir_tree : WX_tree.t;
  dir_label : WX_label.t;
  dir_branch : WX_tree.node;
  mutable dir_selected : bool;
  mutable dir_open : bool;
} 
and file_desc = {
  file_name : string;
  file_path : string;
  file_label : WX_label.t;
  file_leaf : WX_tree.node;
  mutable file_selected : bool;
} 
val sel_objs_afs : file_desc list ref
val sel_dirs_afs : dir_desc list ref
val sel_files_unix : file_desc list ref
val sel_dirs_unix : dir_desc list ref
val verbose : bool ref
val copy_buf : (Amoeba.capability * string) ref
type copy_flags = {
  mutable copy_overwrite : bool;
  mutable copy_destroy : bool;
} 
val copy_flags : copy_flags
type del_flags = { mutable del_destroy : bool; } 
val del_flags : del_flags
val dying : bool ref
val service_sema : Sema.semaphore
val service_fun : (unit -> unit) ref
val service_thr : unit -> unit
val unsel_all : unit -> unit
val file_cap : Amoeba.capability ref
val init_file_cap : string -> unit
val load_unix_dir :
  string -> (string * Amoeba.status) list * (string * Amoeba.status) list
val load_afs_dir :
  string -> (string * Amoeba.status) list * (string * Amoeba.status) list
val copy_fromafs : src:string -> dst:string -> Amoeba.status
val copy_fromunix : src:string -> dst:string -> Amoeba.status
val copy_from_afs : unit -> unit
val copy_from_unix : unit -> unit
val show_info : unit -> unit
val show_status : unit -> unit
val mkdir : unit -> unit
val rename : unit -> unit
val dodelfile : string -> unit
val dodeldir : string -> unit
val delfile : unit -> unit
val deldir : unit -> unit
val delobj : unit -> unit
val copy : unit -> unit
val paste : unit -> unit
