open Bytebuf


type dns_dir_entry = {
  mutable d_name : string;
  mutable d_columns : Amoeba.rights_bits array;
} 
val nil_dns_dir_entry : dns_dir_entry
type dns_dir = {
  mutable dd_capset : Capset.capset;
  mutable dd_ncols : int;
  mutable dd_nrows : int;
  mutable dd_colnames : string array;
  mutable dd_rows : dns_dir_entry list;
  mutable dd_curpos : int;
} 
val default_colmask : unit -> int * int array
val _dns_rootdir : Capset.capset ref
val _dns_workdir : Capset.capset ref
val get_workdir : unit -> Amoeba.status * Capset.capset
val get_rootdir : unit -> Amoeba.status * Capset.capset
val set_workdir : Amoeba.capability -> unit
val set_rootdir : Amoeba.capability -> unit

(*
**
*)
val dns_lookup :
  root:Capset.capset -> path:string -> Amoeba.status * Capset.capset
val dns_setlookup :
  server:Capset.capset -> dirs:(Capset.capset*string) list ->
  Amoeba.status * (Amoeba.status*int*Capset.capset) list
val dns_rename:
  dir:Capset.capset -> oldname:string -> newname:string ->
  Amoeba.status
val dns_append: dir:Capset.capset ->
    name:string -> obj:Capset.capset -> cols:int array -> Amoeba.status
val dns_delete: dir:Capset.capset ->
    name:string -> Amoeba.status
val dns_create: dir:Capset.capset -> cols:string array ->
        Amoeba.status * Capset.capset
(*
** Try to get default file server cap from directory server.
*)
  
val dns_getdefafs: dir:Capset.capset -> Amoeba.status * Amoeba.capability

