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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
** High level directory service stubs
**
**
**    $ENDOFINFO
**
*)



open Amoeba
open Cmdreg
open Stderr
open Stdcom
open Capset
open Soap       (* obsolete *)


let _ds_ncolumns    = ref 0 
let _ds_colnames    = ref [| "owner";"group";"other" |] 
let _ds_NCOLUMNS    = 3
let _ds_colmasks    = ref [| 0 ; 0 ; 0 |] 

(*
** Returns the (status,capability) tuple for the directory lookup
** of name. The server capability is an optional argument. 
*)

let dir_lookup ~root ~name:name  =

    let (err_stat,object_cs) =
        if(root <> nilcap) then
        begin
            let root_cs = cs_singleton root in
            sp_lookup root_cs name 
        end
        else
            sp_lookup sp_DEFAULT name 
    in

    if (err_stat = std_OK) then
    begin
        let (errignore,obj_cap) = cs_to_cap object_cs in
        (err_stat,obj_cap) 
    end
    else
        (err_stat,nilcap)


    
(*
** Append a new object capability under with the given name
** to the directory tree.
*)

let dir_append ~root ~name:name ~obj:objcap =

    let object_cs = cs_singleton objcap in

    let err_stat =
        if(root <> nilcap) then
        begin
            let root_cs = cs_singleton root in
            sp_append ~dir:root_cs ~name:name 
                      ~obj:object_cs ~cols:(!_ds_colmasks);
        end
        else
            sp_append ~dir:sp_DEFAULT ~name:name 
                      ~obj:object_cs ~cols:(!_ds_colmasks);
    in

    (* return result *)
    err_stat    

(*
** Rename a directory entry.
*)

let dir_rename ~dir ~oldname ~newname =

    let object_cs = cs_singleton dir in

    let err_stat =
        if(dir <> nilcap) then
        begin
            let root_cs = cs_singleton dir in
            Dns_client.dns_rename ~dir:root_cs ~oldname:oldname 
                                  ~newname:newname
        end
        else
            std_CAPBAD
    in

    (* return result *)
    err_stat    


(*
** This function sets the default column masks used when appending names
** with the dir/name interface.  It should be called when the masks as
** specified by the environment variable SPMASK are not what we need.
**
*)

let dir_set_colmasks columns ncols =
    if (ncols > _ds_NCOLUMNS) then
        failwith "dir_set_colmasks: invalid ncols argument";
    for i = 0 to (ncols-1)
    do
        (!_ds_colmasks).(i) <- columns.(i);
    done;
    _ds_ncolumns := ncols


let dir_delete ~root ~name:name =
    let err_stat =
        if(root <> nilcap) then
        begin
            let root_cs = cs_singleton root in
            sp_delete ~dir:root_cs ~name:name; 
        end
        else
            sp_delete ~dir:sp_DEFAULT ~name:name; 
    in
    err_stat

(*
** Create a new directory for server 'server'. Returns the new 
** directory capability set. This capset can then appended somewhere
** in the directory tree. 
*)

let dir_create ~server =

    let err_stat,cs =
        if(server <> nilcap) then
        begin
            let srv_cs = cs_singleton server in
            sp_create ~server:srv_cs  ~cols:(!_ds_colnames);
        end
        else
            sp_create ~server:sp_DEFAULT ~cols:(!_ds_colnames);
    in

    (* return result *)
    (cs_to_cap cs)
    

(*
** UNIX like directory interface: open - readnext -close. The directory
** capability can be resolved with 'dir_lookup'.
**
** 
*)
type dir_row = 
{
    mutable dr_name: string;
    mutable dr_time: int;
    mutable dr_cols: int array;
}

type dir_desc = 
{
    mutable dir_rows    : dir_row array;
    mutable dir_curpos  : int;
    mutable dir_ncols   : int;
    mutable dir_nrows   : int;
    mutable dir_colnames: string array;
}

let nil_dir_desc = {dir_rows=[||];dir_curpos=0;dir_ncols=0;dir_nrows=0;
                    dir_colnames=[||]}

(*
** Return dirdesc structure
*)

let dir_open ~dir = 
    try
    begin
        let dir_cs = cs_singleton dir in
        let stat,dd = sp_list dir_cs in

        if (stat <> std_OK) then
            raise (Error stat);

        let nrows  = dd.dd_nrows in
        let ncols  = dd.dd_ncols in
        let ddrows = Array.of_list dd.dd_rows in
        let ndlist = ref [] in
        let didrows = Array.map 
                        (fun de -> 
                            let cols = Array.map 
                                         (fun c ->
                                            let Rights_bits a = c in a
                                         ) de.d_columns
                            in 
                            ndlist := !ndlist @ [dir_cs,de.d_name];
                            {dr_name = de.d_name;
                             dr_cols = cols;
                             dr_time = 0}
                        ) ddrows
        in
        (*
        ** Get the row times
        *)
        let stat,stc = Dns_client.dns_setlookup 
                            ~server:dir_cs
                            ~dirs:!ndlist
            in
        if (stat <> std_OK) then
            raise (Error stat);
    
        let timel = Array.of_list stc in
        for i = 0 to (Array.length didrows)-1
        do
            let _,time,_ = timel.(i) in
            didrows.(i).dr_time <- time;
        done;
 
        std_OK,{
                dir_rows = didrows;
                dir_curpos = 0;
                dir_ncols = dd.dd_ncols;
                dir_nrows = dd.dd_nrows;
                dir_colnames = dd.dd_colnames;
             }
    end  
    with
        | Error err -> err,nil_dir_desc

(*
** Get the next row (name,colmasks) entry.
*)

exception Dir_empty

let dir_next ~dirdesc =
    let cur = dirdesc.dir_curpos in
    if (cur >= dirdesc.dir_nrows) then
        raise Dir_empty;

    dirdesc.dir_curpos <- cur + 1;
    dirdesc.dir_rows.(cur)

    
let dir_close ~dirdesc =
    ()  (* ? *)
            
   



(*
** Init this module
**  - get the soap mask parameters
*)

let _ =
    let (cols,col_arr) = sp_mask () in
    _ds_ncolumns := cols;
    _ds_colmasks := col_arr

