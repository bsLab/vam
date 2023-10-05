
open Os

let init () =
    match vm_os with 
    | OS_Unix   -> Xos_unix.init ();
    | OS_Amoeba -> Xos_amoeba.init()

