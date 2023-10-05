
(*
**  This file contains the definitions of the symbols used by servers
**  in their stdinfo string to identify the object.
**  Note:  only objects are identified this way.  Servers for the object
**  describe themselves with a longer string at present, although they
**  could be of the object type server and return S followed by the symbol
**  of the type of object.
*)

let objsym_TTY      = '+'           (* TTY                       *)
let objsym_BULLET   = '-'           (* Bullet File               *)
let objsym_AFS      = objsym_BULLET (* AFS File                  *)
let objsym_FILE     = objsym_BULLET (* AFS File                  *)
let objsym_DIRECT   = '/'           (* Directory                 *)
let objsym_DIR      = '/'           (* Directory                 *)
let objsym_DNS      = objsym_DIRECT (* Directory                 *)
let objsym_KERNEL   = '%'           (* Kernel Directory          *)
let objsym_DISK     = '@'           (* Disk, Virtual or Physical *)
let objsym_PROCESS  = '!'           (* Process, Running or not   *)
let objsym_PIPE     = '|'           (* Pipe ?                    *)
let objsym_RANDOM   = '?'           (* Random Number Generator   *)

