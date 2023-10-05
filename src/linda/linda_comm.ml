(*
** Tuple argumenr types
*)
type c_linda_data_type =
    | CHAR_TYPE         (* signed char                          *)
    | CHARP_TYPE        (* signed char array/string (pointer)   *) 
    | STRING_TYPE       (* a string, null terminated            *)
    | INT_TYPE          (* signed int                           *)
    | INTP_TYPE         (* signed int array (pointer)           *)
    | FLOAT_TYPE        (* float (single prec.)                 *)
    | FLOATP_TYPE       (* float array (pointer)                *)
    | DOUBLE_TYPE       (* double (double prec.)                *)
    | DOUBLEP_TYPE      (* double array                         *)
    | SHORT_TYPE        (* signed short (int16)                 *)
    | SHORTP_TYPE       (* signed short array (pointer)         *)


let c_num_of_type t =
    match t with
    | CHAR_TYPE   ->    1       
    | CHARP_TYPE  ->    2        
    | STRING_TYPE ->    3       
    | INT_TYPE    ->    4       
    | INTP_TYPE   ->    5       
    | FLOAT_TYPE  ->    6       
    | FLOATP_TYPE ->    7       
    | DOUBLE_TYPE ->    8       
    | DOUBLEP_TYPE ->   9       
    | SHORT_TYPE   ->   10      
    | SHORTP_TYPE  ->   11      

(*
** Data sizes of different tuple data types
*) 

let size_char = 1
let size_int16 = int16_SIZE
let size_int32 = int32_SIZE
let size_float = 8

let c_size_of_type t =
    match t with
    | CHAR_TYPE   ->    size_char       
    | CHARP_TYPE  ->    size_char
    | STRING_TYPE ->    size_char
    | INT_TYPE    ->    size_int32
    | INTP_TYPE   ->    size_int32
    | FLOAT_TYPE  ->    size_float
    | FLOATP_TYPE ->    size_float
    | DOUBLE_TYPE ->    size_float
    | DOUBLEP_TYPE ->   size_float
    | SHORT_TYPE   ->   size_int16
    | SHORTP_TYPE  ->   size_int16



(*
** Tuple inode field tag
*)
let d_FIELD = 1                 (* it's an actual inode                 *)
let q_FIELD = 2                 (* it's a formal inode (question)       *)


(*
** signal that our server cap is packed in
** the rpc data buf instead of an array
*)
let svr_CAP_TYPE = 0x80         
let index_TYPE = 0x40           (* It's an index number                 *)
let scalar_type = 0x10          (* it's a scalar type, incl. strings    *)
let aggregate_TYPE = 0x20       (* It's an array                        *)
let linda_MAGIC = 0x16EA        (* what else ?                          *)

(*
**
** The client stub interface; hidden. 
** Generic format:
**
** Out/In reqbuf:
** 
** Tuple inode format in data rpc buffer for scalar types and strings:
**
** first the tuple mask string:
**
** buf_put_int32(LINDA_MAGIC)
** buf_put_int32(tuple mask length (incl. \0) 
** <tuple_mask>
**
** buf_put_int32(LINDA_MAGIC)
** buf_put_int32(type)
** buf_put_int32(field_tag)
**              field_tag:      (D_FIELD or Q_FIELD) & SCALAR_TYPE
** but_put_int32(inode number)
** buf_put_int32(tuple inode data si
** <data> if D_FIELD, else <none>
**
** ...next tuple inode ...
**
** 
** h_offset=total size of data buffer (with headers) in bytes 
** h_extra=total number of tuple members in data buffer
**
** Special case: Aggregates(Arrays) :
**
** buf_put_int32(LINDA_MAGIC)
** buf_put_int32(type)
** bif_put_int32(field_tag)
**              field_tag:      (D_FIELD or Q_FIELD) & 
**                               AGGREGATE_TYPE &
**                               SVR_CAP_TYPE   if D_FIELD
** but_put_int32(inode number)
** buf_put_int32(tuple inode data size)  in type units!!!!
** if D_FIELD:
** buf_put_cap(our array server cap)
** buf_put_int32(table index)
**
** ...next tuple inode ...
**
** The tuple server will do a transaction to our array server. The
** array server will get the pointer to the desired array (choosen
** by the inode number) by the global linda_array_tbl[] pointer
** table. To protect mixing of different out/in reuqests, each
** linda command is protected by a mutex linda_array_lock.
** Only one thread at each time can handle a linda array request.
**
** Out: repbuf=NILBUF
** In: repbuf:
**
** Only formals are packed:
**
** buf_get_int32(LINDA_MAGIC)
** buf_get_int32(type)
** buf_get_int32(field_tag)
**              field_tag:      (Q_FIELD) & (SCALAR_TYPE|AGG_TYPE) 
** but_get_int32(inode number)
** buf_get_int32(tuple inode data size)         in type units !!!!
**
** <data> if SCALAR_YTPE, else <none>
**
** buf_get_int32(internal server inode number)
**
*)

let  linda_OUT       = Command (linda_FIRST_COM+1)
let  linda_IN        = Command (linda_FIRST_COM+2)
let  linda_INP       = Command (linda_FIRST_COM+3)
let  linda_RD        = Command (linda_FIRST_COM+4)
let  linda_RDP       = Command (linda_FIRST_COM+5)
let  linda_EVAL      = Command (linda_FIRST_COM+6)

(*
** OUT:
** The tuple server does a transaction to us to get arrays:
**
** h_extra =  type
** h_size  =  inode number 
** h_offset  =  size in type units
**
** repbuf  =  pointer to data array
** 
** IN:
** If the client must do a trans to tuple server to get an array:
**
** h_extra  = type
** h_size = tuple argument id number from server
** h_offset   = size in type units
**
** trans repbuf = pointer to data array
**
*)

let  linda_GET_ARRY      = Command (linda_FIRST_COM+7)

type linda_Array_Tbl = {
    latbl_type: linda_data_type;        (* array base type      *)
    latbl_size: int;                    (* size in type units   *)
    latbl_buf: buffer;
}

(*
** the size of the linda request rpc buffer
*)
let linda_RPC_BUFSIZE = 30000

(*
** the client user public put capability
*)
let _linda_tuple_server_putcap = ref nilcap

(*
** our local array server put(public) port
*)
let _linda_array_putcap = ref nilcap

(*
** the linda rpc requests share global data; protected with a mutex
*)

let _linda_rpc_lock = mu_create ()
let _linda_lock = mu_create ()
let _linda_array_tbl_lock = mu_create ()



(*
** maximal number of hosts managed by linda
*)
let ts_MAX_HOSTS   = 100

(*
** maximal number of child processes
*)
let ts_MAX_PROCESS = 100

