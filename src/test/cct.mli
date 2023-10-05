

open Amoeba
open Bytebuf
open Rpc

external tt: (header * buffer * int * header * buffer * int ) 
                -> status*int
    = "tt_c"

external hdr_new: unit -> header
    = "hdr_new"
