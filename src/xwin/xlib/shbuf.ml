
  (************************************************************
  
  The compatibility with threads is expensive. A special version may
  be useful when there is no threads.
  
  *)
open Xbuffer

let max_len = 250
let buffer = String.create (max_len*4)
let cur_len = ref 0
  
let newBuffer len = 
  cur_len := len;
  if len > max_len then String.create (len*4) else buffer
let bufSize _ = !cur_len
  
let read_buffer = String.create (max_len*4)
let readBuffer _ = read_buffer
let addBuffer buf len =
  if len + 8 > max_len then 
    let b2 = newString (len+8) in
    string_blit read_buffer 0 b2 0 32;
    b2
  else read_buffer