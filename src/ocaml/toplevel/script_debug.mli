

(*
** Where the debug_file function stores the debug perpared source
** file we must use instead the original one.
*)

val debug_trace_path: string ref

(*
** from the byterun machine in backtrace.c:
*)

external ___pc___: int -> unit
    ="___pc___"

(*
** get the current pc index value
*)

external debug_pc_value: unit -> int
    ="debug_pc_value"


(*
** The debug trace function called on each debug point with
** an increasing and unique index number:
*)


val debug_trace_func: string ref



type source_loc =
{
    pc_index: int;         
    pc_source_start :int;   (* absolut char position in source file 
                            ** for a new debug trace point.
                            *)
    pc_source_end:int;
    pc_source_line:int;     (* line number in source file *)
    pc_source_pos:int       (* character position in this line *) 
}

val debug_trace_pc_list: source_loc list ref

type pc_module_entry =
{
    pc_module_name:string;
    pc_first_index:int;
    pc_last_index:int;
}

val debug_trace_module_list: pc_module_entry list ref


(*
** Create a new file from filename with additional debug trace
** points inserted.
**
** It's stored in the debug_trace_path  directory with the name extension
** ".debug". 
*)

val debug_file : string -> unit

(*
** Resolve a pc index. Returns the module name, the line and position
** in this line of the source file. 
*)

val debug_trace_index : int -> string * int * int


