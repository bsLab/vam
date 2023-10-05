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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     27.11.2005
**    $VERSION:     1.03
**
**    $INFO:
**
**   Format of a symbol table(derived from a.out)
**
**    $ENDOFINFO
**
*)
open Machtype
open Bytebuf
open Buf
open Printf


let  n_UNDF  = word8 0x0             (* undefined *)
let  n_ABS   = word8 0x2             (* absolute *)
let  n_TEXT  = word8 0x4             (* text *)
let  n_DATA  = word8 0x6             (* data *)
let  n_BSS   = word8 0x8             (* bss *)
let  n_COMM  = word8 0x12            (* common (internal to ld) *)
let  n_FN    = word8 0x1e            (* file name symbol *)

let  n_EXT   = word8 1              (* external bit, or'ed in *)
let  n_TYPE  = word8 0x1e            (* mask for all the type bits *)

let n_STAB   = word8 0xe0


(*
** The following two values are Amoeba specific.
** They are only used in checking the byteorder independent
** internal symbol table format.
*)
let  n_AM_MAGIC      = word32s "0x89abcdef"     (* magic value *)

let  n_AM_VERSION    = word32 1     (* representation as of 5 feb 91 *)
let  n_AM_FOREIGN    = word32 0     (* non-standard symtab format *)



(*
** STAB debug symbols
*)
let  n_GSYM          = word8 0x20    (* global symbol *)
let  n_FNAME         = word8 0x22    (* F77 function name *)
let  n_FUN           = word8 0x24    (* procedure name *)
let  n_STSYM         = word8 0x26    (* data segment variable *)
let  n_LCSYM         = word8 0x28    (* bss segment variable *)
let  n_MAIN          = word8 0x2a    (* main function name *)
let  n_PC            = word8 0x30    (* global Pascal symbol *)
let  n_RSYM          = word8 0x40    (* register variable *)
let  n_SLINE         = word8 0x44    (* text segment line number *)
let  n_DSLINE        = word8 0x46    (* data segment line number *)
let  n_BSLINE        = word8 0x48    (* bss segment line number *)
let  n_SSYM          = word8 0x60    (* structure/union element *)
let  n_SO            = word8 0x64    (* main source file name *)
let  n_LSYM          = word8 0x80    (* stack variable *)
let  n_BINCL         = word8 0x82    (* include file beginning *)
let  n_SOL           = word8 0x84    (* included source file name *)
let  n_PSYM          = word8 0xa0    (* parameter variable *)
let  n_EINCL         = word8 0xa2    (* include file end *)
let  n_ENTRY         = word8 0xa4    (* alternate entry point *)
let  n_LBRAC         = word8 0xc0    (* left bracket *)
let  n_EXCL          = word8 0xc2    (* deleted include file *)
let  n_RBRAC         = word8 0xe0    (* right bracket *)
let  n_BCOMM         = word8 0xe2    (* begin common *)
let  n_ECOMM         = word8 0xe4    (* end common *)
let  n_ECOML         = word8 0xe8    (* end common (local name) *)
let  n_LENG          = word8 0xfe    (* length of preceding entry *)


let w8_0 = word8 0
let w16_0 = word16 0
let w32_0 = word32 0

let is_SLINE t = (t land n_SLINE) = n_SLINE
let is_TEXT t  = (t land n_STAB) = w8_0 &&
                 (t land n_TEXT)  = n_TEXT

(*
** One symbol table entry
*)

type nlist = {
    mutable n_name  : string;   (* for use when in-core *)
    mutable n_strx  : word32;   (* index into file string table *)
    mutable n_type  : word8;    (* type flag (N_TEXT,..) *)
    mutable n_other : word8;    
    mutable n_desc  : word16;
    mutable n_value : word32;   (* value of symbol (or sdb offset) *)
}

let nilnlist = {
    n_name = ""; n_strx = w32_0; n_type = w8_0; n_other = w8_0;
    n_desc = w16_0; n_value = w32_0;
}

(*
** buf_get_symtab(): get a symbol table and a string table from a buffer
** containing an Amoeba symbol table, which has the following format:
**
** long                 : magic value, for checking purposes
** long                 : version, ditto
** long                 : size of symbol table following it
** struct nlist []      : symbol table
** long                 : size of string table following it
** char []              : string table
**
** The buffers for the symbol and string table are allocated by this
** routine, as the caller doesn't know how big they should be.
**
** The differences between a Unix symbol table and a symbol table
** present in Amoeba binaries are the following:
** 
** - a long magic & version prefix
** - the Amoeba symbol table has an integer prefix.
**   This is needed because there is no ``a_syms'' field in an Amoeba
**   process descriptor.
** - all integers are in network byte order.
**   This has the advantage that programs like `nm' can easily work on
**   binaries of different endianness.
** - The integer telling how big the string table is does not include the
**   4 bytes of itself.
**   This was only done to make things more consistent.
*)

let buf_get_symtab buf pos = 
    (*
    ** magic & version field for consistency checks
    *)
    let pos,magic = buf_get_mach buf pos Word32 in  
    let pos,ver = buf_get_mach buf pos Word32 in
    if magic <> n_AM_MAGIC || ver <> n_AM_VERSION then
        raise Exit;

    let s8 = 1 in let s16 = 2 in let s32 = 4 in
    let symsize = s32 + s8 + s8 + s16 + s32 in

    let pos,nsymsize = buf_get_int32 buf pos in
    let nsymtab = nsymsize / symsize in
    let symtab = if nsymtab = 0 then [||] else Array.create nsymtab nilnlist in
    let pos = ref pos in

 
    if nsymtab > 0 then
        for i = 0 to nsymtab-1
        do
            let pos' = !pos in
            let pos',(n_strx:word32)    = buf_get_mach buf pos' Word32 in
            let pos',(n_type:word8)     = buf_get_mach buf pos' Word8 in
            let pos',(n_other:word8)    = buf_get_mach buf pos' Word8 in
            let pos',(n_desc:word16)    = buf_get_mach buf pos' Word16 in
            let pos',(n_value:word32)    = buf_get_mach buf pos' Word32 in
            symtab.(i) <- {
                n_name = "";
                n_strx = n_strx;
                n_type = n_type;
                n_other = n_other;
                n_desc = n_desc;
                n_value = n_value;
                };
            pos := pos';
        done;
    (*
    ** get integer telling how big the string table is
    *)
    let pos',nstrtab = buf_get_int32 buf !pos in


    pos := pos';
    let strtab = buf_create nstrtab in
    if nstrtab > 0 then
        blit_bb ~src:buf ~src_pos: !pos    
                ~dst:strtab ~dst_pos:0
                ~len:nstrtab;
    pos := !pos + nstrtab;
    !pos,symtab,strtab


(*
** buf_put_symtab() puts a symbol table + string table in an
** buffer to form an Amoeba symbol table.
** See buf_get_symtab() for a description of its structure.
*)

let buf_put_symtab ~buf ~pos ~symtab ~strtab = 
    (*
    ** magic & version field for consistency checks
    *)
    let pos = buf_put_mach buf pos n_AM_MAGIC in
    let pos = buf_put_mach buf pos n_AM_VERSION in
    (*
    ** symbol table length prefix
    *)
    let nsymtab = Array.length symtab in
    let s8 = 1 in let s16 = 2 in let s32 = 4 in
    let symsize = s32 + s8 + s8 + s16 + s32 in
    let pos = buf_put_int32 buf pos (nsymtab * symsize) in
    let pos = ref pos in
    if nsymtab > 0 then
    begin
        (*
        ** copy symbol table in buffer in network byte order
        *)
        Array.iter (fun sym ->
                let pos' = !pos in
                let pos' = buf_put_mach buf pos' sym.n_strx in
                let pos' = buf_put_mach buf pos' sym.n_type in
                let pos' = buf_put_mach buf pos' sym.n_other in
                let pos' = buf_put_mach buf pos' sym.n_desc in
                let pos' = buf_put_mach buf pos' sym.n_value in
                pos := pos';
            ) symtab;    
    end;
    (*
    ** put string table in buffer: a long + the string table itself
    *)
    let nstrtab = buf_len strtab in
    let pos' = buf_put_int32 buf !pos nstrtab in
    pos := pos';
    if nstrtab > 0 then
        blit_bb ~src:strtab ~src_pos:0
                ~dst:buf ~dst_pos: !pos
                ~len:nstrtab;
    !pos + nstrtab 

(*
** Print a symbol table
*)

let print_symtab symtab =
    let out = print_string in
    let nl = print_newline in

    let symtype t =
        let t' = t land n_TYPE in

        if (t land n_STAB) = w8_0 then
        (match t' with
         | t when (t = n_UNDF) -> "UNDEF";
         | t when (t = n_ABS)  -> "ABS  ";
         | t when (t = n_TEXT) -> "TEXT ";
         | t when (t = n_DATA) -> "DATA ";
         | t when (t = n_BSS)  -> "BSS  ";
         | t when (t = n_COMM) -> "COMM ";
         | t when (t = n_FN)   -> "FILE ";
         | _ -> sprintf "%5x" (to_int t'); 
        ) ^ (if (t land n_EXT) = n_EXT then ":EXT" else "    ")
        else 
        (match t with
        | t when (t = n_GSYM) -> "GSYM";
        | t when (t = n_FNAME) -> "FNAME";
        | t when (t = n_FUN) -> "FUN";
        | t when (t = n_STSYM) -> "STSYM";
        | t when (t = n_LCSYM) -> "LCSYM";
        | t when ( t = n_MAIN) -> "MAIN";
        | t when ( t = n_RSYM) -> "RSYM";
        | t when ( t = n_SLINE) -> "SLINE";
        | t when ( t = n_DSLINE) -> "DSLIN";
        | t when ( t = n_BSLINE) -> "BSLIN";
        | t when ( t = n_SSYM) -> "SSYM";
        | t when ( t = n_SO) -> "SO";
        | t when ( t = n_LSYM) -> "LSYM";
        | t when ( t = n_BINCL) -> "BINCL";
        | t when ( t = n_SOL) -> "SOL";
        | t when ( t = n_PSYM) -> "PSYM";
        | t when ( t = n_EINCL) -> "EINCL";
        | t when ( t = n_ENTRY) -> "ENTRY";
        | t when ( t = n_LBRAC) -> "LBRAC";
        | t when ( t = n_EXCL) -> "EXCL";
        | t when ( t = n_RBRAC) -> "RBRAC";
        | t when ( t = n_BCOMM) -> "BCOMM";
        | t when ( t = n_ECOMM) -> "ECOMM";
        | t when ( t = n_ECOML) -> "ECOML";
        | t when ( t = n_LENG) -> "LENG";
        | _ -> sprintf "U%x" (to_int t);
        )
        in

    Array.iter (fun sym ->
            out (sprintf "%40s [%s] -> %s"
                         sym.n_name
                         (symtype sym.n_type)
                         (to_str sym.n_value));
            nl ();            
        ) symtab


(*
** Iterate the symtab list and extract symbol names from strtab.
** Sort symtab array by increasing address.
*)
let resolve_symtab symtab strtab =
    let strlen = buf_len strtab in

    let symname off symtype desc =
        let off = (to_int off) - 4  in
        if off >= 0 && off < strlen && 
           (((symtype land n_TYPE) <> n_UNDF) ||
            ((symtype land n_STAB) <> w8_0)) then
        begin
            (*
            ** extract length
            *)
            let start = off in
            let stop = ref off in

            protect (
                while !stop < strlen 
                do
                    if (buf_getc strtab !stop) = '\000' then
                        raise Exit;            
                    incr stop;
                done
            );
            if !stop = strlen then 
                "???"
            else            
                buf_tostring strtab start (!stop-start)
        end
        else if (is_SLINE symtype)  then
                        sprintf "L%d" (to_int desc)
        else
            ""
        in

    Array.iter (fun sym ->
            let name = symname sym.n_strx sym.n_type sym.n_desc in
            sym.n_name <- name;
        ) symtab;
            
    Array.sort (fun s1 s2 -> 
            if s1.n_value > s2.n_value then 1 
            else if s1.n_value < s2.n_value then (-1)
            else 0 ) symtab

(*
** Find a symbol name around address 'addr'
*)
let find_sym_name symtab addr =
    let symlen = Array.length symtab in
    let is_cobj name =
        let len = String.length name in
        (name.[len-1] = 'o' || name.[len-1] = 'c') && name.[len-2] = '.'
        in

    if symlen > 0 then
    begin
        let lastn = symlen - 1 in
        let lastaddr = ref symtab.(lastn).n_value in
        let lastname = ref "???" in
        protect (Array.iter (fun sym ->
            if addr < sym.n_value && addr >= !lastaddr  &&
               (is_TEXT sym.n_type) && not (is_cobj sym.n_name) then
            begin
                (*
                ** Found...
                *)
                raise Exit;
            end;
            if (is_TEXT sym.n_type) && not (is_cobj sym.n_name) then
            begin
                lastname := sym.n_name;
                lastaddr := sym.n_value;
            end;
        ) symtab);
        !lastname
    end
    else
        "Empty Symboltable"

(*
** Find source line around address 'addr'
*)
let find_sym_line symtab addr =
    let symlen = Array.length symtab in

    if symlen > 0 then
    begin
        let lastn = symlen - 1 in
        let lastname = ref "" in
        protect (Array.iter (fun sym ->
            if (is_SLINE sym.n_type) && addr = sym.n_value then raise Exit;
            if (is_SLINE sym.n_type) then
            begin
                lastname := sym.n_name;
            end;
        ) symtab; lastname := "");
        !lastname
    end
    else
        ""


(*
** Find a filename (object) for a symbol around address 'addr'
*)
let find_sym_file symtab addr =
    let symlen = Array.length symtab in
    let is_cobj name =
        let len = String.length name in
        (name.[len-1] = 'o' || name.[len-1] = 'c') && name.[len-2] = '.'
        in

    if symlen > 0 then
    begin
        let lastn = symlen - 1 in
        let lastaddr = ref symtab.(lastn).n_value in
        let lastname = ref "???" in
        protect (Array.iter (fun sym ->
            if addr < sym.n_value && addr >= !lastaddr  &&
               (is_TEXT sym.n_type) && (is_cobj sym.n_name) then
            begin
                (*
                ** Found...
                *)
                raise Exit;
            end;
            if (is_TEXT sym.n_type) && (is_cobj sym.n_name) then
            begin
                lastname := sym.n_name;
                lastaddr := sym.n_value;
            end;
        ) symtab);
        !lastname
    end
    else
        "Empty Symboltable"

