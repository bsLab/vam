(*
** This files helps to extend the debug information 
** "event at char position xyz" to a more human friendly form
** "event at line , pos (in line) xyz".
**
** To keep the old debug event structure, we abuse the ev_cahr field
** in the following way (done in bytegen.ml) :
**
** ev_char = [(line in source file)*1000 + (char pos in source file)]
**
** init_line_of_pos scanns the source file and records all line start
** and end positions in the init_line_of_pos_table.
**
** From bytegen.ml, comp_expr, 
**      line_of_pos lev_loc 
** is called and returns the true position in the file 
**      (line,pos_in_line)
**
** When a preprocessor is used, line derictives from the preprocessor
** must be recognized and resolved.
**
** Written by
**  Stefan Bosse
**  sbosse@physik.uni-bremen.de
**
*) 

let mod_line_of_pos_debug_ver = 1.10

let init_line_of_pos_done   = ref false 
let current_table_index     = ref 1 
let current_filepos         = ref 1
let table_length            = ref 1

type line_of_pos_t =
    {
      linenum: int;
      startpos: int;
      endpos: int;
    }
      
let line_of_pos_table = ref [| {linenum=0;startpos=0;endpos=0} |] 

let init_line_of_pos sourcefile preprocess =
    let ic          = open_in sourcefile in
    let filepos     = ref 1 in
    let eof         = ref false in
    let linecount   = ref 0 in      (* physical line number *)
    let linenum     = ref 0 in      (* logical line number  *)

    (*
    ** The preprocessors adds one additional line at the beginning
    ** of the file.
    *)

    (* always reset the above list first *)
    line_of_pos_table := [| {linenum=0;startpos=0;endpos=0} |];        

    while (!eof=false)
    do
        try 
        (
            let oneline = input_line ic in
            let slen = String.length oneline in

            (*
            ** Ceck for preprocessor line derictives like
            **
            **      '# 1 "myfile.ml"'
            **
            ** and correct the line number.
            *)
            if (preprocess = true) then
            begin
                if (slen > 0 && 
                    oneline.[0]='#' &&
                    slen > 2 &&
                    oneline.[1]=' ') then
                begin
                try
                  begin
                    (*
                    ** This must be a line number from the preprocessor!
                    *)
                    let p1 = String.index_from oneline 2 ' ' in
                    
                    let newlinenum = String.sub oneline 2
                                                        (p1-2) in
                    linenum := (int_of_string newlinenum) - 1;
                  end
                with
                    | _ -> failwith 
                            ("bytecomp: invalid preprocessor line: '"^
                             oneline^"'");
                end
                else
                    incr linenum;
            end
            else
                incr linenum;

            incr linecount;

            line_of_pos_table := Array.append 
                                    (!line_of_pos_table)

                                    [| {
                                      linenum    = !linenum;
                                      startpos   = !filepos;
                                      endpos     = (!filepos + slen + 1)
                                    } |];
            filepos := !filepos + slen + 1 ;
        ) 
        with End_of_file -> eof:=true;
        
    done;
    close_in ic; 
    current_table_index := 1;
    current_filepos     := 1;
    table_length        := !linecount;
    init_line_of_pos_done := true

(*
** Return the (line*1000 + pos) value of the current file position
** used instead for the ev_char position in the debug entry of
** a bytecode file.
*)

let line_of_pos filepos =
    let found = ref false in
    let line  = ref 0 in
    let cpos  = ref 0 in    
    let tind = ref (!current_table_index) in

    if(!init_line_of_pos_done = false) then
        failwith "line_of_pos: programming error: not initialized!";

    if (filepos >= !current_filepos) then
    begin
        (* go forward through the pos table *)

        while(!found = false)
        do
            let tabentry = (!line_of_pos_table).(!tind) in
            let pstart   = tabentry.startpos in
            let pend     = tabentry.endpos in

            if (filepos >= pstart)&& (filepos < pend) then
            begin
                (* found, tough *)
                line := tabentry.linenum;
                cpos := (filepos - pstart + 1);
                found := true;
            end 
            else
            begin
                incr tind;
                if (!tind > !table_length) then
                    failwith ("line_of_pos: file position not found : "
                              ^(string_of_int filepos));
            end;
        done;        
    end
    else
    begin
        (* go backward through the pos table *)
        while(!found = false)
        do
            let tabentry = (!line_of_pos_table).(!tind) in
            let pstart   = tabentry.startpos in
            let pend     = tabentry.endpos in

            if (filepos >= pstart) && (filepos < pend) then
            begin
                (* found, tough *)
                line := tabentry.linenum;
                cpos := (filepos - pstart + 1);
                found := true;
            end 
            else
            begin
                decr tind;
                if (!tind < 1) then
                    failwith ("line_of_pos: file position not found :"
                              ^(string_of_int filepos));
            end;
        done;        
    end;
    current_table_index := !tind;
    current_filepos     := filepos;

    (!line * 1000 + !cpos)

