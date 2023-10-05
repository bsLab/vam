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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**  Print level Debug support.
**
**    $ENDOFINFO
**
*)



let dblevel = ref 1
 
module Pr =
struct
let s l s =
    if (l <= !dblevel) then
    begin
        print_string s;
	print_newline ();
    end

let ss l s s2 =
    if (l <= !dblevel) then
    begin
        print_string (s^": "^s2);
	print_newline ();
    end

let sss l s s2 s3 =
    if (l <= !dblevel) then
    begin
        print_string (s^": ["^s2^","^s3^"]");
	print_newline ();
    end

let sd l s d =
    if (l <= !dblevel) then
    begin
        print_string (s^": "^(string_of_int d));
	print_newline ();
    end

let sdd l s d1 d2  =
    if (l <= !dblevel) then
    begin
        print_string (s^": ["^(string_of_int d1)^","^(string_of_int d2)^"]");
	print_newline ();
    end

let sddd l s d1 d2 d3 =
    if (l <= !dblevel) then
    begin
        print_string (s^": ["^(string_of_int d1)^","^(string_of_int d2)^","^
                              (string_of_int d3)^"]");
	print_newline ();
    end
let sdddd l s d1 d2 d3 d4 =
    if (l <= !dblevel) then
    begin
        print_string (s^": ["^(string_of_int d1)^","^(string_of_int d2)^","^
                              (string_of_int d3)^","^(string_of_int d4)^"]");
	print_newline ();
    end

end

let set_level l = 
    dblevel := l

