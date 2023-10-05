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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2004-2006 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.09
**
**    $INFO:
**
** ManDoc frontend program
**
**    $ENDOFINFO
**
*)


open Printf
open Unix

open Doc_core
open Doc_html
open Doc_latex
open Doc_text

let version = "1.08"

type option =
    | Op_TextDoc     (* -text -o <docname.txt>         *)
    | Op_HtmlDoc     (* -html -o <docname.html>        *)
    | Op_LatexDoc    (* -latex -o <docname.tex>        *)
    | Op_PsDoc       (* -ps -o <docname.ps>            *)
    | Op_PdfDoc      (* -pdf -o <docname.ps>           *)
    | Op_Out of string 
    | Op_In of string
    | Op_None
    | Op_Print

let info str =
    print_string ("MLD: "^str);
    print_newline ()

let protect_sys system =
    try
    begin
        let stat = Unix.system system in
        match stat with
        | WEXITED n -> if n <> 0 then
                       begin
                            info (sprintf "system returned failure status %d: %s"
                                    n system);
                            exit 1;
                       end;
        | _ -> info ("unknown system status: "^system);
                    exit 1
    end
        with | _ -> info ("system execution failed: "^system);
                    exit 1

let html iname oname options =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    info "Creating html file(s)...";
    html_of_tree ds options []

                                                           
let latex iname oname options =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    info "Creating latex file...";
    tex_of_tree ds options []

let text iname oname options =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    info "Creating text file...";
    text_of_tree ds options []

                                                           
let ps iname oname options =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    info "Creating latex file...";    
    tex_of_tree ds options [];
    info "Converting tex to dvi (first run)...";
    protect_sys (sprintf "latex %s.tex" oname);
    info "Converting tex to dvi (second run)...";
    protect_sys (sprintf "latex %s.tex" oname);
    info "Converting dvi to ps...";
    protect_sys (sprintf "dvips -t a4 %s.dvi -o %s.ps" oname oname)
    
let pdf iname oname options =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    info "Creating pdf-latex file...";    
    tex_of_tree ds options [];
    info "Converting tex to pdf...";
    protect_sys (sprintf "pdflatex %s.tex" oname)
    
   
let print iname =
    info "Reading mandoc file...";
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in   
    print_tree ds

let usage s =
    let us = "
    mld [Options] -text -html -latex -ps -pdf -o <outputfile[.html,.tex,.ps,.pdf> <inputfile>   

    Modes:
        -text: Produce ASCII text output.

        -html: Produce HTML output. 
            (needs: eps2png for eps graphics)

        -latex: Produce Latex output.
        
        -ps: Produce Postscript output. 
            (needs: latex, dvips)

        -pdf: Produce Postscript output. 
            (needs: pdflatex, epstopdf)


    Options:
        -s1: [HTML only] divide document in S1 separated files 
             [Latex only] page break on section start
        -s12: [HTML only] divide document in S1 & S2 separated files 
             [Latex only] page break on section start
        -s123: [HTML only] divide document in S1 & S2 & S3 separated files 
             [Latex only] page break on section start
        -mp: [HTML only] each manual page starts with a new file
             [Latex only] page break on section start

        -color: [Latex only] colored output

        -toc: print a Table of Content (HTML: in a separate file).

        -html_body <file>: read user customized HTML from file. Header
              and trailer must be separated by the line:
              %%CONTENT%%

        -image_src <path>: source image directory [HTML only].
        -image_dst <path>: target image directory [HTML only].

        -man: Produce only manual page files (Sections 
                                              specified with S_Filename)
              No main file is created. [TEXT only]

        -copyright <str>: Set a copyright string append at the end
                          of each created file. [TEXT only]
    " in 
    output_string Pervasives.stdout (s^"\n"^"usage: \n"^us^"\n");
    exit 0

let read_file fname =
    let str = ref "" in
    try
    begin
        let ic = open_in fname in
        while  true
        do    
            str := !str ^ (input_line ic);
        done;
        !str
    end
    with | End_of_file -> !str;
         | _ -> usage ("reading of file failed: "^fname)


     
let _ =
    let argv = Array.to_list (Sys.argv) in
    let argn = List.length argv in
    
    let do_format  = ref Op_None in
    let do_out  = ref Op_None in
    let do_in   = ref Op_None in
    let doc_single = ref true in

    let options = ref [] in
        
    if (argn = 1) then
    begin
        usage "Input filename missing";
    end
    else
    begin
        let rec iter ol =
            match ol with
            | hd::tl -> 
              begin
                match hd with 
                | "-v" -> info (sprintf "mld version %s" version);
                          exit 0;
                | "-o" -> 
                  begin
                          let name = match tl with
                                     | hd'::tl' -> 
                                        begin
                                            iter tl';
                                            hd';
                                        end;
                                     | [] -> usage "Invalid -o option";   
                          in
                          do_out := Op_Out name;
                  end;        
                | "-html" -> do_format := Op_HtmlDoc; iter tl;
                | "-text" -> do_format := Op_TextDoc; iter tl;
                | "-latex" -> do_format := Op_LatexDoc; iter tl;
                | "-ps" -> do_format := Op_PsDoc; iter tl;
                | "-pdf" -> do_format := Op_PdfDoc; 
                            options := !options @ [Doc_pdftex];
                            iter tl;
                | "-print" -> do_format := Op_Print; iter tl;
                | "-man" -> options := !options @ [Doc_manfile];
                            iter tl;
                | "-s1" -> options := !options @ [Doc_multi_s1]; 
                           doc_single := false;
                           iter tl;
                | "-s12" -> options := !options @ [Doc_multi_s1;
                                                   Doc_multi_s2];
                           doc_single := false;
                           iter tl;
                | "-s123" -> options := !options @ [Doc_multi_s1;
                                                    Doc_multi_s2;
                                                    Doc_multi_s3];
                           doc_single := false;
                           iter tl;
                | "-mp" -> options := !options @ [Doc_multi_mp];
                           doc_single := false;
                           iter tl;
                | "-toc" -> options := !options @ [Doc_with_toc]; 
                           iter tl;
                | "-copyright" -> 
                begin
                    match tl with
                    | hd'::tl' -> options := !options @ [Doc_copyright hd'];
                                  iter tl';
                    | [] -> usage "missing copyright string";
                end;
                | "-html_body" -> 
                begin
                    let name = match tl with
                               | hd'::tl' -> 
                               begin
                                iter tl';
                                hd';
                               end;
                               | [] -> usage "Invalid -html_body option";   
                          in
                    let str = read_file name in
                    let str'' = Str.split (Str.regexp "%%CONTENT%%") str in
                    let str''_n = List.length str'' in
                    if str''_n <> 2 then
                        usage "Invalid HTML body file";

                    options := !options @ [Doc_head (List.nth str'' 0);
                                           Doc_tail (List.nth str'' 1)];
                end;
                | "-image_src" ->
                    let path = match tl with
                               | hd'::tl' -> 
                               begin
                                iter tl';
                                hd';
                               end;
                               | [] -> usage "Invalid -image_src option";   
                          in
                    options := !options @ [Doc_image_path_src path];
                | "-image_dst" ->
                    let path = match tl with
                               | hd'::tl' -> 
                               begin
                                iter tl';
                                hd';
                               end;
                               | [] -> usage "Invalid -image_dst option";   
                          in
                    options := !options @ [Doc_image_path_dst path];
                | "-color" -> options := !options @ [Doc_color]; 
                           iter tl;
                
                | s -> do_in := Op_In s; iter tl;
              end;
            | [] -> ();      
        in
        iter argv;

        match !do_format with
        | Op_HtmlDoc ->
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := Filename.chop_extension (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            if !doc_single then
                options := [Doc_single] @ !options;

            options := [Doc_Main (!oname^".html")] @ !options;

            html !iname !oname !options;
        end

        | Op_LatexDoc ->
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := Filename.chop_extension (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            if !doc_single then
                options := [Doc_single] @ !options;

            options := [Doc_Main (!oname^".tex")] @ !options;

            latex !iname !oname !options;
        end;

        | Op_TextDoc ->
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := Filename.chop_extension (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else if not (List.mem Doc_manfile !options) then
                usage "Output filename missing"
            else
                oname := "dummy.txt";
            
            if !doc_single then
                options := [Doc_single] @ !options;

            options := [Doc_Main (!oname^".txt")] @ !options;

            text !iname !oname !options;
        end;

        | Op_PsDoc ->
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := Filename.chop_extension (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            if !doc_single then
                options := [Doc_single] @ !options;

            options := [Doc_Main (!oname^".tex")] @ !options;

            ps !iname !oname !options;
        end;

        | Op_PdfDoc ->
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := Filename.chop_extension (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            if !doc_single then
                options := [Doc_single] @ !options;

            options := [Doc_Main (!oname^".tex")] @ !options;

            pdf !iname !oname !options;
        end;
        | Op_Print -> 
        begin
            let iname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";
            print !iname;
        end;
        | _ ->
                usage "Mode specifier missing!!!";
    end
