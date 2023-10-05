.S1
.NA S1 Section .AN
The first Section in the document. .NL
Some more paragraphs...
.S2
    .NA S2 Subsection .AN
    The first subsection inside the section.
.2S
.S2
    .NA S2 Subsection 2 .AN
    The next subsection.
    .S3 
        .NA S3 Unit .AN
        This kind of unit is often used.
        .S4
            .NA S4 Named Paragraph .AN
            More words...
        .4S
    .3S

    The following programming interface gives an impression
    of the MANDoc system.
    .IN
        .NA ML .AN
        .IF
            .NA ml_fun .AN
            .RV ret1:int .VR
            .RV ret2:bool .VR
            .AR ~arg:float .RA
            .AR ~kind:bool .RA
        .FI
        .IF
            .NA ml_fun .AN
            .RV ret1:int .VR
            .RV ret2:bool .VR
            .AR arg:float .RA
            .AR kind:bool .RA
        .FI
        .IF
            .NA ml_fun11 .AN
            .RV unit .VR
            .AR () .RA
        .FI
        .IV
            .NA ml_value .AN
            .AR ~arg:float .RA
            .AR ~kind:bool .RA
            .AR int .RA
        .VI
        .IV
            .NA ml_value2 .AN
            .AR int .RA
        .VI
        .IE
            .NA ml_ext .AN
            .AR float .RA
            .AR bool .RA
            .AR int .RA
            .NA ext_c_fun .AN
        .EI
        .IT
            .NA ml_type .AN
            .AR T_Bold .RA
            .AR T_Italic .RA
            .AR T_BoldItalic .RA
            .AR T_Type .RA
            .AR T_Asis .RA
        .TI
        .IT
            .NA ml_type2 .AN
            .AR another_type .RA
        .TI
        .IX
            .NA Ml_exeception .AN
        .XI
        .IS
            .NA ml_struc .AN
            .AR .MU a:int .RA
            .AR .MU b:int .RA
            .AR .MU c:float .RA
        .SI
    .NI
    Modules are build from the above shown atomic interfaces:
    .IN
        .NA Module .AN
        .IM
            .NA ml_module .AN
            .IF
                .NA ml_fun .AN
                .RV ret1:int .VR
                .RV ret2:bool .VR
                .AR ~arg:float .RA
                .AR ~kind:bool .RA
            .FI
            .IF
                .NA ml_fun .AN
                .RV ret1:int .VR
                .RV ret2:bool .VR
                .AR arg:float .RA
                .AR kind:bool .RA
            .FI
            .IF
                .NA ml_fun11 .AN
                .RV unit .VR
                .AR () .RA
            .FI
            .IV
                .NA ml_value .AN
                .AR ~arg:float .RA
                .AR ~kind:bool .RA
                .AR int .RA
            .VI
            .IV
                .NA ml_value2 .AN
                .AR int .RA
            .VI 
            .IE
                .NA ml_ext .AN
                .AR float .RA
                .AR bool .RA
                .AR int .RA
                .NA ext_c_fun .AN
            .EI
            .IT
                .NA ml_type .AN
                .AR T_Bold .RA
                .AR T_Italic .RA
                .AR T_BoldItalic .RA
                .AR T_Type .RA
                .AR T_Asis .RA
            .TI
            .IT
                .NA ml_type2 .AN
                .AR another_type .RA
            .TI
            .IS
                .NA ml_struc .AN
                .AR .MU a:int .RA
                .AR .MU b:int .RA
                .AR .MU c:float .RA
            .SI
        .MI
    .NI
    Not only ML, C interfaces are supported, too.
    .IN
        .NA ML .AN

        .CF
            .NA mycfun .AN
            .RV struct module_s .VR
            .AR int a .RA
            .AR float b .RA
            .AR struct module_s *p .RA
        .FC
    .NI
.2S
.1S
.S2
    .NA S2 single .AN
    One more isolated S2 section.
    A table:
    .TB
        .TH Table .HT
        .TR
            .TC
    The special interfaces are used to show ML or C functions, types,
    submodules and values. Only the content must be given by the user,
    not the alignment. Commonly, they are collected in an interface
    subunit.
            .CT
            .TC
                1+1=2
            .CT
        .RT
    .BT
    .TB
        .TH Table .HT
        .TR
            .TC
                Operation
            .CT
            .TC
                1+1=2
            .CT
        .RT
    .BT
.2S
.S1
    .NA Next Section 1 .AN
    Here comes another section following the above main section.
.1S
