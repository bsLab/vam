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
    .3S
    .S4
        .NA S4 Named Paragraph .AN
        More words...
    .4S

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
        .IE
            .NA buf_physical .AN
            .AR ~size: int .RA
            .AR buffer .RA
            .NA = "ext_buf_physical" .AN 
        .EI
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
        .CV
            .NA myvar .AN
            .AR struct model .RA
        .VC
        .CS
            .NA c_struc .AN
            .AR char name[20] .RA
            .AR int len .RA
            .AR float zahl .RA
        .SC
    .NI
    Now we have an image:
    .FG
    .PI
        .NA logo.eps .AN
    .IP
    (Fig. 1) All the components of the VAMNET system together in an example
    configuration. .NL
    .GF
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
