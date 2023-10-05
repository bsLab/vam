open Thread

let f1 a b i =
    if (i=2) then print_string (thread_debuginfo(false));
    let c = a + 1 in
    let d = b + a - 1 in
    let x = 0x048110/2 in 
    c*d+x

let f2 c d =
    for i = 1 to 2
    do
        let z = c * d in
        ignore(f1 z (z/10) i)
    done

let test () =
    ignore(f2 12 13)
