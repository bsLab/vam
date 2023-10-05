(*
** Test the backtrace generator in Vam
*)


external extraise: unit -> unit = "extraise"

exception Bt_test

let t () =
    extraise ()

let f3 a1 a2 =
    let a = Array.create 1 0 in
#ifdef 0
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
#endif
#ifdef DEBUG
    Db.Pr.ss 1 "Debug test" "Test";
#endif
    a.(2) <- 1 + 4;
    let res = a1 + a.(0) + a2 in
#ifdef DEBUG
    Db.Pr.ss 1 "Debug test" "Test";
#endif
    if (res > 0) then
        failwith "Backtrace test";
    res

let f2 b1 b2 =
  let f = ref 0 in
  for i = 0 to 100
  do
#ifdef DEBUG
    Db.Pr.ss 1 "Debug test" "Test";
#endif
#ifdef 0
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
    Db.Pr.ss 1 "Debug test" "Test";
#endif
#ifdef DEBUG
    Db.Pr.ss 1 "Debug test" "Test";
#endif
    let u = b1 + b2 in
    let w = b1 - b2 in
    f:=f3 u w; 
  done;
  !f

let test () =
  for z = 1 to 20
  do
    let i = z * 100 in
    ignore(f2 z i); 
    ignore(f3 z i);
  done;
#ifdef DEBUG
    Db.Pr.ss 1 "Debug test" "Test";
#endif
  1

    
        