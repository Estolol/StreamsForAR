open Stringtrees
open Streams
open Conversion
open Printing
open Testing


(* creating a bunch of stringtrees and a pipetree *)
let create_big_pipetree () =
  let v=make 7 40 in
  let (t1,t2,t3,t4,t5,t6,t7)=(v.(0),v.(1),v.(2),v.(3),v.(4),v.(5),v.(6)) in
  AND(OR(AND(AND(AND(Leaf(t1),Leaf(t2)),OR(Leaf(t3),Leaf(t4))),Leaf(t5)),Leaf(t6)),Leaf(t7));;

let create_small_pipetree () =
  let v=make 4 10 in
  let (t1,t2,t3,t4)=(v.(0),v.(1),v.(2),v.(3)) in
  AND(AND(Leaf(t1),Leaf(t2)),OR(Leaf(t3),Leaf(t4)));;

(* final testing function *)
let test (t:stringtree pipetree)  (stream_display:int) (arg:word) =
  print_pipetree t;
  let s = stream_of_pipetree t in
  (* s1 has an extra fake element *)
  let sfake1 b = Cons([C;C;B;C;C;C;C],s) in
  (* s2 has less elements *)
  let Cons(_,sfake2)= s [C] in
  print_newline ();
  print_string "Natural test, hopefully everything goes right:";
  test_stream t s stream_display arg;
  print_newline();
  print_newline();
  print_string "Here we expect to see an error at the beginning:";
  test_stream t sfake1 stream_display arg;
  print_newline();
  print_newline();
  print_string "Here we expect to have unseen elements:";
  test_stream t sfake2 stream_display arg;;


  test (create_small_pipetree ()) 10000 [];;
  test (create_big_pipetree ()) 10000 [];;
