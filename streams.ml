(* Defining types *)
(* A stream required an argument to be computed. Once computed it is either empty (Endofstream), a timeout with a new stream to call, or an output and a remainer stream *)
type 'a comp_stream = Endofstream | Timeout of ('a stream) | Cons of 'a * ('a stream)
and 'a stream = 'a->'a comp_stream

(* a pipetree is used to store complex combinations of streams, it shows how different streams should be combined to form a new stream *)                       
type 'a pipetree = AND of 'a pipetree * 'a pipetree | OR of 'a pipetree * 'a pipetree | Leaf of 'a
                  
    
(* The or_ and and_ combine functions *)
let rec or_combine (flot1:'a stream) (flot2:'a stream) =
  let or_stream arg =
    match flot1 arg with
    |Endofstream->flot2 arg
    |Timeout(s)->begin match flot2 arg with
                 |Endofstream->Timeout(s)
                 |Timeout(sprime)->Timeout(or_combine s sprime)
                 |Cons(b,flot3)->Cons(b, or_combine s flot3)
                  end  
    |Cons(a,flot3)->Cons(a,or_combine flot2 flot3)
  in or_stream

(* left_to_right is needed for and_combine, it simply loops over elements from the first stream and applies them to the second *)     
let rec left_to_right (flot1:'a stream) (flot2: 'a stream) =
  let ltr_stream arg=
    match flot1 arg with
    |Endofstream->Endofstream
    |Timeout(s)->Timeout(left_to_right s flot2)
    |Cons(a,flot3)->match flot2 a with
                    |Endofstream->Timeout(left_to_right flot3 flot2)
                    |Timeout(s)->Timeout(or_combine (left_to_right flot3 flot2) s)
                    |Cons(b,flot4)->Cons(b,or_combine (left_to_right flot3 flot2) flot4)
  in ltr_stream
  
                                    
       
let rec and_combine (flot1:'a stream) (flot2:'a stream) =
  or_combine (left_to_right flot1 flot2) (left_to_right flot2 flot1)
  
