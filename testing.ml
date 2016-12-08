(* Check if w has a prefix in pipetree t called with input input *)
let rec test_word_prefix (w:word) (t:stringtree pipetree) (input:word) = match t with
  |Leaf(stringt)->
    begin
      let rec aux (stringt:stringtree) (w:word) = match w with
                    |[]->false
                    |x::r->match stringt with
                           |Nil->false
                           |Node(w,a,b,c) when w=[]->true
                           |Node(w,a,b,c)->match x with
                                           |A->aux a r
                                           |B->aux b r
                                           |C->aux c r
      in
      (aux stringt w)&&(prefix input w)
    end
  |AND(g,d)->(test_word_prefix w g input)&&(test_word_prefix w g input)
  |OR(g,d)->(test_word_prefix w g input)||(test_word_prefix w d input)

(* check if w is exactly contained in a pipetree t called with input input *)                                            
let rec test_word_equal (w:word) (t:stringtree pipetree) (input:word) = match t with
  |Leaf(stringt)->
    begin
      match (move stringt w) with
      |Node(w,_,_,_) when w!=[]->(prefix input w)
      |_->false
    end
  |AND(g,d)->((test_word_prefix w g input)&&(test_word_equal w d input))||((test_word_equal w g input)&&(test_word_prefix w d input))
  |OR(g,d)->(test_word_equal w g input)||(test_word_equal w d input)

    

let test_stream (t:stringtree pipetree) (s:word stream) (m:int) (arg:word) =
  print_newline();
  print_string("testing strm: ");
  let rec aux s m=match (s arg,m) with
    |(_,0)->()
    |(Endofstream,_)->print_string("Stream ended");
    |(Timeout(sprime),m)->(aux sprime m)
    |(Cons(x,r),m)->begin match test_word_equal x t arg with
                    |true->print_string (string_of_word x);
                    |false->print_string "Error:";
                            print_string (string_of_word x);
                            end;
                          print_string(" ");
                          aux r (m-1)
  in
  aux s m;
  print_newline();;
