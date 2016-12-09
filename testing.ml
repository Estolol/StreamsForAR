let rec remove (x:'a) (l:'a list) = match l with
  |[]->[]
  |y::r when y=x->r
  |y::r->y::(remove x r)

let rec print_list (l:word list) = match l with
  |[]->()
  |w::r->print_string (string_of_word w);
         print_string " ";
         print_list r;;
  
(* stream testing function: we compare the stream's outputs to the expected outputs computed from the trees *)
let test_stream (t:stringtree pipetree) (s:word stream) (m:int) (arg:word) =
  print_newline();
  print_string("testing strm: ");
  let expected = list_of_pipetree t arg in
  let unseen = ref expected in
  let rec aux s m=match (s arg,m) with
    |(_,0)->()
    |(Endofstream,_)->print_string("Stream ended");
    |(Timeout(sprime),m)->(aux sprime m)
    |(Cons(x,r),m)->begin match (List.mem x expected) with
                    |true->unseen:=(remove x (!unseen));
                           print_string (string_of_word x);
                    |false->print_string "Error:";
                            print_string (string_of_word x);
                            end;
                          print_string(" ");
                          aux r (m-1)
  in
  aux s m;
  match (!unseen) with
  |[]->()
  |_->print_newline ();
      print_string "Unseen: ";
      print_list (!unseen);
  print_newline();;
