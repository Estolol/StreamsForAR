(*
open Streams
open Stringtrees
open Conversion
 *)

let rec print_wordlist_aux (l:word list)  = match l with
  |[]->()
  |x::r->print_string (string_of_word x);
         print_string " ";
         print_wordlist_aux r;;
      
let print_wordlist (l:word list) =
  print_wordlist_aux l;;
  
let print_stream (s:word stream) (m:int) (arg:word) =
  print_newline();
  print_string("printing strm: ");
  let rec aux s m=match (s arg,m) with
    |(_,0)->()
    |(Endofstream,_)->()
    |(Timeout(sprime),m)->(aux sprime m)
    |(Cons(x,r),m)->print_string (string_of_word x);
                          print_string(" ");
                          aux r (m-1)
  in
  aux s m;
  print_newline();;

  
                                            
let print_pipetree (t:stringtree pipetree) =
  print_newline();
  let k=ref 1 in
  print_string("Structure: ");
  let rec aux t = match t with
    |Leaf(stringt)->print_string("Leaf{stringtree");
              print_int(!k);
              k:=(!k)+1;
              print_string("}");
    |AND(g,d)->print_string("AND[");
               aux g;
               print_string(",");
               aux d;
               print_string("]");
    |OR(g,d)->print_string("OR(");
              aux g;
              print_string(",");
              aux d;
              print_string(")");
  in aux t;
  print_newline();
  k:=1;   
  let rec auxtwo t = match t with
    |Leaf(stringt)->print_newline();
              print_string("Printing tree stringtree");
              print_int(!k);
              k:=(!k)+1;
              print_string(": ");
              print_wordlist (list_of_tree stringt);
    |AND(g,d)->auxtwo g;
               auxtwo d;
    |OR(g,d)->auxtwo g;
              auxtwo d;
  in auxtwo t;
     print_newline();;
