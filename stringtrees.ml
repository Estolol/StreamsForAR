(* we work with a three letters alphabet *)
type character = A | B | C
(* words are stored as lists of characters rather than strings to make it easier to loop over letters *)
type word = character list
(* Finally, we store sets of words as trees, 
which are either empty, 
or a Node with 
          either a word or no word (stored as []), 
          and three subtrees, each corresponding to a possibility for the next letter *)
type stringtree = Node of word * stringtree * stringtree * stringtree | Nil


(* generating random words *)         
open Random;;
let seed=53920693;;
let max_length=5;;

let random_word () =
  let length = (1+(Random.int (max_length))) in
  let l=ref [] in
  for j=1 to length do
    let x = match (Random.int 3) with
      |0->A
      |1->B
      |2->C
      |_->print_string("Error");
          C
    in l:=x::(!l);   
  done;
  !l;;

(* we add a word to a tree, at the position which corresponds to moving through the tree by following the word path *)
let rec add_to_tree_aux (path:word) (t:stringtree) (word:word) =
  match path with
  |[]->begin match t with
       |Nil->Node(word,Nil,Nil,Nil)
       |Node(w,a,b,c)->Node(word,a,b,c)
       end
  |x::r->match t with
         |Nil->begin match x with
               |A->let tprime = add_to_tree_aux r Nil word in
                   Node([],tprime,Nil,Nil)
               |B->let tprime = add_to_tree_aux r Nil word in
                   Node([],Nil,tprime,Nil)
               |C->let tprime = add_to_tree_aux r Nil word in
                   Node([],Nil,Nil,tprime)
               end
         |Node(w,a,b,c)->match x with
                       |A->let tprime = add_to_tree_aux r a word in
                           Node(w,tprime,b,c)
                       |B->let tprime = add_to_tree_aux r b word in
                           Node(w,a,tprime,c)
                       |C->let tprime = add_to_tree_aux r c word in
                           Node(w,a,b,tprime)

(* to add a word to a tree at the correct position, just pick the word as the path *)
let add_to_tree (word:word) (t:stringtree) = add_to_tree_aux word t word 
    
                       
           
               
(* to make a tree with at most size elements, we just start from an empty tree, then generate an element an add it to the tree size times *)              (* we can have a tree size strictly lower than size if we pick the same word at random several times*) 
let rec make_tree (size:int) = match size with
  |0->Nil
  |_->let tprime = make_tree (size-1) in
      (add_to_tree (random_word ()) tprime)

(* Here we make number trees with size elements *)        
let make number size =
  Random.init seed;
  let v = Array.make number Nil in
  for i=0 to (number-1) do
    v.(i)<-make_tree size;
  done;        
  v;;

(* returns true if w1 is a prefix of w2, false otherwise *)
let rec prefix (w1:word) (w2:word) = match w1 with
  |[]->true
  |x::r->match w2 with
         |[]->false
         |y::q->(x==y)&&(prefix r q)

(* assume w1 is a prefix of w2, remove w1 at the beginning of w2 and return the rest *)                          
let rec diff (w1:word) (w2:word) = match w1 with
  |[]->w2
  |x::r->match w2 with
         |[]->print_string("Error");
              []
         |y::q->diff r q

(* returns the subtree of t found after moving following the path path, returns Nil if the path leads nowhere *)
let rec move (t:stringtree) (path:word) = match path with
                    |[]->t
                    |x::r->match t with
                           |Nil->Nil
                           |Node(w,a,b,c)->match x with
                                           |A->move a r
                                           |B->move b r
                                           |C->move c r

(* Returns the next word stored in t, and removes it from t, in such a way as to guarantee the following invariant for every subtree tree of t: either tree is Nil or there is a word in tree *)                                                   
let rec find_next (t:stringtree) = match t with
  |Nil->(None,t)
  |Node(w,a,b,c) when w=[]->
    begin
      match (a,b,c) with
      |(Nil,Nil,Nil)->(None,Nil)
      |(Nil,Nil,_)->let (next,cprime) = find_next c in
                    begin
                      match cprime with
                      |Nil->(next,Nil)
                      |_->(next,Node(w,a,b,cprime))
                    end
      |(Nil,_,_)->let (next,bprime) = find_next b in
                  begin
                    match (bprime,c) with
                    |(Nil,Nil)->(next,Nil)
                    |_->(next,Node(w,a,bprime,c))
                  end
      |_->let (next,aprime) = find_next a in
          begin
            match (aprime,b,c) with
            |(Nil,Nil,Nil)->(next,Nil)
            |_->(next,Node(w,aprime,b,c))
          end
    end
  |Node(w,a,b,c)->
    match (a,b,c) with
    |(Nil,Nil,Nil)->(Some(w),Nil)     
    |_->(Some(w),Node([],a,b,c))
                                          
(* Computes the size of a stringtree *)                                  
let rec size (t:stringtree) = match t with  
  |Nil->0
  |Node(w,a,b,c) when w=[]->(size a) + (size b) + (size c)
  |Node(w,a,b,c)->1+(size a)+(size b)+(size c);;
                                    

      
    
