let rec char_list_of_word (w:word) = match w with
  |[]->[]
  |x::r->let lprime = char_list_of_word r in
         match x with
         |A->'a'::lprime
         |B->'b'::lprime
         |C->'c'::lprime
                    
let string_of_word (w:word) = String.concat "" (List.map (String.make 1) (char_list_of_word w))

                                      
    
(* Creating an actual stream from a stringtree. *)
let rec stream_of_tree_aux (t: stringtree) (position:word) (arg:word) =
  match t with
  |Nil->Endofstream
  |_->
    let pref = prefix arg position in
    match ((prefix position arg)||pref) with
    |false->Endofstream
    |true->
      let (tmoved,newpos) = match pref with
        |false->(move t (diff position arg),arg)
        |true->(t,position)
      in
      let (next,tprime) = find_next tmoved in
      match next with
      |None->Endofstream
      |Some(word)->
        let f nextarg = stream_of_tree_aux tprime newpos nextarg in
        Cons(word,f)


let stream_of_tree (t:stringtree) = stream_of_tree_aux t []


let rec stream_of_pipetree (t:stringtree pipetree) = match t with
  |AND(g,d)->and_combine (stream_of_pipetree g) (stream_of_pipetree d)
  |OR(g,d)->or_combine (stream_of_pipetree g) (stream_of_pipetree d)
  |Leaf(stringt)->stream_of_tree stringt

(* compute the list of words with prefix arg in tree t *)
let rec list_of_tree_aux (t:stringtree) (l:word list) = match t with
  |Nil->l
  |Node(w,a,b,c)->
    let l1 = list_of_tree_aux c l in
    let l2 = list_of_tree_aux b l1 in
    let l3 = list_of_tree_aux a l2 in
    match w with
    |[]->l3
    |_->w::l3
             
let list_of_tree (t:stringtree) (arg:word) = list_of_tree_aux (move t arg) []

let rec insert (x:'a) (l:'a list) = match l with
  |[]->[x]
  |y::r when y=x->l
  |y::r->y::(insert x r)

let rec merge (l1:'a list) (l2:'a list) = match l1 with
  |[]->l2
  |x::r->insert x (merge r l2)

let rec has_prefix (w:word) (l:word list) = match l with
  |[]->false
  |x::r->(prefix x w)||(has_prefix w r)
                         
let rec left_to_right_list (l:word list) (t:stringtree pipetree) = match l with
  |[]->[]
  |x::r->(merge (list_of_pipetree t x) (left_to_right_list r t))
and list_of_pipetree (ppt:stringtree pipetree) (arg:word)= match ppt with
  |Leaf(t)->(list_of_tree t arg)
  |AND(g,d)->let l1 = list_of_pipetree g arg in
             let l2 = list_of_pipetree d arg in
             merge (left_to_right_list l1 d) (left_to_right_list l2 g)
  |OR(g,d)->let l1 = list_of_pipetree g arg in
            let l2 = list_of_pipetree d arg in
            merge l1 l2
