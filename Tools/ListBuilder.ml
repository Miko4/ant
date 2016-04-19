
(* A |builder 'a| can be used to create a |list 'a| by successively appending elements
   to the end of the list. A list [x1;...; xn] is represented by the value [t; x1;...; xn]
   where <t> is a pointer to the last element of the list. *)

type builder 'a = list 'a;

(* |make ()| creates a new list builder. O(1) *)

value make () = do
{
  let rec b = [b];

  Obj.magic b
};


(*** private helper functions ***)


(* |tail <b>| returns the tail pointer and |list <b>| the current list. *)

value tail b = Obj.magic (List.hd b);
value list b = List.tl b;

(* |set_field <x> <y> <n>| sets the <n>th field of the block <x> to <y>. *)

value set_field x y n = Obj.set_field (Obj.repr x) n (Obj.repr y);

(* |set_tail <b> <x>| and |set_list <b> <x>| set, respectively, the tail pointer
   and the current list to <x>. *)

value set_tail b x = set_field b x 0;
value set_list b x = set_field b x 1;

value set_last b x = set_field (tail b) x 0;

(* |append_to_tail <b> <l>| appends the list <l> to <b> without changing
   the tail pointer. *)

value append_to_tail b l = do
{
  Obj.set_field (Obj.repr (tail b)) 1 (Obj.repr l)
};

(* |append_builders <b1> <b2>| appends the contents of <b2> to <b1>. <b2> is not modified.
   It is assumed that <b2> is non-empty and different from <b1>. *)

value append_builders b1 b2 = do
{
  append_to_tail b1 (list b2);
  set_tail b1 (tail b2)
};


(*** public functions ***)


(* |is_empty <b>| checks whether the current list is still empty.
   O(1), pure *)

value is_empty b = match list b with
[ [] -> True
| _  -> False
];

(* |clear <b>| empties the builder. O(1), non-pure *)

value clear b = do
{
  set_tail b b;
  append_to_tail b []
};

(* |add <b> <x>| adds the element <x> at the end of the current list.
   |add_first <b> <x>| adds <x> to the front. O(1), non-pure *)

value add b x = do
{
  let n = [x];

  append_to_tail b n;
  set_tail b n
};

value add_first b x = do
{
  if is_empty b then         (* If the list is empty we also need to update the tail pointer. *)
    add b x
  else
    set_list b [x :: list b]
};

(* |add_list <b> <x>| adds all elements of the list <x> to the end of the current
   list. O(x), non-pure *)

value rec add_list b lst = match lst with
[ []      -> ()
| [x::xs] -> do
  {
    add b x;
    add_list b xs
  }
];

(* |append <b1> <b2>| appends the contents of <b2> to <b1> and resets <b2>.
   O(1), non-pure *)

value append b1 b2 = match list b2 with
[ [] -> ()
| _  -> if b1 != b2 then do
        {
          append_builders b1 b2;
          clear b2
        }
        else
          invalid_arg "ListBuilder.append: equal arguments not permitted!"
];

(* |get <b>| returns the current list and resets the builder. O(1), non-pure *)

value get b = do
{
  let l = list b;

  clear b;

  l
};

(* |get_last <b>| returns the last element of the current list. O(1), pure *)

value get_last b = match tail b with
[ [] -> invalid_arg "ListBuilder.get_last: empty list!"
| _  -> List.hd (tail b)
];

(* |replace_last <b> <x>| replaces the last element of the current list with <x>. *)

value replace_last b x = match list b with
[ [] -> invalid_arg "ListBuilder.replace_last: empty list!"
| _  -> set_last b x
];

