
(* A |builder 'a| can be used to create a |list 'a| by successively appending
   elements to the end of the list. *)

type builder 'a;

(* |make ()| creates a new list builder. O(1) *)

value make : unit -> builder 'a;

(* |is_empty <b>| checks whether the current list is still empty.
   O(1), pure *)

value is_empty : builder 'a -> bool;

(* |clear <b>| empties the builder. O(1), non-pure *)

value clear : builder 'a -> unit;

(* |add <b> <x>| adds the element <x> at the end of the current list.
   |add_first <b> <x>| adds <x> to the front. O(1), non-pure *)

value add       : builder 'a -> 'a -> unit;
value add_first : builder 'a -> 'a -> unit;

(* |add_list <b> <x>| adds all elements of the list <x> to the end of the current
   list. O(x), non-pure *)

value add_list : builder 'a -> list 'a -> unit;

(* |append <b1> <b2>| appends the contents of <b2> to <b1> and resets <b2>.
   O(1), non-pure *)

value append : builder 'a -> builder 'a -> unit;

(* |get <b>| returns the current list and resets the builder. O(1), non-pure *)

value get : builder 'a -> list 'a;

(* |get_last <b>| returns the last element of the current list. O(1), pure *)

value get_last : builder 'a -> 'a;

(* |replace_last <b> <x>| replaces the last element of the current list with <x>. *)

value replace_last : builder 'a -> 'a -> unit;

