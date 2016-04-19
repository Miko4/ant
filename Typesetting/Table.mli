
open Box;

type table_entry 'a =
{
  te_left     : int;       (* first column of the entry *)
  te_right    : int;       (* its last column           *)
  te_top      : int;       (* its first row             *)
  te_baseline : int;       (* the row of the baseline   *)
  te_bottom   : int;       (* the last row              *)
  te_contents : 'a         (* the contents of the entry *)
};

value make : int -> int -> list (table_entry (list box)) -> Galley.line_params -> box;

