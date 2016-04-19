
open Engine;
open ParseState;

(* |begin_group| starts a new group. |end_group| ends the current group. *)

value begin_group ps = do
{
  add_node ps (Node.BeginGroup (location ps))
(*  open_node_list ps (current_mode ps); *)
};

value end_group ps = do
{
  add_node ps (Node.EndGroup (location ps))
(*  let nodes = close_node_list ps (current_mode ps);

  add_node ps (`Group nodes)
*)
};

