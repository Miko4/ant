
open Runtime;
open Unicode;
open Logging;

(* counters *)

type counter =
{
  number : mutable int;            (* value of the counter *)
  reset  : mutable (list counter)  (* counters which arer reset everytime this counter changes *)
};

value make val =
{
  number = val;
  reset  = []
};

value rec set ctr val = do
{
  ctr.number := val;

  List.iter reset ctr.reset
}
and reset ctr = set ctr 0;

value add_reset ctr sub_ctr = do
{
  if List.memq sub_ctr ctr.reset then
    ()
  else
    ctr.reset := [sub_ctr :: ctr.reset]
};

(* counter table *)

type counter_table = DynUCTrie.t counter;

value empty_table = DynUCTrie.empty;

value new_counter loc table name val super = do
{
  if DynUCTrie.mem_string name table then do
  {
    log_warn loc "counter \"";
    log_uc_string name;
    log_string "\" redefined!\n"
  }
  else ();

  let new_ctr = make val;

  let new_table = DynUCTrie.add_string name new_ctr table;

  match super with
  [ None     -> ()
  | Some ctr -> do
    {
      try
        let c = DynUCTrie.find_string ctr new_table;

        add_reset c new_ctr
      with
      [ Not_found -> do
        {
          log_warn loc "counter \"";
          log_uc_string name;
          log_string "\" undefined!\n"
        }
      ]
    }
  ];

  new_table
};

value get_counter loc table name = do
{
  try
    let c = DynUCTrie.find_string name table;

    c.number
  with
  [ Not_found -> do
    {
      log_warn loc "counter \"";
      log_uc_string name;
      log_string "\" undefined!\n";
      0
    }
  ]
};

value set_counter loc table name val = do
{
  try do
  {
    let c = DynUCTrie.find_string name table;

    set c val;

    table
  }
  with
  [ Not_found -> new_counter loc table name val None ]
};

