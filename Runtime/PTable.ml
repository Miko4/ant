
open Unicode.Types;

(* implementation of a table with a selected element *)

type table 'a =
{
  current : 'a;                         (* the selected element            *)
  key     : uc_string;                  (* the key of the selected element *)
  table   : DynUCTrie.t 'a       (* the whole table                 *)
};

(* |create <key> <element>| creates a table containing the pair <key>, <element>. *)

value create key element =
{
  current = element;
  key     = key;
  table   = DynUCTrie.add_string key element DynUCTrie.empty
};

(* |sync <t>| adds the current element back to the table. *)

value sync t =
{
  (t)

  with

  table = DynUCTrie.add_string t.key t.current t.table
};

(* |current <table>| returns the current element and |key <table>| returns its key. *)

value current t = t.current;
value key     t = t.key;

(* |table <table|| returns the table. *)

value table t = do
{
  (sync t).table
};

(* |get <table> <key>| searchs for <key> without affecting the current element. *)

value get table key = do
{
  let t = sync table;

  DynUCTrie.find_string key t.table
};

(* |select <table> <key>| sets the selected element. *)

value select table key = do
{
  let t = sync table;

  {
    (t)

    with

    key     = key;
    current = DynUCTrie.find_string key t.table
  }
};

(* |add <table> <key> <element>| adds a new elemement to the table. *)

value add table key element = do
{
  let t = sync table;

  {
    key     = key;
    current = element;
    table   = DynUCTrie.add_string key element t.table
  }
};

(* |set <table> <element>| modifies the selected element without updating the whole table. *)

value set table element =
{
  (table)

  with

  current = element
};

(* |update <table> <t>| replaces the table <table> with <t>. *)

value update table t =
{
  key     = table.key;
  current = DynUCTrie.find_string table.key t;
  table   = t
};

(* |map <table> <f>| applies <f> to all elements of <table>. *)

value map table f = do
{
  let t = sync table;

  update t (DynUCTrie.map f t.table)
};

(* |mapi <table> <f>| applies <f> to all elements of <table>. *)

value mapi table f = do
{
  let t = sync table;

  update t (DynUCTrie.mapi f t.table)
};

