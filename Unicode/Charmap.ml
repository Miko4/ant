
open Types;

type charmap 'a = array (array 'a);

value create x = do
{
  let page = Array.make 0x100 x;

  Array.make 0x100 page
};

(* Converts array (array 'a) into charmap 'a. *)

value build array = do
{
  (* check whether array is a valid charma *)

  if Array.length array < 256 then
    invalid_arg "Runtime.Charmap.build"
  else ();

  for i = 0 to Array.length array - 1 do
  {
    if Array.length array.(i) <> 256 then
      invalid_arg "Runtime.Charmap.build"
    else ();
  };

  array
};

value lookup map char = do
{
  let i = char / 0x100;
  let k = char mod 0x100;

  if i < Array.length map then
    map.(i).(k)
  else
    invalid_arg "Charmap.lookup"
};

value set map char x = do
{
  let i = char / 0x100;
  let k = char mod 0x100;

  if i < Array.length map then do
  {
    map.(i) := Array.copy map.(i);
    map.(i).(k) := x
  }
  else
    invalid_arg "Charmap.lookup"
};

value copy map = do
{
  Array.copy map
};

