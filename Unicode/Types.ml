
type uc_char   = int;
type uc_list   = list uc_char;
type uc_string = array uc_char;

module OrderedChar =
struct

  type t = uc_char;

  value compare = (compare : uc_char -> uc_char -> int);

end;

module DynamicCharMap = Map.Make(OrderedChar);

