
type uc_char   = int;
type uc_list   = list uc_char;
type uc_string = array uc_char;

module DynamicCharMap: Map.S with type key = uc_char;

