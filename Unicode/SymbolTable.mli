
open Types;

type symbol = int;

value alloc_symbol     : unit -> symbol;        (* returns a symbol without string representation *)
value string_to_symbol : uc_string -> symbol;
value symbol_to_string : symbol -> uc_string;

module SymbolMap : Map.S with type key = int;

value map_to_list : SymbolMap.t 'a -> list (symbol * 'a);

