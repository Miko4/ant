
open Unicode.Types;

type t 'a;

value empty                 : t 'a;
value is_empty              : t 'a -> bool;
value prefix                : t 'a -> uc_char -> t 'a;
value root_value            : t 'a -> option 'a;
value depth                 : t 'a -> int;

value find_string           : uc_string -> t 'a -> 'a;
value mem_string            : uc_string -> t 'a -> bool;
value find_list             : uc_list -> t 'a -> 'a;
value mem_list              : uc_list -> t 'a -> bool;
value generic_lookup        : ((t 'a -> uc_char -> t 'a) -> t 'a -> 'b -> t 'a) ->
                              'b -> t 'a -> option 'a;
value generic_lookup_prefix : (((t 'a * t 'a) -> uc_char -> (t 'a * t 'a)) ->
                               (t 'a * t 'a) -> 'b -> (t 'a * t 'a)) ->
                              'b -> t 'a -> option 'a;
value lookup_string         : uc_string -> t 'a -> option 'a;
value lookup_list           : uc_list -> t 'a -> option 'a;
value lookup_prefix_string  : uc_string -> t 'a -> option 'a;
value lookup_prefix_list    : uc_list -> t 'a -> option 'a;
value lookup_prefix_stream  : UCStream.istream -> t 'a -> option 'a;
value add_string            : uc_string -> 'a -> t 'a -> t 'a;
value remove_string         : uc_string -> t 'a -> t 'a;
value add_list              : uc_list -> 'a -> t 'a -> t 'a;
value remove_list           : uc_list -> t 'a -> t 'a;
value merge                 : t 'a -> t 'a -> t 'a;
value map                   : ('a -> 'b) -> t 'a -> t 'b;
value mapi                  : (uc_string -> 'a -> 'b) -> t 'a -> t 'b;
value iter                  : (uc_string -> 'a -> unit) -> t 'a -> unit;
value fold                  : (uc_string -> 'a -> 'b -> 'b) -> t 'a -> 'b -> 'b;

