
open Types;

value read_uc_char       : IO.istream -> uc_char;
value write_uc_char      : IO.ostream -> uc_char -> unit;
value to_unicode         : list int -> uc_list;
value from_unicode       : uc_list -> list int;
value string_to_bytes    : string -> list int;
value bytes_to_string    : list int -> string;
value of_ascii           : string -> uc_list;
value to_ascii           : uc_list -> string;
value uc_string_of_ascii : string -> uc_string;
value uc_string_to_ascii : uc_string -> string;
value of_string          : string -> uc_list;
value to_string          : uc_list -> string;
value append             : uc_string -> uc_string -> uc_string;
value set_string_format  : [> `ASCII | `Latin1 | `UTF8 | `Unicode ] -> unit;

