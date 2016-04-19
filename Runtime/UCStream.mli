
open Unicode.Types;

type location = (string * int * int);

type istream;

value create        : unit -> istream;
value of_list       : uc_list -> istream;
value to_list       : istream -> uc_list;
value of_string     : uc_string -> istream;
value to_string     : istream -> uc_string;
value of_file       : string -> istream;
value assign        : istream -> istream -> unit;
value exchange      : istream -> istream -> unit;
value duplicate     : istream -> istream;
value eof           : istream -> bool;
value location      : istream -> location;
value set_location  : istream -> location -> bool -> unit;
value get_char      : istream -> int -> uc_char;
value next_char     : istream -> uc_char;
value take          : istream -> int -> uc_list;
value remove        : istream -> int -> unit;
value pop           : istream -> uc_char;
value clear         : istream -> unit;
value match_prefix  : istream -> uc_list -> bool;
value insert_list   : istream -> uc_list -> unit;
value insert_string : istream -> uc_string -> unit;
value insert_stream : istream -> istream -> unit;
value include_file  : istream -> string -> unit;

