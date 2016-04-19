
open XNum;
open Unicode.Types;

value log_open      : string -> unit;
value log_string    : string -> unit;
value log_uc_list   : uc_list -> unit;
value log_uc_string : uc_string -> unit;
value log_int       : int -> unit;
value log_num       : num -> unit;
value log_info      : UCStream.location -> string -> unit;
value log_warn      : UCStream.location -> string -> unit;
value log_error     : UCStream.location -> string -> unit;

