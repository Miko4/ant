
type io +'a;

type io_r   = [= `IO_Read];
type io_w   = [= `IO_Write];
type io_s   = [= `IO_Seek];
type io_rs  = [= io_r | io_s];
type io_ws  = [= io_w | io_s];
type io_rw  = [= io_r | io_w];
type io_rws = [= io_r | io_w | io_s];

type istream   = io io_r;
type irstream  = io io_rs;
type ostream   = io io_w;
type orstream  = io io_ws;
type iostream  = io io_rw;
type iorstream = io io_rws;

value io_make :
  (unit -> unit) ->
  (unit -> char) -> (int -> string)  -> (unit -> bool) ->
  (char -> unit) -> (string -> unit) -> (unit -> int)  ->
  (unit -> int)  -> (unit -> int)    -> (int -> unit)  -> iorstream;

value io_make_read :
  (unit -> unit) ->
  (unit -> char) -> (int -> string)  -> (unit -> bool) -> istream;

value io_make_write :
  (unit -> unit) ->
  (char -> unit) -> (string -> unit) -> (unit -> int)  -> ostream;

value io_make_read_seek :
  (unit -> unit) ->
  (unit -> char) -> (int -> string)  -> (unit -> bool) ->
  (unit -> int)  -> (unit -> int)    -> (int -> unit)  -> irstream;

value io_make_write_seek :
  (unit -> unit) ->
  (char -> unit) -> (string -> unit) -> (unit -> int)  ->
  (unit -> int)  -> (unit -> int)    -> (int -> unit)  -> orstream;

value io_make_read_write :
  (unit -> unit) ->
  (unit -> char) -> (int -> string)  -> (unit -> bool) ->
  (char -> unit) -> (string -> unit) -> (unit -> int)  -> iostream;

value io_free          : io 'a        -> unit;
value io_read_char     : io [> io_r ] -> char;
value io_read_string   : io [> io_r ] -> int -> string;
value io_eof           : io [> io_r ] -> bool;
value io_write_char    : io [> io_w ] -> char -> unit;
value io_write_string  : io [> io_w ] -> string -> unit;
value io_bytes_written : io [> io_w ] -> int;
value io_size          : io [> io_s ] -> int;
value io_pos           : io [> io_s ] -> int;
value io_seek          : io [> io_s ] -> int -> unit;

value io_coerce_i   : io [> io_r   ] -> istream;
value io_coerce_o   : io [> io_w   ] -> ostream;
value io_coerce_ir  : io [> io_rs  ] -> irstream;
value io_coerce_or  : io [> io_ws  ] -> orstream;
value io_coerce_io  : io [> io_rw  ] -> iostream;
value io_coerce_ior : io [> io_rws ] -> iorstream;


