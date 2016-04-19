
open XNum;
open IO_Base;

type istream   = IO_Base.istream;
type irstream  = IO_Base.irstream;
type ostream   = IO_Base.ostream;
type orstream  = IO_Base.orstream;
type iostream  = IO_Base.iostream;
type iorstream = IO_Base.iorstream;

value coerce_i   : io [> io_r   ] -> istream;
value coerce_o   : io [> io_w   ] -> ostream;
value coerce_ir  : io [> io_rs  ] -> irstream;
value coerce_or  : io [> io_ws  ] -> orstream;
value coerce_io  : io [> io_rw  ] -> iostream;
value coerce_ior : io [> io_rws ] -> iorstream;

(* IO routines *)

value size          : io [> io_s ] -> int;
value pos           : io [> io_s ] -> int;
value seek          : io [> io_s ] -> int -> unit;
value skip          : io [> io_r ] -> int -> unit;
value bytes_written : io [> io_w ] -> int;
value eof           : io [> io_r ] -> bool;
value free          : io 'a -> unit;

(* Append the contents of a channel to a stream. *)

value append_channel : io [> io_w ] -> in_channel -> unit;

value append : io [> io_w ] -> io [> io_r ] -> unit;

(* Write the contents of a stream to a channel. *)

value to_channel : io [> io_r ] -> out_channel -> unit;

value to_string   : io [> io_rs ] -> string;
value from_string : string -> iorstream;
value to_buffer   : io [> io_r ] -> iorstream;

value sub_stream : io [> io_r ] -> int -> iorstream;

(* reading from a stream *)

value read_char   : io [> io_r ] -> char;
value read_byte   : io [> io_r ] -> int;
value read_string : io [> io_r ] -> int -> string;

value peek_char   : io [> io_rs  ] -> int -> char;
value peek_string : io [> io_rs  ] -> int -> int -> string;
value skip_while  : io [> io_rs  ] -> (char -> bool) -> unit;

(* reading bigendian integers *)

value read_be_u8  : io [> io_r ] -> int;
value read_be_u16 : io [> io_r ] -> int;
value read_be_u24 : io [> io_r ] -> int;
value read_be_u32 : io [> io_r ] -> num;
value read_be_i8  : io [> io_r ] -> int;
value read_be_i16 : io [> io_r ] -> int;
value read_be_i24 : io [> io_r ] -> int;
value read_be_i32 : io [> io_r ] -> num;

value read_utf8_char : io [> io_r ] -> int;

(* writing to a stream *)

value write_char   : io [> io_w ] -> char -> unit;
value write_byte   : io [> io_w ] -> int -> unit;
value write_string : io [> io_w ] -> string -> unit;

value printf       : io [> io_w ] -> format4 'a unit string unit -> 'a;

value write_be_u8  : io [> io_w ] -> int -> unit;
value write_be_u16 : io [> io_w ] -> int -> unit;
value write_be_u24 : io [> io_w ] -> int -> unit;
value write_be_u32 : io [> io_w ] -> num -> unit;
value write_be_i8  : io [> io_w ] -> int -> unit;
value write_be_i16 : io [> io_w ] -> int -> unit;
value write_be_i24 : io [> io_w ] -> int -> unit;
value write_be_i32 : io [> io_w ] -> num -> unit;

value write_utf8_char : io [> io_w ] -> int -> unit;

(* compression *)

value compress   : io [> io_rs ] -> int -> irstream;
value uncompress : io [> io_rs ] -> irstream;

(* implementations *)

value make_in_stream      : string -> istream;
value make_out_stream     : string -> ostream;
value make_rand_in_stream : string -> irstream;
value make_buffer_stream  : int    -> iorstream;
value make_string_stream  : string -> irstream;

