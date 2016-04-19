
open XNum;
open IO_Base;

value num_0x100       = num_of_int 0x100;
value num_0x10000     = num_of_int 0x10000;
value num_0x1000000   = num_of_int 0x1000000;
value num_0x100000000 = num_0x10000 */ num_0x10000;
value num_0x80000000  = num_0x100000000 // num_of_int 2;

type istream   = IO_Base.istream;
type irstream  = IO_Base.irstream;
type ostream   = IO_Base.ostream;
type orstream  = IO_Base.orstream;
type iostream  = IO_Base.iostream;
type iorstream = IO_Base.iorstream;

value coerce_i   = io_coerce_i;
value coerce_o   = io_coerce_o;
value coerce_ir  = io_coerce_ir;
value coerce_or  = io_coerce_or;
value coerce_io  = io_coerce_io;
value coerce_ior = io_coerce_ior;

(* IO routines ************************************************************************************)

value size           = io_size;
value pos            = io_pos;
value seek           = io_seek;
value bytes_written  = io_bytes_written;
value eof            = io_eof;
value free           = io_free;
value read_char      = io_read_char;
value read_string    = io_read_string;
value write_char     = io_write_char;
value write_string   = io_write_string;

value skip cs off    = ignore (io_read_string cs off);

(* reading from a stream *)

value read_byte cs = do
{
  if io_eof cs then
    -1
  else
    int_of_char (io_read_char cs)
};

value peek_char cs off = do
{
  let pos = io_pos cs;

  io_seek cs (pos + off);

  let chr = io_read_char cs;

  io_seek cs pos;

  chr
};

value rec peek_string cs off len = do
{
  let pos = io_pos cs;

  io_seek cs (pos + off);

  let str = io_read_string cs len;

  io_seek cs pos;

  str
};

value skip_while cs p = do
{
  while not (io_eof cs) && p (peek_char cs 0) do
  {
    skip cs 1
  }
};

(* reading bigendian integers *)

value read_be_u8 cs = read_byte cs;

value read_be_u16 cs = do
{
  let x = read_byte cs;
  let y = read_byte cs;

  0x100 * x + y
};

value read_be_u24 cs = do
{
  let x = read_byte cs;
  let y = read_byte cs;
  let z = read_byte cs;

  0x10000 * x + 0x100 * y + z
};

value read_be_u32 cs = do
{
  let x = read_be_u16 cs;
  let y = read_be_u16 cs;

  num_of_int 0x10000 */ num_of_int x +/ num_of_int y
};

value read_be_i8 cs = do
{
  let x = read_be_u8 cs;

  if x > 0x7f then
    x - 0x100
  else
    x
};

value read_be_i16 cs = do
{
  let x = read_be_u16 cs;

  if x > 0x7fff then
    x - 0x10000
  else
    x
};

value read_be_i24 cs = do
{
  let x = read_be_u24 cs;

  if x > 0x7fffff then
    x - 0x1000000
  else
    x
};

value read_be_i32 cs = do
{
  let x = read_be_u32 cs;

  if x >=/ num_0x80000000 then
    x -/ num_0x100000000
  else
    x
};

(* writing to a stream *)

value write_byte cs x = io_write_char cs (Char.unsafe_chr (x land 0xff));

value printf cs = Printf.kprintf (write_string cs);

value write_be_u8 cs x = do
{
  write_byte cs x
};

value write_be_u16 cs x = do
{
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)
};

value write_be_u24 cs x = do
{
  write_byte cs ((x lsr 16) land 0xff);
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)
};

value write_be_u32 cs n = do
{
  let y = mod_num n (num_of_int 0x10000);
  let x = quo_num (n -/ y) (num_of_int 0x10000);
  let u = int_of_num x;
  let v = int_of_num y;
  let b = (0x10000 + u) mod 0x100;
  let a = (u - b) / 0x100;
  let d = (0x10000 + v) mod 0x100;
  let c = (v - d) / 0x100;

  write_byte cs a;
  write_byte cs b;
  write_byte cs c;
  write_byte cs d
};

value write_be_i8 cs x = do
{
  if x < 0 then
    write_byte cs (0x100 + x)
  else
    write_byte cs x
};

value write_be_i16 cs x = do
{
  if x < 0 then
    write_be_u16 cs (0x10000 + x)
  else
    write_be_u16 cs x
};

value write_be_i24 cs x = do
{
  if x < 0 then
    write_be_u24 cs (0x1000000 + x)
  else
    write_be_u24 cs x
};

value write_be_i32 cs n = do
{
  let y = mod_num n (num_of_int 0x10000);
  let x = quo_num (n -/ y) (num_of_int 0x10000);
  let u = int_of_num x;
  let v = int_of_num y;
  let b = (0x10000 + u) mod 0x100;
  let a = (u - b) / 0x100;
  let d = (0x10000 + v) mod 0x100;
  let c = (v - d) / 0x100;

  if a < 0 then
    write_byte cs (a + 0x100)
  else
    write_byte cs a;

  write_byte cs b;
  write_byte cs c;
  write_byte cs d
};

value read_utf8_char cs = do
{
  let c = read_byte cs;

  if c < 0x80 then
    c
  else if c < 0xc0 then
    c                            (* should never happen *)
  else if c < 0xe0 then do
  {
    let c2 = read_byte cs;

    if c2 < 0 then
      0x40 * (c - 0xc0)
    else
      0x40 * (c - 0xc0) + c2 - 0x80
  }
  else do
  {
    let c2 = read_byte cs;
    let c3 = read_byte cs;

    if c2 < 0 then
      0x1000 * (c - 0xe0)
    else if c3 < 0 then
      0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80)
    else
      0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80) + c3 - 0x80
  }
};

value write_utf8_char cs x = do
{
  if x < 0x80 then
    write_byte cs x
  else if x < 0x800 then do
  {
    write_byte cs (0xc0 + (x lsr 6));
    write_byte cs (0x80 + (x land 0x3f))
  }
  else do
  {
    write_byte cs (0xe0 + (x lsr 12));
    write_byte cs (0x80 + ((x lsr 6) land 0x3f));
    write_byte cs (0x80 + (x land 0x3f))
  }
};


(* Implementations ********************************************************************************)


(* |power_of_two <x>| returns the greatest power of two less than or equal to <x>. *)

value power_of_two x = do
{
  iter 1 x

  where rec iter p x = do
  {
    if x = 0 then
      (p lsr 1)
    else
      iter (p lsl 1) (x lsr 1)
  }
};

(* in_channels *)

value in_channel_read_char ic eof = do
{
  try
    input_char ic
  with
  [ End_of_file -> do
    {
      !eof := True;
      '\000'
    }
  ]
};

value in_channel_read_string ic eof len = do
{
  let buf = String.create len;

  iter 0 len

  where rec iter off len = do
  {
    let read = input ic buf off len;

    if read = len then
      buf
    else if read = 0 then do
    {
      !eof := True;
      ""
    }
    else
      iter (off + read) (len - read)
  }
};

value make_in_stream filename = do
{
  let ic  = open_in_bin filename;
  let eof = ref False;

  io_make_read
    (fun ()  -> close_in ic)
    (fun ()  -> in_channel_read_char ic eof)
    (fun len -> in_channel_read_string ic eof len)
    (fun ()  -> !eof)
};

value make_rand_in_stream filename = do
{
  let ic  = open_in_bin filename;
  let eof = ref False;

  io_make_read_seek
    (fun ()  -> close_in ic)
    (fun ()  -> in_channel_read_char ic eof)
    (fun len -> in_channel_read_string ic eof len)
    (fun ()  -> !eof)
    (fun ()  -> in_channel_length ic)
    (fun ()  -> pos_in ic)
    (fun off -> seek_in ic off)
};

value make_out_stream filename = do
{
  let oc = open_out_bin filename;

  io_make_write
    (fun ()  -> close_out oc)
    (fun chr -> output_char   oc chr)
    (fun str -> output_string oc str)
    (fun ()  -> pos_out oc)
};

value make_rand_out_stream filename = do
{
  let oc = open_out_bin filename;

  io_make_write_seek
    (fun ()  -> close_out oc)
    (fun chr -> output_char   oc chr)
    (fun str -> output_string oc str)
    (fun ()  -> pos_out oc)
    (fun ()  -> out_channel_length oc)
    (fun ()  -> pos_out oc)
    (fun off -> seek_out oc off)
};

value make_buffer_stream buffer_size = do
{
  let buf_size = power_of_two (min buffer_size Sys.max_string_length);

  let buffer = ref [| String.create buf_size |];
  let size   = ref 0;
  let pos    = ref 0;

  let resize_buffer len = do
  {
    !buffer := Array.init len
                 (fun i ->
                   if i < Array.length !buffer then
                     !buffer.(i)
                   else
                     String.create buf_size)
  };

  (* doubles the size of the buffer array *)

  let double_buffer () = do
  {
    resize_buffer (2 * Array.length !buffer)
  };

  (* returns the entry in the buffer array that contains the given position *)

  let rec get_buffer pos = do
  {
    if pos < buf_size * Array.length !buffer then
      !buffer.(pos / buf_size)
    else do
    {
      double_buffer ();
      get_buffer pos
    }
  };

  let get_buffer_pos pos =
    pos land (buf_size - 1);

  (* returns the offset within a buffer of the given position *)

  let get_char pos = (get_buffer pos).[get_buffer_pos pos];

  let set_char pos chr = do
  {
    (get_buffer pos).[get_buffer_pos pos] := chr
  };

  let seek new_pos = do
  {
    if new_pos < 0 then
      !pos := 0
    else if new_pos >= !size then
      !pos := !size
    else
      !pos := new_pos
  };
  let read_char () = do
  {
    if !pos >= !size then
      '\000'
    else do
    {
      let c = get_char !pos;
      !pos := !pos + 1;
      c
    }
  };
  let rec read_string len = do
  {
    if !pos + len > !size then
      read_string (!size - !pos)
    else do
    {
      let str = String.create len;

      for i = 0 to len - 1 do
      {
        str.[i] := read_char ()
      };

      str
    }
  };
  let write_char chr = do
  {
    if !pos >= !size then
      !size := !pos + 1
    else ();

    set_char !pos chr;

    !pos := !pos + 1
  };
  let write_string str = do
  {
    for i = 0 to String.length str - 1 do
    {
      write_char str.[i]
    }
  };

  io_make
    (fun ()  -> ())
    read_char
    read_string
    (fun ()  -> (!pos = !size))
    write_char
    write_string
    (fun ()  -> !size)
    (fun ()  -> !size)
    (fun ()  -> !pos)
    seek
};

value make_string_stream str = do
{
  let pos = ref 0;

  let seek new_pos = do
  {
    if new_pos < 0 then
      !pos := 0
    else if new_pos >= String.length str then
      !pos := String.length str
    else
      !pos := new_pos
  };
  let read_char () = do
  {
    if !pos >= String.length str then
      '\000'
    else do
    {
      let c = str.[!pos];
      !pos := !pos + 1;
      c
    }
  };
  let read_string len = do
  {
    if !pos + len > String.length str then
      String.sub str !pos (String.length str - !pos)
    else
      String.sub str !pos len
  };

  io_make_read_seek
    (fun ()  -> ())
    read_char
    read_string
    (fun ()  -> (!pos = String.length str))
    (fun ()  -> String.length str)
    (fun ()  -> !pos)
    seek
};

(* Conversions *)

value consume_stream is f = do
{
  while not (io_eof is) do
  {
    f (io_read_string is 0x1000)
  }
};

value produce_stream os f = do
{
  iter ()

  where rec iter () = match f () with
  [ ""  -> ()
  | str -> do
    {
      io_write_string os str;
      iter ()
    }
  ]
};

(* Append the contents of a channel to a stream. *)

value append_channel os ic = do
{
  let buffer = String.create 0x1000;

  let read () = do
  {
    let len = input ic buffer 0 0x1000;

    String.sub buffer 0 len
  };

  produce_stream os read
};

value append os is = do
{
  consume_stream is (io_write_string os)
};

(* Write the contents of a stream to a channel. *)

value to_channel is oc = do
{
  consume_stream is (output_string oc)
};

value from_string str = do
{
  let cs = make_buffer_stream (String.length str);

  write_string cs str;

  seek cs 0;

  cs
};

value to_string is = do
{
  let buf = Buffer.create 0x1000;

  seek is 0;
  consume_stream is (Buffer.add_string buf);

  Buffer.contents buf
};

value sub_stream cs len = do
{
  let new_cs = make_buffer_stream len;

  if len <= Sys.max_string_length then
    write_string new_cs (read_string cs len)
  else do
  {
    iter 0

    where rec iter i = do
    {
      if i + Sys.max_string_length <= len then do
      {
        write_string new_cs (read_string cs Sys.max_string_length);

        iter (i + Sys.max_string_length)
      }
      else
        write_string new_cs (read_string cs (len - i));
    }
  };

  seek new_cs 0;
  new_cs
};

value to_buffer is = do
{
  let os = make_buffer_stream 0x1000;

  append os is;

  os
};

(* compression *)

value compress cs level = do
{
  let buffer_size = min (io_size cs) 0x10000;
  let new_cs      = make_buffer_stream buffer_size;
  let pos         = pos cs;

  seek cs 0;

  let zs = Zlib.deflate_init buffer_size level;

  iter 0

  where rec iter i = do
  {
    let str = Zlib.get_output zs;

    if str <> "" then do
    {
      write_string new_cs str;
      iter i
    }
    else do
    {
      if not (eof cs) then do
      {
        if Zlib.avail_input zs = 0 then do
        {
          Zlib.set_input zs (read_string cs buffer_size);

          iter (i+1)
        }
        else do
        {
          Zlib.deflate zs Zlib.no_flush;

          iter i
        }
      }
      else do
      {
        Zlib.deflate zs Zlib.finish;

        let str = Zlib.get_output zs;

        if str <> "" then do
        {
          write_string new_cs str;
          iter i
        }
        else do
        {
          Zlib.deflate_end zs;
          seek new_cs 0;
          seek cs pos;
          io_coerce_ir new_cs
        }
      }
    }
  }
};

value uncompress cs = do
{
  let buffer_size = min (io_size cs) 0x10000;
  let new_cs      = make_buffer_stream buffer_size;
  let pos         = pos cs;

  seek cs 0;

  let zs = Zlib.inflate_init buffer_size;

  iter 0

  where rec iter i = do
  {
    let str = Zlib.get_output zs;

    if str <> "" then do
    {
      write_string new_cs str;
      iter i
    }
    else do
    {
      if not (eof cs) then do
      {
        if Zlib.avail_input zs = 0 then do
        {
          Zlib.set_input zs (read_string cs buffer_size);

          iter (i+1)
        }
        else do
        {
          Zlib.inflate zs Zlib.no_flush;

          iter i
        }
      }
      else do
      {
        Zlib.inflate zs Zlib.finish;

        let str = Zlib.get_output zs;

        if str <> "" then do
        {
          write_string new_cs str;
          iter i
        }
        else do
        {
          Zlib.inflate_end zs;
          seek new_cs 0;
          seek cs pos;
          io_coerce_ir new_cs
        }
      }
    }
  }
};

(*
(* Classes for character streams ******************************************************************)


class type consumer =
object
  method consume : !'a . ('a -> (string * 'a)) -> 'a -> 'a;
end;

class type producer =
object
  method produce : !'a . ('a -> string -> 'a) -> 'a -> 'a;
end;

class type random_access =
object
  method size : int;
  method pos  : int;
  method seek : int -> unit;
end;

class basic_stream =
object
  method free = ();
end;

class virtual istream =
object (self : #producer)
  inherit basic_stream;

  method virtual eof : bool;

  method read_char = do
  {
    (* if |read_char| is not defined we use |read_string| instead. *)

    if self#eof then
      '\000'
    else
      (self#read_string 1).[0];
  };

  method read_string len = do
  {
    (* if |read_string| is not defined we use |read_char| instead. *)

    let str = String.create len in

    iter 0

    where rec iter pos = do
    {
      if pos >= len then
        str
      else if self#eof then
        String.sub str 0 pos
      else do
      {
        str.[pos] := self#read_char;

        iter (pos + 1)
      }
    }
  };

  method produce f state = do
  {
    iter state

    where rec iter state = do
    {
      if self#eof then
        state
      else
        iter (f state (self#read_string 0x1000))
    }
  };
end;

class virtual ostream =
object (self : #consumer)
  inherit basic_stream;

  method virtual bytes_written : int;

  method write_char chr = do
  {
    (* if |write_char| is not defined we use |write_string| instead. *)

    self#write_string (String.make 1 chr)
  };

  method write_string str = do
  {
    (* if |write_string| is not defined we use |write_char| instead. *)

    for i = 0 to String.length str - 1 do
    {
      self#write_char str.[i]
    }
  };

  method consume f state = do
  {
    iter state

    where rec iter state = match f state with
    [ ("",  s) -> s
    | (str, s) -> do
      {
        self#write_string str;
        iter s
      }
    ]
  };
end;

class type virtual irstream =
object
  inherit istream;
  inherit random_access;
end;

class type virtual orstream =
object
  inherit ostream;
  inherit random_access;
end;

class type virtual istream_consumer =
object
  inherit istream;
  inherit consumer;
end;

class type virtual ostream_producer =
object
  inherit ostream;
  inherit producer;
end;

class type virtual iostream =
object
  inherit istream;
  inherit ostream;
end;

class type virtual iorstream =
object
  inherit iostream;
  inherit random_access;
end;


(* Implementations ********************************************************************************)

(* |power_of_two <x>| returns the greatest power of two less than or equal to <x>. *)

value power_of_two x = do
{
  iter 1 x

  where rec iter p x = do
  {
    if x = 0 then
      (p lsr 1)
    else
      iter (p lsl 1) (x lsr 1)
  }
};

class in_channel_stream name =
object
  inherit istream;

  value ic = open_in_bin name;

  value mutable eof = False;

  method eof = eof;

  method free = close_in ic;

  method read_char = do
  {
    try
      input_char ic
    with
    [ End_of_file -> do
      {
        eof := True;
        '\000'
      }
    ]
  };

  method read_string len = do
  {
    let buf = String.create len in

    iter 0 len

    where rec iter off len = do
    {
      let read = input ic buf off len in

      if read = len then
        buf
      else if read = 0 then do
      {
        eof := True;
        ""
      }
      else
        iter (off + read) (len - read)
    }
  };
end;

class rand_in_channel_stream name =
object
  inherit in_channel_stream name;

  method size     = in_channel_length ic;
  method pos      = pos_in ic;
  method seek off = seek_in ic off;
end;

class out_channel_stream name =
object
  inherit ostream;

  value oc = open_out_bin name;

  method bytes_written = pos_out oc;

  method free = close_out oc;

  method write_char   chr = output_char oc chr;
  method write_string str = output_string oc str;
end;

class rand_out_channel_stream name =
object
  inherit out_channel_stream name;

  method size     = out_channel_length oc;
  method pos      = pos_out oc;
  method seek off = seek_out oc off;
end;

class buffer_stream buf_size =
  let buffer_size        = power_of_two (min buf_size Sys.max_string_length) in
  let get_buffer_pos pos = pos land (buffer_size - 1) in
object (self : #iostream)
  inherit istream;
  method virtual free : unit;  (* avoid warning *)
  inherit ostream;

  value mutable buffer = [| String.create buffer_size |];
  value mutable size   = 0;
  value mutable pos    = 0;

  (* resizes the buffer array *)

  method private resize_buffer len = do
  {
    buffer := Array.init
                len
                (fun i ->
                  if i < Array.length buffer then
                    buffer.(i)
                  else
                    String.create buffer_size)
  };

  (* doubles the size of the buffer array *)

  method private double_buffer = do
  {
    self#resize_buffer (2 * Array.length buffer)
  };

  (* returns the entry in the buffer array that contains the given position *)

  method private get_buffer pos = do
  {
    if pos < buffer_size * Array.length buffer then
      buffer.(pos / buffer_size)
    else do
    {
      self#double_buffer;
      self#get_buffer pos
    }
  };

  (* returns the offset within a buffer of the given position *)

  method private get_char pos = (self#get_buffer pos).[get_buffer_pos pos];

  method private set_char pos chr = do
  {
    (self#get_buffer pos).[get_buffer_pos pos] := chr
  };

  method pos           = pos;
  method size          = size;
  method bytes_written = size;
  method eof           = (pos = size);

  method seek new_pos = do
  {
    if new_pos < 0 then
      pos := 0
    else if new_pos >= size then
      pos := size
    else
      pos := new_pos
  };

  method read_char = do
  {
    if pos >= size then
      '\000'
    else do
    {
      let c = self#get_char pos in
      pos := pos + 1;
      c
    }
  };

  method read_string len = do
  {
    if pos + len > size then
      self#read_string (size - pos)
    else do
    {
      let str = String.create len in

      for i = 0 to len - 1 do
      {
        str.[i] := self#read_char
      };

      str
    }
  };

  method write_char chr = do
  {
    if pos >= size then
      size := pos + 1
    else ();

    self#set_char pos chr;

    pos := pos + 1
  };

  method write_string str = do
  {
    for i = 0 to String.length str - 1 do
    {
      self#write_char str.[i]
    }
  };

  method produce f state = do
  {
    iter 0 state

    where rec iter i state = do
    {
      if (i + 1) * buffer_size >= size then do
      {
        f state (String.sub buffer.(i) 0 (size - i * buffer_size))
      }
      else do
      {
        iter (i+1) (f state buffer.(i))
      }
    }
  };
end;

class string_stream str =
object (self : #irstream)
  inherit istream;

  value mutable pos = 0;

  method pos  = pos;
  method size = String.length str;
  method eof  = (pos = String.length str);

  method seek new_pos = do
  {
    if new_pos < 0 then
      pos := 0
    else if new_pos >= String.length str then
      pos := String.length str
    else
      pos := new_pos
  };

  method read_char = do
  {
    if pos >= String.length str then
      '\000'
    else do
    {
      let c = str.[pos] in
      pos := pos + 1;
      c
    }
  };

  method read_string len = do
  {
    if pos + len > String.length str then
      String.sub str pos (String.length str - pos)
    else
      String.sub str pos len
  };

  method produce f state = do
  {
    f state str
  };
end;

value make_in_stream       filename    = (new in_channel_stream filename       :> istream);
value make_out_stream      filename    = (new out_channel_stream filename      :> ostream);
value make_rand_in_stream  filename    = (new rand_in_channel_stream filename  :> irstream);
value make_rand_out_stream filename    = (new rand_out_channel_stream filename :> orstream);
value make_buffer_stream   buffer_size = (new buffer_stream buffer_size        :> iorstream);
value make_string_stream   str         = (new string_stream str                :> irstream);


(* IO routines ************************************************************************************)


value size cs          = cs#size;
value pos  cs          = cs#pos;
value seek cs pos      = cs#seek pos;
value skip cs off      = ignore (cs#read_string off);
value bytes_written cs = cs#bytes_written;
value eof  cs          = cs#eof;
value free cs          = cs#free;

value read_char    cs     = cs#read_char;
value read_string  cs len = cs#read_string len;
value write_char   cs chr = cs#write_char chr;
value write_string cs str = cs#write_string str;

(* Append the contents of a channel to a stream. *)

value append_channel (cs : #consumer) ic = do
{
  let buffer = String.create 0x1000 in

  let read ic = do
  {
    let len = input ic buffer 0 0x1000 in

    (String.sub buffer 0 len, ic)
  }
  in

  ignore (cs#consume read ic)
};

value append (os : #ostream) (is : #producer) = do
{
  is#produce (fun () str -> write_string os str) ()
};

(* Write the contents of a stream to a channel. *)

value to_channel (cs : #producer) oc = do
{
  let write oc str = do
  {
    output_string oc str;
    oc
  }
  in

  ignore (cs#produce write oc)
};

value from_string str = do
{
  let cs = make_buffer_stream (String.length str) in

  write_string cs str;

  seek cs 0;

  cs
};

value to_string (cs : #producer) = do
{
  let buf = Buffer.create 0x1000 in

  let write buf str = do
  {
    Buffer.add_string buf str;
    buf
  }
  in

  ignore (cs#produce write buf);

  Buffer.contents buf
};

value sub_stream cs len = do
{
  let new_cs = make_buffer_stream len in

  if len <= Sys.max_string_length then
    write_string new_cs (read_string cs len)
  else do
  {
    iter 0

    where rec iter i = do
    {
      if i + Sys.max_string_length <= len then do
      {
        write_string new_cs (read_string cs Sys.max_string_length);

        iter (i + Sys.max_string_length)
      }
      else
        write_string new_cs (read_string cs (len - i));
    }
  };

  seek new_cs 0;
  new_cs
};

value to_buffer (cs : #producer) = do
{
  let copy new_cs str = do
  {
    write_string new_cs str;
    new_cs
  }
  in

  cs#produce copy (make_buffer_stream 0x1000)
};

(* reading from a stream *)

value read_byte cs = do
{
  if cs#eof then
    -1
  else
    int_of_char cs#read_char
};

value peek_char cs off = do
{
  let pos = cs#pos in

  cs#seek (pos + off);

  let chr = cs#read_char in

  cs#seek pos;

  chr
};

value rec peek_string cs off len = do
{
  let pos = cs#pos in

  cs#seek (pos + off);

  let str = cs#read_string len in

  cs#seek pos;

  str
};

value skip_while cs p = do
{
  while not cs#eof && p (peek_char cs 0) do
  {
    skip cs 1
  }
};

(* reading bigendian integers *)

value read_be_u8 cs = read_byte cs;

value read_be_u16 cs = do
{
  let x = read_byte cs in
  let y = read_byte cs in

  0x100 * x + y
};

value read_be_u24 cs = do
{
  let x = read_byte cs in
  let y = read_byte cs in
  let z = read_byte cs in

  0x10000 * x + 0x100 * y + z
};

value read_be_u32 cs = do
{
  let x = read_be_u16 cs in
  let y = read_be_u16 cs in

  num_of_int 0x10000 */ num_of_int x +/ num_of_int y
};

value read_be_i8 cs = do
{
  let x = read_be_u8 cs in

  if x > 0x7f then
    x - 0x100
  else
    x
};

value read_be_i16 cs = do
{
  let x = read_be_u16 cs in

  if x > 0x7fff then
    x - 0x10000
  else
    x
};

value read_be_i24 cs = do
{
  let x = read_be_u24 cs in

  if x > 0x7fffff then
    x - 0x1000000
  else
    x
};

value read_be_i32 cs = do
{
  let x = read_be_u32 cs in

  if x >=/ num_0x80000000 then
    x -/ num_0x100000000
  else
    x
};

(* writing to a stream *)

value write_byte cs x = cs#write_char (Char.unsafe_chr (x land 0xff));

value printf (cs : #ostream) = Printf.kprintf (write_string cs);

value write_be_u8 cs x = do
{
  write_byte cs x
};

value write_be_u16 cs x = do
{
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)
};

value write_be_u24 cs x = do
{
  write_byte cs ((x lsr 16) land 0xff);
  write_byte cs ((x lsr 8) land 0xff);
  write_byte cs (x land 0xff)
};

value write_be_u32 cs n = do
{
  let y = mod_num n (num_of_int 0x10000)        in
  let x = quo_num (n -/ y) (num_of_int 0x10000) in
  let u = int_of_num x                   in
  let v = int_of_num y                   in
  let b = (0x10000 + u) mod 0x100        in
  let a = (u - b) / 0x100                in
  let d = (0x10000 + v) mod 0x100        in
  let c = (v - d) / 0x100                in

  write_byte cs a;
  write_byte cs b;
  write_byte cs c;
  write_byte cs d
};

value write_be_i8 cs x = do
{
  if x < 0 then
    write_byte cs (0x100 + x)
  else
    write_byte cs x
};

value write_be_i16 cs x = do
{
  if x < 0 then
    write_be_u16 cs (0x10000 + x)
  else
    write_be_u16 cs x
};

value write_be_i24 cs x = do
{
  if x < 0 then
    write_be_u24 cs (0x1000000 + x)
  else
    write_be_u24 cs x
};

value write_be_i32 cs n = do
{
  let y = mod_num n (num_of_int 0x10000)        in
  let x = quo_num (n -/ y) (num_of_int 0x10000) in
  let u = int_of_num x                   in
  let v = int_of_num y                   in
  let b = (0x10000 + u) mod 0x100        in
  let a = (u - b) / 0x100                in
  let d = (0x10000 + v) mod 0x100        in
  let c = (v - d) / 0x100                in

  if a < 0 then
    write_byte cs (a + 0x100)
  else
    write_byte cs a;

  write_byte cs b;
  write_byte cs c;
  write_byte cs d
};

value read_utf8_char cs = do
{
  let c = read_byte cs in

  if c < 0x80 then
    c
  else if c < 0xc0 then
    c                            (* should never happen *)
  else if c < 0xe0 then do
  {
    let c2 = read_byte cs in

    if c2 < 0 then
      0x40 * (c - 0xc0)
    else
      0x40 * (c - 0xc0) + c2 - 0x80
  }
  else do
  {
    let c2 = read_byte cs in
    let c3 = read_byte cs in

    if c2 < 0 then
      0x1000 * (c - 0xe0)
    else if c3 < 0 then
      0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80)
    else
      0x1000 * (c - 0xe0) + 0x40 * (c2 - 0x80) + c3 - 0x80
  }
};

value write_utf8_char cs x = do
{
  if x < 0x80 then
    write_byte cs x
  else if x < 0x800 then do
  {
    write_byte cs (0xc0 + (x lsr 6));
    write_byte cs (0x80 + (x land 0x3f))
  }
  else do
  {
    write_byte cs (0xe0 + (x lsr 12));
    write_byte cs (0x80 + ((x lsr 6) land 0x3f));
    write_byte cs (0x80 + (x land 0x3f))
  }
};

(* compression *)

value compress cs level = do
{
  let buffer_size = min cs#size 0x10000            in
  let new_cs      = make_buffer_stream buffer_size in
  let pos         = pos cs                         in

  seek cs 0;

  let zs = Zlib.deflate_init buffer_size level in

  iter 0

  where rec iter i = do
  {
    let str = Zlib.get_output zs in

    if str <> "" then do
    {
      write_string new_cs str;
      iter i
    }
    else do
    {
      if not (eof cs) then do
      {
        if Zlib.avail_input zs = 0 then do
        {
          Zlib.set_input zs (read_string cs buffer_size);

          iter (i+1)
        }
        else do
        {
          Zlib.deflate zs Zlib.no_flush;

          iter i
        }
      }
      else do
      {
        Zlib.deflate zs Zlib.finish;

        let str = Zlib.get_output zs in

        if str <> "" then do
        {
          write_string new_cs str;
          iter i
        }
        else do
        {
          Zlib.deflate_end zs;
          seek new_cs 0;
          seek cs pos;
          (new_cs :> irstream)
        }
      }
    }
  }
};

value uncompress cs = do
{
  let buffer_size = min cs#size 0x10000            in
  let new_cs      = make_buffer_stream buffer_size in
  let pos         = pos cs                         in

  seek cs 0;

  let zs = Zlib.inflate_init buffer_size in

  iter 0

  where rec iter i = do
  {
    let str = Zlib.get_output zs in

    if str <> "" then do
    {
      write_string new_cs str;
      iter i
    }
    else do
    {
      if not (eof cs) then do
      {
        if Zlib.avail_input zs = 0 then do
        {
          Zlib.set_input zs (read_string cs buffer_size);

          iter (i+1)
        }
        else do
        {
          Zlib.inflate zs Zlib.no_flush;

          iter i
        }
      }
      else do
      {
        Zlib.inflate zs Zlib.finish;

        let str = Zlib.get_output zs in

        if str <> "" then do
        {
          write_string new_cs str;
          iter i
        }
        else do
        {
          Zlib.inflate_end zs;
          seek new_cs 0;
          seek cs pos;
          (new_cs :> irstream)
        }
      }
    }
  }
};
*)
