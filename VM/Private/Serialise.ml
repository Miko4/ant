
open XNum;
open Types;
open Unicode.Types;
open Unicode.SymbolTable;

(* serialise *)

type ser_data =
{
  stream    : IO.ostream;
  num_syms  : mutable int;
  anon_syms : mutable SymbolMap.t int
};

value empty_sym = string_to_symbol [| |];

value write_symbol data sym = do
{
  let str = symbol_to_string sym;
  let len = Array.length str;

  if len > 0 || sym = empty_sym then do
  {
    IO.printf data.stream "%d" len;

    Array.iter
      (fun c -> IO.write_be_u32 data.stream (num_of_int c))
      str
  }
  else do
  {
    (* Special treatment of anonymous symbols. *)
    try
      let idx = SymbolMap.find sym data.anon_syms;

      IO.printf data.stream "@%d" idx
    with
    [ Not_found -> do
      {
        data.num_syms  := data.num_syms + 1;
        data.anon_syms := SymbolMap.add data.num_syms sym data.anon_syms;

        IO.printf data.stream "@%d" data.num_syms
      }
    ]
  }
};

value serialise_unknown os x = do
{
  let data =
  {
    stream    = os;
    num_syms  = 0;
    anon_syms = SymbolMap.empty
  };

  serialise data x

  where rec serialise data x = match !x with
  [ Bool False   -> IO.write_string data.stream "b0"
  | Bool True    -> IO.write_string data.stream "b1"
  | Number n     -> do
    {
      IO.write_char data.stream 'n';
      serialise_num data.stream n
    }
  | Char c -> do
    {
      IO.printf data.stream "c%d" c
    }
  | Symbol s -> do
    {
      IO.write_char data.stream 's';
      write_symbol data s
    }
  | Nil      -> IO.write_char data.stream ']'
  | List a b -> do
    {
      IO.write_char data.stream '[';
      serialise data a;
      serialise data b
    }
  | Tuple xs -> do
    {
      IO.printf data.stream "(%d" (Array.length xs);
      Array.iter (serialise data) xs
    }
  | Dictionary d -> do
    {
      let size = SymbolMap.fold (fun _ _ n -> n + 1) d 0;

      IO.printf data.stream "{%d" size;

      SymbolMap.iter
        (fun k v -> do
         {
           write_symbol data k;
           serialise data v
         })
        d
    }
  | Unbound
  | Constraint _
  | LinForm _
  | Primitive1 _
  | Primitive2 _
  | PrimitiveN _ _
  | Function _ _ _
  | Chain _
  | Relation _ _
  | Application _ _ _
  | Opaque _ -> IO.write_char data.stream '?'
  ]
};


(* unserialise *)


type unser_data =
{
  stream    : IO.irstream;
  anon_syms : mutable array symbol
};

value read_integer is = do
{
  iter 0

  where rec iter n = do
  {
    let c = int_of_char (IO.peek_char is 0);

    if c >= 48 && c <= 57 then do
    {
      IO.skip is 1;
      iter (10 * n + c - 48)
    }
    else
      n
  }
};

value read_symbol data = do
{
  if IO.peek_char data.stream 0 <> '@' then do
  {
    let l   = read_integer data.stream;
    let str = Array.init l
                (fun _ -> int_of_num (IO.read_be_u32 data.stream));
    string_to_symbol str
  }
  else do
  {
    let lookup_anon_symbol sym_array idx = do
    {
      let sym = sym_array.(idx);

      if sym >= 0 then
        sym
      else do
      {
        let sym = alloc_symbol ();
        sym_array.(idx) := sym;
        sym
      }
    };

    IO.skip data.stream 1;
    let idx = read_integer data.stream;

    if idx < Array.length data.anon_syms then
      lookup_anon_symbol data.anon_syms idx
    else do
    {
      let old_len = Array.length data.anon_syms;
      let new_len = max (idx + 1) (2 * old_len);
      let arr     = Array.init new_len
                      (fun i -> if i < old_len then
                                  data.anon_syms.(i)
                                else
                                  -1);
      data.anon_syms := arr;
      lookup_anon_symbol arr idx
    }
  }
};

value unserialise_unknown is = do
{
  let data =
  {
    stream    = (is :> IO.irstream);
    anon_syms = Array.make 16 (-1)
  };

  unserialise data

  where rec unserialise data = match IO.read_char is with
  [ '?' -> Unbound
  | 'b' -> match IO.read_char is with
    [ '0' -> Bool False
    | '1' -> Bool True
    | _   -> runtime_error "Corrupt data."
    ]
  | 'c' -> Char   (read_integer is)
  | 's' -> Symbol (read_symbol data)
  | 'n' -> Number (unserialise_num data.stream)
  | ']' -> Nil
  | '[' -> do
    {
      let a = unserialise data;
      let b = unserialise data;
      List (ref a) (ref b)
    }
  | '(' -> do
    {
      let l = read_integer is;

      Tuple (Array.init l
               (fun _ -> ref (unserialise data)))
    }
  | '{' -> do
    {
      let l = read_integer is;

      iter l SymbolMap.empty

      where rec iter i d = do
      {
        if i <= 0 then
          Dictionary d
        else do
        {
          let k = (read_symbol data);
          let v = unserialise data;
          iter (i-1) (SymbolMap.add k (ref v) d)
        }
      }
    }
  | _ -> runtime_error "Corrupt data."
  ]
};

