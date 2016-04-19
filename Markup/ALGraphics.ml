
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ParseState;
open ALCoding;
open ALEnvironment;
open ALDim;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* gfx-commands *)

value encode_line_cap cap = match cap with
[ Graphic.Butt   -> Types.Symbol sym_Butt
| Graphic.Circle -> Types.Symbol sym_Circle
| Graphic.Square -> Types.Symbol sym_Square
];

value encode_line_join join = match join with
[ Graphic.Miter -> Types.Symbol sym_Miter
| Graphic.Round -> Types.Symbol sym_Round
| Graphic.Bevel -> Types.Symbol sym_Bevel
];

value encode_gfx_cmd cmd = match cmd with
[ Graphic.PutBox x y b  -> Types.Tuple [|ref (Types.Symbol sym_PutBox);
                                         ref (encode_dim_arg x);
                                         ref (encode_dim_arg y);
                                         ref Types.Unbound|]  (* FIX *)
| Graphic.Draw pc p     -> do
  {
    let sym = match pc with
    [ Graphic.Stroke -> sym_Stroke
    | Graphic.Fill   -> sym_Fill
    | Graphic.Clip   -> sym_Clip
    ];

    Types.Tuple [|ref (Types.Symbol sym); ref (encode_path p)|]
  }
| Graphic.SetColour     c -> Types.Tuple [|ref (Types.Symbol sym_SetColour);
                                           ref (encode_colour c)|]
| Graphic.SetBgColour   c -> Types.Tuple [|ref (Types.Symbol sym_SetBgColour);
                                           ref (encode_colour c)|]
| Graphic.SetAlpha      a -> Types.Tuple [|ref (Types.Symbol sym_SetAlpha);
                                           ref (Types.Number a)|]
| Graphic.SetLineWidth  w -> Types.Tuple [|ref (Types.Symbol sym_SetLineWidth);
                                           ref (Types.Number w)|]
| Graphic.SetLineCap    c -> Types.Tuple [|ref (Types.Symbol sym_SetLineCap);
                                           ref (encode_line_cap c)|]
| Graphic.SetLineJoin   j -> Types.Tuple [|ref (Types.Symbol sym_SetLineJoin);
                                           ref (encode_line_join j)|]
| Graphic.SetMiterLimit l -> Types.Tuple [|ref (Types.Symbol sym_SetMiterLimit);
                                           ref (Types.Number l)|]
]
where rec encode_path path = match path with
[ [] -> Types.Nil
| [(ax,ay,bx,by,cx,cy,dx,dy) :: ps] ->
  Types.List
    (ref (Types.Tuple [|ref (encode_dim_arg ax); ref (encode_dim_arg ay);
                        ref (encode_dim_arg bx); ref (encode_dim_arg by);
                        ref (encode_dim_arg cx); ref (encode_dim_arg cy);
                        ref (encode_dim_arg dx); ref (encode_dim_arg dy)|]))
    (ref (encode_path ps))
];

value decode_line_cap name cap = do
{
  let s = decode_symbol name cap;

  if s = sym_Butt then
    Graphic.Butt
  else if s = sym_Circle then
    Graphic.Circle
  else if s = sym_Square then
    Graphic.Square
  else
    Types.runtime_error (name ^ ": invalid line cap")
};

value decode_line_join name join = do
{
  let s = decode_symbol name join;

  if s = sym_Miter then
    Graphic.Miter
  else if s = sym_Round then
    Graphic.Round
  else if s = sym_Bevel then
    Graphic.Bevel
  else
    Types.runtime_error (name ^ ": invalid line join")
};

value decode_gfx_cmd name cmd = do
{
  let arr = decode_tuple name cmd;

  if Array.length arr < 1 then
    Types.runtime_error (name ^ ": invalid argument")
  else do
  {
    let s = decode_symbol name arr.(0);

    if s = sym_PutBox then do
    {
      if Array.length arr <> 4 then
        Types.runtime_error (name ^ ": PutBox expects 3 arguments")
      else
        Graphic.PutBox (decode_dim_arg name arr.(1)) (decode_dim_arg name arr.(2))
          Box.empty_box (* FIX *)
    }
    else if s = sym_Stroke then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Stroke expects 1 argument")
      else
        Graphic.Draw Graphic.Stroke (decode_path name arr.(1))
    }
    else if s = sym_Fill then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Fill expects 1 argument")
      else
        Graphic.Draw Graphic.Fill (decode_path name arr.(1))
    }
    else if s = sym_Clip then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": Fill expects 1 argument")
      else
        Graphic.Draw Graphic.Clip (decode_path name arr.(1))
    }
    else if s = sym_SetColour then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetColour expects 1 argument")
      else
        Graphic.SetColour (decode_colour name arr.(1))
    }
    else if s = sym_SetBgColour then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetBgColour expects 1 argument")
      else
        Graphic.SetBgColour (decode_colour name arr.(1))
    }
    else if s = sym_SetAlpha then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetAlpha expects 1 argument")
      else
        Graphic.SetAlpha (Machine.decode_num name arr.(1))
    }
    else if s = sym_SetLineWidth then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineWidth expects 1 argument")
      else
        Graphic.SetLineWidth (Machine.decode_num name arr.(1))
    }
    else if s = sym_SetLineCap then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineCap expects 1 argument")
      else
        Graphic.SetLineCap (decode_line_cap name arr.(1))
    }
    else if s = sym_SetLineJoin then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetLineJoin expects 1 argument")
      else
        Graphic.SetLineJoin (decode_line_join name arr.(1))
    }
    else if s = sym_SetMiterLimit then do
    {
      if Array.length arr <> 2 then
        Types.runtime_error (name ^ ": SetMiterLimit expects 1 argument")
      else
        Graphic.SetMiterLimit (Machine.decode_num name arr.(1))
    }
    else
      Types.runtime_error (name ^ ": invalid graphics command")
  }
}
where rec decode_path name path = do
{
  List.map
    (fun x -> match decode_tuple name x with
      [ [|ax; ay; bx; by; cx; cy; dx; dy|] -> do
        {
          (decode_dim_arg name ax, decode_dim_arg name ay,
           decode_dim_arg name bx, decode_dim_arg name by,
           decode_dim_arg name cx, decode_dim_arg name cy,
           decode_dim_arg name dx, decode_dim_arg name dy)
        }
      | _ -> Types.runtime_error (name ^ ": invalid path segment")
      ])
    (Machine.decode_list name path)
};

