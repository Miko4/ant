
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
open ALGraphics;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* boxes *)

value encode_box_cmd cmd = Types.Unbound; (* FIX *)

value decode_box_cmd name cmd = do
{
  `GfxCmd (Graphic.SetColour (Graphic.Grey num_zero)) (* FIX *)
};

(* area contents *)

value encode_area_contents c = Types.Unbound; (* FIX *)

value decode_area_contents name c = do
{
  `Galley ([||],
           Evaluate.const_pt num_zero, Evaluate.const_pt num_zero,
           Evaluate.const_pt num_zero, Evaluate.const_pt num_zero)
};

(* glue functions *)

value encode_glue_function c = Types.Unbound; (* FIX *)

value decode_glue_function name f = do
{
  (fun _ b -> b)
};

value encode_image_format f = match f with
[ LoadImage.Bitmap     -> Types.Symbol sym_Bitmap
| LoadImage.Bmp        -> Types.Symbol sym_Bmp
| LoadImage.PostScript -> Types.Symbol sym_PostScript
| LoadImage.PDF        -> Types.Symbol sym_PDF
];

value decode_image_format name f = do
{
  let s = decode_symbol name f;

  if s = sym_Bitmap then
    LoadImage.Bitmap
  else if s = sym_Bmp then
    LoadImage.Bmp
  else if s = sym_PostScript then
    LoadImage.PostScript
  else if s = sym_PDF then
    LoadImage.PDF
  else
    Types.runtime_error (name ^ ": invalid image format")
};

(* nodes *)

value make_node0 sym loc =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc)|];
value make_node1 sym loc x =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc); ref x|];
value make_node2 sym loc x y =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc); ref x; ref y|];
value make_node3 sym loc x y z =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc); ref x; ref y; ref z|];
value make_node4 sym loc x0 x1 x2 x3 =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc);
                ref x0; ref x1; ref x2; ref x3|];
value make_node5 sym loc x0 x1 x2 x3 x4 =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc);
                ref x0; ref x1; ref x2; ref x3; ref x4|];
value make_node6 sym loc x0 x1 x2 x3 x4 x5 =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc);
                ref x0; ref x1; ref x2; ref x3; ref x4; ref x5|];
value make_node7 sym loc x0 x1 x2 x3 x4 x5 x6 =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc);
                ref x0; ref x1; ref x2; ref x3; ref x4; ref x5; ref x6|];
value make_node8 sym loc x0 x1 x2 x3 x4 x5 x6 x7 =
  Types.Tuple [|ref (Types.Symbol sym); ref (encode_location loc);
                ref x0; ref x1; ref x2; ref x3; ref x4; ref x5; ref x6; ref x7|];

value rec encode_node node = match node with
[ Node.Nodes ns                    -> encode_node_list ns
| Node.Command loc cmd             -> make_node1 sym_Command loc
                                        (encode_env_cmd "<unnamed>" cmd)
| Node.CommandBox loc c            -> make_node1 sym_CommandBox loc
                                        (encode_box_cmd c)
| Node.GfxCommand loc c            -> make_node1 sym_GfxCommand loc
                                        (encode_gfx_cmd c)
| Node.NewGalley loc n m           -> make_node2 sym_NewGalley loc
                                        (Machine.uc_string_to_char_list n)
                                        (encode_skip_arg m)
| Node.NewLayout loc n w h         -> make_node3 sym_NewLayout loc
                                        (Machine.uc_string_to_char_list n)
                                        (encode_skip_arg w)
                                        (encode_skip_arg h)
| Node.NewArea loc n x y w h t b c -> make_node8 sym_NewArea loc
                                        (Machine.uc_string_to_char_list n)
                                        (encode_skip_arg x)
                                        (encode_skip_arg y)
                                        (encode_skip_arg w)
                                        (encode_skip_arg h)
                                        (encode_skip_arg t)
                                        (encode_skip_arg b)
                                        (encode_area_contents c)
| Node.ShipOut loc e o n           -> make_node3 sym_ShipOut loc
                                        (Machine.uc_string_to_char_list e)
                                        (Machine.uc_string_to_char_list o)
                                        (Types.Number (num_of_int n))
| Node.AddToGalley loc g n         -> make_node2 sym_AddToGalley loc
                                        (Machine.uc_string_to_char_list g)
                                        (encode_node_list n)
| Node.PutGalleyInVBox loc a n     -> make_node2 sym_PutGalleyInVBox loc
                                        (Types.Bool a)
                                        (Machine.uc_string_to_char_list n)
| Node.ModifyGalleyGlue loc f      -> make_node1 sym_ModifyGalleyGlue loc
                                        (encode_glue_function f)
| Node.Paragraph loc b             -> make_node1 sym_Paragraph loc (encode_node_list b)
| Node.BeginGroup loc              -> make_node0 sym_BeginGroup loc
| Node.EndGroup loc                -> make_node0 sym_EndGroup loc
| Node.Float loc n b               -> make_node2 sym_Float loc
                                        (Machine.uc_string_to_char_list n)
                                        (encode_node_list b)
| Node.Glyph loc g                 -> make_node1 sym_Glyph loc (Types.Number (num_of_int g))
| Node.Letter loc char             -> make_node1 sym_Letter loc (Types.Char char)
| Node.Space loc                   -> make_node0 sym_Space loc
| Node.Glue loc w h i d            -> make_node4 sym_Glue loc
                                        (encode_dim_arg w)
                                        (encode_dim_arg h)
                                        (Types.Bool i)
                                        (Types.Bool d)
| Node.Break loc p h pre post no   -> make_node5 sym_Break loc
                                        (match p with
                                         [ Some x -> Types.Number x
                                         | None   -> Types.Symbol sym_None
                                         ])
                                        (Types.Bool h)
                                        (encode_node_list pre)
                                        (encode_node_list post)
                                        (encode_node_list no)
| Node.Rule loc w h d              -> make_node3 sym_Rule loc
                                        (encode_dim_arg w)
                                        (encode_dim_arg h)
                                        (encode_dim_arg d)
| Node.Image loc f fmt w h         -> make_node4 sym_Image loc
                                        (Machine.uc_list_to_char_list (UString.of_string f))
                                        (encode_image_format fmt)
                                        (encode_skip_arg w)
                                        (encode_skip_arg h)
| Node.Accent loc a c              -> make_node2 sym_Accent loc
                                        (Types.Char a)
                                        (encode_node_list c)
| Node.HBox loc d b                -> make_node2 sym_HBox loc
                                        (encode_hbox_dir d)
                                        (encode_node_list b)
| Node.HBoxTo loc d w b            -> make_node3 sym_HBoxTo loc
                                        (encode_hbox_dir d)
                                        (encode_skip_arg w)
                                        (encode_node_list b)
| Node.HBoxSpread loc d a b        -> make_node3 sym_HBoxSpread loc
                                        (encode_hbox_dir d)
                                        (encode_skip_arg a)
                                        (encode_node_list b)
| Node.VBox loc b                  -> make_node1 sym_VBox loc
                                        (encode_node_list b)
| Node.VBoxTo loc h b              -> make_node2 sym_VBoxTo loc
                                        (encode_skip_arg h)
                                        (encode_node_list b)
| Node.VBoxSpread loc a b          -> make_node2 sym_VBoxSpread loc
                                        (encode_skip_arg a)
                                        (encode_node_list b)
| Node.Phantom loc h v n           -> make_node3 sym_Phantom loc
                                        (Types.Bool h)
                                        (Types.Bool v)
                                        (encode_node_list n)
| Node.HLeaders loc w n            -> make_node2 sym_HLeaders loc
                                        (encode_dim_arg w)
                                        (encode_node_list n)
| Node.VInsert loc b ns            -> make_node2 sym_VInsert loc
                                        (Types.Bool b)
                                        (encode_node_list ns)
| Node.Table loc n                 -> make_node1 sym_Table loc
                                        (encode_node_list n)
| Node.TableEntry loc l r t bl b c -> make_node6 sym_TableEntry loc
                                        (Types.Number (num_of_int l))
                                        (Types.Number (num_of_int r))
                                        (Types.Number (num_of_int t))
                                        (Types.Number (num_of_int bl))
                                        (Types.Number (num_of_int b))
                                        (encode_node_list c)
| Node.Math loc n                  -> make_node1 sym_Math loc
                                        (encode_node_list n)
| Node.MathCode loc c n            -> make_node2 sym_MathCode loc
                                        (encode_math_code c)
                                        (encode_node_list n)
| Node.MathChar loc (cd, (f1,f2), (c1,c2)) ->
    make_node1 sym_MathChar loc
      (Types.Tuple
        [|ref (encode_math_code cd);
          ref (Types.Number (num_of_int f1));
          ref (Types.Char c1);
          ref (Types.Number (num_of_int f2));
          ref (Types.Char c2)|])
| Node.SubScript loc n             -> make_node1 sym_SubScript   loc (encode_node_list n)
| Node.SuperScript loc n           -> make_node1 sym_SuperScript loc (encode_node_list n)
| Node.Fraction loc n d l r t      -> make_node5 sym_Fraction loc
                                        (encode_node_list n)
                                        (encode_node_list d)
                                        (encode_node l)
                                        (encode_node r)
                                        (encode_skip_arg t)
| Node.Underline loc n             -> make_node1 sym_Underline loc (encode_node_list n)
| Node.Overline loc n              -> make_node1 sym_Overline  loc (encode_node_list n)
| Node.MathAccent loc f c n        -> make_node3 sym_MathAccent loc
                                        (Types.Number (num_of_int f))
                                        (Types.Char c)
                                        (encode_node_list n)
| Node.Root loc sf sc lf lc n      -> make_node5 sym_Root loc
                                        (Types.Number (num_of_int sf))
                                        (Types.Char sc)
                                        (Types.Number (num_of_int lf))
                                        (Types.Char lc)
                                    (encode_node_list n)
| Node.LeftRight loc ns            -> make_node1 sym_LeftRight     loc (encode_node_list_list ns)
| Node.MathStyle loc s             -> make_node1 sym_MathStyle     loc (encode_math_style s)
| Node.IndexPosition loc p         -> make_node1 sym_IndexPosition loc (encode_index_position p)
]
and encode_node_list nodes = match nodes with
[ []      -> Types.Nil
| [n::ns] -> Types.List (ref (encode_node n)) (ref (encode_node_list ns))
]
and encode_node_list_list nodes = match nodes with
[ []      -> Types.Nil
| [n::ns] -> Types.List (ref (encode_node_list n)) (ref (encode_node_list_list ns))
];

value decode_tuple0 name xs = match xs with
[ [|_; loc|] -> decode_location name loc
| _          -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple1 name xs = match xs with
[ [|_; loc; x|] -> (decode_location name loc, x)
| _             -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple2 name xs = match xs with
[ [|_; loc; x; y|] -> (decode_location name loc, x, y)
| _                -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple3 name xs = match xs with
[ [|_; loc; x; y; z|] -> (decode_location name loc, x, y, z)
| _                   -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple4 name xs = match xs with
[ [|_; loc; x0; x1; x2; x3|] ->
  (decode_location name loc, x0, x1, x2, x3)
| _ -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple5 name xs = match xs with
[ [|_; loc; x0; x1; x2; x3; x4|] ->
  (decode_location name loc, x0, x1, x2, x3, x4)
| _ -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple6 name xs = match xs with
[ [|_; loc; x0; x1; x2; x3; x4; x5|] ->
  (decode_location name loc, x0, x1, x2, x3, x4, x5)
| _ -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple7 name xs = match xs with
[ [|_; loc; x0; x1; x2; x3; x4; x5; x6|] ->
  (decode_location name loc, x0, x1, x2, x3, x4, x5, x6)
| _ -> Types.runtime_error (name ^ ": invalid node")
];

value decode_tuple8 name xs = match xs with
[ [|_; loc; x0; x1; x2; x3; x4; x5; x6; x7|] ->
  (decode_location name loc, x0, x1, x2, x3, x4, x5, x6, x7)
| _ -> Types.runtime_error (name ^ ": invalid node")
];

value rec decode_node name node = match !node with
[ Types.Nil      -> Node.Nodes []
| Types.List _ _ -> Node.Nodes (decode_node_list name node)
| Types.Tuple xs -> do
  {
    if Array.length xs = 0 then
      Types.runtime_error (name ^ ": invalid node")
    else match !(xs.(0)) with
    [ Types.Symbol s -> do
      {
        if s = sym_Command then
          let (loc, c) = decode_tuple1 name xs in
          Node.Command loc (decode_env_cmd name c)
        else if s = sym_CommandBox then
          let (loc, c) = decode_tuple1 name xs in
          Node.CommandBox loc (decode_box_cmd name c)
        else if s = sym_NewGalley then
          let (loc, n, m) = decode_tuple2 name xs in
          Node.NewGalley loc
            (decode_uc_string name n)
            (decode_skip_arg name m)
        else if s = sym_NewLayout then
          let (loc, n, w, h) = decode_tuple3 name xs in
          Node.NewLayout loc
            (decode_uc_string name n)
            (decode_skip_arg name w)
            (decode_skip_arg name h)
        else if s = sym_NewArea then
          let (loc, n, x, y, w, h, t, b, c) = decode_tuple8 name xs in
          Node.NewArea loc
            (decode_uc_string name n)
            (decode_skip_arg name x)
            (decode_skip_arg name y)
            (decode_skip_arg name w)
            (decode_skip_arg name h)
            (decode_skip_arg name t)
            (decode_skip_arg name b)
            (decode_area_contents name c)
        else if s = sym_ShipOut then
          let (loc, e, o, n) = decode_tuple3 name xs in
          Node.ShipOut loc
            (decode_uc_string name e)
            (decode_uc_string name o)
            (decode_int name n)
        else if s = sym_AddToGalley then
          let (loc, g, ns) = decode_tuple2 name xs in
          Node.AddToGalley loc
            (decode_uc_string name g)
            (decode_node_list name ns)
        else if s = sym_PutGalleyInVBox then
          let (loc, a, n) = decode_tuple2 name xs in
          Node.PutGalleyInVBox loc
            (decode_bool name a)
            (decode_uc_string name n)
        else if s = sym_ModifyGalleyGlue then
          let (loc, f) = decode_tuple1 name xs in
          Node.ModifyGalleyGlue loc
            (decode_glue_function name f)
        else if s = sym_Paragraph then
          let (loc, b) = decode_tuple1 name xs in
          Node.Paragraph loc
            (decode_node_list name b)
        else if s = sym_BeginGroup then
          let loc = decode_tuple0 name xs in
          Node.BeginGroup loc
        else if s = sym_EndGroup then
          let loc = decode_tuple0 name xs in
          Node.EndGroup loc
        else if s = sym_Glyph then
          let (loc, g) = decode_tuple1 name xs in
          Node.Glyph loc (decode_int name g)
        else if s = sym_Letter then
          let (loc, c) = decode_tuple1 name xs in
          Node.Letter loc (decode_char name c)
        else if s = sym_Space then
          let loc = decode_tuple0 name xs in
          Node.Space loc
        else if s = sym_Glue then
          let (loc, w, h, i, d) = decode_tuple4 name xs in
          Node.Glue loc
            (decode_dim_arg name w)
            (decode_dim_arg name h)
            (decode_bool name i)
            (decode_bool name d)
        else if s = sym_Break then do
        {
          let (loc, p, h, pre, post, no) = decode_tuple5 name xs;

          Node.Break loc
            (decode_option name Machine.decode_num p)
            (decode_bool name h)
            (decode_node_list name pre)
            (decode_node_list name post)
            (decode_node_list name no)
        }
        else if s = sym_Rule then
          let (loc, w, h, d) = decode_tuple3 name xs in
          Node.Rule loc
            (decode_dim_arg name w)
            (decode_dim_arg name h)
            (decode_dim_arg name d)
        else if s = sym_Image then
          let (loc, f, fmt, w, h) = decode_tuple4 name xs in
          Node.Image loc
            (UString.to_string (Machine.decode_string name f))
            (decode_image_format name fmt)
            (decode_skip_arg name w)
            (decode_skip_arg name h)
        else if s = sym_Accent then
          let (loc, c, n) = decode_tuple2 name xs in
          Node.Accent loc
            (decode_char name c)
            (decode_node_list name n)
        else if s = sym_HBox then
          let (loc, d, n) = decode_tuple2 name xs in
          Node.HBox loc
            (decode_hbox_dir name d)
            (decode_node_list name n)
        else if s = sym_HBoxTo then
          let (loc, d, w, n) = decode_tuple3 name xs in
          Node.HBoxTo loc
            (decode_hbox_dir name d)
            (decode_skip_arg name w)
            (decode_node_list name n)
        else if s = sym_HBoxSpread then
          let (loc, d, a, n) = decode_tuple3 name xs in
          Node.HBoxSpread loc
            (decode_hbox_dir name d)
            (decode_skip_arg name a)
            (decode_node_list name n)
        else if s = sym_VBox then
          let (loc, n) = decode_tuple1 name xs in
          Node.VBox loc
            (decode_node_list name n)
        else if s = sym_VBoxTo then
          let (loc, h, n) = decode_tuple2 name xs in
          Node.VBoxTo loc
            (decode_skip_arg name h)
            (decode_node_list name n)
        else if s = sym_VBoxSpread then
          let (loc, a, n) = decode_tuple2 name xs in
          Node.VBoxSpread loc
            (decode_skip_arg name a)
            (decode_node_list name n)
        else if s = sym_Phantom then
          let (loc, w, h, n) = decode_tuple3 name xs in
          Node.Phantom loc
            (decode_bool name w)
            (decode_bool name h)
            (decode_node_list name n)
        else if s = sym_HLeaders then
          let (loc, w, n) = decode_tuple2 name xs in
          Node.HLeaders loc
            (decode_dim_arg name w)
            (decode_node_list name n)
        else if s = sym_VInsert then
          let (loc, a, n) = decode_tuple2 name xs in
          Node.VInsert loc
            (decode_bool name a)
            (decode_node_list name n)
        else if s = sym_Table then
          let (loc, n) = decode_tuple1 name xs in
          Node.Table loc
            (decode_node_list name n)
        else if s = sym_TableEntry then
          let (loc, l, r, t, bl, b, c) = decode_tuple6 name xs in
          Node.TableEntry loc
            (decode_int name l)
            (decode_int name r)
            (decode_int name t)
            (decode_int name bl)
            (decode_int name b)
            (decode_node_list name c)
        else if s = sym_Math then
          let (loc, n) = decode_tuple1 name xs in
          Node.Math loc
            (decode_node_list name n)
        else if s = sym_MathCode then
          let (loc, c, b) = decode_tuple2 name xs in
          Node.MathCode loc
            (decode_math_code name c)
            (decode_node_list name b)
        else if s = sym_MathChar then do
        {
          let (loc, char) = decode_tuple1 name xs;

          match !char with
          [ Types.Tuple [|c; f1; c1; f2; c2|] ->
              Node.MathChar loc
                (decode_math_code name c,
                (decode_int name f1, decode_int name f2),
                (decode_int name c1, decode_int name c2))
          | _ -> Types.runtime_error (name ^ ": invalid argument")
          ]
        }
        else if s = sym_SubScript then
          let (loc, n) = decode_tuple1 name xs in
          Node.SubScript loc
            (decode_node_list name n)
        else if s = sym_SuperScript then
          let (loc, n) = decode_tuple1 name xs in
          Node.SuperScript loc
            (decode_node_list name n)
        else if s = sym_Fraction then
          let (loc, n, d, l, r, t) = decode_tuple5 name xs in
          Node.Fraction loc
            (decode_node_list name n)
            (decode_node_list name d)
            (decode_node name l)
            (decode_node name r)
            (decode_skip_arg name t)
        else if s = sym_Underline then
          let (loc, n) = decode_tuple1 name xs in
          Node.Underline loc
            (decode_node_list name n)
        else if s = sym_Overline then
          let (loc, n) = decode_tuple1 name xs in
          Node.Overline loc
            (decode_node_list name n)
        else if s = sym_MathAccent then
          let (loc, f, c, n) = decode_tuple3 name xs in
          Node.MathAccent loc
            (decode_int name f)
            (decode_char name c)
            (decode_node_list name n)
        else if s = sym_Root then
          let (loc, f1, c1, f2, c2, n) = decode_tuple5 name xs in
          Node.Root loc
            (decode_int  name f1)
            (decode_char name c1)
            (decode_int  name f2)
            (decode_char name c2)
            (decode_node_list name n)
        else if s = sym_LeftRight then
          let (loc, ns) = decode_tuple1 name xs in
          Node.LeftRight loc
            (decode_node_list_list name ns)
        else if s = sym_MathStyle then
          let (loc, s) = decode_tuple1 name xs in
          Node.MathStyle loc
            (decode_math_style name s)
        else if s = sym_IndexPosition then
          let (loc, p) = decode_tuple1 name xs in
          Node.IndexPosition loc
            (decode_index_position name p)
        else
          Types.runtime_error (name ^ ": invalid node")
      }
    | _ -> Types.runtime_error (name ^ ": invalid node")
    ]
  }
| _ -> Types.runtime_error (name ^ ": invalid node")
]
and decode_node_list name nodes = do
{
  List.map (decode_node name) (Machine.decode_list name nodes)
}
and decode_node_list_list name nodes = do
{
  List.map (decode_node_list name) (Machine.decode_list name nodes)
};

