
open XNum;
open Substitute;
open GlyphMetric;
open FontMetric;

module LigKern =
struct

type lig_kern_cmd =
[ LigCmd  of int and int and int and int
| KernCmd of int and int and num
];

(* access methods *)

value is_lig lk = match lk with
[ LigCmd _ _ _ _ -> True
| KernCmd _ _ _  -> False
];

value is_kern lk = match lk with
[ LigCmd _ _ _ _ -> False
| KernCmd _ _ _  -> True
];

value skip lk = match lk with
[ LigCmd s _ _ _ -> s
| KernCmd s _ _  -> s
];

value next lk = match lk with
[ LigCmd _ n _ _ -> n
| KernCmd _ n _  -> n
];

value operand lk = match lk with
[ LigCmd _ _ o _ -> o
| KernCmd _ _ _  -> raise (Invalid_argument "LigKern.operand applied to kern!")
];

value remainder lk = match lk with
[ LigCmd _ _ _ r -> r
| KernCmd _ _ _  -> raise (Invalid_argument "LigKern.remainder applied to kern!")
];

value kern lk = match lk with
[ LigCmd _ _ _ _ -> raise (Invalid_argument "LigKern.kern applied to ligature!")
| KernCmd _ _ k  -> k
];

(* |next_lig_kern| can be used to enumerate all lig-kern pairs. *)

value next_lig_kern lk_array pos = do
{
  let lk = lk_array.(pos);
  let s  = skip lk;
  let n  = next lk;

  let next_pos = if s < 128 then
    pos + s + 1
  else
    -1;

  if s <= 128 then do
  {
    if is_lig lk then do
    {
      let op = operand lk;

      (next_pos, n,
       GlyphMetric.Ligature (remainder lk) (op lsr 2) ((op lsr 1) land 1 = 1) (op land 1 = 1))
    }
    else
      (next_pos, n, GlyphMetric.Kern (kern lk))
  }
  else
    (-1, -1, NoLigKern)
};

(*
  Determines the ligature or kerning of two characterpos. lk_array is the array of LigKern commands,
  pos the position corresponding to the current character, and next_char the next character.
*)

value rec get_lig_kern lk_array pos next_char = do
{
  let lk = lk_array.(pos);
  let s  = skip lk;
  let n  = next lk;

  if (n = next_char) && (s <= 128) then
    if is_lig lk then do
    {
      let op = operand lk;

      GlyphMetric.Ligature (remainder lk) (op lsr 2) ((op lsr 1) land 1 = 1) (op land 1 = 1)
    }
    else
      GlyphMetric.Kern (kern lk)
  else if s < 128 then
    get_lig_kern lk_array (pos + s + 1) next_char
  else
    NoLigKern
};

value rec list_lig_kerns lk_array pos = do
{
  let lk = lk_array.(pos);
  let s  = skip lk;
  let n  = next lk;

  if s > 128 then
    []
  else if is_lig lk then do
  {
    let op = operand lk;

    let l = (n, GlyphMetric.Ligature (remainder lk) (op lsr 2) ((op lsr 1) land 1 = 1) (op land 1 = 1));

    if s < 128 then
      [l :: list_lig_kerns lk_array (pos + s + 1)]
    else
      [l]
  }
  else do
  {
    let k = (n, GlyphMetric.Kern (kern lk));

    if s < 128 then
      [k :: list_lig_kerns lk_array (pos + s + 1)]
    else
      [k]
  }
};

end;

value num_0x100     = num_of_int 0x100;
value num_0x10000   = num_of_int 0x10000;
value num_0x100000  = num_of_int 0x100000;
value num_0x1000000 = num_of_int 0x1000000;

value read_fix ic = IO.read_be_i32 ic // num_0x100000;

value read_4 ic = do
{
  let x1 = IO.read_be_u8 ic;
  let x2 = IO.read_be_u8 ic;
  let x3 = IO.read_be_u8 ic;
  let x4 = IO.read_be_u8 ic;

  (x1, x2, x3, x4)
};

value read_array ic read_fun len = do
{
  if len <= 0 then
    [| |]
  else do
  {
    let a = Array.make len (read_fun ic);

    for i = 1 to len - 1 do
    {
      a.(i) := read_fun ic
    };

    a
  }
};

value tfm_kerning lig_kern font c1 c2 = do
{
  match (get_glyph_metric font (Simple c1)).gm_extra with
  [ GXI_LigKern lk -> LigKern.get_lig_kern lig_kern lk c2
  | _              -> NoLigKern
  ];
};

value get_adjustment_tables lig_kern_table glyphs first_glyph last_glyph = do
{
  iter first_glyph DynUCTrie.empty DynUCTrie.empty

  where rec iter g pos subst = do
  {
    if g > last_glyph then
      (if DynUCTrie.is_empty pos   then [] else [Substitute.DirectLookup pos],
       if DynUCTrie.is_empty subst then [] else [Substitute.DirectLookup subst])
    else match glyphs.(g - first_glyph).gm_extra with
    [ GXI_LigKern lk -> do
      {
        let lks = LigKern.list_lig_kerns lig_kern_table lk;

        add_lig_kern lks pos subst

        where rec add_lig_kern lks pos subst = match lks with
        [ []       -> iter (g+1) pos subst
        | [l::lks] -> match l with
            [ (g2, GlyphMetric.Ligature c s k1 k2) -> do
              {
                add_lig_kern
                  lks
                  pos
                  (DynUCTrie.add_list
                    [g; g2]
                    (tex_ligature_cmd (Simple c) k1 k2, s)
                    subst)
              }
            | (g2, GlyphMetric.Kern x) -> do
              {
                add_lig_kern
                  lks
                  (DynUCTrie.add_list
                    [g; g2]
                    (simple_pair_kerning_cmd x, 1)
                    pos)
                  subst
              }
            | (_, NoLigKern) -> assert False
            ]
        ]
      }
    | _ -> iter (g+1) pos subst
    ]
  }
};

value get_glyph_bitmap bitmaps fm code = do
{
  if !bitmaps = None then do
  {
    match FontPK.read_pk_font fm !default_bitmap_resolution with
    [ None         -> ()
    | Some (_, gs) -> !bitmaps := Some gs
    ]
  }
  else ();

  match !bitmaps with
  [ None    -> GlyphBitmap.empty_glyph
  | Some bm -> bm.(code - fm.first_glyph)
  ]
};

value make_lig_kern kern (x1,x2,x3,x4) = do
{
  if x3 < 128 then
    LigKern.LigCmd x1 x2 x3 x4
  else
    LigKern.KernCmd x1 x2 kern.(0x100 * (x3 - 128) + x4)
};

value make_glyph_metric glyph_idx letter_spacing extra_kern size width height depth italic lig exten (w,x1,x2,r) = do
{
  let h  = x1 lsr 4;
  let d  = x1 land 0xf;
  let i  = x2 lsr 2;
  let t  = x2 land 0x3;

  let user_kern_info = try
    let ki = List.assoc glyph_idx extra_kern;
    {
      ki_after_space    = size */ ki.ki_after_space;
      ki_before_space   = size */ ki.ki_before_space;
      ki_after_margin   = size */ ki.ki_after_margin;
      ki_before_margin  = size */ ki.ki_before_margin;
      ki_after_foreign  = size */ ki.ki_after_foreign;
      ki_before_foreign = size */ ki.ki_before_foreign
    }
  with [ Not_found -> zero_kern_info ];

  (* If italic.(i) = 0 then we do not need to allocate a new structure. *)
  let kern_info = if italic.(i) <>/ num_zero then
    {
      (user_kern_info)
      with
      ki_before_foreign = italic.(i) +/ user_kern_info.ki_before_foreign
    }
  else
    user_kern_info;

  let extra = match t with
  [ 0 -> GXI_Normal
  | 1 -> let lk = lig.(r) in
         GXI_LigKern
           (if LigKern.is_lig lk && LigKern.skip lk > 128 then
              256 * LigKern.operand lk + LigKern.remainder lk
            else
              r)
  | 2 -> GXI_List r
  | _ -> let (t,m,b,r) = exten.(r) in
         GXI_Extendable t m b r
  ];

  {
    gm_width      = width.(w) +/ num_two */ size */ letter_spacing;
    gm_height     = height.(h);
    gm_depth      = depth.(d);
    gm_italic     = italic.(i);
    gm_extra      = extra;
    gm_extra_kern = kern_info
  }
};

value tfm_composer pos subst fm _ _ = do
{
  let (p_trie, p_state) = make_adjustment_trie pos;
  let (s_trie, s_state) = make_adjustment_trie subst;

  two_phase_composer fm (match_substitution_trie (get_border_glyph fm) s_trie s_state)
                        (match_substitution_trie (get_border_glyph fm) p_trie p_state)
};

value read_tfm file name params = do
{
  let ic = IO.make_in_stream file;

  let _file_length  = IO.read_be_u16 ic;
  let header_length = IO.read_be_u16 ic;
  let first_glyph   = IO.read_be_u16 ic;
  let last_glyph    = IO.read_be_u16 ic;

  let glyph_metric_table_len = last_glyph - first_glyph + 1;

  let width_table_len  = IO.read_be_u16 ic;
  let height_table_len = IO.read_be_u16 ic;
  let depth_table_len  = IO.read_be_u16 ic;
  let italic_table_len = IO.read_be_u16 ic;
  let lig_table_len    = IO.read_be_u16 ic;
  let kern_table_len   = IO.read_be_u16 ic;
  let ext_table_len    = IO.read_be_u16 ic;
  let param_table_len  = IO.read_be_u16 ic;

  let check_sum   = IO.read_be_u32 ic;
  let design_size = read_fix ic;

  let size = if params.flp_size >=/ num_zero then
               params.flp_size
             else
               design_size;

  IO.skip ic (4 * header_length - 8);

  let glyph_metric = read_array ic read_4   glyph_metric_table_len;
  let width     = Array.map (fun x -> x */ size) (read_array ic read_fix width_table_len);
  let height    = Array.map (fun x -> x */ size) (read_array ic read_fix height_table_len);
  let depth     = Array.map (fun x -> x */ size) (read_array ic read_fix depth_table_len);
  let italic    = Array.map (fun x -> x */ size) (read_array ic read_fix italic_table_len);
  let lig       = read_array ic read_4   lig_table_len;
  let kern      = Array.map (fun x -> x */ size) (read_array ic read_fix kern_table_len);
  let ext       = read_array ic read_4   ext_table_len;
  let param     = read_array ic read_fix param_table_len;

  let lig_cmds  = Array.map (fun x -> make_lig_kern kern x) lig;

  let (enc,dec) = match params.flp_encoding with
  [ [| |] -> (Encodings.raw_encoding,    Encodings.raw_decoding)
  | m     -> (Encodings.charmap_encoding (Encodings.fake_encoding m),
              Encodings.array_decoding m)
  ];

  let lookup_char x = match enc x with
  [ Simple g -> g
  | _        -> (-1)
  ];
  let lookup_name x = (-1); (* FIX *)

  let extra_kern =
    List.map
      (fun (g,k) ->
        (glyph_spec_to_index lookup_char lookup_name g, k))
      params.flp_extra_kern;

  let gm_table  = Array.mapi
                    (fun i gm -> make_glyph_metric (i + first_glyph)
                                   params.flp_letter_spacing extra_kern
                                   size width height depth italic
                                   lig_cmds ext gm)
                    glyph_metric;
  let (p,s) = get_adjustment_tables lig_cmds gm_table first_glyph last_glyph;
  let extra_pos =
    if GlyphSpecTrie.is_empty params.flp_extra_pos then
      p
    else
      [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_pos :: p];
  let pos_table =
    add_border_kern
      (last_glyph + 1) (last_glyph + 2) (last_glyph + 3)
      size
      extra_kern
      extra_pos;
  let subst_table =
    if GlyphSpecTrie.is_empty params.flp_extra_subst then
      s
    else
      [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_subst :: s];

  let hyphen_glyph = match params.flp_hyphen_glyph with
  [ Undef -> Simple 45
  | h     -> h
  ];

  let composer x y = tfm_composer pos_table subst_table x y;

  {
    name                = name;
    ps_name             = name;
    file_name           = file;
    font_type           = Other;
    first_glyph         = first_glyph;
    last_glyph          = last_glyph;
    glyph_metric        = gm_table;
    design_size         = design_size;
    at_size             = size;
    check_sum           = check_sum;
    get_glyph           = enc;
    get_unicode         = dec;
    draw_simple_glyph   = if params.flp_letter_spacing =/ num_zero then
                             draw_simple_glyph
                           else
                             draw_displaced_simple_glyph
                               (size */ params.flp_letter_spacing)
                               num_zero;
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_composer        = composer;
    kerning             = tfm_kerning lig_cmds;
    get_glyph_bitmap    = (get_glyph_bitmap (ref None));
    get_glyph_name      = (fun g -> Printf.sprintf "c%d" g);
    parameter           =
      {
        hyphen_glyph     = hyphen_glyph;
        skew_glyph       = params.flp_skew_glyph;
        margin_glyph     = Simple (last_glyph + 1);
        space_glyph      = Simple (last_glyph + 2);
        foreign_glyph    = Simple (last_glyph + 3);
        slant            = if param_table_len >  0 then param.( 0) else num_zero;
        space            = if param_table_len >  1 then size */ param.( 1) else num_zero;
        space_stretch    = if param_table_len >  2 then size */ param.( 2) else num_zero;
        space_shrink     = if param_table_len >  3 then size */ param.( 3) else num_zero;
        x_height         = if param_table_len >  4 then size */ param.( 4) else num_zero;
        quad             = if param_table_len >  5 then size */ param.( 5) else design_size;
        extra_space      = if param_table_len >  6 then size */ param.( 6) else num_zero;
        num_shift_1      = if param_table_len >  7 then size */ param.( 7) else num_zero;
        num_shift_2      = if param_table_len >  8 then size */ param.( 8) else num_zero;
        num_shift_3      = if param_table_len >  9 then size */ param.( 9) else num_zero;
        denom_shift_1    = if param_table_len > 10 then size */ param.(10) else num_zero;
        denom_shift_2    = if param_table_len > 11 then size */ param.(11) else num_zero;
        super_shift_1    = if param_table_len > 12 then size */ param.(12) else num_zero;
        super_shift_2    = if param_table_len > 13 then size */ param.(13) else num_zero;
        super_shift_3    = if param_table_len > 14 then size */ param.(14) else num_zero;
        sub_shift_1      = if param_table_len > 15 then size */ param.(15) else num_zero;
        sub_shift_2      = if param_table_len > 16 then size */ param.(16) else num_zero;
        super_drop       = if param_table_len > 17 then size */ param.(17) else num_zero;
        sub_drop         = if param_table_len > 18 then size */ param.(18) else num_zero;
        delim_1          = if param_table_len > 19 then size */ param.(19) else num_zero;
        delim_2          = if param_table_len > 20 then size */ param.(20) else num_zero;
        axis_height      = if param_table_len > 21 then size */ param.(21) else num_zero;
        rule_thickness   = if param_table_len >  7 then size */ param.( 7) else num_zero;
        big_op_spacing_1 = if param_table_len >  8 then size */ param.( 8) else num_zero;
        big_op_spacing_2 = if param_table_len >  9 then size */ param.( 9) else num_zero;
        big_op_spacing_3 = if param_table_len > 10 then size */ param.(10) else num_zero;
        big_op_spacing_4 = if param_table_len > 11 then size */ param.(11) else num_zero;
        big_op_spacing_5 = if param_table_len > 12 then size */ param.(12) else num_zero
      }
  }
};

