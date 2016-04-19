
open XNum;
open Maps;
open Unicode;
open Substitute;
open GlyphMetric;
open FontMetric;
open FreeType;
open OpenType;

value ft_kerning face scale c1 c2 = do
{
  let (x,y) = ft_get_kerning face c1 c2 ft_kerning_unscaled;

  if x <> 0 then
    Kern (scale */ num_of_int x)
  else
    NoLigKern
};

value make_glyph_metric params extra_kern scale glyph_idx glyph = do
{
  let (width, height, h_off, v_off, adv) = glyph_metrics glyph;

  let left_bound  = max 0 (~-h_off);
  let right_bound = max 0 (h_off + width - adv);

  let l = scale */ num_of_int left_bound;
  let r = scale */ num_of_int right_bound;

  let user_kern_info = try
    let ki = List.assoc glyph_idx extra_kern;
    {
      ki_after_space    = params.flp_size */ ki.ki_after_space;
      ki_before_space   = params.flp_size */ ki.ki_before_space;
      ki_after_margin   = params.flp_size */ ki.ki_after_margin;
      ki_before_margin  = params.flp_size */ ki.ki_before_margin;
      ki_after_foreign  = params.flp_size */ ki.ki_after_foreign;
      ki_before_foreign = params.flp_size */ ki.ki_before_foreign
    }
  with [ Not_found -> zero_kern_info ];

  (* If these values are zero we do not need to allocate a new structure. *)
  let kern_info = if left_bound <> 0 || right_bound <> 0 then
    {
      (user_kern_info)
      with
      ki_after_foreign  = l +/ user_kern_info.ki_after_foreign;
      ki_before_foreign = r +/ user_kern_info.ki_before_foreign
    }
  else
    user_kern_info;

  {
    gm_width      = scale */ num_of_int adv +/ num_two */ params.flp_size */ params.flp_letter_spacing;
    gm_height     = scale */ num_of_int v_off;
    gm_depth      = scale */ num_of_int (height - v_off);
    gm_italic     = r;
    gm_extra      = GXI_Normal;
    gm_extra_kern = kern_info
  }
};

value get_glyph_metric params extra_kern scale face = do
{
  let (em, _asc, _desc, _height, _ul_pos, _ul_thick) =
    face_metrics face;
  let num_glyphs = face_num_glyphs face;
  let gm         = Array.make num_glyphs empty_glyph_metric;

  for i = 1 to num_glyphs - 1 do
  {
    ft_load_glyph face i (ft_load_no_hinting + ft_load_no_scale + ft_load_linear_design);

    gm.(i - 1) := make_glyph_metric params extra_kern scale i (face_glyph face)
  };

  gm
};

value get_glyph_bitmap face fm code = do
{
  let gm = fm.glyph_metric.(code - fm.first_glyph);

  ft_set_char_size face fm.at_size !default_bitmap_resolution;
  ft_load_glyph face code ft_load_monochrome;

  let (x, y, b) = glyph_to_bitmap (face_glyph face);
  let dpp       = num_of_ints 100 7227 */ num_of_int !default_bitmap_resolution;

  let g = GlyphBitmap.make
            code
            (int_of_num (round_num (gm.gm_width  */ dpp)))
            (int_of_num (round_num (gm.gm_height */ dpp)))
            (int_of_num (round_num (gm.gm_depth  */ dpp)))
            (float_of_num dpp)
            (float_of_num dpp)
            (x, y - b.Bitmap.bm_height + 1)
            (x + b.Bitmap.bm_width - 1, y);

  { (g) with GlyphBitmap.g_bitmap = b }
};

value builtin_encoding face char = match ft_get_char_index face char with
[ 0 -> Undef
| g -> Simple g
];

value builtin_decoding face glyph = do
{
  let lookup glyph = do
  {
    iter (ft_get_first_char face)

    where rec iter (c,g) = do
    {
      if g = 0 then
        []
      else if g = glyph then
        [c]
      else
        iter (ft_get_next_char face (c,g))
    }
  };

  Array.of_list (decode glyph)

  where rec decode glyph = match glyph with
  [ Undef              -> []
  | Border _           -> []
  | Simple g           -> lookup g
  | Accent a g         -> lookup a @ lookup g
  | Sequence gs        -> List.concat (List.map lookup gs)
  | Extendable t m b _ -> decode t @ decode m @ decode b
  ]
};

module Composer =
struct

(* table to memorise already computed composers *)

module SymbolTrie = SymbolSet.SymbolTrie;

value empty_table = [];

value rec add_composer table script lang features composer = match table with
[ []              -> [(script, lang, SymbolTrie.add features composer SymbolTrie.empty)]
| [(s,l,m) :: xs] -> if s = script && l = lang then
                       [(s,l, SymbolTrie.add features composer m) :: xs]
                     else
                       add_composer xs script lang features composer
];

value rec lookup_composer table script lang features = match table with
[ []              -> raise Not_found
| [(s,l,m) :: xs] -> if s = script && l = lang then
                       SymbolTrie.find features m
                     else
                       lookup_composer xs script lang features
];


(* translating lookups to adjustments *)

(* move the next glyph by |pos.p_x_off| and |pos.p_y_off| *)

value pos_to_pre_kern scale pos = do
{
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_x_off)
            (scale */ num_of_int pos.OTF_Pos_Subst.p_y_off)
};

(* increase the advance width of the preceding glyph by |pos.p_x_adv| *)

value pos_to_post_kern scale pos = do
{
  (* FIX: Should this be |h_adv_off - x_off| ? *)
  ConstKern (scale */ num_of_int pos.OTF_Pos_Subst.p_h_adv_off)
            num_zero
};

value position_to_adj scale p = do
{
  (single_positioning_cmd
    (scale */ num_of_int p.OTF_Pos_Subst.p_x_off)
    (scale */ num_of_int p.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int p.OTF_Pos_Subst.p_h_adv_off),
   0)
};

value kern_to_adj scale p1 p2 = do
{
  (pair_positioning_cmd
    (scale */ num_of_int p1.OTF_Pos_Subst.p_x_off)
    (scale */ num_of_int p1.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int (p1.OTF_Pos_Subst.p_h_adv_off + p2.OTF_Pos_Subst.p_x_off))
    (scale */ num_of_int p2.OTF_Pos_Subst.p_y_off)
    (scale */ num_of_int p2.OTF_Pos_Subst.p_h_adv_off),
   1)
};

value substitution_to_adj g = do
{
  (replace_with_single_glyph_cmd 1 (Simple g),
   0)
};

value multi_subst_to_adj glyphs = do
{
  (replace_with_multiple_glyphs_cmd 1 (Array.map (fun g -> Simple g) glyphs),
    0)
};

value ligature_to_adj n lig = do
{
  (replace_with_single_glyph_cmd n (Simple lig),
   0)
};

value pos_rule_to_adj scale glyphs rule = do
{
  let lookups = Array.make (Array.length glyphs) [||];

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) :=
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
      })
    rule;

  let adjs = ListBuilder.make ();

  for i = 0 to Array.length glyphs - 1 do
  {
    let cmds = lookups.(i);

    iter_cmds 0

    where rec iter_cmds k = do
    {
      if k >= Array.length cmds then do
      {
        ListBuilder.add_list adjs
          [ConstGlyph (Simple glyphs.(i));
           CopyCommands i i];
      }
      else match cmds.(k) with
      [ OTF_Pos_Subst.Position pos -> do
        {
          try do
          {
            let p = IntMap.find glyphs.(i) pos;

            ListBuilder.add_list adjs
              [pos_to_pre_kern scale p;
               ConstGlyph (Simple glyphs.(i));
               pos_to_post_kern scale p;
               CopyCommands i i];
          }
          with
          [ Not_found -> iter_cmds (k+1) ]
        }
      | _ -> iter_cmds (k+1)
      ]
    }
  };

  (ListBuilder.get adjs, 0)
};

value subst_rule_to_adj glyphs rule = do
{
  let lookups = Array.make (Array.length glyphs) [||];

  Array.iter
    (fun l -> do
      {
        lookups.(l.OTF_Pos_Subst.prr_seq_idx) :=
          l.OTF_Pos_Subst.prr_lookup.OTF_Pos_Subst.l_commands
      })
    rule;

  let adjs = ListBuilder.make ();

  for i = 0 to Array.length glyphs - 1 do
  {
    let cmds = lookups.(i);

    iter_cmds 0

    where rec iter_cmds k = do
    {
      if k >= Array.length cmds then do
      {
        ListBuilder.add_list adjs
          [ConstGlyph (Simple glyphs.(i));
           CopyCommands i i];
      }
      else match cmds.(k) with
      [ OTF_Pos_Subst.Substitution subst -> do
        {
          try do
          {
            let g2 = IntMap.find glyphs.(i) subst;

            ListBuilder.add_list adjs
              [ConstGlyph (Simple g2);
               CopyCommands i i];
          }
          with
          [ Not_found -> iter_cmds (k+1) ]
        }
      | _ -> iter_cmds (k+1)
      ]
    }
  };

  (ListBuilder.get adjs, 0)
};

value pos_subst_to_adjsutment scale cmd = match cmd with
[ OTF_Pos_Subst.NoCommand    -> NoAdjustment
| OTF_Pos_Subst.Position pos -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g p trie -> DynUCTrie.add_list [g] (position_to_adj scale p) trie)
        pos
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.CursiveAnchors entry exit -> NoAdjustment (* FIX *)
| OTF_Pos_Subst.MarkToBaseAnchors _ _
| OTF_Pos_Subst.MarkToLigAnchors  _ _
| OTF_Pos_Subst.MarkToMarkAnchors _ _ -> NoAdjustment
| OTF_Pos_Subst.Kern kerns -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g1 m trie ->
          IntMap.fold
            (fun g2 (p1,p2) trie ->
              DynUCTrie.add_list
                [g1; g2]
                (kern_to_adj scale p1 p2)
                trie)
            m
            trie)
        kerns
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.KernClass n classes1 classes2 pos1 pos2 -> do
  {
    ClassPairLookup n classes1 classes2
      (Array.init (Array.length pos1)
        (fun i -> kern_to_adj scale pos1.(i) pos2.(i)))
  }
| OTF_Pos_Subst.Substitution subst -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g s trie -> DynUCTrie.add_list [g] (substitution_to_adj s) trie)
        subst
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.Multiple map -> do
  {
    DirectLookup
      (IntMap.fold
        (fun g s trie -> DynUCTrie.add_list [g] (multi_subst_to_adj s) trie)
        map
        DynUCTrie.empty)
  }
| OTF_Pos_Subst.Alternate map -> NoAdjustment (* FIX *)
| OTF_Pos_Subst.Ligature ligs -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs s -> ligature_to_adj (Array.length gs) s)
        ligs)
  }
| OTF_Pos_Subst.ContextGlyphPos rules -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs r -> pos_rule_to_adj scale gs r)
        rules)
  }
| OTF_Pos_Subst.ContextGlyphSubst rules -> do
  {
    DirectLookup
      (DynUCTrie.mapi
        (fun gs r -> subst_rule_to_adj gs r)
        rules)
  }
(*
| OTF_Pos_Subst.ContextClassPos      of IntMap.t int and array (pos_subst_rule (array int))
| OTF_Pos_Subst.ContextClassSubst    of IntMap.t int and array (pos_subst_rule (array int))
| OTF_Pos_Subst.ContextCoveragePos   of array (pos_subst_rule (array (array int)))
| OTF_Pos_Subst.ContextCoverageSubst of array (pos_subst_rule (array (array int)))
| OTF_Pos_Subst.ChainGlyphPos        of (array (pos_subst_rule (array int * array int * array int)))
| OTF_Pos_Subst.ChainGlyphSubst      of (array (pos_subst_rule (array int * array int * array int)))
| OTF_Pos_Subst.ChainClassPos        of IntMap.t int and IntMap.t int and IntMap.t int and
                                        array (pos_subst_rule (array int * array int * array int))
| OTF_Pos_Subst.ChainClassSubst      of IntMap.t int and IntMap.t int and IntMap.t int and
                                        array (pos_subst_rule (array int * array int * array int))
| OTF_Pos_Subst.ChainCoveragePos     of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| OTF_Pos_Subst.ChainCoverageSubst   of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| OTF_Pos_Subst.ReverseSubst         of array int and array int and array (array int) and array (array int)
*)
| _ -> NoAdjustment
];

value lookup_to_adjustment scale lookups = do
{
  Array.map
    (fun l -> Array.map
                (pos_subst_to_adjsutment scale)
                l.OTF_Pos_Subst.l_commands)
    lookups
};

(* table mapping a lang-sys to its adjustments *)

type lang_sys =
{
  ls_required    : array int;         (* index to at_adjustment_tables *)
  ls_tags        : array Tag.tag;
  ls_adjustments : array (array int)  (* index to at_adjustment_tables *)
};

type adj_table =
{
  at_scripts           : Tag.TagMap.t (Tag.TagMap.t lang_sys);
  at_adjustment_tables : array (array adjustment_table);
  at_user_adjustments  : array adjustment_table
};

value make_adjustment_table ps_table scale user_adjustments = do
{
  let conv_script s = do
  {
    Tag.TagMap.map
      (fun ls -> do
        {
          let r = match ls.OTF_Pos_Subst.ls_required with
            [ None   -> [||]
            | Some f -> f.OTF_Pos_Subst.f_lookups
            ];
          let t = Array.map (fun f -> f.OTF_Pos_Subst.f_tag)     ls.OTF_Pos_Subst.ls_features;
          let l = Array.map (fun f -> f.OTF_Pos_Subst.f_lookups) ls.OTF_Pos_Subst.ls_features;

          {
            ls_required    = r;
            ls_tags        = t;
            ls_adjustments = l
          }
        })
      s
  };

  {
    at_scripts           = Tag.TagMap.map conv_script ps_table.OTF_Pos_Subst.t_scripts;
    at_adjustment_tables = lookup_to_adjustment scale ps_table.OTF_Pos_Subst.t_lookups;
    at_user_adjustments  = Array.of_list user_adjustments
  }
};

value make_simple_adjustment_table face scale user_adjustments = do
{
  let kerns_to_adjustments face scale = do
  {
    let last = face_num_glyphs face;

    iter 1 1 DynUCTrie.empty

    where rec iter g1 g2 adj = do
    {
      if g1 > last then
        adj
      else if g2 > last then
        iter (g1 + 1) 1 adj
      else do
      {
        let (k,_) = ft_get_kerning face g1 g2 ft_kerning_unscaled;

        let new_adj =
          if k <> 0 then
            DynUCTrie.add_string
              [|g1; g2|]
              (Substitute.simple_pair_kerning_cmd (scale */ num_of_int k),
               1)
              adj
          else
            adj;

        iter g1 (g2 + 1) new_adj
      }
    }
  };

  let n   = List.length user_adjustments;
  let adj = Array.make (n+1) NoAdjustment;

  List.fold_left
    (fun i a -> do { adj.(i) := a; (i+1) })
    0
    user_adjustments;

  adj.(n) := DirectLookup (kerns_to_adjustments face scale);

  {
    at_scripts           = Tag.TagMap.empty;
    at_adjustment_tables = [||];
    at_user_adjustments  = adj
  }
};

value get_adjustments adj_table script lang_sys features = do
{
  let mark_feature_adjs used adjs = do
  {
    Array.iter
      (fun i -> used.(i) := True)
      adjs
  };
  let mark_lang_sys_adjs used lang_sys features = do
  {
    mark_feature_adjs used lang_sys.ls_required;

    for i = 0 to Array.length lang_sys.ls_tags - 1 do
    {
      if Tag.TagSet.mem lang_sys.ls_tags.(i) features then
        mark_feature_adjs used lang_sys.ls_adjustments.(i)
      else ()
    }
  };

  let num_adjs = Array.length adj_table.at_adjustment_tables;
  let marks    = Array.make num_adjs False;

  let ls = try do
  {
    let s = Tag.TagMap.find script adj_table.at_scripts;

    try
      Tag.TagMap.find lang_sys s
    with [ Not_found -> Tag.TagMap.find Tag.dflt_tag s ]
  }
  with [ Not_found -> { ls_required = [||]; ls_tags = [||]; ls_adjustments = [||] } ];

  mark_lang_sys_adjs marks ls features;

  let (_, active) =
    Array.fold_right
      (fun l (i, lu) -> do
        {
          if marks.(i) then
            (i-1, [l :: lu])
          else
            (i-1, lu)
        })
      adj_table.at_adjustment_tables
      (num_adjs - 1, []);

  match adj_table.at_user_adjustments with
  [ [||] -> active
  | _    -> [adj_table.at_user_adjustments :: active]
  ]
};

value make_matcher memo_table scale get_border_glyph adj_table script features = do
{
  let s = UString.uc_string_to_ascii script;

  let (s_tag, l_tag) =
    if Array.length script = 9 && s.[4] = '.' then
      (Tag.make_tag (String.sub s 0 4),
       Tag.make_tag (String.sub s 5 4))
    else if Array.length script = 4 then
      (Tag.make_tag s, Tag.dflt_tag)
    else
      (Tag.latn_tag, Tag.dflt_tag);

  let trie =
    try
      lookup_composer !memo_table s_tag l_tag features
    with
    [ Not_found -> do
      {
        let f_tags = SymbolSet.fold
                       (fun set f ->
                         Tag.TagSet.add
                           (Tag.make_tag_uc (SymbolTable.symbol_to_string f))
                           set)
                       Tag.TagSet.empty
                       features;
        let adj = get_adjustments adj_table s_tag l_tag f_tags;

        let max_depth = max2_adjustment_depth adj;

        let is_empty (n, _)        = (n > max_depth);
        let prefix (n, glyphs) g   = (n+1, [g :: glyphs]);
        let root_value (_, glyphs) =
          lookup2_adjustments adj (List.rev glyphs);

        let trie = (is_empty, prefix, root_value);

        !memo_table := add_composer !memo_table s_tag l_tag features trie;

        trie
      }
    ];

  match_substitution_trie get_border_glyph trie (0, [])
};

value make_simple_matcher face scale get_border_glyph extra_adjustments = do
{
  let max_depth = max2_adjustment_depth extra_adjustments;

  let is_empty (n, _)        = (n > max_depth);
  let prefix (n, glyphs) g   = (n+1, [g :: glyphs]);
  let root_value (n, glyphs) = do
  {
    match lookup2_adjustments extra_adjustments (List.rev glyphs) with
    [ (Some _) as cmds -> cmds
    | None             -> do
      {
        match glyphs with
        [ [g2; g1] -> match ft_kerning face scale g1 g2 with
          [ NoLigKern          -> None
          | Kern k             -> Some (simple_pair_kerning_cmd k, 1)
          | Ligature l s k1 k2 -> Some (tex_ligature_cmd (Simple l) k1 k2, s)
          ]
        | _ -> None
        ]
      }
    ]
  };

  match_substitution_trie get_border_glyph (is_empty, prefix, root_value) (0, [])
};

value get_composer face scale get_border_glyph (pos, subst) p_table s_table fm scr feat = do
{
  if Array.length pos.at_adjustment_tables = 0 && Array.length pos.at_user_adjustments = 0 then
    simple_composer fm (make_matcher s_table scale get_border_glyph subst scr feat)
  else
    if Array.length subst.at_adjustment_tables = 0 && Array.length subst.at_user_adjustments = 0 then
      simple_composer fm (make_matcher p_table scale get_border_glyph pos scr feat)
    else
      two_phase_composer fm (make_matcher s_table scale get_border_glyph subst scr feat)
                            (make_matcher p_table scale get_border_glyph pos   scr feat)
};

end;

value read_ft file name params = do
{
  let face = ft_new_face file;

  if ft_is_postscript face then do
  {
    (* look for an afm file *)

    if String.length file >= 4 then
      ignore (ft_attach_file face (String.sub file 0 (String.length file - 4) ^ ".afm"))
    else ();

    ft_attach_file face (file ^ ".afm");
    ()
  }
  else ();

  let last_glyph = face_num_glyphs face - 1;

  let (em, asc, desc, _height, _ul_pos, _ul_thick) =
    face_metrics face;

  let (enc,dec) = match params.flp_encoding with
  [ [| |] -> (builtin_encoding face,
              builtin_decoding face)
  | m     -> (Encodings.charmap_encoding (Encodings.fake_encoding m),
              Encodings.array_decoding m)
  ];
  let lookup_char x = match enc x with
  [ Simple g -> g
  | _        -> (-1)
  ];
  let lookup_name x = do
  {
    if ft_has_ps_glyph_names face then do
    {
      iter 1

      where rec iter i = do
      {
        if i > face_num_glyphs face then
          (-1)
        else if ft_get_glyph_name face i = x then
          i - 1
        else
          iter (i+1)
      }
    }
    else
      (-1)
  };

  let size         = params.flp_size;
  let scale        = size // num_of_int em;
  let design_size  = scale */ num_of_int (asc - desc);

  let extra_kern =
    List.map
      (fun (g,k) ->
        (glyph_spec_to_index lookup_char lookup_name g, k))
      params.flp_extra_kern;

  let glyph_metric = get_glyph_metric params extra_kern scale face;

  let space_glyph  = ft_get_char_index face 32;
  let x_glyph      = ft_get_char_index face 102;
  let m_glyph      = ft_get_char_index face 77;
  let space        = if space_glyph > 0 then                               (* width of " "  *)
                       glyph_metric.(space_glyph - 1).gm_width
                     else
                       size // num_of_int 3;
  let x_height     = if x_glyph > 0 then                                   (* height of "x" *)
                       glyph_metric.(x_glyph - 1).gm_width
                     else
                       size // num_of_int 2;
  let quad         = if m_glyph > 0 then                                   (* width of "M"  *)
                       glyph_metric.(m_glyph - 1).gm_width
                     else
                       size;
  let hyphen_glyph = match params.flp_hyphen_glyph with
                     [ Undef -> builtin_encoding face 45
                     | h     -> h
                     ];

  let get_border_glyph b = match b with
  [ Margin  -> Simple (last_glyph + 1)
  | Space   -> Simple (last_glyph + 2)
  | Foreign -> Simple (last_glyph + 3)
  ];

  let s_table = ref Composer.empty_table;
  let p_table = ref Composer.empty_table;

  let (font_type, adj_table) = do
  {
    let (tables, pos, subst) = try do
      {
        let tables = read_font_tables file;

        match get_pos_subst tables with
        [ (Some p, Some s) -> (tables, p, s)
        | (Some p, None)   -> (tables, p, OTF_Pos_Subst.empty_pos_subst)
        | (None,   Some s) -> (tables, OTF_Pos_Subst.empty_pos_subst, s)
        | (None,   None)   -> (tables, OTF_Pos_Subst.empty_pos_subst,
                                       OTF_Pos_Subst.empty_pos_subst)
        ]
      }
      with
      [ _ -> (Tag.TagMap.empty,
              OTF_Pos_Subst.empty_pos_subst,
              OTF_Pos_Subst.empty_pos_subst) ];

    let font_type =
      if ft_is_sfnt face then
        if is_cff tables then
          OpenTypeCFF
        else
          TrueType
      else if ft_is_postscript face then
        PostScript
      else
        Other;

    let extra_pos =
      if GlyphSpecTrie.is_empty params.flp_extra_pos then
        []
      else
        [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_pos ];
    let extra_subst =
      if GlyphSpecTrie.is_empty params.flp_extra_subst then
        []
      else
        [ adjustment_spec_to_table lookup_char lookup_name params.flp_extra_subst ];
    let user_pos =
      add_border_kern
        (last_glyph + 1) (last_glyph + 2) (last_glyph + 3)
        params.flp_size
        extra_kern
        extra_pos;
    let pos_adj =
      if Array.length pos.OTF_Pos_Subst.t_lookups = 0 then
        Composer.make_simple_adjustment_table face scale user_pos
      else
        Composer.make_adjustment_table pos scale user_pos;
    let subst_adj = Composer.make_adjustment_table subst scale extra_subst;

    (font_type, (pos_adj, subst_adj))
  };

  let composer fm s f =
    Composer.get_composer
      face scale
      get_border_glyph
      adj_table p_table s_table
      fm s f;

  {
    name                = name;
    ps_name             = ft_get_postscript_name face;
    file_name           = file;
    font_type           = font_type;
    first_glyph         = 1;
    last_glyph          = last_glyph;
    design_size         = design_size;
    at_size             = size;
    check_sum           = num_zero;
    get_glyph           = enc;
    get_unicode         = dec;
    get_composer        = composer;
    kerning             = fun _ c1 c2 -> ft_kerning face scale c1 c2;
    draw_simple_glyph   = if params.flp_letter_spacing =/ num_zero then
                             draw_simple_glyph
                           else
                             draw_displaced_simple_glyph
                               (scale */ num_of_int em */ params.flp_letter_spacing)
                               num_zero;
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_glyph_bitmap    = get_glyph_bitmap face;
    get_glyph_name      = ft_get_glyph_name face;
    parameter           =
      {
        hyphen_glyph     = hyphen_glyph;
        skew_glyph       = params.flp_skew_glyph;
        margin_glyph     = Simple (last_glyph + 1);
        space_glyph      = Simple (last_glyph + 2);
        foreign_glyph    = Simple (last_glyph + 3);
        slant            = num_zero;
        space            = space;
        space_stretch    = space // num_of_int 2;
        space_shrink     = space // num_of_int 3;
        x_height         = x_height;
        quad             = quad;
        extra_space      = num_zero;
        num_shift_1      = num_zero;
        num_shift_2      = num_zero;
        num_shift_3      = num_zero;
        denom_shift_1    = num_zero;
        denom_shift_2    = num_zero;
        super_shift_1    = num_zero;
        super_shift_2    = num_zero;
        super_shift_3    = num_zero;
        sub_shift_1      = num_zero;
        sub_shift_2      = num_zero;
        super_drop       = num_zero;
        sub_drop         = num_zero;
        delim_1          = num_zero;
        delim_2          = num_zero;
        axis_height      = num_zero;
        rule_thickness   = num_zero;
        big_op_spacing_1 = num_zero;
        big_op_spacing_2 = num_zero;
        big_op_spacing_3 = num_zero;
        big_op_spacing_4 = num_zero;
        big_op_spacing_5 = num_zero
      };
    glyph_metric = glyph_metric
  }
};

