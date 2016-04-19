
open XNum;
open Unicode.Types;
open Dim;
open Graphic;
open Substitute;
open GlyphMetric;

(* font metrics *)

type font_parameter =
{
  hyphen_glyph     : glyph_desc;
  skew_glyph       : glyph_desc;
  margin_glyph     : glyph_desc;
  space_glyph      : glyph_desc;
  foreign_glyph    : glyph_desc;
  slant            : num;
  space            : num;
  space_stretch    : num;
  space_shrink     : num;
  x_height         : num;
  quad             : num;
  extra_space      : num;
  num_shift_1      : num;
  num_shift_2      : num;
  num_shift_3      : num;
  denom_shift_1    : num;
  denom_shift_2    : num;
  super_shift_1    : num;
  super_shift_2    : num;
  super_shift_3    : num;
  sub_shift_1      : num;
  sub_shift_2      : num;
  super_drop       : num;
  sub_drop         : num;
  delim_1          : num;
  delim_2          : num;
  axis_height      : num;
  rule_thickness   : num;
  big_op_spacing_1 : num;
  big_op_spacing_2 : num;
  big_op_spacing_3 : num;
  big_op_spacing_4 : num;
  big_op_spacing_5 : num
};

type font_type =
[ PostScript
| OpenTypeCFF
| TrueType
| Other
];

type font_metric =
{
  name                : string;
  ps_name             : string;
  file_name           : string;
  font_type           : font_type;
  first_glyph         : int;
  last_glyph          : int;
  design_size         : num;
  at_size             : num;
  check_sum           : num;
  parameter           : font_parameter;
  get_glyph           : uc_char -> glyph_desc;
  get_unicode         : glyph_desc -> uc_string;
  get_composer        : !'box 'cmd . get_composer_type 'box 'cmd;
  kerning             : font_metric -> int -> int -> lig_kern;
  draw_simple_glyph   : font_metric -> int -> simple_box;
  accent_base_point   : font_metric -> glyph_metric -> (num * num);
  accent_attach_point : font_metric -> glyph_metric -> (num * num);
  get_glyph_bitmap    : font_metric -> uc_char -> GlyphBitmap.glyph;
  get_glyph_name      : int -> string;
  glyph_metric        : array glyph_metric
}

and get_composer_type 'box 'cmd = font_metric -> uc_string -> SymbolSet.t -> glyph_composer font_metric 'box 'cmd

and simple_box =
[ Empty
| SimpleGlyph of int and font_metric
| Rule of num and num
| Image of num and num and string and LoadImage.format
| Group of list (graphic_command num simple_box)
| Command of simple_cmd
]

and simple_cmd =
[= `DVI_Special of string
];

type glyph_spec =
[ GlyphIndex of int
| GlyphChar of uc_char
| GlyphName of string
];

type adjustment_spec =
[ AdjKern of num
| AdjLig  of glyph_spec
];

module GlyphSpecTrie = DynamicTrie.Make(struct type t = glyph_spec; value compare = compare; end);

type font_load_params =
{
  flp_size           : num;                                (* scale font to this size     *)
  flp_encoding       : array uc_string;                    (* overrides built in encoding *)
  flp_hyphen_glyph   : glyph_desc;                         (* specifies the hyphen glyph  *)  (* FIX: replace these two by *)
  flp_skew_glyph     : glyph_desc;                         (* specifies the skew glyph    *)  (* a complete font_parameter *)
  flp_letter_spacing : num;                                (* additional letter spacing   *)
  flp_extra_pos      : GlyphSpecTrie.t adjustment_spec;    (* additional kerning pairs and ligatures *)
  flp_extra_subst    : GlyphSpecTrie.t adjustment_spec;    (* additional kerning pairs and ligatures *)
  flp_extra_kern     : list (glyph_spec * extra_kern_info) (* kerning with border glyphs  *)
};

(* pages *)

type page =
{
  p_contents : simple_box;
  p_number   : int;
  p_width    : num;
  p_height   : num
};

(* resolution used to load bitmap fonts *)

value default_bitmap_resolution = ref 1200;
value default_mf_mode           = ref "ljfzzz";

(* |get_glyph <font> <char>| returns the glyph corresponding to the unicode <char> and
   |get_unicode <font> <glyph>| computes the unicode of the given glyph.
*)

value get_glyph   font char  = font.get_glyph   char;
value get_unicode font glyph = font.get_unicode glyph;

(* |glyph_exists <font> <glyph>| tests whether the font contains a glyph with the number <glyph>. *)

value glyph_exists font glyph = do
{
  (glyph <= font.last_glyph && glyph >= font.first_glyph)
};

(* |glyph_spec_to_index <lookup-char> <lookup-name> <spec>| translates a |glyph_spec|
   into the corresponding glyph index.
*)

value glyph_spec_to_index lookup_char lookup_name spec = match spec with
[ GlyphIndex x -> x
| GlyphChar  x -> lookup_char x
| GlyphName  x -> lookup_name x
];

(*
  |index_to_glyph <font> <index>| checks whether <font> contains a glyph of the given index and returns
  the corresponding |glyph_desc|.
*)

value index_to_glyph font idx = do
{
  if glyph_exists font idx then
    Simple idx
  else
    Undef
};

(*
  |accent_base_point <font> <glyph-metric>| returns the point where the glyph is attached to another glyph
  when it is used as an accent.
*)

value accent_base_point font gm = do
{
  font.accent_base_point font gm
};

(*
  |accent_attach_point <font> <glyph-metric>| returns the point where an accent should be attached
  to the glyph.
*)

value accent_attach_point font gm = do
{
  font.accent_attach_point font gm
};

(*
  |accent_position <acc-font> <acc-gm> <chr-font> <chr-gm>| calculates the position of the accent glyph
  in a composite glyph.
*)

value accent_position acc_font acc_gm chr_font chr_gm = do
{
  let (acc_x, acc_y) = accent_base_point   acc_font acc_gm;
  let (chr_x, chr_y) = accent_attach_point chr_font chr_gm;

(*  let pos_y  = chr_gm.gm_height -/ acc_font.parameter.x_height;
  let pos_x  = (chr_gm.gm_width -/ acc_gm.gm_width) // num_of_int 2
               +/ (chr_font.parameter.slant */ chr_gm.gm_height
               -/ acc_font.parameter.slant */ acc_font.parameter.x_height);*)

  (chr_x -/ acc_x, chr_y -/ acc_y)
};

value rec get_glyph_metric font glyph = match glyph with
[ Undef
| Border _           -> empty_glyph_metric
| Simple g           -> font.glyph_metric.(g - font.first_glyph)
| Accent a g         -> construct_accent font (Simple a) font (Simple g)
| Sequence gs        -> construct_sequence font gs
| Extendable t m b _ -> do
  {
    let tgm = get_glyph_metric font t;
    let mgm = get_glyph_metric font m;
    let bgm = get_glyph_metric font b;

    {
      gm_width      = max_num tgm.gm_width (max_num mgm.gm_width bgm.gm_width);
      gm_height     = tgm.gm_height +/ tgm.gm_depth  +/ mgm.gm_height;
      gm_depth      = mgm.gm_depth  +/ bgm.gm_height +/ bgm.gm_depth;
      gm_italic     = num_zero;
      gm_extra      = GXI_Normal;
      gm_extra_kern = zero_kern_info
    }
  }
]

(*
  |construct_accent <acc-font> <acc> <chr-font> <chr>| constructs a glyph_metric structure for the
  composite glyph.
*)

and construct_accent acc_font acc chr_font chr = do
{
  let acc_gm = get_glyph_metric acc_font acc;
  let chr_gm = get_glyph_metric chr_font chr;

  let (_, pos_y) = accent_position acc_font acc_gm chr_font chr_gm;
  {
    (chr_gm)

    with

    gm_height = max_num chr_gm.gm_height (acc_gm.gm_height +/ pos_y);
    gm_depth  = max_num chr_gm.gm_depth  (acc_gm.gm_depth  -/ pos_y)
  }
}

and construct_sequence font glyphs = match glyphs with
[ []            -> empty_glyph_metric
| [g]           -> get_glyph_metric font (Simple g)
| [first :: gs] -> do
  {
    let m1 = get_glyph_metric font (Simple first);

    iter m1.gm_width m1.gm_height m1.gm_depth gs

    where rec iter width height depth glyphs = match glyphs with
    [ [last] -> do
      {
        let m2 = get_glyph_metric font (Simple last);
        {
          gm_width  = width +/ m2.gm_width;
          gm_height = max_num height m2.gm_height;
          gm_depth  = max_num depth  m2.gm_depth;
          gm_italic = m2.gm_italic;
          gm_extra  = GXI_Normal;
          gm_extra_kern = merge_kern_infos m1.gm_extra_kern m2.gm_extra_kern
        }
      }
    | [g::gs] -> do
      {
        let m = get_glyph_metric font (Simple g);

        iter
          (width +/ m.gm_width)
          (max_num height m.gm_height)
          (max_num depth  m.gm_depth)
          gs
      }
    | [] -> assert False
    ]
  }
];

(* Default value for |accent_base_point| where the base point lies at the x-height. *)

value accent_base_point_x_height font gm = do
{
  let pos_x = (gm.gm_width // num_of_int 2) +/ font.parameter.slant */ font.parameter.x_height;

  (pos_x, font.parameter.x_height)
};

(* Default value for |accent_attach_point| where the accent is attached to the top of the glyph. *)

value accent_attach_point_top font gm = do
{
  let pos_x = (gm.gm_width // num_of_int 2) +/ font.parameter.slant */ gm.gm_height;

  (pos_x, gm.gm_height)
};

(*
  |next_glyph <font> <glyph>| returns the next variant of a character, or |Undef| if there is none.
*)

value next_glyph font glyph = do
{
  match (get_glyph_metric font glyph).gm_extra with
  [ GXI_List n -> match (get_glyph_metric font (Simple n)).gm_extra with
                  [ GXI_Extendable t m b r -> Extendable (Simple t) (Simple m) (Simple b) (Simple r)
                  | _                      -> Simple n
                  ]
  | _             -> Undef
  ]
};

(*
  |get_lig_kern <font> <glyph-1> <glyph-2>| returns either the amount of kerning between two glyphs
  or the ligature formed by them.
*)

value get_lig_kern font glyph1 glyph2 = match (glyph1, glyph2) with
[ (Simple g1,   Simple g2)   -> font.kerning font g1 g2
| (Simple g1,   Accent _ g2) -> font.kerning font g1 g2
| (Accent _ g1, Simple g2)   -> font.kerning font g1 g2
| (Accent _ g1, Accent _ g2) -> font.kerning font g1 g2
| (Border b,    g)           -> let k = get_after_kerning  (get_glyph_metric font g) b in
                                if k <>/ num_zero then
                                  Kern k
                                else
                                  NoLigKern
| (g,           Border b)    -> let k = get_before_kerning (get_glyph_metric font g) b in
                                if k <>/ num_zero then
                                  Kern k
                                else
                                  NoLigKern
| _                          -> NoLigKern
];

value get_glyph_composer font script features = font.get_composer font script features;

value simple_ligature_substitution font items = do
{
  match items with
  [ [((`Glyph (g1, _), _) as gf1); ((`Glyph (g2, _), _) as gf2) :: rest] -> match get_lig_kern font g1 g2 with
    [ Ligature c s k1 k2 -> Some ([gf1; gf2],
                                  rest,
                                  (tex_ligature_cmd (Simple c) k1 k2, s))
    | Kern x             -> Some ([gf1; gf2],
                                  rest,
                                  (simple_pair_kerning_cmd x, 1))
    | NoLigKern          -> None
    ]
  | _ -> None
  ]
};

value simple_composer = Substitute.substitute;

value two_phase_composer font find_subst1 find_subst2 items = do
{
  Substitute.substitute font find_subst2
    (Substitute.substitute font find_subst1 items)
};

value add_border_kern margin_glyph space_glyph foreign_glyph size border_kern adjustments = do
{
  let add_kern g1 g2 kern trie = do
  {
    if kern =/ num_zero then
      trie
    else
      DynUCTrie.add_string
        [|g1; g2|]
        (simple_pair_kerning_cmd (size */ kern), 1)
        trie
  };

  iter border_kern DynUCTrie.empty

  where rec iter kern trie = match kern with
  [ [] -> do
    {
      if DynUCTrie.is_empty trie then
        adjustments
      else
        [DirectLookup trie :: adjustments]
    }
  | [(g,ki) :: ks] -> do
    {
      iter ks
        (add_kern margin_glyph g ki.ki_after_margin
          (add_kern g margin_glyph ki.ki_before_margin
            (add_kern space_glyph g ki.ki_after_space
              (add_kern g space_glyph ki.ki_before_space
                (add_kern foreign_glyph g ki.ki_after_foreign
                  (add_kern g foreign_glyph ki.ki_before_foreign
                    trie))))))
    }
  ]
};

value adjustment_spec_to_table lookup_char lookup_name adj = do
{
  let lookup g = glyph_spec_to_index lookup_char lookup_name g;

  DirectLookup (translate adj)

  where translate adj =
    GlyphSpecTrie.fold
      (fun key val table -> do
       {
         let k = Array.map lookup key;
         let v = match val with
         [ AdjKern x -> (Substitute.simple_pair_kerning_cmd x,
                         1)
         | AdjLig  x -> (Substitute.replace_with_single_glyph_cmd 2
                           (Substitute.Simple (lookup x)),
                         0)
         ];

         DynUCTrie.add_string k v table
       })
      adj
      DynUCTrie.empty
};

(* Default value for |draw_simple_glyph|. *)

value draw_simple_glyph font glyph = SimpleGlyph glyph font;

(* Variant of |draw_simple_glyph| that allows for displacing the glyph by some amount. *)

value draw_displaced_simple_glyph dx dy font glyph = do
{
  Group [Graphic.PutBox dx dy (SimpleGlyph glyph font)]
};

value rec draw_glyph font glyph = match glyph with
[ Undef
| Border _   -> Empty
| Simple g   -> do
  {
    (* <g> might be a non-existing glyph index, indicating e.g. a border glyph *)
    if g >= font.first_glyph && g <= font.last_glyph then
      font.draw_simple_glyph font g
    else
      Empty
  }
| Accent a g -> do
  {
    let a_gm = get_glyph_metric font (Simple a);
    let g_gm = get_glyph_metric font (Simple g);

    let (pos_x, pos_y) = accent_position font a_gm font g_gm;

    Group
      [Graphic.PutBox num_zero num_zero (draw_glyph font (Simple g));
       Graphic.PutBox pos_x    pos_y    (draw_glyph font (Simple a))];
  }
| Sequence gs -> do
  {
    let cmds = ListBuilder.make ();

    iter num_zero gs

    where rec iter x glyphs = match glyphs with
    [ []      -> Group (ListBuilder.get cmds)
    | [g::gs] -> do
      {
        let m = get_glyph_metric font (Simple g);

        ListBuilder.add cmds (Graphic.PutBox x num_zero (draw_glyph font (Simple g)));

        iter (x +/ m.gm_width) gs
      }
    ]
  }
| Extendable t m b _ -> do
  {
    let tgm = get_glyph_metric font t;
    let mgm = get_glyph_metric font m;
    let bgm = get_glyph_metric font b;

    Group
      [Graphic.PutBox num_zero (tgm.gm_depth +/ mgm.gm_height) (draw_glyph font t);
       Graphic.PutBox num_zero num_zero                        (draw_glyph font m);
       Graphic.PutBox num_zero (mgm.gm_depth +/ bgm.gm_height) (draw_glyph font b)]
  }
];

value get_hyphen_glyph  fm = fm.parameter.hyphen_glyph;
value get_skew_glyph    fm = fm.parameter.skew_glyph;
value get_margin_glyph  fm = fm.parameter.margin_glyph;
value get_space_glyph   fm = fm.parameter.space_glyph;
value get_foreign_glyph fm = fm.parameter.foreign_glyph;

value get_border_glyph fm b = match b with
[ Margin  -> get_margin_glyph  fm
| Space   -> get_space_glyph   fm
| Foreign -> get_foreign_glyph fm
];

(* Shorthand to access the dimension of a normal and an extended space of the font. *)

value space_glue font =
{
  d_base           = font.parameter.space;
  d_stretch_factor = font.parameter.space_stretch;
  d_stretch_order  = 0;
  d_shrink_factor  = font.parameter.space_shrink;
  d_shrink_order   = 0
};

value xspace_glue font =
{
  d_base           = font.parameter.space +/ font.parameter.extra_space;
  d_stretch_factor = font.parameter.space_stretch;
  d_stretch_order  = 0;
  d_shrink_factor  = font.parameter.space_shrink;
  d_shrink_order   = 0
};

value empty_parameter =
{
  hyphen_glyph     = Undef;
  skew_glyph       = Undef;
  margin_glyph     = Undef;
  space_glyph      = Undef;
  foreign_glyph    = Undef;
  slant            = num_zero;
  space            = num_zero;
  space_stretch    = num_zero;
  space_shrink     = num_zero;
  x_height         = num_zero;
  quad             = num_zero;
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

value empty_font =
{
  name                = "<null>";
  ps_name             = "";
  file_name           = "<internal>";
  font_type           = Other;
  first_glyph         = 0;
  last_glyph          = -1;
  design_size         = num_one;
  at_size             = num_one;
  check_sum           = num_zero;
  get_glyph           = (fun _ -> Undef);
  get_unicode         = (fun _ -> [||]);
  kerning             = (fun _ -> assert False);
  get_composer        = (fun _ _ _ i -> i);
  draw_simple_glyph   = (fun _ _ -> Empty);
  accent_base_point   = (fun _ -> assert False);
  accent_attach_point = (fun _ -> assert False);
  get_glyph_bitmap    = (fun _ -> assert False);
  get_glyph_name      = (fun _ -> assert False);
  glyph_metric        = [| |];
  parameter           = empty_parameter
};

value empty_load_params =
{
  flp_size           = num_zero;
  flp_encoding       = [| |];
  flp_hyphen_glyph   = Undef;
  flp_skew_glyph     = Undef;
  flp_letter_spacing = num_zero;
  flp_extra_pos      = GlyphSpecTrie.empty;
  flp_extra_subst    = GlyphSpecTrie.empty;
  flp_extra_kern     = []
};

