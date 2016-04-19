
open XNum;
open Substitute;
open GlyphMetric;
open FontMetric;

type virtual_glyph =
{
  vg_width  : num;
  vg_height : num;
  vg_depth  : num;
  vg_italic : num;
  vg_glyph  : simple_box
};

value get_glyph_bitmap _ _ = do
{
  raise (Failure "Bitmaps are not supported for virtual fonts.")
};

value make_metrics glyphs = do
{
  Array.init
    (Array.length glyphs)
    (fun i ->
      {
        gm_width      = glyphs.(i).vg_width;
        gm_height     = glyphs.(i).vg_height;
        gm_depth      = glyphs.(i).vg_depth;
        gm_italic     = glyphs.(i).vg_italic;
        gm_extra      = GXI_Normal;
        gm_extra_kern = zero_kern_info
      })
};

value get_kerning lig_kern _ g1 g2 = do
{
  try
    List.assoc (g1, g2) lig_kern
  with
  [ Not_found -> NoLigKern ]
};

value vf_composer fm _ _ = do
{
  simple_composer fm (simple_ligature_substitution fm)
};

value make_virtual_font name size glyphs lig_kern params = do
{
  let gm_table = make_metrics glyphs;

  {
    name                = name;
    ps_name             = name;
    file_name           = "";
    font_type           = Other;
    first_glyph         = 0;
    last_glyph          = Array.length glyphs - 1;
    design_size         = size;
    at_size             = size;
    check_sum           = num_zero;
    get_glyph           = Encodings.raw_encoding;
    get_unicode         = Encodings.raw_decoding;
    draw_simple_glyph   = (fun _ g -> glyphs.(g).vg_glyph);
    accent_base_point   = accent_base_point_x_height;
    accent_attach_point = accent_attach_point_top;
    get_composer        = vf_composer;
    kerning             = get_kerning lig_kern;
    get_glyph_bitmap    = get_glyph_bitmap;
    get_glyph_name      = (fun g -> Printf.sprintf "c%d" g);
    parameter           =
      {
        hyphen_glyph     = Simple 45;
        skew_glyph       = Undef;
        margin_glyph     = Undef;
        space_glyph      = Undef;
        foreign_glyph    = Undef;
        slant            = params.(0);
        space            = size */ params.(1);
        space_stretch    = size */ params.(2);
        space_shrink     = size */ params.(3);
        x_height         = size */ params.(4);
        quad             = size */ params.(5);
        extra_space      = size */ params.(6);
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
    glyph_metric = gm_table
  }
};

