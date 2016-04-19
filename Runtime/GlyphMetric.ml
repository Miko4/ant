
open XNum;
open Unicode.Types;
open Substitute;

type glyph_extra_info =
[ GXI_Normal
| GXI_LigKern    of int
| GXI_List       of int
| GXI_Extendable of int and int and int and int
];

type extra_kern_info =
{
  ki_after_space    : num;
  ki_before_space   : num;
  ki_after_margin   : num;
  ki_before_margin  : num;
  ki_after_foreign  : num;
  ki_before_foreign : num
};

type glyph_metric =
{
  gm_width      : num;
  gm_height     : num;
  gm_depth      : num;
  gm_italic     : num;
  gm_extra      : glyph_extra_info;
  gm_extra_kern : extra_kern_info
};

value zero_kern_info =
{
  ki_after_space    = num_zero;
  ki_before_space   = num_zero;
  ki_after_margin   = num_zero;
  ki_before_margin  = num_zero;
  ki_after_foreign  = num_zero;
  ki_before_foreign = num_zero
};

value empty_glyph_metric =
{
  gm_width      = num_zero;
  gm_height     = num_zero;
  gm_depth      = num_zero;
  gm_italic     = num_zero;
  gm_extra      = GXI_Normal;
  gm_extra_kern = zero_kern_info
};

(* The the after values from <left> and the before ones from <right>. *)

value merge_kern_infos left right =
{
  ki_after_space    = left.ki_after_space;
  ki_before_space   = right.ki_before_space;
  ki_after_margin   = left.ki_after_margin;
  ki_before_margin  = right.ki_before_margin;
  ki_after_foreign  = left.ki_after_foreign;
  ki_before_foreign = right.ki_before_foreign
};

type lig_kern =
[ NoLigKern
| Ligature of int and int and bool and bool  (* glyph skip keep-first? keep-second? *)
| Kern of num
];

type char_item 'box 'cmd =
[= `Char of uc_char
|  `Kern of (num * num)
|  `Box of 'box
|  `Command of 'cmd
|  `Break of (num * bool * list (char_item 'box 'cmd) * list (char_item 'box 'cmd) * list (char_item 'box 'cmd))
];

value get_after_kerning glyph_metric border = match border with
[ Space   -> glyph_metric.gm_extra_kern.ki_after_space
| Margin  -> glyph_metric.gm_extra_kern.ki_after_margin
| Foreign -> glyph_metric.gm_extra_kern.ki_after_foreign
];

value get_before_kerning glyph_metric border = match border with
[ Space   -> glyph_metric.gm_extra_kern.ki_before_space
| Margin  -> glyph_metric.gm_extra_kern.ki_before_margin
| Foreign -> glyph_metric.gm_extra_kern.ki_before_foreign
];

