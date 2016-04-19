
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

value zero_kern_info     : extra_kern_info;
value empty_glyph_metric : glyph_metric;
value merge_kern_infos   : extra_kern_info -> extra_kern_info -> extra_kern_info;
value get_after_kerning  : glyph_metric -> border_glyph -> num;
value get_before_kerning : glyph_metric -> border_glyph -> num;

