
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open FontMetric;
open Dim;
open Graphic;

type index_position = [ LeftIndex | RightIndex | VertIndex ];

type math_code =
[ NoMath    | Ordinary    | BinOp
| Relation  | Operator    | Punct
| Open      | Close       | Inner
| SubScript | SuperScript | IndexPosition of index_position
];


type page_info =
{
  pi_width     : num;
  pi_height    : num;
  pi_page_no   : int;
  pi_old_marks : list (uc_string * uc_string);
  pi_new_marks : list (uc_string * uc_string)
};

(* basic box *)

type basic_box 'a =
{
  b_width    : dim;
  b_height   : dim;
  b_depth    : dim;
  b_contents : 'a
};

(* contents of a box *)

type box = basic_box contents_type

and contents_type =
[ EmptyBox
| CharBox of glyph_desc and font_metric
| GlueBox of bool and bool
| RuleBox
| ImageBox of string and LoadImage.format
| CompBox of list gfx_cmd
| MathBox of math_code and box
| BreakBox of num and bool and list box and list box and list box
| CommandBox of box_cmd
| ProcBox of page_info -> (num * num) -> box -> list gfx_cmd
]

and box_cmd =
[= `GfxCmd of gfx_cmd
|  `ParCmd of par_cmd
|  `PageCmd of page_cmd
|  `Special of simple_cmd
]

and gfx_cmd = graphic_command dim box

and par_cmd =
[ VInsert of bool and (list box)
| CallParFunction of int -> unit
]

and floating = (uc_string * list extended_glyph_item)

and page_cmd =
[ SetNextLayout of uc_string
| SetMark of uc_string and uc_string
| CallPageFunction of page_info -> (num * num) -> unit
| Float of floating
]

and simple_glyph_item   = JustHyph.simple_glyph_item   box box
and extended_glyph_item = JustHyph.extended_glyph_item box box;

type glyph_composer     = Substitute.glyph_composer font_metric box box;

value empty_box            : box;
value is_emtpy_box         : box -> bool;

value is_real_box          : box -> bool;

value new_glyph_box        : glyph_desc -> font_metric -> box;
value new_char_box         : uc_char -> font_metric -> box;
value is_char_box          : box -> bool;

value new_glue_box         : dim -> dim -> bool -> bool -> box;
value new_kern_box         : num -> num -> box;
value is_glue_box          : box -> bool;
value is_implicit_glue     : box -> bool;
value is_discardable_glue  : box -> bool;

value new_rule_box         : dim -> dim -> dim -> box;
value is_rule_box          : box -> bool;

value new_compound_box      : dim -> dim -> dim -> list (gfx_cmd) -> box;
value is_compound_box       : box -> bool;
value wrap_in_compound_box  : box -> box;
value add_to_compound_box   : box -> list (gfx_cmd) -> box;
value scale_box_horiz       : box -> (num * int) -> box;
value scale_box_vert        : box -> (num * int) -> box;
value scale_box_horiz_upto  : box -> (num * int) -> box;
value scale_box_vert_upto   : box -> (num * int) -> box;
value resize_box_horiz      : box -> num -> box;
value resize_box_vert       : box -> num -> num -> box;
value resize_box_horiz_upto : box -> num -> box;
value resize_box_vert_upto  : box -> num -> num -> box;
value shift_compound_vert   : box -> num -> box;
value make_phantom          : box -> box;
value make_hphantom         : box -> box;
value make_vphantom         : box -> box;

value new_image_box        : num -> num -> string -> LoadImage.format -> box;

value new_math_box         : math_code -> box -> box;
value is_math_box          : box -> bool;
value math_box_code        : box -> math_code;

value new_break_box        : num -> bool -> list box -> list box -> list box -> box;
value forced_break_box     : box;
value break_box_penalty    : box -> num;
value break_box_hyph       : box -> bool;
value break_box_pre        : box -> list box;
value break_box_post       : box -> list box;
value break_box_no         : box -> list box;

value new_command_box      : box_cmd -> box;

value new_proc_box         : dim -> dim -> dim ->
                             (page_info -> (num * num) -> box -> list gfx_cmd) ->
                             box;

value discard_glue         : list box -> (list box * list box);
value remove_breaks        : list box -> list box;
value simple_item_to_box   : simple_glyph_item -> box;
value extended_item_to_box : extended_glyph_item -> box;
value draw_box             : page_info -> num -> num -> box -> simple_box;
value log_box              : box -> unit;
value long_dump_box        : box -> unit;
value short_dump_box       : box -> unit;
value short_dump_box_list  : list box -> unit;

