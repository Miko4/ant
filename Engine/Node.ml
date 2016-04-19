
open XNum;
open Runtime;
open Unicode.Types;
open Dim;
open Typesetting;
open Box;
open Environment;

type area_contents_arg =
[= `Galley of (uc_string * skip_arg * skip_arg * skip_arg * skip_arg)
|  `Float of (FloatVertical.vert_alignment * skip_arg * skip_arg * dim_arg)
|  `Footnote of (list node * skip_arg * skip_arg * skip_arg *
                 line_param_modifier * par_param_modifier * line_break_param_modifier *
                 hyphen_param_modifier * space_param_modifier * math_param_modifier)
|  `Direct of page_info -> (num * num) -> list node
]
and node =
[ Nodes of list node
| Command of UCStream.location and env_cmd
| CommandBox of UCStream.location and box_cmd
| GfxCommand of UCStream.location and Graphic.graphic_command dim_arg box
| NewGalley of UCStream.location and uc_string and skip_arg
| NewLayout of UCStream.location and uc_string and skip_arg and skip_arg
| NewArea of UCStream.location and uc_string and skip_arg and skip_arg and skip_arg and skip_arg and skip_arg and skip_arg and area_contents_arg
| ShipOut of UCStream.location and uc_string and uc_string and int
| AddToGalley of UCStream.location and uc_string and list node
| PutGalleyInVBox of UCStream.location and bool and uc_string
| ModifyGalleyGlue of UCStream.location and environment -> list box -> list box
| Paragraph of UCStream.location and list node
| BeginGroup of UCStream.location
| EndGroup of UCStream.location
| Float of UCStream.location and uc_string and list node
| Glyph of UCStream.location and int
| Letter of UCStream.location and uc_char
| Space of UCStream.location
| Glue of UCStream.location and dim_arg and dim_arg and bool and bool
| Break of UCStream.location and option num and bool and list node and list node and list node
| Rule of UCStream.location and dim_arg and dim_arg and dim_arg
| Image of UCStream.location and string and LoadImage.format and skip_arg and skip_arg
| Accent of UCStream.location and uc_char and list node
| HBox of UCStream.location and [= `LR | `RL | `Default] and list node
| HBoxTo of UCStream.location and [= `LR | `RL | `Default] and skip_arg and list node
| HBoxSpread of UCStream.location and [= `LR | `RL | `Default] and skip_arg and list node
| VBox of UCStream.location and list node
| VBoxTo of UCStream.location and skip_arg and list node
| VBoxSpread of UCStream.location and skip_arg and list node
| Phantom of UCStream.location and bool and bool and list node
| HLeaders of UCStream.location and dim_arg and list node
| VInsert of UCStream.location and bool and list node
| Table of UCStream.location and list node
| TableEntry of UCStream.location and int and int and int and int and int and list node
| Math of UCStream.location and list node
| MathCode of UCStream.location and math_code and list node
| MathChar of UCStream.location and (math_code * (int * int) * (uc_char * uc_char))
| SubScript of UCStream.location and list node
| SuperScript of UCStream.location and list node
| Fraction of UCStream.location and list node and list node and node and node and skip_arg
| Underline of UCStream.location and list node
| Overline of UCStream.location and list node
| MathAccent of UCStream.location and int and uc_char and list node
| Root of UCStream.location and int and uc_char and int and uc_char and list node
| LeftRight of UCStream.location and list (list node)
| MathStyle of UCStream.location and MathLayout.math_style
| IndexPosition of UCStream.location and Box.index_position
];

