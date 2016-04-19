
open XNum;

type colour =
[ Grey of num
| RGB  of num and num and num
| CMYK of num and num and num and num
];

type bezier 'a = ('a * 'a * 'a * 'a * 'a * 'a * 'a * 'a);

type path 'a = list (bezier 'a);

type path_cmd   = [ Stroke | Fill | Clip ];

type line_cap  = [ Butt  | Circle | Square ];
type line_join = [ Miter | Round  | Bevel  ];

type graphic_command 'dim 'box =
[ PutBox        of 'dim and 'dim and 'box
| Draw          of path_cmd and path 'dim
| SetColour     of colour
| SetBgColour   of colour
| SetAlpha      of num
| SetLineWidth  of num
| SetLineCap    of line_cap
| SetLineJoin   of line_join
| SetMiterLimit of num
];

value command_to_string : graphic_command 'a 'b -> string;

value compare_colour : colour -> colour -> int;

