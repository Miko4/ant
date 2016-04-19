
open XNum;

type colour =
[ Grey of num
| RGB  of num and num and num
| CMYK of num and num and num and num
];

type bezier 'a = ('a * 'a * 'a * 'a * 'a * 'a * 'a * 'a);

type path 'a = list (bezier 'a);

type path_cmd  = [ Stroke | Fill | Clip ];

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

value command_to_string cmd = match cmd with
[ PutBox  _ _ _   -> "put-box"
| Draw pc _       -> match pc with
                     [ Stroke -> "stroke"
                     | Fill   -> "fill"
                     | Clip   -> "clip"
                     ]
| SetColour     _ -> "set-colour"
| SetBgColour   _ -> "set-bg-colour"
| SetAlpha      _ -> "set-alpha"
| SetLineWidth  _ -> "set-line-width"
| SetLineCap    _ -> "set-line-cap"
| SetLineJoin   _ -> "set-line-join"
| SetMiterLimit _ -> "set-miter-limit"
];

value compare_colour c1 c2 = match (c1,c2) with
[ (Grey g1, Grey g2)                   -> compare_num g1 g2
| (Grey _,           _)                -> -1
| (_,                Grey _)           -> 1
| (RGB r1 g1 b1,     RGB r2 g2 b2)     -> if r1 </ r2 then -1
                                          else if r1 >/ r2 then 1
                                          else if g1 </ g2 then -1
                                          else if g1 >/ g2 then 1
                                          else if b1 </ b2 then -1
                                          else if b1 >/ b2 then -1
                                          else 0
| (RGB _ _ _,        _)                -> -1
| (_,                RGB _ _ _)        -> 1
| (CMYK c1 m1 y1 k1, CMYK c2 m2 y2 k2) -> if c1 </ c2 then -1
                                          else if c1 >/ c2 then 1
                                          else if m1 </ m2 then -1
                                          else if m1 >/ m2 then 1
                                          else if y1 </ y2 then -1
                                          else if y1 >/ y2 then -1
                                          else if k1 </ k2 then -1
                                          else if k1 >/ k2 then -1
                                          else 0
(* unused:
| (CMYK _ _ _ _, _)                    -> -1
| (_,                CMYK _ _ _ _)     -> 1
*)
];

