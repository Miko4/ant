
open XNum;

type node 'a =
[ Horiz of num and node 'a and node 'a    (* horizontal division *)
| Vert  of num and node 'a and node 'a    (* vertical division   *)
| Empty
| Value of 'a
];

type rect =
{
  min_x : num;
  min_y : num;
  max_x : num;
  max_y : num
};

value in_rect r x y = do
{
  x >=/ r.min_x && x <=/ r.max_x &&
  y >=/ r.min_y && y <=/ r.max_y
};

type interval = (num * num);

type map 'a =
{
  nodes  : node 'a;
  bounds : rect
};

value empty x0 y0 x1 y1 =
{
  nodes  = Empty;
  bounds = {
             min_x = min_num x0 x1;
             min_y = min_num y0 y1;
             max_x = max_num x0 x1;
             max_y = max_num y0 y1
           }
};

value get_value map x y = do
{
  if in_rect map.bounds x y then
    iter map.nodes
  else
    None
}
where rec iter node = match node with
[ Empty       -> None
| Value v     -> Some v
| Vert z a b  -> if x <=/ z then
                   iter a
                 else
                   iter b
| Horiz z a b -> if y <=/ z then
                   iter a
                 else
                   iter b
];

value rec internal_set_rect map x0 y0 x1 y1 val =
{
  (map)

  with

  nodes =
    iter
      map.bounds.min_x map.bounds.min_y map.bounds.max_x map.bounds.max_y
      (max_num x0 map.bounds.min_x)
      (max_num y0 map.bounds.min_y)
      (min_num x1 map.bounds.max_x)
      (min_num y1 map.bounds.max_y)
      map.nodes
}
where rec iter bx0 by0 bx1 by1 x0 y0 x1 y1 node = match node with
[ Empty   -> insert_rect bx0 by0 bx1 by1 x0 y0 x1 y1 Empty
| Value v -> insert_rect bx0 by0 bx1 by1 x0 y0 x1 y1 (Value v)
| Vert z a b -> do
  {
    if z <=/ x0 then
      Vert z a (iter z by0 bx1 by1 x0 y0 x1 y1 b)
    else if z >=/ x1 then
      Vert z (iter bx0 by0 z by1 x0 y0 x1 y1 a) b
    else
      Vert z (iter bx0 by0 z by1 x0 y0 z y1 a)
             (iter z by0 bx1 by1 z y0 x1 y1 b)
  }
| Horiz z a b -> do
  {
    if z <=/ y0 then
      Horiz z a (iter bx0 z bx1 by1 x0 y0 x1 y1 b)
    else if z >=/ y1 then
      Horiz z (iter bx0 by0 bx1 z x0 y0 x1 y1 a) b
    else
      Horiz z (iter bx0 by0 bx1 z x0 y0 x1 z a)
              (iter bx0 z bx1 by1 x0 z x1 y1 b)
  }
]
where rec insert_rect bx0 by0 bx1 by1 x0 y0 x1 y1 v = do
{
  if by0 </ y0 then
    Horiz y0 v (insert_rect bx0 y0 bx1 by1 x0 y0 x1 y1 v)
  else if by1 >/ y1 then
    Horiz y1 (insert_rect bx0 by0 bx1 y1 x0 y0 x1 y1 v) v
  else if bx0 </ x0 then
    Vert x0 v (insert_rect x0 by0 bx1 by1 x0 y0 x1 y1 v)
  else if bx1 >/ x1 then
    Vert x1 (insert_rect bx0 by0 x1 by1 x0 y0 x1 y1 v) v
  else
    val
};

value set_rect   map x0 y0 x1 y1 val = internal_set_rect map x0 y0 x1 y1 (Value val);
value clear_rect map x0 y0 x1 y1     = internal_set_rect map x0 y0 x1 y1 Empty;

value rec merge_intervals l0 l1 = match (l0, l1) with
[ ([], _) -> l1
| (_, []) -> l0
| ([(a,b) :: xs], [(c,d) :: ys]) -> do
  {
    if b <=/ c then
      [(a,b) :: merge_intervals xs l1]
    else if d <=/ a then
      [(c,d) :: merge_intervals l0 ys]
    else
      [(min_num a c, max_num b d) :: merge_intervals xs ys]
  }
];

value horiz_strip map y0 y1 = do
{
  iter (max_num y0 map.bounds.min_y)
       (min_num y1 map.bounds.max_y)
       map.bounds.min_x
       map.bounds.max_x
       map.nodes
}
where rec iter y0 y1 x0 x1 node = match node with
[ Empty       -> []
| Value _     -> [(x0,x1)]
| Vert  z a b -> iter y0 y1 x0 z a @ iter y0 y1 z x1 b
| Horiz z a b -> do
  {
    if z <=/ y0 then
      iter y0 y1 x0 x1 b
    else if z >=/ y1 then
      iter y0 y1 x0 x1 a
    else
      merge_intervals (iter y0 z x0 x1 a) (iter z y1 x0 x1 b)
  }
];

value vert_strip map x0 x1 = do
{
  iter (max_num x0 map.bounds.min_x)
       (min_num x1 map.bounds.max_x)
       map.bounds.min_y
       map.bounds.max_y
       map.nodes
}
where rec iter x0 x1 y0 y1 node = match node with
[ Empty       -> []
| Value _     -> [(y0,y1)]
| Horiz z a b -> iter x0 x1 y0 z a @ iter x0 x1 z y1 b
| Vert  z a b -> do
  {
    if z <=/ x0 then
      iter x0 x1 y0 y1 b
    else if z >=/ x1 then
      iter x0 x1 y0 y1 a
    else
      merge_intervals (iter x0 z y0 y1 a) (iter z x1 y0 y1 b)
  }
];

(*
  |find_first_free_interval <p> <min> <max> <strip>| returns the first non-empty interval
  in <strip> satisfying <p>.
*)

value find_first_free_interval p min max strip = do
{
  iter min strip

  where rec iter z strip = match strip with
  [ [] -> do
    {
      if z </ max && p z max then
        Some (z, max)
      else
        None
    }
  | [(a,b) :: cs] -> do
    {
      if z </ a && p z a then
        Some (z, a)
      else
        iter b cs
    }
  ]
};

value find_last_free_interval p min max strip = do
{
  iter max (List.rev strip)

  where rec iter z strip = match strip with
  [ [] -> do
    {
      if min </ z && p min z then
        Some (min, z)
      else
        None
    }
  | [(a,b) :: cs] -> do
    {
      if b </ z && p b z then
        Some (b, z)
      else
        iter a cs
    }
  ]
};

(*
  The next four routines look for a free rectangle.
  |find_free_top <map> <rect> <height>| returns a pair |(a, b)| where <y> is
  the top most position such that a box of the given height fits on the page.
  |c - y| is the height of the free space.
*)

value find_free_top map min_x min_y max_x max_y height = do
{
  find_last_free_interval
    (fun a b -> max_num min_y a +/ height <=/ min_num max_y b)
    map.bounds.min_y map.bounds.max_y
    (vert_strip map min_x max_x)
};

value find_free_bottom map min_x min_y max_x max_y height = do
{
  find_first_free_interval
    (fun a b -> max_num min_y a +/ height <=/ min_num max_y b)
    map.bounds.min_y map.bounds.max_y
    (vert_strip map min_x max_x)
};

value find_free_left map min_x min_y max_x max_y width = do
{
  find_first_free_interval
    (fun a b -> max_num min_x a +/ width <=/ min_num max_x b)
    map.bounds.min_x map.bounds.max_x
    (vert_strip map min_y max_y)
};

value find_free_right map min_x min_y max_x max_y width = do
{
  find_last_free_interval
    (fun a b -> max_num min_x a +/ width <=/ min_num max_x b)
    map.bounds.min_x map.bounds.max_x
    (vert_strip map min_y max_y)
};

