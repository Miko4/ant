
;; constructors and accessors

;; points and vectors

geo_polar rad angle := (rad * cosd angle, rad * sind angle);

geo_rotate_90 (x,y) := (~y, x);

geo_decompose_vector vec axis :=
  local s t;
  begin
    vec = s * axis + t * (geo_rotate_90 axis),
    (s, t)
  end;

;; lines and segments

geo_line point=(_,_) dir=(_,_) := (Line, point, dir);
geo_line point=(_,_) angle     := (Line, point, geo_polar 1 angle);

geo_segment x y := (Segment, x, y);

geo_line_of_segment (Segment, x, y) := (Line, x, y - x);
geo_line_through_points p_1 p_2     := (Line, p_1, p_2 - p_1);

geo_midpoint (Segment, x, y) := 1/2[x,y];

geo_direction (Line, _, d)    := d;
geo_direction (Segment, x, y) := y - x;

geo_is_parallel a b :=
  geo_direction a * geo_rotate_90 (geo_direction b) == 0;

geo_intersection (Line, p_1, d_1) (Line, p_2, d_2) :=
  local q;
  begin
    q = p_1 + _ * d_1,
    q = p_2 + _ * d_2,
    q
  end;

geo_bisector d_1=(_,_) d_2=(_,_) :=
  d_1 / abs d_1 + d_2 / abs d_2;

geo_bisector l_1=(Line, p_1, d_1) l_2=(Line, p_2, d_2) :=
  if geo_is_parallel l_1 l_2 then
    (Line, 1/2[p_1,p_2], d_1)
  else
    (Line,
      geo_intersection l_1 l_2,
      geo_bisector d_1 d_2)
  end;

geo_bisector_between_points a b c :=
  (Line, b, geo_bisector (a - b) (c - b));

geo_parallel (Line, _, d)    p := (Line, p, d);
geo_parallel (Segment, x, y) p := (Line, p, y - x);

geo_perpendicular (Line, _, d)    p := (Line, p, geo_rotate_90 d);
geo_perpendicular (Segment, x, y) p := (Line, p, geo_rotate_90 (y - x));

geo_projection l=(Line, _, _) p := geo_intersection l (geo_perpendicular l p);

;; conics

;; ellipse : point -> num -> num -> angle -> conic

geo_circle centre rad :=
  (Ellipse, centre, (rad, 0), (0, rad));

geo_ellipse centre rad_a rad_b angle :=
  (Ellipse,
    centre,
    geo_polar rad_a angle,
    geo_polar rad_b (angle + 90));

geo_hyperbola centre rad_a rad_b orientation :=
  (Hyperbola,
    centre,
    geo_polar rad_a orientation,
    geo_polar rad_b (orientation + 90));

geo_parabole apex=(_,_) axis=(_,_) :=
  (Parabola,
    apex,
    1/4 geo_rotate_90 axis,
    axis);

geo_parabola focus=(_,_) directrix=(Line,_,_) :=
  local x := geo_projection directrix focus;
  geo_parabola 1/2[focus, x] (focus - x);

geo_conic focus_1=(_,_) focus_2=(_,_) eccentricity :=
  local begin
    centre := 1/2[focus_1,focus_2];
    f      := 1 / eccentricity;
    d      := focus_2 - focus_1;
    rad_a  := 1/2 f * d;
    rad_b  := geo_rotate_90 d;
  end
  if eccentricity < 1 then
    (Ellipse,   centre, rad_a, sqrt (f * f - 1) * rad_b)
  else
    (Hyperbola, centre, rad_a, sqrt (1 - f * f) * rad_b)
  end;

geo_conic focus=(_,_) directrix=(Line,_,_) eccentricity :=
  if eccentricity == 1 then
    geo_parabola focus directrix
  else
    local x := geo_projection directrix focus;
    geo_conic focus (2x - focus) eccentricity
  end;

geo_foci (Parabola, centre, rad_a, rad_b) :=
  local f := centre + 4 rad_b;
  (f, f);
geo_foci (Ellipse, centre, rad_a, rad_b) :=
  local f := (rad_b * rad_b) / (rad_a * rad_a);
  if f <= 1 then
    local c := sqrt (1 - f * f) * rad_a;
    (centre - c, centre + c)
  else
    local c := sqrt (1 - 1 / f * f) * rad_b;
    (centre - c, centre + c)
  end;
geo_foci (Hyperbola, centre, rad_a, rad_b) :=
  local c := sqrt (1 + (rad_b * rad_b) / (rad_a * rad_a)) * rad_a;
  (centre - c, centre + c);

;; radius : circle -> num

geo_circle_touching_point centre point :=
  local rad := point - centre;
  (Ellipse, centre, rad, geo_rotate_90 rad);

geo_circle_through_points p_1 p_2 p_3 :=
  local begin
    l_1 := geo_line 1/2[p_1,p_2] (geo_rotate_90 (p_1 - p_2));
    l_2 := geo_line 1/2[p_2,p_3] (geo_rotate_90 (p_2 - p_3));
    c   := geo_intersection l_1 l_2;
    r   := p_2 - c;
  end
  (Ellipse, c, r, geo_rotate_90 r);

geo_major_axis (Ellipse,   _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Ellipse,   _, r_a, r_b) := min (abs r_a) (abs r_b);
geo_major_axis (Parabola,  _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Parabola,  _, r_a, r_b) := min (abs r_a) (abs r_b);
geo_major_axis (Hyperbola, _, r_a, r_b) := max (abs r_a) (abs r_b);
geo_minor_axis (Hyperbola, _, r_a, r_b) := min (abs r_a) (abs r_b);

geo_eccentricity (Ellipse,   _, r_a, r_b) := sqrt (1 - (r_b * r_b) / (r_a * r_a));
geo_eccentricity (Parabola,  _, r_a, r_b) := 1;
geo_eccentricity (Hyperbola, _, r_a, r_b) := sqrt (1 + (r_b * r_b) / (r_a * r_a));

;; polygons

geo_triangle a b c := (Polygon, [a, b, c]);

geo_equilateral size angle :=
  (Polygon,
    [geo_polar size (angle + 90),
     geo_polar size (angle + 210),
     geo_polar size (angle + 330)]);

geo_isosceles_of_angle size angle orientation :=
  local begin
    a := geo_polar size (270 - angle + orientation);
    b := geo_polar size (270 + angle + orientation);
    c := (a + b) / 3;
  end
  (Polygon, [a - c, ~c, b - c]);
geo_isosceles_of_base len base orientation :=
  local begin
    a := 1/2geo_polar base orientation;
    b := geo_polar (sqrt (len * len - base * base/4)) (orientation + 90);
  end
  (Polygon, [a - b/3, b/3, ~a - b/3]);

geo_triangle_of_angles size a_1 a_2 orientation :=
  local begin
    p := geo_polar size orientation;
    q := geo_intersection (geo_line (0,0) (geo_polar 1 a_1))
                          (geo_line p     (geo_polar 1 a_2));
    c := (p + q) / 3;
  end
  (Polygon, [p - c, q - c, ~c]);

geo_triangle_of_lengths len_1 len_2 len_3 orientation :=
  local begin
    d := geo_polar 1 orientation;
    x := 1/2(len_1 * len_1 + len_2 * len_2 - len_3 * len_3);
    y := sqrt (len_2 * len_2 - x * x);
    a := len_1 * d;
    b := a - x * d + y * geo_rotate_90 d;
    c := (a + b) / 3;
  end
  (Polygon, [a - c, b - c, ~c]);

geo_triangle_of_angle len_1 len_2 angle orientation :=
  local base := sqrt (len_1 * len_1 + len_2 * len_2 - 2 * len_1 * len_2 * cosd angle);
  geo_triangle_of_lengths base len_1 len_2 orientation;

geo_height (Polygon, [a, b, c]) :=
  local x := geo_projection (geo_line_through_points a b) c;
  abs (c - x);

;; geo_incircle (Polygon, [a, b, c]) :=

geo_parallelogram edge_1=(_,_) edge_2=(_,_) :=
  local begin
    p_1 := (_,_);
    p_2 := (_,_);
    p_3 := (_,_);
    p_4 := (_,_);
  end
  begin
    p_4 - p_1 = edge_1,
    p_3 - p_2 = edge_1,
    p_2 - p_1 = edge_2,
    p_3 - p_4 = edge_2,
    (Polygon, [p_1, p_2, p_3, p_4])
  end;
geo_parallelogram (Segment,x,y) edge_2=(_,_) :=
  local begin
    p_1 := (_,_);
    p_2 := (_,_);
  end
  begin
    p_2 - x = edge_2,
    p_1 - y = edge_2,
    p_2 - p_1 = y - x,
    (Polygon, [x, y, p_1, p_2])
  end;
geo_parallelogram edge_1=(_,_) (Segment,x,y) :=
  local begin
    p_1 := (_,_);
    p_2 := (_,_);
  end
  begin
    p_2 - x = edge_1,
    p_1 - y = edge_1,
    p_2 - p_1 = y - x,
    (Polygon, [x, y, p_1, p_2])
  end;
geo_parallelogram width height :=
  { angle_1 angle_2 :=
      geo_parallelogram
        (geo_polar angle_1 width)
        (geo_polar angle_2 height)
  };
geo_rectangle width height angle := geo_parallelogram width height angle (angle+90);
geo_square    size         angle := geo_parallelogram size  size   angle (angle+90);

geo_polygon vertices := (Polygon, vertices);

geo_regular_polygon n size :=
  (Polygon,
    map { i := geo_polar size 360i/n }
        (enum_from_to 0 (n-1)));

geo_barycentre (Polygon, vertices) :=
  geo_barycentre vertices;

geo_barycentre [] :=
  error "geo_barycentre: empty list!";

geo_barycentre points=[(_,(_,_)) : _] :=
  foldl { sum (s,p) := sum + s * p } (0,0) points / length points;

geo_barycentre points=[(_,_) : _] :=
  foldl (+) (0,0) points / length points;

;; distance:
;;   point point
;;   point line
;;   line  point

geo_distance p_1=(_,_) p_2=(_,_) := geo_length (p_2 - p_1);

geo_distance x=(_,_) (Line, p, dir=(d_x, d_y)) :=
  abs ((~d_y, d_x) * (x - p) / abs dir);

geo_distance (Line, p, dir=(d_x, d_y)) x=(_,_) :=
  abs ((~d_y, d_x) * (x - p) / abs dir);

;; FIX: other distances: point conic, conic conic, segent line,...

;; geometric transformations:
;; translation, rotation, relfection

geo_translate p=(_,_)                  vec := p + vec;
geo_translate (Line, p, d)             vec := (Line,      p + vec, d);
geo_translate (Segment, x, y)          vec := (Segment,   x + vec, y + vec);
geo_translate (Ellipse,   c, r_a, r_b) vec := (Ellipse,   c + vec, r_a, r_b);
geo_translate (Hyperbola, c, r_a, r_b) vec := (Hyperbola, c + vec, r_a, r_b);
geo_translate (Parabola,  c, r_a, r_b) vec := (Parabola,  c + vec, r_a, r_b);
geo_translate (Polygon, vertices)      vec := (Polygon,   map (+ vec) vertices);

geo_rotate_vec (x,y) angle :=
  (x * cosd angle - y * sind angle,
   x * sind angle + y * cosd angle);

geo_rotate p=(_,_) point angle :=
  point + geo_rotate_vec (p - point) angle;

geo_rotate (Line, p, d) point angle :=
  (Line,
    geo_rotate p point angle,
    geo_rotate_vec d angle);

geo_rotate (Segment, x, y) point angle :=
  (Segment,
    geo_rotate x point angle,
    geo_rotate y point angle);

geo_rotate (Ellipse, c, r_a, r_b) point angle :=
  (Ellipse,
    geo_rotate c point angle,
    geo_rotate_vec r_a angle,
    geo_rotate_vec r_b angle);

geo_rotate (Hyperbola, c, r_a, r_b) point angle :=
  (Hyperbola,
    geo_rotate c point angle,
    geo_rotate_vec r_a angle,
    geo_rotate_vec r_b angle);

geo_rotate (Parabola, c, r_a, r_b) point angle :=
  (Parabola,
    geo_rotate c point angle,
    geo_rotate_vec r_a angle,
    geo_rotate_vec r_b angle);

geo_rotate (Polygon, vertices) point angle :=
  (Polygon,
    map { p := geo_rotate p point angle } vertices);

geo_reflect_vec vec=(_,_) l=(Line, _, d) :=
  local (s,t) := geo_decompose_vector vec d;
  (s * d - t * geo_rotate_90 d);

geo_reflect p=(_,_) l=(Line, b, _) :=
  b + geo_reflect_vec (p - b) l;

geo_reflect (Line, p, d) l=(Line, _, _) :=
  (Line,
    geo_reflect p l,
    geo_reflect_vec d l);

geo_reflect (Segment, x, y) l=(Line, _, _) :=
  (Segment,
    geo_reflect x l,
    geo_reflect y l);

geo_reflect (Ellipse, c, r_a, r_b) l=(Line, _, _) :=
  (Ellipse,
    geo_reflect c l,
    geo_reflect_vec r_a l,
    geo_reflect_vec r_b l);

geo_reflect (Hyperbola, c, r_a, r_b) l=(Line, _, _) :=
  (Hyperbola,
    geo_reflect c l,
    geo_reflect_vec r_a l,
    geo_reflect_vec r_b l);

geo_reflect (Parabola, c, r_a, r_b) l=(Line, _, _) :=
  (Parabola,
    geo_reflect c l,
    geo_reflect_vec r_a l,
    geo_reflect_vec r_b l);

geo_reflect (Polygon, vertices) l=(Line, _, _) :=
  (Polygon,
    map { p := geo_reflect p l } vertices);

;; length
;;   point
;;   segment

geo_length p=(_,_) := abs p;

geo_length (Segment, x, y) := geo_length (y - x);

;; angle
;;   vector
;;   line
;;   segment
;;   conic
;;   vector, vector
;;   point, point, point

geo_angle (x,y) :=
  if y > 0 then
    90 - arctand (x / y)
  elseif y < 0 then
    270 - arctand (x / y)
  elseif x >= 0 then
    0
  else
    180
  end;

geo_angle (Line, _, d)             := geo_angle d;
geo_angle (Segment, x, y)          := geo_angle (y - x);
geo_angle (Ellipse,   _, rad_a, _) := geo_angle rad_a;
geo_angle (Parabola,  _, rad_a, _) := geo_angle rad_a;
geo_angle (Hyperbola, _, rad_a, _) := geo_angle rad_a;

geo_angle_between_vectors v_1=(_,_) v_2=(_,_) :=
  geo_angle v_1 - geo_angle v_2;

geo_angle_between_points p_1=(_,_) p_2=(_,_) p_3=(_,_) :=
  geo_angle (p_1 - p_2) - geo_angle (p_3 - p_2);

;; geo_centre

geo_centre (Segment, x, y)      := 1/2[x,y];
geo_centre (Ellipse,   c, _, _) := c;
geo_centre (Parabola,  c, _, _) := c;
geo_centre (Hyperbola, c, _, _) := c;

;; geo_point

geo_point s (Line, p, d)    := p + s * d / abs d;
geo_point s (Segment, x, y) := s[x,y];

geo_point s (Ellipse,   c, r_a, r_b) := c + cosd t * r_a + sind t * r_b
                                        where t := 360s end;
geo_point s (Parabola,  c, r_a, r_b) := c + s * r_a + s^2 * r_b;
geo_point s (Hyperbola, c, r_a, r_b) := c - r_a/cosd t + tand t * r_b
                                        where t := 360s end;

;; geo_argument

geo_argument (Line, x, d) p :=
  local s;
  begin
    p = x + s * d / abs d,
    s
  end;
geo_argument (Segment, x, y) p :=
  local s;
  begin
    p = s[x,y],
    s
  end;
geo_argument (Ellipse, c, r_a, r_b) p :=
  geo_angle (c + r_a) c p / 360;
geo_argument (Parabola, c, r_a, r_b) p :=
  local s;
  begin
    p = c + s * r_a + _ * r_b,
    s
  end;
geo_argument (Hyperbola, c, r_a, r_b) p :=
  local s;
  begin
    p = c + _ * r_a + s * r_b,
    arctand s / 360
  end;

geo_bounds (x,y) := (x,y,x,y);
geo_bounds (Segment, (x_0,y_0), (x_1,y_1)) :=
  (min x_0 x_1, min y_0 y_1,
   max x_0 x_1, max y_0 y_1);

geo_bounds (Ellipse, c=(cx,cy), r_a=(rax,ray), r_b=(rbx,rby)) :=
  if ray == 0 then
    local begin
      dx := abs rax;
      dy := abs rby
    end
    (cx - dx, cy - dy,
     cx + dx, cy + dy)
  elseif rax == 0 then
    local begin
      dx := abs rbx;
      dy := abs ray
    end
    (cx - dx, cy - dy,
     cx + dx, cy + dy)
  else
    local begin
      a     := abs r_a;
      b     := abs r_b;
      phi_x := arctand (~ b / a * ray / rax);
      phi_y := arctand (a / b * rax / ray);
      dx    := abs (rax * cosd phi_x - rbx * sind phi_x);
      dy    := abs (ray * sind phi_y + rby * cosd phi_y);
    end
    (cx - dx, cy - dy,
     cx + dx, cx + dy)
  end;

geo_bounds (Polygon, [(x,y):ps]) :=
  foldl
    { (min_x, min_y, max_x, max_y) (x,y) :=
        (min min_x x, min min_y y,
         max max_x x, max max_y y) }
    (x,y,x,y)
    ps;

;; FIX: respect <bounds>

;; geo_in_bounds <rect> <point> tests whether <point> lies inside <rect>.

geo_in_bounds (min_x,min_y,max_x,max_y) (x,y) :=
  min_x <= x && x <= max_x &&
  min_y <= y && y <= max_y;

;; geo_intersect_intervals a b c d computes the intersection of the intervals [a,b] and [c,d].

geo_intersect_intervals a b c d :=
  local begin
    min_ab := min a b;
    max_ab := max a b;
    min_cd := min c d;
    max_cd := max c d;
  end
  if max_ab < min_cd || max_cd < min_ab then
    None
  else
    (Some, max min_ab min_cd,
           min max_ab max_cd)
  end;

geo_clip_line_to_rect rect=(min_x,min_y,max_x,max_y) a=(ax,ay) b=(bx,by) :=
  if geo_in_bounds rect a
  && geo_in_bounds rect b then
    (Some, (a,b))
  elseif ax == bx then
    if ax < min_x || ax > max_x then
      None
    else
      local begin
        s t;
        d := by - ay;
      end
      begin
        ay + s * d = min_y,
        ay + t * d = max_y,
        match geo_intersect_intervals s t 0 1 with
        { None         := None
        | (Some, u, v) := (Some, (ax, ay + u * d), (ax, ay + v * d))
        }
      end
    end
  elseif ay == by then
    if ay < min_y || ay > max_y then
      None
    else
      local begin
        s t;
        d := bx - ax;
      end
      begin
        ax + s * d = min_x,
        ax + t * d = max_x,
        match geo_intersect_intervals s t 0 1 with
        { None         := None
        | (Some, u, v) := (Some, (ax + u * d, ay), (ax + v * d, ay))
        }
      end
    end
  else
    local begin
      s t u v;
      dx := bx - ax;
      dy := by - ay;
    end
    begin
      ax + s * dx = min_x,
      ax + t * dx = max_x,
      ay + u * dy = min_y,
      ay + v * dy = max_y,
      match geo_intersect_intervals s t 0 1 with
      { None           := None
      | (Some, ss, tt) := match geo_intersect_intervals ss tt u v with
        { None           := None
        | (Some, uu, vv) := (Some, (ax + uu * dx, ay + uu * dy),
                                   (ax + vv * dx, ay + vv * dy))
        }
      }
    end
  end;

geo_clip_spline_to_rect bounds spline := spline; ;; FIX

geo_draw_line bounds p_1 p_2 :=
  match geo_clip_line_to_rect bounds p_1 p_2 with
  { (Some, a, b) := [(a, 1/3[a,b], 2/3[a,b], b)]
  | None         := []
  };

geo_draw_superarc bounds superness centre rad_a rad_b :=
  geo_clip_spline_to_rect bounds
    ((path_add_out_dir rad_b >>
      path_add_out_tension ~1 >>
      path_add_in_dir (rad_a - rad_b) >>
      path_add_in_tension ~1 >>
      path_add_point (centre + superness * (rad_a + rad_b)) >>
      path_add_out_tension ~1 >>
      path_add_in_dir ~rad_a >>
      path_add_in_tension ~1 >>
      path_add_point (centre + rad_b))
     (make_path (centre + rad_a)));

geo_draw bounds p=(_,_) :=
  if geo_in_bounds bounds p then
    [(p,p,p,p)]
  else
    []
  end;

geo_draw bounds (Segment, a, b) := geo_draw_line bounds a b;

geo_draw bounds=(min_x,min_y,max_x,max_y) (Line, (px,py), (dx,dy)) :=
  if dx == 0 then
    geo_draw_line bounds (px, min_y) (px, max_y)
  else
    local s t;
    begin
      px + s * dx = min_x,
      px + t * dx = max_x,
      geo_draw_line bounds (px + s * dx, py + s * dy)
                           (px + t * dx, py + t * dy)
    end
  end;

geo_draw bounds (Ellipse, c, ra, rb) :=
  geo_draw_superarc bounds (1/ sqrt 2) c  ra  rb +
  geo_draw_superarc bounds (1/ sqrt 2) c  rb ~ra +
  geo_draw_superarc bounds (1/ sqrt 2) c ~ra ~rb +
  geo_draw_superarc bounds (1/ sqrt 2) c ~rb  ra;

geo_draw bounds (Polygon, [p:ps]) := iter p ps
  where
    iter a []     := geo_draw_line a p;
    iter a [b:bs] := geo_draw_line a b + iter b bs;
  end;


;; altitude : point -> point -> point -> line
;; homothecy : circle -> point -> num -> circle
;; homothecy : conic -> point -> num -> conic
;; homothecy : line -> point -> num -> line
;; homothecy : point -> point -> num -> point;
;; homothecy : segment -> point -> number -> segment
;; intersection : circle -> circle -> (var * var)
;; intersection : line -> circle -> (var * var)
;; intersection : line -> conic -> (var * var)
;; intersection : line -> line -> point;
;; median : point -> point -> point -> line
;; orthocentre : point -> point -> point -> point;
;; vertices : conic -> (var * var)
;; bisector : segment -> line
;; tangent : xxx -> line

;; vim:set ft=al:
