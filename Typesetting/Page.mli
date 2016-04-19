
open XNum;
open Runtime;
open Unicode.Types;
open Logging;
open Dim;
open Box;

type page =
{
  p_width  : num;                       (* dimensions *)
  p_height : num;
  p_used   : PlanePartition.map unit;   (* area covered by boxes *)
  p_boxes  : list (num * num * box)     (* list of boxes and their position *)
};

type area_shape =                       (* an area of the page                          *)
{
  as_pos_x  : num;                      (* position of the base line                    *)
  as_pos_y  : num;
  as_width  : num;
  as_height : num;                      (* distance between first and last baseline     *)
  as_top    : num;                      (* maximal amount the area might extend above   *)
  as_bottom : num                       (* the first and below the last baseline        *)
};

value new_page        : num -> num -> page;
value allocate_rect   : page -> num -> num -> num -> num -> page;
value put_box         : page -> num -> num -> box -> page;
value put_box_on_page : page -> num -> num -> box -> page;

value find_place_in_area_top    : page -> area_shape -> dim -> xdim -> dim -> option (num * (num * int));
value find_place_in_area_bottom : page -> area_shape -> dim -> xdim -> dim -> option (num * (num * int));
value find_place_in_area_left   : page -> area_shape -> dim -> option (num * (num * int));
value find_place_in_area_right  : page -> area_shape -> dim -> option (num * (num * int));

value area_free_vert  : page -> area_shape -> list (num * num);
value area_free_horiz : page -> area_shape -> list (num * num);

