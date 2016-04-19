
open XNum;

type map 'a;

type interval = (num * num);

value empty            : num -> num -> num -> num -> map 'a;
value get_value        : map 'a -> num -> num -> option 'a;
value set_rect         : map 'a -> num -> num -> num -> num -> 'a -> map 'a;
value clear_rect       : map 'a -> num -> num -> num -> num -> map 'a;
value horiz_strip      : map 'a -> num -> num -> list interval;
value vert_strip       : map 'a -> num -> num -> list interval;
value find_free_top    : map 'a -> num -> num -> num -> num -> num -> option interval;
value find_free_bottom : map 'a -> num -> num -> num -> num -> num -> option interval;
value find_free_left   : map 'a -> num -> num -> num -> num -> num -> option interval;
value find_free_right  : map 'a -> num -> num -> num -> num -> num -> option interval;

value find_first_free_interval : (num -> num -> bool) -> num -> num -> list interval -> option interval;
value find_last_free_interval  : (num -> num -> bool) -> num -> num -> list interval -> option interval;

