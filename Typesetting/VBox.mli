
open XNum;
open Runtime;
open Dim;
open Box;

value calc_vert_dimensions : list box -> (dim * xdim * dim);
value calc_height          : list box -> (dim * dim);

value layout_scaled   : (num * int) -> list box -> box;
value to_top          : box -> box;

value make            : list box -> box;
value make_to         : num -> list box -> box;
value make_scaled     : num -> list box -> box;
value make_spread     : num -> list box -> box;
value make_top        : list box -> box;
value make_top_to     : num -> list box -> box;
value make_top_scaled : num -> list box -> box;
value make_top_spread : num -> list box -> box;

