
open XNum;
open Runtime;
open Dim;
open Box;

type direction = [ LR | RL ];

value dimensions          : list box -> (dim * dim * dim);
value calc_xwidth         : list box -> xdim;
value calc_width          : list box -> dim;
value calc_width_and_glue : list box -> (xdim * list xdim);

value make        : direction -> list box -> box;
value make_to     : direction -> num -> list box -> box;
value make_scaled : direction -> num -> list box -> box;
value make_spread : direction -> num -> list box -> box;

