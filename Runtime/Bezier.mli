
open XNum;

type spline = (num * num * num * num * num * num * num * num);

type path_spec;

value angle_of_vec : num -> num -> float;

value make_spec          : num -> num -> path_spec;
value close_spec         : path_spec -> bool -> array spline;

value add_point          : path_spec -> num -> num -> path_spec;
value add_in_dir         : path_spec -> float -> path_spec;
value add_in_curl        : path_spec -> num -> path_spec;
value add_in_tension     : path_spec -> num -> path_spec;
value add_out_dir        : path_spec -> float -> path_spec;
value add_out_curl       : path_spec -> num -> path_spec;
value add_out_tension    : path_spec -> num -> path_spec;
value add_control_points : path_spec -> num -> num -> num -> num -> path_spec;

