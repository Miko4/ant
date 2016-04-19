
open Types;

value make_path          : unknown -> partial_value;
value close_path         : unknown -> unknown -> partial_value;
value add_point          : unknown -> unknown -> partial_value;
value add_in_dir         : unknown -> unknown -> partial_value;
value add_in_angle       : unknown -> unknown -> partial_value;
value add_in_curl        : unknown -> unknown -> partial_value;
value add_in_tension     : unknown -> unknown -> partial_value;
value add_out_dir        : unknown -> unknown -> partial_value;
value add_out_angle      : unknown -> unknown -> partial_value;
value add_out_curl       : unknown -> unknown -> partial_value;
value add_out_tension    : unknown -> unknown -> partial_value;
value add_control_points : list unknown -> partial_value;

