
open ParseState;

value init_source_specials : Engine.Job.job -> unit;

value begin_paragraph : parse_state -> unit;
value end_paragraph   : parse_state -> unit;
value begin_math      : parse_state -> unit;
value end_math        : parse_state -> unit;
value set_mode        : parse_state -> mode -> bool;
value ensure_par_mode : parse_state -> unit;
value leave_par_mode  : parse_state -> unit;

