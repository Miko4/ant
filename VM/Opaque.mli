
exception Type_error;

type opaque 'a;

value declare_type : string -> ('b -> 'a -> 'a) -> ('b -> 'b -> bool) -> ('b -> 'b -> bool) ->
                     (('b -> opaque 'a) * (opaque 'a -> 'b));
value type_name    : opaque 'a -> string;
value same_type    : opaque 'a -> opaque 'a -> bool;
value apply        : opaque 'a -> 'a -> 'a;
value compare      : opaque 'a -> opaque 'a -> bool;
value unify        : opaque 'a -> opaque 'a -> bool;

