
(* maybe : 'b -> ('a -> 'b) -> 'a optional -> 'b *)

value maybe n f x = match x with
[ Some y -> f y
| None   -> n
];

(* is_some : 'a option -> bool *)

value is_some x = match x with
[ Some _ -> True
| None   -> False
];

(* is_none : 'a option -> bool *)

value is_none x = match x with
[ Some _ -> False
| None   -> True
];

(* from_some : 'a option -> 'a *)

value from_some x = match x with
[ Some v -> v
| None   -> failwith "from_some"
];

(* from_option : 'a -> 'a option -> 'a *)

value from_option d x = match x with
[ None   -> d
| Some v -> v
];

(* compare : 'a -> 'a option -> bool *)

value compare x y = match y with
[ None   -> False
| Some v -> x = v
];

(* compareq : 'a -> 'a option -> bool *)

value compareq x y = match y with
[ None   -> False
| Some v -> x == v
];

(* to_list : 'a option -> 'a list *)

value to_list x = match x with
[ Some v -> [v]
| None   -> []
];

(* from_list : 'a list -> 'a option *)

value from_list l = match l with
[ []       -> None
| [x :: _] -> Some x
];

(* concat : 'a option list -> 'a list *)

value rec concat l = match l with
[ []             -> []
| [None :: xs]   -> concat xs
| [Some x :: xs] -> [x :: concat xs]
];

(* map : ('a -> 'b option) -> 'a list -> 'b list *)

value map f l = concat (List.map f l);
