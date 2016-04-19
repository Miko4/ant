
module type OrderedType = Map.OrderedType;

module type S =
sig
  type elt;

  type t 'a;

  value empty                 : t 'a;
  value is_empty              : t 'a -> bool;
  value prefix                : t 'a -> elt -> t 'a;
  value root_value            : t 'a -> option 'a;
  value depth                 : t 'a -> int;

  value find_array            : array elt -> t 'a -> 'a;
  value mem_array             : array elt -> t 'a -> bool;
  value find_list             : list  elt -> t 'a -> 'a;
  value mem_list              : list  elt -> t 'a -> bool;
  value generic_lookup        : ((t 'a -> elt -> t 'a) -> t 'a -> 'b -> t 'a) ->
                                'b -> t 'a -> option 'a;
  value generic_lookup_prefix : (((t 'a * t 'a) -> elt -> (t 'a * t 'a)) ->
                                 (t 'a * t 'a) -> 'b -> (t 'a * t 'a)) ->
                                'b -> t 'a -> option 'a;
  value lookup_array          : array elt -> t 'a -> option 'a;
  value lookup_list           : list  elt -> t 'a -> option 'a;
  value lookup_prefix_array   : array elt -> t 'a -> option 'a;
  value lookup_prefix_list    : list  elt -> t 'a -> option 'a;
  value add_array             : array elt -> 'a -> t 'a -> t 'a;
  value remove_array          : array elt -> t 'a -> t 'a;
  value add_list              : list  elt -> 'a -> t 'a -> t 'a;
  value remove_list           : list  elt -> t 'a -> t 'a;
  value merge                 : t 'a -> t 'a -> t 'a;
  value map                   : ('a -> 'b) -> t 'a -> t 'b;
  value mapi                  : (array elt -> 'a -> 'b) -> t 'a -> t 'b;
  value iter                  : (array elt -> 'a -> unit) -> t 'a -> unit;
  value fold                  : (array elt -> 'a -> 'b -> 'b) -> t 'a -> 'b -> 'b;
end;

module Make(Ord: OrderedType) : S with type elt = Ord.t;

