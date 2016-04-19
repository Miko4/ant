
open XNum;

type update_solution 'graph 'aux 'solution    = 'graph -> list 'solution -> 'aux -> int -> int -> list 'solution
                                                 -> (list 'solution * option 'aux);
type is_forced       'graph                   = 'graph -> int -> bool;
type compute_path    'graph 'solution 'result = 'graph -> array (list 'solution) -> 'result;

value find_shortest_path :
  update_solution 'graph 'aux 'solution ->
  is_forced       'graph ->
  compute_path    'graph 'solution 'result ->
  'solution -> 'aux -> 'graph -> int -> 'result;

