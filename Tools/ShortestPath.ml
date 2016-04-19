
open XNum;

type update_solution 'graph 'aux 'solution    = 'graph -> list 'solution -> 'aux -> int -> int -> list 'solution
                                                 -> (list 'solution * option 'aux);
type is_forced       'graph                   = 'graph -> int -> bool;
type compute_path    'graph 'solution 'result = 'graph -> array (list 'solution) -> 'result;

(*
  |find_shortest_way <update-solutions> <is-forced> <compute-path>
    <initial-solution> <graph> <num-vertices>|

  computes the shortest path in a graph with <num-vertices> vertices
  from the first vertex to the last one. It is assumed that the graph
  is directed with an edge between <i> and <k> iff |i < k|. For each
  vertex <i>, a list of possible shortest paths from the first vertex
  to <i> is computed.

  |update_solutions <graph> <previous-solutions> <aux> <previous>
    <current> <current-solutions>|

  updates the solutions of the <current> vertex by adding possible paths
  from the <previous> vertex.

  |is-forced <graph> <i>| should return |True| if any path is forced to
  visit vertex <i>. If <i> might be skipped it returns |False|.

  |compute_path <graph> <solutions>| computes the resulting shortest
  path from the array of partial solutions. Usually, it will select
  one element of the list |solutions.(num-vertices - 1)|.

  <initial-solution> is the data associated to the path from the first
  vertex to itself.

  <graph> contains data associated with the graph.

  <num-vertices> is the number of vertices of <graph>.
*)

value find_shortest_path
        update_solution
        is_forced
        compute_path
        initial_solution
        initial_data
        graph
        num_vertices =
do
{
  let solutions = Array.make num_vertices [];
  let aux_data  = Array.make num_vertices (Some initial_data);

  solutions.(0) := [initial_solution];

  (* Compute all shortest paths from the first vertex to vertex <current>. *)
  let rec fill_array current = do
  {
    if current >= num_vertices then
      compute_path graph solutions
    else
      iter_active (current - 1) current
  }
  and fill_next_vertex current = do
  {
    (* Mark the current vertex as inactive if we have found no solutions. *)
    match solutions.(current) with
    [ [] -> aux_data.(current) := None
    | _  -> ()
    ];

    fill_array (current + 1)
  }
  and iter_active prev current = do
  {
    if prev < 0 then do
    {
      (* Mark the current vertex as inactive if we have found no solutions. *)
      match solutions.(current) with
      [ [] -> aux_data.(current) := None
      | _  -> ()
      ];

      fill_next_vertex current
    }
    else match aux_data.(prev) with
    [ None     -> iter_active (prev - 1) current
    | Some aux -> process_active_vertex aux prev current
    ]
  }
  (* Consider the edge from <previous> to <current>. *)
  and process_active_vertex aux previous current = do
  {
    let (new_sol, new_aux) = update_solution graph solutions.(previous) aux previous current solutions.(current);

    solutions.(current) := new_sol;
    aux_data.(previous) := new_aux;

    if is_forced graph previous then
      (* Do not scan over a forced vertex. *)
      fill_next_vertex current
    else
      iter_active (previous - 1) current
  };

  fill_array 1
};

