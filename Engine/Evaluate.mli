
open XNum;
open Runtime;
open Unicode.Types;
open Typesetting;
open Environment;

value tracing_engine : ref bool;

value const_pt        : num -> skip_arg;
value const_em        : num -> skip_arg;
value const_ex        : num -> skip_arg;
value const_mu        : num -> skip_arg;
value const_fixed_dim : skip_arg -> dim_arg;

value eval_node_list : environment -> Builder.builder_interface -> list Node.node -> environment;
value evaluate       : list Node.node -> list FontMetric.page;

