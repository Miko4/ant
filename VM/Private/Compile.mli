
open Types;
open Runtime;

value compile_declarations : Scope.scope -> UCStream.istream -> array bytecode;
value compile_expression   : Scope.scope -> UCStream.istream -> array bytecode;

