[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

val are_equal_regs : Reg.t -> Reg.t -> bool

val go_back_const : int

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells :
  Cfg.basic Cfg.instruction DLL.cell ->
  int ->
  Cfg.basic Cfg.instruction DLL.cell list

val is_bitwise_op : Mach.integer_operation -> bool

val bitwise_overflow_assert : int -> int -> (int -> int -> int) -> unit

val no_32_bit_overflow : int -> int -> (int -> int -> int) -> bool

type rule =
  Cfg.basic Cfg.instruction DLL.cell ->
  Cfg.basic Cfg.instruction DLL.cell option
