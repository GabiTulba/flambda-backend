(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-40-41-42-44-45"]

module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm
open Arch

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
  | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ -> fn arg
  | _ ->
    let id = V.create_local name in
    Clet (VP.create id, arg, fn (Cvar id))

let bind_load name arg fn =
  match arg with Cop (Cload _, [Cvar _], _) -> fn arg | _ -> bind name arg fn

let bind_nonvar name arg fn =
  match arg with
  | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ -> fn arg
  | _ ->
    let id = V.create_local name in
    Clet (VP.create id, arg, fn (Cvar id))

let bind_list name args fn =
  let rec aux bound_args = function
    | [] -> fn bound_args
    | arg :: args ->
      bind name arg (fun bound_arg -> aux (bound_arg :: bound_args) args)
  in
  aux [] (List.rev args)

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8

let caml_local = Nativeint.shift_left (Nativeint.of_int 2) 8
(* cf. runtime/caml/gc.h *)

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

let block_header tag sz =
  Nativeint.add
    (Nativeint.shift_left (Nativeint.of_int sz) 10)
    (Nativeint.of_int tag)

(* Static data corresponding to "value"s must be marked black in case we are in
   no-naked-pointers mode. See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black

let local_block_header tag sz = Nativeint.logor (block_header tag sz) caml_local

let white_closure_header sz = block_header Obj.closure_tag sz

let black_closure_header sz = black_block_header Obj.closure_tag sz

let local_closure_header sz = local_block_header Obj.closure_tag sz

let infix_header ofs = block_header Obj.infix_tag ofs

let float_header = block_header Obj.double_tag (size_float / size_addr)

let float_local_header =
  local_block_header Obj.double_tag (size_float / size_addr)

let boxedvec128_header = block_header Obj.abstract_tag (size_vec128 / size_addr)

let boxedvec128_local_header =
  local_block_header Obj.abstract_tag (size_vec128 / size_addr)

let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0
  then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)

let string_header len =
  block_header Obj.string_tag ((len + size_addr) / size_addr)

let boxedint32_header = block_header Obj.custom_tag 2

let boxedint64_header = block_header Obj.custom_tag (1 + (8 / size_addr))

let boxedintnat_header = block_header Obj.custom_tag 2

let boxedint32_local_header = local_block_header Obj.custom_tag 2

let boxedint64_local_header =
  local_block_header Obj.custom_tag (1 + (8 / size_addr))

let boxedintnat_local_header = local_block_header Obj.custom_tag 2

let caml_nativeint_ops = "caml_nativeint_ops"

let caml_int32_ops = "caml_int32_ops"

let caml_int64_ops = "caml_int64_ops"

let pos_arity_in_closinfo = (8 * size_addr) - 8
(* arity = the top 8 bits of the closinfo word *)

let pack_closure_info ~arity ~startenv ~is_last =
  assert (-128 <= arity && arity <= 127);
  assert (0 <= startenv && startenv < 1 lsl (pos_arity_in_closinfo - 2));
  Nativeint.(
    add
      (shift_left (of_int arity) pos_arity_in_closinfo)
      (add
         (shift_left
            (Bool.to_int is_last |> Nativeint.of_int)
            (pos_arity_in_closinfo - 1))
         (add (shift_left (of_int startenv) 1) 1n)))

let closure_info' ~arity ~startenv ~is_last =
  let arity =
    match arity with
    | Lambda.Tupled, l -> -List.length l
    | Lambda.Curried _, l -> List.length l
  in
  pack_closure_info ~arity ~startenv ~is_last

let closure_info ~(arity : Clambda.arity) ~startenv ~is_last =
  closure_info'
    ~arity:(arity.function_kind, arity.params_layout)
    ~startenv ~is_last

let alloc_float_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (float_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (float_local_header, dbg)

let alloc_boxedvector_header vi mode dbg =
  let header, local_header =
    match vi with
    | Primitive.Pvec128 -> boxedvec128_header, boxedvec128_local_header
  in
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (local_header, dbg)

let alloc_floatarray_header len dbg = Cconst_natint (floatarray_header len, dbg)

let alloc_closure_header ~mode sz dbg =
  match (mode : Lambda.alloc_mode) with
  | Alloc_heap -> Cconst_natint (white_closure_header sz, dbg)
  | Alloc_local -> Cconst_natint (local_closure_header sz, dbg)

let alloc_infix_header ofs dbg = Cconst_natint (infix_header ofs, dbg)

let alloc_closure_info ~arity ~startenv ~is_last dbg =
  Cconst_natint (closure_info ~arity ~startenv ~is_last, dbg)

let alloc_boxedint32_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedint32_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedint32_local_header, dbg)

let alloc_boxedint64_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedint64_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedint64_local_header, dbg)

let alloc_boxedintnat_header mode dbg =
  match mode with
  | Lambda.Alloc_heap -> Cconst_natint (boxedintnat_header, dbg)
  | Lambda.Alloc_local -> Cconst_natint (boxedintnat_local_header, dbg)

(* Integers *)

let max_repr_int = max_int asr 1

let min_repr_int = min_int asr 1

let int_const dbg n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int ((n lsl 1) + 1, dbg)
  else
    Cconst_natint
      (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n, dbg)

let natint_const_untagged dbg n =
  if n > Nativeint.of_int max_int || n < Nativeint.of_int min_int
  then Cconst_natint (n, dbg)
  else Cconst_int (Nativeint.to_int n, dbg)

let cint_const n =
  Cint (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let targetint_const n =
  Targetint.add (Targetint.shift_left (Targetint.of_int n) 1) Targetint.one

let add_no_overflow n x c dbg =
  let d = n + x in
  if d = 0 then c else Cop (Caddi, [c; Cconst_int (d, dbg)], dbg)

let rec add_const c n dbg =
  if n = 0
  then c
  else
    match c with
    | Cconst_int (x, _) when Misc.no_overflow_add x n -> Cconst_int (x + n, dbg)
    | Cop (Caddi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
    | Cop (Caddi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
    | Cop (Csubi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      Cop (Csubi, [Cconst_int (n + x, dbg); c], dbg)
    | Cop (Csubi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_sub n x ->
      add_const c (n - x) dbg
    | c -> Cop (Caddi, [c; Cconst_int (n, dbg)], dbg)

let incr_int c dbg = add_const c 1 dbg

let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  match c1, c2 with
  | Cconst_int (n, _), c | c, Cconst_int (n, _) -> add_const c n dbg
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
    add_const (add_int c1 c2 dbg) n1 dbg
  | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) ->
    add_const (add_int c1 c2 dbg) n2 dbg
  | _, _ -> Cop (Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  match c1, c2 with
  | c1, Cconst_int (n2, _) when n2 <> min_int -> add_const c1 (-n2) dbg
  | c1, Cop (Caddi, [c2; Cconst_int (n2, _)], _) when n2 <> min_int ->
    add_const (sub_int c1 c2 dbg) (-n2) dbg
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), c2 ->
    add_const (sub_int c1 c2 dbg) n1 dbg
  | c1, c2 -> Cop (Csubi, [c1; c2], dbg)

let neg_int c dbg = sub_int (Cconst_int (0, dbg)) c dbg

let rec lsl_int c1 c2 dbg =
  match c1, c2 with
  | Cop (Clsl, [c; Cconst_int (n1, _)], _), Cconst_int (n2, _)
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
    Cop (Clsl, [c; Cconst_int (n1 + n2, dbg)], dbg)
  | Cop (Caddi, [c1; Cconst_int (n1, _)], _), Cconst_int (n2, _)
    when Misc.no_overflow_lsl n1 n2 ->
    add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | _, _ -> Cop (Clsl, [c1; c2], dbg)

let is_power2 n = n = 1 lsl Misc.log2 n

and mult_power2 c n dbg = lsl_int c (Cconst_int (Misc.log2 n, dbg)) dbg

let rec mul_int c1 c2 dbg =
  match c1, c2 with
  | c, Cconst_int (0, _) | Cconst_int (0, _), c ->
    Csequence (c, Cconst_int (0, dbg))
  | c, Cconst_int (1, _) | Cconst_int (1, _), c -> c
  | c, Cconst_int (-1, _) | Cconst_int (-1, _), c ->
    sub_int (Cconst_int (0, dbg)) c dbg
  | c, Cconst_int (n, _) when is_power2 n -> mult_power2 c n dbg
  | Cconst_int (n, _), c when is_power2 n -> mult_power2 c n dbg
  | Cop (Caddi, [c; Cconst_int (n, _)], _), Cconst_int (k, _)
  | Cconst_int (k, _), Cop (Caddi, [c; Cconst_int (n, _)], _)
    when Misc.no_overflow_mul n k ->
    add_const (mul_int c (Cconst_int (k, dbg)) dbg) (n * k) dbg
  | c1, c2 -> Cop (Cmuli, [c1; c2], dbg)

let ignore_low_bit_int = function
  | Cop
      ( Caddi,
        [(Cop (Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)],
        _ )
    when n > 0 ->
    c
  | Cop (Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

(* removes the 1-bit sign-extension left by untag_int (tag_int c) *)
let ignore_high_bit_int = function
  | Cop (Casr, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
    c
  | c -> c

let lsr_int c1 c2 dbg =
  match c2 with
  | Cconst_int (0, _) -> c1
  | Cconst_int (n, _) when n > 0 -> Cop (Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ -> Cop (Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  match c2 with
  | Cconst_int (0, _) -> c1
  | Cconst_int (n, _) when n > 0 -> Cop (Casr, [ignore_low_bit_int c1; c2], dbg)
  | _ -> Cop (Casr, [c1; c2], dbg)

let tag_int i dbg =
  match i with
  | Cconst_int (n, _) -> int_const dbg n
  | Cop (Casr, [c; Cconst_int (n, _)], _) when n > 0 ->
    Cop
      (Cor, [asr_int c (Cconst_int (n - 1, dbg)) dbg; Cconst_int (1, dbg)], dbg)
  | c -> incr_int (lsl_int c (Cconst_int (1, dbg)) dbg) dbg

let untag_int i dbg =
  match i with
  | Cconst_int (n, _) -> Cconst_int (n asr 1, dbg)
  | Cop (Cor, [Cop (Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < (size_int * 8) - 1 ->
    Cop (Casr, [c; Cconst_int (n + 1, dbg)], dbg)
  | Cop (Cor, [Cop (Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < (size_int * 8) - 1 ->
    Cop (Clsr, [c; Cconst_int (n + 1, dbg)], dbg)
  | c -> asr_int c (Cconst_int (1, dbg)) dbg

let mk_if_then_else dbg value_kind cond ifso_dbg ifso ifnot_dbg ifnot =
  match cond with
  | Cconst_int (0, _) -> ifnot
  | Cconst_int (1, _) -> ifso
  | _ -> Cifthenelse (cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg, value_kind)

let mk_not dbg cmm =
  match cmm with
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg')
    -> (
    match c with
    | Cop (Ccmpi cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpa cmp, [c1; c2], dbg'') ->
      tag_int
        (Cop (Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg''))
        dbg'
    | Cop (Ccmpf cmp, [c1; c2], dbg'') ->
      tag_int (Cop (Ccmpf (negate_float_comparison cmp), [c1; c2], dbg'')) dbg'
    | _ ->
      (* 0 -> 3, 1 -> 1 *)
      Cop
        ( Csubi,
          [Cconst_int (3, dbg); Cop (Clsl, [c; Cconst_int (1, dbg)], dbg)],
          dbg ))
  | Cconst_int (3, _) -> Cconst_int (1, dbg)
  | Cconst_int (1, _) -> Cconst_int (3, dbg)
  | c ->
    (* 1 -> 3, 3 -> 1 *)
    Cop (Csubi, [Cconst_int (4, dbg); c], dbg)

let mk_compare_ints_untagged dbg a1 a2 =
  bind "int_cmp" a2 (fun a2 ->
      bind "int_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpi Cgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpi Clt, [a1; a2], dbg) in
          sub_int op1 op2 dbg))

let mk_compare_ints dbg a1 a2 =
  match a1, a2 with
  | Cconst_int (c1, _), Cconst_int (c2, _) -> int_const dbg (Int.compare c1 c2)
  | Cconst_natint (c1, _), Cconst_natint (c2, _) ->
    int_const dbg (Nativeint.compare c1 c2)
  | Cconst_int (c1, _), Cconst_natint (c2, _) ->
    int_const dbg Nativeint.(compare (of_int c1) c2)
  | Cconst_natint (c1, _), Cconst_int (c2, _) ->
    int_const dbg Nativeint.(compare c1 (of_int c2))
  | a1, a2 -> tag_int (mk_compare_ints_untagged dbg a1 a2) dbg

let mk_compare_floats_untagged dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
      bind "float_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpf CFgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpf CFlt, [a1; a2], dbg) in
          let op3 = Cop (Ccmpf CFeq, [a1; a1], dbg) in
          let op4 = Cop (Ccmpf CFeq, [a2; a2], dbg) in
          (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1, and the
             result is op1 - op2.

             If at least one of the operands is NaN, then op1 = op2 = 0, and the
             result is op3 - op4, which orders NaN before other values.

             To detect if the operand is NaN, we use the property:

             for all x, NaN is not equal to x, even if x is NaN.

             Therefore, op3 is 0 if and only if a1 is NaN, and op4 is 0 if and
             only if a2 is NaN. See also caml_float_compare_unboxed in
             runtime/floats.c *)
          add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg))

let mk_compare_floats dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
      bind "float_cmp" a1 (fun a1 ->
          let op1 = Cop (Ccmpf CFgt, [a1; a2], dbg) in
          let op2 = Cop (Ccmpf CFlt, [a1; a2], dbg) in
          let op3 = Cop (Ccmpf CFeq, [a1; a1], dbg) in
          let op4 = Cop (Ccmpf CFeq, [a2; a2], dbg) in
          (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1, and the
             result is op1 - op2.

             If at least one of the operands is NaN, then op1 = op2 = 0, and the
             result is op3 - op4, which orders NaN before other values.

             To detect if the operand is NaN, we use the property: for all x,
             NaN is not equal to x, even if x is NaN.

             Therefore, op3 is 0 if and only if a1 is NaN, and op4 is 0 if and
             only if a2 is NaN.

             See also caml_float_compare_unboxed in runtime/floats.c *)
          tag_int (add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg) dbg))

let create_loop body dbg =
  let cont = Lambda.next_raise_count () in
  let call_cont = Cexit (Lbl cont, [], []) in
  let body = Csequence (body, call_cont) in
  Ccatch (Recursive, [cont, [], body, dbg], call_cont, Any)

(* Turning integer divisions into multiply-high then shift. The
   [division_parameters] function is used in module Emit for those target
   platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint. Algorithm: Hacker's Delight
   section 9.3 *)

let udivmod n d =
  Nativeint.(
    if d < 0n
    then if ucompare n d < 0 then 0n, n else 1n, sub n d
    else
      let q = shift_left (div (shift_right_logical n 1) d) 1 in
      let r = sub n (mul q d) in
      if ucompare r d >= 0 then succ q, sub r d else q, r)

(* Compute division parameters. Algorithm: Hacker's Delight chapter 10, fig
   10-1. *)

let divimm_parameters d =
  Nativeint.(
    assert (d > 0n);
    let twopsm1 = min_int in
    (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
    let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
    let rec loop p (q1, r1) (q2, r2) =
      let p = p + 1 in
      let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
      let q1, r1 = if ucompare r1 nc >= 0 then succ q1, sub r1 nc else q1, r1 in
      let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
      let q2, r2 = if ucompare r2 d >= 0 then succ q2, sub r2 d else q2, r2 in
      let delta = sub d r2 in
      if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
      then loop p (q1, r1) (q2, r2)
      else succ q2, p - size
    in
    loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

   2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1) (i)

   from which it follows that

   floor(n / d) = floor(n * m / 2^(wordsize+p)), if 0 <= n < 2^(wordsize-1)

   ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1, if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below. It was
   exhaustively tested for values of d from 2 to 10^9 in the wordsize = 64
   case.

 * let add2 (xh, xl) (yh, yl) =
 *   let zl = add xl yl and zh = add xh yh in
 *   (if ucompare zl xl < 0 then succ zh else zh), zl
 *
 * let shl2 (xh, xl) n =
 *   assert (0 < n && n < size + size);
 *   if n < size
 *   then
 *     logor (shift_left xh n) (shift_right_logical xl (size - n)),
 *       shift_left xl n
 *   else shift_left xl (n - size), 0n
 *
 * let mul2 x y =
 *   let halfsize = size / 2 in
 *   let halfmask = pred (shift_left 1n halfsize) in
 *   let xl = logand x halfmask and xh = shift_right_logical x halfsize in
 *   let yl = logand y halfmask and yh = shift_right_logical y halfsize in
 *   add2
 *     (mul xh yh, 0n)
 *     (add2
 *        (shl2 (0n, mul xl yh) halfsize)
 *        (add2 (shl2 (0n, mul xh yl) halfsize) (0n, mul xl yl)))
 *
 * let ucompare2 (xh, xl) (yh, yl) =
 *   let c = ucompare xh yh in
 *   if c = 0 then ucompare xl yl else c
 *
 * let validate d m p =
 *   let md = mul2 m d in
 *   let one2 = 0n, 1n in
 *   let twoszp = shl2 one2 (size + p) in
 *   let twop1 = shl2 one2 (p + 1) in
 *   ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
 *)

let raise_symbol dbg symb =
  Cop
    (Craise Lambda.Raise_regular, [Cconst_symbol (global_symbol symb, dbg)], dbg)

let rec div_int c1 c2 is_safe dbg =
  match c1, c2 with
  | c1, Cconst_int (0, _) ->
    Csequence (c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | c1, Cconst_int (1, _) -> c1
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Cconst_int (n1 / n2, dbg)
  | c1, Cconst_int (n, _) when n <> min_int ->
    let l = Misc.log2 n in
    if n = 1 lsl l
    then
      (* Algorithm:

         t = shift-right-signed(c1, l - 1)

         t = shift-right(t, W - l)

         t = c1 + t res = shift-right-signed(c1 + t, l) *)
      Cop
        ( Casr,
          [ bind "dividend" c1 (fun c1 ->
                assert (l >= 1);
                let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
                let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
                add_int c1 t dbg);
            Cconst_int (l, dbg) ],
          dbg )
    else if n < 0
    then
      sub_int
        (Cconst_int (0, dbg))
        (div_int c1 (Cconst_int (-n, dbg)) is_safe dbg)
        dbg
    else
      let m, p = divimm_parameters (Nativeint.of_int n) in
      (* Algorithm:

         t = multiply-high-signed(c1, m) if m < 0,

         t = t + c1 if p > 0,

         t = shift-right-signed(t, p)

         res = t + sign-bit(c1) *)
      bind "dividend" c1 (fun c1 ->
          let t =
            Cop
              (Cmulhi { signed = true }, [c1; natint_const_untagged dbg m], dbg)
          in
          let t = if m < 0n then Cop (Caddi, [t; c1], dbg) else t in
          let t =
            if p > 0 then Cop (Casr, [t; Cconst_int (p, dbg)], dbg) else t
          in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1, dbg)) dbg) dbg)
  | c1, c2 when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
    Cop (Cdivi, [c1; c2], dbg)
  | c1, c2 ->
    bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
            Cifthenelse
              ( c2,
                dbg,
                Cop (Cdivi, [c1; c2], dbg),
                dbg,
                raise_symbol dbg "caml_exn_Division_by_zero",
                dbg,
                Any )))

let mod_int c1 c2 is_safe dbg =
  match c1, c2 with
  | c1, Cconst_int (0, _) ->
    Csequence (c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | c1, Cconst_int ((1 | -1), _) -> Csequence (c1, Cconst_int (0, dbg))
  | Cconst_int (n1, _), Cconst_int (n2, _) -> Cconst_int (n1 mod n2, dbg)
  | c1, (Cconst_int (n, _) as c2) when n <> min_int ->
    let l = Misc.log2 n in
    if n = 1 lsl l
    then
      (* Algorithm:

         t = shift-right-signed(c1, l - 1)

         t = shift-right(t, W - l)

         t = c1 + t

         t = bit-and(t, -n)

         res = c1 - t *)
      bind "dividend" c1 (fun c1 ->
          assert (l >= 1);
          let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
          let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
          let t = add_int c1 t dbg in
          let t = Cop (Cand, [t; Cconst_int (-n, dbg)], dbg) in
          sub_int c1 t dbg)
    else
      bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 is_safe dbg) c2 dbg) dbg)
  | c1, c2 when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
    (* Flambda already generates that test *)
    Cop (Cmodi, [c1; c2], dbg)
  | c1, c2 ->
    bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
            Cifthenelse
              ( c2,
                dbg,
                Cop (Cmodi, [c1; c2], dbg),
                dbg,
                raise_symbol dbg "caml_exn_Division_by_zero",
                dbg,
                Any )))

(* Division or modulo on boxed integers. The overflow case min_int / -1 can
   occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
  | Cconst_int (n, _) -> n <> x
  | Cconst_natint (n, _) -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop kind is_safe mkm1 c1 c2 bi dbg =
  bind "divisor" c2 (fun c2 ->
      bind "dividend" c1 (fun c1 ->
          let c = mkop c1 c2 is_safe dbg in
          if Arch.division_crashes_on_overflow
             && (size_int = 4 || bi <> Primitive.Pint32)
             && not (is_different_from (-1) c2)
          then
            Cifthenelse
              ( Cop (Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
                dbg,
                c,
                dbg,
                mkm1 c1 dbg,
                dbg,
                kind )
          else c))

let safe_div_bi is_safe =
  safe_divmod_bi div_int Any is_safe (fun c1 dbg ->
      Cop (Csubi, [Cconst_int (0, dbg); c1], dbg))

let safe_mod_bi is_safe =
  safe_divmod_bi mod_int Any is_safe (fun _ dbg -> Cconst_int (0, dbg))

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _)
    ->
    c
  | Cconst_int (n, dbg) ->
    if n = 1 then Cconst_int (0, dbg) else Cconst_int (1, dbg)
  | c -> Cop (Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float dbg m c = Cop (Calloc m, [alloc_float_header m dbg; c], dbg)

let rec unbox_float dbg =
  map_tail ~kind:Any (function
    | Cop (Calloc _, [Cconst_natint (hdr, _); c], _)
      when Nativeint.equal hdr float_header
           || Nativeint.equal hdr float_local_header ->
      c
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name with
      | Some (Uconst_float x) -> Cconst_float (x, dbg) (* or keep _dbg? *)
      | _ -> Cop (Cload (Double, Immutable), [cmm], dbg))
    | Cregion e as cmm -> (
      (* It is valid to push unboxing inside a Cregion except when the extra
         unboxing logic pushes a tail call out of tail position *)
      match
        map_tail ~kind:Any
          (function
            | Cop (Capply (_, Rc_close_at_apply), _, _) -> raise Exit
            | Ctail e -> Ctail (unbox_float dbg e)
            | e -> unbox_float dbg e)
          e
      with
      | e -> Cregion e
      | exception Exit -> Cop (Cload (Double, Immutable), [cmm], dbg))
    | Ctail e -> Ctail (unbox_float dbg e)
    | cmm -> Cop (Cload (Double, Immutable), [cmm], dbg))

(* Vectors *)

let box_vector dbg vi m c =
  Cop (Calloc m, [alloc_boxedvector_header vi m dbg; c], dbg)

let rec unbox_vec128 dbg =
  map_tail ~kind:Any (function
    | Cop (Calloc _, [Cconst_natint (hdr, _); c], _)
      when Nativeint.equal hdr boxedvec128_header
           || Nativeint.equal hdr boxedvec128_local_header ->
      c
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name with
      | Some (Uconst_vec128 { low; high }) ->
        Cconst_vec128 ({ low; high }, dbg) (* or keep _dbg? *)
      | _ -> Cop (Cload (Onetwentyeight, Immutable), [cmm], dbg))
    | Cregion e as cmm -> (
      (* It is valid to push unboxing inside a Cregion except when the extra
         unboxing logic pushes a tail call out of tail position *)
      match
        map_tail ~kind:Any
          (function
            | Cop (Capply (_, Rc_close_at_apply), _, _) -> raise Exit
            | Ctail e -> Ctail (unbox_vec128 dbg e)
            | e -> unbox_vec128 dbg e)
          e
      with
      | e -> Cregion e
      | exception Exit -> Cop (Cload (Onetwentyeight, Immutable), [cmm], dbg))
    | Ctail e -> Ctail (unbox_vec128 dbg e)
    | cmm -> Cop (Cload (Onetwentyeight, Immutable), [cmm], dbg))

let unbox_vector dbg vi e =
  match vi with Primitive.Pvec128 -> unbox_vec128 dbg e

(* Complex *)

let box_complex dbg c_re c_im =
  Cop
    (Calloc Lambda.alloc_heap, [alloc_floatarray_header 2 dbg; c_re; c_im], dbg)

let complex_re c dbg = Cop (Cload (Double, Immutable), [c], dbg)

let complex_im c dbg =
  Cop
    ( Cload (Double, Immutable),
      [Cop (Cadda, [c; Cconst_int (size_float, dbg)], dbg)],
      dbg )

(* Unit *)

let return_unit dbg c = Csequence (c, Cconst_int (1, dbg))

let rec remove_unit = function
  | Cconst_int (1, _) -> Ctuple []
  | Csequence (c, Cconst_int (1, _)) -> c
  | Csequence (c1, c2) -> Csequence (c1, remove_unit c2)
  | Cifthenelse (cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg, kind) ->
    Cifthenelse
      (cond, ifso_dbg, remove_unit ifso, ifnot_dbg, remove_unit ifnot, dbg, kind)
  | Cswitch (sel, index, cases, dbg, kind) ->
    Cswitch
      ( sel,
        index,
        Array.map (fun (case, dbg) -> remove_unit case, dbg) cases,
        dbg,
        kind )
  | Ccatch (rec_flag, handlers, body, kind) ->
    let map_h (n, ids, handler, dbg) = n, ids, remove_unit handler, dbg in
    Ccatch (rec_flag, List.map map_h handlers, remove_unit body, kind)
  | Ctrywith (body, kind, exn, handler, dbg, value_kind) ->
    Ctrywith (remove_unit body, kind, exn, remove_unit handler, dbg, value_kind)
  | Clet (id, c1, c2) -> Clet (id, c1, remove_unit c2)
  | Cop (Capply (_mty, pos), args, dbg) ->
    Cop (Capply (typ_void, pos), args, dbg)
  | Cop (Cextcall c, args, dbg) ->
    Cop (Cextcall { c with ty = typ_void }, args, dbg)
  | Cexit (_, _, _) as c -> c
  | Ctuple [] as c -> c
  | c -> Csequence (c, Ctuple [])

(* Access to block fields *)

let field_address ptr n dbg =
  if n = 0 then ptr else Cop (Cadda, [ptr; Cconst_int (n * size_addr, dbg)], dbg)

let get_field_gen_given_memory_chunk memory_chunk mut ptr n dbg =
  Cop (Cload (memory_chunk, mut), [field_address ptr n dbg], dbg)

let get_field_gen mut ptr n dbg =
  get_field_gen_given_memory_chunk Word_val mut ptr n dbg

let get_field_codepointer mut ptr n dbg =
  get_field_gen_given_memory_chunk Word_int mut ptr n dbg

let set_field ptr n newval init dbg =
  Cop (Cstore (Word_val, init), [field_address ptr n dbg; newval], dbg)

let non_profinfo_mask =
  if Config.profinfo then (1 lsl (64 - Config.profinfo_width)) - 1 else 0
(* [non_profinfo_mask] is unused in this case *)

let get_header ptr dbg =
  (* We cannot deem this as [Immutable] due to the presence of [Obj.truncate]
     and [Obj.set_tag]. *)
  Cop
    ( Cload (Word_int, Mutable),
      [Cop (Cadda, [ptr; Cconst_int (-size_int, dbg)], dbg)],
      dbg )

let get_header_without_profinfo ptr dbg =
  if Config.profinfo
  then Cop (Cand, [get_header ptr dbg; Cconst_int (non_profinfo_mask, dbg)], dbg)
  else get_header ptr dbg

let tag_offset = if big_endian then -1 else -size_int

let get_tag ptr dbg =
  if Proc.word_addressed
  then
    (* If byte loads are slow *)
    Cop (Cand, [get_header ptr dbg; Cconst_int (255, dbg)], dbg)
  else
    (* If byte loads are efficient *)
    (* Same comment as [get_header] above *)
    Cop
      ( Cload (Byte_unsigned, Mutable),
        [Cop (Cadda, [ptr; Cconst_int (tag_offset, dbg)], dbg)],
        dbg )

let get_size ptr dbg =
  Cop (Clsr, [get_header_without_profinfo ptr dbg; Cconst_int (10, dbg)], dbg)

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr

let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9

let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array_hdr hdr dbg =
  Cop
    ( Ccmpi Cne,
      [Cop (Cand, [hdr; Cconst_int (255, dbg)], dbg); floatarray_tag dbg],
      dbg )

let is_addr_array_ptr ptr dbg =
  Cop (Ccmpi Cne, [get_tag ptr dbg; floatarray_tag dbg], dbg)

let addr_array_length_shifted hdr dbg =
  Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)

let float_array_length_shifted hdr dbg =
  Cop (Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg)

let lsl_const c n dbg =
  if n = 0 then c else Cop (Clsl, [c; Cconst_int (n, dbg)], dbg)

(* Produces a pointer to the element of the array [ptr] on the position [ofs]
   with the given element [log2size] log2 element size.

   [ofs] is given as a tagged int expression.

   The optional ?typ argument is the C-- type of the result. By default, it is
   Addr, meaning we are constructing a derived pointer into the heap. If we know
   the pointer is outside the heap (this is the case for bigarray indexing), we
   give type Int instead. *)

let array_indexing ?typ log2size ptr ofs dbg =
  let add =
    match typ with
    | None | Some Addr -> Cadda
    | Some Int -> Caddi
    | _ -> assert false
  in
  match ofs with
  | Cconst_int (n, _) ->
    let i = n asr 1 in
    if i = 0
    then ptr
    else Cop (add, [ptr; Cconst_int (i lsl log2size, dbg)], dbg)
  | Cop (Caddi, [Cop (Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg')
    ->
    Cop (add, [ptr; lsl_const c log2size dbg], dbg')
  | Cop (Caddi, [c; Cconst_int (n, _)], dbg') when log2size = 0 ->
    Cop
      ( add,
        [Cop (add, [ptr; untag_int c dbg], dbg); Cconst_int (n asr 1, dbg)],
        dbg' )
  | Cop (Caddi, [c; Cconst_int (n, _)], _) ->
    Cop
      ( add,
        [ Cop (add, [ptr; lsl_const c (log2size - 1) dbg], dbg);
          Cconst_int ((n - 1) lsl (log2size - 1), dbg) ],
        dbg )
  | _ when log2size = 0 -> Cop (add, [ptr; untag_int ofs dbg], dbg)
  | _ ->
    Cop
      ( add,
        [ Cop (add, [ptr; lsl_const ofs (log2size - 1) dbg], dbg);
          Cconst_int (-1 lsl (log2size - 1), dbg) ],
        dbg )

let addr_array_ref arr ofs dbg =
  Cop
    (Cload (Word_val, Mutable), [array_indexing log2_size_addr arr ofs dbg], dbg)

let int_array_ref arr ofs dbg =
  Cop
    (Cload (Word_int, Mutable), [array_indexing log2_size_addr arr ofs dbg], dbg)

let unboxed_float_array_ref arr ofs dbg =
  Cop
    (Cload (Double, Mutable), [array_indexing log2_size_float arr ofs dbg], dbg)

let float_array_ref mode arr ofs dbg =
  box_float dbg mode (unboxed_float_array_ref arr ofs dbg)

let addr_array_set_heap arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_modify";
          ty = typ_void;
          alloc = false;
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty_args = []
        },
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

let addr_array_set_local arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_modify_local";
          ty = typ_void;
          alloc = false;
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty_args = []
        },
      [arr; untag_int ofs dbg; newval],
      dbg )

let addr_array_set (mode : Lambda.modify_mode) arr ofs newval dbg =
  match mode with
  | Modify_heap -> addr_array_set_heap arr ofs newval dbg
  | Modify_maybe_stack -> addr_array_set_local arr ofs newval dbg

(* int and float arrays can be written to uniformly regardless of their mode *)

let int_array_set arr ofs newval dbg =
  Cop
    ( Cstore (Word_int, Assignment),
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

let float_array_set arr ofs newval dbg =
  Cop
    ( Cstore (Double, Assignment),
      [array_indexing log2_size_float arr ofs dbg; newval],
      dbg )

let addr_array_initialize arr ofs newval dbg =
  Cop
    ( Cextcall
        { func = "caml_initialize";
          builtin = false;
          returns = true;
          effects = Arbitrary_effects;
          coeffects = Has_coeffects;
          ty = typ_void;
          alloc = false;
          ty_args = []
        },
      [array_indexing log2_size_addr arr ofs dbg; newval],
      dbg )

(* Get the field of a block given a possibly inconstant index *)

let get_field_computed imm_or_ptr mut ~block ~index dbg =
  let kind =
    match imm_or_ptr with
    | Lambda.Immediate -> Word_int
    | Lambda.Pointer -> Word_val
  in
  let field_address = array_indexing log2_size_addr block index dbg in
  Cop (Cload (kind, mut), [field_address], dbg)

(* String length *)

(* Length of string block *)

let string_length exp dbg =
  bind "str" exp (fun str ->
      let tmp_var = V.create_local "tmp" in
      Clet
        ( VP.create tmp_var,
          Cop
            ( Csubi,
              [ Cop
                  ( Clsl,
                    [get_size str dbg; Cconst_int (log2_size_addr, dbg)],
                    dbg );
                Cconst_int (1, dbg) ],
              dbg ),
          Cop
            ( Csubi,
              [ Cvar tmp_var;
                Cop
                  ( Cload (Byte_unsigned, Mutable),
                    [Cop (Cadda, [str; Cvar tmp_var], dbg)],
                    dbg ) ],
              dbg ) ))

let bigstring_length ba dbg =
  Cop (Cload (Word_int, Mutable), [field_address ba 5 dbg], dbg)

(* Message sending *)

let lookup_tag obj tag dbg =
  bind "tag" tag (fun tag ->
      Cop
        ( Cextcall
            { func = "caml_get_public_method";
              ty = typ_val;
              builtin = false;
              returns = true;
              effects = Arbitrary_effects;
              coeffects = Has_coeffects;
              alloc = false;
              ty_args = []
            },
          [obj; tag],
          dbg ))

let lookup_label obj lab dbg =
  bind "lab" lab (fun lab ->
      let table = Cop (Cload (Word_val, Mutable), [obj], dbg) in
      addr_array_ref table lab dbg)

module Extended_machtype_component = struct
  type t =
    | Val
    | Addr
    | Tagged_int
    | Any_int
    | Float
    | Vec128

  let of_machtype_component (component : machtype_component) =
    match component with
    | Val -> Val
    | Addr -> Addr
    | Int -> Any_int
    | Float -> Float
    | Vec128 -> Vec128

  let to_machtype_component t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Tagged_int | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128

  let change_tagged_int_to_val t : machtype_component =
    match t with
    | Val -> Val
    | Addr -> Addr
    | Tagged_int -> Val
    | Any_int -> Int
    | Float -> Float
    | Vec128 -> Vec128
end

module Extended_machtype = struct
  type t = Extended_machtype_component.t array

  let typ_val = [| Extended_machtype_component.Val |]

  let typ_tagged_int = [| Extended_machtype_component.Tagged_int |]

  let typ_any_int = [| Extended_machtype_component.Any_int |]

  let typ_int64 = [| Extended_machtype_component.Any_int |]

  let typ_float = [| Extended_machtype_component.Float |]

  let typ_vec128 = [| Extended_machtype_component.Vec128 |]

  let typ_void = [||]

  let of_machtype machtype =
    Array.map Extended_machtype_component.of_machtype_component machtype

  let to_machtype t =
    Array.map Extended_machtype_component.to_machtype_component t

  let change_tagged_int_to_val t =
    Array.map Extended_machtype_component.change_tagged_int_to_val t

  let of_layout (layout : Lambda.layout) =
    match layout with
    | Ptop -> Misc.fatal_error "No Extended_machtype for layout [Ptop]"
    | Pbottom ->
      Misc.fatal_error "No unique Extended_machtype for layout [Pbottom]"
    | Punboxed_float -> typ_float
    | Punboxed_vector Pvec128 -> typ_vec128
    | Punboxed_int _ ->
      (* Only 64-bit architectures, so this is always [typ_int] *)
      typ_any_int
    | Pvalue Pintval -> typ_tagged_int
    | Pvalue _ -> typ_val
end

let machtype_of_layout layout =
  layout |> Extended_machtype.of_layout |> Extended_machtype.to_machtype

let machtype_of_layout_changing_tagged_int_to_val layout =
  layout |> Extended_machtype.of_layout
  |> Extended_machtype.change_tagged_int_to_val

let machtype_identifier t =
  let char_of_component (component : machtype_component) =
    match component with
    | Val -> 'V'
    | Int -> 'I'
    | Float -> 'F'
    | Vec128 -> 'X'
    | Addr ->
      Misc.fatal_error "[Addr] is forbidden inside arity for generic functions"
  in
  String.of_seq (Seq.map char_of_component (Array.to_seq t))

let unique_arity_identifier (arity : Cmm.machtype list) =
  if List.for_all (function [| Val |] -> true | _ -> false) arity
  then Int.to_string (List.length arity)
  else String.concat "_" (List.map machtype_identifier arity)

let send_function_name arity result (mode : Lambda.alloc_mode) =
  let res =
    match result with [| Val |] -> "" | _ -> "_R" ^ machtype_identifier result
  in
  let suff = match mode with Alloc_heap -> "" | Alloc_local -> "L" in
  global_symbol ("caml_send" ^ unique_arity_identifier arity ^ res ^ suff)

let call_cached_method obj tag cache pos args args_type result (apos, mode) dbg
    =
  let cache = array_indexing log2_size_addr cache pos dbg in
  Compilenv.need_send_fun
    (List.map Extended_machtype.change_tagged_int_to_val args_type)
    (Extended_machtype.change_tagged_int_to_val result)
    mode;
  Cop
    ( Capply (Extended_machtype.to_machtype result, apos),
      (* See the cases for caml_apply regarding [change_tagged_int_to_val]. *)
      Cconst_symbol
        ( send_function_name
            (List.map Extended_machtype.change_tagged_int_to_val args_type)
            (Extended_machtype.change_tagged_int_to_val result)
            mode,
          dbg )
      :: obj :: tag :: cache :: args,
      dbg )

(* Allocation *)

let make_alloc_generic ~mode set_fn dbg tag wordsize args =
  (* allocs of size 0 must be statically allocated else the Gc will bug *)
  assert (List.compare_length_with args 0 > 0);
  if Lambda.is_local_mode mode || wordsize <= Config.max_young_wosize
  then
    let hdr =
      match mode with
      | Lambda.Alloc_local -> local_block_header tag wordsize
      | Lambda.Alloc_heap -> block_header tag wordsize
    in
    Cop (Calloc mode, Cconst_natint (hdr, dbg) :: args, dbg)
  else
    let id = V.create_local "*alloc*" in
    let rec fill_fields idx = function
      | [] -> Cvar id
      | e1 :: el ->
        Csequence
          ( set_fn (Cvar id) (Cconst_int (idx, dbg)) e1 dbg,
            fill_fields (idx + 2) el )
    in
    Clet
      ( VP.create id,
        Cop
          ( Cextcall
              { func = "caml_alloc";
                ty = typ_val;
                alloc = true;
                builtin = false;
                returns = true;
                effects = Arbitrary_effects;
                coeffects = Has_coeffects;
                ty_args = []
              },
            [Cconst_int (wordsize, dbg); Cconst_int (tag, dbg)],
            dbg ),
        fill_fields 1 args )

let make_alloc ~mode dbg tag args =
  let addr_array_init arr ofs newval dbg =
    Cop
      ( Cextcall
          { func = "caml_initialize";
            ty = typ_void;
            alloc = false;
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty_args = []
          },
        [array_indexing log2_size_addr arr ofs dbg; newval],
        dbg )
  in
  make_alloc_generic ~mode addr_array_init dbg tag (List.length args) args

let make_float_alloc ~mode dbg tag args =
  make_alloc_generic ~mode float_array_set dbg tag
    (List.length args * size_float / size_addr)
    args

(* Bounds checking *)

let make_checkbound dbg = function
  | [Cop (Clsr, [a1; Cconst_int (n, _)], _); Cconst_int (m, _)] when m lsl n > n
    ->
    Cop (Ccheckbound, [a1; Cconst_int ((m lsl n) + (1 lsl n) - 1, dbg)], dbg)
  | args -> Cop (Ccheckbound, args, dbg)

(* Record application and currying functions *)

let apply_function_name arity result (mode : Lambda.alloc_mode) =
  let res =
    match result with [| Val |] -> "" | _ -> "_R" ^ machtype_identifier result
  in
  let suff = match mode with Alloc_heap -> "" | Alloc_local -> "L" in
  "caml_apply" ^ unique_arity_identifier arity ^ res ^ suff

let apply_function_sym arity result mode =
  let arity = List.map Extended_machtype.change_tagged_int_to_val arity in
  let result = Extended_machtype.change_tagged_int_to_val result in
  assert (List.length arity > 0);
  Compilenv.need_apply_fun arity result mode;
  global_symbol (apply_function_name arity result mode)

let curry_function_sym_name function_kind arity result =
  match function_kind with
  | Lambda.Curried { nlocal } ->
    Compilenv.need_curry_fun function_kind arity result;
    "caml_curry"
    ^ unique_arity_identifier arity
    ^ (match result with
      | [| Val |] -> ""
      | _ -> "_R" ^ machtype_identifier result)
    ^ if nlocal > 0 then "L" ^ Int.to_string nlocal else ""
  | Lambda.Tupled -> (
    if List.exists (function [| Val |] | [| Int |] -> false | _ -> true) arity
    then
      Misc.fatal_error
        "tuplify_function is currently unsupported if arity contains non-values";
    (* Always use [Val] to ensure we don't generate duplicate tuplify functions
       when [Int] machtypes are involved. *)
    Compilenv.need_curry_fun function_kind
      (List.map (fun _ -> [| Val |]) arity)
      result;
    "caml_tuplify"
    ^ Int.to_string (List.length arity)
    ^
    match result with [| Val |] -> "" | _ -> "_R" ^ machtype_identifier result)

let curry_function_sym function_kind arity result =
  { sym_name = curry_function_sym_name function_kind arity result;
    sym_global = Global
  }

(* Big arrays *)

let bigarray_elt_size_in_bytes : Lambda.bigarray_kind -> int = function
  | Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> 4
  | Pbigarray_float64 -> 8
  | Pbigarray_sint8 -> 1
  | Pbigarray_uint8 -> 1
  | Pbigarray_sint16 -> 2
  | Pbigarray_uint16 -> 2
  | Pbigarray_int32 -> 4
  | Pbigarray_int64 -> 8
  | Pbigarray_caml_int -> size_int
  | Pbigarray_native_int -> size_int
  | Pbigarray_complex32 -> 8
  | Pbigarray_complex64 -> 16

(* Produces a pointer to the element of the bigarray [b] on the position [args].
   [args] is given as a list of tagged int expressions, one per array
   dimension. *)
let bigarray_indexing unsafe elt_kind layout b args dbg =
  let check_ba_bound bound idx v =
    Csequence (make_checkbound dbg [bound; idx], v)
  in
  (* Validates the given multidimensional offset against the array bounds and
     transforms it into a one dimensional offset. The offsets are expressions
     evaluating to tagged int. *)
  let rec ba_indexing dim_ofs delta_ofs = function
    | [] -> assert false
    | [arg] ->
      if unsafe
      then arg
      else
        bind "idx" arg (fun idx ->
            (* Load the untagged int bound for the given dimension *)
            let bound =
              Cop (Cload (Word_int, Mutable), [field_address b dim_ofs dbg], dbg)
            in
            let idxn = untag_int idx dbg in
            check_ba_bound bound idxn idx)
    | arg1 :: argl ->
      (* The remainder of the list is transformed into a one dimensional
         offset *)
      let rem = ba_indexing (dim_ofs + delta_ofs) delta_ofs argl in
      (* Load the untagged int bound for the given dimension *)
      let bound =
        Cop (Cload (Word_int, Mutable), [field_address b dim_ofs dbg], dbg)
      in
      if unsafe
      then add_int (mul_int (decr_int rem dbg) bound dbg) arg1 dbg
      else
        bind "idx" arg1 (fun idx ->
            bind "bound" bound (fun bound ->
                let idxn = untag_int idx dbg in
                (* [offset = rem * (tag_int bound) + idx] *)
                let offset =
                  add_int (mul_int (decr_int rem dbg) bound dbg) idx dbg
                in
                check_ba_bound bound idxn offset))
  in
  (* The offset as an expression evaluating to int *)
  let offset =
    match (layout : Lambda.bigarray_layout) with
    | Pbigarray_unknown_layout -> assert false
    | Pbigarray_c_layout ->
      ba_indexing (4 + List.length args) (-1) (List.rev args)
    | Pbigarray_fortran_layout ->
      ba_indexing 5 1
        (List.map (fun idx -> sub_int idx (Cconst_int (2, dbg)) dbg) args)
  and elt_size = bigarray_elt_size_in_bytes elt_kind in
  (* [array_indexing] can simplify the given expressions *)
  array_indexing ~typ:Addr (Misc.log2 elt_size)
    (Cop (Cload (Word_int, Mutable), [field_address b 1 dbg], dbg))
    offset dbg

let bigarray_word_kind : Lambda.bigarray_kind -> memory_chunk = function
  | Pbigarray_unknown -> assert false
  | Pbigarray_float32 -> Single
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word_int
  | Pbigarray_caml_int -> Word_int
  | Pbigarray_native_int -> Word_int
  | Pbigarray_complex32 -> Single
  | Pbigarray_complex64 -> Double

let bigarray_get unsafe elt_kind layout b args dbg =
  bind "ba" b (fun b ->
      match (elt_kind : Lambda.bigarray_kind) with
      | Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size_in_bytes elt_kind / 2 in
        bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
          (fun addr ->
            bind "reval"
              (Cop (Cload (kind, Mutable), [addr], dbg))
              (fun reval ->
                bind "imval"
                  (Cop
                     ( Cload (kind, Mutable),
                       [Cop (Cadda, [addr; Cconst_int (sz, dbg)], dbg)],
                       dbg ))
                  (fun imval -> box_complex dbg reval imval)))
      | _ ->
        Cop
          ( Cload (bigarray_word_kind elt_kind, Mutable),
            [bigarray_indexing unsafe elt_kind layout b args dbg],
            dbg ))

let bigarray_set unsafe elt_kind layout b args newval dbg =
  bind "ba" b (fun b ->
      match (elt_kind : Lambda.bigarray_kind) with
      | Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size_in_bytes elt_kind / 2 in
        bind "newval" newval (fun newv ->
            bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
              (fun addr ->
                Csequence
                  ( Cop
                      ( Cstore (kind, Assignment),
                        [addr; complex_re newv dbg],
                        dbg ),
                    Cop
                      ( Cstore (kind, Assignment),
                        [ Cop (Cadda, [addr; Cconst_int (sz, dbg)], dbg);
                          complex_im newv dbg ],
                        dbg ) )))
      | _ ->
        Cop
          ( Cstore (bigarray_word_kind elt_kind, Assignment),
            [bigarray_indexing unsafe elt_kind layout b args dbg; newval],
            dbg ))

(* the three functions below assume either 32-bit or 64-bit words *)
let () = assert (size_int = 4 || size_int = 8)

let check_64_bit_target func =
  if size_int <> 8
  then
    Misc.fatal_errorf
      "Cmm helpers function %s can only be used on 64-bit targets" func

(* low_32 x is a value which agrees with x on at least the low 32 bits *)
let rec low_32 dbg = function
  | x when size_int = 4 ->
    x (* Ignore sign and zero extensions, which do not affect the low bits *)
  | Cop (Casr, [Cop (Clsl, [x; Cconst_int (32, _)], _); Cconst_int (32, _)], _)
  | Cop (Cand, [x; Cconst_natint (0xFFFFFFFFn, _)], _) ->
    low_32 dbg x
  | Clet (id, e, body) -> Clet (id, e, low_32 dbg body)
  | x -> x

(* Like [low_32] but for 63-bit integers held in 64-bit registers. *)
(* CR gbury: Why not use Cmm.map_tail here ? It seems designed for that kind of
   thing (and covers more cases than just Clet). *)
let rec low_63 dbg e =
  check_64_bit_target "low_63";
  match e with
  | Cop (Casr, [Cop (Clsl, [x; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
    low_63 dbg x
  | Cop (Cand, [x; Cconst_natint (0x7FFF_FFFF_FFFF_FFFFn, _)], _) ->
    low_63 dbg x
  | Clet (id, x, body) -> Clet (id, x, low_63 dbg body)
  | _ -> e

(* sign_extend_32 sign-extends values from 32 bits to the word size. (if the
   word size is 32, this is a no-op) *)
let sign_extend_32 dbg e =
  if size_int = 4
  then e
  else
    match low_32 dbg e with
    | Cop (Cload ((Thirtytwo_unsigned | Thirtytwo_signed), mut), args, dbg) ->
      Cop (Cload (Thirtytwo_signed, mut), args, dbg)
    | e ->
      Cop
        ( Casr,
          [Cop (Clsl, [e; Cconst_int (32, dbg)], dbg); Cconst_int (32, dbg)],
          dbg )

(* CR-someday mshinwell/gbury: sign_extend_63 then tag_int should simplify to
   just tag_int. Similarly, untag_int then sign_extend_63 should simplify to
   untag_int. *)
let sign_extend_63 dbg e =
  check_64_bit_target "sign_extend_63";
  let e = low_63 dbg e in
  Cop
    (Casr, [Cop (Clsl, [e; Cconst_int (1, dbg)], dbg); Cconst_int (1, dbg)], dbg)

(* zero_extend_32 zero-extends values from 32 bits to the word size. (if the
   word size is 32, this is a no-op) *)
let zero_extend_32 dbg e =
  if size_int = 4
  then e
  else
    match low_32 dbg e with
    | Cop (Cload ((Thirtytwo_signed | Thirtytwo_unsigned), mut), args, dbg) ->
      Cop (Cload (Thirtytwo_unsigned, mut), args, dbg)
    | e -> Cop (Cand, [e; natint_const_untagged dbg 0xFFFFFFFFn], dbg)

let zero_extend_63 dbg e =
  check_64_bit_target "zero_extend_63";
  let e = low_63 dbg e in
  Cop (Cand, [e; natint_const_untagged dbg 0x7FFF_FFFF_FFFF_FFFFn], dbg)

let and_int e1 e2 dbg =
  let is_mask32 = function
    | Cconst_natint (0xFFFF_FFFFn, _) -> true
    | Cconst_int (n, _) -> Nativeint.of_int n = 0xFFFF_FFFFn
    | _ -> false
  in
  match e1, e2 with
  | e, m when is_mask32 m -> zero_extend_32 dbg e
  | m, e when is_mask32 m -> zero_extend_32 dbg e
  | e1, e2 -> Cop (Cand, [e1; e2], dbg)

let or_int e1 e2 dbg = Cop (Cor, [e1; e2], dbg)

let xor_int e1 e2 dbg = Cop (Cxor, [e1; e2], dbg)

(* Boxed integers *)

let operations_boxed_int (bi : Primitive.boxed_integer) =
  let sym_name =
    match bi with
    | Pnativeint -> caml_nativeint_ops
    | Pint32 -> caml_int32_ops
    | Pint64 -> caml_int64_ops
  in
  global_symbol sym_name

let alloc_header_boxed_int (bi : Primitive.boxed_integer) mode dbg =
  match bi with
  | Pnativeint -> alloc_boxedintnat_header mode dbg
  | Pint32 -> alloc_boxedint32_header mode dbg
  | Pint64 -> alloc_boxedint64_header mode dbg

let box_int_gen dbg (bi : Primitive.boxed_integer) mode arg =
  let arg' =
    if bi = Primitive.Pint32 && size_int = 8
    then
      if big_endian
      then Cop (Clsl, [arg; Cconst_int (32, dbg)], dbg)
      else sign_extend_32 dbg arg
    else arg
  in
  Cop
    ( Calloc mode,
      [ alloc_header_boxed_int bi mode dbg;
        Cconst_symbol (operations_boxed_int bi, dbg);
        arg' ],
      dbg )

let split_int64_for_32bit_target arg dbg =
  bind "split_int64" arg (fun arg ->
      let first = Cop (Cadda, [Cconst_int (size_int, dbg); arg], dbg) in
      let second = Cop (Cadda, [Cconst_int (2 * size_int, dbg); arg], dbg) in
      Ctuple
        [ Cop (Cload (Thirtytwo_unsigned, Mutable), [first], dbg);
          Cop (Cload (Thirtytwo_unsigned, Mutable), [second], dbg) ])

let alloc_matches_boxed_int bi ~hdr ~ops =
  match (bi : Primitive.boxed_integer), hdr, ops with
  | Pnativeint, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedintnat_header
    || Nativeint.equal hdr boxedintnat_local_header)
    && String.equal sym.sym_name caml_nativeint_ops
  | Pint32, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint32_header
    || Nativeint.equal hdr boxedint32_local_header)
    && String.equal sym.sym_name caml_int32_ops
  | Pint64, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
    (Nativeint.equal hdr boxedint64_header
    || Nativeint.equal hdr boxedint64_local_header)
    && String.equal sym.sym_name caml_int64_ops
  | (Pnativeint | Pint32 | Pint64), _, _ -> false

let rec unbox_int dbg bi =
  let default arg =
    if size_int = 4 && bi = Primitive.Pint64
    then split_int64_for_32bit_target arg dbg
    else
      Cop
        ( Cload
            ( (if bi = Primitive.Pint32 then Thirtytwo_signed else Word_int),
              Immutable ),
          [Cop (Cadda, [arg; Cconst_int (size_addr, dbg)], dbg)],
          dbg )
  in
  map_tail ~kind:Any (function
    | Cop
        ( Calloc _,
          [hdr; ops; Cop (Clsl, [contents; Cconst_int (32, _)], _dbg')],
          _dbg )
      when bi = Primitive.Pint32 && size_int = 8 && big_endian
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend_32 dbg contents
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when bi = Primitive.Pint32 && size_int = 8 && (not big_endian)
           && alloc_matches_boxed_int bi ~hdr ~ops ->
      (* Force sign-extension of low 32 bits *)
      sign_extend_32 dbg contents
    | Cop (Calloc _, [hdr; ops; contents], _dbg)
      when alloc_matches_boxed_int bi ~hdr ~ops ->
      contents
    | Cconst_symbol (s, _dbg) as cmm -> (
      match Cmmgen_state.structured_constant_of_sym s.sym_name, bi with
      | Some (Uconst_nativeint n), Primitive.Pnativeint ->
        natint_const_untagged dbg n
      | Some (Uconst_int32 n), Primitive.Pint32 ->
        natint_const_untagged dbg (Nativeint.of_int32 n)
      | Some (Uconst_int64 n), Primitive.Pint64 ->
        if size_int = 8
        then natint_const_untagged dbg (Int64.to_nativeint n)
        else
          let low = Int64.to_nativeint n in
          let high = Int64.to_nativeint (Int64.shift_right_logical n 32) in
          if big_endian
          then
            Ctuple
              [natint_const_untagged dbg high; natint_const_untagged dbg low]
          else
            Ctuple
              [natint_const_untagged dbg low; natint_const_untagged dbg high]
      | _ -> default cmm)
    | Cregion e as cmm -> (
      (* It is valid to push unboxing inside a Cregion except when the extra
         unboxing logic pushes a tail call out of tail position *)
      match
        map_tail ~kind:Any
          (function
            | Cop (Capply (_, Rc_close_at_apply), _, _) -> raise Exit
            | Ctail e -> Ctail (unbox_int dbg bi e)
            | e -> unbox_int dbg bi e)
          e
      with
      | e -> Cregion e
      | exception Exit -> default cmm)
    | Ctail e -> Ctail (unbox_int dbg bi e)
    | cmm -> default cmm)

let make_unsigned_int bi arg dbg =
  if bi = Primitive.Pint32 && size_int = 8 then zero_extend_32 dbg arg else arg

let unaligned_load_16 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop (Cload (Sixteen_unsigned, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Cop (Cor, [lsl_int b1 (cconst_int 8) dbg; b2], dbg)

let unaligned_set_16 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop
      (Cstore (Sixteen_unsigned, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v2 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Csequence
      ( Cop (Cstore (Byte_unsigned, Assignment), [add_int ptr idx dbg; b1], dbg),
        Cop
          ( Cstore (Byte_unsigned, Assignment),
            [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
            dbg ) )

let unaligned_load_32 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop (Cload (Thirtytwo_unsigned, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let v3 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 2) dbg],
          dbg )
    in
    let v4 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 3) dbg],
          dbg )
    in
    let b1, b2, b3, b4 =
      if Arch.big_endian then v1, v2, v3, v4 else v4, v3, v2, v1
    in
    Cop
      ( Cor,
        [ Cop
            ( Cor,
              [lsl_int b1 (cconst_int 24) dbg; lsl_int b2 (cconst_int 16) dbg],
              dbg );
          Cop (Cor, [lsl_int b3 (cconst_int 8) dbg; b4], dbg) ],
        dbg )

let unaligned_set_32 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop
      ( Cstore (Thirtytwo_unsigned, Assignment),
        [add_int ptr idx dbg; newval],
        dbg )
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop
        (Cand, [Cop (Clsr, [newval; cconst_int 24], dbg); cconst_int 0xFF], dbg)
    in
    let v2 =
      Cop
        (Cand, [Cop (Clsr, [newval; cconst_int 16], dbg); cconst_int 0xFF], dbg)
    in
    let v3 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v4 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4 =
      if Arch.big_endian then v1, v2, v3, v4 else v4, v3, v2, v1
    in
    Csequence
      ( Csequence
          ( Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int ptr idx dbg; b1],
                dbg ),
            Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                dbg ) ),
        Csequence
          ( Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                dbg ),
            Cop
              ( Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                dbg ) ) )

let unaligned_load_64 ptr idx dbg =
  assert (size_int = 8);
  if Arch.allow_unaligned_access
  then Cop (Cload (Word_int, Mutable), [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop (Cload (Byte_unsigned, Mutable), [add_int ptr idx dbg], dbg) in
    let v2 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 1) dbg],
          dbg )
    in
    let v3 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 2) dbg],
          dbg )
    in
    let v4 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 3) dbg],
          dbg )
    in
    let v5 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 4) dbg],
          dbg )
    in
    let v6 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 5) dbg],
          dbg )
    in
    let v7 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 6) dbg],
          dbg )
    in
    let v8 =
      Cop
        ( Cload (Byte_unsigned, Mutable),
          [add_int (add_int ptr idx dbg) (cconst_int 7) dbg],
          dbg )
    in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1
    in
    Cop
      ( Cor,
        [ Cop
            ( Cor,
              [ Cop
                  ( Cor,
                    [ lsl_int b1 (cconst_int (8 * 7)) dbg;
                      lsl_int b2 (cconst_int (8 * 6)) dbg ],
                    dbg );
                Cop
                  ( Cor,
                    [ lsl_int b3 (cconst_int (8 * 5)) dbg;
                      lsl_int b4 (cconst_int (8 * 4)) dbg ],
                    dbg ) ],
              dbg );
          Cop
            ( Cor,
              [ Cop
                  ( Cor,
                    [ lsl_int b5 (cconst_int (8 * 3)) dbg;
                      lsl_int b6 (cconst_int (8 * 2)) dbg ],
                    dbg );
                Cop (Cor, [lsl_int b7 (cconst_int 8) dbg; b8], dbg) ],
              dbg ) ],
        dbg )

let unaligned_set_64 ptr idx newval dbg =
  assert (size_int = 8);
  if Arch.allow_unaligned_access
  then Cop (Cstore (Word_int, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 7)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v2 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 6)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v3 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 5)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v4 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 4)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v5 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 3)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v6 =
      Cop
        ( Cand,
          [Cop (Clsr, [newval; cconst_int (8 * 2)], dbg); cconst_int 0xFF],
          dbg )
    in
    let v7 =
      Cop (Cand, [Cop (Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v8 = Cop (Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1
    in
    Csequence
      ( Csequence
          ( Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int ptr idx dbg; b1],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                    dbg ) ),
            Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                    dbg ) ) ),
        Csequence
          ( Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 4) dbg; b5],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 5) dbg; b6],
                    dbg ) ),
            Csequence
              ( Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 6) dbg; b7],
                    dbg ),
                Cop
                  ( Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 7) dbg; b8],
                    dbg ) ) ) )

let max_or_zero a dbg =
  bind "size" a (fun a ->
      (* equivalent to:

         Cifthenelse(Cop(Ccmpi Cle, [a; cconst_int 0]), cconst_int 0, a)

         if a is positive, sign is 0 hence sign_negation is full of 1 so
         sign_negation&a = a

         if a is negative, sign is full of 1 hence sign_negation is 0 so
         sign_negation&a = 0 *)
      let sign = Cop (Casr, [a; Cconst_int ((size_int * 8) - 1, dbg)], dbg) in
      let sign_negation = Cop (Cxor, [sign; Cconst_int (-1, dbg)], dbg) in
      Cop (Cand, [sign_negation; a], dbg))

let check_bound safety access_size dbg length a2 k =
  match (safety : Lambda.is_safe) with
  | Unsafe -> k
  | Safe ->
    let offset =
      match (access_size : Clambda_primitives.memory_access_size) with
      | Sixteen -> 1
      | Thirty_two -> 3
      | Sixty_four -> 7
    in
    let a1 = sub_int length (Cconst_int (offset, dbg)) dbg in
    Csequence (make_checkbound dbg [max_or_zero a1 dbg; a2], k)

let opaque e dbg = Cop (Copaque, [e], dbg)

let unaligned_set size ptr idx newval dbg =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> unaligned_set_16 ptr idx newval dbg
  | Thirty_two -> unaligned_set_32 ptr idx newval dbg
  | Sixty_four -> unaligned_set_64 ptr idx newval dbg

let unaligned_load size ptr idx dbg =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> unaligned_load_16 ptr idx dbg
  | Thirty_two -> unaligned_load_32 ptr idx dbg
  | Sixty_four -> unaligned_load_64 ptr idx dbg

let box_sized size mode dbg exp =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> tag_int exp dbg
  | Thirty_two -> box_int_gen dbg Pint32 mode exp
  | Sixty_four -> box_int_gen dbg Pint64 mode exp

(* Simplification of some primitives into C calls *)

let default_prim name =
  Primitive.simple_on_values ~name ~arity:0 (*ignored*) ~alloc:true

let int64_native_prim name arity ~alloc =
  let u64 = Primitive.(Prim_global, Unboxed_integer Pint64) in
  let rec make_args = function 0 -> [] | n -> u64 :: make_args (n - 1) in
  let effects, coeffects =
    if alloc
    then Primitive.Arbitrary_effects, Primitive.Has_coeffects
    else Primitive.No_effects, Primitive.No_coeffects
  in
  Primitive.make ~name ~native_name:(name ^ "_native") ~alloc ~c_builtin:false
    ~effects ~coeffects ~native_repr_args:(make_args arity) ~native_repr_res:u64

(* TODO: On 32-bit, these will do heap allocations even in situations where
   local allocs are allowed *)
let simplif_primitive_32bits :
    Clambda_primitives.primitive -> Clambda_primitives.primitive = function
  | Pbintofint (Pint64, _) -> Pccall (default_prim "caml_int64_of_int")
  | Pintofbint Pint64 -> Pccall (default_prim "caml_int64_to_int")
  | Pcvtbint (Pint32, Pint64, _) -> Pccall (default_prim "caml_int64_of_int32")
  | Pcvtbint (Pint64, Pint32, _) -> Pccall (default_prim "caml_int64_to_int32")
  | Pcvtbint (Pnativeint, Pint64, _) ->
    Pccall (default_prim "caml_int64_of_nativeint")
  | Pcvtbint (Pint64, Pnativeint, _) ->
    Pccall (default_prim "caml_int64_to_nativeint")
  | Pnegbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_neg" 1 ~alloc:false)
  | Paddbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_add" 2 ~alloc:false)
  | Psubbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_sub" 2 ~alloc:false)
  | Pmulbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_mul" 2 ~alloc:false)
  | Pdivbint { size = Pint64 } ->
    Pccall (int64_native_prim "caml_int64_div" 2 ~alloc:true)
  | Pmodbint { size = Pint64 } ->
    Pccall (int64_native_prim "caml_int64_mod" 2 ~alloc:true)
  | Pandbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_and" 2 ~alloc:false)
  | Porbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_or" 2 ~alloc:false)
  | Pxorbint (Pint64, _) ->
    Pccall (int64_native_prim "caml_int64_xor" 2 ~alloc:false)
  | Plslbint (Pint64, _) -> Pccall (default_prim "caml_int64_shift_left")
  | Plsrbint (Pint64, _) ->
    Pccall (default_prim "caml_int64_shift_right_unsigned")
  | Pasrbint (Pint64, _) -> Pccall (default_prim "caml_int64_shift_right")
  | Pbintcomp (Pint64, Lambda.Ceq) -> Pccall (default_prim "caml_equal")
  | Pbintcomp (Pint64, Lambda.Cne) -> Pccall (default_prim "caml_notequal")
  | Pbintcomp (Pint64, Lambda.Clt) -> Pccall (default_prim "caml_lessthan")
  | Pbintcomp (Pint64, Lambda.Cgt) -> Pccall (default_prim "caml_greaterthan")
  | Pbintcomp (Pint64, Lambda.Cle) -> Pccall (default_prim "caml_lessequal")
  | Pbintcomp (Pint64, Lambda.Cge) -> Pccall (default_prim "caml_greaterequal")
  | Pcompare_bints Pint64 -> Pccall (default_prim "caml_int64_compare")
  | Pbigarrayref (_unsafe, n, Pbigarray_int64, _layout) ->
    Pccall (default_prim ("caml_ba_get_" ^ Int.to_string n))
  | Pbigarrayset (_unsafe, n, Pbigarray_int64, _layout) ->
    Pccall (default_prim ("caml_ba_set_" ^ Int.to_string n))
  | Pstring_load (Sixty_four, _, _) -> Pccall (default_prim "caml_string_get64")
  | Pbytes_load (Sixty_four, _, _) -> Pccall (default_prim "caml_bytes_get64")
  | Pbytes_set (Sixty_four, _) -> Pccall (default_prim "caml_bytes_set64")
  | Pbigstring_load (Sixty_four, _, _) ->
    Pccall (default_prim "caml_ba_uint8_get64")
  | Pbigstring_set (Sixty_four, _) ->
    Pccall (default_prim "caml_ba_uint8_set64")
  | Pbbswap (Pint64, _) -> Pccall (default_prim "caml_int64_bswap")
  | p -> p

let simplif_primitive p : Clambda_primitives.primitive =
  match (p : Clambda_primitives.primitive) with
  | Pduprecord _ -> Pccall (default_prim "caml_obj_dup")
  | Pbigarrayref (_unsafe, n, Pbigarray_unknown, _layout) ->
    Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset (_unsafe, n, Pbigarray_unknown, _layout) ->
    Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | Pbigarrayref (_unsafe, n, _kind, Pbigarray_unknown_layout) ->
    Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset (_unsafe, n, _kind, Pbigarray_unknown_layout) ->
    Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | p -> if size_int = 8 then p else simplif_primitive_32bits p

(* Build switchers both for constants and blocks *)

let transl_isout h arg dbg = tag_int (Cop (Ccmpa Clt, [h; arg], dbg)) dbg

(* Build an actual switch (ie jump table) *)

let make_switch arg cases actions dbg kind =
  let extract_uconstant = function
    (* Constant integers loaded from a table should end in 1, so that Cload
       never produces untagged integers *)
    | Cconst_int (n, _), _dbg when n land 1 = 1 ->
      Some (Cint (Nativeint.of_int n))
    | Cconst_natint (n, _), _dbg when Nativeint.(to_int (logand n one) = 1) ->
      Some (Cint n)
    | Cconst_symbol (s, _), _dbg -> Some (Csymbol_address s)
    | _ -> None
  in
  let extract_affine ~cases ~const_actions =
    let length = Array.length cases in
    if length >= 2
    then
      match const_actions.(cases.(0)), const_actions.(cases.(1)) with
      | Cint v0, Cint v1 ->
        let slope = Nativeint.sub v1 v0 in
        let check i = function
          | Cint v -> v = Nativeint.(add (mul (of_int i) slope) v0)
          | _ -> false
        in
        if Misc.Stdlib.Array.for_alli
             (fun i idx -> check i const_actions.(idx))
             cases
        then Some (v0, slope)
        else None
      | _, _ -> None
    else None
  in
  let make_table_lookup ~cases ~const_actions arg dbg =
    let table = Compilenv.new_const_symbol () in
    Cmmgen_state.add_constant table
      (Const_table
         ( Local,
           Array.to_list (Array.map (fun act -> const_actions.(act)) cases) ));
    let table_sym = { sym_name = table; sym_global = Local } in
    addr_array_ref (Cconst_symbol (table_sym, dbg)) (tag_int arg dbg) dbg
  in
  let make_affine_computation ~offset ~slope arg dbg =
    (* In case the resulting integers are an affine function of the index, we
       don't emit a table, and just compute the result directly *)
    add_int
      (mul_int arg (natint_const_untagged dbg slope) dbg)
      (natint_const_untagged dbg offset)
      dbg
  in
  match Misc.Stdlib.Array.all_somes (Array.map extract_uconstant actions) with
  | None -> Cswitch (arg, cases, actions, dbg, kind)
  | Some const_actions -> (
    match extract_affine ~cases ~const_actions with
    | Some (offset, slope) -> make_affine_computation ~offset ~slope arg dbg
    | None -> make_table_lookup ~cases ~const_actions arg dbg)

module SArgBlocks = struct
  type primitive = operation

  let eqint = Ccmpi Ceq

  let neint = Ccmpi Cne

  let leint = Ccmpi Cle

  let ltint = Ccmpi Clt

  let geint = Ccmpi Cge

  let gtint = Ccmpi Cgt

  type arg = expression

  type test = expression

  type act = expression

  type loc = Debuginfo.t

  type layout = kind_for_unboxing

  (* CR mshinwell: GPR#2294 will fix the Debuginfo here *)

  let make_const i = Cconst_int (i, Debuginfo.none)

  let make_prim p args = Cop (p, args, Debuginfo.none)

  let make_offset arg n = add_const arg n Debuginfo.none

  let make_isout h arg = Cop (Ccmpa Clt, [h; arg], Debuginfo.none)

  let make_isin h arg = Cop (Ccmpa Cge, [h; arg], Debuginfo.none)

  let make_is_nonzero arg = arg

  let arg_as_test arg = arg

  let make_if value_kind cond ifso ifnot =
    Cifthenelse
      ( cond,
        Debuginfo.none,
        ifso,
        Debuginfo.none,
        ifnot,
        Debuginfo.none,
        value_kind )

  let make_switch dbg value_kind arg cases actions =
    let actions = Array.map (fun expr -> expr, dbg) actions in
    make_switch arg cases actions dbg value_kind

  let bind arg body = bind "switcher" arg body

  let make_catch kind handler =
    match handler with
    | Cexit (Lbl i, [], []) -> i, fun e -> e
    | _ -> (
      let dbg = Debuginfo.none in
      let i = Lambda.next_raise_count () in
      (* Printf.eprintf "SHARE CMM: %i\n" i ; Printcmm.expression
         Format.str_formatter handler ; Printf.eprintf "%s\n"
         (Format.flush_str_formatter ()) ; *)
      ( i,
        fun body ->
          match body with
          | Cexit (j, _, _) -> if Lbl i = j then handler else body
          | _ -> ccatch (i, [], body, handler, dbg, kind) ))

  let make_exit i = Cexit (Lbl i, [], [])
end

(* cmm store, as sharing as normally been detected in previous phases, we only
   share exits *)
(* Some specific patterns can lead to switches where several cases point to the
   same action, but this action is not an exit (see GPR#1370). The addition of
   the index in the action array as context allows to share them correctly
   without duplication. *)
module StoreExpForSwitch = Switch.CtxStore (struct
  type t = expression

  type key = int option * int

  type context = int

  let make_key index expr =
    let continuation =
      match expr with Cexit (Lbl i, [], []) -> Some i | _ -> None
    in
    Some (continuation, index)

  let compare_key (cont, index) (cont', index') =
    match cont, cont' with
    | Some i, Some i' when i = i' -> 0
    | _, _ -> Stdlib.compare index index'
end)

(* For string switches, we can use a generic store *)
module StoreExp = Switch.Store (struct
  type t = expression

  type key = int

  let make_key = function Cexit (Lbl i, [], []) -> Some i | _ -> None

  let compare_key = Stdlib.compare
end)

module SwitcherBlocks = Switch.Make (SArgBlocks)

(* Int switcher, arg in [low..high], cases is list of individual cases, and is
   sorted by first component *)

let transl_int_switch dbg value_kind arg low high cases default =
  match cases with
  | [] -> assert false
  | _ :: _ ->
    let store = StoreExp.mk_store () in
    assert (store.Switch.act_store () default = 0);
    let cases =
      List.map (fun (i, act) -> i, store.Switch.act_store () act) cases
    in
    let rec inters plow phigh pact = function
      | [] ->
        if phigh = high
        then [plow, phigh, pact]
        else [plow, phigh, pact; phigh + 1, high, 0]
      | (i, act) :: rem ->
        if i = phigh + 1
        then
          if pact = act
          then inters plow i pact rem
          else (plow, phigh, pact) :: inters i i act rem
        else if (* insert default *)
                pact = 0
        then
          if act = 0
          then inters plow i 0 rem
          else (plow, i - 1, pact) :: inters i i act rem
        else
          (* pact <> 0 *)
          (plow, phigh, pact)
          ::
          (if act = 0
          then inters (phigh + 1) i 0 rem
          else (phigh + 1, i - 1, 0) :: inters i i act rem)
    in
    let inters =
      match cases with
      | [] -> assert false
      | (k0, act0) :: rem ->
        if k0 = low then inters k0 k0 act0 rem else inters low (k0 - 1) 0 cases
    in
    bind "switcher" arg (fun a ->
        SwitcherBlocks.zyva dbg value_kind (low, high) a (Array.of_list inters)
          store)

let transl_switch_clambda loc value_kind arg index cases =
  let store = StoreExpForSwitch.mk_store () in
  let index = Array.map (fun j -> store.Switch.act_store j cases.(j)) index in
  let n_index = Array.length index in
  let inters = ref []
  and this_high = ref (n_index - 1)
  and this_low = ref (n_index - 1)
  and this_act = ref index.(n_index - 1) in
  for i = n_index - 2 downto 0 do
    let act = index.(i) in
    if act = !this_act
    then decr this_low
    else (
      inters := (!this_low, !this_high, !this_act) :: !inters;
      this_high := i;
      this_low := i;
      this_act := act)
  done;
  inters := (0, !this_high, !this_act) :: !inters;
  match !inters with
  | [_] -> cases.(0)
  | inters ->
    bind "switcher" arg (fun a ->
        SwitcherBlocks.zyva loc value_kind
          (0, n_index - 1)
          a (Array.of_list inters) store)

let strmatch_compile =
  let module S = Strmatch.Make (struct
    let string_block_length ptr = get_size ptr Debuginfo.none

    let transl_switch = transl_int_switch
  end) in
  S.compile

let ptr_offset ptr offset dbg =
  if offset = 0
  then ptr
  else Cop (Caddv, [ptr; Cconst_int (offset * size_addr, dbg)], dbg)

let direct_apply lbl ty args (pos, _mode) dbg =
  Cop (Capply (ty, pos), Cconst_symbol (lbl, dbg) :: args, dbg)

let call_caml_apply extended_ty extended_args_type mut clos args pos mode dbg =
  (* Treat tagged int arguments and results as [typ_val], to avoid generating
     excessive numbers of caml_apply functions. *)
  let ty = Extended_machtype.to_machtype extended_ty in
  let really_call_caml_apply clos args =
    let cargs =
      Cconst_symbol (apply_function_sym extended_args_type extended_ty mode, dbg)
      :: args
      @ [clos]
    in
    Cop (Capply (ty, pos), cargs, dbg)
  in
  if !Flambda_backend_flags.caml_apply_inline_fast_path
  then
    (* Generate the following expression:
     *  (if (= clos.arity N)
     *      (app clos.direct a1 ... aN clos)
     *      (app caml_applyN a1 .. aN clos)
     *)
    (* CR-someday gyorsh: in the [else] case above, call another version of
       caml_applyN that has only the cold path. *)
    bind_list "arg" args (fun args ->
        bind "fun" clos (fun clos ->
            Cifthenelse
              ( Cop
                  ( Ccmpi Ceq,
                    [ Cop
                        ( Casr,
                          [ get_field_gen mut clos 1 dbg;
                            Cconst_int (pos_arity_in_closinfo, dbg) ],
                          dbg );
                      Cconst_int (List.length extended_args_type, dbg) ],
                    dbg ),
                dbg,
                Cop
                  ( Capply (ty, pos),
                    (get_field_gen mut clos 2 dbg :: args) @ [clos],
                    dbg ),
                dbg,
                really_call_caml_apply clos args,
                dbg,
                Any )))
  else really_call_caml_apply clos args

let generic_apply mut clos args args_type result (pos, mode) dbg =
  match args with
  | [arg] ->
    bind "fun" clos (fun clos ->
        Cop
          ( Capply (Extended_machtype.to_machtype result, pos),
            [get_field_gen mut clos 0 dbg; arg; clos],
            dbg ))
  | _ -> call_caml_apply result args_type mut clos args pos mode dbg

let send kind met obj args args_type result akind dbg =
  let call_met obj args args_type clos =
    (* met is never a simple expression, so it never gets turned into an
       Immutable load *)
    generic_apply Asttypes.Mutable clos (obj :: args)
      (Extended_machtype.typ_val :: args_type)
      result akind dbg
  in
  bind "obj" obj (fun obj ->
      match (kind : Lambda.meth_kind), args, args_type with
      | Self, _, _ ->
        bind "met" (lookup_label obj met dbg) (call_met obj args args_type)
      | Cached, cache :: pos :: args, _ :: _ :: args_type ->
        call_cached_method obj met cache pos args args_type result akind dbg
      | _ -> bind "met" (lookup_tag obj met dbg) (call_met obj args args_type))

(*
 * CAMLprim value caml_cache_public_method (value meths, value tag,
 *                                          value *cache)
 * {
 *   int li = 3, hi = Field(meths,0), mi;
 *   while (li < hi) { // no need to check the 1st time
 *     mi = ((li+hi) >> 1) | 1;
 *     if (tag < Field(meths,mi)) hi = mi-2;
 *     else li = mi;
 *   }
 *   *cache = (li-3)*sizeof(value)+1;
 *   return Field (meths, li-1);
 * }
 *)

let cache_public_method meths tag cache dbg =
  let raise_num = Lambda.next_raise_count () in
  let cconst_int i = Cconst_int (i, dbg) in
  let li = V.create_local "*li*"
  and hi = V.create_local "*hi*"
  and mi = V.create_local "*mi*"
  and tagged = V.create_local "*tagged*" in
  Clet_mut
    ( VP.create li,
      typ_int,
      cconst_int 3,
      Clet_mut
        ( VP.create hi,
          typ_int,
          Cop (Cload (Word_int, Mutable), [meths], dbg),
          Csequence
            ( ccatch
                ( raise_num,
                  [],
                  create_loop
                    (Clet
                       ( VP.create mi,
                         Cop
                           ( Cor,
                             [ Cop
                                 ( Clsr,
                                   [ Cop (Caddi, [Cvar li; Cvar hi], dbg);
                                     cconst_int 1 ],
                                   dbg );
                               cconst_int 1 ],
                             dbg ),
                         Csequence
                           ( Cifthenelse
                               ( Cop
                                   ( Ccmpi Clt,
                                     [ tag;
                                       Cop
                                         ( Cload (Word_int, Mutable),
                                           [ Cop
                                               ( Cadda,
                                                 [ meths;
                                                   lsl_const (Cvar mi)
                                                     log2_size_addr dbg ],
                                                 dbg ) ],
                                           dbg ) ],
                                     dbg ),
                                 dbg,
                                 Cassign
                                   ( hi,
                                     Cop (Csubi, [Cvar mi; cconst_int 2], dbg)
                                   ),
                                 dbg,
                                 Cassign (li, Cvar mi),
                                 dbg,
                                 Any ),
                             Cifthenelse
                               ( Cop (Ccmpi Cge, [Cvar li; Cvar hi], dbg),
                                 dbg,
                                 Cexit (Lbl raise_num, [], []),
                                 dbg,
                                 Ctuple [],
                                 dbg,
                                 Any ) ) ))
                    dbg,
                  Ctuple [],
                  dbg,
                  Any ),
              Clet
                ( VP.create tagged,
                  Cop
                    ( Caddi,
                      [ lsl_const (Cvar li) log2_size_addr dbg;
                        cconst_int (1 - (3 * size_addr)) ],
                      dbg ),
                  Csequence
                    ( Cop
                        ( Cstore (Word_int, Assignment),
                          [cache; Cvar tagged],
                          dbg ),
                      Cvar tagged ) ) ) ) )

let has_local_allocs e =
  let rec loop = function
    | Cregion e ->
      (* Local allocations within a nested region do not affect this region,
         except inside a Ctail block *)
      loop_until_tail e
    | Cop (Calloc Alloc_local, _, _) | Cop ((Cextcall _ | Capply _), _, _) ->
      raise Exit
    | e -> iter_shallow loop e
  and loop_until_tail = function
    | Ctail e -> loop e
    | Cregion _ -> ()
    | e -> ignore (iter_shallow_tail loop_until_tail e)
  in
  match loop e with () -> false | exception Exit -> true

let remove_region_tail e =
  let rec has_tail = function
    | Ctail _ | Cop (Capply (_, Rc_close_at_apply), _, _) -> raise Exit
    | Cregion _ -> ()
    | e -> ignore (iter_shallow_tail has_tail e)
  in
  let rec remove_tail = function
    | Ctail e -> e
    | Cop (Capply (mach, Rc_close_at_apply), args, dbg) ->
      Cop (Capply (mach, Rc_normal), args, dbg)
    | Cregion _ as e -> e
    | e -> map_shallow_tail remove_tail e
  in
  match has_tail e with () -> e | exception Exit -> remove_tail e

let region e =
  (* [Cregion e] is equivalent to [e] if [e] contains no local allocs *)
  if has_local_allocs e then Cregion e else remove_region_tail e

(* CR mshinwell: These will be filled in by later pull requests. *)
let placeholder_dbg () = Debuginfo.none

let placeholder_fun_dbg ~human_name:_ = Debuginfo.none

(* Generate an application function:
 *  (defun caml_applyN (a1 ... aN clos)
 *    (if (= clos.arity N)
 *      (app clos.direct a1 ... aN clos)
 *      (let (clos1 (app clos.code a1 clos)
 *            clos2 (app clos1.code a2 clos)
 *            ...
 *            closN-1 (app closN-2.code aN-1 closN-2))
 *        (app closN-1.code aN closN-1))))
 *)

let apply_function_body arity result (mode : Lambda.alloc_mode) =
  let dbg = placeholder_dbg in
  let args = List.map (fun _ -> V.create_local "arg") arity in
  let clos = V.create_local "clos" in
  (* In the slowpath, a region is necessary in case the initial applications do
     local allocations *)
  let region =
    if not Config.stack_allocation
    then None
    else
      match mode with
      | Alloc_heap -> Some (V.create_local "region")
      | Alloc_local -> None
  in
  let rec app_fun clos args =
    match args with
    | [] -> Misc.fatal_error "apply_function_body for empty arity"
    | [arg] -> (
      let app =
        Cop
          ( Capply (result, Rc_normal),
            [ get_field_gen Asttypes.Mutable (Cvar clos) 0 (dbg ());
              Cvar arg;
              Cvar clos ],
            dbg () )
      in
      match region with
      | None -> app
      | Some region ->
        (* To preserve tail-call behaviour, we do a runtime check whether
           anything has been allocated in [region]. If not, then we can do a
           direct tail call without waiting to end the region afterwards. *)
        Cifthenelse
          ( Cop
              (Ccmpi Ceq, [Cvar region; Cop (Cbeginregion, [], dbg ())], dbg ()),
            dbg (),
            app,
            dbg (),
            (let res = V.create_local "result" in
             Clet
               ( VP.create res,
                 app,
                 Csequence (Cop (Cendregion, [Cvar region], dbg ()), Cvar res)
               )),
            dbg (),
            Any ))
    | arg :: args ->
      let newclos = V.create_local "clos" in
      Clet
        ( VP.create newclos,
          Cop
            ( Capply (typ_val, Rc_normal),
              [ get_field_gen Asttypes.Mutable (Cvar clos) 0 (dbg ());
                Cvar arg;
                Cvar clos ],
              dbg () ),
          app_fun newclos args )
  in
  let code =
    match region with
    | None -> app_fun clos args
    | Some reg ->
      Clet (VP.create reg, Cop (Cbeginregion, [], dbg ()), app_fun clos args)
  in
  let all_args = args @ [clos] in
  ( args,
    clos,
    if List.compare_length_with arity 1 = 0
    then code
    else
      Cifthenelse
        ( Cop
            ( Ccmpi Ceq,
              [ Cop
                  ( Casr,
                    [ get_field_gen Asttypes.Mutable (Cvar clos) 1 (dbg ());
                      Cconst_int (pos_arity_in_closinfo, dbg ()) ],
                    dbg () );
                Cconst_int (List.length arity, dbg ()) ],
              dbg () ),
          dbg (),
          Cop
            ( Capply (result, Rc_normal),
              get_field_gen Asttypes.Mutable (Cvar clos) 2 (dbg ())
              :: List.map (fun s -> Cvar s) all_args,
              dbg () ),
          dbg (),
          code,
          dbg (),
          Any ) )

let send_function (arity, result, mode) =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let args, clos', body = apply_function_body (typ_val :: arity) result mode in
  let cache = V.create_local "cache"
  and obj = List.hd args
  and tag = V.create_local "tag" in
  let clos =
    let cache = Cvar cache and obj = Cvar obj and tag = Cvar tag in
    let meths = V.create_local "meths" and cached = V.create_local "cached" in
    let real = V.create_local "real" in
    let mask = get_field_gen Asttypes.Mutable (Cvar meths) 1 (dbg ()) in
    let cached_pos = Cvar cached in
    let tag_pos =
      Cop
        ( Cadda,
          [ Cop (Cadda, [cached_pos; Cvar meths], dbg ());
            cconst_int ((3 * size_addr) - 1) ],
          dbg () )
    in
    let tag' = Cop (Cload (Word_int, Mutable), [tag_pos], dbg ()) in
    Clet
      ( VP.create meths,
        Cop (Cload (Word_val, Mutable), [obj], dbg ()),
        Clet
          ( VP.create cached,
            Cop
              ( Cand,
                [Cop (Cload (Word_int, Mutable), [cache], dbg ()); mask],
                dbg () ),
            Clet
              ( VP.create real,
                Cifthenelse
                  ( Cop (Ccmpa Cne, [tag'; tag], dbg ()),
                    dbg (),
                    cache_public_method (Cvar meths) tag cache (dbg ()),
                    dbg (),
                    cached_pos,
                    dbg (),
                    Any ),
                Cop
                  ( Cload (Word_val, Mutable),
                    [ Cop
                        ( Cadda,
                          [ Cop (Cadda, [Cvar real; Cvar meths], dbg ());
                            cconst_int ((2 * size_addr) - 1) ],
                          dbg () ) ],
                    dbg () ) ) ) )
  in
  let body = Clet (VP.create clos', clos, body) in
  let cache = cache in
  let fun_name = send_function_name arity result mode in
  let fun_args =
    [obj, typ_val; tag, typ_int; cache, typ_addr]
    @ List.combine (List.tl args) arity
  in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) fun_args;
      fun_body = body;
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

let apply_function (arity, result, mode) =
  let args, clos, body = apply_function_body arity result mode in
  let all_args = List.combine args arity @ [clos, typ_val] in
  let fun_name = global_symbol (apply_function_name arity result mode) in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) all_args;
      fun_body = body;
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

(* Generate tuplifying functions:
 *    (defun caml_tuplifyN (arg clos)
 *      (app clos.direct #0(arg) ... #N-1(arg) clos))
 *)

let tuplify_function arity return =
  if List.exists (function [| Val |] | [| Int |] -> false | _ -> true) arity
  then
    Misc.fatal_error
      "tuplify_function is currently unsupported if arity contains non-values";
  let arity = List.length arity in
  let dbg = placeholder_dbg in
  let arg = V.create_local "arg" in
  let clos = V.create_local "clos" in
  let rec access_components i =
    if i >= arity
    then []
    else
      get_field_gen Asttypes.Mutable (Cvar arg) i (dbg ())
      :: access_components (i + 1)
  in
  let fun_name =
    global_symbol
      ("caml_tuplify" ^ Int.to_string arity
      ^
      match return with
      | [| Val |] -> ""
      | _ -> "_R" ^ machtype_identifier return)
  in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
      fun_body =
        Cop
          ( Capply (return, Rc_normal),
            get_field_gen Asttypes.Mutable (Cvar clos) 2 (dbg ())
            :: access_components 0
            @ [Cvar clos],
            dbg () );
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

(* Generate currying functions:
 *
 *   (defun caml_curryN (arg clos)
 *      (alloc HDR caml_curryN_1 <arity (N-1)> caml_curry_N_1_app arg clos))
 *   (defun caml_curryN_1 (arg clos)
 *      (alloc HDR caml_curryN_2 <arity (N-2)> caml_curry_N_2_app arg clos))
 *   ...
 *   (defun caml_curryN_N-1 (arg clos)
 *      (let (closN-2 clos.vars[1]
 *            closN-3 closN-2.vars[1]
 *            ...
 *            clos1 clos2.vars[1]
 *            clos clos1.vars[1])
 *        (app clos.direct
 *             clos1.vars[0] ... closN-2.vars[0] clos.vars[0] arg clos)))
 *
 * Special "shortcut" functions are also generated to handle the case where a
 * partially applied function is applied to all remaining arguments in one go.
 *
 *   (defun caml_curry_N_1_app (arg2 ... argN clos)
 *     (let clos' clos.vars[1]
 *        (app clos'.direct clos.vars[0] arg2 ... argN clos')))
 *
 * Those shortcuts may lead to a quadratic number of application primitives
 * being generated in the worst case, which resulted in linking time blowup in
 * practice (PR#5933), so we only generate and use them when below a fixed arity
 * 'max_arity_optimized'. *)

let max_arity_optimized = 15

let ints_per_float = size_float / Arch.size_int

let ints_per_vec128 = size_vec128 / Arch.size_int

let machtype_stored_size t =
  Array.fold_left
    (fun cur c ->
      match c with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Val | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Vec128 -> cur + ints_per_vec128)
    0 t

let machtype_non_scanned_size t =
  Array.fold_left
    (fun cur c ->
      match c with
      | Addr -> Misc.fatal_error "[Addr] cannot be stored"
      | Val -> cur
      | Int -> cur + 1
      | Float -> cur + ints_per_float
      | Vec128 -> cur + ints_per_vec128)
    0 t

let make_tuple l = match l with [e] -> e | _ -> Ctuple l

let value_slot_given_machtype vs =
  let non_scanned, scanned =
    List.partition
      (fun (_, c) ->
        match c with
        | Int | Float | Vec128 -> true
        | Val -> false
        | Addr -> assert false)
      vs
  in
  List.map (fun (v, _) -> Cvar v) (non_scanned @ scanned)

let read_from_closure_given_machtype t clos base_offset dbg =
  let load chunk offset =
    Cop (Cload (chunk, Asttypes.Mutable), [field_address clos offset dbg], dbg)
  in
  let _, l =
    List.fold_left_map
      (fun (non_scanned_pos, scanned_pos) c ->
        match c with
        | Int ->
          (non_scanned_pos + 1, scanned_pos), load Word_int non_scanned_pos
        | Float ->
          ( (non_scanned_pos + ints_per_float, scanned_pos),
            load Double non_scanned_pos )
        | Vec128 ->
          ( (non_scanned_pos + ints_per_vec128, scanned_pos),
            load Onetwentyeight non_scanned_pos )
        | Val -> (non_scanned_pos, scanned_pos + 1), load Word_val scanned_pos
        | Addr -> Misc.fatal_error "[Addr] cannot be read")
      (base_offset, base_offset + machtype_non_scanned_size t)
      (Array.to_list t)
  in
  make_tuple l

let curry_clos_has_nary_application ~narity n =
  narity <= max_arity_optimized && n < narity - 1

let rec make_curry_apply result narity args_type args clos n =
  let dbg = placeholder_dbg in
  match args_type with
  | [] ->
    Cop
      ( Capply (result, Rc_normal),
        (get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ()) :: args)
        @ [Cvar clos],
        dbg () )
  | arg_type :: args_type ->
    let newclos = V.create_local "clos" in
    let arg_pos = if curry_clos_has_nary_application ~narity n then 3 else 2 in
    let clos_pos = arg_pos + machtype_stored_size arg_type in
    Clet
      ( VP.create newclos,
        get_field_gen Asttypes.Mutable (Cvar clos) clos_pos (dbg ()),
        make_curry_apply result narity args_type
          (read_from_closure_given_machtype arg_type (Cvar clos) arg_pos
             (dbg ())
          :: args)
          newclos (n - 1) )

let final_curry_function nlocal arity result =
  let last_arg = V.create_local "arg" in
  let last_clos = V.create_local "clos" in
  let narity = List.length arity in
  let fun_name =
    global_symbol
      (curry_function_sym_name (Lambda.Curried { nlocal }) arity result
      ^ "_"
      ^ Int.to_string (narity - 1))
  in
  let args_type = List.rev arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
    { fun_name;
      fun_args =
        [VP.create last_arg, List.hd args_type; VP.create last_clos, typ_val];
      fun_body =
        make_curry_apply result narity (List.tl args_type) [Cvar last_arg]
          last_clos (narity - 1);
      fun_codegen_options = [];
      fun_dbg;
      fun_poll = Default_poll
    }

let intermediate_curry_functions ~nlocal ~arity result =
  let name1 =
    curry_function_sym_name (Lambda.Curried { nlocal }) arity result
  in
  let narity = List.length arity in
  let dbg = placeholder_dbg in
  let rec loop accumulated_args remaining_args num =
    match remaining_args with
    | [] -> Misc.fatal_error "Empty arity for [intermediate_curry_functions]"
    | [_] -> [final_curry_function nlocal arity result]
    | arg_type :: remaining_args ->
      let name2 = if num = 0 then name1 else name1 ^ "_" ^ Int.to_string num in
      let clos = V.create_local "clos" in
      let args =
        List.init (Array.length arg_type) (fun i ->
            V.create_local "arg", arg_type.(i))
      in
      let fun_dbg = placeholder_fun_dbg ~human_name:name2 in
      let mode : Lambda.alloc_mode =
        if num >= narity - nlocal then Lambda.alloc_local else Lambda.alloc_heap
      in
      let has_nary = curry_clos_has_nary_application ~narity (num + 1) in
      let function_slot_size = if has_nary then 3 else 2 in
      Cfunction
        { fun_name = global_symbol name2;
          fun_args =
            List.map (fun (arg, t) -> VP.create arg, [| t |]) args
            @ [VP.create clos, typ_val];
          fun_body =
            Cop
              ( Calloc mode,
                [ alloc_closure_header ~mode
                    (function_slot_size + machtype_stored_size arg_type + 1)
                    (dbg ());
                  Cconst_symbol
                    ( global_symbol (name1 ^ "_" ^ Int.to_string (num + 1)),
                      dbg () );
                  Cconst_natint
                    ( pack_closure_info
                        ~arity:(if has_nary then narity - num - 1 else 1)
                        ~startenv:
                          (function_slot_size
                          + machtype_non_scanned_size arg_type)
                        ~is_last:true,
                      dbg () ) ]
                @ (if has_nary
                  then
                    [ Cconst_symbol
                        ( global_symbol
                            (name1 ^ "_" ^ Int.to_string (num + 1) ^ "_app"),
                          dbg () ) ]
                  else [])
                @ value_slot_given_machtype args
                @ [Cvar clos],
                dbg () );
          fun_codegen_options = [];
          fun_dbg;
          fun_poll = Default_poll
        }
      ::
      (if has_nary
      then
        let direct_args =
          List.mapi
            (fun i ty ->
              V.create_local (Printf.sprintf "arg%d" (i + num + 2)), ty)
            remaining_args
        in
        let fun_args =
          List.map
            (fun (arg, ty) -> VP.create arg, ty)
            (direct_args @ [clos, typ_val])
        in
        let fun_name =
          global_symbol (name1 ^ "_" ^ Int.to_string (num + 1) ^ "_app")
        in
        let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
        let cf =
          Cfunction
            { fun_name;
              fun_args;
              fun_body =
                make_curry_apply result narity
                  (arg_type :: accumulated_args)
                  (List.map (fun (arg, _) -> Cvar arg) direct_args)
                  clos (num + 1);
              fun_codegen_options = [];
              fun_dbg;
              fun_poll = Default_poll
            }
        in
        [cf]
      else [])
      @ loop (arg_type :: accumulated_args) remaining_args (num + 1)
  in
  loop [] arity 0

let curry_function (kind, arity, return) =
  match kind with
  | Lambda.Tupled -> [tuplify_function arity return]
  | Lambda.Curried { nlocal } ->
    intermediate_curry_functions ~nlocal ~arity return

let default_generic_fns : Cmx_format.generic_fns =
  { curry_fun = [];
    apply_fun =
      [ [typ_val; typ_val], typ_val, Lambda.alloc_heap;
        [typ_val; typ_val; typ_val], typ_val, Lambda.alloc_heap ];
    send_fun = []
  }
(* These apply funs are always present in the main program because the run-time
   system needs them (cf. runtime/<arch>.S) . *)

module Generic_fns_tbl = struct
  type t =
    { curry : (Lambda.function_kind * machtype list * machtype, unit) Hashtbl.t;
      apply : (machtype list * machtype * Lambda.alloc_mode, unit) Hashtbl.t;
      send : (machtype list * machtype * Lambda.alloc_mode, unit) Hashtbl.t
    }

  let make () =
    { curry = Hashtbl.create 10;
      apply = Hashtbl.create 10;
      send = Hashtbl.create 10
    }

  let add_uncached t Cmx_format.{ curry_fun; apply_fun; send_fun } =
    List.iter (fun f -> Hashtbl.replace t.curry f ()) curry_fun;
    List.iter (fun f -> Hashtbl.replace t.apply f ()) apply_fun;
    List.iter (fun f -> Hashtbl.replace t.send f ()) send_fun

  let entries t : Cmx_format.generic_fns =
    let sorted_keys tbl =
      let keys = Hashtbl.fold (fun k () acc -> k :: acc) tbl [] in
      List.sort compare keys
    in
    { curry_fun = sorted_keys t.curry;
      apply_fun = sorted_keys t.apply;
      send_fun = sorted_keys t.send
    }

  module Precomputed = struct
    let check_result = function [| Val |] -> true | _ -> false

    let len_arity arity =
      List.fold_left
        (fun acc a -> match a with [| Val |] -> acc + 1 | _ -> acc)
        0 arity

    let max_send = 20

    let max_apply = 404

    let max_tuplify = 100

    let considered_as_small_threshold = 20

    let is_curry (kind, arity, result) =
      if not (check_result result)
      then false
      else
        match kind with
        | Lambda.Tupled ->
          let l = len_arity arity in
          2 <= l && l <= considered_as_small_threshold
        | Lambda.Curried { nlocal } ->
          let l = len_arity arity in
          let in_bounds = 2 <= l && l <= Lambda.max_arity () in
          if not in_bounds
          then false
          else if nlocal = 0
          then true
          else if nlocal = 1
          then true
          else if nlocal = l
          then true
          else if l <= considered_as_small_threshold
          then true
          else false

    let is_send (arity, result, alloc) =
      if not (check_result result)
      then false
      else
        match alloc with
        | Lambda.Alloc_local -> len_arity arity = 0
        | Lambda.Alloc_heap -> len_arity arity <= max_send

    let is_apply (arity, result, alloc) =
      if not (check_result result)
      then false
      else
        match alloc with
        | Lambda.Alloc_local ->
          let l = len_arity arity in
          2 <= l && l <= considered_as_small_threshold
        | Lambda.Alloc_heap ->
          let l = len_arity arity in
          2 <= l && l <= max_apply

    let gen () =
      (* [is_curry], [is_send] and [is_apply] are also used to determine if a
         generate function was cached. When we generate the cached generated
         functions, we explore the space of all potential candidates and rely on
         these functions to filter out the one that we'll actually generate.
         It's okay to have a search space bigger than needed, however it's not
         okay to have a search space that does not englobe all candidates as it
         will result in weird errors at link-time. We maybe could use Z3 to
         automatically derive a good search space in the future as the filters
         might become more complexed with unboxed types. *)
      assert (considered_as_small_threshold <= max_tuplify);
      assert (considered_as_small_threshold <= max_apply);
      assert (considered_as_small_threshold <= max_send);
      assert (considered_as_small_threshold <= Lambda.max_arity ());
      let arity n = List.init n (fun _ -> [| Val |]) in
      let result = [| Val |] in
      let tuplify =
        List.init max_tuplify (fun n -> Lambda.Tupled, arity n, result)
      in
      let curry =
        List.init (Lambda.max_arity ()) (fun n ->
            List.init (Lambda.max_arity ()) (fun nlocal ->
                Lambda.Curried { nlocal }, arity n, result))
        |> List.concat
      in
      let send =
        List.init max_send (fun n ->
            [ arity n, result, Lambda.alloc_local;
              arity n, result, Lambda.alloc_heap ])
        |> List.concat
      in
      let apply =
        List.init max_apply (fun n ->
            [ arity n, result, Lambda.alloc_local;
              arity n, result, Lambda.alloc_heap ])
        |> List.concat
      in
      let t = make () in
      add_uncached t
        Cmx_format.
          { curry_fun = List.filter is_curry (tuplify @ curry);
            send_fun = List.filter is_send send;
            apply_fun = List.filter is_apply apply
          };
      t
  end

  let add t (Cmx_format.{ curry_fun; apply_fun; send_fun } as f) =
    if !Flambda_backend_flags.use_cached_generic_functions
    then (
      List.iter
        (fun f ->
          if not (Precomputed.is_curry f) then Hashtbl.replace t.curry f ())
        curry_fun;
      List.iter
        (fun f ->
          if not (Precomputed.is_apply f) then Hashtbl.replace t.apply f ())
        apply_fun;
      List.iter
        (fun f ->
          if not (Precomputed.is_send f) then Hashtbl.replace t.send f ())
        send_fun)
    else add_uncached t f

  let of_fns fns =
    let t = make () in
    add t fns;
    t
end

let generic_functions shared tbl =
  if not shared then Generic_fns_tbl.add tbl default_generic_fns;
  let ({ curry_fun; apply_fun; send_fun } : Cmx_format.generic_fns) =
    Generic_fns_tbl.entries tbl
  in
  List.concat_map curry_function curry_fun
  @ List.map send_function send_fun
  @ List.map apply_function apply_fun

(* Primitives *)

type unary_primitive = expression -> Debuginfo.t -> expression

let floatfield n ptr dbg =
  Cop
    ( Cload (Double, Mutable),
      [ (if n = 0
        then ptr
        else Cop (Cadda, [ptr; Cconst_int (n * size_float, dbg)], dbg)) ],
      dbg )

let int_as_pointer arg dbg = Cop (Caddi, [arg; Cconst_int (-1, dbg)], dbg)
(* always a pointer outside the heap *)

let raise_prim raise_kind arg dbg =
  if !Clflags.debug
  then Cop (Craise raise_kind, [arg], dbg)
  else Cop (Craise Lambda.Raise_notrace, [arg], dbg)

let negint arg dbg = Cop (Csubi, [Cconst_int (2, dbg); arg], dbg)

(* [offsetint] moved down to reuse add_int_caml *)

let offsetref n arg dbg =
  return_unit dbg
    (bind "ref" arg (fun arg ->
         Cop
           ( Cstore (Word_int, Assignment),
             [ arg;
               add_const
                 (Cop (Cload (Word_int, Mutable), [arg], dbg))
                 (n lsl 1) dbg ],
             dbg )))

let arraylength kind arg dbg =
  let hdr = get_header_without_profinfo arg dbg in
  match (kind : Lambda.array_kind) with
  | Pgenarray ->
    let len =
      if wordsize_shift = numfloat_shift
      then Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
      else
        bind "header" hdr (fun hdr ->
            Cifthenelse
              ( is_addr_array_hdr hdr dbg,
                dbg,
                Cop (Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg),
                dbg,
                Cop (Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg),
                dbg,
                Any ))
    in
    Cop (Cor, [len; Cconst_int (1, dbg)], dbg)
  | Paddrarray | Pintarray ->
    Cop (Cor, [addr_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)
  | Pfloatarray ->
    Cop (Cor, [float_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)

(* CR-soon gyorsh: effects and coeffects for primitives are set conservatively
   to Arbitrary_effects and Has_coeffects, resp. Check if this can be improved
   (e.g., bswap). *)

let bbswap bi arg dbg =
  let bitwidth : Cmm.bswap_bitwidth =
    match (bi : Primitive.boxed_integer) with
    | Pnativeint -> if size_int = 4 then Thirtytwo else Sixtyfour
    | Pint32 -> Thirtytwo
    | Pint64 -> Sixtyfour
  in
  let op = Cbswap { bitwidth } in
  if (bi = Primitive.Pint64 && size_int = 4)
     || not (Proc.operation_supported op)
  then
    let prim, tyarg =
      match (bi : Primitive.boxed_integer) with
      | Pnativeint -> "nativeint", XInt
      | Pint32 -> "int32", XInt32
      | Pint64 -> "int64", XInt64
    in
    Cop
      ( Cextcall
          { func = Printf.sprintf "caml_%s_direct_bswap" prim;
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty = typ_int;
            alloc = false;
            ty_args = [tyarg]
          },
        [arg],
        dbg )
  else Cop (op, [arg], dbg)

let bswap16 arg dbg =
  let op = Cbswap { bitwidth = Cmm.Sixteen } in
  if Proc.operation_supported op
  then Cop (op, [arg], dbg)
  else
    Cop
      ( Cextcall
          { func = "caml_bswap16_direct";
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects;
            ty = typ_int;
            alloc = false;
            ty_args = []
          },
        [arg],
        dbg )

type binary_primitive = expression -> expression -> Debuginfo.t -> expression

(* let pfield_computed = addr_array_ref *)

(* Helper for compilation of initialization and assignment operations *)

type assignment_kind =
  | Caml_modify
  | Caml_modify_local
  | Caml_initialize (* never local *)
  | Simple of initialization_or_assignment

let assignment_kind (ptr : Lambda.immediate_or_pointer)
    (init : Lambda.initialization_or_assignment) =
  match init, ptr with
  | Assignment Modify_heap, Pointer -> Caml_modify
  | Assignment Modify_maybe_stack, Pointer ->
    assert Config.stack_allocation;
    Caml_modify_local
  | Heap_initialization, _ -> Caml_initialize
  | Assignment _, Immediate -> Simple Assignment
  | Root_initialization, (Immediate | Pointer) -> Simple Initialization

let setfield n ptr init arg1 arg2 dbg =
  match assignment_kind ptr init with
  | Caml_modify ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_modify";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [field_address arg1 n dbg; arg2],
           dbg ))
  | Caml_modify_local ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_modify_local";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [arg1; Cconst_int (n, dbg); arg2],
           dbg ))
  | Caml_initialize ->
    return_unit dbg
      (Cop
         ( Cextcall
             { func = "caml_initialize";
               ty = typ_void;
               alloc = false;
               builtin = false;
               returns = true;
               effects = Arbitrary_effects;
               coeffects = Has_coeffects;
               ty_args = []
             },
           [field_address arg1 n dbg; arg2],
           dbg ))
  | Simple init -> return_unit dbg (set_field arg1 n arg2 init dbg)

let setfloatfield n init arg1 arg2 dbg =
  let init =
    match init with
    | Lambda.Assignment _ -> Assignment
    | Lambda.Heap_initialization | Lambda.Root_initialization -> Initialization
  in
  return_unit dbg
    (Cop
       ( Cstore (Double, init),
         [ (if n = 0
           then arg1
           else Cop (Cadda, [arg1; Cconst_int (n * size_float, dbg)], dbg));
           arg2 ],
         dbg ))

let add_int_caml arg1 arg2 dbg = decr_int (add_int arg1 arg2 dbg) dbg

(* Unary primitive delayed to reuse add_int_caml *)
let offsetint n arg dbg =
  if Misc.no_overflow_lsl n 1
  then add_const arg (n lsl 1) dbg
  else add_int_caml arg (int_const dbg n) dbg

let sub_int_caml arg1 arg2 dbg = incr_int (sub_int arg1 arg2 dbg) dbg

let mul_int_caml arg1 arg2 dbg =
  (* decrementing the non-constant part helps when the multiplication is
     followed by an addition; for example, using this trick compiles

     (100 * a + 7)

     into

     (+ ( * a 100) -85)

     rather than

     (+ ( * 200 (>>s a 1)) 15) *)
  match arg1, arg2 with
  | (Cconst_int _ as c1), c2 ->
    incr_int (mul_int (untag_int c1 dbg) (decr_int c2 dbg) dbg) dbg
  | c1, c2 -> incr_int (mul_int (decr_int c1 dbg) (untag_int c2 dbg) dbg) dbg

let div_int_caml is_safe arg1 arg2 dbg =
  tag_int (div_int (untag_int arg1 dbg) (untag_int arg2 dbg) is_safe dbg) dbg

let mod_int_caml is_safe arg1 arg2 dbg =
  tag_int (mod_int (untag_int arg1 dbg) (untag_int arg2 dbg) is_safe dbg) dbg

let and_int_caml arg1 arg2 dbg = and_int arg1 arg2 dbg

let or_int_caml arg1 arg2 dbg = or_int arg1 arg2 dbg

let xor_int_caml arg1 arg2 dbg =
  Cop
    ( Cor,
      [ xor_int (ignore_low_bit_int arg1) (ignore_low_bit_int arg2) dbg;
        Cconst_int (1, dbg) ],
      dbg )

let lsl_int_caml arg1 arg2 dbg =
  incr_int (lsl_int (decr_int arg1 dbg) (untag_int arg2 dbg) dbg) dbg

let lsr_int_caml arg1 arg2 dbg =
  Cop (Cor, [lsr_int arg1 (untag_int arg2 dbg) dbg; Cconst_int (1, dbg)], dbg)

let asr_int_caml arg1 arg2 dbg =
  Cop (Cor, [asr_int arg1 (untag_int arg2 dbg) dbg; Cconst_int (1, dbg)], dbg)

let int_comp_caml cmp arg1 arg2 dbg =
  tag_int (Cop (Ccmpi cmp, [arg1; arg2], dbg)) dbg

let stringref_unsafe arg1 arg2 dbg =
  tag_int
    (Cop
       ( Cload (Byte_unsigned, Mutable),
         [add_int arg1 (untag_int arg2 dbg) dbg],
         dbg ))
    dbg

let stringref_safe arg1 arg2 dbg =
  tag_int
    (bind "index" (untag_int arg2 dbg) (fun idx ->
         bind "str" arg1 (fun str ->
             Csequence
               ( make_checkbound dbg [string_length str dbg; idx],
                 Cop (Cload (Byte_unsigned, Mutable), [add_int str idx dbg], dbg)
               ))))
    dbg

let string_load size unsafe mode arg1 arg2 dbg =
  box_sized size mode dbg
    (bind "index" (untag_int arg2 dbg) (fun idx ->
         bind "str" arg1 (fun str ->
             check_bound unsafe size dbg (string_length str dbg) idx
               (unaligned_load size str idx dbg))))

let bigstring_load size unsafe mode arg1 arg2 dbg =
  box_sized size mode dbg
    (bind "index" (untag_int arg2 dbg) (fun idx ->
         bind "ba" arg1 (fun ba ->
             bind "ba_data"
               (Cop (Cload (Word_int, Mutable), [field_address ba 1 dbg], dbg))
               (fun ba_data ->
                 check_bound unsafe size dbg (bigstring_length ba dbg) idx
                   (unaligned_load size ba_data idx dbg)))))

let arrayref_unsafe rkind arg1 arg2 dbg =
  match (rkind : Lambda.array_ref_kind) with
  | Pgenarray_ref mode ->
    bind "index" arg2 (fun idx ->
        bind "arr" arg1 (fun arr ->
            Cifthenelse
              ( is_addr_array_ptr arr dbg,
                dbg,
                addr_array_ref arr idx dbg,
                dbg,
                float_array_ref mode arr idx dbg,
                dbg,
                Any )))
  | Paddrarray_ref -> addr_array_ref arg1 arg2 dbg
  | Pintarray_ref ->
    (* CR mshinwell: for int/addr_array_ref move "dbg" to first arg *)
    int_array_ref arg1 arg2 dbg
  | Pfloatarray_ref mode -> float_array_ref mode arg1 arg2 dbg

let arrayref_safe rkind arg1 arg2 dbg =
  match (rkind : Lambda.array_ref_kind) with
  | Pgenarray_ref mode ->
    bind "index" arg2 (fun idx ->
        bind "arr" arg1 (fun arr ->
            bind "header" (get_header_without_profinfo arr dbg) (fun hdr ->
                if wordsize_shift = numfloat_shift
                then
                  Csequence
                    ( make_checkbound dbg
                        [addr_array_length_shifted hdr dbg; idx],
                      Cifthenelse
                        ( is_addr_array_hdr hdr dbg,
                          dbg,
                          addr_array_ref arr idx dbg,
                          dbg,
                          float_array_ref mode arr idx dbg,
                          dbg,
                          Any ) )
                else
                  Cifthenelse
                    ( is_addr_array_hdr hdr dbg,
                      dbg,
                      Csequence
                        ( make_checkbound dbg
                            [addr_array_length_shifted hdr dbg; idx],
                          addr_array_ref arr idx dbg ),
                      dbg,
                      Csequence
                        ( make_checkbound dbg
                            [float_array_length_shifted hdr dbg; idx],
                          float_array_ref mode arr idx dbg ),
                      dbg,
                      Any ))))
  | Paddrarray_ref ->
    bind "index" arg2 (fun idx ->
        bind "arr" arg1 (fun arr ->
            Csequence
              ( make_checkbound dbg
                  [ addr_array_length_shifted
                      (get_header_without_profinfo arr dbg)
                      dbg;
                    idx ],
                addr_array_ref arr idx dbg )))
  | Pintarray_ref ->
    bind "index" arg2 (fun idx ->
        bind "arr" arg1 (fun arr ->
            Csequence
              ( make_checkbound dbg
                  [ addr_array_length_shifted
                      (get_header_without_profinfo arr dbg)
                      dbg;
                    idx ],
                int_array_ref arr idx dbg )))
  | Pfloatarray_ref mode ->
    box_float dbg mode
      (bind "index" arg2 (fun idx ->
           bind "arr" arg1 (fun arr ->
               Csequence
                 ( make_checkbound dbg
                     [ float_array_length_shifted
                         (get_header_without_profinfo arr dbg)
                         dbg;
                       idx ],
                   unboxed_float_array_ref arr idx dbg ))))

type ternary_primitive =
  expression -> expression -> expression -> Debuginfo.t -> expression

let setfield_computed ptr init arg1 arg2 arg3 dbg =
  match assignment_kind ptr init with
  | Caml_modify -> return_unit dbg (addr_array_set_heap arg1 arg2 arg3 dbg)
  | Caml_modify_local ->
    return_unit dbg (addr_array_set_local arg1 arg2 arg3 dbg)
  | Caml_initialize ->
    return_unit dbg (addr_array_initialize arg1 arg2 arg3 dbg)
  | Simple _ -> return_unit dbg (int_array_set arg1 arg2 arg3 dbg)

let bytesset_unsafe arg1 arg2 arg3 dbg =
  return_unit dbg
    (Cop
       ( Cstore (Byte_unsigned, Assignment),
         [ add_int arg1 (untag_int arg2 dbg) dbg;
           ignore_high_bit_int (untag_int arg3 dbg) ],
         dbg ))

let bytesset_safe arg1 arg2 arg3 dbg =
  return_unit dbg
    (bind "newval" (untag_int arg3 dbg) (fun newval ->
         bind "index" (untag_int arg2 dbg) (fun idx ->
             bind "str" arg1 (fun str ->
                 Csequence
                   ( make_checkbound dbg [string_length str dbg; idx],
                     Cop
                       ( Cstore (Byte_unsigned, Assignment),
                         [add_int str idx dbg; ignore_high_bit_int newval],
                         dbg ) )))))

let arrayset_unsafe skind arg1 arg2 arg3 dbg =
  return_unit dbg
    (match (skind : Lambda.array_set_kind) with
    | Pgenarray_set mode ->
      bind "newval" arg3 (fun newval ->
          bind "index" arg2 (fun index ->
              bind "arr" arg1 (fun arr ->
                  Cifthenelse
                    ( is_addr_array_ptr arr dbg,
                      dbg,
                      addr_array_set mode arr index newval dbg,
                      dbg,
                      float_array_set arr index (unbox_float dbg newval) dbg,
                      dbg,
                      Any ))))
    | Paddrarray_set mode -> addr_array_set mode arg1 arg2 arg3 dbg
    | Pintarray_set -> int_array_set arg1 arg2 arg3 dbg
    | Pfloatarray_set -> float_array_set arg1 arg2 arg3 dbg)

let arrayset_safe skind arg1 arg2 arg3 dbg =
  return_unit dbg
    (match (skind : Lambda.array_set_kind) with
    | Pgenarray_set mode ->
      bind "newval" arg3 (fun newval ->
          bind "index" arg2 (fun idx ->
              bind "arr" arg1 (fun arr ->
                  bind "header" (get_header_without_profinfo arr dbg)
                    (fun hdr ->
                      if wordsize_shift = numfloat_shift
                      then
                        Csequence
                          ( make_checkbound dbg
                              [addr_array_length_shifted hdr dbg; idx],
                            Cifthenelse
                              ( is_addr_array_hdr hdr dbg,
                                dbg,
                                addr_array_set mode arr idx newval dbg,
                                dbg,
                                float_array_set arr idx (unbox_float dbg newval)
                                  dbg,
                                dbg,
                                Any ) )
                      else
                        Cifthenelse
                          ( is_addr_array_hdr hdr dbg,
                            dbg,
                            Csequence
                              ( make_checkbound dbg
                                  [addr_array_length_shifted hdr dbg; idx],
                                addr_array_set mode arr idx newval dbg ),
                            dbg,
                            Csequence
                              ( make_checkbound dbg
                                  [float_array_length_shifted hdr dbg; idx],
                                float_array_set arr idx (unbox_float dbg newval)
                                  dbg ),
                            dbg,
                            Any )))))
    | Paddrarray_set mode ->
      bind "newval" arg3 (fun newval ->
          bind "index" arg2 (fun idx ->
              bind "arr" arg1 (fun arr ->
                  Csequence
                    ( make_checkbound dbg
                        [ addr_array_length_shifted
                            (get_header_without_profinfo arr dbg)
                            dbg;
                          idx ],
                      addr_array_set mode arr idx newval dbg ))))
    | Pintarray_set ->
      bind "newval" arg3 (fun newval ->
          bind "index" arg2 (fun idx ->
              bind "arr" arg1 (fun arr ->
                  Csequence
                    ( make_checkbound dbg
                        [ addr_array_length_shifted
                            (get_header_without_profinfo arr dbg)
                            dbg;
                          idx ],
                      int_array_set arr idx newval dbg ))))
    | Pfloatarray_set ->
      bind_load "newval" arg3 (fun newval ->
          bind "index" arg2 (fun idx ->
              bind "arr" arg1 (fun arr ->
                  Csequence
                    ( make_checkbound dbg
                        [ float_array_length_shifted
                            (get_header_without_profinfo arr dbg)
                            dbg;
                          idx ],
                      float_array_set arr idx newval dbg )))))

let bytes_set size unsafe arg1 arg2 arg3 dbg =
  return_unit dbg
    (bind "newval" arg3 (fun newval ->
         bind "index" (untag_int arg2 dbg) (fun idx ->
             bind "str" arg1 (fun str ->
                 check_bound unsafe size dbg (string_length str dbg) idx
                   (unaligned_set size str idx newval dbg)))))

let bigstring_set size unsafe arg1 arg2 arg3 dbg =
  return_unit dbg
    (bind "newval" arg3 (fun newval ->
         bind "index" (untag_int arg2 dbg) (fun idx ->
             bind "ba" arg1 (fun ba ->
                 bind "ba_data"
                   (Cop
                      (Cload (Word_int, Mutable), [field_address ba 1 dbg], dbg))
                   (fun ba_data ->
                     check_bound unsafe size dbg (bigstring_length ba dbg) idx
                       (unaligned_set size ba_data idx newval dbg))))))

(* Symbols *)

let cdefine_symbol sym = [Cdefine_symbol sym]

let emit_block symb white_header cont =
  (* Headers for structured constants must be marked black in case we are in
     no-naked-pointers mode. See [caml_darken]. *)
  let black_header = Nativeint.logor white_header caml_black in
  (Cint black_header :: cdefine_symbol symb) @ cont

let emit_string_constant_fields s cont =
  let n = size_int - 1 - (String.length s mod size_int) in
  Cstring s :: Cskip n :: Cint8 n :: cont

let emit_boxed_int32_constant_fields n cont =
  let n = Nativeint.of_int32 n in
  if size_int = 8
  then
    Csymbol_address (global_symbol caml_int32_ops)
    :: Cint32 n :: Cint32 0n :: cont
  else Csymbol_address (global_symbol caml_int32_ops) :: Cint n :: cont

let emit_boxed_int64_constant_fields n cont =
  let lo = Int64.to_nativeint n in
  if size_int = 8
  then Csymbol_address (global_symbol caml_int64_ops) :: Cint lo :: cont
  else
    let hi = Int64.to_nativeint (Int64.shift_right n 32) in
    if big_endian
    then
      Csymbol_address (global_symbol caml_int64_ops)
      :: Cint hi :: Cint lo :: cont
    else
      Csymbol_address (global_symbol caml_int64_ops)
      :: Cint lo :: Cint hi :: cont

let emit_boxed_nativeint_constant_fields n cont =
  Csymbol_address (global_symbol caml_nativeint_ops) :: Cint n :: cont

let emit_float_constant symb f cont =
  emit_block symb float_header (Cdouble f :: cont)

let emit_string_constant symb s cont =
  emit_block symb
    (string_header (String.length s))
    (emit_string_constant_fields s cont)

let emit_int32_constant symb n cont =
  emit_block symb boxedint32_header (emit_boxed_int32_constant_fields n cont)

let emit_int64_constant symb n cont =
  emit_block symb boxedint64_header (emit_boxed_int64_constant_fields n cont)

let emit_nativeint_constant symb n cont =
  emit_block symb boxedintnat_header
    (emit_boxed_nativeint_constant_fields n cont)

let emit_vec128_constant symb bits cont =
  emit_block symb boxedvec128_header (Cvec128 bits :: cont)

let emit_float_array_constant symb fields cont =
  emit_block symb
    (floatarray_header (List.length fields))
    (Misc.map_end (fun f -> Cdouble f) fields cont)

let make_symbol ?compilation_unit name =
  let compilation_unit =
    match compilation_unit with
    | None -> Compilation_unit.get_current_exn ()
    | Some compilation_unit -> compilation_unit
  in
  Symbol.for_name compilation_unit name
  |> Symbol.linkage_name |> Linkage_name.to_string

(* Generate the entry point *)
(*
 * CAMLprim value caml_program()
 * {
 *   int id = 0;
 *   while (true) {
 *     if (id == len_caml_globals_entry_functions) goto out;
 *     caml_globals_entry_functions[id]();
 *     caml_globals_inited += 1;
 *     id += 1;
 *   }
 *   out:
 *   return 1;
 * }
 *)
let entry_point namelist =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let cconst_symbol sym = Cconst_symbol (sym, dbg ()) in
  let incr_global_inited () =
    Cop
      ( Cstore (Word_int, Assignment),
        [ cconst_symbol (global_symbol "caml_globals_inited");
          Cop
            ( Caddi,
              [ Cop
                  ( Cload (Word_int, Mutable),
                    [cconst_symbol (global_symbol "caml_globals_inited")],
                    dbg () );
                cconst_int 1 ],
              dbg () ) ],
        dbg () )
  in
  let table_symbol = global_symbol "caml_globals_entry_functions" in
  let call i =
    let f =
      Cop
        ( Cadda,
          [ cconst_symbol table_symbol;
            Cop (Cmuli, [Cconst_int (Arch.size_addr, dbg ()); i], dbg ()) ],
          dbg () )
    in
    Csequence
      ( Cop
          ( Capply (typ_void, Rc_normal),
            [Cop (Cload (Word_int, Immutable), [f], dbg ())],
            dbg () ),
        incr_global_inited () )
  in
  let data =
    List.map
      (fun name ->
        Csymbol_address
          (global_symbol (make_symbol ~compilation_unit:name "entry")))
      namelist
  in
  let data = Cdefine_symbol table_symbol :: data in
  let raise_num = Lambda.next_raise_count () in
  let id = VP.create (Ident.create_local "*id*") in
  let high = cconst_int (List.length namelist) in
  let body =
    let dbg = dbg () in
    let incr_i =
      Cassign
        (VP.var id, Cop (Caddi, [Cvar (VP.var id); Cconst_int (1, dbg)], dbg))
    in
    let exit_if_last_iteration =
      Cifthenelse
        ( Cop (Ccmpi Ceq, [Cvar (VP.var id); high], dbg),
          dbg,
          Cexit (Lbl raise_num, [], []),
          dbg,
          Ctuple [],
          dbg,
          Any )
    in
    Clet_mut
      ( id,
        typ_int,
        cconst_int 0,
        ccatch
          ( raise_num,
            [],
            create_loop
              (Csequence
                 ( exit_if_last_iteration,
                   Csequence (call (Cvar (VP.var id)), incr_i) ))
              dbg,
            Ctuple [],
            dbg,
            Any ) )
  in
  let fun_name = global_symbol "caml_program" in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  [ Cdata data;
    Cfunction
      { fun_name;
        fun_args = [];
        fun_body = Csequence (body, cconst_int 1);
        fun_codegen_options = [Reduce_code_size; Use_linscan_regalloc];
        fun_dbg;
        fun_poll = Default_poll
      } ]

(* Generate the table of globals *)

let cint_zero = Cint 0n

let global_table namelist =
  let mksym name =
    Csymbol_address
      (global_symbol (make_symbol ~compilation_unit:name "gc_roots"))
  in
  Cdata
    ((Cdefine_symbol (global_symbol "caml_globals") :: List.map mksym namelist)
    @ [cint_zero])

let reference_symbols namelist =
  let mksym name = Csymbol_address name in
  Cdata (List.map mksym namelist)

let global_data sym_name v =
  let symbol = { sym_name; sym_global = Global } in
  Cdata (emit_string_constant symbol (Marshal.to_string v []) [])

let globals_map v = global_data "caml_globals_map" v

(* Generate the master table of frame descriptors *)

let frame_table namelist =
  let mksym name =
    Csymbol_address
      (global_symbol (make_symbol ~compilation_unit:name "frametable"))
  in
  Cdata
    (Cdefine_symbol (global_symbol "caml_frametable")
     :: List.map mksym namelist
    @ [cint_zero])

(* Generate the table of module data and code segments *)

let segment_table namelist symbol begname endname =
  let addsyms name lst =
    Csymbol_address (global_symbol (make_symbol ~compilation_unit:name begname))
    :: Csymbol_address
         (global_symbol (make_symbol ~compilation_unit:name endname))
    :: lst
  in
  Cdata
    (Cdefine_symbol (global_symbol symbol)
    :: List.fold_right addsyms namelist [cint_zero])

let data_segment_table namelist =
  segment_table namelist "caml_data_segments" "data_begin" "data_end"

let code_segment_table namelist =
  segment_table namelist "caml_code_segments" "code_begin" "code_end"

(* Initialize a predefined exception *)

let predef_exception i name =
  let name_sym =
    { sym_name = Compilenv.new_const_symbol (); sym_global = Local }
  in
  let data_items = emit_string_constant name_sym name [] in
  let exn_sym = global_symbol ("caml_exn_" ^ name) in
  let tag = Obj.object_tag in
  let size = 2 in
  let fields = Csymbol_address name_sym :: cint_const (-i - 1) :: data_items in
  let data_items = emit_block exn_sym (block_header tag size) fields in
  Cdata data_items

(* Header for a plugin *)

let plugin_header units =
  global_data "caml_plugin_header"
    ({ dynu_magic = Config.cmxs_magic_number; dynu_units = units }
      : Cmxs_format.dynheader)

(* To compile "let rec" over values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun (f : Clambda.ufunction) ->
      let indirect_call_code_pointer_size =
        match f.arity with
        | { function_kind = Curried _; params_layout = [] | [_]; _ } ->
          0
          (* arity 1 does not need an indirect call handler. arity 0 cannot be
             indirect called *)
        | _ -> 1
        (* For other arities there is an indirect call handler.

           if arity >= 2 it is caml_curry...

           if arity < 0 it is caml_tuplify... *)
      in
      sz := !sz + 1 + 2 + indirect_call_code_pointer_size)
    fundecls;
  !sz

(* Emit constant closures *)

let emit_constant_closure symb fundecls clos_vars cont =
  let closure_symbol (f : Clambda.ufunction) =
    if Config.flambda
    then
      cdefine_symbol
        { sym_name = f.label ^ "_closure"; sym_global = symb.sym_global }
    else []
  in
  match (fundecls : Clambda.ufunction list) with
  | [] ->
    (* This should probably not happen: dead code has normally been eliminated
       and a closure cannot be accessed without going through a
       [Project_closure], which depends on the function. *)
    assert (clos_vars = []);
    cdefine_symbol symb @ clos_vars @ cont
  | f1 :: remainder -> (
    let startenv = fundecls_size fundecls in
    let rec emit_others pos = function
      | [] -> clos_vars @ cont
      | (f2 : Clambda.ufunction) :: rem -> (
        let is_last = match rem with [] -> true | _ :: _ -> false in
        match f2.arity with
        | { function_kind = Curried _; params_layout = [] | [_]; _ } as arity ->
          (Cint (infix_header pos) :: closure_symbol f2)
          @ Csymbol_address
              { sym_name = f2.label; sym_global = symb.sym_global }
            :: Cint (closure_info ~arity ~startenv:(startenv - pos) ~is_last)
            :: emit_others (pos + 3) rem
        | arity ->
          (* See note in the apply function code about the conversion from
             tagged integer to value machtypes. *)
          let params_machtypes =
            List.map machtype_of_layout_changing_tagged_int_to_val
              arity.params_layout
          in
          let return_machtype =
            machtype_of_layout_changing_tagged_int_to_val arity.return_layout
          in
          (Cint (infix_header pos) :: closure_symbol f2)
          @ Csymbol_address
              (curry_function_sym arity.function_kind params_machtypes
                 return_machtype)
            :: Cint (closure_info ~arity ~startenv:(startenv - pos) ~is_last)
            :: Csymbol_address
                 { sym_name = f2.label; sym_global = symb.sym_global }
            :: emit_others (pos + 4) rem)
    in
    let is_last = match remainder with [] -> true | _ :: _ -> false in
    Cint (black_closure_header (fundecls_size fundecls + List.length clos_vars))
    :: cdefine_symbol symb
    @ closure_symbol f1
    @
    match f1.arity with
    | { function_kind = Curried _; params_layout = [] | [_]; _ } as arity ->
      Csymbol_address { sym_name = f1.label; sym_global = symb.sym_global }
      :: Cint (closure_info ~arity ~startenv ~is_last)
      :: emit_others 3 remainder
    | arity ->
      Csymbol_address
        (curry_function_sym arity.function_kind
           (List.map machtype_of_layout_changing_tagged_int_to_val
              arity.params_layout)
           (machtype_of_layout_changing_tagged_int_to_val arity.return_layout))
      :: Cint (closure_info ~arity ~startenv ~is_last)
      :: Csymbol_address { sym_name = f1.label; sym_global = symb.sym_global }
      :: emit_others 4 remainder)

(* Build the NULL terminated array of gc roots *)

let emit_gc_roots_table ~symbols cont =
  let table_symbol = global_symbol (make_symbol "gc_roots") in
  Cdata
    (Cdefine_symbol table_symbol
     :: List.map (fun s -> Csymbol_address s) symbols
    @ [Cint 0n])
  :: cont

(* Build preallocated blocks (used for Flambda [Initialize_symbol] constructs,
   and Clambda global module) *)

let preallocate_block cont { Clambda.symbol; exported; tag; fields } =
  let mksym sym_name =
    { sym_name; sym_global = (if exported then Global else Local) }
  in
  let space =
    (* These words will be registered as roots and as such must contain valid
       values, in case we are in no-naked-pointers mode. Likewise the block
       header must be black, below (see [caml_darken]), since the overall record
       may be referenced. *)
    List.map
      (fun field ->
        match field with
        | None -> Cint (Nativeint.of_int 1 (* Val_unit *))
        | Some (Clambda.Uconst_field_int n) -> cint_const n
        | Some (Clambda.Uconst_field_ref label) -> Csymbol_address (mksym label))
      fields
  in
  let data =
    emit_block (mksym symbol) (block_header tag (List.length fields)) space
  in
  Cdata data :: cont

let emit_preallocated_blocks preallocated_blocks cont =
  let symbols =
    List.map
      (fun ({ Clambda.symbol; exported } : Clambda.preallocated_block) ->
        { sym_name = symbol; sym_global = (if exported then Global else Local) })
      preallocated_blocks
  in
  let c1 = emit_gc_roots_table ~symbols cont in
  List.fold_left preallocate_block c1 preallocated_blocks

(* Helper functions and values used by Flambda 2. *)

let typ_int64 =
  match Arch.size_int with
  | 4 -> [| Cmm.Int; Cmm.Int |]
  | 8 -> [| Cmm.Int |]
  | _ -> Misc.fatal_errorf "Unsupported Arch.size_int = %d" Arch.size_int

let void = Ctuple []

let unit ~dbg = Cconst_int (1, dbg)

let var v = Cvar v

let symbol ~dbg sym = Cconst_symbol (sym, dbg)

let float ~dbg f = Cconst_float (f, dbg)

(* CR Gbury: this conversion int -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int ~dbg i = natint_const_untagged dbg (Nativeint.of_int i)

let int32 ~dbg i = natint_const_untagged dbg (Nativeint.of_int32 i)

(* CR Gbury: this conversion int64 -> nativeint is potentially unsafe when
   cross-compiling for 64-bit on a 32-bit host *)
let int64 ~dbg i = natint_const_untagged dbg (Int64.to_nativeint i)

let vec128 ~dbg bits = Cconst_vec128 (bits, dbg)

let nativeint ~dbg i = natint_const_untagged dbg i

let letin v ~defining_expr ~body =
  match body with
  | Cvar v' when Backend_var.same (Backend_var.With_provenance.var v) v' ->
    defining_expr
  | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cconst_vec128 _ | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _
  | Ctuple _ | Cop _ | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _
  | Cexit _ | Ctrywith _ | Cregion _ | Ctail _ ->
    Clet (v, defining_expr, body)

let letin_mut v ty e body = Clet_mut (v, ty, e, body)

let assign x e = Cassign (x, e)

let sequence x y =
  match x, y with
  | Ctuple [], _ -> y
  | _, Ctuple [] -> x
  | _, _ -> Csequence (x, y)

let ite ~dbg ~then_dbg ~then_ ~else_dbg ~else_ cond =
  Cifthenelse (cond, then_dbg, then_, else_dbg, else_, dbg, Any)

let trywith ~dbg ~kind ~body ~exn_var ~handler () =
  Ctrywith (body, kind, exn_var, handler, dbg, Any)

type static_handler =
  int
  * (Backend_var.With_provenance.t * Cmm.machtype) list
  * Cmm.expression
  * Debuginfo.t

let handler ~dbg id vars body = id, vars, body, dbg

let cexit id args trap_actions = Cmm.Cexit (Cmm.Lbl id, args, trap_actions)

let trap_return arg trap_actions =
  Cmm.Cexit (Cmm.Return_lbl, [arg], trap_actions)

let create_ccatch ~rec_flag ~handlers ~body =
  let rec_flag = if rec_flag then Cmm.Recursive else Cmm.Nonrecursive in
  Cmm.Ccatch (rec_flag, handlers, body, Any)

let unary op ~dbg x = Cop (op, [x], dbg)

let binary op ~dbg x y = Cop (op, [x; y], dbg)

let int_of_float = unary Cintoffloat

let float_of_int = unary Cfloatofint

let lsl_int_caml_raw ~dbg arg1 arg2 =
  incr_int (lsl_int (decr_int arg1 dbg) arg2 dbg) dbg

let lsr_int_caml_raw ~dbg arg1 arg2 =
  Cop (Cor, [lsr_int arg1 arg2 dbg; Cconst_int (1, dbg)], dbg)

let asr_int_caml_raw ~dbg arg1 arg2 =
  Cop (Cor, [asr_int arg1 arg2 dbg; Cconst_int (1, dbg)], dbg)

let eq ~dbg x y =
  match x, y with
  | Cconst_int (n, _), Cop (Csubi, [Cconst_int (m, _); c], _)
  | Cop (Csubi, [Cconst_int (m, _); c], _), Cconst_int (n, _)
    when Misc.no_overflow_sub m n ->
    (* [n = m - c] <=> [c = m - n]

       This is typically generated by expressions of the form [if not expr then
       ...], with [not expr] being compiled to [4 - c] and the condition for the
       test becomes [1 = 4 - c].

       We need to impose the side condition because the above equivalence hides
       a subtlety: While [c] is a full-blooded native integer, [m] and [n] are
       OCaml ints that will be sign-extended between now and run time. That in
       itself doesn't break the equivalence. The problem is that we intend to
       compute [m - n] right now, while [m] and [n] are one bit shorter. Thus
       there's a bit of sleight of hand going on: the [m - n] we compute now may
       not be the [m - n] that appears in the equivalence. [m - c], however,
       _is_ subtraction of full native ints (it must be, since [c] can be any
       native int). So [m - c] and [m - n] refer to two different operations and
       we're cheekily swapping one for the other. We'll get away with it,
       however, _so long as [m - n] doesn't overflow_.

       Formally, writing [se] for sign extension, we can write a version of our
       equivalence that's unconditionally true: [se(n) = se(m) - c] <=> [c =
       se(m) - se(n)], where now [-] consistently means subtraction of native
       ints. Effectively, we intend to write [c = se(m - n)] in the compiled
       code (here [-] is instead subtraction of OCaml ints). This is the same as
       [c = se(m) - se(n)] exactly when [se(m - n) = se(m) - se(n)], which is
       another way of saying that [m - n] doesn't overflow.

       The following z3 script confirms that this check is sufficient: *)
    (*
     *   (define-sort int63 () (_ BitVec 63))
     *   (define-sort int64 () (_ BitVec 64))
     *   (define-const z63 int63 ((_ int2bv 63) 0))
     *
     *   (declare-const m int63)
     *   (declare-const n int63)
     *   (declare-const c int64)
     *
     *   ; let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0
     *   (define-fun no_overflow_sub ((a int63) (b int63)) Bool
     *     (bvslt (bvor (bvxor a (bvnot b)) (bvxor b (bvsub a b))) z63))
     *
     *   (assert (no_overflow_sub m n))
     *
     *   (assert (not (=
     *     (= ((_ sign_extend 1) n) (bvsub ((_ sign_extend 1) m) c))
     *     (= c ((_ sign_extend 1) (bvsub m n)))
     *   )))
     *
     *   (check-sat)
     *)
    binary (Ccmpi Ceq) ~dbg c (Cconst_int (m - n, dbg))
  | _, _ -> binary (Ccmpi Ceq) ~dbg x y

let neq = binary (Ccmpi Cne)

let lt = binary (Ccmpi Clt)

let le = binary (Ccmpi Cle)

let gt = binary (Ccmpi Cgt)

let ge = binary (Ccmpi Cge)

let ult = binary (Ccmpa Clt)

let ule = binary (Ccmpa Cle)

let ugt = binary (Ccmpa Cgt)

let uge = binary (Ccmpa Cge)

let float_abs = unary Cabsf

let float_neg = unary Cnegf

let float_add = binary Caddf

let float_sub = binary Csubf

let float_mul = binary Cmulf

let float_div = binary Cdivf

let float_eq = binary (Ccmpf CFeq)

let float_neq = binary (Ccmpf CFneq)

let float_lt = binary (Ccmpf CFlt)

let float_le = binary (Ccmpf CFle)

let float_gt = binary (Ccmpf CFgt)

let float_ge = binary (Ccmpf CFge)

let beginregion ~dbg = Cop (Cbeginregion, [], dbg)

let endregion ~dbg region = Cop (Cendregion, [region], dbg)

let probe ~dbg ~name ~handler_code_linkage_name ~enabled_at_init ~args =
  Cop
    ( Cprobe
        { name; handler_code_sym = handler_code_linkage_name; enabled_at_init },
      args,
      dbg )

let load ~dbg kind mut ~addr = Cop (Cload (kind, mut), [addr], dbg)

let store ~dbg kind init ~addr ~new_value =
  Cop (Cstore (kind, init), [addr; new_value], dbg)

let direct_call ~dbg ty pos f_code_sym args =
  Cop (Capply (ty, pos), f_code_sym :: args, dbg)

let indirect_call ~dbg ty pos alloc_mode f args_type args =
  match args with
  | [arg] ->
    (* Use a variable to avoid duplicating the cmm code of the closure [f]. *)
    let v = Backend_var.create_local "*closure*" in
    let v' = Backend_var.With_provenance.create v in
    letin v' ~defining_expr:f
      ~body:
        (Cop
           ( Capply (Extended_machtype.to_machtype ty, pos),
             [load ~dbg Word_int Asttypes.Mutable ~addr:(Cvar v); arg; Cvar v],
             dbg ))
  | args ->
    call_caml_apply ty args_type Asttypes.Mutable f args pos alloc_mode dbg

let indirect_full_call ~dbg ty pos alloc_mode f args_type = function
  (* the single-argument case is already optimized by indirect_call *)
  | [_] as args -> indirect_call ~dbg ty pos alloc_mode f args_type args
  | args ->
    (* Use a variable to avoid duplicating the cmm code of the closure [f]. *)
    let v = Backend_var.create_local "*closure*" in
    let v' = Backend_var.With_provenance.create v in
    (* get the function's code pointer *)
    let fun_ptr =
      load ~dbg Word_int Asttypes.Mutable ~addr:(field_address (Cvar v) 2 dbg)
    in
    letin v' ~defining_expr:f
      ~body:
        (Cop
           ( Capply (Extended_machtype.to_machtype ty, pos),
             (fun_ptr :: args) @ [Cvar v],
             dbg ))

let bigarray_load ~dbg ~elt_kind ~elt_size ~elt_chunk ~bigarray ~index =
  let ba_data_f = field_address bigarray 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p index dbg
  in
  match (elt_kind : Lambda.bigarray_kind) with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int ~dbg (elt_size / 2)) in
    box_complex dbg
      (load ~dbg elt_chunk Mutable ~addr)
      (load ~dbg elt_chunk Mutable ~addr:addr')
  | _ ->
    (* Note that no sign extension operation is necessary here: if the element
       type of the bigarray is signed, then the backend will emit a
       sign-extending load instruction. *)
    load ~dbg elt_chunk Mutable ~addr

let bigarray_store ~dbg ~(elt_kind : Lambda.bigarray_kind) ~elt_size ~elt_chunk
    ~bigarray ~index ~new_value =
  let ba_data_f = field_address bigarray 1 dbg in
  let ba_data_p = load ~dbg Word_int Mutable ~addr:ba_data_f in
  let addr =
    array_indexing ~typ:Addr (Misc.log2 elt_size) ba_data_p index dbg
  in
  match elt_kind with
  | Pbigarray_complex32 | Pbigarray_complex64 ->
    let addr' = binary Cadda ~dbg addr (int ~dbg (elt_size / 2)) in
    return_unit dbg
      (sequence
         (store ~dbg elt_chunk Assignment ~addr
            ~new_value:(complex_re new_value dbg))
         (store ~dbg elt_chunk Assignment ~addr:addr'
            ~new_value:(complex_im new_value dbg)))
  | _ -> return_unit dbg (store ~dbg elt_chunk Assignment ~addr ~new_value)

(* Infix field address. Contrary to regular field addresses, these addresses are
   valid ocaml values, and can be live at gc points. *)

let infix_field_address ~dbg ptr n =
  if n = 0
  then ptr
  else Cmm.Cop (Cmm.Caddv, [ptr; int ~dbg (n * Arch.size_addr)], dbg)

(* Data items *)

let cint i = Cmm.Cint i

let cfloat f = Cmm.Cdouble f

let cvec128 bits = Cmm.Cvec128 bits

let symbol_address s = Cmm.Csymbol_address s

let define_symbol symbol = [Cdefine_symbol symbol]

(* Cmm phrases *)

let cfunction decl = Cmm.Cfunction decl

let cdata d = Cmm.Cdata d

let fundecl fun_name fun_args fun_body fun_codegen_options fun_dbg fun_poll =
  { Cmm.fun_name; fun_args; fun_body; fun_codegen_options; fun_dbg; fun_poll }

(* Gc root table *)

let gc_root_table syms =
  let table_symbol = make_symbol ?compilation_unit:None "gc_roots" in
  cdata
    (define_symbol { sym_name = table_symbol; sym_global = Global }
    @ List.map symbol_address syms
    @ [cint 0n])

let cmm_arith_size (e : Cmm.expression) =
  let rec cmm_arith_size0 (e : Cmm.expression) =
    match e with
    | Cop
        ( ( Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor
          | Clsl | Clsr | Casr ),
          l,
          _ ) ->
      List.fold_left ( + ) 1 (List.map cmm_arith_size0 l)
    | _ -> 0
  in
  match e with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _ | Cvar _
  | Cconst_vec128 _ ->
    Some 0
  | Cop _ -> Some (cmm_arith_size0 e)
  | Clet _ | Clet_mut _ | Cphantom_let _ | Cassign _ | Ctuple _ | Csequence _
  | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ | Cregion _
  | Ctail _ ->
    None

let transl_property : Lambda.property -> Cmm.property = function
  | Zero_alloc -> Zero_alloc

let transl_attrib : Lambda.check_attribute -> Cmm.codegen_option list = function
  | Default_check -> []
  | Ignore_assert_all p -> [Ignore_assert_all (transl_property p)]
  | Check { property; strict; assume; loc } ->
    [Check { property = transl_property property; strict; assume; loc }]

let kind_of_layout (layout : Lambda.layout) =
  match layout with
  | Pvalue Pfloatval -> Boxed_float
  | Pvalue (Pboxedintval bi) -> Boxed_integer bi
  | Pvalue (Pboxedvectorval vi) -> Boxed_vector vi
  | Pvalue (Pgenval | Pintval | Pvariant _ | Parrayval _)
  | Ptop | Pbottom | Punboxed_float | Punboxed_int _ | Punboxed_vector _ ->
    Any
