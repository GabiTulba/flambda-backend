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

(* Translation from closed lambda to C-- *)

[@@@ocaml.warning "-40"]

open Misc
open Arch
open Asttypes
open Primitive
open Types
open Lambda
open Clambda
open Clambda_primitives
open Cmm

module String = Misc.Stdlib.String
module IntMap = Map.Make(Int)
module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm_helpers

(* Environments used for translation to Cmm. *)

type boxed_number =
  | Boxed_float of alloc_mode * Debuginfo.t
  | Boxed_integer of boxed_integer * alloc_mode * Debuginfo.t

type env = {
  unboxed_ids : (V.t * boxed_number) V.tbl;
  notify_catch : (Cmm.expression list -> unit) IntMap.t;
  environment_param : V.t option;
}

(* notify_catch associates to each catch handler a callback
   which will be passed the list of arguments of each
   staticfail instruction pointing to that handler. This
   allows transl_catch to observe concrete arguments passed to each
   handler parameter and decide whether to unbox them accordingly.

   Other ways to achieve the same result would be to either (1) traverse
   the body of the catch block after translation (this would be costly
   and could easily lead to quadratric behavior) or (2) return
   a description of arguments passed to each catch handler as an extra
   value to be threaded through all transl_* functions (this would be
   quite heavy, and probably less efficient that the callback approach).
*)


let empty_env =
  {
    unboxed_ids = V.empty;
    notify_catch = IntMap.empty;
    environment_param = None;
  }

let create_env ~environment_param =
  { empty_env with
    environment_param;
  }

let is_unboxed_id id env =
  try Some (V.find_same id env.unboxed_ids)
  with Not_found -> None

let add_unboxed_id id unboxed_id bn env =
  { env with
    unboxed_ids = V.add id (unboxed_id, bn) env.unboxed_ids;
  }

let add_notify_catch n f env =
  { env with
    notify_catch = IntMap.add n f env.notify_catch
  }

let notify_catch i env l =
  match IntMap.find_opt i env.notify_catch with
  | Some f -> f l
  | None -> ()

(* Description of the "then" and "else" continuations in [transl_if]. If
   the "then" continuation is true and the "else" continuation is false then
   we can use the condition directly as the result. Similarly, if the "then"
   continuation is false and the "else" continuation is true then we can use
   the negation of the condition directly as the result. *)
type then_else =
  | Then_true_else_false
  | Then_false_else_true
  | Unknown

let invert_then_else = function
  | Then_true_else_false -> Then_false_else_true
  | Then_false_else_true -> Then_true_else_false
  | Unknown -> Unknown

let mut_from_env env ptr =
  match env.environment_param with
  | None -> Asttypes.Mutable
  | Some environment_param ->
    match ptr with
    | Cvar ptr ->
      (* Loads from the current function's closure are immutable. *)
      if V.same environment_param ptr then Asttypes.Immutable
      else Asttypes.Mutable
    | _ -> Asttypes.Mutable

let get_field env layout ptr n dbg =
  let mut = mut_from_env env ptr in
  let memory_chunk =
    match layout with
    | Pvalue Pintval | Punboxed_int _ -> Word_int
    | Pvalue _ -> Word_val
    | Punboxed_float -> Double
    | Punboxed_vector _ ->
      Misc.fatal_error "SIMD vectors are not yet suppored in the upstream compiler build."
    | Ptop ->
        Misc.fatal_errorf "get_field with Ptop: %a" Debuginfo.print_compact dbg
    | Pbottom ->
        Misc.fatal_errorf "get_field with Pbottom: %a" Debuginfo.print_compact
          dbg
  in
  get_field_gen_given_memory_chunk memory_chunk mut ptr n dbg

type rhs_kind =
  | RHS_block of Lambda.alloc_mode * int
  | RHS_infix of { blocksize : int; offset : int; blockmode: Lambda.alloc_mode }
  | RHS_floatblock of Lambda.alloc_mode * int
  | RHS_nonrec
;;

let rec expr_size env = function
  | Uvar id ->
      begin try V.find_same id env with Not_found -> RHS_nonrec end
  | Uclosure { functions ; not_scanned_slots ; scanned_slots } ->
      (* should all have the same mode *)
      let fn_mode = (List.hd functions).mode in
      List.iter (fun f -> assert (Lambda.eq_mode fn_mode f.mode)) functions;
      RHS_block (fn_mode,
                 fundecls_size functions + List.length not_scanned_slots
                 + List.length scanned_slots)
  | Ulet(_str, _kind, id, exp, body) ->
      expr_size (V.add (VP.var id) (expr_size env exp) env) body
  | Uletrec(bindings, body) ->
      let env =
        List.fold_right
          (fun (id, exp) env -> V.add (VP.var id) (expr_size env exp) env)
          bindings env
      in
      expr_size env body
  | Uprim(Pmakeblock (_, _, _, mode), args, _) ->
      RHS_block (mode, List.length args)
  | Uprim(Pmakearray((Paddrarray | Pintarray), _, mode), args, _) ->
      RHS_block (mode, List.length args)
  | Uprim(Pmakearray(Pfloatarray, _, mode), args, _) ->
      RHS_floatblock (mode, List.length args)
  | Uprim(Pmakearray(Pgenarray, _, _mode), _, _) ->
     (* Pgenarray is excluded from recursive bindings by the
        check in Translcore.check_recursive_lambda *)
     RHS_nonrec
  | Uprim (Pduprecord ((Record_boxed _ | Record_inlined (_, Variant_boxed _)),
                       sz), _, _) ->
      RHS_block (Lambda.alloc_heap, sz)
  | Uprim (Pduprecord ((Record_unboxed
                       | Record_inlined (_, Variant_unboxed)),
                       _), _, _) ->
      assert false
  | Uprim (Pduprecord (Record_inlined (_, Variant_extensible), sz), _, _) ->
      RHS_block (Lambda.alloc_heap, sz + 1)
  | Uprim (Pduprecord (Record_float, sz), _, _) ->
      RHS_floatblock (Lambda.alloc_heap, sz)
  | Uprim (Pccall { prim_name; _ }, closure::_, _)
        when prim_name = "caml_check_value_is_closure" ->
      (* Used for "-clambda-checks". *)
      expr_size env closure
  | Usequence(_exp, exp') ->
      expr_size env exp'
  | Uoffset (exp, offset) ->
      (match expr_size env exp with
      | RHS_block (blockmode, blocksize) ->
         RHS_infix { blocksize; offset; blockmode }
      | RHS_nonrec -> RHS_nonrec
      | _ -> assert false)
  | Uregion exp ->
      expr_size env exp
  | Uexclave exp ->
      expr_size env exp
  | _ -> RHS_nonrec

(* Translate structured constants to Cmm data items *)

let transl_constant dbg = function
  | Uconst_int n ->
      int_const dbg n
  | Uconst_ref (label, def_opt) ->
      Option.iter
        (fun def -> Cmmgen_state.add_structured_constant label def)
        def_opt;
      Cconst_symbol (label, dbg)

let emit_constant cst cont =
  match cst with
  | Uconst_int n ->
      cint_const n
      :: cont
  | Uconst_ref (sym, _) ->
      Csymbol_address sym :: cont

let emit_structured_constant ((_sym, is_global) as symb) cst cont =
  match cst with
  | Uconst_float s ->
      emit_float_constant symb s cont
  | Uconst_string s ->
      emit_string_constant symb s cont
  | Uconst_int32 n ->
      emit_int32_constant symb n cont
  | Uconst_int64 n ->
      emit_int64_constant symb n cont
  | Uconst_nativeint n ->
      emit_nativeint_constant symb n cont
  | Uconst_block (tag, csts) ->
      let cont = List.fold_right emit_constant csts cont in
      emit_block symb (block_header tag (List.length csts)) cont
  | Uconst_float_array fields ->
      emit_float_array_constant symb fields cont
  | Uconst_closure(fundecls, lbl, fv) ->
      Cmmgen_state.add_constant lbl (Const_closure (is_global, fundecls, fv));
      List.iter (fun f -> Cmmgen_state.add_function f) fundecls;
      cont

(* Boxed integers *)

let box_int_constant sym bi n =
  match bi with
    Pnativeint ->
      emit_nativeint_constant (sym, Local) n []
  | Pint32 ->
      let n = Nativeint.to_int32 n in
      emit_int32_constant (sym, Local) n []
  | Pint64 ->
      let n = Int64.of_nativeint n in
      emit_int64_constant (sym, Local) n []

let box_int dbg bi mode arg =
  match arg with
  | Cconst_int (n, _) ->
      let sym = Compilenv.new_const_symbol () in
      let data_items = box_int_constant sym bi (Nativeint.of_int n) in
      Cmmgen_state.add_data_items data_items;
      Cconst_symbol (sym, dbg)
  | Cconst_natint (n, _) ->
      let sym = Compilenv.new_const_symbol () in
      let data_items = box_int_constant sym bi n in
      Cmmgen_state.add_data_items data_items;
      Cconst_symbol (sym, dbg)
  | _ ->
      box_int_gen dbg bi mode arg

(* Boxed numbers *)

let typ_of_boxed_number = function
  | Boxed_float _ -> Cmm.typ_float
  | Boxed_integer (Pint64, _,_) when size_int = 4 -> [|Int;Int|]
  | Boxed_integer _ -> Cmm.typ_int

let equal_unboxed_integer ui1 ui2 =
  match ui1, ui2 with
  | Pnativeint, Pnativeint -> true
  | Pint32, Pint32 -> true
  | Pint64, Pint64 -> true
  | _, _ -> false

let equal_boxed_number bn1 bn2 =
  match bn1, bn2 with
  | Boxed_float _, Boxed_float _ -> true
  | Boxed_integer(ui1, m, _), Boxed_integer(ui2, m', _) ->
    equal_unboxed_integer ui1 ui2 && Lambda.eq_mode m m'
  | _, _ -> false

let box_number bn arg =
  match bn with
  | Boxed_float (m, dbg) -> box_float dbg m arg
  | Boxed_integer (bi, m, dbg) -> box_int dbg bi m arg

(* Returns the unboxed representation of a boxed float or integer.
   For Pint32 on 64-bit archs, the high 32 bits of the result are undefined. *)
let unbox_number dbg bn arg =
  match bn with
  | Boxed_float (_, dbg) ->
    unbox_float dbg arg
  | Boxed_integer (Pint32, _, _) ->
    low_32 dbg (unbox_int dbg Pint32 arg)
  | Boxed_integer (bi, _, _) ->
    unbox_int dbg bi arg

(* Auxiliary functions for optimizing "let" of boxed numbers (floats and
   boxed integers *)

type unboxed_number_kind =
    No_unboxing
  | Boxed of boxed_number * bool (* true: boxed form available at no cost *)
  | No_result (* expression never returns a result *)

(* Given unboxed_number_kind from two branches of the code, returns the
   resulting unboxed_number_kind.

   If [strict=false], one knows that the type of the expression
   is an unboxable number, and we decide to return an unboxed value
   if this indeed eliminates at least one allocation.

   If [strict=true], we need to ensure that all possible branches
   return an unboxable number (of the same kind).  This could not
   be the case in presence of GADTs.
*)
let join_unboxed_number_kind ~strict k1 k2 =
  match k1, k2 with
  | Boxed (b1, c1), Boxed (b2, c2) when equal_boxed_number b1 b2 ->
      Boxed (b1, c1 && c2)
  | No_result, k | k, No_result ->
        k (* if a branch never returns, it is safe to unbox it *)
  | No_unboxing, k | k, No_unboxing when not strict ->
      k
  | _, _ -> No_unboxing

let is_unboxed_number_cmm ~strict cmm =
  let r = ref No_result in
  let notify k =
    r := join_unboxed_number_kind ~strict !r k
  in
  let rec aux = function
    | Cop(Calloc mode, [Cconst_natint (hdr, _); _], dbg)
      when Nativeint.equal hdr float_header ->
        notify (Boxed (Boxed_float (mode,dbg), false))
    | Cop(Calloc mode, [Cconst_natint (hdr, _); Cconst_symbol (ops, _); _], dbg) ->
        if Nativeint.equal hdr boxedintnat_header
        && String.equal ops caml_nativeint_ops
        then
          notify (Boxed (Boxed_integer (Pnativeint, mode, dbg), false))
        else
        if Nativeint.equal hdr boxedint32_header
        && String.equal ops caml_int32_ops
        then
          notify (Boxed (Boxed_integer (Pint32, mode, dbg), false))
        else
        if Nativeint.equal hdr boxedint64_header
        && String.equal ops caml_int64_ops
        then
          notify (Boxed (Boxed_integer (Pint64, mode, dbg), false))
        else
          notify No_unboxing
    | Cconst_symbol (s, _) ->
        begin match Cmmgen_state.structured_constant_of_sym s with
        | Some (Uconst_float _) ->
            notify (Boxed (Boxed_float (alloc_heap, Debuginfo.none), true))
        | Some (Uconst_nativeint _) ->
            notify (Boxed (Boxed_integer (Pnativeint, alloc_heap, Debuginfo.none), true))
        | Some (Uconst_int32 _) ->
            notify (Boxed (Boxed_integer (Pint32, alloc_heap, Debuginfo.none), true))
        | Some (Uconst_int64 _) ->
            notify (Boxed (Boxed_integer (Pint64, alloc_heap, Debuginfo.none), true))
        | _ ->
            notify No_unboxing
        end
    | Cregion e ->
        aux e
    | l ->
        if not (Cmm.iter_shallow_tail aux l) then
          notify No_unboxing
  in
  aux cmm;
  !r

(* Translate an expression *)

let rec transl env e =
  match e with
    Uvar id ->
      begin match is_unboxed_id id env with
      | None -> Cvar id
      | Some (unboxed_id, bn) -> box_number bn (Cvar unboxed_id)
      end
  | Uconst sc ->
      transl_constant Debuginfo.none sc
  | Uclosure { functions ; not_scanned_slots = [] ; scanned_slots = [] } ->
      let sym = Compilenv.new_const_symbol() in
      Cmmgen_state.add_constant sym (Const_closure (Local, functions, []));
      List.iter (fun f -> Cmmgen_state.add_function f) functions;
      let dbg =
        match functions with
        | [] -> Debuginfo.none
        | fundecl::_ -> fundecl.dbg
      in
      Cconst_symbol (sym, dbg)
  | Uclosure { functions ; not_scanned_slots ; scanned_slots } ->
      let startenv = fundecls_size functions + List.length not_scanned_slots in
      let mode =
        Option.get @@
        List.fold_left (fun s { mode; dbg; _ } ->
          match s with
          | None -> Some mode
          | Some m' ->
             if not (Lambda.eq_mode mode m') then
               Misc.fatal_errorf "Inconsistent modes in let rec at %s"
                 (Debuginfo.to_string dbg);
             s) None functions in
      let rec transl_fundecls pos = function
          [] ->
            List.map (transl env) (not_scanned_slots @ scanned_slots)
        | f :: rem ->
            let is_last = match rem with [] -> true | _::_ -> false in
            Cmmgen_state.add_function f;
            let dbg = f.dbg in
            let without_header =
              match f.arity with
              | { function_kind = Curried _ ; params_layout = ([] | [_]) } as arity ->
                Cconst_symbol (f.label, dbg) ::
                alloc_closure_info ~arity
                                   ~startenv:(startenv - pos) ~is_last dbg ::
                transl_fundecls (pos + 3) rem
              | arity ->
                Cconst_symbol
                  (curry_function_sym
                     arity.function_kind
                     (List.map machtype_of_layout_changing_tagged_int_to_val
                       arity.params_layout)
                     (machtype_of_layout_changing_tagged_int_to_val
                       arity.return_layout),
                   dbg) ::
                alloc_closure_info ~arity
                                   ~startenv:(startenv - pos) ~is_last dbg ::
                Cconst_symbol (f.label, dbg) ::
                transl_fundecls (pos + 4) rem
            in
            if pos = 0
            then without_header
            else alloc_infix_header pos f.dbg :: without_header
      in
      let dbg =
        match functions with
        | [] -> Debuginfo.none
        | fundecl::_ -> fundecl.dbg
      in
      make_alloc ~mode dbg Obj.closure_tag (transl_fundecls 0 functions)
  | Uoffset(arg, offset) ->
      (* produces a valid Caml value, pointing just after an infix header *)
      let ptr = transl env arg in
      let dbg = Debuginfo.none in
      ptr_offset ptr offset dbg
  | Udirect_apply(handler_code_sym, args, Some { name; }, _, _, dbg) ->
      let args = List.map (transl env) args in
      return_unit dbg
        (Cop(Cprobe { name; handler_code_sym; }, args, dbg))
  | Udirect_apply(lbl, args, None, result_layout, kind, dbg) ->
      let args = List.map (transl env) args in
      direct_apply lbl (machtype_of_layout result_layout) args kind dbg
  | Ugeneric_apply(clos, args, args_layout, result_layout, kind, dbg) ->
      let clos = transl env clos in
      let args = List.map (transl env) args in
      if List.mem Pbottom args_layout then
        (* [Extended_machtype.of_layout] will fail on Pbottom, convert it to a
           sequence and remove the call, preserving the execution order. *)
        List.fold_left2 (fun rest arg arg_layout ->
            if arg_layout = Pbottom then
              arg
            else
              Csequence(remove_unit arg, rest)
          ) (Ctuple []) args args_layout
      else
        let args_type = List.map Extended_machtype.of_layout args_layout in
        let return = Extended_machtype.of_layout result_layout in
        generic_apply (mut_from_env env clos) clos args args_type return kind dbg
  | Usend(kind, met, obj, args, args_layout, result_layout, pos, dbg) ->
      let met = transl env met in
      let obj = transl env obj in
      let args = List.map (transl env) args in
      let args_type = List.map Extended_machtype.of_layout args_layout in
      let return = Extended_machtype.of_layout result_layout in
      send kind met obj args args_type return pos dbg
  | Ulet(str, kind, id, exp, body) ->
      transl_let env str kind id exp (fun env -> transl env body)
  | Uphantom_let (var, defining_expr, body) ->
      let defining_expr =
        match defining_expr with
        | None -> None
        | Some defining_expr ->
          let defining_expr =
            match defining_expr with
            | Uphantom_const (Uconst_ref (sym, _defining_expr)) ->
              Cphantom_const_symbol sym
            | Uphantom_read_symbol_field { sym; field; } ->
              Cphantom_read_symbol_field { sym; field; }
            | Uphantom_const (Uconst_int i) ->
              Cphantom_const_int (targetint_const i)
            | Uphantom_var var -> Cphantom_var var
            | Uphantom_read_field { var; field; } ->
              Cphantom_read_field { var; field; }
            | Uphantom_offset_var { var; offset_in_words; } ->
              Cphantom_offset_var { var; offset_in_words; }
            | Uphantom_block { tag; fields; } ->
              Cphantom_block { tag; fields; }
          in
          Some defining_expr
      in
      Cphantom_let (var, defining_expr, transl env body)
  | Uletrec(bindings, body) ->
      transl_letrec env bindings (transl env body)

  (* Primitives *)
  | Uprim(prim, args, dbg) ->
      begin match (simplif_primitive prim, args) with
      | (Pread_symbol sym, []) ->
          Cconst_symbol (sym, dbg)
      | (Pmakeblock _, []) ->
          assert false
      | (Pmakeblock(tag, _mut, _kind, mode), args) ->
          make_alloc ~mode dbg tag (List.map (transl env) args)
      | (Pccall prim, args) ->
          transl_ccall env prim args dbg
      | (Pduparray (kind, _), [Uprim (Pmakearray (kind', _, _), args, _dbg)]) ->
          (* We arrive here in two cases:
             1. When using Closure, all the time.
             2. When using Flambda, if a float array longer than
             [Translcore.use_dup_for_constant_arrays_bigger_than] turns out
             to be non-constant.
             If for some reason Flambda fails to lift a constant array we
             could in theory also end up here.
             Note that [kind] above is unconstrained, but with the current
             state of [Translcore], we will in fact only get here with
             [Pfloatarray]s. *)
          assert (kind = kind');
          transl_make_array dbg env kind alloc_heap args
      | (Pduparray _, [arg]) ->
          let prim_obj_dup =
            Primitive.simple_on_values ~name:"caml_obj_dup" ~arity:1 ~alloc:true
          in
          transl_ccall env prim_obj_dup [arg] dbg
      | (Pmakearray _, []) ->
          Misc.fatal_error "Pmakearray is not allowed for an empty array"
      | (Pmakearray (kind, _, mode), args) ->
         transl_make_array dbg env kind mode args
      | (Pbigarrayref(unsafe, _num_dims, elt_kind, layout), arg1 :: argl) ->
          let elt =
            bigarray_get unsafe elt_kind layout
              (transl env arg1) (List.map (transl env) argl) dbg in
          begin match elt_kind with
          (* TODO: local allocation of bigarray elements *)
            Pbigarray_float32 | Pbigarray_float64 -> box_float dbg alloc_heap elt
          | Pbigarray_complex32 | Pbigarray_complex64 -> elt
          | Pbigarray_int32 -> box_int dbg Pint32 alloc_heap elt
          | Pbigarray_int64 -> box_int dbg Pint64 alloc_heap elt
          | Pbigarray_native_int -> box_int dbg Pnativeint alloc_heap elt
          | Pbigarray_caml_int -> tag_int elt dbg
          | Pbigarray_sint8 | Pbigarray_uint8
          | Pbigarray_sint16 | Pbigarray_uint16 -> tag_int elt dbg
          | Pbigarray_unknown -> assert false
          end
      | (Pbigarrayset(unsafe, _num_dims, elt_kind, layout), arg1 :: argl) ->
          let (argidx, argnewval) = split_last argl in
          return_unit dbg (bigarray_set unsafe elt_kind layout
            (transl env arg1)
            (List.map (transl env) argidx)
            (match elt_kind with
              Pbigarray_float32 | Pbigarray_float64 ->
                transl_unbox_float dbg env argnewval
            | Pbigarray_complex32 | Pbigarray_complex64 -> transl env argnewval
            | Pbigarray_int32 -> transl_unbox_int dbg env Pint32 argnewval
            | Pbigarray_int64 -> transl_unbox_int dbg env Pint64 argnewval
            | Pbigarray_native_int ->
                transl_unbox_int dbg env Pnativeint argnewval
            | Pbigarray_caml_int ->
                untag_int (transl env argnewval) dbg
            | Pbigarray_sint8 | Pbigarray_uint8
            | Pbigarray_sint16 | Pbigarray_uint16 ->
                ignore_high_bit_int (untag_int (transl env argnewval) dbg)
            | Pbigarray_unknown -> assert false)
            dbg)
      | (Pbigarraydim(n), [b]) ->
          let dim_ofs = 4 + n in
          tag_int (Cop(Cload (Word_int, Mutable),
            [field_address (transl env b) dim_ofs dbg],
                       dbg)) dbg
      | (Pprobe_is_enabled {name}, []) ->
          tag_int (Cop(Cprobe_is_enabled {name}, [], dbg)) dbg
      | (p, [arg]) ->
          transl_prim_1 env p arg dbg
      | (p, [arg1; arg2]) ->
          transl_prim_2 env p arg1 arg2 dbg
      | (p, [arg1; arg2; arg3]) ->
          transl_prim_3 env p arg1 arg2 arg3 dbg
      | (Pread_symbol _, _::_::_::_::_)
      | (Pbigarrayset (_, _, _, _), [])
      | (Pbigarrayref (_, _, _, _), [])
      | ((Pbigarraydim _ | Pduparray (_, _)), ([] | _::_::_::_::_))
      | (Pprobe_is_enabled _, _)
        ->
          fatal_error "Cmmgen.transl:prim, wrong arity"
      | ((Pfield_computed|Psequand
         | Psequor | Pnot | Pnegint | Paddint | Psubint
         | Pmulint | Pandint | Porint | Pxorint | Plslint
         | Plsrint | Pasrint | Pintoffloat | Pfloatofint _
         | Pnegfloat _ | Pabsfloat _ | Paddfloat _ | Psubfloat _
         | Pmulfloat _ | Pdivfloat _ | Pstringlength | Pstringrefu
         | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
         | Pbytesrefs | Pbytessets | Pisint | Pisout
         | Pbswap16 | Pint_as_pointer | Popaque | Pfield _
         | Psetfield (_, _, _) | Psetfield_computed (_, _)
         | Pfloatfield _ | Psetfloatfield (_, _) | Pduprecord (_, _)
         | Praise _ | Pdivint _ | Pmodint _ | Pintcomp _ | Poffsetint _
         | Pcompare_ints | Pcompare_floats | Pcompare_bints _
         | Poffsetref _ | Pfloatcomp _ | Parraylength _
         | Parrayrefu _ | Parraysetu _ | Parrayrefs _ | Parraysets _
         | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _
         | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _
         | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _
         | Pasrbint _ | Pbintcomp (_, _) | Pstring_load _ | Pbytes_load _
         | Pbytes_set _ | Pbigstring_load _ | Pbigstring_set _
         | Punbox_float | Pbox_float _ | Punbox_int _ | Pbox_int _
         | Pbbswap _), _)
        ->
          fatal_error "Cmmgen.transl:prim"
      end

  (* Control structures *)
  | Uswitch(arg, s, dbg, kind) ->
      (* As in the bytecode interpreter, only matching against constants
         can be checked *)
      if Array.length s.us_index_blocks = 0 then
        make_switch
          (untag_int (transl env arg) dbg)
          s.us_index_consts
          (Array.map (fun expr -> transl env expr, dbg) s.us_actions_consts)
          dbg (kind_of_layout kind)
      else if Array.length s.us_index_consts = 0 then
        bind "switch" (transl env arg) (fun arg ->
          transl_switch dbg (kind_of_layout kind) env (get_tag arg dbg)
            s.us_index_blocks s.us_actions_blocks)
      else
        bind "switch" (transl env arg) (fun arg ->
          Cifthenelse(
          Cop(Cand, [arg; Cconst_int (1, dbg)], dbg),
          dbg,
          transl_switch dbg (kind_of_layout kind) env
            (untag_int arg dbg) s.us_index_consts s.us_actions_consts,
          dbg,
          transl_switch dbg (kind_of_layout kind) env
            (get_tag arg dbg) s.us_index_blocks s.us_actions_blocks,
          dbg, kind_of_layout kind))
  | Ustringswitch(arg,sw,d, kind) ->
      let dbg = Debuginfo.none in
      bind "switch" (transl env arg)
        (fun arg ->
          strmatch_compile dbg (kind_of_layout kind) arg (Option.map (transl env) d)
            (List.map (fun (s,act) -> s,transl env act) sw))
  | Ustaticfail (nfail, args) ->
      let cargs = List.map (transl env) args in
      notify_catch nfail env cargs;
      Cexit (nfail, cargs)
  | Ucatch(nfail, [], body, handler, kind) ->
      let dbg = Debuginfo.none in
      make_catch (kind_of_layout kind) nfail (transl env body) (transl env handler) dbg
  | Ucatch(nfail, ids, body, handler, kind) ->
      let dbg = Debuginfo.none in
      transl_catch (kind_of_layout kind) env nfail ids body handler dbg
  | Utrywith(body, exn, handler, kind) ->
      let dbg = Debuginfo.none in
      Ctrywith(transl env body, exn, transl env handler, dbg, kind_of_layout kind)
  | Uifthenelse(cond, ifso, ifnot, kind) ->
      let ifso_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      let dbg = Debuginfo.none in
      let ifso = transl env ifso in
      let ifnot = transl env ifnot in
      let approx =
        match ifso, ifnot with
        | Cconst_int (1, _), Cconst_int (3, _) -> Then_false_else_true
        | Cconst_int (3, _), Cconst_int (1, _) -> Then_true_else_false
        | _, _ -> Unknown
      in
      transl_if env (kind_of_layout kind) approx dbg cond
        ifso_dbg ifso ifnot_dbg ifnot
  | Usequence(exp1, exp2) ->
      Csequence(remove_unit(transl env exp1), transl env exp2)
  | Uwhile(cond, body) ->
      let dbg = Debuginfo.none in
      let raise_num = next_raise_count () in
      return_unit dbg
        (ccatch
           (raise_num, [],
            create_loop(transl_if env Any Unknown dbg cond
                    dbg (remove_unit(transl env body))
                    dbg (Cexit (raise_num,[])))
              dbg,
            Ctuple [],
            dbg, Any))
  | Ufor(id, low, high, dir, body) ->
      let dbg = Debuginfo.none in
      let tst = match dir with Upto -> Cgt   | Downto -> Clt in
      let inc = match dir with Upto -> Caddi | Downto -> Csubi in
      let raise_num = next_raise_count () in
      let id_prev = VP.create (V.create_local "*id_prev*") in
      return_unit dbg
        (Clet_mut
           (id, typ_int, transl env low,
            bind_nonvar "bound" (transl env high) (fun high ->
              ccatch
                (raise_num, [],
                 Cifthenelse
                   (Cop(Ccmpi tst, [Cvar (VP.var id); high], dbg),
                    dbg,
                    Cexit (raise_num, []),
                    dbg,
                    create_loop
                      (Csequence
                         (remove_unit(transl env body),
                         Clet(id_prev, Cvar (VP.var id),
                          Csequence
                            (Cassign(VP.var id,
                               Cop(inc, [Cvar (VP.var id); Cconst_int (2, dbg)],
                                 dbg)),
                             Cifthenelse
                               (Cop(Ccmpi Ceq, [Cvar (VP.var id_prev); high],
                                  dbg),
                                dbg, Cexit (raise_num,[]),
                                dbg, Ctuple [],
                                dbg, Any)))))
                      dbg,
                   dbg, Any),
                 Ctuple [],
                 dbg, Any))))
  | Uassign(id, exp) ->
      let dbg = Debuginfo.none in
      let cexp = transl env exp in
      begin match is_unboxed_id id env with
      | None ->
          return_unit dbg (Cassign(id, cexp))
      | Some (unboxed_id, bn) ->
          return_unit dbg (Cassign(unboxed_id, unbox_number dbg bn cexp))
      end
  | Uunreachable ->
      let dbg = Debuginfo.none in
      Cop(Cload (Word_int, Mutable), [Cconst_int (0, dbg)], dbg)
  | Uregion e ->
      region (transl env e)
  | Uexclave e ->
      Cexclave (transl env e)

and transl_catch (kind : Cmm.kind_for_unboxing) env nfail ids body handler dbg =
  let ids = List.map (fun (id, kind) -> (id, kind, ref No_result)) ids in
  (* Translate the body, and while doing so, collect the "unboxing type" for
     each argument.  *)
  let report args =
    List.iter2
      (fun (_id, layout, u) c ->
         let strict = match kind_of_layout layout with
           | Boxed_integer _ | Boxed_float -> false
           | Any -> true
         in
         u := join_unboxed_number_kind ~strict !u
             (is_unboxed_number_cmm ~strict c)
      )
      ids args
  in
  let env_body = add_notify_catch nfail report env in
  let body = transl env_body body in
  let new_env, rewrite, ids =
    List.fold_right
      (fun (id, layout, u) (env, rewrite, ids) ->
         match !u with
         | No_unboxing | Boxed (_, true) | No_result ->
             env,
             (fun x -> x) :: rewrite,
             (id, machtype_of_layout layout) :: ids
         | Boxed (bn, false) ->
             let unboxed_id = V.create_local (VP.name id) in
             add_unboxed_id (VP.var id) unboxed_id bn env,
             (unbox_number Debuginfo.none bn) :: rewrite,
             (VP.create unboxed_id, typ_of_boxed_number bn) :: ids
      )
      ids (env, [], [])
  in
  if env == new_env then
    (* No unboxing *)
    ccatch (nfail, ids, body, transl env handler, dbg, kind)
  else
    (* allocate new "nfail" to catch errors more easily *)
    let new_nfail = next_raise_count () in
    let body =
      (* Rewrite the body to unbox the call sites *)
      let rec aux e =
        match Cmm.map_shallow aux e with
        | Cexit (n, el) when n = nfail ->
            Cexit (new_nfail, List.map2 (fun f e -> f e) rewrite el)
        | c -> c
      in
      aux body
    in
    ccatch (new_nfail, ids, body, transl new_env handler, dbg, kind)

and transl_make_array dbg env kind mode args =
  match kind with
  | Pgenarray ->
      let prim =
        match (mode : Lambda.alloc_mode) with
        | Alloc_heap -> "caml_make_array"
        | Alloc_local ->
          assert Config.stack_allocation;
          "caml_make_array_local"
      in
      Cop(Cextcall(prim, typ_val, [], true),
          [make_alloc ~mode dbg 0 (List.map (transl env) args)], dbg)
  | Paddrarray | Pintarray ->
      make_alloc ~mode dbg 0 (List.map (transl env) args)
  | Pfloatarray ->
      make_float_alloc ~mode dbg Obj.double_array_tag
                      (List.map (transl_unbox_float dbg env) args)

and transl_ccall env prim args dbg =
  let transl_arg native_repr arg =
    (* CR layouts v2: This match to be extended with
         | Same_as_ocaml_repr Float64 -> (XFloat, transl env arg)
       in the PR that adds Float64 *)
    match native_repr with
    | Same_as_ocaml_repr Value ->
        (XInt, transl env arg)
    | Same_as_ocaml_repr Void -> assert false
    | Unboxed_float ->
        (XFloat, transl_unbox_float dbg env arg)
    | Unboxed_integer bi ->
        let xty =
          match bi with
          | Pnativeint -> XInt
          | Pint32 -> XInt32
          | Pint64 -> XInt64 in
        (xty, transl_unbox_int dbg env bi arg)
    | Unboxed_vector _ ->
      Misc.fatal_error "SIMD vectors are not yet suppored in the upstream compiler build."
    | Untagged_int ->
        (XInt, untag_int (transl env arg) dbg)
  in
  let rec transl_args native_repr_args args =
    match native_repr_args, args with
    | [], args ->
        (* We don't require the two lists to be of the same length as
           [default_prim] always sets the arity to [0]. *)
        (List.map (fun _ -> XInt) args, List.map (transl env) args)
    | _, [] ->
        assert false
    | (_, native_repr) :: native_repr_args, arg :: args ->
        let (ty1, arg') = transl_arg native_repr arg in
        let (tys, args') = transl_args native_repr_args args in
        (ty1 :: tys, arg' :: args')
  in
  let typ_res, wrap_result =
    match prim.prim_native_repr_res with
    (* CR layouts v2: This match to be extended with
         | Same_as_ocaml_repr Float64 -> (typ_float, fun x -> x)
       in the PR that adds Float64 *)
    | _, Same_as_ocaml_repr Value -> (typ_val, fun x -> x)
    | _, Same_as_ocaml_repr Void -> assert false
    (* TODO: Allow Alloc_local on suitably typed C stubs *)
    | _, Unboxed_float -> (typ_float, box_float dbg alloc_heap)
    | _, Unboxed_integer Pint64 when size_int = 4 ->
        ([|Int; Int|], box_int dbg Pint64 alloc_heap)
    | _, Unboxed_integer bi -> (typ_int, box_int dbg bi alloc_heap)
    | _, Untagged_int -> (typ_int, (fun i -> tag_int i dbg))
    | _, Unboxed_vector _ ->
      Misc.fatal_error "SIMD vectors are not yet suppored in the upstream compiler build."
  in
  let typ_args, args = transl_args prim.prim_native_repr_args args in
  wrap_result
    (Cop(Cextcall(Primitive.native_name prim,
                  typ_res, typ_args, prim.prim_alloc), args, dbg))

and transl_prim_1 env p arg dbg =
  match p with
  (* Generic operations *)
    Popaque ->
      opaque (transl env arg) dbg
  (* Heap operations *)
  | Pfield (n, layout) ->
      get_field env layout (transl env arg) n dbg
  | Pfloatfield (n,mode) ->
      let ptr = transl env arg in
      box_float dbg mode (floatfield n ptr dbg)
  | Pint_as_pointer ->
      int_as_pointer (transl env arg) dbg
  (* Exceptions *)
  | Praise rkind ->
      raise_prim rkind (transl env arg) dbg
  (* Integer operations *)
  | Pnegint ->
      negint (transl env arg) dbg
  | Poffsetint n ->
      offsetint n (transl env arg) dbg
  | Poffsetref n ->
      offsetref n (transl env arg) dbg
  | Punbox_int bi ->
    transl_unbox_int dbg env bi arg
  | Pbox_int (bi, m) ->
    box_int dbg bi m (transl env arg)
  (* Floating-point operations *)
  | Punbox_float ->
      transl_unbox_float dbg env arg
  | Pbox_float m ->
      box_float dbg m (transl env arg)
  | Pfloatofint m ->
      box_float dbg m (Cop(Cfloatofint, [untag_int(transl env arg) dbg], dbg))
  | Pintoffloat ->
     tag_int(Cop(Cintoffloat, [transl_unbox_float dbg env arg], dbg)) dbg
  | Pnegfloat m ->
      box_float dbg m (Cop(Cnegf, [transl_unbox_float dbg env arg], dbg))
  | Pabsfloat m ->
      box_float dbg m (Cop(Cabsf, [transl_unbox_float dbg env arg], dbg))
  (* String operations *)
  | Pstringlength | Pbyteslength ->
      tag_int(string_length (transl env arg) dbg) dbg
  (* Array operations *)
  | Parraylength kind ->
      arraylength kind (transl env arg) dbg
  (* Boolean operations *)
  | Pnot ->
      transl_if env Any Then_false_else_true
        dbg arg
        dbg (Cconst_int (1, dbg))
        dbg (Cconst_int (3, dbg))
  (* Test integer/block *)
  | Pisint ->
      tag_int(Cop(Cand, [transl env arg; Cconst_int (1, dbg)], dbg)) dbg
  (* Boxed integers *)
  | Pbintofint (bi, m) ->
      box_int dbg bi m (untag_int (transl env arg) dbg)
  | Pintofbint bi ->
      tag_int (transl_unbox_int dbg env bi arg) dbg
  | Pcvtbint(bi1, bi2, m) ->
      box_int dbg bi2 m (transl_unbox_int dbg env bi1 arg)
  | Pnegbint (bi, m) ->
      box_int dbg bi m
        (Cop(Csubi, [Cconst_int (0, dbg); transl_unbox_int dbg env bi arg],
          dbg))
  | Pbbswap (bi, m) ->
      box_int dbg bi m (bbswap bi (transl_unbox_int dbg env bi arg) dbg)
  | Pbswap16 ->
      tag_int (bswap16 (ignore_high_bit_int (untag_int
        (transl env arg) dbg)) dbg) dbg
  | (Pfield_computed | Psequand | Psequor
    | Paddint | Psubint | Pmulint | Pandint
    | Porint | Pxorint | Plslint | Plsrint | Pasrint
    | Paddfloat _ | Psubfloat _ | Pmulfloat _ | Pdivfloat _
    | Pstringrefu | Pstringrefs | Pbytesrefu | Pbytessetu
    | Pbytesrefs | Pbytessets | Pisout | Pread_symbol _
    | Pmakeblock (_, _, _, _) | Psetfield (_, _, _) | Psetfield_computed (_, _)
    | Psetfloatfield (_, _) | Pduprecord (_, _) | Pccall _ | Pdivint _
    | Pmodint _ | Pintcomp _ | Pfloatcomp _ | Pmakearray (_, _, _)
    | Pcompare_ints | Pcompare_floats | Pcompare_bints _
    | Pduparray (_, _) | Parrayrefu _ | Parraysetu _
    | Parrayrefs _ | Parraysets _ | Paddbint _ | Psubbint _ | Pmulbint _
    | Pdivbint _ | Pmodbint _ | Pandbint _ | Porbint _ | Pxorbint _
    | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp (_, _)
    | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _)
    | Pbigarraydim _ | Pstring_load _ | Pbytes_load _ | Pbytes_set _
    | Pbigstring_load _ | Pbigstring_set _ | Pprobe_is_enabled _)
    ->
      fatal_errorf "Cmmgen.transl_prim_1: %a"
        Printclambda_primitives.primitive p

and transl_prim_2 env p arg1 arg2 dbg =
  match p with
  (* Heap operations *)
  | Pfield_computed ->
      addr_array_ref (transl env arg1) (transl env arg2) dbg
  | Psetfield(n, ptr, init) ->
      setfield n ptr init (transl env arg1) (transl env arg2) dbg
  | Psetfloatfield (n, init) ->
      let ptr = transl env arg1 in
      let float_val = transl_unbox_float dbg env arg2 in
      setfloatfield n init ptr float_val dbg

  (* Boolean operations *)
  | Psequand ->
      let dbg' = Debuginfo.none in
      transl_sequand env Any Then_true_else_false
        dbg arg1
        dbg' arg2
        dbg (Cconst_int (3, dbg))
        dbg' (Cconst_int (1, dbg))
      (* let id = V.create_local "res1" in
      Clet(id, transl env arg1,
           Cifthenelse(test_bool dbg (Cvar id), transl env arg2, Cvar id)) *)
  | Psequor ->
      let dbg' = Debuginfo.none in
      transl_sequor env Any Then_true_else_false
        dbg arg1
        dbg' arg2
        dbg (Cconst_int (3, dbg))
        dbg' (Cconst_int (1, dbg))
  (* Integer operations *)
  | Paddint ->
      add_int_caml (transl env arg1) (transl env arg2) dbg
  | Psubint ->
      sub_int_caml (transl env arg1) (transl env arg2) dbg
  | Pmulint ->
      mul_int_caml (transl env arg1) (transl env arg2) dbg
  | Pdivint is_safe ->
      div_int_caml is_safe (transl env arg1) (transl env arg2) dbg
  | Pmodint is_safe ->
      mod_int_caml is_safe (transl env arg1) (transl env arg2) dbg
  | Pandint ->
      and_int_caml (transl env arg1) (transl env arg2) dbg
  | Porint ->
      or_int_caml (transl env arg1) (transl env arg2) dbg
  | Pxorint ->
      xor_int_caml (transl env arg1) (transl env arg2) dbg
  | Plslint ->
      lsl_int_caml (transl env arg1) (transl env arg2) dbg
  | Plsrint ->
      lsr_int_caml (transl env arg1) (transl env arg2) dbg
  | Pasrint ->
      asr_int_caml (transl env arg1) (transl env arg2) dbg
  | Pintcomp cmp ->
      int_comp_caml cmp (transl env arg1) (transl env arg2) dbg
  | Pcompare_ints ->
      (* Compare directly on tagged ints *)
      mk_compare_ints dbg (transl env arg1) (transl env arg2)
  | Pcompare_bints bi ->
      let a1 = transl_unbox_int dbg env bi arg1 in
      let a2 = transl_unbox_int dbg env bi arg2 in
      mk_compare_ints dbg a1 a2
  | Pcompare_floats ->
      let a1 = transl_unbox_float dbg env arg1 in
      let a2 = transl_unbox_float dbg env arg2 in
      mk_compare_floats dbg a1 a2
  | Pisout ->
      transl_isout (transl env arg1) (transl env arg2) dbg
  (* Float operations *)
  | Paddfloat m ->
      box_float dbg m (Cop(Caddf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Psubfloat m ->
      box_float dbg m (Cop(Csubf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pmulfloat m ->
      box_float dbg m (Cop(Cmulf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pdivfloat m ->
      box_float dbg m (Cop(Cdivf,
                    [transl_unbox_float dbg env arg1;
                     transl_unbox_float dbg env arg2],
                    dbg))
  | Pfloatcomp cmp ->
      tag_int(Cop(Ccmpf cmp,
                  [transl_unbox_float dbg env arg1;
                   transl_unbox_float dbg env arg2],
                  dbg)) dbg

  (* String operations *)
  | Pstringrefu | Pbytesrefu ->
      stringref_unsafe (transl env arg1) (transl env arg2) dbg
  | Pstringrefs | Pbytesrefs ->
      stringref_safe (transl env arg1) (transl env arg2) dbg
  | Pstring_load(size, unsafe, mode) | Pbytes_load(size, unsafe, mode) ->
      string_load size unsafe mode (transl env arg1) (transl env arg2) dbg
  | Pbigstring_load(size, unsafe, mode) ->
      bigstring_load size unsafe mode (transl env arg1) (transl env arg2) dbg

  (* Array operations *)
  | Parrayrefu rkind ->
      arrayref_unsafe rkind (transl env arg1) (transl env arg2) dbg
  | Parrayrefs rkind ->
      arrayref_safe rkind (transl env arg1) (transl env arg2) dbg

  (* Boxed integers *)
  | Paddbint (bi, mode) ->
      box_int dbg bi mode (add_int
                        (transl_unbox_int_low dbg env bi arg1)
                        (transl_unbox_int_low dbg env bi arg2) dbg)
  | Psubbint (bi, mode) ->
      box_int dbg bi mode (sub_int
                        (transl_unbox_int_low dbg env bi arg1)
                        (transl_unbox_int_low dbg env bi arg2) dbg)
  | Pmulbint (bi, mode) ->
      box_int dbg bi mode (mul_int
                        (transl_unbox_int_low dbg env bi arg1)
                        (transl_unbox_int_low dbg env bi arg2) dbg)
  | Pdivbint { size = bi; is_safe; mode } ->
      box_int dbg bi mode (safe_div_bi is_safe
                      (transl_unbox_int dbg env bi arg1)
                      (transl_unbox_int dbg env bi arg2)
                      bi dbg)
  | Pmodbint { size = bi; is_safe; mode } ->
      box_int dbg bi mode (safe_mod_bi is_safe
                      (transl_unbox_int dbg env bi arg1)
                      (transl_unbox_int dbg env bi arg2)
                      bi dbg)
  | Pandbint (bi,mode) ->
      box_int dbg bi mode (Cop(Cand,
                     [transl_unbox_int_low dbg env bi arg1;
                      transl_unbox_int_low dbg env bi arg2], dbg))
  | Porbint (bi, mode) ->
      box_int dbg bi mode (Cop(Cor,
                     [transl_unbox_int_low dbg env bi arg1;
                      transl_unbox_int_low dbg env bi arg2], dbg))
  | Pxorbint (bi, mode) ->
      box_int dbg bi mode (Cop(Cxor,
                     [transl_unbox_int_low dbg env bi arg1;
                      transl_unbox_int_low dbg env bi arg2], dbg))
  | Plslbint (bi, mode) ->
      box_int dbg bi mode (lsl_int
                        (transl_unbox_int_low dbg env bi arg1)
                        (untag_int(transl env arg2) dbg) dbg)
  | Plsrbint (bi, mode) ->
      box_int dbg bi mode (lsr_int
                        (make_unsigned_int bi (transl_unbox_int dbg env bi arg1)
                                        dbg)
                        (untag_int(transl env arg2) dbg) dbg)
  | Pasrbint (bi, mode) ->
      box_int dbg bi mode (asr_int
                        (transl_unbox_int dbg env bi arg1)
                        (untag_int(transl env arg2) dbg) dbg)
  | Pbintcomp(bi, cmp) ->
      tag_int (Cop(Ccmpi cmp,
                     [transl_unbox_int dbg env bi arg1;
                      transl_unbox_int dbg env bi arg2], dbg)) dbg
  | Pnot | Pnegint | Pintoffloat | Pfloatofint _ | Pnegfloat _
  | Pabsfloat _ | Pstringlength | Pbyteslength | Pbytessetu | Pbytessets
  | Pisint | Pbswap16 | Pint_as_pointer | Popaque | Pread_symbol _
  | Pmakeblock (_, _, _, _) | Pfield _ | Psetfield_computed (_, _)
  | Pfloatfield _
  | Pduprecord (_, _) | Pccall _ | Praise _ | Poffsetint _ | Poffsetref _
  | Pmakearray (_, _, _) | Pduparray (_, _) | Parraylength _ | Parraysetu _
  | Parraysets _ | Pbintofint _ | Pintofbint _ | Pcvtbint (_, _, _)
  | Pnegbint _ | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _)
  | Pbigarraydim _ | Pbytes_set _ | Pbigstring_set _ | Pbbswap _
  | Pprobe_is_enabled _
  | Punbox_float | Pbox_float _ | Punbox_int _ | Pbox_int _
    ->
      fatal_errorf "Cmmgen.transl_prim_2: %a"
        Printclambda_primitives.primitive p

and transl_prim_3 env p arg1 arg2 arg3 dbg =
  match p with
  (* Heap operations *)
  | Psetfield_computed(ptr, init) ->
      setfield_computed ptr init
        (transl env arg1) (transl env arg2) (transl env arg3) dbg
  (* String operations *)
  | Pbytessetu ->
      bytesset_unsafe
        (transl env arg1) (transl env arg2) (transl env arg3) dbg
  | Pbytessets ->
      bytesset_safe
        (transl env arg1) (transl env arg2) (transl env arg3) dbg

  (* Array operations *)
  | Parraysetu skind ->
      let newval =
        match skind with
        | Pfloatarray_set -> transl_unbox_float dbg env arg3
        | _ -> transl env arg3
      in
      arrayset_unsafe skind (transl env arg1) (transl env arg2) newval dbg
  | Parraysets skind ->
      let newval =
        match skind with
        | Pfloatarray_set -> transl_unbox_float dbg env arg3
        | _ -> transl env arg3
      in
      arrayset_safe skind (transl env arg1) (transl env arg2) newval dbg

  | Pbytes_set(size, unsafe) ->
      bytes_set size unsafe (transl env arg1) (transl env arg2)
        (transl_unbox_sized size dbg env arg3) dbg

  | Pbigstring_set(size, unsafe) ->
      bigstring_set size unsafe (transl env arg1) (transl env arg2)
        (transl_unbox_sized size dbg env arg3) dbg

  | Pfield_computed | Psequand | Psequor | Pnot | Pnegint | Paddint
  | Psubint | Pmulint | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
  | Pintoffloat | Pfloatofint _ | Pnegfloat _ | Pabsfloat _ | Paddfloat _ | Psubfloat _
  | Pmulfloat _ | Pdivfloat _ | Pstringlength | Pstringrefu | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytesrefs | Pisint | Pisout
  | Pbswap16 | Pint_as_pointer | Popaque | Pread_symbol _
  | Pmakeblock (_, _, _, _)
  | Pfield _ | Psetfield (_, _, _) | Pfloatfield _ | Psetfloatfield (_, _)
  | Pduprecord (_, _) | Pccall _ | Praise _ | Pdivint _ | Pmodint _ | Pintcomp _
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Poffsetint _ | Poffsetref _ | Pfloatcomp _ | Pmakearray (_, _, _)
  | Pduparray (_, _) | Parraylength _ | Parrayrefu _ | Parrayrefs _
  | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _ | Paddbint _
  | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _ | Porbint _
  | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp (_, _)
  | Pbigarrayref (_, _, _, _) | Pbigarrayset (_, _, _, _) | Pbigarraydim _
  | Pstring_load _ | Pbytes_load _ | Pbigstring_load _ | Pbbswap _
  | Pprobe_is_enabled _
  | Punbox_float | Pbox_float _ | Punbox_int _ | Pbox_int _
    ->
      fatal_errorf "Cmmgen.transl_prim_3: %a"
        Printclambda_primitives.primitive p

and transl_unbox_float dbg env exp =
  unbox_float dbg (transl env exp)

and transl_unbox_int dbg env bi exp =
  unbox_int dbg bi (transl env exp)

(* transl_unbox_int, but may return garbage in upper bits *)
and transl_unbox_int_low dbg env bi e =
  let e = transl_unbox_int dbg env bi e in
  if bi = Pint32 then low_32 dbg e else e

and transl_unbox_sized size dbg env exp =
  match size with
  | Sixteen ->
     ignore_high_bit_int (untag_int (transl env exp) dbg)
  | Thirty_two -> transl_unbox_int dbg env Pint32 exp
  | Sixty_four -> transl_unbox_int dbg env Pint64 exp

and transl_let_value env str (kind : Lambda.value_kind) id exp transl_body =
  let dbg = Debuginfo.none in
  let cexp = transl env exp in
  let unboxing =
    (* If [id] is a mutable variable (introduced to eliminate a local
       reference) and it contains a type of unboxable numbers, then
       force unboxing.  Indeed, if not boxed, each assignment to the variable
       might require some boxing, but such local references are often
       used in loops and we really want to avoid repeated boxing.

       We conservatively mark these as Alloc_heap, although with more tracking
       of allocation mode it may be possible to mark some Alloc_local *)
    match str, kind with
    | Mutable, Pfloatval ->
        Boxed (Boxed_float (alloc_heap, dbg), false)
    | Mutable, Pboxedintval bi ->
        Boxed (Boxed_integer (bi, alloc_heap, dbg), false)
    | _, Pboxedvectorval _ ->
        Misc.fatal_error "SIMD vectors are not yet suppored in the upstream compiler build."
    | _, (Pfloatval | Pboxedintval _) ->
        (* It would be safe to always unbox in this case, but
           we do it only if this indeed allows us to get rid of
           some allocations in the bound expression. *)
        is_unboxed_number_cmm ~strict:false cexp
    | _, (Pgenval | Pvariant _ | Parrayval _) ->
        (* Here we don't know statically that the bound expression
           evaluates to an unboxable number type.  We need to be stricter
           and ensure that all possible branches in the expression
           return a boxed value (of the same kind).  Indeed, with GADTs,
           different branches could return different types. *)
        is_unboxed_number_cmm ~strict:true cexp
    | _, Pintval ->
        No_unboxing
  in
  match unboxing with
  | No_unboxing | Boxed (_, true) | No_result ->
      (* N.B. [body] must still be traversed even if [exp] will never return:
         there may be constant closures inside that need lifting out. *)
      begin match str, kind with
      | (Immutable | Immutable_unique), _ -> Clet(id, cexp, transl_body env)
      | Mutable, Pintval -> Clet_mut(id, typ_int, cexp, transl_body env)
      | Mutable, _ -> Clet_mut(id, typ_val, cexp, transl_body env)
      end
  | Boxed (boxed_number, false) ->
      let unboxed_id = V.create_local (VP.name id) in
      let v = VP.create unboxed_id in
      let cexp = unbox_number dbg boxed_number cexp in
      let body =
        transl_body (add_unboxed_id (VP.var id) unboxed_id boxed_number env) in
      begin match str, boxed_number with
      | (Immutable | Immutable_unique), _ -> Clet (v, cexp, body)
      | Mutable, bn -> Clet_mut (v, typ_of_boxed_number bn, cexp, body)
      end

and transl_let env str (layout : Lambda.layout) id exp transl_body =
  match layout with
  | Ptop ->
      Misc.fatal_errorf "Variable %a with layout [Ptop] can't be compiled"
        VP.print id
  | Pbottom ->
      let cexp = transl env exp in
      (* N.B. [body] must still be traversed even if [exp] will never return:
         there may be constant closures inside that need lifting out. *)
      let _cbody : expression = transl_body env in
      cexp
  | Punboxed_vector _ ->
      Misc.fatal_error "SIMD vectors are not yet suppored in the upstream compiler build."
  | Punboxed_float | Punboxed_int _ -> begin
      let cexp = transl env exp in
      let cbody = transl_body env in
      match str with
      | (Immutable | Immutable_unique) ->
        Clet(id, cexp, cbody)
      | Mutable ->
        let typ = machtype_of_layout layout in
        Clet_mut(id, typ, cexp, cbody)
  end
  | Pvalue kind ->
      transl_let_value env str kind id exp transl_body

and make_catch (kind : Cmm.kind_for_unboxing) ncatch body handler dbg = match body with
| Cexit (nexit,[]) when nexit=ncatch -> handler
| _ ->  ccatch (ncatch, [], body, handler, dbg, kind)

and is_shareable_cont exp =
  match exp with
  | Cexit (_,[]) -> true
  | _ -> false

and make_shareable_cont (kind : Cmm.kind_for_unboxing) dbg mk exp =
  if is_shareable_cont exp then mk exp
  else begin
    let nfail = next_raise_count () in
    make_catch
      kind
      nfail
      (mk (Cexit (nfail,[])))
      exp
      dbg
  end

and transl_if env (kind : Cmm.kind_for_unboxing) (approx : then_else)
      (dbg : Debuginfo.t) cond
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  match cond with
  | Uconst (Uconst_int 0) -> else_
  | Uconst (Uconst_int 1) -> then_
  | Uifthenelse (arg1, arg2, Uconst (Uconst_int 0), _) ->
      (* CR mshinwell: These Debuginfos will flow through from Clambda *)
      let inner_dbg = Debuginfo.none in
      let ifso_dbg = Debuginfo.none in
      transl_sequand env kind approx
        inner_dbg arg1
        ifso_dbg arg2
        then_dbg then_
        else_dbg else_
  | Ulet(str, kind_, id, exp, cond) ->
      transl_let env str kind_ id exp (fun env ->
        transl_if env kind approx dbg cond then_dbg then_ else_dbg else_)
  | Uprim (Psequand, [arg1; arg2], inner_dbg) ->
      transl_sequand env kind approx
        inner_dbg arg1
        inner_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uifthenelse (arg1, Uconst (Uconst_int 1), arg2, _) ->
      let inner_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      transl_sequor env kind approx
        inner_dbg arg1
        ifnot_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uprim (Psequor, [arg1; arg2], inner_dbg) ->
      transl_sequor env kind approx
        inner_dbg arg1
        inner_dbg arg2
        then_dbg then_
        else_dbg else_
  | Uprim (Pnot, [arg], _dbg) ->
      transl_if env kind (invert_then_else approx)
        dbg arg
        else_dbg else_
        then_dbg then_
  | Uifthenelse (Uconst (Uconst_int 1), ifso, _, _) ->
      let ifso_dbg = Debuginfo.none in
      transl_if env kind approx
        ifso_dbg ifso
        then_dbg then_
        else_dbg else_
  | Uifthenelse (Uconst (Uconst_int 0), _, ifnot, _) ->
      let ifnot_dbg = Debuginfo.none in
      transl_if env kind approx
        ifnot_dbg ifnot
        then_dbg then_
        else_dbg else_
  | Uifthenelse (cond, ifso, ifnot, _) ->
      let inner_dbg = Debuginfo.none in
      let ifso_dbg = Debuginfo.none in
      let ifnot_dbg = Debuginfo.none in
      make_shareable_cont kind then_dbg
        (fun shareable_then ->
           make_shareable_cont kind else_dbg
             (fun shareable_else ->
                mk_if_then_else
                  inner_dbg kind (test_bool inner_dbg (transl env cond))
                  ifso_dbg (transl_if env kind approx
                    ifso_dbg ifso
                    then_dbg shareable_then
                    else_dbg shareable_else)
                  ifnot_dbg (transl_if env kind approx
                    ifnot_dbg ifnot
                    then_dbg shareable_then
                    else_dbg shareable_else))
             else_)
        then_
  | _ -> begin
      match approx with
      | Then_true_else_false ->
          transl env cond
      | Then_false_else_true ->
          mk_not dbg (transl env cond)
      | Unknown ->
          mk_if_then_else
            dbg kind (test_bool dbg (transl env cond))
            then_dbg then_
            else_dbg else_
    end

and transl_sequand env (kind : Cmm.kind_for_unboxing) (approx : then_else)
      (arg1_dbg : Debuginfo.t) arg1
      (arg2_dbg : Debuginfo.t) arg2
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  make_shareable_cont kind else_dbg
    (fun shareable_else ->
       transl_if env kind Unknown
         arg1_dbg arg1
         arg2_dbg (transl_if env kind approx
           arg2_dbg arg2
           then_dbg then_
           else_dbg shareable_else)
         else_dbg shareable_else)
    else_

and transl_sequor env (kind : Cmm.kind_for_unboxing) (approx : then_else)
      (arg1_dbg : Debuginfo.t) arg1
      (arg2_dbg : Debuginfo.t) arg2
      (then_dbg : Debuginfo.t) then_
      (else_dbg : Debuginfo.t) else_ =
  make_shareable_cont kind then_dbg
    (fun shareable_then ->
       transl_if env kind Unknown
         arg1_dbg arg1
         then_dbg shareable_then
         arg2_dbg (transl_if env kind approx
           arg2_dbg arg2
           then_dbg shareable_then
           else_dbg else_))
    then_

(* This assumes that [arg] can be safely discarded if it is not used. *)
and transl_switch dbg (kind : Cmm.kind_for_unboxing) env arg index cases = match Array.length cases with
| 0 -> fatal_error "Cmmgen.transl_switch"
| 1 -> transl env cases.(0)
| _ ->
    let cases = Array.map (transl env) cases in
    transl_switch_clambda dbg kind arg index cases

and transl_letrec env bindings cont =
  let dbg = Debuginfo.none in
  let bsz =
    List.map (fun (id, exp) -> (id, exp, expr_size V.empty exp))
      bindings
  in
  let op_alloc prim args =
    Cop(Cextcall(prim, typ_val, [], true), args, dbg) in
  let rec init_blocks = function
    | [] -> fill_nonrec bsz
    | (_, _,
       (RHS_block (Alloc_local, _) |
        RHS_infix {blockmode=Alloc_local; _} |
        RHS_floatblock (Alloc_local, _))) :: _ ->
      Misc.fatal_error "Invalid stack allocation found"
    | (id, _exp, RHS_block (Alloc_heap, sz)) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy" [int_const dbg sz],
          init_blocks rem)
    | (id, _exp, RHS_infix { blocksize; offset; blockmode=Alloc_heap }) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy_infix"
             [int_const dbg blocksize; int_const dbg offset],
             init_blocks rem)
    | (id, _exp, RHS_floatblock (Alloc_heap, sz)) :: rem ->
        Clet(id, op_alloc "caml_alloc_dummy_float" [int_const dbg sz],
          init_blocks rem)
    | (id, _exp, RHS_nonrec) :: rem ->
        Clet (id, Cconst_int (1, dbg), init_blocks rem)
  and fill_nonrec = function
    | [] -> fill_blocks bsz
    | (_id, _exp,
       (RHS_block _ | RHS_infix _ | RHS_floatblock _)) :: rem ->
        fill_nonrec rem
    | (id, exp, RHS_nonrec) :: rem ->
        Clet(id, transl env exp, fill_nonrec rem)
  and fill_blocks = function
    | [] -> cont
    | (id, exp, (RHS_block _ | RHS_infix _ | RHS_floatblock _)) :: rem ->
        let op =
          Cop(Cextcall("caml_update_dummy", typ_void, [], false),
              [Cvar (VP.var id); transl env exp], dbg) in
        Csequence(op, fill_blocks rem)
    | (_id, _exp, RHS_nonrec) :: rem ->
        fill_blocks rem
  in init_blocks bsz

(* Translate a function definition *)

let transl_function f =
  let body = f.body in
  let cmm_body =
    let env = create_env ~environment_param:f.env in
    if !Clflags.afl_instrument then
      Afl_instrument.instrument_function (transl env body) f.dbg
    else
      transl env body in
  let fun_codegen_options =
    if !Clflags.optimize_for_speed then
      []
    else
      [ Reduce_code_size ]
  in
  let params_layout =
    if List.length f.params = List.length f.arity.params_layout then
      f.arity.params_layout
    else
      f.arity.params_layout @ [Lambda.layout_function]
  in
  Cfunction {fun_name = f.label;
             fun_args = List.map2 (fun id ty -> (id, machtype_of_layout ty))
                 f.params params_layout;
             fun_body = cmm_body;
             fun_codegen_options;
             fun_poll = f.poll;
             fun_dbg  = f.dbg}

(* Translate all function definitions *)

let rec transl_all_functions already_translated cont =
  match Cmmgen_state.next_function () with
  | None -> cont, already_translated
  | Some f ->
    let sym = f.label in
    if String.Set.mem sym already_translated then
      transl_all_functions already_translated cont
    else begin
      transl_all_functions
        (String.Set.add sym already_translated)
        ((f.dbg, transl_function f) :: cont)
    end

(* Emit constant blocks *)

let emit_constant_table symb elems =
  cdefine_symbol symb @
  elems

(* Emit all structured constants *)

let transl_clambda_constants (constants : Clambda.preallocated_constant list)
      cont =
  let c = ref cont in
  let emit_clambda_constant symbol global cst =
     let cst = emit_structured_constant (symbol, global) cst [] in
     c := (Cdata cst) :: !c
  in
  List.iter
    (fun { symbol; exported; definition = cst; provenance = _; } ->
       let global : Cmmgen_state.is_global =
         if exported then Global else Local
       in
       emit_clambda_constant symbol global cst)
    constants;
  !c

let emit_cmm_data_items_for_constants cont =
  let c = ref cont in
  String.Map.iter (fun symbol (cst : Cmmgen_state.constant) ->
      match cst with
      | Const_closure (global, fundecls, clos_vars) ->
          let cmm =
            emit_constant_closure (symbol, global) fundecls
              (List.fold_right emit_constant clos_vars []) []
          in
          c := (Cdata cmm) :: !c
      | Const_table (global, elems) ->
          c := (Cdata (emit_constant_table (symbol, global) elems)) :: !c)
    (Cmmgen_state.get_and_clear_constants ());
  Cdata (Cmmgen_state.get_and_clear_data_items ()) :: !c

let transl_all_functions cont =
  let rec aux already_translated cont translated_functions =
    if Cmmgen_state.no_more_functions ()
    then cont, translated_functions
    else
      let translated_functions, already_translated =
        transl_all_functions already_translated translated_functions
      in
      aux already_translated cont translated_functions
  in
  let cont, translated_functions =
    aux String.Set.empty cont []
  in
  let translated_functions =
    (* Sort functions according to source position *)
    List.map snd
      (List.sort (fun (dbg1, _) (dbg2, _) ->
           Debuginfo.compare dbg1 dbg2) translated_functions)
  in
  translated_functions @ cont

(* Translate a compilation unit *)

let compunit (ulam, preallocated_blocks, constants) =
  assert (Cmmgen_state.no_more_functions ());
  let dbg = Debuginfo.none in
  Cmmgen_state.set_structured_constants constants;
  let init_code =
    if !Clflags.afl_instrument then
      Afl_instrument.instrument_initialiser (transl empty_env ulam)
        (fun () -> dbg)
    else
      transl empty_env ulam in
  let c1 = [Cfunction {fun_name = make_symbol "entry";
                       fun_args = [];
                       fun_body = init_code;
                       (* This function is often large and run only once.
                          Compilation time matter more than runtime.
                          See MPR#7630 *)
                       fun_codegen_options =
                         if Config.flambda then [
                           Reduce_code_size;
                           No_CSE;
                         ]
                         else [ Reduce_code_size ];
                       fun_poll = Default_poll;
                       fun_dbg  = Debuginfo.none }] in
  let c2 = transl_clambda_constants constants c1 in
  let c3 = transl_all_functions c2 in
  Cmmgen_state.set_structured_constants [];
  let c4 = emit_preallocated_blocks preallocated_blocks c3 in
  emit_cmm_data_items_for_constants c4
