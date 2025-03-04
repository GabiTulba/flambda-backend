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

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Types
open Lambda
open Switch
open Instruct
open Debuginfo.Scoped_location

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter; !label_counter

(**** Operations on compilation environments. ****)

let empty_env =
  { ce_stack = Ident.empty; ce_heap = Ident.empty; ce_rec = Ident.empty }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack;
    ce_heap = env.ce_heap;
    ce_rec = env.ce_rec }

let rec add_vars idlist pos env =
  match idlist with
    [] -> env
  | id :: rem -> add_vars rem (pos + 1) (add_var id pos env)

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
    Kbranch lbl :: _ as cont -> (lbl, cont)
  | Klabel lbl :: _ as cont -> (lbl, cont)
  | cont -> let lbl = new_label() in (lbl, Klabel lbl :: cont)

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let rec make_branch_2 lbl n cont =
  function
    Kreturn m :: _ -> (Kreturn (n + m), cont)
  | Klabel _ :: c  -> make_branch_2 lbl n cont c
  | Kpop m :: c    -> make_branch_2 lbl (n + m) cont c
  | _              ->
      match lbl with
        Some lbl -> (Kbranch lbl, cont)
      | None     -> let lbl = new_label() in (Kbranch lbl, Klabel lbl :: cont)

let make_branch cont =
  match cont with
    (Kbranch _ as branch) :: _ -> (branch, cont)
  | (Kreturn _ as return) :: _ -> (return, cont)
  | Kraise k :: _ -> (Kraise k, cont)
  | Klabel lbl :: _ -> make_branch_2 (Some lbl) 0 cont cont
  | _ ->  make_branch_2 (None) 0 cont cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label cont = match cont with
| Klabel label0::_ when label = label0 -> cont
| _ -> Kbranch label::cont

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
    [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Check if we're in tailcall position *)

let rec is_tailcall = function
    Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

(* Will this primitive result in an OCaml call which would benefit
   from the tail call optimization? *)

let preserve_tailcall_for_prim = function
    Popaque _ | Psequor | Psequand
  | Pobj_magic _ ->
      true
  | Pbytes_to_string | Pbytes_of_string
  | Parray_to_iarray | Parray_of_iarray
  | Pignore
  | Pgetglobal _ | Psetglobal _ | Pgetpredef _
  | Pmakeblock _ | Pmakefloatblock _
  | Pfield _ | Pfield_computed _ | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Pccall _ | Praise _ | Pnot | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _ | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Poffsetint _ | Poffsetref _ | Pintoffloat
  | Pfloatofint _ | Pnegfloat _ | Pabsfloat _ | Paddfloat _ | Psubfloat _ | Pmulfloat _
  | Punbox_float | Pbox_float _ | Punbox_int _ | Pbox_int _
  | Pdivfloat _ | Pfloatcomp _ | Pstringlength | Pstringrefu  | Pstringrefs
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  | Pmakearray _ | Pduparray _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Parrayrefs _ | Parraysets _ | Pisint _ | Pisout | Pbintofint _ | Pintofbint _
  | Pcvtbint _ | Pnegbint _ | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _
  | Pmodbint _ | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _
  | Pasrbint _ | Pbintcomp _ | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _
  | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_64 _ | Pbytes_load_16 _
  | Pbytes_load_32 _ | Pbytes_load_64 _ | Pbytes_set_16 _ | Pbytes_set_32 _
  | Pbytes_set_64 _ | Pbigstring_load_16 _ | Pbigstring_load_32 _
  | Pbigstring_load_64 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pprobe_is_enabled _ | Pobj_dup
  | Pbigstring_set_64 _ | Pctconst _ | Pbswap16 | Pbbswap _ | Pint_as_pointer ->
      false

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0 then cont else
    match cont with
      Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn(n + m) :: cont
    | Kraise _ :: _ -> cont
    | _ -> Kpop n :: cont

(* Add the constant "unit" in front of a continuation *)

let add_const_unit = function
    (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont

let rec push_dummies n k = match n with
| 0 -> k
| _ -> Kconst const_unit::Kpush::push_dummies (n-1) k


(**** Auxiliary for compiling "let rec" ****)

type rhs_kind =
  | RHS_block of int
  | RHS_infix of { blocksize : int; offset : int }
  | RHS_floatblock of int
  | RHS_nonrec
  | RHS_function of int * int
;;

let rec check_recordwith_updates id e =
  match e with
  | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; _], _), cont)
      -> id2 = id && check_recordwith_updates id cont
  | Lvar id2 -> id2 = id
  | _ -> false
;;

let rec size_of_lambda env = function
  | Lvar id ->
      begin try Ident.find_same id env with Not_found -> RHS_nonrec end
  | Lfunction{params} as funct ->
      RHS_function (2 + Ident.Set.cardinal(free_variables funct),
                    List.length params)
  | Llet (Strict, _k, id, Lprim (Pduprecord (kind, size), _, _), body)
    when check_recordwith_updates id body ->
      begin match kind with
      | Record_boxed _ | Record_inlined (_, Variant_boxed _) -> RHS_block size
      | Record_unboxed | Record_inlined (_, Variant_unboxed) -> assert false
      | Record_float -> RHS_floatblock size
      | Record_inlined (_, Variant_extensible) -> RHS_block (size + 1)
      end
  | Llet(_str, _k, id, arg, body) ->
      size_of_lambda (Ident.add id (size_of_lambda env arg) env) body
  (* See the Lletrec case of comp_expr *)
  | Lletrec(bindings, body) when
      List.for_all (function (_, Lfunction _) -> true | _ -> false) bindings ->
      (* let rec of functions *)
      let fv =
        Ident.Set.elements (free_variables (Lletrec(bindings, lambda_unit))) in
      (* See Instruct(CLOSUREREC) in interp.c *)
      let blocksize = List.length bindings * 3 - 1 + List.length fv in
      let offsets = List.mapi (fun i (id, _e) -> (id, i * 3)) bindings in
      let env = List.fold_right (fun (id, offset) env ->
        Ident.add id (RHS_infix { blocksize; offset }) env) offsets env in
      size_of_lambda env body
  | Lletrec(bindings, body) ->
      let env = List.fold_right
        (fun (id, e) env -> Ident.add id (size_of_lambda env e) env)
        bindings env
      in
      size_of_lambda env body
  | Lprim(Pmakeblock _, args, _) -> RHS_block (List.length args)
  | Lprim (Pmakearray ((Paddrarray|Pintarray), _, _), args, _) ->
      RHS_block (List.length args)
  | Lprim (Pmakearray (Pfloatarray, _, _), args, _)
  | Lprim (Pmakefloatblock _, args, _) ->
      RHS_floatblock (List.length args)
  | Lprim (Pmakearray (Pgenarray, _, _), _, _) ->
     (* Pgenarray is excluded from recursive bindings by the
        check in Translcore.check_recursive_lambda *)
      RHS_nonrec
  | Lprim (Pduprecord ((Record_boxed _ | Record_inlined (_, Variant_boxed _)),
                       size), _, _) ->
      RHS_block size
  | Lprim (Pduprecord ((Record_unboxed
                       | Record_inlined (_, Variant_unboxed)),
           _), _, _) ->
      assert false
  | Lprim (Pduprecord (Record_inlined (_, Variant_extensible), size), _, _) ->
      RHS_block (size + 1)
  | Lprim (Pduprecord (Record_float, size), _, _) -> RHS_floatblock size
  | Levent (lam, _) -> size_of_lambda env lam
  | Lsequence (_lam, lam') -> size_of_lambda env lam'
  | Lregion (lam, _) -> size_of_lambda env lam
  | _ -> RHS_nonrec

(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev with
    ev_pos = 0;                   (* patched in emitcode *)
    ev_kind = kind;
    ev_info = info;
    ev_repr = repr }

let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
    Event_other, info -> info
  | info, Event_other -> info
  | _                 -> fatal_error "Bytegen.merge_infos"

let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
    Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | Event_child r, Event_parent r' when r == r' -> Event_parent r
  | _, _          -> fatal_error "Bytegen.merge_repr"

let merge_events ev ev' =
  let (maj, min) =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)
      Event_pseudo,  _                              -> ev', ev
    | _,             Event_pseudo                   -> ev,  ev'
    (* Keep following event, supposedly more informative *)
    | Event_before,  (Event_after _ | Event_before) -> ev',  ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)

let weaken_event ev cont =
  match ev.ev_kind with
    Event_after _ ->
      begin match cont with
        Kpush :: Kevent ({ev_repr = Event_none} as ev') :: c ->
          begin match ev.ev_info with
            Event_return _ ->
              (* Weaken event *)
              let repr = ref 1 in
              let ev =
                copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
              and ev' =
                copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr)
              in
              Kevent ev :: Kpush :: Kevent ev' :: c
          | _ ->
              (* Only keep following event, equivalent *)
              cont
          end
      | _ ->
          Kevent ev :: cont
      end
  | _ ->
      Kevent ev :: cont

let add_event ev =
  function
    Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont               -> weaken_event ev cont

(* Pseudo events are ignored by the debugger. They are only used for
   generating backtraces.

   We prefer adding this event here rather than in lambda generation
   1) there are many different situations where a Pmakeblock can
      be generated
   2) we prefer inserting a pseudo event rather than an event after
      to prevent the debugger to stop at every single allocation. *)
let add_pseudo_event loc modname c =
  if !Clflags.debug then
    let ev_defname = string_of_scoped_location loc in
    let ev =
      { ev_pos = 0;                   (* patched in emitcode *)
        ev_module = modname;
        ev_loc = to_location loc;
        ev_defname;
        ev_kind = Event_pseudo;
        ev_info = Event_other;        (* Dummy *)
        ev_typenv = Env.Env_empty;    (* Dummy *)
        ev_typsubst = Subst.identity; (* Dummy *)
        ev_compenv = empty_env;       (* Dummy *)
        ev_stacksize = 0;             (* Dummy *)
        ev_repr = Event_none }        (* Dummy *)
    in
    add_event ev c
  else c

(**** Compilation of a lambda expression ****)

let try_blocks = ref []  (* list of stack size for each nested try block *)

(* association staticraise numbers -> (lbl,size of stack, try_blocks *)

let sz_static_raises = ref []

let push_static_raise i lbl_handler sz =
  sz_static_raises := (i, (lbl_handler, sz, !try_blocks)) :: !sz_static_raises

let find_raise_label i =
  try
    List.assoc i !sz_static_raises
  with
  | Not_found ->
      Misc.fatal_error
        ("exit("^Int.to_string i^") outside appropriated catch")

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump l sz = match l with
| Lstaticraise (i,[]) ->
    let label,size,tb = find_raise_label i in
    if sz = size && tb == !try_blocks then
      Some label
    else
      None
| _ -> None

(* Function bodies that remain to be compiled *)

type function_to_compile =
  { params: Ident.t list;               (* function parameters *)
    body: lambda;                       (* the function body *)
    label: label;                       (* the label of the function entry *)
    free_vars: Ident.t list;            (* free variables of the function *)
    num_defs: int;            (* number of mutually recursive definitions *)
    rec_vars: Ident.t list;             (* mutually recursive fn names *)
    rec_pos: int }                      (* rank in recursive definition *)

let functions_to_compile  = (Stack.create () : function_to_compile Stack.t)

(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref ""

(* Maximal stack size reached during the current function body *)

let max_stack_used = ref 0


(* Sequence of string tests *)


(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let comp_bint_primitive bi suff args =
  let pref =
    match bi with Pnativeint -> "caml_nativeint_"
                | Pint32 -> "caml_int32_"
                | Pint64 -> "caml_int64_" in
  Kccall(pref ^ suff, List.length args)

let comp_primitive p args =
  match p with
    Pgetglobal cu ->
      Kgetglobal (cu |> Compilation_unit.to_global_ident_for_bytecode)
  | Psetglobal cu ->
      Ksetglobal (cu |> Compilation_unit.to_global_ident_for_bytecode)
  | Pgetpredef id -> Kgetglobal id
  | Pintcomp cmp -> Kintcomp cmp
  | Pcompare_ints -> Kccall("caml_int_compare", 2)
  | Pcompare_floats -> Kccall("caml_float_compare", 2)
  | Pcompare_bints bi -> comp_bint_primitive bi "compare" args
  | Pfield (n, _sem) -> Kgetfield n
  | Pfield_computed _sem -> Kgetvectitem
  | Psetfield(n, _ptr, _init) -> Ksetfield n
  | Psetfield_computed(_ptr, _init) -> Ksetvectitem
  | Pfloatfield (n, _sem, _mode) -> Kgetfloatfield n
  | Psetfloatfield (n, _init) -> Ksetfloatfield n
  | Pduprecord _ -> Kccall("caml_obj_dup", 1)
  | Pccall p -> Kccall(p.prim_name, p.prim_arity)
  | Pnegint -> Knegint
  | Paddint -> Kaddint
  | Psubint -> Ksubint
  | Pmulint -> Kmulint
  | Pdivint _ -> Kdivint
  | Pmodint _ -> Kmodint
  | Pandint -> Kandint
  | Porint -> Korint
  | Pxorint -> Kxorint
  | Plslint -> Klslint
  | Plsrint -> Klsrint
  | Pasrint -> Kasrint
  | Poffsetint n -> Koffsetint n
  | Poffsetref n -> Koffsetref n
  | Pintoffloat -> Kccall("caml_int_of_float", 1)
  | Pfloatofint _ -> Kccall("caml_float_of_int", 1)
  | Pnegfloat _ -> Kccall("caml_neg_float", 1)
  | Pabsfloat _ -> Kccall("caml_abs_float", 1)
  | Paddfloat _ -> Kccall("caml_add_float", 2)
  | Psubfloat _ -> Kccall("caml_sub_float", 2)
  | Pmulfloat _ -> Kccall("caml_mul_float", 2)
  | Pdivfloat _ -> Kccall("caml_div_float", 2)
  | Pstringlength -> Kccall("caml_ml_string_length", 1)
  | Pbyteslength -> Kccall("caml_ml_bytes_length", 1)
  | Pstringrefs -> Kccall("caml_string_get", 2)
  | Pbytesrefs -> Kccall("caml_bytes_get", 2)
  | Pbytessets -> Kccall("caml_bytes_set", 3)
  | Pstringrefu -> Kgetstringchar
  | Pbytesrefu -> Kgetbyteschar
  | Pbytessetu -> Ksetbyteschar
  | Pstring_load_16(_) -> Kccall("caml_string_get16", 2)
  | Pstring_load_32(_) -> Kccall("caml_string_get32", 2)
  | Pstring_load_64(_) -> Kccall("caml_string_get64", 2)
  | Pbytes_set_16(_) -> Kccall("caml_bytes_set16", 3)
  | Pbytes_set_32(_) -> Kccall("caml_bytes_set32", 3)
  | Pbytes_set_64(_) -> Kccall("caml_bytes_set64", 3)
  | Pbytes_load_16(_) -> Kccall("caml_bytes_get16", 2)
  | Pbytes_load_32(_) -> Kccall("caml_bytes_get32", 2)
  | Pbytes_load_64(_) -> Kccall("caml_bytes_get64", 2)
  | Parraylength _ -> Kvectlength
  (* In bytecode, nothing is ever actually stack-allocated, so we ignore the
     array modes (allocation for [Parrayref{s,u}], modification for
     [Parrayset{s,u}]). *)
  | Parrayrefs (Pgenarray_ref _) -> Kccall("caml_array_get", 2)
  | Parrayrefs (Pfloatarray_ref _) -> Kccall("caml_floatarray_get", 2)
  | Parrayrefs (Paddrarray_ref | Pintarray_ref) ->
      Kccall("caml_array_get_addr", 2)
  | Parraysets (Pgenarray_set _) -> Kccall("caml_array_set", 3)
  | Parraysets Pfloatarray_set -> Kccall("caml_floatarray_set", 3)
  | Parraysets (Paddrarray_set _ | Pintarray_set) ->
      Kccall("caml_array_set_addr", 3)
  | Parrayrefu (Pgenarray_ref _) -> Kccall("caml_array_unsafe_get", 2)
  | Parrayrefu (Pfloatarray_ref _) -> Kccall("caml_floatarray_unsafe_get", 2)
  | Parrayrefu (Paddrarray_ref | Pintarray_ref) -> Kgetvectitem
  | Parraysetu (Pgenarray_set _) -> Kccall("caml_array_unsafe_set", 3)
  | Parraysetu Pfloatarray_set -> Kccall("caml_floatarray_unsafe_set", 3)
  | Parraysetu (Paddrarray_set _ | Pintarray_set) -> Ksetvectitem
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     Kccall(Printf.sprintf "caml_sys_const_%s" const_name, 1)
  | Pisint _ -> Kisint
  | Pisout -> Kisout
  | Pbintofint (bi,_) -> comp_bint_primitive bi "of_int" args
  | Pintofbint bi -> comp_bint_primitive bi "to_int" args
  | Pcvtbint(src, dst, _) ->
      begin match (src, dst) with
      | (Pint32, Pnativeint) -> Kccall("caml_nativeint_of_int32", 1)
      | (Pnativeint, Pint32) -> Kccall("caml_nativeint_to_int32", 1)
      | (Pint32, Pint64) -> Kccall("caml_int64_of_int32", 1)
      | (Pint64, Pint32) -> Kccall("caml_int64_to_int32", 1)
      | (Pnativeint, Pint64) -> Kccall("caml_int64_of_nativeint", 1)
      | (Pint64, Pnativeint) -> Kccall("caml_int64_to_nativeint", 1)
      | ((Pint32 | Pint64 | Pnativeint), _) ->
          fatal_error "Bytegen.comp_primitive: invalid Pcvtbint cast"
      end
  | Pnegbint (bi,_) -> comp_bint_primitive bi "neg" args
  | Paddbint (bi,_) -> comp_bint_primitive bi "add" args
  | Psubbint (bi,_) -> comp_bint_primitive bi "sub" args
  | Pmulbint (bi,_) -> comp_bint_primitive bi "mul" args
  | Pdivbint { size = bi } -> comp_bint_primitive bi "div" args
  | Pmodbint { size = bi } -> comp_bint_primitive bi "mod" args
  | Pandbint(bi,_) -> comp_bint_primitive bi "and" args
  | Porbint(bi,_) -> comp_bint_primitive bi "or" args
  | Pxorbint(bi,_) -> comp_bint_primitive bi "xor" args
  | Plslbint(bi,_) -> comp_bint_primitive bi "shift_left" args
  | Plsrbint(bi,_) -> comp_bint_primitive bi "shift_right_unsigned" args
  | Pasrbint(bi,_) -> comp_bint_primitive bi "shift_right" args
  | Pbintcomp(_, Ceq) -> Kccall("caml_equal", 2)
  | Pbintcomp(_, Cne) -> Kccall("caml_notequal", 2)
  | Pbintcomp(_, Clt) -> Kccall("caml_lessthan", 2)
  | Pbintcomp(_, Cgt) -> Kccall("caml_greaterthan", 2)
  | Pbintcomp(_, Cle) -> Kccall("caml_lessequal", 2)
  | Pbintcomp(_, Cge) -> Kccall("caml_greaterequal", 2)
  | Pbigarrayref(_, n, _, _) -> Kccall("caml_ba_get_" ^ Int.to_string n, n + 1)
  | Pbigarrayset(_, n, _, _) -> Kccall("caml_ba_set_" ^ Int.to_string n, n + 2)
  | Pbigarraydim(n) -> Kccall("caml_ba_dim_" ^ Int.to_string n, 1)
  | Pbigstring_load_16(_) -> Kccall("caml_ba_uint8_get16", 2)
  | Pbigstring_load_32(_) -> Kccall("caml_ba_uint8_get32", 2)
  | Pbigstring_load_64(_) -> Kccall("caml_ba_uint8_get64", 2)
  | Pbigstring_set_16(_) -> Kccall("caml_ba_uint8_set16", 3)
  | Pbigstring_set_32(_) -> Kccall("caml_ba_uint8_set32", 3)
  | Pbigstring_set_64(_) -> Kccall("caml_ba_uint8_set64", 3)
  | Pbswap16 -> Kccall("caml_bswap16", 1)
  | Pbbswap(bi,_) -> comp_bint_primitive bi "bswap" args
  | Pint_as_pointer -> Kccall("caml_int_as_pointer", 1)
  | Pbytes_to_string -> Kccall("caml_string_of_bytes", 1)
  | Pbytes_of_string -> Kccall("caml_bytes_of_string", 1)
  | Parray_to_iarray -> Kccall("caml_iarray_of_array", 1)
  | Parray_of_iarray -> Kccall("caml_array_of_iarray", 1)
  | Pobj_dup -> Kccall("caml_obj_dup", 1)
  (* The cases below are handled in [comp_expr] before the [comp_primitive] call
     (in the order in which they appear below),
     so they should never be reached in this function. *)
  | Pignore | Popaque _ | Pobj_magic _
  | Pnot | Psequand | Psequor
  | Praise _
  | Pmakearray _ | Pduparray _
  | Pfloatcomp _
  | Pmakeblock _
  | Pmakefloatblock _
  | Pprobe_is_enabled _
  | Punbox_float | Pbox_float _ | Punbox_int _ | Pbox_int _
    ->
      fatal_error "Bytegen.comp_primitive"

let is_immed n = immed_min <= n && n <= immed_max

let is_nontail = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

module Storer =
  Switch.Store
    (struct type t = lambda type key = lambda
      let compare_key = Stdlib.compare
      let make_key = Lambda.make_key end)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rec comp_expr env exp sz cont =
  if sz > !max_stack_used then max_stack_used := sz;
  match exp with
    Lvar id | Lmutvar id ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        Kacc(sz - pos) :: cont
      with Not_found ->
      try
        let pos = Ident.find_same id env.ce_heap in
        Kenvacc(pos) :: cont
      with Not_found ->
      try
        let ofs = Ident.find_same id env.ce_rec in
        Koffsetclosure(ofs) :: cont
      with Not_found ->
        fatal_error ("Bytegen.comp_expr: var " ^ Ident.unique_name id)
      end
  | Lconst cst ->
      Kconst cst :: cont
  | Lapply{ap_func = func; ap_args = args; ap_region_close = rc} ->
      let nargs = List.length args in
      if is_tailcall cont && not (is_nontail rc) then begin
        comp_args env args sz
          (Kpush :: comp_expr env func (sz + nargs)
            (Kappterm(nargs, sz + nargs) :: discard_dead_code cont))
      end else begin
        if nargs < 4 then
          comp_args env args sz
            (Kpush :: comp_expr env func (sz + nargs) (Kapply nargs :: cont))
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args env args (sz + 3)
            (Kpush :: comp_expr env func (sz + 3 + nargs)
                      (Kapply nargs :: cont1))
        end
      end
  | Lsend(kind, met, obj, args, rc, _, _, _) ->
      assert (kind <> Cached);
      let nargs = List.length args + 1 in
      let getmethod, args' =
        if kind = Self then (Kgetmethod, met::obj::args) else
        match met with
          Lconst(Const_base(Const_int n)) -> (Kgetpubmet n, obj::args)
        | _ -> (Kgetdynmet, met::obj::args)
      in
      if is_tailcall cont && not (is_nontail rc) then
        comp_args env args' sz
          (getmethod :: Kappterm(nargs, sz + nargs) :: discard_dead_code cont)
      else
        if nargs < 4 then
          comp_args env args' sz
            (getmethod :: Kapply nargs :: cont)
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args env args' (sz + 3)
            (getmethod :: Kapply nargs :: cont1)
        end
  | Lfunction{params; body; loc} -> (* assume kind = Curried *)
      let cont = add_pseudo_event loc !compunit_name cont in
      let lbl = new_label() in
      let fv = Ident.Set.elements(free_variables exp) in
      let to_compile =
        { params = List.map fst params; body = body; label = lbl;
          free_vars = fv; num_defs = 1; rec_vars = []; rec_pos = 0 } in
      Stack.push to_compile functions_to_compile;
      comp_args env (List.map (fun n -> Lvar n) fv) sz
        (Kclosure(lbl, List.length fv) :: cont)
  | Llet(_, _k, id, arg, body)
  | Lmutlet(_k, id, arg, body) ->
      comp_expr env arg sz
        (Kpush :: comp_expr (add_var id (sz+1) env) body (sz+1)
          (add_pop 1 cont))
  | Lletrec(decl, body) ->
      let ndecl = List.length decl in
      if List.for_all (function (_, Lfunction _) -> true | _ -> false)
                      decl then begin
        (* let rec of functions *)
        let fv =
          Ident.Set.elements (free_variables (Lletrec(decl, lambda_unit))) in
        let rec_idents = List.map (fun (id, _lam) -> id) decl in
        let rec comp_fun pos = function
            [] -> []
          | (_id, Lfunction{params; body}) :: rem ->
              let lbl = new_label() in
              let to_compile =
                { params = List.map fst params; body = body; label = lbl;
                  free_vars = fv; num_defs = ndecl; rec_vars = rec_idents;
                  rec_pos = pos} in
              Stack.push to_compile functions_to_compile;
              lbl :: comp_fun (pos + 1) rem
          | _ -> assert false in
        let lbls = comp_fun 0 decl in
        comp_args env (List.map (fun n -> Lvar n) fv) sz
          (Kclosurerec(lbls, List.length fv) ::
            (comp_expr (add_vars rec_idents (sz+1) env) body (sz + ndecl)
                       (add_pop ndecl cont)))
      end else begin
        let decl_size =
          List.map (fun (id, exp) -> (id, exp, size_of_lambda Ident.empty exp))
            decl in
        let rec comp_init new_env sz = function
          | [] -> comp_nonrec new_env sz ndecl decl_size
          | (id, _exp, RHS_floatblock blocksize) :: rem ->
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy_float", 1) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, _exp, RHS_block blocksize) :: rem ->
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy", 1) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, _exp, RHS_infix { blocksize; offset }) :: rem ->
              Kconst(Const_base(Const_int offset)) ::
              Kpush ::
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy_infix", 2) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, _exp, RHS_function (blocksize,arity)) :: rem ->
              Kconst(Const_base(Const_int arity)) ::
              Kpush ::
              Kconst(Const_base(Const_int blocksize)) ::
              Kccall("caml_alloc_dummy_function", 2) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
          | (id, _exp, RHS_nonrec) :: rem ->
              Kconst(Const_base(Const_int 0)) :: Kpush ::
              comp_init (add_var id (sz+1) new_env) (sz+1) rem
        and comp_nonrec new_env sz i = function
          | [] -> comp_rec new_env sz ndecl decl_size
          | (_id, _exp, (RHS_block _ | RHS_infix _ |
                         RHS_floatblock _ | RHS_function _))
            :: rem ->
              comp_nonrec new_env sz (i-1) rem
          | (_id, exp, RHS_nonrec) :: rem ->
              comp_expr new_env exp sz
                (Kassign (i-1) :: comp_nonrec new_env sz (i-1) rem)
        and comp_rec new_env sz i = function
          | [] -> comp_expr new_env body sz (add_pop ndecl cont)
          | (_id, exp, (RHS_block _ | RHS_infix _ |
                        RHS_floatblock _ | RHS_function _))
            :: rem ->
              comp_expr new_env exp sz
                (Kpush :: Kacc i :: Kccall("caml_update_dummy", 2) ::
                 comp_rec new_env sz (i-1) rem)
          | (_id, _exp, RHS_nonrec) :: rem ->
              comp_rec new_env sz (i-1) rem
        in
        comp_init env sz decl_size
      end
  | Lprim((Popaque _ | Pobj_magic _), [arg], _) ->
      comp_expr env arg sz cont
  | Lprim((Pbox_float _ | Punbox_float), [arg], _) ->
      comp_expr env arg sz cont
  | Lprim((Pbox_int _ | Punbox_int _), [arg], _) ->
      comp_expr env arg sz cont
  | Lprim(Pignore, [arg], _) ->
      comp_expr env arg sz (add_const_unit cont)
  | Lprim(Pnot, [arg], _) ->
      let newcont =
        match cont with
          Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
        | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
        | _ -> Kboolnot :: cont in
      comp_expr env arg sz newcont
  | Lprim(Psequand, [exp1; exp2], _) ->
      begin match cont with
        Kbranchifnot lbl :: _ ->
          comp_expr env exp1 sz (Kbranchifnot lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchif lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchifnot lbl2 ::
            comp_expr env exp2 sz (Kbranchif lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchifnot lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Psequor, [exp1; exp2], _) ->
      begin match cont with
        Kbranchif lbl :: _ ->
          comp_expr env exp1 sz (Kbranchif lbl ::
            comp_expr env exp2 sz cont)
      | Kbranchifnot lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr env exp1 sz (Kbranchif lbl2 ::
            comp_expr env exp2 sz (Kbranchifnot lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr env exp1 sz (Kstrictbranchif lbl ::
            comp_expr env exp2 sz cont1)
      end
  | Lprim(Praise k, [arg], _) ->
      comp_expr env arg sz (Kraise k :: discard_dead_code cont)
  | Lprim(Paddint, [arg; Lconst(Const_base(Const_int n))], _)
    when is_immed n ->
      comp_expr env arg sz (Koffsetint n :: cont)
  | Lprim(Psubint, [arg; Lconst(Const_base(Const_int n))], _)
    when is_immed (-n) ->
      comp_expr env arg sz (Koffsetint (-n) :: cont)
  | Lprim (Poffsetint n, [arg], _)
    when not (is_immed n) ->
      comp_expr env arg sz
        (Kpush::
         Kconst (Const_base (Const_int n))::
         Kaddint::cont)
  | Lprim (Pmakefloatblock _, args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args env args sz (Kmakefloatblock (List.length args) :: cont)
  | Lprim(Pmakearray (kind, _, _), args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      begin match kind with
        Pintarray | Paddrarray ->
          comp_args env args sz (Kmakeblock(List.length args, 0) :: cont)
      | Pfloatarray ->
          comp_args env args sz (Kmakefloatblock(List.length args) :: cont)
      | Pgenarray ->
          if args = []
          then Kmakeblock(0, 0) :: cont
          else comp_args env args sz
                 (Kmakeblock(List.length args, 0) ::
                  Kccall("caml_make_array", 1) :: cont)
      end
  | Lprim (Pduparray (kind, mutability),
           [Lprim (Pmakearray (kind',_,m),args,_)], loc) ->
      assert (kind = kind');
      comp_expr env (Lprim (Pmakearray (kind, mutability, m), args, loc)) sz cont
  | Lprim (Pduparray _, [arg], loc) ->
      let prim_obj_dup =
        Primitive.simple_on_values ~name:"caml_obj_dup" ~arity:1 ~alloc:true
      in
      comp_expr env (Lprim (Pccall prim_obj_dup, [arg], loc)) sz cont
  | Lprim (Pduparray _, _, _) ->
      Misc.fatal_error "Bytegen.comp_expr: Pduparray takes exactly one arg"
(* Integer first for enabling further optimization (cf. emitcode.ml)  *)
  | Lprim (Pintcomp c, [arg ; (Lconst _ as k)], _) ->
      let p = Pintcomp (swap_integer_comparison c)
      and args = [k ; arg] in
      comp_args env args sz (comp_primitive p args :: cont)
  | Lprim (Pfloatcomp cmp, args, _) ->
      let cont =
        match cmp with
        | CFeq -> Kccall("caml_eq_float", 2) :: cont
        | CFneq -> Kccall("caml_neq_float", 2) :: cont
        | CFlt -> Kccall("caml_lt_float", 2) :: cont
        | CFnlt -> Kccall("caml_lt_float", 2) :: Kboolnot :: cont
        | CFgt -> Kccall("caml_gt_float", 2) :: cont
        | CFngt -> Kccall("caml_gt_float", 2) :: Kboolnot :: cont
        | CFle -> Kccall("caml_le_float", 2) :: cont
        | CFnle -> Kccall("caml_le_float", 2) :: Kboolnot :: cont
        | CFge -> Kccall("caml_ge_float", 2) :: cont
        | CFnge -> Kccall("caml_ge_float", 2) :: Kboolnot :: cont
      in
      comp_args env args sz cont
  | Lprim(Pmakeblock(tag, _mut, _, _), args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args env args sz (Kmakeblock(List.length args, tag) :: cont)
  | Lprim(Pfloatfield (n, _, _), args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args env args sz (Kgetfloatfield n :: cont)
  | Lprim(p, args, _) ->
      comp_args env args sz (comp_primitive p args :: cont)
  | Lstaticcatch (body, (i, vars) , handler, _) ->
      let vars = List.map fst vars in
      let nvars = List.length vars in
      let branch1, cont1 = make_branch cont in
      let r =
        if nvars <> 1 then begin (* general case *)
          let lbl_handler, cont2 =
            label_code
              (comp_expr
                (add_vars vars (sz+1) env)
                handler (sz+nvars) (add_pop nvars cont1)) in
          push_static_raise i lbl_handler (sz+nvars);
          push_dummies nvars
            (comp_expr env body (sz+nvars)
            (add_pop nvars (branch1 :: cont2)))
        end else begin (* small optimization for nvars = 1 *)
          let var = match vars with [var] -> var | _ -> assert false in
          let lbl_handler, cont2 =
            label_code
              (Kpush::comp_expr
                (add_var var (sz+1) env)
                handler (sz+1) (add_pop 1 cont1)) in
          push_static_raise i lbl_handler sz;
          comp_expr env body sz (branch1 :: cont2)
        end in
      sz_static_raises := List.tl !sz_static_raises ;
      r
  | Lstaticraise (i, args) ->
      let cont = discard_dead_code cont in
      let label,size,tb = find_raise_label i in
      let cont = branch_to label cont in
      let rec loop sz tbb =
        if tb == tbb then add_pop (sz-size) cont
        else match tbb with
        | [] -> assert false
        | try_sz :: tbb -> add_pop (sz-try_sz-4) (Kpoptrap :: loop try_sz tbb)
      in
      let cont = loop sz !try_blocks in
      begin match args with
      | [arg] -> (* optim, argument passed in accumulator *)
          comp_expr env arg sz cont
      | _ -> comp_exit_args env args sz size cont
      end
  | Ltrywith(body, id, handler, _kind) ->
      let (branch1, cont1) = make_branch cont in
      let lbl_handler = new_label() in
      let body_cont =
        Kpoptrap :: branch1 ::
        Klabel lbl_handler :: Kpush ::
        comp_expr (add_var id (sz+1) env) handler (sz+1) (add_pop 1 cont1)
      in
      try_blocks := sz :: !try_blocks;
      let l = comp_expr env body (sz+4) body_cont in
      try_blocks := List.tl !try_blocks;
      Kpushtrap lbl_handler :: l
  | Lifthenelse(cond, ifso, ifnot, _kind) ->
      comp_binary_test env cond ifso ifnot sz cont
  | Lsequence(exp1, exp2) ->
      comp_expr env exp1 sz (comp_expr env exp2 sz cont)
  | Lwhile {wh_cond; wh_body} ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals ::
        comp_expr env wh_body sz
          (Klabel lbl_test ::
            comp_expr env wh_cond sz
              (Kbranchif lbl_loop :: add_const_unit cont))
  | Lfor {for_id; for_from; for_to; for_dir; for_body} ->
      let lbl_loop = new_label() in
      let lbl_exit = new_label() in
      let offset = match for_dir with Upto -> 1 | Downto -> -1 in
      let comp = match for_dir with Upto -> Cgt | Downto -> Clt in
      comp_expr env for_from sz
        (Kpush :: comp_expr env for_to (sz+1)
          (Kpush :: Kpush :: Kacc 2 :: Kintcomp comp :: Kbranchif lbl_exit ::
           Klabel lbl_loop :: Kcheck_signals ::
           comp_expr (add_var for_id (sz+1) env) for_body (sz+2)
             (Kacc 1 :: Kpush :: Koffsetint offset :: Kassign 2 ::
              Kacc 1 :: Kintcomp Cne :: Kbranchif lbl_loop ::
              Klabel lbl_exit :: add_const_unit (add_pop 2 cont))))
  | Lswitch(arg, sw, _loc, _kind) ->
      let (branch, cont1) = make_branch cont in
      let c = ref (discard_dead_code cont1) in

(* Build indirection vectors *)
      let store = Storer.mk_store () in
      let act_consts = Array.make sw.sw_numconsts 0
      and act_blocks = Array.make sw.sw_numblocks 0 in
      begin match sw.sw_failaction with (* default is index 0 *)
      | Some fail -> ignore (store.act_store () fail)
      | None      -> ()
      end ;
      List.iter
        (fun (n, act) -> act_consts.(n) <- store.act_store () act) sw.sw_consts;
      List.iter
        (fun (n, act) -> act_blocks.(n) <- store.act_store () act) sw.sw_blocks;
(* Compile and label actions *)
      let acts = store.act_get () in
(*
      let a = store.act_get_shared () in
      Array.iter
        (function
          | Switch.Shared (Lstaticraise _) -> ()
          | Switch.Shared act ->
              Printlambda.lambda Format.str_formatter act ;
              Printf.eprintf "SHARE BYTE:\n%s\n" (Format.flush_str_formatter ())
          | _ -> ())
        a ;
*)
      let lbls = Array.make (Array.length acts) 0 in
      for i = Array.length acts-1 downto 0 do
        let lbl,c1 = label_code (comp_expr env acts.(i) sz (branch :: !c)) in
        lbls.(i) <- lbl ;
        c := discard_dead_code c1
      done ;

(* Build label vectors *)
      let lbl_blocks = Array.make sw.sw_numblocks 0 in
      for i = sw.sw_numblocks - 1 downto 0 do
        lbl_blocks.(i) <- lbls.(act_blocks.(i))
      done;
      let lbl_consts = Array.make sw.sw_numconsts 0 in
      for i = sw.sw_numconsts - 1 downto 0 do
        lbl_consts.(i) <- lbls.(act_consts.(i))
      done;
      comp_expr env arg sz (Kswitch(lbl_consts, lbl_blocks) :: !c)
  | Lstringswitch (arg,sw,d,loc, kind) ->
      comp_expr env (Matching.expand_stringswitch loc kind arg sw d) sz cont
  | Lassign(id, expr) ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        comp_expr env expr sz (Kassign(sz - pos) :: cont)
      with Not_found ->
        fatal_error "Bytegen.comp_expr: assign"
      end
  | Levent(lam, lev) ->
      let ev_defname = match lev.lev_loc with
        | Loc_unknown -> "??"
        | Loc_known { loc = _; scopes } -> string_of_scopes scopes in
      let event kind info =
        { ev_pos = 0;                   (* patched in emitcode *)
          ev_module = !compunit_name;
          ev_loc = to_location lev.lev_loc;
          ev_kind = kind;
          ev_defname;
          ev_info = info;
          ev_typenv = Env.summary lev.lev_env;
          ev_typsubst = Subst.identity;
          ev_compenv = env;
          ev_stacksize = sz;
          ev_repr =
            begin match lev.lev_repr with
              None ->
                Event_none
            | Some ({contents = 1} as repr) when lev.lev_kind = Lev_function ->
                Event_child repr
            | Some ({contents = 1} as repr) ->
                Event_parent repr
            | Some repr when lev.lev_kind = Lev_function ->
                Event_parent repr
            | Some repr ->
                Event_child repr
            end }
      in
      begin match lev.lev_kind with
        Lev_before ->
          let c = comp_expr env lam sz cont in
          let ev = event Event_before Event_other in
          add_event ev c
      | Lev_function ->
          let c = comp_expr env lam sz cont in
          let ev = event Event_pseudo Event_function in
          add_event ev c
      | Lev_pseudo ->
          let c = comp_expr env lam sz cont in
          let ev = event Event_pseudo Event_other in
          add_event ev c
      | Lev_after ty ->
          let preserve_tailcall =
            match lam with
            | Lprim(prim, _, _) -> preserve_tailcall_for_prim prim
            | Lapply {ap_region_close=rc; _}
            | Lsend(_, _, _, _, rc, _, _, _) ->
               not (is_nontail rc)
            | _ -> true
          in
          if preserve_tailcall && is_tailcall cont then
            (* don't destroy tail call opt *)
            comp_expr env lam sz cont
          else begin
            let info =
              match lam with
                Lapply{ap_args = args}  -> Event_return (List.length args)
              | Lsend(_, _, _, args, _, _, _, _) ->
                  Event_return (List.length args + 1)
              | Lprim(_,args,_)         -> Event_return (List.length args)
              | _                       -> Event_other
            in
            let ev = event (Event_after ty) info in
            let cont1 = add_event ev cont in
            comp_expr env lam sz cont1
          end
      end
  | Lifused (_, exp) ->
      comp_expr env exp sz cont
  | Lregion (exp, _) ->
      comp_expr env exp sz cont
  | Lexclave exp ->
      comp_expr env exp sz cont

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args env argl sz cont =
  comp_expr_list env (List.rev argl) sz cont

and comp_expr_list env exprl sz cont = match exprl with
    [] -> cont
  | [exp] -> comp_expr env exp sz cont
  | exp :: rem ->
      comp_expr env exp sz (Kpush :: comp_expr_list env rem (sz+1) cont)

and comp_exit_args  env argl sz pos cont =
   comp_expr_list_assign env (List.rev argl) sz pos cont

and comp_expr_list_assign env exprl sz pos cont = match exprl with
  | [] -> cont
  | exp :: rem ->
      comp_expr env exp sz
        (Kassign (sz-pos)::comp_expr_list_assign env rem sz (pos-1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit then begin
      let (lbl_end, cont1) = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr env ifso sz cont1
    end else
    match code_as_jump ifso sz with
    | Some label ->
      let cont = comp_expr env ifnot sz cont in
      Kbranchif label :: cont
    | _ ->
        match code_as_jump ifnot sz with
        | Some label ->
            let cont = comp_expr env ifso sz cont in
            Kbranchifnot label :: cont
        | _ ->
            let (branch_end, cont1) = make_branch cont in
            let (lbl_not, cont2) = label_code(comp_expr env ifnot sz cont1) in
            Kbranchifnot lbl_not ::
            comp_expr env ifso sz (branch_end :: cont2) in

  comp_expr env cond sz cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  max_stack_used := 0;
  let code = comp_expr env exp sz cont in
  let used_safe = !max_stack_used + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold then
    Kconst(Const_base(Const_int used_safe)) ::
    Kccall("caml_ensure_stack_capacity", 1) ::
    code
  else
    code

(**** Compilation of functions ****)

let comp_function tc cont =
  let arity = List.length tc.params in
  let rec positions pos delta = function
      [] -> Ident.empty
    | id :: rem -> Ident.add id pos (positions (pos + delta) delta rem) in
  let env =
    { ce_stack = positions arity (-1) tc.params;
      ce_heap = positions (3 * (tc.num_defs - tc.rec_pos) - 1) 1 tc.free_vars;
      ce_rec = positions (-3 * tc.rec_pos) 3 tc.rec_vars } in
  let cont =
    comp_block env tc.body arity (Kreturn arity :: cont) in
  if arity > 1 then
    Krestart :: Klabel tc.label :: Kgrab(arity - 1) :: cont
  else
    Klabel tc.label :: cont

let comp_remainder cont =
  let c = ref cont in
  begin try
    while true do
      c := comp_function (Stack.pop functions_to_compile) !c
    done
  with Stack.Empty ->
    ()
  end;
  !c

(**** Compilation of a lambda phrase ****)

let compile_implementation modulename expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  sz_static_raises := [] ;
  compunit_name := modulename;
  let init_code = comp_block empty_env expr 0 [] in
  if Stack.length functions_to_compile > 0 then begin
    let lbl_init = new_label() in
    Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code)
  end else
    init_code

let compile_phrase expr =
  Stack.clear functions_to_compile;
  label_counter := 0;
  sz_static_raises := [] ;
  let init_code = comp_block empty_env expr 1 [Kreturn 1] in
  let fun_code = comp_remainder [] in
  (init_code, fun_code)

let reset () =
  label_counter := 0;
  sz_static_raises := [];
  compunit_name := "";
  Stack.clear functions_to_compile;
  max_stack_used := 0
