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

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Layouts
open Types
open Btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ident.create_predef

let ident_int = ident_create "int"
and ident_char = ident_create "char"
and ident_bytes = ident_create "bytes"
and ident_float = ident_create "float"
and ident_float_unboxed = ident_create "float#"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_exn = ident_create "exn"
and ident_array = ident_create "array"
and ident_iarray = ident_create "iarray"
and ident_list = ident_create "list"
and ident_option = ident_create "option"
and ident_nativeint = ident_create "nativeint"
and ident_int32 = ident_create "int32"
and ident_int64 = ident_create "int64"
and ident_vec128 = ident_create "vec128"
and ident_lazy_t = ident_create "lazy_t"
and ident_string = ident_create "string"
and ident_extension_constructor = ident_create "extension_constructor"
and ident_floatarray = ident_create "floatarray"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
(* CR layouts v2: we need to look through all the places where [path_float] is
   checked and add a corresponding check for [path_float_unboxed]. *)
and path_float_unboxed = Pident ident_float_unboxed
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_iarray = Pident ident_iarray
and path_list = Pident ident_list
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_vec128 = Pident ident_vec128
and path_lazy_t = Pident ident_lazy_t
and path_string = Pident ident_string
and path_extension_constructor = Pident ident_extension_constructor
and path_floatarray = Pident ident_floatarray

let type_int = newgenty (Tconstr(path_int, [], ref Mnil))
and type_char = newgenty (Tconstr(path_char, [], ref Mnil))
and type_bytes = newgenty (Tconstr(path_bytes, [], ref Mnil))
and type_float = newgenty (Tconstr(path_float, [], ref Mnil))
and type_float_unboxed = newgenty (Tconstr(path_float_unboxed, [], ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, [], ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, [], ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, [], ref Mnil))
and type_array t = newgenty (Tconstr(path_array, [t], ref Mnil))
and type_iarray t = newgenty (Tconstr(path_iarray, [t], ref Mnil))
and type_list t = newgenty (Tconstr(path_list, [t], ref Mnil))
and type_option t = newgenty (Tconstr(path_option, [t], ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, [], ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, [], ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, [], ref Mnil))
and type_vec128 = newgenty (Tconstr(path_vec128, [], ref Mnil))
and type_lazy_t t = newgenty (Tconstr(path_lazy_t, [t], ref Mnil))
and type_string = newgenty (Tconstr(path_string, [], ref Mnil))
and type_extension_constructor =
      newgenty (Tconstr(path_extension_constructor, [], ref Mnil))
and type_floatarray = newgenty (Tconstr(path_floatarray, [], ref Mnil))

let ident_match_failure = ident_create "Match_failure"
and ident_out_of_memory = ident_create "Out_of_memory"
and ident_invalid_argument = ident_create "Invalid_argument"
and ident_failure = ident_create "Failure"
and ident_not_found = ident_create "Not_found"
and ident_sys_error = ident_create "Sys_error"
and ident_end_of_file = ident_create "End_of_file"
and ident_division_by_zero = ident_create "Division_by_zero"
and ident_stack_overflow = ident_create "Stack_overflow"
and ident_sys_blocked_io = ident_create "Sys_blocked_io"
and ident_assert_failure = ident_create "Assert_failure"
and ident_undefined_recursive_module =
        ident_create "Undefined_recursive_module"

let all_predef_exns = [
  ident_match_failure;
  ident_out_of_memory;
  ident_invalid_argument;
  ident_failure;
  ident_not_found;
  ident_sys_error;
  ident_end_of_file;
  ident_division_by_zero;
  ident_stack_overflow;
  ident_sys_blocked_io;
  ident_assert_failure;
  ident_undefined_recursive_module;
]

let path_match_failure = Pident ident_match_failure
and path_invalid_argument = Pident ident_invalid_argument
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Location.none;
    cd_attributes = [];
    cd_uid = Uid.of_predef_id id;
  }

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_void = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"
and ident_none = ident_create "None"
and ident_some = ident_create "Some"

let mk_add_type add_type
      ?manifest type_ident
      ?(kind=Type_abstract)
      ?(layout=Layout.value ~why:(Primitive type_ident))
      env =
  let decl =
    {type_params = [];
     type_arity = 0;
     type_kind = kind;
     type_layout = layout;
     type_loc = Location.none;
     type_private = Asttypes.Public;
     type_manifest = manifest;
     type_variance = [];
     type_separability = [];
     type_is_newtype = false;
     type_expansion_scope = lowest_level;
     type_attributes = [];
     type_unboxed_default = false;
     type_uid = Uid.of_predef_id type_ident;
    }
  in
  add_type type_ident decl env

(* CR layouts: Changes will be needed here as we add support for the built-ins
   to work with non-values, and as we relax the mixed block restriction. *)
let common_initial_env add_type add_extension empty_env =
  let add_type = mk_add_type add_type
  and add_type1 type_ident
        ?(kind=fun _ -> Type_abstract)
        ?(layout=Layout.value ~why:(Primitive type_ident))
      ~variance ~separability env =
    let param = newgenvar (Layout.value ~why:Type_argument) in
    let decl =
      {type_params = [param];
       type_arity = 1;
       type_kind = kind param;
       type_layout = layout;
       type_loc = Location.none;
       type_private = Asttypes.Public;
       type_manifest = None;
       type_variance = [variance];
       type_separability = [separability];
       type_is_newtype = false;
       type_expansion_scope = lowest_level;
       type_attributes = [];
       type_unboxed_default = false;
       type_uid = Uid.of_predef_id type_ident;
      }
    in
    add_type type_ident decl env
  in
  let add_extension id args layouts =
    add_extension id
      { ext_type_path = path_exn;
        ext_type_params = [];
        ext_args = Cstr_tuple (List.map (fun x -> (x, Unrestricted)) args);
        ext_arg_layouts = layouts;
        ext_constant = args = [];
        ext_ret_type = None;
        ext_private = Asttypes.Public;
        ext_loc = Location.none;
        ext_attributes = [Ast_helper.Attr.mk
                            (Location.mknoloc "ocaml.warn_on_literal_pattern")
                            (Parsetree.PStr [])];
        ext_uid = Uid.of_predef_id id;
      }
  in
  let variant constrs layouts = Type_variant (constrs, Variant_boxed layouts) in
  empty_env
  (* Predefined types - alphabetical order *)
  |> add_type1 ident_array
       ~variance:Variance.full
       ~separability:Separability.Ind
  |> add_type1 ident_iarray
       ~variance:Variance.covariant
       ~separability:Separability.Ind
  |> add_type ident_bool
       ~kind:(variant [cstr ident_false []; cstr ident_true []]
                [| [| |]; [| |] |])
       ~layout:(Layout.immediate ~why:Enumeration)
  |> add_type ident_char ~layout:(Layout.immediate ~why:(Primitive ident_char))
  |> add_type ident_exn
       ~kind:Type_open
       ~layout:(Layout.value ~why:Extensible_variant)
  |> add_type ident_extension_constructor
  |> add_type ident_float
  |> add_type ident_float_unboxed
  |> add_type ident_floatarray
  |> add_type ident_int ~layout:(Layout.immediate ~why:(Primitive ident_int))
  |> add_type ident_int32
  |> add_type ident_int64
  |> add_type1 ident_lazy_t
       ~variance:Variance.covariant
       ~separability:Separability.Ind
  |> add_type1 ident_list
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_nil [];
                  cstr ident_cons [tvar, Unrestricted;
                                   type_list tvar, Unrestricted]]
           [| [| |]; [| Layout.value ~why:Type_argument;
                        Layout.value ~why:Boxed_variant |] |] )
       ~layout:(Layout.value ~why:Boxed_variant)
  |> add_type ident_nativeint
  |> add_type1 ident_option
       ~variance:Variance.covariant
       ~separability:Separability.Ind
       ~kind:(fun tvar ->
         variant [cstr ident_none []; cstr ident_some [tvar, Unrestricted]]
           [| [| |]; [| Layout.value ~why:Type_argument |] |])
       ~layout:(Layout.value ~why:Boxed_variant)
  |> add_type ident_string
  |> add_type ident_unit
       ~kind:(variant [cstr ident_void []] [| [| |] |])
       ~layout:(Layout.immediate ~why:Enumeration)
  |> add_type ident_vec128
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[type_string; type_int; type_int])]
       [| Layout.value ~why:Tuple |]
  |> add_extension ident_division_by_zero [] [||]
  |> add_extension ident_end_of_file [] [||]
  |> add_extension ident_failure [type_string]
       [| Layout.value ~why:(Primitive ident_string) |]
  |> add_extension ident_invalid_argument [type_string]
       [| Layout.value ~why:(Primitive ident_string) |]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[type_string; type_int; type_int])]
       [| Layout.value ~why:Tuple |]
  |> add_extension ident_not_found [] [||]
  |> add_extension ident_out_of_memory [] [||]
  |> add_extension ident_stack_overflow [] [||]
  |> add_extension ident_sys_blocked_io [] [||]
  |> add_extension ident_sys_error [type_string]
       [| Layout.value ~why:(Primitive ident_string) |]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[type_string; type_int; type_int])]
       [| Layout.value ~why:Tuple |]

let build_initial_env add_type add_exception empty_env =
  let common = common_initial_env add_type add_exception empty_env in
  let add_type = mk_add_type add_type in
  let safe_string = add_type ident_bytes common in
  let unsafe_string = add_type ident_bytes ~manifest:type_string common in
  (safe_string, unsafe_string)

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
