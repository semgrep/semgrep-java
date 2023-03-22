(**
   Boilerplate to be used as a template when mapping the java CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_binary_integer_literal (env : env) (tok : CST.binary_integer_literal) =
  (* binary_integer_literal *) token env tok

let map_decimal_floating_point_literal (env : env) (tok : CST.decimal_floating_point_literal) =
  (* decimal_floating_point_literal *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_imm_tok_bslash_pat_36cdeeb (env : env) (tok : CST.imm_tok_bslash_pat_36cdeeb) =
  (* imm_tok_bslash_pat_36cdeeb *) token env tok

let map_hex_floating_point_literal (env : env) (tok : CST.hex_floating_point_literal) =
  (* hex_floating_point_literal *) token env tok

let map_character_literal (env : env) (tok : CST.character_literal) =
  (* character_literal *) token env tok

let map_decimal_integer_literal (env : env) (tok : CST.decimal_integer_literal) =
  (* decimal_integer_literal *) token env tok

let map_pat_9347a80 (env : env) (tok : CST.pat_9347a80) =
  (* pattern "\"[^\"]*" *) token env tok

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_integral_type (env : env) (x : CST.integral_type) =
  (match x with
  | `Byte tok -> R.Case ("Byte",
      (* "byte" *) token env tok
    )
  | `Short tok -> R.Case ("Short",
      (* "short" *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Long tok -> R.Case ("Long",
      (* "long" *) token env tok
    )
  | `Char tok -> R.Case ("Char",
      (* "char" *) token env tok
    )
  )

let map_octal_integer_literal (env : env) (tok : CST.octal_integer_literal) =
  (* octal_integer_literal *) token env tok

let map_string_fragment (env : env) (tok : CST.string_fragment) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_hex_integer_literal (env : env) (tok : CST.hex_integer_literal) =
  (* hex_integer_literal *) token env tok

let map_line_comment (env : env) (tok : CST.line_comment) =
  (* line_comment *) token env tok

let map_requires_modifier (env : env) (x : CST.requires_modifier) =
  (match x with
  | `Tran tok -> R.Case ("Tran",
      (* "transitive" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  )

let map_block_comment (env : env) (tok : CST.block_comment) =
  (* block_comment *) token env tok

let map_pat_98d585a (env : env) (tok : CST.pat_98d585a) =
  (* pattern "[^\"]+" *) token env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Open tok -> R.Case ("Open",
      (* "open" *) token env tok
    )
  | `Module tok -> R.Case ("Module",
      (* "module" *) token env tok
    )
  | `Record tok -> R.Case ("Record",
      (* "record" *) token env tok
    )
  )

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_floating_point_type (env : env) (x : CST.floating_point_type) =
  (match x with
  | `Float tok -> R.Case ("Float",
      (* "float" *) token env tok
    )
  | `Double tok -> R.Case ("Double",
      (* "double" *) token env tok
    )
  )

let map_break_statement (env : env) ((v1, v2, v3) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_escape_sequence_ (env : env) (x : CST.escape_sequence_) =
  (match x with
  | `Imm_tok_bslash_pat_36cdeeb x -> R.Case ("Imm_tok_bslash_pat_36cdeeb",
      map_imm_tok_bslash_pat_36cdeeb env x
    )
  | `Esc_seq tok -> R.Case ("Esc_seq",
      (* escape_sequence *) token env tok
    )
  )

let map_multiline_string_fragment (env : env) (x : CST.multiline_string_fragment) =
  (match x with
  | `Pat_98d585a x -> R.Case ("Pat_98d585a",
      map_pat_98d585a env x
    )
  | `Pat_9347a80_rep_pat_98d585a (v1, v2) -> R.Case ("Pat_9347a80_rep_pat_98d585a",
      let v1 = map_pat_9347a80 env v1 in
      let v2 =
        R.List (List.map (fun x ->
          map_pat_98d585a env x
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_id_0e59f50 (env : env) (x : CST.anon_choice_id_0e59f50) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
    )
  | `Choice_open x -> R.Case ("Choice_open",
      map_reserved_identifier env x
    )
  )

let rec map_name (env : env) (x : CST.name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
    )
  | `Choice_open x -> R.Case ("Choice_open",
      map_reserved_identifier env x
    )
  | `Scoped_id (v1, v2, v3) -> R.Case ("Scoped_id",
      let v1 = map_name env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_id_0e59f50 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_id_0e59f50 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_to_name_rep_COMMA_name_2956291 (env : env) ((v1, v2, v3) : CST.anon_to_name_rep_COMMA_name_2956291) =
  let v1 = (* "to" *) token env v1 in
  let v2 = map_name env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_name env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Str_lit_ (v1, v2, v3) -> R.Case ("Str_lit_",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_frag tok -> R.Case ("Str_frag",
              (* pattern "[^\"\\\\]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Mult_str_lit (v1, v2, v3) -> R.Case ("Mult_str_lit",
      let v1 = (* "\"\"\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Mult_str_frag x -> R.Case ("Mult_str_frag",
              map_multiline_string_fragment env x
            )
          | `Esc_seq_ x -> R.Case ("Esc_seq_",
              map_escape_sequence_ env x
            )
          )
        ) v2)
      in
      let v3 = (* "\"\"\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Deci_int_lit tok -> R.Case ("Deci_int_lit",
      (* decimal_integer_literal *) token env tok
    )
  | `Hex_int_lit tok -> R.Case ("Hex_int_lit",
      (* hex_integer_literal *) token env tok
    )
  | `Octal_int_lit tok -> R.Case ("Octal_int_lit",
      (* octal_integer_literal *) token env tok
    )
  | `Bin_int_lit tok -> R.Case ("Bin_int_lit",
      (* binary_integer_literal *) token env tok
    )
  | `Deci_floa_point_lit tok -> R.Case ("Deci_floa_point_lit",
      (* decimal_floating_point_literal *) token env tok
    )
  | `Hex_floa_point_lit tok -> R.Case ("Hex_floa_point_lit",
      (* hex_floating_point_literal *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `Char_lit tok -> R.Case ("Char_lit",
      (* character_literal *) token env tok
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  )

let map_module_directive (env : env) (x : CST.module_directive) =
  (match x with
  | `Requis_module_dire (v1, v2, v3, v4) -> R.Case ("Requis_module_dire",
      let v1 = (* "requires" *) token env v1 in
      let v2 = R.List (List.map (map_requires_modifier env) v2) in
      let v3 = map_name env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exports_module_dire (v1, v2, v3, v4) -> R.Case ("Exports_module_dire",
      let v1 = (* "exports" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_to_name_rep_COMMA_name_2956291 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Opens_module_dire (v1, v2, v3, v4) -> R.Case ("Opens_module_dire",
      let v1 = (* "opens" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_to_name_rep_COMMA_name_2956291 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Uses_module_dire (v1, v2, v3) -> R.Case ("Uses_module_dire",
      let v1 = (* "uses" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Provis_module_dire (v1, v2, v3, v4, v5, v6) -> R.Case ("Provis_module_dire",
      let v1 = (* "provides" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = (* "with" *) token env v3 in
      let v4 = map_name env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_name env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

let map_module_body (env : env) ((v1, v2, v3) : CST.module_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_module_directive env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_annotation (env : env) (x : CST.annotation) =
  (match x with
  | `Marker_anno (v1, v2) -> R.Case ("Marker_anno",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_name env v2 in
      R.Tuple [v1; v2]
    )
  | `Anno_ (v1, v2, v3) -> R.Case ("Anno_",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = map_annotation_argument_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Elem_value x -> R.Case ("Elem_value",
        map_element_value env x
      )
    | `Opt_elem_value_pair_rep_COMMA_elem_value_pair opt -> R.Case ("Opt_elem_value_pair_rep_COMMA_elem_value_pair",
        (match opt with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_element_value_pair env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_element_value_pair env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_annotation_type_body (env : env) ((v1, v2, v3) : CST.annotation_type_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Anno_type_elem_decl x -> R.Case ("Anno_type_elem_decl",
          map_annotation_type_element_declaration env x
        )
      | `Cst_decl x -> R.Case ("Cst_decl",
          map_constant_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Inte_decl x -> R.Case ("Inte_decl",
          map_interface_declaration env x
        )
      | `Enum_decl x -> R.Case ("Enum_decl",
          map_enum_declaration env x
        )
      | `Anno_type_decl x -> R.Case ("Anno_type_decl",
          map_annotation_type_declaration env x
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_annotation_type_declaration (env : env) ((v1, v2, v3, v4) : CST.annotation_type_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "@interface" *) token env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 = map_annotation_type_body env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_annotation_type_element_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.annotation_type_element_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 = (* "(" *) token env v4 in
  let v5 = (* ")" *) token env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_dimensions env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_default_value env x
      ))
    | None -> R.Option None)
  in
  let v8 = (* ";" *) token env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_anon_choice_formal_param_3e261ef (env : env) (x : CST.anon_choice_formal_param_3e261ef) =
  (match x with
  | `Formal_param x -> R.Case ("Formal_param",
      map_formal_parameter env x
    )
  | `Spread_param (v1, v2, v3, v4) -> R.Case ("Spread_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = (* "..." *) token env v3 in
      let v4 = map_variable_declarator env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_prim_exp_bbf4eda (env : env) (x : CST.anon_choice_prim_exp_bbf4eda) =
  (match x with
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Super tok -> R.Case ("Super",
      (* "super" *) token env tok
    )
  )

and map_anon_choice_type_205a2ac (env : env) (x : CST.anon_choice_type_205a2ac) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Wild (v1, v2, v3) -> R.Case ("Wild",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_wildcard_bounds env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_exp_rep_COMMA_exp_0bb260c env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_array_creation_expression (env : env) ((v1, v2, v3, v4) : CST.array_creation_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 = R.List (List.map (map_annotation env) v2) in
  let v3 = map_simple_type env v3 in
  let v4 =
    (match v4 with
    | `Rep1_dimens_expr_opt_dimens (v1, v2) -> R.Case ("Rep1_dimens_expr_opt_dimens",
        let v1 = R.List (List.map (map_dimensions_expr env) v1) in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_dimensions env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Dimens_array_init (v1, v2) -> R.Case ("Dimens_array_init",
        let v1 = map_dimensions env v1 in
        let v2 = map_array_initializer env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_array_initializer (env : env) ((v1, v2, v3, v4) : CST.array_initializer) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_variable_initializer env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_variable_initializer env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_assert_statement (env : env) (x : CST.assert_statement) =
  (match x with
  | `Assert_exp_SEMI (v1, v2, v3) -> R.Case ("Assert_exp_SEMI",
      let v1 = (* "assert" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Assert_exp_COLON_exp_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Assert_exp_COLON_exp_SEMI",
      let v1 = (* "assert" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_catch_formal_parameter env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_block env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_catch_formal_parameter (env : env) ((v1, v2, v3) : CST.catch_formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_catch_type env v2 in
  let v3 = map_variable_declarator_id env v3 in
  R.Tuple [v1; v2; v3]

and map_catch_type (env : env) ((v1, v2) : CST.catch_type) =
  let v1 = map_unannotated_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_class_body_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_class_body_declaration (env : env) (x : CST.class_body_declaration) =
  (match x with
  | `Field_decl (v1, v2, v3, v4) -> R.Case ("Field_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_list env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Comp_cons_decl (v1, v2, v3) -> R.Case ("Comp_cons_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v2
      in
      let v3 = map_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Anno_type_decl x -> R.Case ("Anno_type_decl",
      map_annotation_type_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Static_init (v1, v2) -> R.Case ("Static_init",
      let v1 = (* "static" *) token env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Cons_decl x -> R.Case ("Cons_decl",
      map_constructor_declaration env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.class_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "class" *) token env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_superclass env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_super_interfaces env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_permits env x
      ))
    | None -> R.Option None)
  in
  let v8 = map_class_body env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_class_literal (env : env) ((v1, v2, v3) : CST.class_literal) =
  let v1 = map_unannotated_type env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "class" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_constant_declaration (env : env) ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_explicit_constructor_invocation env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_statement env) v3) in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_declaration (env : env) ((v1, v2, v3, v4) : CST.constructor_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_constructor_declarator env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_throws env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_constructor_body env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_constructor_declarator (env : env) ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v2
  in
  let v3 = map_formal_parameters env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Module_decl (v1, v2, v3, v4, v5) -> R.Case ("Module_decl",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "open" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* "module" *) token env v3 in
      let v4 = map_name env v4 in
      let v5 = map_module_body env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Pack_decl (v1, v2, v3, v4) -> R.Case ("Pack_decl",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = (* "package" *) token env v2 in
      let v3 = map_name env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Import_decl (v1, v2, v3, v4, v5) -> R.Case ("Import_decl",
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "static" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "." *) token env v1 in
            let v2 = (* "*" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Anno_type_decl x -> R.Case ("Anno_type_decl",
      map_annotation_type_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  )

and map_default_value (env : env) ((v1, v2) : CST.default_value) =
  let v1 = (* "default" *) token env v1 in
  let v2 = map_element_value env v2 in
  R.Tuple [v1; v2]

and map_dimensions (env : env) (xs : CST.dimensions) =
  R.List (List.map (fun (v1, v2, v3) ->
    let v1 = R.List (List.map (map_annotation env) v1) in
    let v2 = (* "[" *) token env v2 in
    let v3 = (* "]" *) token env v3 in
    R.Tuple [v1; v2; v3]
  ) xs)

and map_dimensions_expr (env : env) ((v1, v2, v3, v4) : CST.dimensions_expr) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = (* "while" *) token env v3 in
  let v4 = map_parenthesized_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_element_value (env : env) (x : CST.element_value) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Elem_value_array_init (v1, v2, v3, v4) -> R.Case ("Elem_value_array_init",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_element_value env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_element_value env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Anno x -> R.Case ("Anno",
      map_annotation env x
    )
  )

and map_element_value_pair (env : env) ((v1, v2, v3) : CST.element_value_pair) =
  let v1 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v1
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_element_value env v3 in
  R.Tuple [v1; v2; v3]

and map_enhanced_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.enhanced_for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_unannotated_type env v4 in
  let v5 = map_variable_declarator_id env v5 in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_expression env v7 in
  let v8 = (* ")" *) token env v8 in
  let v9 = map_statement env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_enum_body (env : env) ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_enum_constant env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_enum_constant env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_enum_body_declarations env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_enum_body_declarations (env : env) ((v1, v2) : CST.enum_body_declarations) =
  let v1 = (* ";" *) token env v1 in
  let v2 =
    R.List (List.map (map_class_body_declaration env) v2)
  in
  R.Tuple [v1; v2]

and map_enum_constant (env : env) ((v1, v2, v3, v4) : CST.enum_constant) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_class_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "enum" *) token env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_super_interfaces env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_enum_body env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_explicit_constructor_invocation (env : env) ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    (match v1 with
    | `Opt_type_args_choice_this (v1, v2) -> R.Case ("Opt_type_args_choice_this",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `This tok -> R.Case ("This",
              (* "this" *) token env tok
            )
          | `Super tok -> R.Case ("Super",
              (* "super" *) token env tok
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Choice_prim_exp_DOT_opt_type_args_super (v1, v2, v3, v4) -> R.Case ("Choice_prim_exp_DOT_opt_type_args_super",
        let v1 =
          (match v1 with
          | `Prim_exp x -> R.Case ("Prim_exp",
              map_primary_expression env x
            )
          )
        in
        let v2 = (* "." *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* "super" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v2 = map_argument_list env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
          )
        | `Choice_open x -> R.Case ("Choice_open",
            map_reserved_identifier env x
          )
        | `Field_access x -> R.Case ("Field_access",
            map_field_access env x
          )
        | `Array_access x -> R.Case ("Array_access",
            map_array_access env x
          )
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ",
            (* ">>>=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Inst_exp (v1, v2, v3, v4, v5) -> R.Case ("Inst_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "instanceof" *) token env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "final" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_type_ env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_id_0e59f50 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Lambda_exp (v1, v2, v3) -> R.Case ("Lambda_exp",
      let v1 =
        (match v1 with
        | `Id tok -> R.Case ("Id",
            (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
          )
        | `Formal_params x -> R.Case ("Formal_params",
            map_formal_parameters env x
          )
        | `Infe_params x -> R.Case ("Infe_params",
            map_inferred_parameters env x
          )
        | `Choice_open x -> R.Case ("Choice_open",
            map_reserved_identifier env x
          )
        )
      in
      let v2 = (* "->" *) token env v2 in
      let v3 =
        (match v3 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Blk x -> R.Case ("Blk",
            map_block env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Cast_exp (v1, v2, v3, v4, v5) -> R.Case ("Cast_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "&" *) token env v1 in
          let v2 = map_type_ env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Switch_exp x -> R.Case ("Switch_exp",
      map_switch_expression env x
    )
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_list env v2 in
  R.Tuple [v1; v2]

and map_field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) =
  let v1 = map_anon_choice_prim_exp_bbf4eda env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "." *) token env v1 in
        let v2 = (* "super" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "." *) token env v3 in
  let v4 =
    (match v4 with
    | `Id tok -> R.Case ("Id",
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      )
    | `Choice_open x -> R.Case ("Choice_open",
        map_reserved_identifier env x
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Local_var_decl x -> R.Case ("Local_var_decl",
        map_local_variable_declaration env x
      )
    | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) -> R.Case ("Opt_exp_rep_COMMA_exp_SEMI",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_exp_rep_COMMA_exp_0bb260c env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* ";" *) token env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anon_exp_rep_COMMA_exp_0bb260c env x
      ))
    | None -> R.Option None)
  in
  let v7 = (* ")" *) token env v7 in
  let v8 = map_statement env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) -> R.Case ("Opt_modifs_unan_type_var_decl_id",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_receiver_parameter env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_formal_param_3e261ef env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_formal_param_3e261ef env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    )
  in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 = map_statement env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_interface_body (env : env) ((v1, v2, v3) : CST.interface_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Cst_decl x -> R.Case ("Cst_decl",
          map_constant_declaration env x
        )
      | `Enum_decl x -> R.Case ("Enum_decl",
          map_enum_declaration env x
        )
      | `Meth_decl x -> R.Case ("Meth_decl",
          map_method_declaration env x
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Inte_decl x -> R.Case ("Inte_decl",
          map_interface_declaration env x
        )
      | `Record_decl x -> R.Case ("Record_decl",
          map_record_declaration env x
        )
      | `Anno_type_decl x -> R.Case ("Anno_type_decl",
          map_annotation_type_declaration env x
        )
      | `SEMI tok -> R.Case ("SEMI",
          (* ";" *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.interface_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "interface" *) token env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_extends_interfaces env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_permits env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_interface_body env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_local_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_method_header env v2 in
  let v3 =
    (match v3 with
    | `Blk x -> R.Case ("Blk",
        map_block env x
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = map_anon_choice_id_0e59f50 env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_dimensions env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_method_header (env : env) ((v1, v2, v3, v4) : CST.method_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_type_parameters env v1 in
        let v2 = R.List (List.map (map_annotation env) v2) in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_method_declarator env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_throws env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_method_invocation (env : env) ((v1, v2) : CST.method_invocation) =
  let v1 =
    (match v1 with
    | `Choice_id x -> R.Case ("Choice_id",
        map_anon_choice_id_0e59f50 env x
      )
    | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id (v1, v2, v3, v4, v5) -> R.Case ("Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id",
        let v1 = map_anon_choice_prim_exp_bbf4eda env v1 in
        let v2 = (* "." *) token env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "super" *) token env v1 in
              let v2 = (* "." *) token env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v4 =
          (match v4 with
          | Some x -> R.Option (Some (
              map_type_arguments env x
            ))
          | None -> R.Option None)
        in
        let v5 = map_anon_choice_id_0e59f50 env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_method_reference (env : env) ((v1, v2, v3, v4) : CST.method_reference) =
  let v1 =
    (match v1 with
    | `Type x -> R.Case ("Type",
        map_type_ env x
      )
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    | `Super tok -> R.Case ("Super",
        (* "super" *) token env tok
      )
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `New tok -> R.Case ("New",
        (* "new" *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_modifiers (env : env) (xs : CST.modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Anno x -> R.Case ("Anno",
        map_annotation env x
      )
    | `Public tok -> R.Case ("Public",
        (* "public" *) token env tok
      )
    | `Prot tok -> R.Case ("Prot",
        (* "protected" *) token env tok
      )
    | `Priv tok -> R.Case ("Priv",
        (* "private" *) token env tok
      )
    | `Abst tok -> R.Case ("Abst",
        (* "abstract" *) token env tok
      )
    | `Static tok -> R.Case ("Static",
        (* "static" *) token env tok
      )
    | `Final tok -> R.Case ("Final",
        (* "final" *) token env tok
      )
    | `Stri tok -> R.Case ("Stri",
        (* "strictfp" *) token env tok
      )
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    | `Sync tok -> R.Case ("Sync",
        (* "synchronized" *) token env tok
      )
    | `Native tok -> R.Case ("Native",
        (* "native" *) token env tok
      )
    | `Tran tok -> R.Case ("Tran",
        (* "transient" *) token env tok
      )
    | `Vola tok -> R.Case ("Vola",
        (* "volatile" *) token env tok
      )
    | `Sealed tok -> R.Case ("Sealed",
        (* "sealed" *) token env tok
      )
    | `NonD tok -> R.Case ("NonD",
        (* "non-sealed" *) token env tok
      )
    )
  ) xs)

and map_object_creation_expression (env : env) (x : CST.object_creation_expression) =
  (match x with
  | `Unqu_obj_crea_exp x -> R.Case ("Unqu_obj_crea_exp",
      map_unqualified_object_creation_expression env x
    )
  | `Prim_exp_DOT_unqu_obj_crea_exp (v1, v2, v3) -> R.Case ("Prim_exp_DOT_unqu_obj_crea_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        map_unqualified_object_creation_expression env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_permits (env : env) ((v1, v2) : CST.permits) =
  let v1 = (* "permits" *) token env v1 in
  let v2 = map_type_list env v2 in
  R.Tuple [v1; v2]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_lit x -> R.Case ("Choice_lit",
      (match x with
      | `Lit x -> R.Case ("Lit",
          map_literal env x
        )
      | `Class_lit x -> R.Case ("Class_lit",
          map_class_literal env x
        )
      | `This tok -> R.Case ("This",
          (* "this" *) token env tok
        )
      | `Id tok -> R.Case ("Id",
          (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
        )
      | `Choice_open x -> R.Case ("Choice_open",
          map_reserved_identifier env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Obj_crea_exp x -> R.Case ("Obj_crea_exp",
          map_object_creation_expression env x
        )
      | `Field_access x -> R.Case ("Field_access",
          map_field_access env x
        )
      | `Array_access x -> R.Case ("Array_access",
          map_array_access env x
        )
      | `Meth_invo x -> R.Case ("Meth_invo",
          map_method_invocation env x
        )
      | `Meth_ref x -> R.Case ("Meth_ref",
          map_method_reference env x
        )
      | `Array_crea_exp x -> R.Case ("Array_crea_exp",
          map_array_creation_expression env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_receiver_parameter (env : env) ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 = map_unannotated_type env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v1
        in
        let v2 = (* "." *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "this" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_record_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.record_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "record" *) token env v2 in
  let v3 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_formal_parameters env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_super_interfaces env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_class_body env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_resource (env : env) (x : CST.resource) =
  (match x with
  | `Opt_modifs_unan_type_var_decl_id_EQ_exp (v1, v2, v3, v4, v5) -> R.Case ("Opt_modifs_unan_type_var_decl_id_EQ_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
    )
  | `Field_access x -> R.Case ("Field_access",
      map_field_access env x
    )
  )

and map_resource_specification (env : env) ((v1, v2, v3, v4, v5) : CST.resource_specification) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_resource env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_resource env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
      )
    | `Scoped_type_id x -> R.Case ("Scoped_type_id",
        map_scoped_type_identifier env x
      )
    | `Gene_type x -> R.Case ("Gene_type",
        map_generic_type env x
      )
    )
  in
  let v2 = (* "." *) token env v2 in
  let v3 = R.List (List.map (map_annotation env) v3) in
  let v4 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v4
  in
  R.Tuple [v1; v2; v3; v4]

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Void_type tok -> R.Case ("Void_type",
      (* "void" *) token env tok
    )
  | `Inte_type x -> R.Case ("Inte_type",
      map_integral_type env x
    )
  | `Floa_point_type x -> R.Case ("Floa_point_type",
      map_floating_point_type env x
    )
  | `Bool_type tok -> R.Case ("Bool_type",
      (* "boolean" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env tok
    )
  | `Scoped_type_id x -> R.Case ("Scoped_type_id",
      map_scoped_type_identifier env x
    )
  | `Gene_type x -> R.Case ("Gene_type",
      map_generic_type env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_decl x -> R.Case ("Choice_decl",
      (match x with
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Exp_stmt x -> R.Case ("Exp_stmt",
          map_expression_statement env x
        )
      | `Labe_stmt x -> R.Case ("Labe_stmt",
          map_labeled_statement env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `While_stmt x -> R.Case ("While_stmt",
          map_while_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `Enha_for_stmt x -> R.Case ("Enha_for_stmt",
          map_enhanced_for_statement env x
        )
      | `Blk x -> R.Case ("Blk",
          map_block env x
        )
      | `SEMI tok -> R.Case ("SEMI",
          (* ";" *) token env tok
        )
      | `Assert_stmt x -> R.Case ("Assert_stmt",
          map_assert_statement env x
        )
      | `Do_stmt x -> R.Case ("Do_stmt",
          map_do_statement env x
        )
      | `Brk_stmt x -> R.Case ("Brk_stmt",
          map_break_statement env x
        )
      | `Cont_stmt x -> R.Case ("Cont_stmt",
          map_continue_statement env x
        )
      | `Ret_stmt x -> R.Case ("Ret_stmt",
          map_return_statement env x
        )
      | `Yield_stmt x -> R.Case ("Yield_stmt",
          map_yield_statement env x
        )
      | `Switch_exp x -> R.Case ("Switch_exp",
          map_switch_expression env x
        )
      | `Sync_stmt x -> R.Case ("Sync_stmt",
          map_synchronized_statement env x
        )
      | `Local_var_decl x -> R.Case ("Local_var_decl",
          map_local_variable_declaration env x
        )
      | `Throw_stmt x -> R.Case ("Throw_stmt",
          map_throw_statement env x
        )
      | `Try_stmt x -> R.Case ("Try_stmt",
          map_try_statement env x
        )
      | `Try_with_resous_stmt x -> R.Case ("Try_with_resous_stmt",
          map_try_with_resources_statement env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_super_interfaces (env : env) ((v1, v2) : CST.super_interfaces) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_list env v2 in
  R.Tuple [v1; v2]

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | `Rep_switch_blk_stmt_group xs -> R.Case ("Rep_switch_blk_stmt_group",
        R.List (List.map (map_switch_block_statement_group env) xs)
      )
    | `Rep_switch_rule xs -> R.Case ("Rep_switch_rule",
        R.List (List.map (map_switch_rule env) xs)
      )
    )
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_switch_block_statement_group (env : env) ((v1, v2) : CST.switch_block_statement_group) =
  let v1 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_switch_label env v1 in
      let v2 = (* ":" *) token env v2 in
      R.Tuple [v1; v2]
    ) v1)
  in
  let v2 = R.List (List.map (map_statement env) v2) in
  R.Tuple [v1; v2]

and map_switch_expression (env : env) ((v1, v2, v3) : CST.switch_expression) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_switch_block env v3 in
  R.Tuple [v1; v2; v3]

and map_switch_label (env : env) (x : CST.switch_label) =
  (match x with
  | `Case_exp_rep_COMMA_exp (v1, v2, v3) -> R.Case ("Case_exp_rep_COMMA_exp",
      let v1 = (* "case" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Defa tok -> R.Case ("Defa",
      (* "default" *) token env tok
    )
  )

and map_switch_rule (env : env) ((v1, v2, v3) : CST.switch_rule) =
  let v1 = map_switch_label env v1 in
  let v2 = (* "->" *) token env v2 in
  let v3 =
    (match v3 with
    | `Exp_stmt x -> R.Case ("Exp_stmt",
        map_expression_statement env x
      )
    | `Throw_stmt x -> R.Case ("Throw_stmt",
        map_throw_statement env x
      )
    | `Blk x -> R.Case ("Blk",
        map_block env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_synchronized_statement (env : env) ((v1, v2, v3) : CST.synchronized_statement) =
  let v1 = (* "synchronized" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_block env v3 in
  R.Tuple [v1; v2; v3]

and map_throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) =
  let v1 = (* "throw" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_throws (env : env) ((v1, v2, v3) : CST.throws) =
  let v1 = (* "throws" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_try_statement (env : env) ((v1, v2, v3) : CST.try_statement) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_block env v2 in
  let v3 =
    (match v3 with
    | `Rep1_catch_clause xs -> R.Case ("Rep1_catch_clause",
        R.List (List.map (map_catch_clause env) xs)
      )
    | `Rep_catch_clause_fina_clause (v1, v2) -> R.Case ("Rep_catch_clause_fina_clause",
        let v1 = R.List (List.map (map_catch_clause env) v1) in
        let v2 = map_finally_clause env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_try_with_resources_statement (env : env) ((v1, v2, v3, v4, v5) : CST.try_with_resources_statement) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_resource_specification env v2 in
  let v3 = map_block env v3 in
  let v4 = R.List (List.map (map_catch_clause env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_finally_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Unan_type x -> R.Case ("Unan_type",
      map_unannotated_type env x
    )
  | `Anno_type (v1, v2) -> R.Case ("Anno_type",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = map_unannotated_type env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_205a2ac env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_205a2ac env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_type_bound (env : env) ((v1, v2, v3) : CST.type_bound) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "&" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_bound env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_unannotated_type (env : env) (x : CST.unannotated_type) =
  (match x with
  | `Choice_void_type x -> R.Case ("Choice_void_type",
      map_simple_type env x
    )
  | `Array_type (v1, v2) -> R.Case ("Array_type",
      let v1 = map_unannotated_type env v1 in
      let v2 = map_dimensions env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `PLUS_exp (v1, v2) -> R.Case ("PLUS_exp",
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_arguments env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_simple_type env v3 in
  let v4 = map_argument_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_class_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) -> R.Case ("Exp_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = (* "++" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_DASHDASH (v1, v2) -> R.Case ("Exp_DASHDASH",
      let v1 = map_expression env v1 in
      let v2 = (* "--" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUSPLUS_exp (v1, v2) -> R.Case ("PLUSPLUS_exp",
      let v1 = (* "++" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASHDASH_exp (v1, v2) -> R.Case ("DASHDASH_exp",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) =
  let v1 = map_variable_declarator_id env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_variable_initializer env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_variable_declarator_id (env : env) ((v1, v2) : CST.variable_declarator_id) =
  let v1 = map_anon_choice_id_0e59f50 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_dimensions env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list) =
  let v1 = map_variable_declarator env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_variable_initializer (env : env) (x : CST.variable_initializer) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Array_init x -> R.Case ("Array_init",
      map_array_initializer env x
    )
  )

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_wildcard_bounds (env : env) (x : CST.wildcard_bounds) =
  (match x with
  | `Extends_type x -> R.Case ("Extends_type",
      map_superclass env x
    )
  | `Super_type (v1, v2) -> R.Case ("Super_type",
      let v1 = (* "super" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_yield_statement (env : env) ((v1, v2, v3) : CST.yield_statement) =
  let v1 = (* "yield" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_program (env : env) (x : CST.program) =
  (match x with
  | `Rep_stmt xs -> R.Case ("Rep_stmt",
      R.List (List.map (map_statement env) xs)
    )
  | `Cons_decl x -> R.Case ("Cons_decl",
      map_constructor_declaration env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_string
  |> print_string
