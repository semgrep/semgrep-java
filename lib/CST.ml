(* Generated by ocaml-tree-sitter. *)
(*
   java grammar

   entrypoint: program
*)

open! Sexplib.Conv
open Tree_sitter_run

type floating_point_type = [
    `Float of Token.t (* "float" *)
  | `Double of Token.t (* "double" *)
]
[@@deriving sexp_of]

type line_comment = Token.t
[@@deriving sexp_of]

type binary_integer_literal = Token.t
[@@deriving sexp_of]

type decimal_floating_point_literal = Token.t
[@@deriving sexp_of]

type integral_type = [
    `Byte of Token.t (* "byte" *)
  | `Short of Token.t (* "short" *)
  | `Int of Token.t (* "int" *)
  | `Long of Token.t (* "long" *)
  | `Char of Token.t (* "char" *)
]
[@@deriving sexp_of]

type identifier = Token.t (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *)
[@@deriving sexp_of]

type line_comment_explicit = unit (* blank *)
[@@deriving sexp_of]

type character_literal = Token.t
[@@deriving sexp_of]

type decimal_integer_literal = Token.t
[@@deriving sexp_of]

type hex_floating_point_literal = Token.t
[@@deriving sexp_of]

type block_comment_explicit = unit (* blank *)
[@@deriving sexp_of]

type block_comment = Token.t
[@@deriving sexp_of]

type octal_integer_literal = Token.t
[@@deriving sexp_of]

type requires_modifier = [
    `Tran of Token.t (* "transitive" *)
  | `Static of Token.t (* "static" *)
]
[@@deriving sexp_of]

type string_literal = Token.t
[@@deriving sexp_of]

type reserved_identifier = [
    `Open of Token.t (* "open" *)
  | `Module of Token.t (* "module" *)
]
[@@deriving sexp_of]

type text_block = Token.t
[@@deriving sexp_of]

type hex_integer_literal = Token.t
[@@deriving sexp_of]

type break_statement = (
    Token.t (* "break" *)
  * identifier (*tok*) option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type inferred_parameters = (
    Token.t (* "(" *)
  * identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type continue_statement = (
    Token.t (* "continue" *)
  * identifier (*tok*) option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type name = [
    `Id of identifier (*tok*)
  | `Choice_open of reserved_identifier
  | `Scoped_id of (name * Token.t (* "." *) * identifier (*tok*))
]
[@@deriving sexp_of]

type anon_choice_id_0e59f50 = [
    `Id of identifier (*tok*)
  | `Choice_open of reserved_identifier
]
[@@deriving sexp_of]

type literal = [
    `Deci_int_lit of decimal_integer_literal (*tok*)
  | `Hex_int_lit of hex_integer_literal (*tok*)
  | `Octal_int_lit of octal_integer_literal (*tok*)
  | `Bin_int_lit of binary_integer_literal (*tok*)
  | `Deci_floa_point_lit of decimal_floating_point_literal (*tok*)
  | `Hex_floa_point_lit of hex_floating_point_literal (*tok*)
  | `True of Token.t (* "true" *)
  | `False of Token.t (* "false" *)
  | `Char_lit of character_literal (*tok*)
  | `Str_lit of string_literal (*tok*)
  | `Text_blk of text_block (*tok*)
  | `Null_lit of Token.t (* "null" *)
]
[@@deriving sexp_of]

type anon_to_name_rep_COMMA_name_2956291 = (
    Token.t (* "to" *)
  * name
  * (Token.t (* "," *) * name) list (* zero or more *)
)
[@@deriving sexp_of]

type module_directive = [
    `Requis_module_dire of (
        Token.t (* "requires" *)
      * requires_modifier list (* zero or more *)
      * name
      * Token.t (* ";" *)
    )
  | `Exports_module_dire of (
        Token.t (* "exports" *)
      * name
      * anon_to_name_rep_COMMA_name_2956291 option
      * Token.t (* ";" *)
    )
  | `Opens_module_dire of (
        Token.t (* "opens" *)
      * name
      * anon_to_name_rep_COMMA_name_2956291 option
      * Token.t (* ";" *)
    )
  | `Uses_module_dire of (Token.t (* "uses" *) * name * Token.t (* ";" *))
  | `Provis_module_dire of (
        Token.t (* "provides" *)
      * name
      * Token.t (* "with" *)
      * name
      * (Token.t (* "," *) * name) list (* zero or more *)
      * Token.t (* ";" *)
    )
]
[@@deriving sexp_of]

type module_body = (
    Token.t (* "{" *)
  * module_directive list (* zero or more *)
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type annotation = [
    `Marker_anno of (Token.t (* "@" *) * name)
  | `Anno_ of (Token.t (* "@" *) * name * annotation_argument_list)
]

and annotation_argument_list = (
    Token.t (* "(" *)
  * [
        `Elem_value of element_value
      | `Opt_elem_value_pair_rep_COMMA_elem_value_pair of
          (
              element_value_pair
            * (Token.t (* "," *) * element_value_pair)
                list (* zero or more *)
          )
            option
    ]
  * Token.t (* ")" *)
)

and annotation_type_body = (
    Token.t (* "{" *)
  * [
        `Anno_type_elem_decl of annotation_type_element_declaration
      | `Cst_decl of constant_declaration
      | `Class_decl of class_declaration
      | `Inte_decl of interface_declaration
      | `Anno_type_decl of annotation_type_declaration
    ]
      list (* zero or more *)
  * Token.t (* "}" *)
)

and annotation_type_declaration = (
    modifiers option
  * Token.t (* "@interface" *)
  * identifier (*tok*)
  * annotation_type_body
)

and annotation_type_element_declaration = (
    modifiers option
  * unannotated_type
  * identifier (*tok*)
  * Token.t (* "(" *)
  * Token.t (* ")" *)
  * dimensions option
  * default_value option
  * Token.t (* ";" *)
)

and anon_choice_formal_param_3e261ef = [
    `Formal_param of formal_parameter
  | `Spread_param of (
        modifiers option
      * unannotated_type
      * Token.t (* "..." *)
      * variable_declarator
    )
]

and anon_choice_prim_exp_bbf4eda = [
    `Prim_exp of primary_expression
  | `Super of Token.t (* "super" *)
]

and anon_choice_type_205a2ac = [
    `Type of type_
  | `Wild of (
        annotation list (* zero or more *)
      * Token.t (* "?" *)
      * wildcard_bounds option
    )
]

and anon_exp_rep_COMMA_exp_0bb260c = (
    expression
  * (Token.t (* "," *) * expression) list (* zero or more *)
)

and argument_list = (
    Token.t (* "(" *)
  * anon_exp_rep_COMMA_exp_0bb260c option
  * Token.t (* ")" *)
)

and array_access = (
    primary_expression * Token.t (* "[" *) * expression * Token.t (* "]" *)
)

and array_creation_expression = (
    Token.t (* "new" *)
  * simple_type
  * [
        `Rep1_dimens_expr_opt_dimens of (
            dimensions_expr list (* one or more *)
          * dimensions option
        )
      | `Dimens_array_init of (dimensions * array_initializer)
    ]
)

and array_initializer = (
    Token.t (* "{" *)
  * (
        variable_initializer
      * (Token.t (* "," *) * variable_initializer) list (* zero or more *)
    )
      option
  * Token.t (* "," *) option
  * Token.t (* "}" *)
)

and assert_statement = [
    `Assert_exp_SEMI of (
        Token.t (* "assert" *) * expression * Token.t (* ";" *)
    )
  | `Assert_exp_COLON_exp_SEMI of (
        Token.t (* "assert" *) * expression * Token.t (* ":" *) * expression
      * Token.t (* ";" *)
    )
]

and binary_expression = [
    `Exp_GT_exp of (expression * Token.t (* ">" *) * expression)
  | `Exp_LT_exp of (expression * Token.t (* "<" *) * expression)
  | `Exp_GTEQ_exp of (expression * Token.t (* ">=" *) * expression)
  | `Exp_LTEQ_exp of (expression * Token.t (* "<=" *) * expression)
  | `Exp_EQEQ_exp of (expression * Token.t (* "==" *) * expression)
  | `Exp_BANGEQ_exp of (expression * Token.t (* "!=" *) * expression)
  | `Exp_AMPAMP_exp of (expression * Token.t (* "&&" *) * expression)
  | `Exp_BARBAR_exp of (expression * Token.t (* "||" *) * expression)
  | `Exp_PLUS_exp of (expression * Token.t (* "+" *) * expression)
  | `Exp_DASH_exp of (expression * Token.t (* "-" *) * expression)
  | `Exp_STAR_exp of (expression * Token.t (* "*" *) * expression)
  | `Exp_SLASH_exp of (expression * Token.t (* "/" *) * expression)
  | `Exp_AMP_exp of (expression * Token.t (* "&" *) * expression)
  | `Exp_BAR_exp of (expression * Token.t (* "|" *) * expression)
  | `Exp_HAT_exp of (expression * Token.t (* "^" *) * expression)
  | `Exp_PERC_exp of (expression * Token.t (* "%" *) * expression)
  | `Exp_LTLT_exp of (expression * Token.t (* "<<" *) * expression)
  | `Exp_GTGT_exp of (expression * Token.t (* ">>" *) * expression)
  | `Exp_GTGTGT_exp of (expression * Token.t (* ">>>" *) * expression)
]

and block = (
    Token.t (* "{" *)
  * statement list (* zero or more *)
  * Token.t (* "}" *)
)

and catch_clause = (
    Token.t (* "catch" *) * Token.t (* "(" *) * catch_formal_parameter
  * Token.t (* ")" *) * block
)

and catch_formal_parameter = (
    modifiers option
  * catch_type
  * variable_declarator_id
)

and catch_type = (
    unannotated_type
  * (Token.t (* "|" *) * unannotated_type) list (* zero or more *)
)

and class_body = (
    Token.t (* "{" *)
  * class_body_declaration list (* zero or more *)
  * Token.t (* "}" *)
)

and class_body_declaration = [
    `Field_decl of (
        modifiers option
      * unannotated_type
      * variable_declarator_list
      * Token.t (* ";" *)
    )
  | `Record_decl of (
        modifiers option
      * Token.t (* "record" *)
      * identifier (*tok*)
      * formal_parameters
      * class_body
    )
  | `Meth_decl of method_declaration
  | `Class_decl of class_declaration
  | `Inte_decl of interface_declaration
  | `Anno_type_decl of annotation_type_declaration
  | `Enum_decl of enum_declaration
  | `Blk of block
  | `Static_init of (Token.t (* "static" *) * block)
  | `Cons_decl of constructor_declaration
  | `SEMI of Token.t (* ";" *)
]

and class_declaration = (
    modifiers option
  * Token.t (* "class" *)
  * identifier (*tok*)
  * type_parameters option
  * superclass option
  * super_interfaces option
  * permits option
  * class_body
)

and class_literal = (
    unannotated_type * Token.t (* "." *) * Token.t (* "class" *)
)

and constant_declaration = (
    modifiers option
  * unannotated_type
  * variable_declarator_list
  * Token.t (* ";" *)
)

and constructor_body = (
    Token.t (* "{" *)
  * explicit_constructor_invocation option
  * statement list (* zero or more *)
  * Token.t (* "}" *)
)

and constructor_declaration = (
    modifiers option
  * constructor_declarator
  * throws option
  * constructor_body
)

and constructor_declarator = (
    type_parameters option
  * identifier (*tok*)
  * formal_parameters
)

and declaration = [
    `Module_decl of (
        annotation list (* zero or more *)
      * Token.t (* "open" *) option
      * Token.t (* "module" *)
      * name
      * module_body
    )
  | `Pack_decl of (
        annotation list (* zero or more *)
      * Token.t (* "package" *)
      * name
      * Token.t (* ";" *)
    )
  | `Import_decl of (
        Token.t (* "import" *)
      * Token.t (* "static" *) option
      * name
      * (Token.t (* "." *) * Token.t (* "*" *)) option
      * Token.t (* ";" *)
    )
  | `Class_decl of class_declaration
  | `Inte_decl of interface_declaration
  | `Anno_type_decl of annotation_type_declaration
  | `Enum_decl of enum_declaration
]

and default_value = (Token.t (* "default" *) * element_value)

and dimensions =
  (
      annotation list (* zero or more *)
    * Token.t (* "[" *)
    * Token.t (* "]" *)
  )
    list (* one or more *)

and dimensions_expr = (
    annotation list (* zero or more *)
  * Token.t (* "[" *)
  * expression
  * Token.t (* "]" *)
)

and do_statement = (
    Token.t (* "do" *) * statement * Token.t (* "while" *)
  * parenthesized_expression * Token.t (* ";" *)
)

and element_value = [
    `Exp of expression
  | `Elem_value_array_init of (
        Token.t (* "{" *)
      * (
            element_value
          * (Token.t (* "," *) * element_value) list (* zero or more *)
        )
          option
      * Token.t (* "," *) option
      * Token.t (* "}" *)
    )
  | `Anno of annotation
]

and element_value_pair = (
    identifier (*tok*) * Token.t (* "=" *) * element_value
)

and enhanced_for_statement = (
    Token.t (* "for" *)
  * Token.t (* "(" *)
  * modifiers option
  * unannotated_type
  * variable_declarator_id
  * Token.t (* ":" *)
  * expression
  * Token.t (* ")" *)
  * statement
)

and enum_body = (
    Token.t (* "{" *)
  * (
        enum_constant
      * (Token.t (* "," *) * enum_constant) list (* zero or more *)
    )
      option
  * Token.t (* "," *) option
  * enum_body_declarations option
  * Token.t (* "}" *)
)

and enum_body_declarations = (
    Token.t (* ";" *)
  * class_body_declaration list (* zero or more *)
)

and enum_constant = (
    modifiers option
  * identifier (*tok*)
  * argument_list option
  * class_body option
)

and enum_declaration = (
    modifiers option
  * Token.t (* "enum" *)
  * identifier (*tok*)
  * super_interfaces option
  * enum_body
)

and explicit_constructor_invocation = (
    [
        `Opt_type_args_choice_this of (
            type_arguments option
          * [
                `This of Token.t (* "this" *)
              | `Super of Token.t (* "super" *)
            ]
        )
      | `Choice_prim_exp_DOT_opt_type_args_super of (
            [ `Prim_exp of primary_expression ]
          * Token.t (* "." *)
          * type_arguments option
          * Token.t (* "super" *)
        )
    ]
  * argument_list
  * Token.t (* ";" *)
)

and expression = [
    `Assign_exp of (
        [
            `Id of identifier (*tok*)
          | `Choice_open of reserved_identifier
          | `Field_access of field_access
          | `Array_access of array_access
        ]
      * [
            `EQ of Token.t (* "=" *)
          | `PLUSEQ of Token.t (* "+=" *)
          | `DASHEQ of Token.t (* "-=" *)
          | `STAREQ of Token.t (* "*=" *)
          | `SLASHEQ of Token.t (* "/=" *)
          | `AMPEQ of Token.t (* "&=" *)
          | `BAREQ of Token.t (* "|=" *)
          | `HATEQ of Token.t (* "^=" *)
          | `PERCEQ of Token.t (* "%=" *)
          | `LTLTEQ of Token.t (* "<<=" *)
          | `GTGTEQ of Token.t (* ">>=" *)
          | `GTGTGTEQ of Token.t (* ">>>=" *)
        ]
      * expression
    )
  | `Bin_exp of binary_expression
  | `Inst_exp of (expression * Token.t (* "instanceof" *) * type_)
  | `Lambda_exp of (
        [
            `Id of identifier (*tok*)
          | `Formal_params of formal_parameters
          | `Infe_params of inferred_parameters
        ]
      * Token.t (* "->" *)
      * [ `Exp of expression | `Blk of block ]
    )
  | `Tern_exp of (
        expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
      * expression
    )
  | `Update_exp of update_expression
  | `Prim_exp of primary_expression
  | `Un_exp of unary_expression
  | `Cast_exp of (
        Token.t (* "(" *)
      * type_
      * (Token.t (* "&" *) * type_) list (* zero or more *)
      * Token.t (* ")" *)
      * expression
    )
  | `Switch_exp of switch_expression
]

and expression_statement = (expression * Token.t (* ";" *))

and extends_interfaces = (Token.t (* "extends" *) * type_list)

and field_access = (
    anon_choice_prim_exp_bbf4eda
  * (Token.t (* "." *) * Token.t (* "super" *)) option
  * Token.t (* "." *)
  * [
        `Id of identifier (*tok*)
      | `Choice_open of reserved_identifier
      | `This of Token.t (* "this" *)
    ]
)

and finally_clause = (Token.t (* "finally" *) * block)

and for_statement = (
    Token.t (* "for" *)
  * Token.t (* "(" *)
  * [
        `Local_var_decl of local_variable_declaration
      | `Opt_exp_rep_COMMA_exp_SEMI of (
            anon_exp_rep_COMMA_exp_0bb260c option
          * Token.t (* ";" *)
        )
    ]
  * expression option
  * Token.t (* ";" *)
  * anon_exp_rep_COMMA_exp_0bb260c option
  * Token.t (* ")" *)
  * statement
)

and formal_parameter = [
    `Opt_modifs_unan_type_var_decl_id of (
        modifiers option
      * unannotated_type
      * variable_declarator_id
    )
  | `Semg_ellips of Token.t (* "..." *)
]

and formal_parameters = (
    Token.t (* "(" *)
  * receiver_parameter option
  * (
        anon_choice_formal_param_3e261ef
      * (Token.t (* "," *) * anon_choice_formal_param_3e261ef)
          list (* zero or more *)
    )
      option
  * Token.t (* ")" *)
)

and generic_type = (
    [ `Id of identifier (*tok*) | `Scoped_type_id of scoped_type_identifier ]
  * type_arguments
)

and if_statement = (
    Token.t (* "if" *)
  * parenthesized_expression
  * statement
  * (Token.t (* "else" *) * statement) option
)

and interface_body = (
    Token.t (* "{" *)
  * [
        `Cst_decl of constant_declaration
      | `Enum_decl of enum_declaration
      | `Meth_decl of method_declaration
      | `Class_decl of class_declaration
      | `Inte_decl of interface_declaration
      | `Anno_type_decl of annotation_type_declaration
      | `SEMI of Token.t (* ";" *)
    ]
      list (* zero or more *)
  * Token.t (* "}" *)
)

and interface_declaration = (
    modifiers option
  * Token.t (* "interface" *)
  * identifier (*tok*)
  * type_parameters option
  * extends_interfaces option
  * permits option
  * interface_body
)

and labeled_statement = (identifier (*tok*) * Token.t (* ":" *) * statement)

and local_variable_declaration = (
    modifiers option
  * unannotated_type
  * variable_declarator_list
  * Token.t (* ";" *)
)

and method_declaration = (
    modifiers option
  * method_header
  * [ `Blk of block | `SEMI of Token.t (* ";" *) ]
)

and method_declarator = (
    anon_choice_id_0e59f50
  * formal_parameters
  * dimensions option
)

and method_header = (
    (type_parameters * annotation list (* zero or more *)) option
  * unannotated_type
  * method_declarator
  * throws option
)

and method_invocation = (
    [
        `Choice_id of anon_choice_id_0e59f50
      | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id of (
            anon_choice_prim_exp_bbf4eda
          * Token.t (* "." *)
          * (Token.t (* "super" *) * Token.t (* "." *)) option
          * type_arguments option
          * anon_choice_id_0e59f50
        )
    ]
  * argument_list
)

and method_reference = (
    [
        `Type of type_
      | `Prim_exp of primary_expression
      | `Super of Token.t (* "super" *)
    ]
  * Token.t (* "::" *)
  * type_arguments option
  * [ `New of Token.t (* "new" *) | `Id of identifier (*tok*) ]
)

and modifiers =
  [
      `Anno of annotation
    | `Public of Token.t (* "public" *)
    | `Prot of Token.t (* "protected" *)
    | `Priv of Token.t (* "private" *)
    | `Abst of Token.t (* "abstract" *)
    | `Static of Token.t (* "static" *)
    | `Final of Token.t (* "final" *)
    | `Stri of Token.t (* "strictfp" *)
    | `Defa of Token.t (* "default" *)
    | `Sync of Token.t (* "synchronized" *)
    | `Native of Token.t (* "native" *)
    | `Tran of Token.t (* "transient" *)
    | `Vola of Token.t (* "volatile" *)
    | `Sealed of Token.t (* "sealed" *)
    | `NonD of Token.t (* "non-sealed" *)
  ]
    list (* one or more *)

and object_creation_expression = [
    `Unqu_obj_crea_exp of unqualified_object_creation_expression
  | `Prim_exp_DOT_unqu_obj_crea_exp of (
        primary_expression * Token.t (* "." *)
      * unqualified_object_creation_expression
    )
]

and parenthesized_expression = (
    Token.t (* "(" *) * expression * Token.t (* ")" *)
)

and permits = (Token.t (* "permits" *) * type_list)

and primary_expression = [
    `Choice_lit of [
        `Lit of literal
      | `Class_lit of class_literal
      | `This of Token.t (* "this" *)
      | `Id of identifier (*tok*)
      | `Choice_open of reserved_identifier
      | `Paren_exp of parenthesized_expression
      | `Obj_crea_exp of object_creation_expression
      | `Field_access of field_access
      | `Array_access of array_access
      | `Meth_invo of method_invocation
      | `Meth_ref of method_reference
      | `Array_crea_exp of array_creation_expression
    ]
  | `Semg_ellips of Token.t (* "..." *)
]

and receiver_parameter = (
    annotation list (* zero or more *)
  * unannotated_type
  * (identifier (*tok*) * Token.t (* "." *)) option
  * Token.t (* "this" *)
)

and resource = [
    `Opt_modifs_unan_type_var_decl_id_EQ_exp of (
        modifiers option
      * unannotated_type
      * variable_declarator_id
      * Token.t (* "=" *)
      * expression
    )
  | `Id of identifier (*tok*)
  | `Field_access of field_access
]

and resource_specification = (
    Token.t (* "(" *)
  * resource
  * (Token.t (* ";" *) * resource) list (* zero or more *)
  * Token.t (* ";" *) option
  * Token.t (* ")" *)
)

and return_statement = (
    Token.t (* "return" *)
  * expression option
  * Token.t (* ";" *)
)

and scoped_type_identifier = (
    [
        `Id of identifier (*tok*)
      | `Scoped_type_id of scoped_type_identifier
      | `Gene_type of generic_type
    ]
  * Token.t (* "." *)
  * annotation list (* zero or more *)
  * identifier (*tok*)
)

and simple_type = [
    `Void_type of Token.t (* "void" *)
  | `Inte_type of integral_type
  | `Floa_point_type of floating_point_type
  | `Bool_type of Token.t (* "boolean" *)
  | `Id of identifier (*tok*)
  | `Scoped_type_id of scoped_type_identifier
  | `Gene_type of generic_type
]

and statement = [
    `Choice_decl of [
        `Decl of declaration
      | `Exp_stmt of expression_statement
      | `Labe_stmt of labeled_statement
      | `If_stmt of if_statement
      | `While_stmt of while_statement
      | `For_stmt of for_statement
      | `Enha_for_stmt of enhanced_for_statement
      | `Blk of block
      | `SEMI of Token.t (* ";" *)
      | `Assert_stmt of assert_statement
      | `Do_stmt of do_statement
      | `Brk_stmt of break_statement
      | `Cont_stmt of continue_statement
      | `Ret_stmt of return_statement
      | `Yield_stmt of yield_statement
      | `Switch_exp of switch_expression
      | `Sync_stmt of synchronized_statement
      | `Local_var_decl of local_variable_declaration
      | `Throw_stmt of throw_statement
      | `Try_stmt of try_statement
      | `Try_with_resous_stmt of try_with_resources_statement
    ]
  | `Semg_ellips of Token.t (* "..." *)
]

and super_interfaces = (Token.t (* "implements" *) * type_list)

and superclass = (Token.t (* "extends" *) * type_)

and switch_block = (
    Token.t (* "{" *)
  * [
        `Rep_switch_blk_stmt_group of
          switch_block_statement_group list (* zero or more *)
      | `Rep_switch_rule of switch_rule list (* zero or more *)
    ]
  * Token.t (* "}" *)
)

and switch_block_statement_group = (
    (switch_label * Token.t (* ":" *)) list (* one or more *)
  * statement list (* zero or more *)
)

and switch_expression = (
    Token.t (* "switch" *) * parenthesized_expression * switch_block
)

and switch_label = [
    `Case_exp_rep_COMMA_exp of (
        Token.t (* "case" *)
      * expression
      * (Token.t (* "," *) * expression) list (* zero or more *)
    )
  | `Defa of Token.t (* "default" *)
]

and switch_rule = (
    switch_label
  * Token.t (* "->" *)
  * [
        `Exp_stmt of expression_statement
      | `Throw_stmt of throw_statement
      | `Blk of block
    ]
)

and synchronized_statement = (
    Token.t (* "synchronized" *) * parenthesized_expression * block
)

and throw_statement = (
    Token.t (* "throw" *) * expression * Token.t (* ";" *)
)

and throws = (
    Token.t (* "throws" *)
  * type_
  * (Token.t (* "," *) * type_) list (* zero or more *)
)

and try_statement = (
    Token.t (* "try" *)
  * block
  * [
        `Rep1_catch_clause of catch_clause list (* one or more *)
      | `Rep_catch_clause_fina_clause of (
            catch_clause list (* zero or more *)
          * finally_clause
        )
    ]
)

and try_with_resources_statement = (
    Token.t (* "try" *)
  * resource_specification
  * block
  * catch_clause list (* zero or more *)
  * finally_clause option
)

and type_ = [
    `Unan_type of unannotated_type
  | `Anno_type of (annotation list (* one or more *) * unannotated_type)
]

and type_arguments = (
    Token.t (* "<" *)
  * (
        anon_choice_type_205a2ac
      * (Token.t (* "," *) * anon_choice_type_205a2ac)
          list (* zero or more *)
    )
      option
  * Token.t (* ">" *)
)

and type_bound = (
    Token.t (* "extends" *)
  * type_
  * (Token.t (* "&" *) * type_) list (* zero or more *)
)

and type_list = (type_ * (Token.t (* "," *) * type_) list (* zero or more *))

and type_parameter = (
    annotation list (* zero or more *)
  * identifier (*tok*)
  * type_bound option
)

and type_parameters = (
    Token.t (* "<" *)
  * type_parameter
  * (Token.t (* "," *) * type_parameter) list (* zero or more *)
  * Token.t (* ">" *)
)

and unannotated_type = [
    `Choice_void_type of simple_type
  | `Array_type of (unannotated_type * dimensions)
]

and unary_expression = [
    `PLUS_exp of (Token.t (* "+" *) * expression)
  | `DASH_exp of (Token.t (* "-" *) * expression)
  | `BANG_exp of (Token.t (* "!" *) * expression)
  | `TILDE_exp of (Token.t (* "~" *) * expression)
]

and unqualified_object_creation_expression = (
    Token.t (* "new" *)
  * type_arguments option
  * simple_type
  * argument_list
  * class_body option
)

and update_expression = [
    `Exp_PLUSPLUS of (expression * Token.t (* "++" *))
  | `Exp_DASHDASH of (expression * Token.t (* "--" *))
  | `PLUSPLUS_exp of (Token.t (* "++" *) * expression)
  | `DASHDASH_exp of (Token.t (* "--" *) * expression)
]

and variable_declarator = (
    variable_declarator_id
  * (Token.t (* "=" *) * variable_initializer) option
)

and variable_declarator_id = (anon_choice_id_0e59f50 * dimensions option)

and variable_declarator_list = (
    variable_declarator
  * (Token.t (* "," *) * variable_declarator) list (* zero or more *)
)

and variable_initializer = [
    `Exp of expression
  | `Array_init of array_initializer
]

and while_statement = (
    Token.t (* "while" *) * parenthesized_expression * statement
)

and wildcard_bounds = [
    `Extends_type of superclass
  | `Super_type of (Token.t (* "super" *) * type_)
]

and yield_statement = (
    Token.t (* "yield" *) * expression * Token.t (* ";" *)
)
[@@deriving sexp_of]

type program = [
    `Rep_stmt of statement list (* zero or more *)
  | `Cons_decl of constructor_declaration
  | `Exp of expression
]
[@@deriving sexp_of]

type true_ (* inlined *) = Token.t (* "true" *)
[@@deriving sexp_of]

type asterisk (* inlined *) = Token.t (* "*" *)
[@@deriving sexp_of]

type null_literal (* inlined *) = Token.t (* "null" *)
[@@deriving sexp_of]

type false_ (* inlined *) = Token.t (* "false" *)
[@@deriving sexp_of]

type semgrep_ellipsis (* inlined *) = Token.t (* "..." *)
[@@deriving sexp_of]

type void_type (* inlined *) = Token.t (* "void" *)
[@@deriving sexp_of]

type this (* inlined *) = Token.t (* "this" *)
[@@deriving sexp_of]

type super (* inlined *) = Token.t (* "super" *)
[@@deriving sexp_of]

type boolean_type (* inlined *) = Token.t (* "boolean" *)
[@@deriving sexp_of]

type dummy_alias1 (* inlined *) = line_comment (*tok*)
[@@deriving sexp_of]

type comment (* inlined *) = [
    `Line_comm_expl of line_comment_explicit (*tok*)
  | `Blk_comm_expl of block_comment_explicit (*tok*)
]
[@@deriving sexp_of]

type dummy_alias0 (* inlined *) = block_comment (*tok*)
[@@deriving sexp_of]

type scoped_identifier (* inlined *) = (
    name * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type provides_module_directive (* inlined *) = (
    Token.t (* "provides" *)
  * name
  * Token.t (* "with" *)
  * name
  * (Token.t (* "," *) * name) list (* zero or more *)
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type requires_module_directive (* inlined *) = (
    Token.t (* "requires" *)
  * requires_modifier list (* zero or more *)
  * name
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type import_declaration (* inlined *) = (
    Token.t (* "import" *)
  * Token.t (* "static" *) option
  * name
  * (Token.t (* "." *) * Token.t (* "*" *)) option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type marker_annotation (* inlined *) = (Token.t (* "@" *) * name)
[@@deriving sexp_of]

type uses_module_directive (* inlined *) = (
    Token.t (* "uses" *) * name * Token.t (* ";" *)
)
[@@deriving sexp_of]

type exports_module_directive (* inlined *) = (
    Token.t (* "exports" *)
  * name
  * anon_to_name_rep_COMMA_name_2956291 option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type opens_module_directive (* inlined *) = (
    Token.t (* "opens" *)
  * name
  * anon_to_name_rep_COMMA_name_2956291 option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type annotated_type (* inlined *) = (
    annotation list (* one or more *)
  * unannotated_type
)
[@@deriving sexp_of]

type annotation_ (* inlined *) = (
    Token.t (* "@" *) * name * annotation_argument_list
)
[@@deriving sexp_of]

type array_type (* inlined *) = (unannotated_type * dimensions)
[@@deriving sexp_of]

type assignment_expression (* inlined *) = (
    [
        `Id of identifier (*tok*)
      | `Choice_open of reserved_identifier
      | `Field_access of field_access
      | `Array_access of array_access
    ]
  * [
        `EQ of Token.t (* "=" *)
      | `PLUSEQ of Token.t (* "+=" *)
      | `DASHEQ of Token.t (* "-=" *)
      | `STAREQ of Token.t (* "*=" *)
      | `SLASHEQ of Token.t (* "/=" *)
      | `AMPEQ of Token.t (* "&=" *)
      | `BAREQ of Token.t (* "|=" *)
      | `HATEQ of Token.t (* "^=" *)
      | `PERCEQ of Token.t (* "%=" *)
      | `LTLTEQ of Token.t (* "<<=" *)
      | `GTGTEQ of Token.t (* ">>=" *)
      | `GTGTGTEQ of Token.t (* ">>>=" *)
    ]
  * expression
)
[@@deriving sexp_of]

type cast_expression (* inlined *) = (
    Token.t (* "(" *)
  * type_
  * (Token.t (* "&" *) * type_) list (* zero or more *)
  * Token.t (* ")" *)
  * expression
)
[@@deriving sexp_of]

type element_value_array_initializer (* inlined *) = (
    Token.t (* "{" *)
  * (
        element_value
      * (Token.t (* "," *) * element_value) list (* zero or more *)
    )
      option
  * Token.t (* "," *) option
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type field_declaration (* inlined *) = (
    modifiers option
  * unannotated_type
  * variable_declarator_list
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type instanceof_expression (* inlined *) = (
    expression * Token.t (* "instanceof" *) * type_
)
[@@deriving sexp_of]

type lambda_expression (* inlined *) = (
    [
        `Id of identifier (*tok*)
      | `Formal_params of formal_parameters
      | `Infe_params of inferred_parameters
    ]
  * Token.t (* "->" *)
  * [ `Exp of expression | `Blk of block ]
)
[@@deriving sexp_of]

type module_declaration (* inlined *) = (
    annotation list (* zero or more *)
  * Token.t (* "open" *) option
  * Token.t (* "module" *)
  * name
  * module_body
)
[@@deriving sexp_of]

type package_declaration (* inlined *) = (
    annotation list (* zero or more *)
  * Token.t (* "package" *)
  * name
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type record_declaration (* inlined *) = (
    modifiers option
  * Token.t (* "record" *)
  * identifier (*tok*)
  * formal_parameters
  * class_body
)
[@@deriving sexp_of]

type spread_parameter (* inlined *) = (
    modifiers option
  * unannotated_type
  * Token.t (* "..." *)
  * variable_declarator
)
[@@deriving sexp_of]

type static_initializer (* inlined *) = (Token.t (* "static" *) * block)
[@@deriving sexp_of]

type ternary_expression (* inlined *) = (
    expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
  * expression
)
[@@deriving sexp_of]

type wildcard (* inlined *) = (
    annotation list (* zero or more *)
  * Token.t (* "?" *)
  * wildcard_bounds option
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_program root
  |> Print_sexp.to_stdout
