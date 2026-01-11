use bnf_rules::bnf_rules_macro::bnf_rules;

bnf_rules! {
    #[generate_code = false]

    source              ::= program

    program             ::= { statement }

    statement           ::= ( let_statement | expression | assignment ) ";" | function_define | class_define | if_statement

    let_statement       ::= "let" literal [ "=" expression ]

    assignment          ::= primary "=" expression

    function_define     ::= "function" literal "(" [ function_argument ] { "," function_argument } ")" [ "->" type_info ] block
    function_argument   ::= literal ":" type_info

    class_define        ::= "class" literal "{" [ class_field ] { "," class_field } "}"
    class_field         ::= literal ":" type_info

    if_statement        ::= if_branch { "else" ( if_branch | block ) }
    if_branch           ::= "if" expression block

    block               ::= "{" program "}"

    type_info           ::= literal

    expression          ::= or_expr | new_expression | return_expression

    or_expr             ::= and_expr { "or" and_expr }
    and_expr            ::= equ_or_ine_expr { "and" equ_or_ine_expr }
    equ_or_ine_expr     ::= les_or_gre_expr [ ( "==" | "!=" ) les_or_gre_expr ]
    les_or_gre_expr     ::= add_or_sub_expr [ ( "<" | ">" | "<=" | ">=" ) add_or_sub_expr ]
    add_or_sub_expr     ::= mul_or_div_expr { ( "+" | "-" ) mul_or_div_expr }
    mul_or_div_expr     ::= factor { ( "*" | "/" ) factor }

    factor              ::= "-" primary | primary

    primary             ::= primary_left { "." primary_right }

    primary_left        ::= literal [ function_call ] | r"\d+(\.\d+)?" | r#"".*""#

    primary_right       ::= literal

    function_call       ::= "(" [ expression ] { "," expression } ")"

    new_expression      ::= "new" literal "{" [ field_assign ] { "," field_assign } "}"
    field_assign        ::= literal ":" expression

    return_expression   ::= "return" [ expression ]

    literal             ::= r"\w+"
}
