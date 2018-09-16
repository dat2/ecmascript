use ast::*;
use combine::stream::state::State;
use combine::{eof, Parser};
use parser::*;

macro_rules! assert_parse_success {
    ($parser:ident, $input:expr, $result:expr) => {
        assert_eq!(
            ($parser(), eof())
                .map(|x| x.0)
                .easy_parse(State::new($input))
                .map(|x| x.0),
            Ok($result)
        );
    };
    ($parser:ident ($($args:expr),*), $input:expr, $result:expr) => {
        assert_eq!(
            ($parser($($args),*), eof())
                .map(|x| x.0)
                .easy_parse(State::new($input))
                .map(|x| x.0),
            Ok($result)
        );
    };
}

macro_rules! assert_parse_failure {
    ($parser:ident, $input:expr) => {
        assert!(($parser(), eof()).easy_parse(State::new($input)).is_err());
    };
    ($parser:ident ($($args:expr),*), $input:expr) => {
        assert!(($parser($($args),*), eof()).easy_parse(State::new($input)).is_err());
    };
}

#[test]
fn test_block_comment() {
    assert_parse_success!(comment, "/**/", ());
    assert_parse_success!(comment, "/* * */", ());
    assert_parse_success!(comment, "/** * **/", ());
    assert_parse_success!(comment, "/* hello *\n\t */", ());
}

#[test]
fn test_identifier_start_invalid_escape_sequence() {
    // making sure that the unicode_escape_sequence satisifies things
    // eg. ZWNJ and ZWJ are not allowed as starts
    assert_parse_failure!(identifier, r"\u000a");
    assert_parse_failure!(identifier, r"\u200d");
    assert_parse_failure!(identifier, r"\u200c");
}

#[test]
fn test_identifier_start_valid() {
    // testing $, _, unicode_escape_sequence as start
    assert_parse_success!(identifier, r"\u0024", "$".to_string());
    assert_parse_success!(identifier, r"_", "_".to_string());
}

#[test]
fn test_identifier_continue_valid() {
    // testing $, _, ZWNJ, ZWJ, unicode_escape_sequence as continue
    assert_parse_success!(identifier, r"a_", "a_".to_string());
    assert_parse_success!(identifier, r"a$", "a$".to_string());
    assert_parse_success!(identifier, r"_\u200d", "_\u{200d}".to_string());
    assert_parse_success!(identifier, r"_\u200c", "_\u{200c}".to_string());
}

#[test]
fn test_identifier_reserved_word() {
    for &keyword in KEYWORDS.iter() {
        assert_parse_failure!(identifier, keyword);
    }
    for &keyword in FUTURE_RESERVED_WORDS.iter() {
        assert_parse_failure!(identifier, keyword);
    }
    for &keyword in FUTURE_RESERVED_WORDS_STRICT.iter() {
        assert_parse_failure!(identifier, keyword);
    }

    // null literal
    assert_parse_failure!(identifier, "null");
    // boolean literal
    assert_parse_failure!(identifier, "true");
    assert_parse_failure!(identifier, "false");
}

#[test]
fn test_null_literal() {
    assert_parse_success!(null_literal, "null", NullLiteral);
}

#[test]
fn test_boolean_literal() {
    assert_parse_success!(boolean_literal, "true", BooleanLiteral(true));
    assert_parse_success!(boolean_literal, "false", BooleanLiteral(false));
}

#[test]
fn test_number_literal_decimal() {
    // decimal
    assert_parse_success!(numeric_literal, "0", NumericLiteral(0f64));
    assert_parse_failure!(numeric_literal, "01");
    assert_parse_failure!(numeric_literal, "01.");
    assert_parse_success!(numeric_literal, "9", NumericLiteral(9f64));
    assert_parse_success!(numeric_literal, "10", NumericLiteral(10f64));
    assert_parse_success!(numeric_literal, "0.1", NumericLiteral(0.1f64));
    assert_parse_success!(numeric_literal, ".1", NumericLiteral(0.1f64));
    assert_parse_success!(numeric_literal, "1e1", NumericLiteral(10f64));
    assert_parse_success!(numeric_literal, ".1e1", NumericLiteral(1f64));
    assert_parse_success!(numeric_literal, "1.1e1", NumericLiteral(11f64));
}

#[test]
fn test_number_literal_binary() {
    // binary
    assert_parse_success!(numeric_literal, "0b1010", NumericLiteral(10f64));
    assert_parse_success!(numeric_literal, "0B1010", NumericLiteral(10f64));
}

#[test]
fn test_number_literal_octal() {
    // octal
    assert_parse_success!(numeric_literal, "0o123", NumericLiteral(83f64));
    assert_parse_success!(numeric_literal, "0O123", NumericLiteral(83f64));
}

#[test]
fn test_number_literal_hex() {
    // hex
    assert_parse_success!(numeric_literal, "0XDEADBEEF", NumericLiteral(3735928559f64));
    assert_parse_success!(numeric_literal, "0xDEADBEEF", NumericLiteral(3735928559f64));
}

#[test]
fn test_regex_literal_empty() {
    // must be non empty
    assert_parse_failure!(regex_literal, "//");
}

#[test]
fn test_regex_literal_start_invalid() {
    // not allowed first chars
    for c in "*\\/[".chars() {
        let slice: &str = &format!("/{}/", c);
        assert_parse_failure!(regex_literal, slice);
    }
}

#[test]
fn test_regex_literal_start_backslash() {
    // backslash as first char
    assert_parse_success!(
        regex_literal,
        "/\\a/",
        RegExpLiteral {
            pattern: "\\a".to_string(),
            flags: String::new(),
        }
    );
}

#[test]
fn test_regex_literal_start_character_class() {
    // character class as first char
    assert_parse_success!(
        regex_literal,
        "/[ab]/",
        RegExpLiteral {
            pattern: "[ab]".to_string(),
            flags: String::new(),
        }
    );
}

#[test]
fn test_regex_literal_continue_backslash() {
    // backslash as second char
    assert_parse_success!(
        regex_literal,
        "/a\\a/",
        RegExpLiteral {
            pattern: "a\\a".to_string(),
            flags: String::new(),
        }
    );
}

#[test]
fn test_regex_literal_continue_character_class() {
    // character class as second char
    assert_parse_success!(
        regex_literal,
        "/a[ab]/",
        RegExpLiteral {
            pattern: "a[ab]".to_string(),
            flags: String::new(),
        }
    );
}

#[test]
fn test_regex_literal_character_class_backslash() {
    // character class with backslash
    assert_parse_success!(
        regex_literal,
        "/a[ab\\]]/",
        RegExpLiteral {
            pattern: "a[ab\\]]".to_string(),
            flags: String::new(),
        }
    );
}

#[test]
fn test_regex_literal_flags() {
    // flags
    assert_parse_success!(
        regex_literal,
        "/a/f",
        RegExpLiteral {
            pattern: "a".to_string(),
            flags: "f".to_string(),
        }
    );
    assert_parse_success!(
        regex_literal,
        "/a/fi",
        RegExpLiteral {
            pattern: "a".to_string(),
            flags: "fi".to_string(),
        }
    );
    assert_parse_failure!(regex_literal, "/a/\\u1234");
}

#[test]
fn test_template_element_empty() {
    assert_parse_success!(
        primary_expression(false, false),
        "``",
        Expression::TemplateLiteral {
            quasis: vec![TemplateElement {
                raw: String::new(),
                cooked: Some(String::new()),
                loc: Some(((1, 0), (1, 2)).into()),
            }],
            expressions: vec![],
            loc: Some(((1, 0), (1, 2)).into()),
        }
    );
}

#[test]
fn test_template_element_no_expressions() {
    assert_parse_success!(
        primary_expression(false, false),
        "`asd`",
        Expression::TemplateLiteral {
            quasis: vec![TemplateElement {
                cooked: Some("asd".to_string()),
                raw: "asd".to_string(),
                loc: Some(((1, 0), (1, 5)).into())
            }],
            expressions: vec![],
            loc: Some(((1, 0), (1, 5)).into())
        }
    );
}

#[test]
fn test_template_element_template_single_expression() {
    assert_parse_success!(
        primary_expression(true, false),
        "`asd ${yield} asd`",
        Expression::TemplateLiteral {
            quasis: vec![
                TemplateElement {
                    cooked: Some("asd ".to_string()),
                    raw: "asd ".to_string(),
                    loc: Some(((1, 0), (1, 7)).into())
                },
                TemplateElement {
                    cooked: Some(" asd".to_string()),
                    raw: " asd".to_string(),
                    loc: Some(((1, 12), (1, 18)).into())
                }
            ],
            expressions: vec![Expression::Yield {
                argument: None,
                delegate: false
            }],
            loc: Some(((1, 0), (1, 18)).into())
        }
    );
}

#[test]
fn test_template_element_template_multiple_expressions() {
    assert_parse_success!(
        primary_expression(true, false),
        "`asd ${yield} asd ${yield} asd`",
        Expression::TemplateLiteral {
            quasis: vec![
                TemplateElement {
                    cooked: Some("asd ".to_string()),
                    raw: "asd ".to_string(),
                    loc: Some(((1, 0), (1, 7)).into()),
                },
                TemplateElement {
                    cooked: Some(" asd ".to_string()),
                    raw: " asd ".to_string(),
                    loc: Some(((1, 12), (1, 20)).into())
                },
                TemplateElement {
                    cooked: Some(" asd".to_string()),
                    raw: " asd".to_string(),
                    loc: Some(((1, 25), (1, 31)).into())
                }
            ],
            expressions: vec![
                Expression::Yield {
                    argument: None,
                    delegate: false
                },
                Expression::Yield {
                    argument: None,
                    delegate: false
                }
            ],
            loc: Some(((1, 0), (1, 31)).into())
        }
    );
}

#[test]
fn test_template_element_template_character() {
    // $
    assert_parse_success!(template_character, "$", ('$', "$".to_string()));
    // escape sequence
    assert_parse_success!(template_character, "\\n", ('\n', "\\n".to_string()));
    assert_parse_success!(template_character, "\\x0A", ('\n', "\\x0A".to_string()));
    assert_parse_success!(
        template_character,
        "\\u2764",
        ('❤', "\\u2764".to_string())
    );
    assert_parse_success!(
        template_character,
        "\\u{2764}",
        ('❤', "\\u{2764}".to_string())
    );
    // line continuation
    for line_continuation_char in "\r\n\u{2028}\u{2029}".chars() {
        let slice: &str = &line_continuation_char.to_string();
        assert_parse_success!(
            template_character,
            slice,
            (line_continuation_char, line_continuation_char.to_string())
        );
    }
}

#[test]
fn test_primary_expression_this() {
    assert_parse_success!(
        primary_expression(false, false),
        "this",
        Expression::ThisExpression {
            loc: Some(((1, 0), (1, 4)).into())
        }
    );
}

#[test]
fn test_primary_expression_identifier_reference() {
    assert_parse_success!(
        primary_expression(false, false),
        "abc123",
        Expression::Identifier {
            loc: Some(((1, 0), (1, 6)).into()),
            name: "abc123".to_string()
        }
    );
}

#[test]
fn test_primary_expression_literal() {
    assert_parse_success!(
        primary_expression(false, false),
        "null",
        Expression::Literal {
            value: Literal::NullLiteral(NullLiteral),
            loc: Some(((1, 0), (1, 4)).into())
        }
    );
    assert_parse_success!(
        primary_expression(false, false),
        "true",
        Expression::Literal {
            value: Literal::BooleanLiteral(BooleanLiteral(true)),
            loc: Some(((1, 0), (1, 4)).into())
        }
    );
    assert_parse_success!(
        primary_expression(false, false),
        "false",
        Expression::Literal {
            value: Literal::BooleanLiteral(BooleanLiteral(false)),
            loc: Some(((1, 0), (1, 5)).into())
        }
    );
    assert_parse_success!(
        primary_expression(false, false),
        "123.e1",
        Expression::Literal {
            value: Literal::NumericLiteral(NumericLiteral(1230f64)),
            loc: Some(((1, 0), (1, 6)).into())
        }
    );
    assert_parse_success!(
        primary_expression(false, false),
        "'abc'",
        Expression::Literal {
            value: Literal::StringLiteral(StringLiteral("abc".to_string())),
            loc: Some(((1, 0), (1, 5)).into()),
        }
    );
    assert_parse_success!(
        primary_expression(false, false),
        "/\\a/",
        Expression::Literal {
            value: Literal::RegExpLiteral(RegExpLiteral {
                pattern: "\\a".to_string(),
                flags: String::new(),
            }),
            loc: Some(((1, 0), (1, 4)).into())
        }
    );
}

#[test]
fn test_primary_expression_object_literal_empty() {
    assert_parse_success!(
        primary_expression(false, false),
        "{}",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 2)).into()),
            properties: Vec::new()
        }
    );
}

#[test]
fn test_primary_expression_object_literal_shorthand() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ id }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 6)).into()),
            properties: vec![ObjectExpressionProperty::Property(Property {
                kind: PropertyKind::Init,
                key: Expression::Identifier {
                    loc: Some(((1, 2), (1, 4)).into()),
                    name: "id".to_string(),
                },
                value: Expression::Identifier {
                    loc: Some(((1, 2), (1, 4)).into()),
                    name: "id".to_string(),
                },
                method: false,
                shorthand: true,
                computed: false,
                loc: Some(((1, 2), (1, 4)).into()),
            })],
        }
    );
}

#[test]
fn test_primary_expression_object_literal_multiple_properties() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ id, id2 }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 11)).into()),
            properties: vec![
                ObjectExpressionProperty::Property(Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier {
                        loc: Some(((1, 2), (1, 4)).into()),
                        name: "id".to_string(),
                    },
                    value: Expression::Identifier {
                        loc: Some(((1, 2), (1, 4)).into()),
                        name: "id".to_string(),
                    },
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 2), (1, 4)).into()),
                }),
                ObjectExpressionProperty::Property(Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier {
                        loc: Some(((1, 6), (1, 9)).into()),
                        name: "id2".to_string(),
                    },
                    value: Expression::Identifier {
                        loc: Some(((1, 6), (1, 9)).into()),
                        name: "id2".to_string(),
                    },
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 6), (1, 9)).into()),
                }),
            ],
        }
    );
}

#[test]
fn test_primary_expression_object_literal_multiple_properties_ending_semicolon() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ id, id2, }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 12)).into()),
            properties: vec![
                ObjectExpressionProperty::Property(Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier {
                        loc: Some(((1, 2), (1, 4)).into()),
                        name: "id".to_string(),
                    },
                    value: Expression::Identifier {
                        loc: Some(((1, 2), (1, 4)).into()),
                        name: "id".to_string(),
                    },
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 2), (1, 4)).into()),
                }),
                ObjectExpressionProperty::Property(Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier {
                        loc: Some(((1, 6), (1, 9)).into()),
                        name: "id2".to_string(),
                    },
                    value: Expression::Identifier {
                        loc: Some(((1, 6), (1, 9)).into()),
                        name: "id2".to_string(),
                    },
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 6), (1, 9)).into()),
                }),
            ],
        }
    );
}

#[test]
fn test_primary_expression_object_literal_initializer() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ id: true }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 12)).into()),
            properties: vec![ObjectExpressionProperty::Property(Property {
                kind: PropertyKind::Init,
                key: Expression::Identifier {
                    loc: Some(((1, 2), (1, 4)).into()),
                    name: "id".to_string(),
                },
                value: Expression::Literal {
                    value: Literal::BooleanLiteral(BooleanLiteral(true)),
                    loc: Some(((1, 6), (1, 10)).into()),
                },
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 10)).into()),
            })],
        }
    );
}

#[test]
fn test_object_literal_initializer_string_literal() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ 'id': true }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 14)).into()),
            properties: vec![ObjectExpressionProperty::Property(Property {
                kind: PropertyKind::Init,
                key: Expression::Literal {
                    value: Literal::StringLiteral(StringLiteral("id".to_string())),
                    loc: Some(((1, 2), (1, 6)).into()),
                },
                value: Expression::Literal {
                    value: Literal::BooleanLiteral(BooleanLiteral(true)),
                    loc: Some(((1, 8), (1, 12)).into()),
                },
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 12)).into()),
            })],
        }
    );
}

#[test]
fn test_object_literal_initializer_numeric_literal() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ 0: true }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 11)).into()),
            properties: vec![ObjectExpressionProperty::Property(Property {
                kind: PropertyKind::Init,
                key: Expression::Literal {
                    value: Literal::NumericLiteral(NumericLiteral(0f64)),
                    loc: Some(((1, 2), (1, 3)).into()),
                },
                value: Expression::Literal {
                    value: Literal::BooleanLiteral(BooleanLiteral(true)),
                    loc: Some(((1, 5), (1, 9)).into()),
                },
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 9)).into()),
            })],
        }
    );
}

#[test]
fn test_object_literal_initializer_computed() {
    assert_parse_success!(
        primary_expression(true, false),
        "{ [yield]: true }",
        Expression::ObjectExpression {
            loc: Some(((1, 0), (1, 17)).into()),
            properties: vec![ObjectExpressionProperty::Property(Property {
                kind: PropertyKind::Init,
                key: Expression::Yield {
                    argument: None,
                    delegate: false,
                },
                value: Expression::Literal {
                    value: Literal::BooleanLiteral(BooleanLiteral(true)),
                    loc: Some(((1, 11), (1, 15)).into()),
                },
                method: false,
                shorthand: false,
                computed: true,
                loc: Some(((1, 2), (1, 15)).into()),
            })],
        }
    );
}

#[test]
fn test_object_literal_method_definition() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ method() {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "method".to_string(),
                    loc: Some(((1, 2), (1, 8)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![],
                    body: vec![],
                    async: false,
                    generator: false,
                },
                kind: PropertyKind::Init,
                method: true,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 15)).into()),
            })],
            loc: Some(((1, 0), (1, 17)).into()),
        }
    );
}

#[test]
fn test_object_literal_method_definition_generator() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ * method() {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "method".to_string(),
                    loc: Some(((1, 4), (1, 10)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![],
                    body: vec![],
                    async: false,
                    generator: true,
                },
                kind: PropertyKind::Init,
                method: true,
                shorthand: false,
                computed: false,
                loc: Some(((1, 4), (1, 17)).into()),
            })],
            loc: Some(((1, 0), (1, 19)).into()),
        }
    );
}

#[test]
fn test_object_literal_method_definition_async() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ async method() {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "method".to_string(),
                    loc: Some(((1, 8), (1, 14)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![],
                    body: vec![],
                    async: true,
                    generator: false,
                },
                kind: PropertyKind::Init,
                method: true,
                shorthand: false,
                computed: false,
                loc: Some(((1, 8), (1, 21)).into()),
            })],
            loc: Some(((1, 0), (1, 23)).into()),
        }
    );
}

#[test]
fn test_object_literal_method_definition_async_generator() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ async * method() {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "method".to_string(),
                    loc: Some(((1, 10), (1, 16)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![],
                    body: vec![],
                    async: true,
                    generator: true,
                },
                kind: PropertyKind::Init,
                method: true,
                shorthand: false,
                computed: false,
                loc: Some(((1, 10), (1, 23)).into()),
            })],
            loc: Some(((1, 0), (1, 25)).into()),
        }
    );
}

#[test]
fn test_object_literal_method_definition_getter() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ get key() {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "key".to_string(),
                    loc: Some(((1, 6), (1, 9)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![],
                    body: vec![],
                    async: false,
                    generator: false,
                },
                kind: PropertyKind::Get,
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 16)).into()),
            })],
            loc: Some(((1, 0), (1, 18)).into()),
        }
    );
}

#[test]
fn test_object_literal_method_definition_setter() {
    assert_parse_success!(
        primary_expression(false, false),
        "{ set key(value) {  } }",
        Expression::ObjectExpression {
            properties: vec![ObjectExpressionProperty::Property(Property {
                key: Expression::Identifier {
                    name: "key".to_string(),
                    loc: Some(((1, 6), (1, 9)).into()),
                },
                value: Expression::FunctionExpression {
                    id: None,
                    params: vec![Pattern::Identifier {
                        name: "value".to_string(),
                        loc: Some(((1, 10), (1, 15)).into()),
                    }],
                    body: vec![],
                    async: false,
                    generator: false,
                },
                kind: PropertyKind::Set,
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 2), (1, 21)).into()),
            })],
            loc: Some(((1, 0), (1, 23)).into()),
        }
    );
}

#[test]
fn test_primary_expression_jsx_self_closing() {
    assert_parse_success!(
        primary_expression(false, false),
        "<div/>",
        Expression::JSXElement {
            attributes: Vec::new(),
            children: Vec::new(),
            name: "div".to_string(),
            loc: Some(((1, 0), (1, 6)).into())
        }
    );
}

#[test]
fn test_primary_expression_jsx_opening_closing_match() {
    assert_parse_success!(
        primary_expression(false, false),
        "<div>\n\n</div>",
        Expression::JSXElement {
            attributes: Vec::new(),
            children: Vec::new(),
            name: "div".to_string(),
            loc: Some(((1, 0), (3, 6)).into())
        }
    );
    assert_parse_failure!(primary_expression(false, false), "<div>\n\n</v>");
}
