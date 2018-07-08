// activate test feature when "nightly" is set
#![cfg_attr(feature = "nightly", feature(test))]
extern crate combine;
extern crate ecmascript;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate glob;
extern crate serde_json;
extern crate test;

#[cfg(all(feature = "nightly", test))]
mod nightly_integration_tests {
    use ecmascript;
    use glob::glob;
    use serde_json::{self, Value};
    use std::env;
    use std::fs::File;
    use test::ShouldPanic::No;
    use test::{test_main, Options, TestDesc, TestDescAndFn, TestFn, TestName};

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(untagged)]
    enum TestFixture {
        Success {
            name: String,
            source: String,
            result: Value,
        },
        Failure {
            name: String,
            source: String,
            error: String,
        },
    }

    impl TestFixture {
        fn name(&self) -> String {
            match self {
                TestFixture::Success { name, .. } => name.clone(),
                TestFixture::Failure { name, .. } => name.clone(),
            }
        }

        fn source(&self) -> String {
            match self {
                TestFixture::Success { source, .. } => source.clone(),
                TestFixture::Failure { source, .. } => source.clone(),
            }
        }
    }

    fn add_test(tests: &mut Vec<TestDescAndFn>, scope: &str, test_fixture: TestFixture) {
        tests.push(TestDescAndFn {
            desc: TestDesc {
                name: TestName::DynTestName(format!("{}::{}", scope, test_fixture.name())),
                ignore: false,
                should_panic: No,
                allow_fail: false,
            },
            testfn: TestFn::DynTestFn(Box::new(move || {
                let result = ecmascript::parse(&test_fixture.source())
                    .map(|result| serde_json::to_value(result).unwrap());
                match (result, test_fixture) {
                    (Ok(value), TestFixture::Success { result, .. }) => {
                        println!(
                            "left: {}, right: {}",
                            serde_json::to_string(&value).unwrap(),
                            serde_json::to_string(&result).unwrap()
                        );
                        assert_eq!(value, result)
                    }
                    (Ok(value), TestFixture::Failure { source, error, .. }) => {
                        println!("Expecting {:?} to fail with message {:?}", source, error);
                        println!("But it passed with {:?}", value);
                        panic!()
                    }
                    (Err(e), TestFixture::Success { source, .. }) => {
                        println!("Source: {:?}\n\nError: {}", source, e);
                        panic!()
                    }
                    (Err(e), TestFixture::Failure { error, .. }) => {
                        assert_eq!(e.to_string(), error)
                    }
                }
            })),
        });
    }

    pub fn main() {
        let args: Vec<_> = env::args().collect();
        let mut tests = Vec::new();
        for entry in glob("tests/fixtures/*.json").unwrap() {
            let path = entry.unwrap();
            let scope = path.file_stem().unwrap().to_str().unwrap();
            let file = File::open(&path).unwrap();
            let fixtures: Vec<TestFixture> = serde_json::from_reader(file).unwrap();
            for fixture in fixtures {
                add_test(&mut tests, scope, fixture);
            }
        }
        test_main(&args, tests, Options::new());
    }
}

#[cfg(all(feature = "nightly", test))]
fn main() {
    nightly_integration_tests::main()
}

#[cfg(not(all(feature = "nightly", test)))]
fn main() {
    println!("Sorry! Integration testing is not supported on the stable or beta channels yet.")
}

/*

#[test]
fn test_boolean_literal() {
    assert_parse_success!(
        boolean_literal,
        "true",
        BooleanLiteral(Some(((1, 1), (1, 5)).into()), true)
    );
    assert_parse_success!(
        boolean_literal,
        "false",
        BooleanLiteral(Some(((1, 1), (1, 6)).into()), false)
    );
}

#[test]
fn test_number_literal_decimal() {
    // decimal
    assert_parse_success!(
        numeric_literal,
        "0",
        NumericLiteral(Some(((1, 1), (1, 2)).into()), 0f64)
    );
    assert_parse_failure!(numeric_literal, "01");
    assert_parse_failure!(numeric_literal, "01.");
    assert_parse_success!(
        numeric_literal,
        "9",
        NumericLiteral(Some(((1, 1), (1, 2)).into()), 9f64)
    );
    assert_parse_success!(
        numeric_literal,
        "10",
        NumericLiteral(Some(((1, 1), (1, 3)).into()), 10f64)
    );
    assert_parse_success!(
        numeric_literal,
        "0.1",
        NumericLiteral(Some(((1, 1), (1, 4)).into()), 0.1f64)
    );
    assert_parse_success!(
        numeric_literal,
        ".1",
        NumericLiteral(Some(((1, 1), (1, 3)).into()), 0.1f64)
    );
    assert_parse_success!(
        numeric_literal,
        "1e1",
        NumericLiteral(Some(((1, 1), (1, 4)).into()), 10f64)
    );
    assert_parse_success!(
        numeric_literal,
        ".1e1",
        NumericLiteral(Some(((1, 1), (1, 5)).into()), 1f64)
    );
    assert_parse_success!(
        numeric_literal,
        "1.1e1",
        NumericLiteral(Some(((1, 1), (1, 6)).into()), 11f64)
    );
}

#[test]
fn test_number_literal_binary() {
    // binary
    assert_parse_success!(
        numeric_literal,
        "0b1010",
        NumericLiteral(Some(((1, 1), (1, 7)).into()), 10f64)
    );
    assert_parse_success!(
        numeric_literal,
        "0B1010",
        NumericLiteral(Some(((1, 1), (1, 7)).into()), 10f64)
    );
}

#[test]
fn test_number_literal_octal() {
    // octal
    assert_parse_success!(
        numeric_literal,
        "0o123",
        NumericLiteral(Some(((1, 1), (1, 6)).into()), 83f64)
    );
    assert_parse_success!(
        numeric_literal,
        "0O123",
        NumericLiteral(Some(((1, 1), (1, 6)).into()), 83f64)
    );
}

#[test]
fn test_number_literal_hex() {
    // hex
    assert_parse_success!(
        numeric_literal,
        "0XDEADBEEF",
        NumericLiteral(Some(((1, 1), (1, 11)).into()), 3735928559f64)
    );
    assert_parse_success!(
        numeric_literal,
        "0xDEADBEEF",
        NumericLiteral(Some(((1, 1), (1, 11)).into()), 3735928559f64)
    );
}

#[test]
fn test_string_literal_empty() {
    // empty
    assert_parse_success!(
        string_literal,
        r#""""#,
        StringLiteral(Some(((1, 1), (1, 3)).into()), String::new())
    );
    assert_parse_success!(
        string_literal,
        "''",
        StringLiteral(Some(((1, 1), (1, 3)).into()), String::new())
    );
}

#[test]
fn test_string_literal_invalid_chars() {
    // not allowed chars
    for not_allowed_char in "\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}".chars() {
        let double_quote_slice: &str = &format!("\"{}\"", not_allowed_char);
        let single_quote_slice: &str = &format!("'{}'", not_allowed_char);
        assert_parse_failure!(string_literal, double_quote_slice);
        assert_parse_failure!(string_literal, single_quote_slice);
    }
}

#[test]
fn test_string_literal_character_escape_sequence() {
    // character escape sequences
    let escape_chars = r#"'"\bfnrtv"#.chars();
    let escape_char_values = "\'\"\\\u{8}\u{c}\n\r\t\u{b}".chars();
    for (escaped_character, value) in escape_chars.zip(escape_char_values) {
        let double_quote_slice: &str = &format!("\"\\{}\"", escaped_character);
        let single_quote_slice: &str = &format!("'\\{}'", escaped_character);
        assert_parse_success!(
            string_literal,
            double_quote_slice,
            StringLiteral(Some(((1, 1), (1, 5)).into()), value.to_string())
        );
        assert_parse_success!(
            string_literal,
            single_quote_slice,
            StringLiteral(Some(((1, 1), (1, 5)).into()), value.to_string())
        );
    }
    // non character escape sequences
    assert_parse_success!(
        string_literal,
        "\"\\a\"",
        StringLiteral(Some(((1, 1), (1, 5)).into()), "a".to_string())
    );
    assert_parse_success!(
        string_literal,
        "'\\a'",
        StringLiteral(Some(((1, 1), (1, 5)).into()), "a".to_string())
    );
}

#[test]
fn test_string_literal_hex_escape_sequence() {
    // hex escape sequence
    assert_parse_success!(
        string_literal,
        r#""\x0A""#,
        StringLiteral(Some(((1, 1), (1, 7)).into()), "\n".to_string())
    );
    assert_parse_success!(
        string_literal,
        r#"'\x0A'"#,
        StringLiteral(Some(((1, 1), (1, 7)).into()), "\n".to_string())
    );
}

#[test]
fn test_string_literal_unicode_escape_sequence() {
    // unicode escape sequence
    assert_parse_success!(
        string_literal,
        r#""\u2764""#,
        StringLiteral(Some(((1, 1), (1, 9)).into()), "❤".to_string())
    );
    assert_parse_success!(
        string_literal,
        r"'\u2764'",
        StringLiteral(Some(((1, 1), (1, 9)).into()), "❤".to_string())
    );
    assert_parse_success!(
        string_literal,
        r#""\u{2764}""#,
        StringLiteral(Some(((1, 1), (1, 11)).into()), "❤".to_string())
    );
    assert_parse_success!(
        string_literal,
        r"'\u{2764}'",
        StringLiteral(Some(((1, 1), (1, 11)).into()), "❤".to_string())
    );
    assert_parse_failure!(string_literal, r"'\u{110000}'");
}

#[test]
fn test_string_literal_line_continuation_invalid() {
    // line continuation
    for line_continuation_char in "\r\n\u{2028}\u{2029}".chars() {
        let double_quote_slice: &str = &format!("\"\\{}\"", line_continuation_char);
        let single_quote_slice: &str = &format!("'\\{}'", line_continuation_char);
        assert_parse_failure!(string_literal, double_quote_slice);
        assert_parse_failure!(string_literal, single_quote_slice);
    }
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
            loc: Some(((1, 1), (1, 5)).into())
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
            loc: Some(((1, 1), (1, 7)).into())
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
            loc: Some(((1, 1), (1, 6)).into())
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
            loc: Some(((1, 1), (1, 8)).into())
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
            loc: Some(((1, 1), (1, 10)).into())
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
            loc: Some(((1, 1), (1, 5)).into())
        }
    );
    assert_parse_success!(
        regex_literal,
        "/a/fi",
        RegExpLiteral {
            pattern: "a".to_string(),
            flags: "fi".to_string(),
            loc: Some(((1, 1), (1, 6)).into())
        }
    );
    assert_parse_failure!(regex_literal, "/a/\\u1234");
}

#[test]
fn test_template_element_empty() {
    assert_parse_success!(
        template,
        "``",
        TemplateElement {
            raw: String::new(),
            cooked: String::new(),
            loc: Some(((1, 1), (1, 3)).into())
        }
    );
}

#[test]
fn test_template_element_no_substitution_template() {
    assert_parse_success!(
        template,
        "`asd`",
        TemplateElement {
            raw: "asd".to_string(),
            cooked: "asd".to_string(),
            loc: Some(((1, 1), (1, 6)).into())
        }
    );
}

#[test]
fn test_template_element_template_head() {
    assert_parse_success!(
        template,
        "`asd ${",
        TemplateElement {
            raw: "asd ".to_string(),
            cooked: "asd ".to_string(),
            loc: Some(((1, 1), (1, 8)).into())
        }
    );
}

#[test]
fn test_template_element_template_middle() {
    assert_parse_success!(
        template_substition_tail,
        "} asd ${",
        TemplateElement {
            raw: " asd ".to_string(),
            cooked: " asd ".to_string(),
            loc: Some(((1, 1), (1, 9)).into())
        }
    );
}

#[test]
fn test_template_element_template_tail() {
    // template_tail
    assert_parse_success!(
        template_substition_tail,
        "} asd",
        TemplateElement {
            raw: " asd".to_string(),
            cooked: " asd".to_string(),
            loc: Some(((1, 1), (1, 6)).into())
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
        primary_expression,
        "this",
        Expression::This(Some(((1, 1), (1, 5)).into()))
    );
}

#[test]
fn test_primary_expression_identifier_reference() {
    assert_parse_success!(
        primary_expression,
        "abc123",
        Expression::Identifier(Identifier(
            Some(((1, 1), (1, 7)).into()),
            "abc123".to_string()
        ))
    );
}

#[test]
fn test_primary_expression_literal() {
    assert_parse_success!(
        primary_expression,
        "null",
        Expression::Literal(Literal::NullLiteral(NullLiteral(Some(
            ((1, 1), (1, 5)).into()
        ))))
    );
    assert_parse_success!(
        primary_expression,
        "true",
        Expression::Literal(Literal::BooleanLiteral(BooleanLiteral(
            Some(((1, 1), (1, 5)).into()),
            true
        )))
    );
    assert_parse_success!(
        primary_expression,
        "false",
        Expression::Literal(Literal::BooleanLiteral(BooleanLiteral(
            Some(((1, 1), (1, 6)).into()),
            false
        )))
    );
    assert_parse_success!(
        primary_expression,
        "123.e1",
        Expression::Literal(Literal::NumericLiteral(NumericLiteral(
            Some(((1, 1), (1, 7)).into()),
            1230f64
        )))
    );
    assert_parse_success!(
        primary_expression,
        "'abc'",
        Expression::Literal(Literal::StringLiteral(StringLiteral(
            Some(((1, 1), (1, 6)).into()),
            "abc".to_string()
        )))
    );
    assert_parse_success!(
        primary_expression,
        "/\\a/",
        Expression::Literal(Literal::RegExpLiteral(RegExpLiteral {
            pattern: "\\a".to_string(),
            flags: String::new(),
            loc: Some(((1, 1), (1, 5)).into())
        }))
    );
}

#[test]
fn test_primary_expression_array_literal_empty() {
    assert_parse_success!(
        primary_expression,
        "[]",
        Expression::ArrayLiteral(Some(((1, 1), (1, 3)).into()), Vec::new())
    );
}

#[test]
fn test_primary_expression_array_literal_elision() {
    assert_parse_success!(
        primary_expression,
        "[,,,,]",
        Expression::ArrayLiteral(Some(((1, 1), (1, 7)).into()), Vec::new())
    );
}

#[test]
fn test_primary_expression_array_literal_elision_and_elements() {
    assert_parse_success!(
        primary_expression,
        "[,,,,yield,,yield,,,]",
        Expression::ArrayLiteral(
            Some(((1, 1), (1, 22)).into()),
            vec![
                ExpressionListItem::Expression(Expression::Yield {
                    argument: None,
                    delegate: false,
                }),
                ExpressionListItem::Expression(Expression::Yield {
                    argument: None,
                    delegate: false,
                }),
            ]
        )
    );
    assert_parse_success!(
        primary_expression,
        "[,,,...yield,,,]",
        Expression::ArrayLiteral(
            Some(((1, 1), (1, 17)).into()),
            vec![ExpressionListItem::Spread(
                Some(((1, 5), (1, 13)).into()),
                Expression::Yield {
                    argument: None,
                    delegate: false,
                },
            )]
        )
    );
}

#[test]
fn test_primary_expression_object_literal_empty() {
    assert_parse_success!(
        primary_expression,
        "{}",
        Expression::ObjectLiteral(Some(((1, 1), (1, 3)).into()), Vec::new())
    );
}

#[test]
fn test_primary_expression_object_literal_shorthand() {
    assert_parse_success!(
        primary_expression,
        "{ id }",
        Expression::ObjectLiteral(
            Some(((1, 1), (1, 7)).into()),
            vec![Property {
                kind: PropertyKind::Init,
                key: Expression::Identifier(Identifier(
                    Some(((1, 3), (1, 5)).into()),
                    "id".to_string(),
                )),
                value: Expression::Identifier(Identifier(
                    Some(((1, 3), (1, 5)).into()),
                    "id".to_string(),
                )),
                method: false,
                shorthand: true,
                computed: false,
                loc: Some(((1, 3), (1, 5)).into()),
            }]
        )
    );
}

#[test]
fn test_primary_expression_object_literal_multiple_properties() {
    assert_parse_success!(
        primary_expression,
        "{ id, id2 }",
        Expression::ObjectLiteral(
            Some(((1, 1), (1, 12)).into()),
            vec![
                Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier(Identifier(
                        Some(((1, 3), (1, 5)).into()),
                        "id".to_string(),
                    )),
                    value: Expression::Identifier(Identifier(
                        Some(((1, 3), (1, 5)).into()),
                        "id".to_string(),
                    )),
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 3), (1, 5)).into()),
                },
                Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier(Identifier(
                        Some(((1, 7), (1, 10)).into()),
                        "id2".to_string(),
                    )),
                    value: Expression::Identifier(Identifier(
                        Some(((1, 7), (1, 10)).into()),
                        "id2".to_string(),
                    )),
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 7), (1, 10)).into()),
                },
            ]
        )
    );
}

#[test]
fn test_primary_expression_object_literal_multiple_properties_ending_semicolon() {
    assert_parse_success!(
        primary_expression,
        "{ id, id2, }",
        Expression::ObjectLiteral(
            Some(((1, 1), (1, 13)).into()),
            vec![
                Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier(Identifier(
                        Some(((1, 3), (1, 5)).into()),
                        "id".to_string(),
                    )),
                    value: Expression::Identifier(Identifier(
                        Some(((1, 3), (1, 5)).into()),
                        "id".to_string(),
                    )),
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 3), (1, 5)).into()),
                },
                Property {
                    kind: PropertyKind::Init,
                    key: Expression::Identifier(Identifier(
                        Some(((1, 7), (1, 10)).into()),
                        "id2".to_string(),
                    )),
                    value: Expression::Identifier(Identifier(
                        Some(((1, 7), (1, 10)).into()),
                        "id2".to_string(),
                    )),
                    method: false,
                    shorthand: true,
                    computed: false,
                    loc: Some(((1, 7), (1, 10)).into()),
                },
            ]
        )
    );
}

#[test]
fn test_primary_expression_object_literal_initializer() {
    assert_parse_success!(
        primary_expression,
        "{ id: true }",
        Expression::ObjectLiteral(
            Some(((1, 1), (1, 13)).into()),
            vec![Property {
                kind: PropertyKind::Init,
                key: Expression::Identifier(Identifier(
                    Some(((1, 3), (1, 5)).into()),
                    "id".to_string(),
                )),
                value: Expression::Literal(Literal::BooleanLiteral(BooleanLiteral(
                    Some(((1, 7), (1, 11)).into()),
                    true,
                ))),
                method: false,
                shorthand: false,
                computed: false,
                loc: Some(((1, 3), (1, 11)).into()),
            }]
        )
    );
}

#[test]
fn test_object_literal_initializer_string_literal() {
    assert_parse_success!(
        primary_expression,
        "{ 'id': true }",
        Ok((
            build_ast!(object [
                [[str "id".to_string()]: [true]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_initializer_numeric_literal() {
    assert_parse_success!(
        primary_expression,
        "{ 0: true }",
        Ok((
            build_ast!(object [
                [[num 0f64]: [true]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_initializer_computed() {
    assert_parse_success!(
        primary_expression,
        "{ [yield]: true }",
        Ok((
            build_ast!(object [
                [[yield]: [true]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition() {
    assert_parse_success!(
        primary_expression,
        "{ method() {  } }",
        Ok((
            build_ast!(object [
                [[id "method".to_string()]: [function [] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition_generator() {
    assert_parse_success!(
        primary_expression,
        "{ * method() {  } }",
        Ok((
            build_ast!(object [
                [[id "method".to_string()]: [function * [] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition_async() {
    assert_parse_success!(
        primary_expression,
        "{ async method() {  } }",
        Ok((
            build_ast!(object [
                [[id "method".to_string()]: [async function [] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition_async_generator() {
    assert_parse_success!(
        primary_expression,
        "{ async * method() {  } }",
        Ok((
            build_ast!(object [
                [[id "method".to_string()]: [async function * [] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition_getter() {
    assert_parse_success!(
        primary_expression,
        "{ get key() {  } }",
        Ok((
            build_ast!(object [
                [get [id "key".to_string()] [function [] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_method_definition_setter() {
    assert_parse_success!(
        primary_expression,
        "{ set key(value) {  } }",
        Ok((
            build_ast!(object [
                [set [id "key".to_string()] [function [[p_id "value".to_string()]] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_primary_expression_jsx_self_closing() {
    assert_parse_success!(
        primary_expression,
        "<div/>",
        Expression::JsxElement {
            attributes: Vec::new(),
            children: Vec::new(),
            name: "div".to_string(),
            loc: Some(((1, 1), (1, 7)).into())
        }
    );
}

#[test]
fn test_primary_expression_jsx_opening_closing_match() {
    assert_parse_success!(
        primary_expression,
        "<div>\n\n</div>",
        Expression::JsxElement {
            attributes: Vec::new(),
            children: Vec::new(),
            name: "div".to_string(),
            loc: Some(((1, 1), (3, 7)).into())
        }
    );
    assert_parse_failure!(primary_expression, "<div>\n\n</v>");
}
*/
