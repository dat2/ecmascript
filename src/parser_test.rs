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
}

macro_rules! assert_parse_failure {
    ($parser:ident, $input:expr) => {
        assert!($parser().easy_parse(State::new($input)).is_err());
    };
}

#[test]
fn test_line_comment() {
    assert_parse_success!(comment, "//\n", ());
    assert_parse_success!(comment, "// hello\n", ());
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
    assert_parse_success!(identifier, r"\u24", "$".to_string());
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
    assert_parse_success!(
        null_literal,
        "null",
        NullLiteral(Some(((1, 1), (1, 5)).into()))
    );
}

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

/*
#[test]
fn test_regex_literal_empty() {
    // must be non empty
    assert!(regex_literal().parse("//").is_err());
}

#[test]
fn test_regex_literal_start_invalid() {
    // not allowed first chars
    for c in "*\\/[".chars() {
        let slice: &str = &format!("/{}/", c);
        assert!(regex_literal().parse(slice).is_err());
    }
}

#[test]
fn test_regex_literal_start_backslash() {
    // backslash as first char
    assert_eq!(
        regex_literal().parse("/\\a/"),
        Ok((build_ast!(regex_lit /{"\\a".to_string()}/), ""))
    );
}

#[test]
fn test_regex_literal_start_character_class() {
    // character class as first char
    assert_eq!(
        regex_literal().parse("/[ab]/"),
        Ok((build_ast!(regex_lit /{"[ab]".to_string()}/), ""))
    );
}

// not allowed second chars
        /*
        for c in "\\/[".chars() {
            let slice: &str = &format!("/a{}/", c);
            assert!(regex_literal().parse(slice).is_err());
        }
        */

#[test]
fn test_regex_literal_continue_backslash() {
    // backslash as second char
    assert_eq!(
        regex_literal().parse("/a\\a/"),
        Ok((build_ast!(regex_lit /{"a\\a".to_string()}/), ""))
    );
}

#[test]
fn test_regex_literal_continue_character_class() {
    // character class as second char
    assert_eq!(
        regex_literal().parse("/a[ab]/"),
        Ok((build_ast!(regex_lit /{"a[ab]".to_string()}/), ""))
    );
}

// character class with unallowed chars
        /*
        for c in "\\/]".chars() {
            let slice: &str = &format!("/a[{}]/", c);
            assert!(regex_literal().parse(slice).is_err());
        }
        */

#[test]
fn test_regex_literal_character_class_backslash() {
    // character class with backslash
    assert_eq!(
        regex_literal().parse("/a[ab\\]]/"),
        Ok((build_ast!(regex_lit /{"a[ab\\]]".to_string()}/), ""))
    );
}

#[test]
fn test_regex_literal_flags() {
    // flags
    assert_eq!(
        regex_literal().parse("/a/f"),
        Ok((
            build_ast!(regex_lit / { "a".to_string() } / { "f".to_string() }),
            ""
        ))
    );
    assert_eq!(
        regex_literal().parse("/a/fi"),
        Ok((
            build_ast!(regex_lit / { "a".to_string() } / { "fi".to_string() }),
            ""
        ))
    );
    assert!(regex_literal().skip(eof()).parse("/a/\\u1234").is_err());
}

#[test]
fn test_template_elements() {
    // empty
    assert_eq!(
        template().parse("``"),
        Ok((build_ast!(templ_el {String::new()}), ""))
    );

    // no_substitution_template
    assert_eq!(
        template().parse("`asd`"),
        Ok((build_ast!(templ_el {"asd".to_string()}), ""))
    );

    // template_head
    assert_eq!(
        template().parse("`asd ${eval}`"),
        Ok((build_ast!(templ_el {"asd ".to_string()}), "eval}`"))
    );

    // template_middle
    assert_eq!(
        template_substition_tail().parse("} asd ${eval}`"),
        Ok((build_ast!(templ_el {" asd ".to_string()}), "eval}`"))
    );

    // template_tail
    assert_eq!(
        template_substition_tail().parse("} asd"),
        Ok((build_ast!(templ_el {" asd".to_string()}), ""))
    );

    // $
    assert_eq!(
        template_character().parse("$123"),
        Ok((('$', "$".to_string()), "123"))
    );
    // escape sequence
    assert_eq!(
        template_character().parse("\\n"),
        Ok((('\n', "\\n".to_string()), ""))
    );
    assert_eq!(
        template_character().parse("\\x0A"),
        Ok((('\n', "\\x0A".to_string()), ""))
    );
    assert_eq!(
        template_character().parse("\\u2764"),
        Ok((('❤', "\\u2764".to_string()), ""))
    );
    assert_eq!(
        template_character().parse("\\u{2764}"),
        Ok((('❤', "\\u{2764}".to_string()), ""))
    );
    // line continuation
    for line_continuation_char in "\r\n\u{2028}\u{2029}".chars() {
        let slice: &str = &line_continuation_char.to_string();
        assert_eq!(
            template_character().parse(slice),
            Ok((
                (line_continuation_char, line_continuation_char.to_string()),
                ""
            ))
        );
    }
}

#[test]
fn test_this() {
    assert_eq!(
        primary_expression().parse("this"),
        Ok((build_ast!(this), ""))
    );
}

#[test]
fn test_identifier_reference() {
    assert_eq!(
        primary_expression().parse("abc123"),
        Ok((build_ast!(id "abc123".to_string()), ""))
    );
}

#[test]
fn test_literal() {
    assert_eq!(
        primary_expression().parse("null"),
        Ok((build_ast!(null), ""))
    );
    assert_eq!(
        primary_expression().parse("true"),
        Ok((build_ast!(true), ""))
    );
    assert_eq!(
        primary_expression().parse("false"),
        Ok((build_ast!(false), ""))
    );
    assert_eq!(
        primary_expression().parse("123.e1"),
        Ok((build_ast!(num 1230f64), ""))
    );
    assert_eq!(
        primary_expression().parse("'abc'"),
        Ok((build_ast!(str "abc".to_string()), ""))
    );
}

#[test]
fn test_array_literal() {
    assert_eq!(
        primary_expression().parse("[]"),
        Ok((build_ast!(array []), ""))
    );
    assert_eq!(
        primary_expression().parse("[,,,,]"),
        Ok((build_ast!(array []), ""))
    );
    assert_eq!(
        primary_expression().parse("[,,,,yield,,yield,,,]"),
        Ok((build_ast!(array [ [yield], [yield] ]), ""))
    );
    assert_eq!(
        primary_expression().parse("[,,,...yield,,,]"),
        Ok((build_ast!(array [ [...[yield]] ]), ""))
    );
}

#[test]
fn test_object_literal_empty() {
    assert_eq!(
        primary_expression().parse("{}"),
        Ok((build_ast!(object []), ""))
    );
}

#[test]
fn test_object_literal_shorthand() {
    assert_eq!(
        primary_expression().parse("{ id }"),
        Ok((
            build_ast!(object [
                [[id "id".to_string()]: [id "id".to_string()]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_multiple_properties() {
    assert_eq!(
        primary_expression().parse("{ id, id2 }"),
        Ok((
            build_ast!(object [
                [[id "id".to_string()]: [id "id".to_string()]],
                [[id "id2".to_string()]: [id "id2".to_string()]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_multiple_properties_ending_semicolon() {
    assert_eq!(
        primary_expression().parse("{ id, id2, }"),
        Ok((
            build_ast!(object [
                [[id "id".to_string()]: [id "id".to_string()]],
                [[id "id2".to_string()]: [id "id2".to_string()]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_initializer() {
    assert_eq!(
        primary_expression().parse("{ id: true }"),
        Ok((
            build_ast!(object [
                [[id "id".to_string()]: [true]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_object_literal_initializer_string_literal() {
    assert_eq!(
        primary_expression().parse("{ 'id': true }"),
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
    assert_eq!(
        primary_expression().parse("{ 0: true }"),
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
    assert_eq!(
        primary_expression().parse("{ [yield]: true }"),
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
    assert_eq!(
        primary_expression().parse("{ method() {  } }"),
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
    assert_eq!(
        primary_expression().parse("{ * method() {  } }"),
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
    assert_eq!(
        primary_expression().parse("{ async method() {  } }"),
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
    assert_eq!(
        primary_expression().parse("{ async * method() {  } }"),
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
    assert_eq!(
        primary_expression().parse("{ get key() {  } }"),
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
    assert_eq!(
        primary_expression().parse("{ set key(value) {  } }"),
        Ok((
            build_ast!(object [
                [set [id "key".to_string()] [function [[p_id "value".to_string()]] []]]
            ]),
            ""
        ))
    );
}

#[test]
fn test_jsx_self_closing() {
    assert_eq!(
        primary_expression().parse("<div/>"),
        Ok((build_ast!(<div />), ""))
    );
}

#[test]
fn test_jsx_opening_closing_match() {
    assert_eq!(
        primary_expression().parse("<div>\n\n</div>"),
        Ok((build_ast!(<div />), ""))
    );
    assert!(primary_expression().parse("<div>\n\n</v>").is_err());
}
*/
