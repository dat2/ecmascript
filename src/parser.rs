//! This module contains a main entry point, that takes a str slice and returns you
//! a constructed Abstract Syntax Tree. The AST types are documented in the ast module.
//!
//! This parser uses the error type from failure to make error interop easier for users.

use ast::*;
use combine::error::{ParseError, StreamError};
use combine::parser::char::{char, crlf, digit, hex_digit, newline, spaces, string};
use combine::parser::choice::{choice, optional};
use combine::parser::combinator::{attempt, not_followed_by};
use combine::parser::error::unexpected;
use combine::parser::item::{none_of, one_of, position, satisfy, token, value};
use combine::parser::repeat::{count, count_min_max, many, many1, sep_by, sep_end_by, skip_until};
use combine::parser::sequence::between;
use combine::stream::state::{SourcePosition, State};
use combine::stream::{Stream, StreamErrorFor};
use combine::{eof, Parser};
use failure::{self, Error};
use std::collections::HashSet;
use unicode_xid::UnicodeXID;

impl From<SourcePosition> for Position {
    fn from(source_position: SourcePosition) -> Self {
        Position {
            line: source_position.line as usize,
            column: source_position.column as usize - 1,
        }
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-lexical-grammar

// This parser will consume all following whitespace tokens, including line terminators.
// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-white-space)
parser! {
    fn ws[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        spaces().map(|_| ())
    }
}

// This parser will consume a single line terminator sequence token. This parser is only needed for the
// line_comment parser as it will consume up to a single line terminator token.
// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-line-terminators)
parser! {
    fn line_terminator[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(newline()),
            attempt(crlf()),
            attempt(char('\r')),
            attempt(char('\u{2028}')),
            attempt(char('\u{2029}'))
        )).map(|_| ())
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-comments
parser! {
    pub fn comment[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        attempt(block_comment()).or(line_comment())
    }
}

// This parses a multiline comment, starting with /* and ending with */.
// It will consume the input and return ().
parser! {
    fn block_comment[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (string("/*"), skip_until(attempt(string("*/"))), string("*/")).map(|_| ())
    }
}

// This parses
parser! {
    fn line_comment[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            string("//"),
            skip_until(line_terminator()),
            line_terminator(),
        )
            .map(|_| ())
    }
}

parser! {
    fn skip_tokens[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (ws(), optional(attempt(comment())), ws()).map(|_| ())
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
fn satisfy_id_start(c: char) -> bool {
    UnicodeXID::is_xid_start(c) || c == '$' || c == '_'
}

parser! {
    fn id_start[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        satisfy(satisfy_id_start)
    }
}

parser! {
    fn unicode_id_start[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        attempt(unicode_escape_sequence().map(|x| x.0).then(|c| {
            if satisfy_id_start(c) {
                value(c).left()
            } else {
                unexpected(c).map(|_| ' ').right()
            }
        })).or(id_start())
    }
}

fn satisfy_id_continue(c: char) -> bool {
    // 200c = ZWNJ, 200d = ZWJ
    UnicodeXID::is_xid_continue(c) || c == '\u{200C}' || c == '\u{200D}' || c == '$' || c == '_'
}

parser! {
    fn id_continue[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        satisfy(satisfy_id_continue)
    }
}

parser! {
    fn unicode_id_continue[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        attempt(unicode_escape_sequence().map(|x| x.0).then(|c| {
            if satisfy_id_continue(c) {
                value(c).left()
            } else {
                unexpected(c).map(|_| ' ').right()
            }
        })).or(id_continue())
    }
}

// TODO strict mode
parser! {
    pub fn identifier[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (unicode_id_start(), many(unicode_id_continue()))
            .map(|(s, c): (char, String)| s.to_string() + &c)
            .then(|id| {
                if KEYWORDS.contains::<str>(&id)
                    || FUTURE_RESERVED_WORDS.contains::<str>(&id)
                    || FUTURE_RESERVED_WORDS_STRICT.contains::<str>(&id)
                    || id == "null"
                    || id == "true"
                    || id == "false"
                {
                    unexpected("reserved word")
                        .map(|_| String::new())
                        .message("reserved word")
                        .right()
                } else {
                    value(id).left()
                }
            })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-reserved-words
lazy_static! {
    pub(crate) static ref KEYWORDS: HashSet<&'static str> = {
        [
            "await",
            "break",
            "case",
            "catch",
            "class",
            "const",
            "continue",
            "debugger",
            "default",
            "delete",
            "do",
            "else",
            "export",
            "extends",
            "finally",
            "for",
            "function",
            "if",
            "import",
            "in",
            "instanceof",
            "new",
            "return",
            "super",
            "switch",
            "this",
            "throw",
            "try",
            "typeof",
            "var",
            "void",
            "while",
            "with",
            "yield",
        ]
        .iter()
        .cloned()
        .collect()
    };
    pub(crate) static ref FUTURE_RESERVED_WORDS: HashSet<&'static str> =
        { ["enum"].iter().cloned().collect() };
    pub(crate) static ref FUTURE_RESERVED_WORDS_STRICT: HashSet<&'static str> = {
        [
            "implements",
            "package",
            "protected",
            "interface",
            "public",
            "private",
        ]
        .iter()
        .cloned()
        .collect()
    };
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-null-literals
parser! {
    pub fn null_literal[I]()(I) -> NullLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        string("null").map(|_| NullLiteral)
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-boolean-literals
parser! {
    pub fn boolean_literal[I]()(I) -> BooleanLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (choice((
            attempt(string("true")).map(|_| true),
            string("false").map(|_| false),
        ))).map(BooleanLiteral)
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-numeric-literals
parser! {
    pub fn numeric_literal[I]()(I) -> NumericLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (choice((
            attempt(binary_integer_literal()),
            attempt(octal_integer_literal()),
            attempt(hex_integer_literal()),
            decimal_literal(),
        ))).map(NumericLiteral)
    }
}

parser! {
    fn decimal_literal[I]()(I) -> f64
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            optional(decimal_integer_literal()),
            optional(
                (token('.'), many::<String, _>(digit()))
                    .map(|(c, s): (char, String)| c.to_string() + &s),
            ),
            optional(exponent_part()),
        )
            .then(|tuple| match tuple {
                (None, None, None) => unexpected("empty").map(|_| String::new()).left(),
                (literal_opt, digits_opt, exponent_opt) => value(
                    literal_opt.unwrap_or_else(String::new)
                        + &digits_opt.unwrap_or_else(String::new)
                        + &exponent_opt.unwrap_or_else(String::new),
                ).right(),
            }).map(|s| s.parse::<f64>().unwrap())
    }
}

parser! {
    fn decimal_integer_literal[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            string("0").skip(not_followed_by(digit())).map(String::from),
            (one_of("123456789".chars()), many::<String, _>(digit()))
                .map(|(c, s): (char, String)| c.to_string() + &s),
        ))
    }
}

parser! {
    fn exponent_part[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('e').or(token('E')),
            optional(token('-').or(token('+'))),
            many1::<String, _>(digit()),
        )
            .map(
                |(e, sign_opt, digits): (char, Option<char>, String)| match sign_opt {
                    Some(sign) => e.to_string() + &sign.to_string() + &digits,
                    None => e.to_string() + &digits,
                },
            )
    }
}

parser! {
    fn binary_integer_literal[I]()(I) -> f64
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('0'),
            token('b').or(token('B')),
            many1::<String, _>(one_of("01".chars())),
        )
            .map(|(_, _, digits)| i64::from_str_radix(&digits, 2).unwrap() as f64)
    }
}

parser! {
    fn octal_integer_literal[I]()(I) -> f64
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('0'),
            token('o').or(token('O')),
            many1::<String, _>(one_of("01234567".chars())),
        )
            .map(|(_, _, digits)| i64::from_str_radix(&digits, 8).unwrap() as f64)
    }
}

parser! {
    fn hex_integer_literal[I]()(I) -> f64
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('0'),
            token('x').or(token('X')),
            many1::<String, _>(hex_digit()),
        )
            .map(|(_, _, digits)| i64::from_str_radix(&digits, 16).unwrap() as f64)
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-string-literals
parser! {
    fn string_literal[I]()(I) -> StringLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (attempt(double_quote_string()).or(single_quote_string())).map(StringLiteral)
    }
}

parser! {
    fn double_quote_string[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('"'),
            token('"'),
            many(double_quote_string_character())
        ).map(|chars_optional: Vec<Option<char>>| chars_optional.iter().flat_map(|c| c).collect())
    }
}

parser! {
    fn double_quote_string_character[I]()(I) -> Option<char>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
        choice((
            attempt(line_continuation()).map(|_| None),
            attempt(escape_sequence()).map(|x| x.0).map(Some),
            none_of("\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}\"".chars()).map(Some)
        ))
    }
}

parser! {
    fn single_quote_string[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('\''),
            token('\''),
            many(single_quote_string_character())
        ).map(|chars_optional: Vec<Option<char>>| chars_optional.iter().flat_map(|c| c).collect())
    }
}

parser! {
    fn single_quote_string_character[I]()(I) -> Option<char>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
        choice((
            attempt(line_continuation()).map(|_| None),
            attempt(escape_sequence()).map(|x| x.0).map(Some),
            none_of("\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}'".chars()).map(Some)
        ))
    }
}

// (char, String) is "cooked" and "raw"
// this is for template elements, to be able to get access to the raw string
// this makes things uglier, but oh well
parser! {
    fn escape_sequence[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(single_escape_character()),
            attempt(non_escape_character()),
            attempt(legacy_octal_escape_sequence()),
            attempt(hex_escape_sequence()),
            attempt(unicode_escape_sequence()),
        ))
    }
}

parser! {
    fn single_escape_character[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        token('\\')
            .and(one_of(r#"'"\bfnrtv"#.chars()))
            .map(|(t, c)| {
                let cooked = match c {
                    'b' => '\u{8}',
                    'f' => '\u{C}',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'v' => '\u{B}',
                    '0' => '\u{0}',
                    other => other,
                };
                (cooked, format!("{}{}", t, c,))
            })
    }
}

parser! {
    fn legacy_octal_escape_sequence[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        token('\\')
            .and(
                choice((
                    attempt(legacy_octal_escape_sequence_single_digit()),
                    attempt(legacy_octal_escape_sequence_two_digits_zero_to_three()),
                    attempt(legacy_octal_escape_sequence_two_digits_four_to_seven()),
                    attempt(legacy_octal_escape_sequence_three_digits()),
                ))
            )
            .map(|(escape, (cooked, raw)): (char, (char, String))| (cooked, escape.to_string() + &raw))
    }
}

parser! {
    fn octal_digit[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        one_of("01234567".chars())
    }
}

parser! {
    fn zero_to_three[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        one_of("0123".chars())
    }
}

parser! {
    fn four_to_seven[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        one_of("4567".chars())
    }
}

fn octal_digit_to_u8(digit: char) -> u8 {
    digit as u8 - '0' as u8
}

parser! {
    fn legacy_octal_escape_sequence_single_digit[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        octal_digit()
            .skip(not_followed_by(octal_digit()))
            .map(|c| {
                (octal_digit_to_u8(c) as char, c.to_string())
            })
    }
}

parser! {
    fn legacy_octal_escape_sequence_two_digits_zero_to_three[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (zero_to_three(), octal_digit())
            .skip(not_followed_by(octal_digit()))
            .map(|(c1, c2)| {
                (
                    (octal_digit_to_u8(c1) * 8 + octal_digit_to_u8(c2)) as char,
                    c1.to_string() + &c2.to_string()
                )
            })
    }
}

parser! {
    fn legacy_octal_escape_sequence_two_digits_four_to_seven[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (four_to_seven(), octal_digit())
            .map(|(c1, c2)| {
                (
                    (octal_digit_to_u8(c1) * 8 + octal_digit_to_u8(c2)) as char,
                    c1.to_string() + &c2.to_string()
                )
            })
    }
}

parser! {
    fn legacy_octal_escape_sequence_three_digits[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (zero_to_three(), octal_digit(), octal_digit())
            .map(|(c1, c2, c3)| {
                (
                    (octal_digit_to_u8(c1) * 64 + octal_digit_to_u8(c2) * 8 + octal_digit_to_u8(c3)) as char,
                    c1.to_string() + &c2.to_string() + &c3.to_string()
                )
            })
    }
}

parser! {
    fn non_escape_character[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        token('\\')
            .and(none_of(
                "'\"\\bfnrtv0123456789xu\r\n\u{2028}\u{2029}".chars(),
            )).map(|(t, c)| (c, format!("{}{}", t, c)))
    }
}

parser! {
    fn hex_escape_sequence[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (token('\\'), token('x'), count_min_max::<String, _>(2, 2, hex_digit())).map(|(t, x, hex_digits)| {
            let code_point = u32::from_str_radix(&hex_digits, 16).unwrap();
            let cooked = ::std::char::from_u32(code_point).unwrap();
            (cooked, format!("{}{}{}", t, x, hex_digits))
        })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#prod-UnicodeEscapeSequence
parser! {
    fn unicode_escape_sequence[I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('\\'),
            token('u'),
            choice((
                (token('{'), count::<String, _>(6, hex_digit()), token('}')).map(
                    |(s, digits, e): (char, String, char)| s.to_string() + &digits + &e.to_string(),
                ),
                count_min_max::<String, _>(4, 4, hex_digit()),
            )),
        )
            .then(|(t, u, digits_raw)| {
                let digits_cooked = if &digits_raw[0..1] == "{" {
                    &digits_raw[1..digits_raw.len() - 1]
                } else {
                    &digits_raw[..]
                };
                let code_point = u32::from_str_radix(digits_cooked, 16).unwrap();
                if code_point > 0x0010_FFFF {
                    unexpected("code point")
                        .map(|_| (' ', String::new()))
                        .message("Code point too large")
                        .right()
                } else {
                    let cooked = ::std::char::from_u32(code_point).unwrap();
                    let raw = format!("{}{}{}", t, u, digits_raw);
                    value((cooked, raw)).left()
                }
            })
    }
}

parser! {
    fn line_continuation[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (token('\\'), line_terminator())
            .map(|_| ())

    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-regular-expression-literals
parser! {
    fn regex_literal_expression[I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (position(), regex_literal(), position()).map(|(start, value, end)| Expression::Literal {
            value: Literal::RegExpLiteral(value),
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    pub fn regex_literal[I]()(I) -> RegExpLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            between(token('/'), token('/'), regex_body()),
            many::<String, _>(id_continue()),
        )
            .map(|(pattern, flags)| RegExpLiteral { pattern, flags })
    }
}

parser! {
    fn regex_body[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (regex_first_char(), many::<String, _>(regex_char())).map(|(s, s2): (String, String)| s + &s2)
    }
}

parser! {
    fn regex_first_char [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        attempt(regex_backslash_sequence())
            .or(attempt(regex_class()))
            .or(none_of("*/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
    }
}

parser! {
    fn regex_char [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        attempt(regex_backslash_sequence())
            .or(attempt(regex_class()))
            .or(none_of("/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
    }
}

parser! {
    fn regex_non_terminator [I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        none_of("\n\r\u{2028}\u{2029}".chars())
    }
}

parser! {
    fn regex_backslash_sequence [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (token('\\'), regex_non_terminator()).map(|(c, s): (char, char)| c.to_string() + &s.to_string())
    }
}

parser! {
    fn regex_class [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('['),
            many::<String, _>(
                attempt(regex_backslash_sequence()).or(none_of("]\\".chars()).map(|c: char| c.to_string())),
                ),
                token(']'),
                )
            .map(|(open, middle, end): (char, String, char)| {
                open.to_string() + &middle + &end.to_string()
            })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-template-literal-lexical-components
parser! {
    fn template_literal [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            choice((
                attempt(no_substition_template()).map(|quasi| (vec![quasi], Vec::new())),
                attempt(substitution_template(*_yield, *_await)),
            )),
            position(),
        )
            .map(
                |(start, (quasis, expressions), end)| Expression::TemplateLiteral {
                    quasis,
                    expressions,
                    loc: Some((start, end).into()),
                },
                )
    }
}

parser! {
    fn no_substition_template [I]()(I) -> TemplateElement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            between(
                token('`'),
                token('`'),
                many::<Vec<_>, _>(template_character()),
            ),
            position(),
        )
        .map(|(start, pairs, end)| {
            let cooked = pairs.iter().cloned().map(|x| x.0).collect();
            let raw = pairs.iter().cloned().map(|x| x.1).collect();
            TemplateElement {
                cooked: Some(cooked),
                raw,
                loc: Some((start, end).into()),
            }
        })
    }
}

parser! {
    fn substitution_template[I](
        _yield: bool,
        _await: bool
    )(I) -> (Vec<TemplateElement>, Vec<Expression>)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (template_head(), assignment_expression(*_yield, *_await), template_spans(*_yield, *_await)).map(
            |(quasi, expression, (mut quasis_span, mut expressions_span))| {
                let mut quasis = vec![quasi];
                quasis.append(&mut quasis_span);
                let mut expressions = vec![expression];
                expressions.append(&mut expressions_span);
                (quasis, expressions)
            },
        )
    }
}

parser! {
    fn template_head [I]()(I) -> TemplateElement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            between(
                token('`'),
                string("${"),
                many::<Vec<_>, _>(template_character()),
                ),
                position(),
                )
            .map(|(start, pairs, end)| {
                let cooked = pairs.iter().cloned().map(|x| x.0).collect();
                let raw = pairs.iter().cloned().map(|x| x.1).collect();
                TemplateElement {
                    cooked: Some(cooked),
                    raw,
                    loc: Some((start, end).into()),
                }
            })
    }
}

parser! {
    fn template_spans[I](
        _yield: bool,
        _await: bool
    )(I) -> (Vec<TemplateElement>, Vec<Expression>)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            many::<Vec<_>, _>(attempt((template_middle(), assignment_expression(*_yield, *_await)))),
            template_tail(),
        )
            .map(|(quasis_expressions, tail)| {
                let mut quasis = Vec::new();
                let mut expressions = Vec::new();
                for (quasi, expression) in quasis_expressions {
                    quasis.push(quasi);
                    expressions.push(expression);
                }
                quasis.push(tail);
                (quasis, expressions)
            })
    }
}

parser! {
    pub fn template_character [I]()(I) -> (char, String)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(token('$').skip(not_followed_by(token('{')))).map(|x: char| (x, x.to_string())),
            attempt(escape_sequence()),
            attempt(one_of("\r\n\u{2028}\u{2029}".chars())).map(|x: char| (x, x.to_string())),
            none_of("`\\$".chars()).map(|x: char| (x, x.to_string())),
        ))
    }
}

parser! {
    fn template_middle [I]()(I) -> TemplateElement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            between(
                token('}'),
                string("${"),
                many::<Vec<_>, _>(template_character()),
            ),
            position(),
        )
            .map(|(start, pairs, end)| {
                let cooked = pairs.iter().cloned().map(|x| x.0).collect();
                let raw = pairs.iter().cloned().map(|x| x.1).collect();
                TemplateElement {
                    cooked: Some(cooked),
                    raw,
                    loc: Some((start, end).into()),
                }
            })
    }
}

parser! {
    fn template_tail [I]()(I) -> TemplateElement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            between(
                token('}'),
                token('`'),
                many::<Vec<_>, _>(template_character()),
            ),
            position(),
        )
            .map(|(start, pairs, end)| {
                let cooked = pairs.iter().cloned().map(|x| x.0).collect();
                let raw = pairs.iter().cloned().map(|x| x.1).collect();
                TemplateElement {
                    cooked: Some(cooked),
                    raw,
                    loc: Some((start, end).into()),
                }
            })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-expressions
parser! {
    pub fn primary_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(this()),
            attempt(identifier_expression()),
            attempt(literal()),
            attempt(array_literal(*_yield, *_await)),
            attempt(object_literal(*_yield, *_await)),
            attempt(regex_literal_expression()),
            attempt(template_literal(*_yield, *_await)),
            attempt(jsx_element(*_yield, *_await)),
        ))
    }
}

parser! {
    fn this [I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (position(), string("this"), position()).map(|(start, _, end)| Expression::ThisExpression {
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn identifier_expression [I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (position(), identifier(), position()).map(|(start, name, end)| Expression::Identifier {
            name,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn literal [I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            choice((
                    attempt(null_literal()).map(Literal::NullLiteral),
                    attempt(boolean_literal()).map(Literal::BooleanLiteral),
                    attempt(numeric_literal()).map(Literal::NumericLiteral),
                    attempt(string_literal()).map(Literal::StringLiteral),
                    )),
                    position(),
                    )
            .map(|(start, value, end)| Expression::Literal {
                value,
                loc: Some((start, end).into()),
            })
    }
}

parser! {
    fn array_literal [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            token('[').skip(skip_tokens()),
            optional(attempt(elision())),
            element_list(*_yield, *_await),
            optional(attempt(elision())),
            token(']').skip(skip_tokens()),
            position(),
        )
        .map(|(start, _, elision_start, array_elements, elision_end, _, end)| {
            let mut elements = vec![];
            elements.extend(elision_start.unwrap_or_else(Vec::new));
            elements.extend(array_elements);
            elements.extend(elision_end.unwrap_or_else(Vec::new));
            Expression::ArrayExpression {
                elements,
                loc: Some((start, end).into()),
            }
        })
    }
}

parser! {
    fn elision [I]()(I) -> Vec<ExpressionListItem>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        many(
            token(',')
                .skip(skip_tokens())
                .map(|_| ExpressionListItem::Null)
        )
    }
}

parser! {
    fn element_list [I](
        _yield: bool,
        _await: bool
    )(I) -> Vec<ExpressionListItem>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        sep_end_by::<Vec<_>, _, _>(
            optional(attempt(elision()))
                .and(choice((
                    attempt(assignment_expression(*_yield, *_await))
                        .map(ExpressionListItem::Expression),
                    attempt(spread_element(*_yield, *_await)),
                )))
                .skip(skip_tokens()),
            token(',').skip(skip_tokens()),
        ).map(|elements| {
            let mut result = vec![];
            for (optional_elision_elements, element) in elements {
                result.extend(optional_elision_elements.unwrap_or_else(Vec::new));
                result.push(element);
            }
            result
        })
    }
}

parser! {
    fn object_literal [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            between(
                token('{').skip(skip_tokens()),
                token('}').skip(skip_tokens()),
                sep_end_by(
                    property_definition(*_yield, *_await).skip(skip_tokens()),
                    token(',').skip(skip_tokens()),
                ),
            ),
            position(),
        )
            .map(|(start, properties, end)| Expression::ObjectExpression {
                loc: Some((start, end).into()),
                properties,
            })
    }
}

parser! {
    fn property_definition [I](
        _yield: bool,
        _await: bool
    )(I) -> ObjectExpressionProperty
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(property_initializer(*_yield, *_await)).map(ObjectExpressionProperty::Property),
            attempt(method_definition(*_yield, *_await)).map(ObjectExpressionProperty::Property),
            attempt(shorthand_property()).map(ObjectExpressionProperty::Property),
        ))
    }
}

parser! {
    fn shorthand_property [I]()(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (position(), identifier_expression(), position()).map(|(start, id, end)| Property {
            key: id.clone(),
            value: id,
            kind: PropertyKind::Init,
            method: false,
            shorthand: true,
            computed: false,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn property_initializer [I](
        _yield: bool,
        _await: bool
    )(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            property_name(*_yield, *_await),
            skip_tokens(),
            token(':'),
            skip_tokens(),
            literal(),
            position(),
        )
        .map(|(start, (key, computed), _, _, _, value, end)| Property {
            key,
            value,
            kind: PropertyKind::Init,
            method: false,
            shorthand: false,
            computed,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn property_name [I](
        _yield: bool,
        _await: bool
    )(I) -> (Expression, bool)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(literal_property_name()).map(|e| (e, false)),
            attempt(computed_property_name(*_yield, *_await)).map(|e| (e, true)),
        ))
    }
}

parser! {
    fn literal_property_name [I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            identifier_expression(),
            (position(), string_literal(), position()).map(|(start, value, end)| Expression::Literal {
                value: Literal::StringLiteral(value),
                loc: Some((start, end).into()),
            }),
            (position(), numeric_literal(), position()).map(|(start, value, end)| {
                Expression::Literal {
                    value: Literal::NumericLiteral(value),
                    loc: Some((start, end).into()),
                }
            }),
        ))
    }
}

parser! {
    fn computed_property_name [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('[').skip(skip_tokens()),
            token(']'),
            assignment_expression(*_yield, *_await),
        )
    }
}

parser! {
    fn spread_element [I](
        _yield: bool,
        _await: bool
    )(I) -> ExpressionListItem
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            string("...").with(assignment_expression(*_yield, *_await)),
            position(),
        )
            .map(|(start, expression, end)| {
                ExpressionListItem::Spread(SpreadElement::SpreadElement {
                    argument: expression,
                    loc: Some((start, end).into()),
                })
            })
    }
}

parser! {
    fn assignment_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(assignment_expression_inner_equal(*_yield, *_await)),
            attempt(assignment_expression_inner_operators(*_yield, *_await)),
            attempt(conditional_expression(*_yield, *_await)),
            attempt(
                if *_yield {
                    yield_expression(*_await).left()
                } else {
                    unexpected("message")
                        .map(|_| Expression::ThisExpression { loc: None })
                        .right()
                }
            ),
        ))
    }
}

parser! {
    fn conditional_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(primary_expression(*_yield, *_await)),
            attempt(conditional_expression_inner(*_yield, *_await)),
        ))
    }
}

parser! {
    fn conditional_expression_inner[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            primary_expression(*_yield, *_await).skip(skip_tokens()),
            token('?').skip(skip_tokens()),
            assignment_expression(*_yield, *_await).skip(skip_tokens()),
            token(':').skip(skip_tokens()),
            assignment_expression(*_yield, *_await).skip(skip_tokens()),
            position(),
        )
            .map(|(start, test, _, consequent, _, alternate, end)| {
                Expression::ConditionalExpression {
                    test: Box::new(test),
                    consequent: Box::new(consequent),
                    alternate: Box::new(alternate),
                    loc: Some((start, end).into()),
                }
            })
    }
}

parser! {
    fn yield_expression [I](
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            string("yield").skip(skip_tokens()),
            optional(token('*').skip(skip_tokens())),
            optional(attempt(assignment_expression(true, *_await).skip(skip_tokens()))),
        )
        .map(|(_, delegate_token, argument)| Expression::Yield {
            argument: argument.map(Box::new),
            delegate: delegate_token.is_some(),
        })
    }
}

parser! {
    fn assignment_expression_inner_equal[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [
        I: Stream<Item=char, Position=SourcePosition>,
        I::Error: ParseError<char, I::Range, I::Position>
    ]
    {
        (
            left_hand_side_expression(*_yield, *_await).skip(skip_tokens()),
            token('=').skip(skip_tokens()),
            assignment_expression(*_yield, *_await)
        ).and_then(|(left, _, right)| {
            left.into_pattern()
                .map_err(|err| StreamErrorFor::<I>::unexpected_message(err.to_string()))
                .map(|left| Expression::AssignmentExpression {
                    operator: AssignmentOperator::Eq,
                    left: Box::new(left),
                    right: Box::new(right)
                })
        })
    }
}

parser! {
    fn assignment_expression_inner_operators[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        assignment_expression_inner_equal(*_yield, *_await)
    }
}

parser! {
    fn left_hand_side_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        new_expression(*_yield, *_await)
    }
}

parser! {
    fn new_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        member_expression(*_yield, *_await)
    }
}

parser! {
    fn member_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        primary_expression(*_yield, *_await)
    }
}

// https://facebook.github.io/jsx/
parser! {
    fn jsx_element [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((attempt(jsx_self_closing_element(*_yield, *_await)), jsx_matched_element(*_yield, *_await)))
    }
}

parser! {
    fn jsx_element_name [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(jsx_namespaced_name()),
            identifier()
        ))
    }
}

parser! {
    fn jsx_namespaced_name[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            identifier(),
            string(":"),
            identifier()
        ).map(|(namespace, token, id)| namespace + &token+ &id)
    }
}

parser! {
    fn jsx_self_closing_element [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            token('<'),
            jsx_element_name().skip(skip_tokens()),
            many(jsx_attribute(*_yield, *_await)),
            string("/>"),
            position(),
        )
        .map(
            |(start, _, name, attributes, _, end)| Expression::JSXElement {
                name,
                attributes,
                children: Vec::new(),
                loc: Some((start, end).into()),

            }
        )
    }
}

parser! {
    fn jsx_opening_element[I](
        _yield: bool,
        _await: bool
    )(I) -> (String, Vec<JsxAttribute>)
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('<'),
            jsx_element_name().skip(skip_tokens()),
            many(jsx_attribute(*_yield, *_await)),
            token('>').skip(skip_tokens()),
        )
            .map(|(_, name, attributes, _)| (name, attributes))
    }
}

parser! {
    fn jsx_attribute [I](
        _yield: bool,
        _await: bool
    )(I) -> JsxAttribute
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((attempt(jsx_spread_attribute(*_yield, *_await)), attempt(jsx_attribute_key_value(*_yield, *_await))))
    }
}

parser! {
    fn jsx_spread_attribute [I](
        _yield: bool,
        _await: bool
    )(I) -> JsxAttribute
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('{').skip(skip_tokens()),
            token('}').skip(skip_tokens()),
            string("...").skip(skip_tokens()).with(assignment_expression(*_yield, *_await)),
        ).map(|expression| JsxAttribute::JsxSpreadAttribute { expression })
    }
}

parser! {
    fn jsx_attribute_key_value [I](
        _yield: bool,
        _await: bool
    )(I) -> JsxAttribute
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            jsx_attribute_name().skip(skip_tokens()),
            optional(attempt(jsx_attribute_initializer(*_yield, *_await))),
        )
        .map(|(name, value)| JsxAttribute::JsxAttribute { name, value })
    }
}

parser! {
    fn jsx_attribute_name [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        identifier()
    }
}

parser! {
    fn jsx_attribute_initializer[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        token('=').with(jsx_attribute_value(*_yield, *_await))
    }
}

parser! {
    fn jsx_attribute_value[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(jsx_attribute_value_string()),
            attempt(jsx_attribute_value_expression(*_yield, *_await)),
            attempt(jsx_element(*_yield, *_await)),
        )).skip(skip_tokens())
    }
}

parser! {
    fn jsx_attribute_value_string [I]()(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            string_literal().map(Literal::StringLiteral),
            position(),
        )
        .map(|(start, value, end)| Expression::Literal {
            value,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn jsx_attribute_value_expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('{').skip(skip_tokens()),
            token('}'),
            assignment_expression(*_yield, *_await),
        )
    }
}

parser! {
    fn jsx_closing_element [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(string("</"), token('>'), jsx_element_name())
    }
}

parser! {
    fn jsx_matched_element [I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            jsx_opening_element(*_yield, *_await),
            jsx_closing_element(),
            position(),
        )
        .then(|(start, (opening_name, attributes), closing_name, end)| {
            if opening_name == closing_name {
                value(Expression::JSXElement {
                    name: opening_name,
                    attributes,
                    children: Vec::new(),
                    loc: Some((start, end).into()),
                }).left()
            } else {
                unexpected("closing element")
                    .map(|_| Expression::JSXElement {
                        name: String::new(),
                        attributes: Vec::new(),
                        children: Vec::new(),
                        loc: None,
                    }).message("closing name is not the same as opening name")
                .right()
            }
        })
    }
}

parser! {
    fn expression[I](
        _yield: bool,
        _await: bool
    )(I) -> Expression
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            assignment_expression(*_yield, *_await),
            expression_inner(*_yield, *_await),
        ).map(|(lhs, optional_rhs)| {
            match optional_rhs {
                Some(Expression::SequenceExpression { expressions }) => {
                    let mut rest = vec![lhs];
                    rest.extend(expressions);
                    Expression::SequenceExpression {
                        expressions: rest
                    }
                },
                Some(rhs) => {
                    Expression::SequenceExpression {
                        expressions: vec![lhs, rhs]
                    }
                },
                None => lhs
            }
        })
    }
}

parser! {
    fn expression_inner[I](
        _yield: bool,
        _await: bool
    )(I) -> Option<Expression>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        optional(attempt((
            token(',').skip(skip_tokens()),
            assignment_expression(*_yield, *_await),
            expression_inner(*_yield, *_await)
        ).map(|(_, rhs, rest_optional)| match rest_optional {
            Some(Expression::SequenceExpression { expressions }) => {
                let mut rest = vec![rhs];
                rest.extend(expressions);
                Expression::SequenceExpression {
                    expressions: rest
                }
            },
            Some(expr) => {
                Expression::SequenceExpression {
                    expressions: vec![rhs, expr]
                }
            },
            None => rhs
        })))
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-statements-and-declarations
parser! {
    fn statement [I](
        _yield: bool,
        _await: bool,
        _return: bool
    )(I) -> Statement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            variable_statement(*_yield, *_await),
            expression_statement(*_yield, *_await)
        ))
    }
}

parser! {
    fn variable_statement[I](
        _yield: bool,
        _await: bool
    )(I) -> Statement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            string("var").skip(skip_tokens()),
            variable_declaration_list(*_yield, *_await),
            optional(attempt(token(';').skip(skip_tokens()))),
            position(),
        ).map(|(start, _, declarations, _, end)| Statement::VariableDeclaration {
            kind: VariableDeclarationKind::Var,
            declarations,
            loc: Some((start, end).into())
        })
    }
}

parser! {
    fn variable_declaration_list[I](
        _yield: bool,
        _await: bool
    )(I) -> Vec<VariableDeclarator>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        sep_by(
            variable_declaration(*_yield, *_await),
            token(',').skip(skip_tokens())
        )
    }
}

parser! {
    fn variable_declaration[I](
        _yield: bool,
        _await: bool
    )(I) -> VariableDeclarator
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        variable_declaration_identifier(*_yield, *_await)
    }
}

parser! {
    fn variable_declaration_identifier[I](
        _yield: bool,
        _await: bool
    )(I) -> VariableDeclarator
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            binding_identifier().skip(skip_tokens()),
            optional(attempt(
                token('=')
                    .skip(skip_tokens())
                    .with(assignment_expression(*_yield, *_await)
            )))
        ).map(|(id, init)| VariableDeclarator {
            id,
            init: init
                .map(VariableDeclaratorInit::Expression)
                .unwrap_or(VariableDeclaratorInit::Null)
        })
    }
}

parser! {
    fn binding_identifier[I]()(I) -> Pattern
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            choice((
                identifier(),
                string("yield").map(|s| s.to_owned()),
                string("await").map(|s| s.to_owned())
            )),
            position(),
        ).map(|(start, name, end)| Pattern::Identifier {
            name,
            loc: Some((start, end).into())
        })
    }
}

parser! {
    fn expression_statement [I](
        _yield: bool,
        _await: bool
    )(I) -> Statement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            expression(*_yield, *_await),
            position()
        ).map(|(start, expression, end)| {
            Statement::ExpressionStatement {
                loc: Some((start, end).into()),
                expression,
            }
        })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-functions-and-classes
parser! {
    fn formal_parameters [I]()(I) -> Vec<Pattern>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('(').skip(skip_tokens()),
            token(')'),
            value(Vec::new()),
        )
    }
}

parser! {
    fn formal_parameter [I]()(I) -> Pattern
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (position(), identifier(), position()).map(|(start, name, end)| Pattern::Identifier {
            name,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn function_body[I](
        _yield: bool,
        _await: bool
    )(I) -> Vec<FunctionBodyStatement>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('{').skip(skip_tokens()),
            token('}'),
            value(Vec::new()),
        )
    }
}

parser! {
    fn method_definition [I](
        _yield: bool,
        _await: bool
    )(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        choice((
            attempt(getter_method_definition(*_yield, *_await)),
            attempt(setter_method_definition(*_yield, *_await)),
            attempt(generator_method_definition()),
            attempt(async_generator_method_definition()),
            attempt(async_method_definition()),
            basic_method_definition(false, false),
        ))
    }
}

parser! {
    fn basic_method_definition[I](
        _yield: bool,
        _await: bool
    )(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            property_name(*_yield, *_await),
            skip_tokens(),
            formal_parameters(),
            skip_tokens(),
            function_body(*_yield, *_await),
            position(),
        )
        .map(
            |(start, (key, computed), _, params, _, body, end)| Property {
                key,
                value: Expression::FunctionExpression {
                    id: None,
                    async: *_await,
                    generator: *_yield,
                    body,
                    params,
                },
                kind: PropertyKind::Init,
                method: true,
                shorthand: false,
                computed,
                loc: Some((start, end).into()),
            },
        )
    }
}

parser! {
    fn generator_method_definition [I]()(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            token('*'),
            skip_tokens(),
            basic_method_definition(true, false),
        )
        .map(|x| x.2)
    }
}

parser! {
    fn async_method_definition [I]()(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            string("async"),
            skip_tokens(),
            basic_method_definition(false, true),
        )
        .map(|x| x.2)
    }
}

parser! {
    fn async_generator_method_definition [I]()(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            string("async"),
            skip_tokens(),
            token('*'),
            skip_tokens(),
            basic_method_definition(true, true),
        )
            .map(|x| x.4)
    }
}

parser! {
    fn getter_method_definition [I](
        _yield: bool,
        _await: bool
    )(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            string("get").skip(skip_tokens()),
            property_name(*_yield, *_await).skip(skip_tokens()),
            token('(').skip(skip_tokens()),
            token(')').skip(skip_tokens()),
            function_body(false, false),
            position(),
        )
        .map(|(start, _, (key, computed), _, _, body, end)| Property {
            key,
            value: Expression::FunctionExpression {
                id: None,
                async: false,
                generator: false,
                body,
                params: Vec::new(),
            },
            kind: PropertyKind::Get,
            method: false,
            shorthand: false,
            computed,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn setter_method_definition [I](
        _yield: bool,
        _await: bool
    )(I) -> Property
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            string("set").skip(skip_tokens()),
            property_name(*_yield, *_await).skip(skip_tokens()),
            between(
                token('(').skip(skip_tokens()),
                token(')').skip(skip_tokens()),
                formal_parameter().skip(skip_tokens()),
            ),
            function_body(false, false),
            position(),
        )
        .map(|(start, _, (key, computed), param, body, end)| Property {
            key,
            value: Expression::FunctionExpression {
                id: None,
                async: false,
                generator: false,
                body,
                params: vec![param],
            },
            kind: PropertyKind::Set,
            method: false,
            shorthand: false,
            computed,
            loc: Some((start, end).into()),
        })
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-scripts-and-modules
parser! {
    fn program[I]()(I) -> Program
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (
            position(),
            skip_tokens(),
            many::<Vec<_>, _>(statement(false, false, false).skip(skip_tokens())),
            eof(),
            position(),
        )
            .map(|(start, _, body, _, end)| Program::Program {
                source_type: SourceType::Script,
                body,
                loc: Some((start, end).into()),
            })
    }
}

/// The main entry point to the parser. This function will return a fully constructed
/// AST or an error message describing why it couldn't parse the input string.
pub fn parse(source: &str) -> Result<Program, Error> {
    let stream = State::new(source);
    let (ast, _) = program()
        .easy_parse(stream)
        .map_err(|e| failure::err_msg(e.to_string()))?;
    Ok(ast)
}
