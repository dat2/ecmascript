//! This module contains a main entry point, that takes a str slice and returns you
//! a constructed Abstract Syntax Tree. The AST types are documented in the ast module.
//!
//! This parser uses the error type from failure to make error interop easier for users.

use ast::*;
use combine::parser::char::{char, crlf, digit, hex_digit, newline, spaces, string};
use combine::parser::choice::{choice, optional};
use combine::parser::combinator::{not_followed_by, try};
use combine::parser::error::unexpected;
use combine::parser::item::{none_of, one_of, position, satisfy, token, value};
use combine::parser::repeat::{count, many, many1, sep_end_by, skip_until};
use combine::parser::sequence::between;
use combine::stream::state::{SourcePosition, State};
use combine::stream::Stream;
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

/// This parser will consume all following whitespace tokens, including line terminators.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-white-space)
parser! {
    fn ws[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        spaces().map(|_| ())
    }
}

/// This parser will consume a single line terminator sequence token. This parser is only needed for the
/// line_comment parser as it will consume up to a single line terminator token.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-line-terminators)
parser! {
    fn line_terminator[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        newline()
            .or(char('\u{000D}'))
            .or(char('\u{2028}'))
            .or(char('\u{2029}'))
            .or(crlf())
            .map(|_| ())
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-comments
parser! {
    pub fn comment[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        try(block_comment()).or(line_comment())
    }
}

/// This parses a multiline comment, starting with /* and ending with */.
/// It will consume the input and return ().
parser! {
    fn block_comment[I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (string("/*"), skip_until(try(string("*/"))), string("*/")).map(|_| ())
    }
}

/// This parses
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
        (ws(), optional(try(comment())), ws()).map(|_| ())
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
        try(unicode_escape_sequence().map(|x| x.0).then(|c| {
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
        try(unicode_escape_sequence().map(|x| x.0).then(|c| {
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
            try(string("true")).map(|_| true),
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
            try(binary_integer_literal()),
            try(octal_integer_literal()),
            try(hex_integer_literal()),
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
    pub fn string_literal[I]()(I) -> StringLiteral
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        (try(double_quote_string()).or(single_quote_string())).map(StringLiteral)
    }
}

parser! {
    fn double_quote_string[I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        between(
            token('"'),
            token('"'),
            many(double_quote_string_character()),
        )
    }
}

parser! {
    fn double_quote_string_character[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
        escape_sequence().map(|x| x.0).or(none_of(
            "\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}\"".chars(),
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
            many(single_quote_string_character()),
        )
    }
}

parser! {
    fn single_quote_string_character[I]()(I) -> char
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
        escape_sequence()
            .map(|x| x.0)
            .or(none_of("\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}'".chars()))
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
            try(character_escape_sequence()),
            try(non_escape_character_sequence()),
            try(hex_escape_sequence()),
            // TODO legacy octal escape sequence
            unicode_escape_sequence(),
        ))
    }
}

parser! {
    fn character_escape_sequence[I]()(I) -> (char, String)
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
                    other => other,
                };
                (cooked, format!("{}{}", t, c,))
            })
    }
}

parser! {
    fn non_escape_character_sequence[I]()(I) -> (char, String)
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
        (token('\\'), token('x'), count::<String, _>(2, hex_digit())).map(|(t, x, hex_digits)| {
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
                count::<String, _>(4, hex_digit()),
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
        try(regex_backslash_sequence())
            .or(try(regex_class()))
            .or(none_of("*/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
    }
}

parser! {
    fn regex_char [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        try(regex_backslash_sequence())
            .or(try(regex_class()))
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
                try(regex_backslash_sequence()).or(none_of("]\\".chars()).map(|c: char| c.to_string())),
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
                try(no_substition_template()).map(|quasi| (vec![quasi], Vec::new())),
                try(substitution_template(*_yield, *_await)),
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
            many::<Vec<_>, _>(try((template_middle(), assignment_expression(*_yield, *_await)))),
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
            try(token('$').skip(not_followed_by(token('{')))).map(|x: char| (x, x.to_string())),
            try(escape_sequence()),
            try(one_of("\r\n\u{2028}\u{2029}".chars())).map(|x: char| (x, x.to_string())),
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
            try(this()),
            try(identifier_expression()),
            try(literal()),
            try(array_literal(*_yield, *_await)),
            try(object_literal(*_yield, *_await)),
            try(regex_literal_expression()),
            try(template_literal(*_yield, *_await)),
            try(jsx_element(*_yield, *_await)),
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
                    try(null_literal()).map(Literal::NullLiteral),
                    try(boolean_literal()).map(Literal::BooleanLiteral),
                    try(numeric_literal()).map(Literal::NumericLiteral),
                    try(string_literal()).map(Literal::StringLiteral),
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
            between(
                token('[').skip(skip_tokens()),
                token(']').skip(skip_tokens()),
                elision().with(element_list(*_yield, *_await)).skip(elision()),
            ),
            position(),
        )
        .map(|(start, elements, end)| Expression::ArrayExpression {
            elements,
            loc: Some((start, end).into()),
        })
    }
}

parser! {
    fn elision [I]()(I) -> ()
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        many::<Vec<_>, _>(token(',')).with(skip_tokens())
    }
}

parser! {
    fn element_list [I](
        _yield: bool,
        _await: bool
    )(I) -> Vec<ExpressionListItem>
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        many(
            choice((
                try(assignment_expression(*_yield, *_await)).map(ExpressionListItem::Expression),
                spread_element(*_yield, *_await),
            )).skip(elision()),
        )
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
            try(property_initializer(*_yield, *_await)).map(ObjectExpressionProperty::Property),
            try(method_definition(*_yield, *_await)).map(ObjectExpressionProperty::Property),
            try(shorthand_property()).map(ObjectExpressionProperty::Property),
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
            try(literal_property_name()).map(|e| (e, false)),
            try(computed_property_name(*_yield, *_await)).map(|e| (e, true)),
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
            try(conditional_expression(*_yield, *_await)),
            try(yield_expression(*_await))
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
            assignment_expression(true, *_await).skip(skip_tokens()),
        )
        .map(|(_, delegate_token, argument)| Expression::Yield {
            argument: Some(Box::new(argument)),
            delegate: delegate_token.is_some(),
        })
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
        choice((try(jsx_self_closing_element(*_yield, *_await)), jsx_matched_element(*_yield, *_await)))
    }
}

parser! {
    fn jsx_element_name [I]()(I) -> String
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        identifier()
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
            |(start, _, name, attributes, _, end)| Expression::JsxElementExpression {
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
        choice((try(jsx_spread_attribute(*_yield, *_await)), try(jsx_attribute_key_value(*_yield, *_await))))
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
            optional(try(jsx_attribute_initializer(*_yield, *_await))),
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
            try(jsx_attribute_value_string()),
            try(jsx_attribute_value_expression(*_yield, *_await)),
            try(jsx_element(*_yield, *_await)),
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
                value(Expression::JsxElementExpression {
                    name: opening_name,
                    attributes,
                    children: Vec::new(),
                    loc: Some((start, end).into()),
                }).left()
            } else {
                unexpected("closing element")
                    .map(|_| Expression::JsxElementExpression {
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

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-statements-and-declarations
parser! {
    fn statement [I]()(I) -> Statement
    where [I: Stream<Item=char, Position=SourcePosition>]
    {
        // TODO use assignment_expression instead
        (position(), primary_expression(true, true), position()).map(|(start, expression, end)| {
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
            try(getter_method_definition(*_yield, *_await)),
            try(setter_method_definition(*_yield, *_await)),
            try(generator_method_definition()),
            try(async_generator_method_definition()),
            try(async_method_definition()),
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
            many::<Vec<_>, _>(statement().skip(skip_tokens())),
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
