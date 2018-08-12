//! This module contains a main entry point, that takes a str slice and returns you
//! a constructed Abstract Syntax Tree. The AST types are documented in the ast module.
//!
//! This parser uses the error type from failure to make error interop easier for users.

use ast::*;
use combine::easy;
use combine::parser::char::{char, crlf, digit, hex_digit, newline, spaces, string};
use combine::parser::choice::{choice, optional};
use combine::parser::combinator::{not_followed_by, try};
use combine::parser::error::unexpected;
use combine::parser::item::{none_of, one_of, position, satisfy, token, value};
use combine::parser::repeat::{count, many, many1, sep_end_by, skip_until};
use combine::parser::sequence::between;
use combine::stream::state::{SourcePosition, State};
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
fn ws<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    spaces().map(|_| ())
}

/// This parser will consume a single line terminator sequence token. This parser is only needed for the
/// line_comment parser as it will consume up to a single line terminator token.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-line-terminators)
fn line_terminator<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    newline()
        .or(char('\u{000D}'))
        .or(char('\u{2028}'))
        .or(char('\u{2029}'))
        .or(crlf())
        .map(|_| ())
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-comments
pub(crate) fn comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    try(block_comment()).or(line_comment())
}

/// This parses a multiline comment, starting with /* and ending with */.
/// It will consume the input and return ().
fn block_comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    (string("/*"), skip_until(try(string("*/"))), string("*/")).map(|_| ())
}

/// This parses
fn line_comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    (
        string("//"),
        skip_until(line_terminator()),
        line_terminator(),
    )
        .map(|_| ())
}

fn skip_tokens<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    (ws(), optional(try(comment())), ws()).map(|_| ())
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
fn satisfy_id_start(c: char) -> bool {
    UnicodeXID::is_xid_start(c) || c == '$' || c == '_'
}

fn id_start<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char>
{
    satisfy(satisfy_id_start)
}

fn unicode_id_start<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    try(unicode_escape_sequence().map(|x| x.0).then(|c| {
        if satisfy_id_start(c) {
            value(c).left()
        } else {
            unexpected(c).map(|_| ' ').right()
        }
    })).or(id_start())
}

fn satisfy_id_continue(c: char) -> bool {
    // 200c = ZWNJ, 200d = ZWJ
    UnicodeXID::is_xid_continue(c) || c == '\u{200C}' || c == '\u{200D}' || c == '$' || c == '_'
}

fn id_continue<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    satisfy(satisfy_id_continue)
}

fn unicode_id_continue<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    try(unicode_escape_sequence().map(|x| x.0).then(|c| {
        if satisfy_id_continue(c) {
            value(c).left()
        } else {
            unexpected(c).map(|_| ' ').right()
        }
    })).or(id_continue())
}

// TODO strict mode
pub(crate) fn identifier<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
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
pub(crate) fn null_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = NullLiteral> {
    string("null").map(|_| NullLiteral)
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-boolean-literals
pub(crate) fn boolean_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = BooleanLiteral> {
    (choice((
        try(string("true")).map(|_| true),
        string("false").map(|_| false),
    ))).map(BooleanLiteral)
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-numeric-literals
pub(crate) fn numeric_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = NumericLiteral> {
    (choice((
        try(binary_integer_literal()),
        try(octal_integer_literal()),
        try(hex_integer_literal()),
        decimal_literal(),
    ))).map(NumericLiteral)
}

fn decimal_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
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

fn decimal_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    choice((
        string("0").skip(not_followed_by(digit())).map(String::from),
        (one_of("123456789".chars()), many::<String, _>(digit()))
            .map(|(c, s): (char, String)| c.to_string() + &s),
    ))
}

fn exponent_part<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
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

fn binary_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('b').or(token('B')),
        many1::<String, _>(one_of("01".chars())),
    )
        .map(|(_, _, digits)| i64::from_str_radix(&digits, 2).unwrap() as f64)
}

fn octal_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('o').or(token('O')),
        many1::<String, _>(one_of("01234567".chars())),
    )
        .map(|(_, _, digits)| i64::from_str_radix(&digits, 8).unwrap() as f64)
}

fn hex_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('x').or(token('X')),
        many1::<String, _>(hex_digit()),
    )
        .map(|(_, _, digits)| i64::from_str_radix(&digits, 16).unwrap() as f64)
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-string-literals
pub(crate) fn string_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = StringLiteral> {
    (try(double_quote_string()).or(single_quote_string())).map(StringLiteral)
}

fn double_quote_string<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    between(
        token('"'),
        token('"'),
        many(double_quote_string_character()),
    )
}

fn double_quote_string_character<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
    escape_sequence().map(|x| x.0).or(none_of(
        "\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}\"".chars(),
    ))
}

fn single_quote_string<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    between(
        token('\''),
        token('\''),
        many(single_quote_string_character()),
    )
}

fn single_quote_string_character<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
    escape_sequence()
        .map(|x| x.0)
        .or(none_of("\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}'".chars()))
}

// (char, String) is "cooked" and "raw"
// this is for template elements, to be able to get access to the raw string
// this makes things uglier, but oh well
fn escape_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    choice((
        try(character_escape_sequence()),
        try(non_escape_character_sequence()),
        try(hex_escape_sequence()),
        // TODO legacy octal escape sequence
        unicode_escape_sequence(),
    ))
}

fn character_escape_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
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

fn non_escape_character_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    token('\\')
        .and(none_of(
            "'\"\\bfnrtv0123456789xu\r\n\u{2028}\u{2029}".chars(),
        )).map(|(t, c)| (c, format!("{}{}", t, c)))
}

fn hex_escape_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    (token('\\'), token('x'), count::<String, _>(2, hex_digit())).map(|(t, x, hex_digits)| {
        let code_point = u32::from_str_radix(&hex_digits, 16).unwrap();
        let cooked = ::std::char::from_u32(code_point).unwrap();
        (cooked, format!("{}{}{}", t, x, hex_digits))
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#prod-UnicodeEscapeSequence
fn unicode_escape_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
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

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-regular-expression-literals
fn regex_literal_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (position(), regex_literal(), position()).map(|(start, value, end)| Expression::Literal {
        value: Literal::RegExpLiteral(value),
        loc: Some((start, end).into()),
    })
}

pub(crate) fn regex_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = RegExpLiteral> {
    (
        between(token('/'), token('/'), regex_body()),
        many::<String, _>(id_continue()),
    )
        .map(|(pattern, flags)| RegExpLiteral { pattern, flags })
}

fn regex_body<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (regex_first_char(), many::<String, _>(regex_char())).map(|(s, s2): (String, String)| s + &s2)
}

fn regex_first_char<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    try(regex_backslash_sequence())
        .or(try(regex_class()))
        .or(none_of("*/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
}

fn regex_char<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    try(regex_backslash_sequence())
        .or(try(regex_class()))
        .or(none_of("/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
}

fn regex_non_terminator<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    none_of("\n\r\u{2028}\u{2029}".chars())
}

fn regex_backslash_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (token('\\'), regex_non_terminator()).map(|(c, s): (char, char)| c.to_string() + &s.to_string())
}

fn regex_class<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
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

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-template-literal-lexical-components
fn template_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        position(),
        choice((
            try(no_substition_template()).map(|quasi| (vec![quasi], Vec::new())),
            try(substitution_template()),
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

fn no_substition_template<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
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
                cooked,
                raw,
                loc: Some((start, end).into()),
            }
        })
}

fn substitution_template<'a>() -> impl Parser<
    Input = easy::Stream<State<&'a str, SourcePosition>>,
    Output = (Vec<TemplateElement>, Vec<Expression>),
> {
    (template_head(), assignment_expression(), template_spans()).map(
        |(quasi, expression, (mut quasis_span, mut expressions_span))| {
            let mut quasis = vec![quasi];
            quasis.append(&mut quasis_span);
            let mut expressions = vec![expression];
            expressions.append(&mut expressions_span);
            (quasis, expressions)
        },
    )
}

fn template_head<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
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
                cooked,
                raw,
                loc: Some((start, end).into()),
            }
        })
}

fn template_spans<'a>() -> impl Parser<
    Input = easy::Stream<State<&'a str, SourcePosition>>,
    Output = (Vec<TemplateElement>, Vec<Expression>),
> {
    (
        many::<Vec<_>, _>(try((template_middle(), assignment_expression()))),
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

pub(crate) fn template_character<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    choice((
        try(token('$').skip(not_followed_by(token('{')))).map(|x: char| (x, x.to_string())),
        try(escape_sequence()),
        try(one_of("\r\n\u{2028}\u{2029}".chars())).map(|x: char| (x, x.to_string())),
        none_of("`\\$".chars()).map(|x: char| (x, x.to_string())),
    ))
}

fn template_middle<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
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
                cooked,
                raw,
                loc: Some((start, end).into()),
            }
        })
}

fn template_tail<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
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
                cooked,
                raw,
                loc: Some((start, end).into()),
            }
        })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-expressions
pub(crate) fn primary_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((
        try(this()),
        try(identifier_expression()),
        try(literal()),
        try(array_literal()),
        try(object_literal()),
        try(regex_literal_expression()),
        try(template_literal()),
        jsx_element(),
    ))
}

fn this<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (position(), string("this"), position()).map(|(start, _, end)| Expression::ThisExpression {
        loc: Some((start, end).into()),
    })
}

fn identifier_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (position(), identifier(), position()).map(|(start, name, end)| Expression::Identifier {
        name,
        loc: Some((start, end).into()),
    })
}

fn literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
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

fn array_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        position(),
        between(
            token('[').skip(skip_tokens()),
            token(']').skip(skip_tokens()),
            elision().with(element_list()).skip(elision()),
        ),
        position(),
    )
        .map(|(start, elements, end)| Expression::ArrayExpression {
            elements,
            loc: Some((start, end).into()),
        })
}

fn elision<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    many::<Vec<_>, _>(token(',')).with(skip_tokens())
}

fn element_list<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<ExpressionListItem>>
{
    many(
        choice((
            try(assignment_expression()).map(ExpressionListItem::Expression),
            spread_element(),
        )).skip(elision()),
    )
}

fn object_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        position(),
        between(
            token('{').skip(skip_tokens()),
            token('}').skip(skip_tokens()),
            sep_end_by(
                property_definition().skip(skip_tokens()),
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

fn property_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    choice((
        try(property_initializer()),
        try(method_definition()),
        try(shorthand_property()),
    ))
}

fn shorthand_property<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
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

fn property_initializer<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        position(),
        property_name(),
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

fn property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (Expression, bool)>
{
    choice((
        try(literal_property_name()).map(|e| (e, false)),
        try(computed_property_name()).map(|e| (e, true)),
    ))
}

fn literal_property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
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

fn computed_property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    between(
        token('[').skip(skip_tokens()),
        token(']'),
        assignment_expression(),
    )
}

fn spread_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ExpressionListItem>
{
    (
        position(),
        string("...").with(assignment_expression()),
        position(),
    )
        .map(|(start, expression, end)| {
            ExpressionListItem::Spread(Some((start, end).into()), expression)
        })
}

fn assignment_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    yield_expression()
}

fn yield_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        string("yield"),
        skip_tokens(),
        // optional((optional(token('*')), ok())),
        skip_tokens(),
    )
        .map(|(_, _, _)| Expression::Yield {
            argument: None,
            delegate: false,
        })
}

// https://facebook.github.io/jsx/
fn jsx_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((try(jsx_self_closing_element()), jsx_matched_element()))
}

fn jsx_self_closing_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        position(),
        between(token('<'), string("/>"), identifier()),
        position(),
    )
        .map(|(start, name, end)| Expression::JsxElementExpression {
            name,
            attributes: Vec::new(),
            children: Vec::new(),
            loc: Some((start, end).into()),
        })
}

fn jsx_matched_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        position(),
        between(token('<'), string(">"), identifier()),
        skip_tokens(),
        between(string("</"), token('>'), identifier()),
        position(),
    )
        .then(|(start, opening_name, _, closing_name, end)| {
            if opening_name == closing_name {
                value(Expression::JsxElementExpression {
                    name: opening_name,
                    attributes: Vec::new(),
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

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-statements-and-declarations
fn statement<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Statement> {
    // TODO use assignment_expression instead
    (position(), primary_expression(), position()).map(|(start, expression, end)| {
        Statement::ExpressionStatement {
            loc: Some((start, end).into()),
            expression,
        }
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-functions-and-classes

fn formal_parameters<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<Pattern>> {
    between(
        token('(').skip(skip_tokens()),
        token(')'),
        value(Vec::new()),
    )
}

fn formal_parameter<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Pattern> {
    (position(), identifier(), position()).map(|(start, name, end)| Pattern::Identifier {
        name,
        loc: Some((start, end).into()),
    })
}

fn function_body<'a>(
    _yield: bool,
    _await: bool,
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<FunctionBodyStatement>>
{
    between(
        token('{').skip(skip_tokens()),
        token('}'),
        value(Vec::new()),
    )
}

fn method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    choice((
        try(getter_method_definition()),
        try(setter_method_definition()),
        try(generator_method_definition()),
        try(async_generator_method_definition()),
        try(async_method_definition()),
        basic_method_definition(false, false),
    ))
}

fn basic_method_definition<'a>(
    _yield: bool,
    _await: bool,
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        position(),
        property_name(),
        skip_tokens(),
        formal_parameters(),
        skip_tokens(),
        function_body(_yield, _await),
        // lololol
        // the value here is just to pass the yield and await into the map below
        value(_yield),
        value(_await),
        position(),
    )
        .map(
            |(start, (key, computed), _, params, _, body, _yield, _await, end)| Property {
                key,
                value: Expression::FunctionExpression {
                    id: None,
                    async: _await,
                    generator: _yield,
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

fn generator_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        token('*'),
        skip_tokens(),
        basic_method_definition(true, false),
    )
        .map(|x| x.2)
}

fn async_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("async"),
        skip_tokens(),
        basic_method_definition(false, true),
    )
        .map(|x| x.2)
}

fn async_generator_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("async"),
        skip_tokens(),
        token('*'),
        skip_tokens(),
        basic_method_definition(true, true),
    )
        .map(|x| x.4)
}

fn getter_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        position(),
        string("get").skip(skip_tokens()),
        property_name().skip(skip_tokens()),
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

fn setter_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        position(),
        string("set").skip(skip_tokens()),
        property_name().skip(skip_tokens()),
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

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-scripts-and-modules
fn program<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Program> {
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

/// The main entry point to the parser. This function will return a fully constructed
/// AST or an error message describing why it couldn't parse the input string.
pub fn parse(source: &str) -> Result<Program, Error> {
    let stream = State::new(source);
    let (ast, _) = program()
        .easy_parse(stream)
        .map_err(|e| failure::err_msg(e.to_string()))?;
    Ok(ast)
}
