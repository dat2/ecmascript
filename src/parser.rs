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
            column: source_position.column as usize,
        }
    }
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-lexical-grammar

#[allow(dead_code)]
/// This parser will consume all following whitespace tokens, including line terminators.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-white-space)
fn ws<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    spaces().map(|_| ())
}

#[allow(dead_code)]
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
#[allow(dead_code)]
pub(crate) fn comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    try(block_comment()).or(line_comment())
}

#[allow(dead_code)]
/// This parses a multiline comment, starting with /* and ending with */.
/// It will consume the input and return ().
fn block_comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    (string("/*"), skip_until(try(string("*/"))), string("*/")).map(|_| ())
}

#[allow(dead_code)]
/// This parses
fn line_comment<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    (
        string("//"),
        skip_until(line_terminator()),
        line_terminator(),
    ).map(|_| ())
}

#[allow(dead_code)]
fn skip_tokens<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    ws().or(comment())
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-names-and-keywords
#[allow(dead_code)]
fn satisfy_id_start(c: char) -> bool {
    UnicodeXID::is_xid_start(c) || c == '$' || c == '_'
}

#[allow(dead_code)]
fn id_start<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char>
{
    satisfy(satisfy_id_start)
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn satisfy_id_continue(c: char) -> bool {
    // 200c = ZWNJ, 200d = ZWJ
    UnicodeXID::is_xid_continue(c) || c == '\u{200C}' || c == '\u{200D}' || c == '$' || c == '_'
}

#[allow(dead_code)]
fn id_continue<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    satisfy(satisfy_id_continue)
}

#[allow(dead_code)]
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
#[allow(dead_code)]
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
        ].iter()
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
        ].iter()
            .cloned()
            .collect()
    };
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-null-literals
#[allow(dead_code)]
pub(crate) fn null_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = NullLiteral> {
    (position(), string("null"), position())
        .map(|(start, _, end)| NullLiteral(Some((start, end).into())))
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-boolean-literals
#[allow(dead_code)]
pub(crate) fn boolean_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = BooleanLiteral> {
    (
        position(),
        choice((
            try(string("true")).map(|_| true),
            string("false").map(|_| false),
        )),
        position(),
    ).map(|(start, value, end)| BooleanLiteral(Some((start, end).into()), value))
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-numeric-literals
#[allow(dead_code)]
pub(crate) fn numeric_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = NumericLiteral> {
    (
        position(),
        choice((
            try(binary_integer_literal()),
            try(octal_integer_literal()),
            try(hex_integer_literal()),
            decimal_literal(),
        )),
        position(),
    ).map(|(start, value, end)| NumericLiteral(Some((start, end).into()), value))
}

#[allow(dead_code)]
fn decimal_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        optional(decimal_integer_literal()),
        optional(
            (token('.'), many::<String, _>(digit()))
                .map(|(c, s): (char, String)| c.to_string() + &s),
        ),
        optional(exponent_part()),
    ).then(|tuple| match tuple {
            (None, None, None) => unexpected("empty").map(|_| String::new()).left(),
            (literal_opt, digits_opt, exponent_opt) => value(
                literal_opt.unwrap_or_else(String::new)
                    + &digits_opt.unwrap_or_else(String::new)
                    + &exponent_opt.unwrap_or_else(String::new),
            ).right(),
        })
        .map(|s| s.parse::<f64>().unwrap())
}

#[allow(dead_code)]
fn decimal_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    choice((
        string("0").skip(not_followed_by(digit())).map(String::from),
        (one_of("123456789".chars()), many::<String, _>(digit()))
            .map(|(c, s): (char, String)| c.to_string() + &s),
    ))
}

#[allow(dead_code)]
fn exponent_part<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (
        token('e').or(token('E')),
        optional(token('-').or(token('+'))),
        many1::<String, _>(digit()),
    ).map(
        |(e, sign_opt, digits): (char, Option<char>, String)| match sign_opt {
            Some(sign) => e.to_string() + &sign.to_string() + &digits,
            None => e.to_string() + &digits,
        },
    )
}

#[allow(dead_code)]
fn binary_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('b').or(token('B')),
        many1::<String, _>(one_of("01".chars())),
    ).map(|(_, _, digits)| i64::from_str_radix(&digits, 2).unwrap() as f64)
}

#[allow(dead_code)]
fn octal_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('o').or(token('O')),
        many1::<String, _>(one_of("01234567".chars())),
    ).map(|(_, _, digits)| i64::from_str_radix(&digits, 8).unwrap() as f64)
}

#[allow(dead_code)]
fn hex_integer_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = f64> {
    (
        token('0'),
        token('x').or(token('X')),
        many1::<String, _>(hex_digit()),
    ).map(|(_, _, digits)| i64::from_str_radix(&digits, 16).unwrap() as f64)
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-string-literals
#[allow(dead_code)]
pub(crate) fn string_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = StringLiteral> {
    (
        position(),
        try(double_quote_string()).or(single_quote_string()),
        position(),
    ).map(|(start, value, end)| StringLiteral(Some((start, end).into()), value))
}

#[allow(dead_code)]
fn double_quote_string<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    between(
        token('"'),
        token('"'),
        many(double_quote_string_character()),
    )
}

#[allow(dead_code)]
fn double_quote_string_character<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    // U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), U+2028 (LINE SEPARATOR), U+2029 (PARAGRAPH SEPARATOR), and U+000A (LINE FEED)
    escape_sequence().map(|x| x.0).or(none_of(
        "\u{005c}\u{000D}\u{2028}\u{2029}\u{000A}\"".chars(),
    ))
}

#[allow(dead_code)]
fn single_quote_string<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    between(
        token('\''),
        token('\''),
        many(single_quote_string_character()),
    )
}

#[allow(dead_code)]
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
#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
fn non_escape_character_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    token('\\')
        .and(none_of(
            "'\"\\bfnrtv0123456789xu\r\n\u{2028}\u{2029}".chars(),
        ))
        .map(|(t, c)| (c, format!("{}{}", t, c)))
}

#[allow(dead_code)]
fn hex_escape_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    (token('\\'), token('x'), count::<String, _>(2, hex_digit())).map(|(t, x, hex_digits)| {
        let code_point = u32::from_str_radix(&hex_digits, 16).unwrap();
        let cooked = ::std::char::from_u32(code_point).unwrap();
        (cooked, format!("{}{}{}", t, x, hex_digits))
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#prod-UnicodeEscapeSequence
#[allow(dead_code)]
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
    ).then(|(t, u, digits_raw)| {
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
#[allow(dead_code)]
pub(crate) fn regex_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = RegExpLiteral> {
    (
        position(),
        between(token('/'), token('/'), regex_body()),
        many::<String, _>(id_continue()),
        position(),
    ).map(|(start, pattern, flags, end)| RegExpLiteral {
        pattern,
        flags,
        loc: Some((start, end).into()),
    })
}

#[allow(dead_code)]
fn regex_body<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (regex_first_char(), many::<String, _>(regex_char())).map(|(s, s2): (String, String)| s + &s2)
}

#[allow(dead_code)]
fn regex_first_char<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    try(regex_backslash_sequence())
        .or(try(regex_class()))
        .or(none_of("*/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
}

#[allow(dead_code)]
fn regex_char<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    try(regex_backslash_sequence())
        .or(try(regex_class()))
        .or(none_of("/\\[\n\r\u{2028}\u{2029}".chars()).map(|c: char| c.to_string()))
}

#[allow(dead_code)]
fn regex_non_terminator<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = char> {
    none_of("\n\r\u{2028}\u{2029}".chars())
}

#[allow(dead_code)]
fn regex_backslash_sequence<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (token('\\'), regex_non_terminator()).map(|(c, s): (char, char)| c.to_string() + &s.to_string())
}

#[allow(dead_code)]
fn regex_class<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = String> {
    (
        token('['),
        many::<String, _>(
            try(regex_backslash_sequence()).or(none_of("]\\".chars()).map(|c: char| c.to_string())),
        ),
        token(']'),
    ).map(|(open, middle, end): (char, String, char)| {
        open.to_string() + &middle + &end.to_string()
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-template-literal-lexical-components
#[allow(dead_code)]
pub(crate) fn template<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
    choice((try(no_substition_template()), template_head()))
}

#[allow(dead_code)]
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
    ).map(|(start, pairs, end)| {
        let cooked = pairs.iter().cloned().map(|x| x.0).collect();
        let raw = pairs.iter().cloned().map(|x| x.1).collect();
        TemplateElement {
            cooked,
            raw,
            loc: Some((start, end).into()),
        }
    })
}

#[allow(dead_code)]
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
    ).map(|(start, pairs, end)| {
        let cooked = pairs.iter().cloned().map(|x| x.0).collect();
        let raw = pairs.iter().cloned().map(|x| x.1).collect();
        TemplateElement {
            cooked,
            raw,
            loc: Some((start, end).into()),
        }
    })
}

#[allow(dead_code)]
pub(crate) fn template_character<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = (char, String)> {
    choice((
        try(token('$').skip(not_followed_by(token('{')))).map(|x: char| (x, x.to_string())),
        try(escape_sequence()),
        try(one_of("\r\n\u{2028}\u{2029}".chars())).map(|x: char| (x, x.to_string())),
        none_of("`\\$".chars()).map(|x: char| (x, x.to_string())),
    ))
}

#[allow(dead_code)]
pub(crate) fn template_substition_tail<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
    choice((try(template_middle()), template_tail()))
}

#[allow(dead_code)]
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
    ).map(|(start, pairs, end)| {
        let cooked = pairs.iter().cloned().map(|x| x.0).collect();
        let raw = pairs.iter().cloned().map(|x| x.1).collect();
        TemplateElement {
            cooked,
            raw,
            loc: Some((start, end).into()),
        }
    })
}

#[allow(dead_code)]
fn template_tail<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = TemplateElement> {
    (
        position(),
        token('}').with(many::<Vec<_>, _>(template_character())),
        position(),
    ).map(|(start, pairs, end)| {
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
#[allow(dead_code)]
pub(crate) fn primary_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((
        try(this()),
        try(identifier_expression()),
        try(literal()),
        try(array_literal()),
        try(object_literal()),
        try(regex_literal())
            .map(Literal::RegExpLiteral)
            .map(Expression::Literal),
        jsx_element(),
    ))
}

#[allow(dead_code)]
fn this<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (position(), string("this"), position())
        .map(|(start, _, end)| Expression::This(Some((start, end).into())))
}

#[allow(dead_code)]
fn identifier_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (position(), identifier(), position())
        .map(|(start, id, end)| Identifier(Some((start, end).into()), id))
        .map(Expression::Identifier)
}

#[allow(dead_code)]
fn literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((
        try(null_literal()).map(Literal::NullLiteral),
        try(boolean_literal()).map(Literal::BooleanLiteral),
        try(numeric_literal()).map(Literal::NumericLiteral),
        try(string_literal()).map(Literal::StringLiteral),
    )).map(Expression::Literal)
}

#[allow(dead_code)]
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
    ).map(|(start, elements, end)| Expression::ArrayLiteral(Some((start, end).into()), elements))
}

#[allow(dead_code)]
fn elision<'a>() -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = ()> {
    many::<Vec<_>, _>(token(',')).with(skip_tokens())
}

#[allow(dead_code)]
fn element_list<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<Expression>> {
    many(choice((try(assignment_expression()), spread_element())).skip(elision()))
}

#[allow(dead_code)]
fn object_literal<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    between(
        token('{').skip(skip_tokens()),
        token('}').skip(skip_tokens()),
        sep_end_by(
            property_definition().skip(skip_tokens()),
            token(',').skip(skip_tokens()),
        ),
    ).map(Expression::ObjectLiteral)
}

#[allow(dead_code)]
fn property_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    choice((
        try(property_initializer()),
        try(method_definition()),
        try(shorthand_property()),
    ))
}

#[allow(dead_code)]
fn shorthand_property<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    identifier_expression().map(|id| Property {
        key: id.clone(),
        value: id,
        kind: PropertyKind::Init,
        is_spread: false,
    })
}

#[allow(dead_code)]
fn property_initializer<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        property_name(),
        skip_tokens(),
        token(':'),
        skip_tokens(),
        literal(),
    ).map(|(key, _, _, _, value)| Property {
        key,
        value,
        kind: PropertyKind::Init,
        is_spread: false,
    })
}

#[allow(dead_code)]
fn property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((try(literal_property_name()), try(computed_property_name())))
}

#[allow(dead_code)]
fn literal_property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((
        identifier_expression(),
        string_literal()
            .map(Literal::StringLiteral)
            .map(Expression::Literal),
        numeric_literal()
            .map(Literal::NumericLiteral)
            .map(Expression::Literal),
    ))
}

#[allow(dead_code)]
fn computed_property_name<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    between(
        token('[').skip(skip_tokens()),
        token(']'),
        assignment_expression(),
    )
}

#[allow(dead_code)]
fn spread_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    string("...")
        .with(assignment_expression())
        .map(Box::new)
        .map(Expression::Spread)
}

#[allow(dead_code)]
fn assignment_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    yield_expression()
}

#[allow(dead_code)]
fn yield_expression<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        string("yield"),
        skip_tokens(),
        // optional((optional(token('*')), ok())),
        skip_tokens(),
    ).map(|(_, _, _)| Expression::Yield {
        argument: None,
        delegate: false,
    })
}

// https://facebook.github.io/jsx/
#[allow(dead_code)]
fn jsx_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    choice((try(jsx_self_closing_element()), jsx_matched_element()))
}

#[allow(dead_code)]
fn jsx_self_closing_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    between(token('<'), string("/>"), identifier()).map(|name| Expression::JsxElement {
        name,
        attributes: Vec::new(),
        children: Vec::new(),
    })
}

#[allow(dead_code)]
fn jsx_matched_element<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Expression> {
    (
        between(token('<'), string(">"), identifier()),
        skip_tokens(),
        between(string("</"), token('>'), identifier()),
    ).then(|(opening_name, _, closing_name)| {
        if opening_name == closing_name {
            value(Expression::JsxElement {
                name: opening_name,
                attributes: Vec::new(),
                children: Vec::new(),
            }).left()
        } else {
            unexpected("closing element")
                .map(|_| Expression::JsxElement {
                    name: String::new(),
                    attributes: Vec::new(),
                    children: Vec::new(),
                })
                .message("closing name is not the same as opening name")
                .right()
        }
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-statements-and-declarations

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-functions-and-classes

#[allow(dead_code)]
fn formal_parameters<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<Pattern>> {
    between(
        token('(').skip(skip_tokens()),
        token(')'),
        value(Vec::new()),
    )
}

#[allow(dead_code)]
fn formal_parameter<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Pattern> {
    (position(), identifier(), position())
        .map(|(start, id, end)| Identifier(Some((start, end).into()), id))
        .map(Pattern::Identifier)
}

#[allow(dead_code)]
fn function_body<'a>(
    _yield: bool,
    _await: bool,
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Vec<Statement>> {
    between(
        token('{').skip(skip_tokens()),
        token('}'),
        value(Vec::new()),
    )
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn basic_method_definition<'a>(
    _yield: bool,
    _await: bool,
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        property_name(),
        skip_tokens(),
        formal_parameters(),
        skip_tokens(),
        function_body(_yield, _await),
        // lololol
        // the value here is just to pass the yield and await into the map below
        value(_yield),
        value(_await),
    ).map(|(key, _, params, _, body, _yield, _await)| Property {
        key,
        value: Expression::Function {
            id: None,
            async: _await,
            generator: _yield,
            body,
            params,
        },
        kind: PropertyKind::Init,
        is_spread: false,
    })
}

#[allow(dead_code)]
fn generator_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        token('*'),
        skip_tokens(),
        basic_method_definition(true, false),
    ).map(|x| x.2)
}

#[allow(dead_code)]
fn async_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("async"),
        skip_tokens(),
        basic_method_definition(false, true),
    ).map(|x| x.2)
}

#[allow(dead_code)]
fn async_generator_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("async"),
        skip_tokens(),
        token('*'),
        skip_tokens(),
        basic_method_definition(true, true),
    ).map(|x| x.4)
}

#[allow(dead_code)]
fn getter_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("get"),
        skip_tokens(),
        property_name(),
        skip_tokens(),
        token('('),
        skip_tokens(),
        token(')'),
        skip_tokens(),
        function_body(false, false),
    ).map(|(_, _, key, _, _, _, _, _, body)| Property {
        key,
        value: Expression::Function {
            id: None,
            async: false,
            generator: false,
            body,
            params: Vec::new(),
        },
        kind: PropertyKind::Get,
        is_spread: false,
    })
}

#[allow(dead_code)]
fn setter_method_definition<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Property> {
    (
        string("set"),
        skip_tokens(),
        property_name(),
        skip_tokens(),
        token('('),
        skip_tokens(),
        formal_parameter(),
        skip_tokens(),
        token(')'),
        skip_tokens(),
        function_body(false, false),
    ).map(|(_, _, key, _, _, _, param, _, _, _, body)| Property {
        key,
        value: Expression::Function {
            id: None,
            async: false,
            generator: false,
            body,
            params: vec![param],
        },
        kind: PropertyKind::Set,
        is_spread: false,
    })
}

// https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-scripts-and-modules
#[allow(dead_code)]
fn program<'a>(
) -> impl Parser<Input = easy::Stream<State<&'a str, SourcePosition>>, Output = Program> {
    eof().map(|_| Program {
        source_type: SourceType::Module,
        body: Vec::new(),
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
