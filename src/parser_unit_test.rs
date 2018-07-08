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
        assert!(($parser(), eof()).easy_parse(State::new($input)).is_err());
    };
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
