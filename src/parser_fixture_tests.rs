use parser;
use serde_json;

macro_rules! assert_fixture_passes {
    ($include:expr) => {
        let src = include_str!(concat!("../esprima/test/fixtures/", $include, ".js"));
        let expected =
            include_str!(concat!("../esprima/test/fixtures/", $include, ".tree.json"));
        assert_eq!(
            parser::parse(src).unwrap(),
            serde_json::from_str(expected).unwrap()
        );
    };
}

#[test]
fn test_expression_primary_literal_string() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0000");
}
