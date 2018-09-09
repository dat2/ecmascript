use parser;
use serde_json;

macro_rules! assert_fixture_passes {
    ($include:expr) => {
        let src = include_str!(concat!("../esprima/test/fixtures/", $include, ".js"));
        let expected = include_str!(concat!("../esprima/test/fixtures/", $include, ".tree.json"));
        println!("{}", src);
        assert_eq!(
            parser::parse(src).unwrap(),
            serde_json::from_str(expected).unwrap()
        );
    };
}

#[test]
fn test_expression_primary_array_0000() {
    assert_fixture_passes!("expression/primary/array/migrated_0000");
}

#[test]
fn test_expression_primary_array_0001() {
    assert_fixture_passes!("expression/primary/array/migrated_0001");
}

#[test]
fn test_expression_primary_array_0002() {
    assert_fixture_passes!("expression/primary/array/migrated_0002");
}

#[test]
fn test_expression_primary_array_0003() {
    assert_fixture_passes!("expression/primary/array/migrated_0003");
}

#[test]
fn test_expression_primary_array_0004() {
    assert_fixture_passes!("expression/primary/array/migrated_0004");
}

#[test]
fn test_expression_primary_array_0005() {
    assert_fixture_passes!("expression/primary/array/migrated_0005");
}

#[test]
fn test_expression_primary_array_0006() {
    assert_fixture_passes!("expression/primary/array/migrated_0006");
}

#[test]
fn test_expression_primary_array_0007() {
    assert_fixture_passes!("expression/primary/array/migrated_0007");
}

#[test]
fn test_expression_primary_literal_string() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0000");
}
