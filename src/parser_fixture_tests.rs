use ast::*;
use parser;
use serde_json;

macro_rules! assert_fixture_passes_inner {
    ($src:expr, $tree:expr) => {
        println!("{}", $src);
        println!("{:?}", $src.as_bytes());
        assert_eq!(
            parser::parse($src).unwrap(),
            serde_json::from_str($tree).unwrap()
        );
    };
}

macro_rules! assert_fixture_passes {
    ($include:expr) => {
        assert_fixture_passes_inner!(
            include_str!(concat!("../esprima/test/fixtures/", $include, ".js")),
            include_str!(concat!("../esprima/test/fixtures/", $include, ".tree.json"))
        )
    };
}

macro_rules! assert_source_fixture_passes {
    ($include:expr) => {
        let src = include_str!(concat!("../esprima/test/fixtures/", $include, ".source.js"));
        let expected = include_str!(concat!("../esprima/test/fixtures/", $include, ".tree.json"));
        let parsed = parser::parse(&src).unwrap();
        let Program::Program { body, .. } = parsed;
        if let Statement::VariableDeclaration {
            ref declarations, ..
        } = body[0]
        {
            if let VariableDeclaratorInit::Expression(ref expr) = declarations[0].init {
                if let Expression::Literal {
                    value: Literal::StringLiteral(StringLiteral(value)),
                    ..
                } = expr
                {
                    assert_fixture_passes_inner!(&value, expected);
                    return;
                }
            }
        }
        assert!(false);
    };
}

macro_rules! assert_fixture_fails {
    ($include:expr) => {
        let src = include_str!(concat!("../esprima/test/fixtures/", $include, ".js"));
        println!("{}", src);
        println!("{:?}", src.as_bytes());
        assert!(parser::parse(src).is_err());
    };
}

// string
#[test]
fn test_expression_primary_literal_string_0000() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0000");
}

#[test]
fn test_expression_primary_literal_string_0001() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0001");
}

#[test]
fn test_expression_primary_literal_string_0002() {
    assert_source_fixture_passes!("expression/primary/literal/string/migrated_0002");
}

#[test]
fn test_expression_primary_literal_string_0003() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0003");
}

#[test]
fn test_expression_primary_literal_string_0006() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0006");
}

#[test]
fn test_expression_primary_literal_string_0007() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0007");
}

#[test]
fn test_expression_primary_literal_string_0008() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0008");
}

#[test]
fn test_expression_primary_literal_string_0009() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0009");
}

#[test]
fn test_expression_primary_literal_string_0010() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0010");
}

#[test]
fn test_expression_primary_literal_string_0011() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0011");
}

#[test]
fn test_expression_primary_literal_string_0012() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0012");
}

#[test]
fn test_expression_primary_literal_string_0013() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0013");
}

#[test]
fn test_expression_primary_literal_string_0015() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0015");
}

#[test]
fn test_expression_primary_literal_string_0016() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0016");
}

#[test]
fn test_expression_primary_literal_string_0017() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0017");
}

#[test]
fn test_expression_primary_literal_string_0018() {
    assert_fixture_passes!("expression/primary/literal/string/migrated_0018");
}

#[test]
fn test_expression_primary_literal_string_invalid_escaped_hex() {
    assert_fixture_fails!("expression/primary/literal/string/invalid_escaped_hex");
}

#[test]
fn test_expression_primary_literal_string_invalid_hex() {
    assert_fixture_fails!("expression/primary/literal/string/invalid_hex");
}

// numeric
#[test]
fn test_expression_primary_literal_numeric_0000() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0000");
}

#[test]
fn test_expression_primary_literal_numeric_0001() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0001");
}

#[test]
fn test_expression_primary_literal_numeric_0002() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0002");
}

#[test]
fn test_expression_primary_literal_numeric_0003() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0003");
}

#[test]
fn test_expression_primary_literal_numeric_0004() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0004");
}

#[test]
fn test_expression_primary_literal_numeric_0005() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0005");
}

#[test]
fn test_expression_primary_literal_numeric_0006() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0006");
}

#[test]
fn test_expression_primary_literal_numeric_0007() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0007");
}

#[test]
fn test_expression_primary_literal_numeric_0008() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0008");
}

#[test]
fn test_expression_primary_literal_numeric_0009() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0009");
}

#[test]
fn test_expression_primary_literal_numeric_0010() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0010");
}

#[test]
fn test_expression_primary_literal_numeric_0011() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0011");
}

#[test]
fn test_expression_primary_literal_numeric_0012() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0012");
}

#[test]
fn test_expression_primary_literal_numeric_0013() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0013");
}

#[test]
fn test_expression_primary_literal_numeric_0014() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0014");
}

#[test]
fn test_expression_primary_literal_numeric_0015() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0015");
}

#[test]
fn test_expression_primary_literal_numeric_0016() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0016");
}

#[test]
fn test_expression_primary_literal_numeric_0017() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0017");
}

#[test]
fn test_expression_primary_literal_numeric_0018() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0018");
}

#[test]
fn test_expression_primary_literal_numeric_0019() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0019");
}

#[test]
fn test_expression_primary_literal_numeric_0020() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0020");
}

#[test]
fn test_expression_primary_literal_numeric_0021() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0021");
}

#[test]
fn test_expression_primary_literal_numeric_0022() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0022");
}

#[test]
fn test_expression_primary_literal_numeric_0023() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0023");
}

#[test]
fn test_expression_primary_literal_numeric_0024() {
    assert_fixture_passes!("expression/primary/literal/numeric/migrated_0024");
}

#[test]
fn test_expression_primary_literal_numeric_invalid_hex() {
    assert_fixture_fails!("expression/primary/literal/numeric/invalid_hex");
}

// array
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
