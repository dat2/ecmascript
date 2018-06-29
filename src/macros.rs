/// This macro makes constructing complicated syntax trees very easy.
/// This can be useful for re-writing parts of the AST, or deriving a
/// a syntax tree from other trees (eg. concatenating syntax trees).
///
/// # Example
/// ```
/// # #[macro_use] extern crate ecmascript;
/// # use ecmascript::ast::*;
/// let my_wrapper_func = build_ast! {
///   [array [
///       [true],
///       [false],
///       [null],
///       [...[array [ [num 1f64] ]]]
///    ]]
/// };
/// ```
///
/// # Conventions
/// - we use [] to represent recursive calls to the macro
///
/// eg:
/// `call [id "my_func".to_string()] [ [id "a".to_string()] [true] [null] ]`
/// recursively expands to
/// `build_ast!(call build_ast!(...) [ build_ast!(...), build_ast!(...), build_ast!(...) ])`
///
/// - we use {} to accept a rust expression

#[macro_export]
macro_rules! build_ast {
    ([$($many:tt)+]) => {
        build_ast!($($many)+)
    };
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-lexical-grammar-literals
    (regex_lit /{$pattern:expr}/{$flags:expr}) => {
        RegexLiteral {
            pattern: $pattern,
            flags: $flags,
        }
    };
    (regex_lit /{$pattern:expr}/) => {
        RegexLiteral {
            pattern: $pattern,
            flags: String::new(),
        }
    };
    (templ_el {$cooked:expr}) => {
        build_ast!(templ_el {$cooked} {$cooked})
    };
    (templ_el {$cooked:expr} {$raw:expr}) => {
        TemplateElement {
            cooked: $cooked,
            raw: $raw,
        }
    };
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-expressions
    (this) => {
        Expression::This
    };
    (id $id:expr) => {
        Expression::IdReference {
            id: $id
        }
    };
    (null) => {
        Expression::Literal {
            literal: ExpressionLiteral::NullLiteral(NullLiteral)
        }
    };
    (true) => {
        Expression::Literal {
            literal: ExpressionLiteral::BooleanLiteral(true)
        }
    };
    (false) => {
        Expression::Literal {
            literal: ExpressionLiteral::BooleanLiteral(false)
        }
    };
    (num $lit:expr) => {
        Expression::Literal {
            literal: ExpressionLiteral::NumberLiteral($lit)
        }
    };
    (str $lit:expr) => {
        Expression::Literal {
            literal: ExpressionLiteral::StringLiteral($lit)
        }
    };
    (array [$($elements:tt),*]) => {
        Expression::ArrayLiteral {
            elements: vec![$(build_ast!($elements)),*]
        }
    };
    (obj [$($properties:tt),+]) => {
        Expression::ObjectLiteral {
            properties: vec![$(build_ast!($params)),+],
        }
    };
    ([$($key:tt)+]: [$($value:tt)+]) => {
        Property {
            key: build_ast!($($key)+),
            value: build_ast!($($value)+),
            kind: PropertyKind::Init,
        }
    };
    (function [$($params:tt),+] {$body:expr}) => {
        Expression::Function {
            id: None,
            params: vec![$(build_ast!($params)),+],
            body: $body,
            generator: false,
            async: false
        }
    };
    (...[$($expression:tt)+]) => {
        Expression::Spread {
            expression: Box::new(build_ast!($($expression)+))
        }
    };
    // whole bunch of other stuff between
    (call [$($id:tt)+] [$($args:tt)+]) => {
        Expression::Call {
            callee: Box::new(build_ast!($($id)+)),
            arguments: vec![$(build_ast!($args)),+]
        }
    };
    (yield) => {
        Expression::Yield {
            argument: None,
            delegate: false,
        }
    };
    // JSX
    (<$id:ident />) => {
        Expression::JsxElement {
            name: stringify!($id).to_string(),
            attributes: Vec::new()
        }
    };
        /*
    (var) => {
        VariableDeclarationKind::Var
    };
    (let) => {
        VariableDeclarationKind::Let
    };
    (const) => {
        VariableDeclarationKind::Const
    };
    ($var:tt [$($id:tt)+]) => {
        Statement::VariableDeclaration {
            declaration: VariableDeclaration {
                kind: build_ast!($var),
                declarations: vec![VariableDeclarator {
                    id: build_ast!($($id)+),
                    init: None,
                }],
            },
        }
    };
    // [] means build another ast
    ($var:tt [$($id:tt)+] = [$($tail:tt)+]) => {
        Statement::VariableDeclaration {
            declaration: VariableDeclaration {
                kind: build_ast!($var),
                declarations: vec![VariableDeclarator {
                    id: build_ast!($($id)+),
                    init: Some(build_ast!($($tail)+)),
                }],
            },
        }
    };
    // { } means defer to rust expression
    ($var:tt [$($id:tt)+] = {$($init:tt)+}) => {
        Statement::VariableDeclaration {
            declaration: VariableDeclaration {
                kind: build_ast!($var),
                declarations: vec![VariableDeclarator {
                    id: build_ast!($($id)+),
                    init: Some($($init)+)
                }],
            },
        }
    };
    (call [$($id:tt)+] [$($args:tt)+]) => {
        Statement::Expression {
            expression: build_ast!(expr_call [$($id)+] [$($args)+])
        }
    };
    */
}

/*
#[macro_export]
macro_rules! match_ast {
    (import [$($id:tt)+] from [$($path:tt)+]) => {
        Statement::Import(ImportSpecifier::ImportDefault($($id)+), $($path)+)
    }
}
*/
