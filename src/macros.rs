// some conventions:
// [] means recursively call the build_ast macro again
// {} means take a rust expression
// when we need to recursively call the macro for like a call expr,
// we do this: [ [arg1] [arg2] [arg3] ]

// two macros that get exported:
// build_ast and match_ast
#[macro_export]
macro_rules! build_ast {
    ([$($many:tt)+]) => {
        build_ast!($($many)+)
    };
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-lexical-grammar-literals
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

    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-expressions
    (this) => {
        Expression::This
    };
    (id $id:expr) => {
        Expression::IdReference {
            id: $id
        }
    };
    (call [$($id:tt)+] [$($args:tt)+]) => {
        Expression::Call {
            callee: Box::new(build_ast!($($id)+)),
            arguments: vec![$(build_ast!($args)),+]
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
