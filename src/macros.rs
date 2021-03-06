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
///       [array_item true],
///       [array_item false],
///       [array_item null],
///       [...[array [ [array_item num 1f64] ]]]
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
    // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-lexical-grammar-literals
    (regex_lit /{$pattern:expr}/{$flags:expr}) => {
        RegExpLiteral {
            pattern: $pattern,
            flags: $flags,
            loc: None,
        }
    };
    (regex_lit /{$pattern:expr}/) => {
        RegExpLiteral {
            pattern: $pattern,
            flags: String::new(),
            loc: None,
        }
    };
    (templ_el {$cooked:expr}) => {
        build_ast!(templ_el {$cooked} {$cooked})
    };
    (templ_el {$cooked:expr} {$raw:expr}) => {
        TemplateElement {
            cooked: $cooked,
            raw: $raw,
            loc: None,
        }
    };
    // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-expressions
    (this) => {
        Expression::This(None)
    };
    (id $id:expr) => {
        Expression::Identifier{ name: $id, loc: None, }
    };
    (null) => {
        Expression::Literal {
            value: Literal::NullLiteral(NullLiteral),
            loc: None
        }
    };
    (true) => {
        Expression::Literal {
            value: Literal::BooleanLiteral(BooleanLiteral(true)),
            loc: None
        }
    };
    (false) => {
        Expression::Literal {
            value: Literal::BooleanLiteral(BooleanLiteral(false)),
            loc: None
        }
    };
    (num $lit:expr) => {
        Expression::Literal {
            value: Literal::NumericLiteral(NumericLiteral($lit)),
            loc: None
        }
    };
    (str $lit:expr) => {
        Expression::Literal {
            value: Literal::StringLiteral(StringLiteral($lit)),
            loc: None
        }
    };
    (array [$($elements:tt),*]) => {
        Expression::ArrayExpression {
            loc: None,
            elements: vec![$(build_ast!($elements)),*]
        }
    };
    (array_item $($expression:tt)+) => {
        ExpressionListItem::Expression(build_ast!($($expression)+))
    };
    (...$($expression:tt)+) => {
        ExpressionListItem::Spread(SpreadElement::SpreadElement {
            argument: build_ast!($($expression)+),
            loc: None,
        })
    };
    (object [$($properties:tt),*]) => {
        Expression::ObjectExpression {
            loc: None,
            properties: vec![$(build_ast!($properties)),*]
        }
    };
    ([$($key:tt)+]: [$($value:tt)+]) => {
        Property {
            key: build_ast!($($key)+),
            value: build_ast!($($value)+),
            kind: PropertyKind::Init,
            computed: false,
            method: false,
            shorthand: false,
            loc: None,
        }
    };
    (get [$($key:tt)+] [$($value:tt)+]) => {
        Property {
            key: build_ast!($($key)+),
            value: build_ast!($($value)+),
            kind: PropertyKind::Get,
            computed: false,
            method: false,
            shorthand: false,
            loc: None,
        }
    };
    (set [$($key:tt)+] [$($value:tt)+]) => {
        Property {
            key: build_ast!($($key)+),
            value: build_ast!($($value)+),
            kind: PropertyKind::Set,
            computed: false,
            method: false,
            shorthand: false,
            loc: None,
        }
    };
    (function [$($params:tt),*] [$($body:tt),*]) => {
        Expression::FunctionExpression {
            id: None,
            params: vec![$(build_ast!($params)),*],
            body: vec![$(build_ast!($body)),*],
            generator: false,
            async: false
        }
    };
    (function * [$($params:tt),*] [$($body:tt),*]) => {
        Expression::FunctionExpression {
            id: None,
            params: vec![$(build_ast!($params)),*],
            body: vec![$(build_ast!($body)),*],
            generator: true,
            async: false
        }
    };
    (async function [$($params:tt),*] [$($body:tt),*]) => {
        Expression::FunctionExpression {
            id: None,
            params: vec![$(build_ast!($params)),*],
            body: vec![$(build_ast!($body)),*],
            generator: false,
            async: true
        }
    };
    (async function * [$($params:tt),*] [$($body:tt),*]) => {
        Expression::FunctionExpression {
            id: None,
            params: vec![$(build_ast!($params)),*],
            body: vec![$(build_ast!($body)),*],
            generator: true,
            async: true
        }
    };
    (function [$($params:tt),*] {$body:expr}) => {
        Expression::FunctionExpression {
            id: None,
            params: vec![$(build_ast!($params)),*],
            body: $body,
            generator: false,
            async: false
        }
    };
    (p_id $id:expr) => {
        Pattern::Identifier(Identifier(None, $id))
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
        Expression::JsxElementExpression {
            name: stringify!($id).to_string(),
            attributes: Vec::new(),
            children: Vec::new(),
            loc: None
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
