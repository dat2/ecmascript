//! This module contains type definitions for the Abstract Syntax elements
//! that make up the ECMAScript language.
//!
//! The types have been designed to be easily understandable and readable. That means
//! we do not explicitly disallow invalid syntax trees in favour of simplicity.
//! For example, the TaggedTemplate is only allowed to have a TemplateLiteral
//! expression as its quasi, but we do not enforce this in the type definition
//! for the sake of brevity.
//!
//! The macros `build_ast` and `match_ast` are meant to be the public API of this
//! module as they abstract away the types in such a way so that the user of the library
//! feels as if they are working with source text almost directly.

/// NullLiteral is the syntax element for `null`.
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-null-literals)
#[derive(Debug, Clone, PartialEq)]
pub struct NullLiteral;

/// BooleanLiteral is the syntax element for `true` and `false`.
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-boolean-literals)
pub type BooleanLiteral = bool;

/// NumberLiteral is the syntax element for numbers. The parser will convert the string
/// values into an f64 for the sake of simplicity.
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-numeric-literals)
pub type NumberLiteral = f64;

/// StringLiteral is a syntax element with quotes (single or double).
/// eg. `'my string literal'` or `"my other string literal"`
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-literals-string-literals)
pub type StringLiteral = String;

/// Id is an identifier in the ecmascript language.
/// eg. `var foo = {};`
/// `foo` is the identifier.
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-identifier-names).
pub type Id = String;

/// RegexLiteral is the syntax element of a regular expression.
/// eg. `/abc[123]/gi`
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-literals-regular-expression-literals)
#[derive(Debug, Clone, PartialEq)]
pub struct RegexLiteral {
    /// This is the text between the slashes.
    pub pattern: String,
    /// This is the text after the slashes. eg the `i` flag is the case insensitive flag.
    pub flags: String,
}

/// TemplateElement is any text between interpolated expressions inside a template literal.
/// eg. ``abc ${} \u{2028}``
/// "abc " and " \u{2028}" would be the TemplateElements for this template literal.
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-template-literal-lexical-components)
#[derive(Debug, Clone, PartialEq)]
pub struct TemplateElement {
    /// If the template element has any sort of escape sequences (eg. \u{2028})
    /// this will represent the evaluated result of that sequence.
    /// eg. if raw == "\u{41}", cooked = "A"
    pub cooked: String,
    /// This will store the exact string value, before being evaluted into the unicode
    /// code points.
    pub raw: String,
}

/// Expression is an enumeration of all possible expressions merged into one big enum.
/// This also includes language extensions, such as JSX.
///
/// This represents all possible computations that can be done in the ecmascript language.
///
/// [Reference](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-expressions)
/// [Primary Expression](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-primary-expression)
/// [Left Hand Side Expressions](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-left-hand-side-expressions)
/// [Update Expressions](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-update-expressions)
/// [JSX Specification](https://facebook.github.io/jsx/)
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    This,
    IdReference {
        id: Id,
    },
    Literal {
        literal: ExpressionLiteral,
    },
    ArrayLiteral {
        elements: Vec<Expression>,
    },
    ObjectLiteral {
        properties: Vec<Property>,
    },
    Function {
        id: Option<Id>,
        params: Vec<Id>,
        body: Vec<Statement>,
        async: bool,
        generator: bool,
    },
    Class,
    RegexLiteral {
        regex: RegexLiteral,
    },
    TemplateLiteral {
        quasis: Vec<TemplateElement>,
        expressions: Vec<Expression>,
    },
    Spread {
        expression: Box<Expression>,
    },
    Member {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        computed: bool, // lhs[rhs]
    },
    Super,
    MetaProperty,
    New {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    TaggedTemplate {
        tag: Box<Expression>,
        // quasi can only be a TemplateLiteral
        // however, we will not make the AST more complicated, and let the
        // parser produce valid AST elements.
        quasi: Box<Expression>,
    },
    Update {
        operator: UpdateOperator,
        argument: Box<Expression>,
        prefix: bool,
    },
    Unary {
        operator: UnaryOperator,
        prefix: bool,
        argument: Box<Expression>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-conditional-operator
    Conditional {
        test: Box<Expression>,
        alternate: Box<Expression>,
        consequent: Box<Expression>,
    },
    Assignment {
        operator: AssignmentOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ArrowFunction {
        body: Vec<Statement>,
        expression: bool,
        async: bool, // async () =>
    },
    Yield {
        argument: Option<Box<Expression>>,
        delegate: bool, // yield *
    },
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-comma-operator
    Comma {
        expressions: Vec<Expression>,
    },
    JsxElement {
        name: String,
        attributes: Vec<JsxAttribute>,
    },
    JsxFragment,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionLiteral {
    NullLiteral(NullLiteral),
    BooleanLiteral(BooleanLiteral),
    NumberLiteral(NumberLiteral),
    StringLiteral(StringLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub key: Expression,
    pub value: Expression,
    pub kind: PropertyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpdateOperator {
    // ++
    Increment,
    // --
    Decrement,
}

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-unary-operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    // -
    Minus,
    // +
    Plus,
    // !
    Not,
    // ~
    BitwiseNot,
    Typeof,
    Void,
    Delete,
}

/// All the operators that have 2 arguments are merged into one big enum here for simplicity
/// sake.
///
/// [Multiplicative Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-multiplicative-operators)
/// [Additive Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-additive-operators)
/// [Bitwise Shift Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-bitwise-shift-operators)
/// [Relational Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-relational-operators)
/// [Equality Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-equality-operators)
/// [Bitwise Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-binary-bitwise-operators)
/// [Logical Operators](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-binary-logical-operators)
/// [Exponentiation Operator](https://www.ecma-international.org/ecma-262/8.0/index.html#sec-exp-operator)
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    /// The double equal operator that does type coercion. (a == b)
    EqEq,
    /// The not equal operator that does type coercion. (a != b)
    NotEq,
    /// The triple equal operator that compares types first, then values second. (a === b)
    EqEqEq,
    /// The not equal operator that compares types first, then values second. (a !== b)
    NotEqEq,
    /// The less than operator. (a < b)
    Lt,
    /// The less than or equal to operator. (a <= b)
    Lte,
    /// The greater than operator. (a > b)
    Gt,
    /// The greater than or equal to operator. (a >= b)
    Gte,
    /// The bitwise shift left operator. (eg. -2 << 1 is -4)
    Shl,
    /// The bitwise shift right operator. (eg. -8 >> 1 is -4)
    Shr,
    /// The unsigned bitwise shift right operator. (eg. -8 >>> 1 is 2147483644)
    UnsignedShr,
    /// (a + b)
    Plus,
    /// (a - b)
    Minus,
    /// (a * b)
    Multiply,
    /// (a / b)
    Divide,
    /// The modulo, or remainder operator. (eg. 7 % 2 is 1)
    Mod,
    /// The bitwise or operator. This does a logical or for each bit of both operands.
    /// (eg. 10 | 5 is 15, 1010 | 0101 = 1111)
    BitwiseOr,
    /// The logical or operator. This works on boolean values rather than numbers.
    /// (eg true || false is true)
    Or,
    /// The bitwise xor operator. This works by performing a logical xor for each bit of
    /// both operands. (eg. 10 ^ 6 is 12) (1010 ^ 0110 = 1100)
    BitwiseXor,
    /// The bitwise and operator. This works by performing a logical and for each bit of
    /// both operands. (eg. 10 & 6 is 2) (1010 & 0110 = 0010)
    BitwiseAnd,
    /// The logical and operator. This works on boolean values instead of numbers.
    /// (eg true && false is false)
    And,
    /// The key existence operator. This checks if a key exists in an object.
    /// eg. 'foo' in {'bar': 'baz'} is false
    In,
    /// The instanceof operator. This checks if the right hand operand exists anywhere
    /// in the prototype chain of the left hand operand.
    InstanceOf,
    /// The expoentation operator. This raises the left hand operand to the power of
    /// the right hand side. (eg 2 ** 4 is 2*2*2*2 or 16)
    Exponentiation,
}

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-assignment-operators
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    // =
    Eq,
    // +=
    PlusEq,
    // -=
    MinusEq,
    // *=
    MultiplyEq,
    // /=
    DivideEq,
    // %=
    ModEq,
    // <<=
    ShlEq,
    // >>=
    ShrEq,
    // >>>=
    UnsignedShrEq,
    // |=
    BitwiseOrEq,
    // ^=
    BitwiseXorEq,
    // &=
    BitwiseAndEq,
}

// https://facebook.github.io/jsx/
#[derive(Debug, Clone, PartialEq)]
pub enum JsxAttribute {
    JsxSpreadAttribute {
        expression: Expression,
    },
    JsxAttribute {
        name: String,
        value: Option<Expression>,
    },
}

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-statements-and-declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {}

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-scripts-and-modules
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub source_type: SourceType,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    Script,
    Module,
}
