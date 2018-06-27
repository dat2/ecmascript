// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-identifier-names
pub type Id = String;

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-lexical-grammar-literals
pub type StringLiteral = String;

pub type BooleanLiteral = bool;

#[derive(Debug, Clone, PartialEq)]
pub struct NullLiteral;

pub type NumberLiteral = f64;

#[derive(Debug, Clone, PartialEq)]
pub struct RegexLiteral {
    pub pattern: String,
    pub flags: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateElement {
    pub cooked: String,
    pub raw: String,
}

// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-ecmascript-language-expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-primary-expression
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
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-left-hand-side-expressions
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
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-update-expressions
    Update {
        operator: UpdateOperator,
        argument: Box<Expression>,
        prefix: bool,
    },
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-unary-operators
    Unary {
        operator: UnaryOperator,
        prefix: bool,
        argument: Box<Expression>,
    },
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-exp-operator
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-multiplicative-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-additive-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-bitwise-shift-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-relational-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-equality-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-binary-bitwise-operators
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-binary-logical-operators
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
    // https://www.ecma-international.org/ecma-262/8.0/index.html#sec-assignment-operators
    Assignment {
        operator: AssignmentOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    ArrowFunction {
        body: Box<Vec<Statement>>,
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
    // LANGUAGE EXTENSIONS
    // https://facebook.github.io/jsx/
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // ==
    EqEq,
    // !=
    NotEq,
    // ===
    EqEqEq,
    // !==
    NotEqEq,
    // <
    Lt,
    // <=
    Lte,
    // >
    Gt,
    // >=
    Gte,
    // <<
    Shl,
    // >>
    Shr,
    // >>>
    UnsignedShr,
    // +
    Plus,
    // -
    Minus,
    // *
    Multiply,
    // /
    Divide,
    // %
    Mod,
    // |
    BitwiseOr,
    // ||
    Or,
    // ^
    BitwiseXor,
    // &
    BitwiseAnd,
    // &&
    And,
    In,
    InstanceOf,
    // **
    Exponent,
}

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
