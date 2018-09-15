//! This module contains type definitions for the Abstract Syntax elements
//! that make up the ECMAScript language.
//!
//! These types have been translated from the [estree spec](https://github.com/estree/estree/blob/master/es5.md).
//!
//! The macros `build_ast` and `match_ast` are meant to be the public API of this
//! module as they abstract away the types in such a way so that the user of the library
//! feels as if they are working with source text almost directly.
use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;

/// ReferenceError represents a failure when trying to convert an expression into a pattern.
#[derive(Debug, Fail)]
pub enum ReferenceError {
    /// This represents an error when you are trying to assign an expression to the lhs that
    /// can't be assigned to. (eg. 1 = 1)
    #[fail(display = "ReferenceError: Invalid left-hand side in assignment")]
    InvalidLeftHandSide,
}

/// Position is a line and a column. The line is 1 indexed, and column is 0 indexed.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Position {
    /// The line number in the original source. This is 1 indexed.
    pub line: usize,
    /// The column number in the original source. This is 0 indexed.
    pub column: usize,
}

impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Position {
        Position { line, column }
    }
}

/// A SourceLocation is where the node starts, and ends.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct SourceLocation {
    /// The start of the syntax token / element.
    pub start: Position,
    /// The end of the syntax token / element.
    pub end: Position,
}

impl<P: Into<Position>> From<(P, P)> for SourceLocation {
    fn from((start, end): (P, P)) -> SourceLocation {
        SourceLocation {
            start: start.into(),
            end: end.into(),
        }
    }
}

/// Id is an identifier in the ecmascript language.
/// eg. `var foo = {};`
/// `foo` is the identifier.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-identifier-names).
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Identifier(pub Option<SourceLocation>, pub String);

/// This represents the Literal production of the PrimaryExpression rule.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#prod-Literal)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum Literal {
    /// This is a wrapper around the null literal.
    NullLiteral(NullLiteral),
    /// This is a wrapper around the boolean literal.
    BooleanLiteral(BooleanLiteral),
    /// This is a wrapper around the number literal.
    NumericLiteral(NumericLiteral),
    /// This is a wrapper around the string literal.
    StringLiteral(StringLiteral),
    /// This is a wrapper around the regexp literal.
    RegExpLiteral(RegExpLiteral),
}

/// NullLiteral is the syntax element for `null`.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-null-literals)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct NullLiteral;

/// BooleanLiteral is the syntax element for `true` and `false`.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-boolean-literals)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BooleanLiteral(pub bool);

/// NumericLiteral is the syntax element for numbers. The parser will convert the string
/// values into an f64 for the sake of simplicity.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-numeric-literals)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct NumericLiteral(pub f64);

/// StringLiteral is a syntax element with quotes (single or double).
/// eg. `'my string literal'` or `"my other string literal"`
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-string-literals)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct StringLiteral(pub String);

/// RegExpLiteral is the syntax element of a regular expression.
/// eg. `/abc[123]/gi`
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-literals-regular-expression-literals)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct RegExpLiteral {
    /// This is the text between the slashes.
    pub pattern: String,
    /// This is the text after the slashes. eg the `i` flag is the case insensitive flag.
    pub flags: String,
}

// programs

// functions

// statements

// expressions

/// TemplateElement is any text between interpolated expressions inside a template literal.
/// eg. ``abc ${} \u{2028}``
/// "abc " and " \u{2028}" would be the TemplateElements for this template literal.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-template-literal-lexical-components)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct TemplateElement {
    /// If the template element has any sort of escape sequences (eg. \u{2028})
    /// this will represent the evaluated result of that sequence.
    /// eg. if raw == "\u{41}", cooked = "A"
    pub cooked: Option<String>,
    /// This will store the exact string value, before being evaluted into the unicode
    /// code points.
    pub raw: String,
    /// This is the source location of the template element
    pub loc: Option<SourceLocation>,
}

/// Expression is an enumeration of all possible expressions merged into one big enum.
/// This also includes language extensions, such as JSX.
///
/// This represents all possible computations that can be done in the ecmascript language.
///
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-expressions)
/// [Primary Expression](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-primary-expression)
/// [Left Hand Side Expressions](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-left-hand-side-expressions)
/// [Update Expressions](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-update-expressions)
/// [JSX Specification](https://facebook.github.io/jsx/)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Expression {
    /// An identifier can also be a primary expression.
    Identifier {
        /// The actual identifier name.
        name: String,
        /// The source location of the expression.
        loc: Option<SourceLocation>,
    },
    /// This is all literals minus the regex literal and the template literal.
    Literal {
        /// This is the value of the literal expression.
        value: Literal,
        /// This is the location where the expression happens.
        loc: Option<SourceLocation>,
    },
    /// The 'this' keyword is a primary expression.
    ThisExpression {
        /// The source location of the expression.
        loc: Option<SourceLocation>,
    },
    /// This is an expression created with [] brackets.
    ArrayExpression {
        /// This is the list of elements in the array expression.
        elements: Vec<ExpressionListItem>,
        /// This is the location where the expression starts.
        loc: Option<SourceLocation>,
    },
    /// This is an expression created by using {} brackets.
    ObjectExpression {
        /// This is the list of properties for the object.
        properties: Vec<ObjectExpressionProperty>,
        /// This is the location where the expression starts.
        loc: Option<SourceLocation>,
    },
    /// A function expression is a function defined in an expression position.
    /// Arrow functions are one where the body is a single statement that is an expression
    /// statement.
    FunctionExpression {
        /// A function expression can be anonymous, where it has no name.
        id: Option<Identifier>,
        /// The formal parameters to a function.
        params: Vec<Pattern>,
        /// The body is a list of statements. This can include pragmas.
        body: Vec<FunctionBodyStatement>,
        /// This is true if the function was defined with the `async` keyword before the
        /// `function` keyword.
        async: bool,
        /// This is true if there is a `*` character after the `function` keyword.
        generator: bool,
    },
    /// A unary expression is a unary operator in prefix position to the operand.
    UnaryExpression {
        /// The operator is one that can only take a single operand.
        operator: UnaryOperator,
        /// This is just for estree.
        prefix: bool,
        /// The expression is the operand that is passed to the operator.
        argument: Box<Expression>,
    },
    /// An update expression is either a postfix or prefix, increment or decrement, operator
    /// applied to an operand.
    UpdateExpression {
        /// The operator is either ++ or --
        operator: UpdateOperator,
        /// The argument is another expression, eg. (++(a))
        argument: Box<Expression>,
        /// This tells you if the operator is in prefix or postfix position.
        prefix: bool,
    },
    /// The binary expression is one of the form (lhs operand rhs).
    BinaryExpression {
        /// The operand that is infixed between the operands.
        operator: BinaryOperator,
        /// The left hand side.
        left: Box<Expression>,
        /// The right hand side.
        right: Box<Expression>,
    },
    /// An assignment operator is one of the form (lhs assigned rhs). This changes the left hand
    /// side of the expression by applying an operator to the right hand side and the left hand
    /// side to get the new value of the left hand side.
    AssignmentExpression {
        /// The operator that is between the operands. This is slightly different to the binary
        /// expression, as it changes the LHS. The binary operators will return a new value
        /// instead of changing the left hand side.
        operator: AssignmentOperator,
        /// The expression that gets changed in some way. eg. (id = some_new_value)
        left: Box<Pattern>,
        /// The expression that changes the lhs.
        right: Box<Expression>,
    },
    /// The logical expression is a binary expression for logical operators only
    LogicalExpression {
        /// The operand that is infixed between the operands.
        operator: LogicalOperator,
        /// The left hand side.
        left: Box<Expression>,
        /// The right hand side.
        right: Box<Expression>,
    },
    /// A member expression is a property access expression.
    /// Eg. `obj.key` or `obj[computed_key]`
    MemberExpression {
        /// The object we're trying to access.
        object: Box<SuperExpression>,
        /// The property we're trying to access. It can be computed, or a basic
        /// IdReference.
        property: Box<Expression>,
        /// This is true if the rhs was written with `[]` notation.
        computed: bool,
    },
    /// The ternary operator. This is of the form (test ? alternate : consequent)
    /// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-conditional-operator)
    ConditionalExpression {
        /// The expression before the ?. This must evaluate to a truthy or falsy value.
        test: Box<Expression>,
        /// The expression returned if the test expression is truthy.
        alternate: Box<Expression>,
        /// The expression returned if the test expression is falsy.
        consequent: Box<Expression>,
        /// The location in code where this expression is found.
        loc: Option<SourceLocation>,
    },
    /// This is a regular function call, eg. `myFunction(expr1, expr2)`
    CallExpression {
        /// The callee is the function we're trying to call. It may be an IIFE (immediately
        /// invoked function expression) or any other dynamic function.
        callee: Box<SuperExpression>,
        /// The list of parameters to pass to the function.
        arguments: Vec<ExpressionListItem>,
    },
    /// This is the `new MemberExpression` expression. It will construct the callee
    /// and return an object.
    NewExpression {
        /// The callee is the function we are trying to construct.
        callee: Box<Expression>,
        /// The arguments is a list of parameters to the function we're trying to construct.
        arguments: Vec<ExpressionListItem>,
    },
    /// This represents a comma expression, eg. (a, b). This will evaluate the first operand,
    /// throw it away, and return the second operand.
    ///
    /// For a list of operands, it will evaluate all operands, throw them away, and then
    /// finally return the last operand.
    ///
    /// This is mainly useful for side effects, eg. (console.log(expr), expr).
    /// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-comma-operator)
    SequenceExpression {
        /// This is the list of expressions separated by a comma.
        expressions: Vec<Expression>,
    },
    /// An arrow function expression is one that binds "this" automatically.
    ArrowFunctionExpression {
        /// The body can either be a list of statements or an expression.
        body: Box<ArrowFunctionBody>,
        /// This tells you if the arrow function is one that returns an expression
        expression: bool,
    },
    /// The yield expression that is only valid inside a generator function.
    /// It is a syntax error if there is a yield expression in the body of a non generator
    /// function.
    Yield {
        /// The generator may yield an expression to the caller, while requesting the caller to
        /// give back another value.
        argument: Option<Box<Expression>>,
        /// If the argument is another generator function, they must delegate all their yields to
        /// until the delegate generator completes.
        delegate: bool, // yield *
    },
    /// A Template literal expression has many template elements with expressions littered
    /// between.
    ///
    /// When a template literal gets passed to the tagged template, it usually gets split into
    /// the quasis (the pieces between the interpolated expressions) as an array for the first
    /// argument, and the expressions get spread into the rest of the function call.
    ///
    /// For the sake of simplicity, we are not representing this in the AST.
    TemplateLiteral {
        /// Quasis includes all strings parsed by the template literal.
        quasis: Vec<TemplateElement>,
        /// All expressions that are interpolated into the final string.
        expressions: Vec<Expression>,
        /// This is the location where the expression starts.
        loc: Option<SourceLocation>,
    },
    /// This is an expression where we pass the elements of the template literal to the
    /// tag function.
    ///
    /// eg.
    /// ```javascript
    /// function tag(stringParts, expr1, expr2) {
    ///
    /// }
    /// tag`123 ${}`
    /// ```
    TaggedTemplateExpression {
        /// This is the function we're trying to pass the template elements to.
        tag: Box<Expression>,
        /// The only expression that is valid for the quasi is another TemplateLiteral
        quasi: Box<Expression>,
    },
    /// An await expression is an expression that can be used inside an async function.
    AwaitExpression {
        /// The expression that is being awaited for.
        argument: Box<Expression>,
        /// The source location of the expression.
        loc: Option<SourceLocation>,
    },
    /// This is the `new.target` expression that was introduced in ES2015. This
    /// tells you if the function was called with the `new` operator.
    MetaProperty,
    // Class,
    /// *NOTE*: This is an extension to the language proposed by facebook.
    /// The JsxElement is an inlined expression of the form:
    /// <name key={value}>
    /// The JsxElement must be matched by a closing element, or else it is a syntax error.
    JSXElement {
        /// The name of the element to construct.
        name: String,
        /// The key={value} pairs.
        attributes: Vec<JsxAttribute>,
        /// The child elements.
        children: Vec<Expression>,
        /// The source location of the element.
        loc: Option<SourceLocation>,
    },
    ///*NOTE*: This is an extension to the language proposed by facebook.
    /// This is an anonymous JsxElement, used when you want to return an array of
    /// elements without actually wrapping things into an unneeded DOM element.
    JsxFragment {
        /// The child expressions of the fragment
        children: Vec<Expression>,
        /// The source location in code
        loc: Option<SourceLocation>,
    },
}

impl Expression {
    /// This allows us to convert an expression into a pattern, in the case of assignment
    /// expression. We need to parse a LeftHandSideExpression, but it needs to be coerced into a
    /// Pattern
    pub fn into_pattern(self) -> Result<Pattern, ReferenceError> {
        match self {
            Expression::Identifier { name, loc } => Ok(Pattern::Identifier { name, loc }),
            _ => Err(ReferenceError::InvalidLeftHandSide),
        }
    }
}

/// A pattern is any way you can destructure an object or array.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ExpressionListItem {
    /// This is just a regular expression.
    Expression(Expression),
    /// This prevents a spread expression from being in an invalid syntax tree.
    Spread(SpreadElement),
    /// This is to support elision on array elements.
    #[serde(rename = "null")]
    Null,
}

/// A spread element is something that be spread, eg. an array or an object.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum SpreadElement {
    /// This is an element that is being spread into another expression.
    SpreadElement {
        /// The expression to spread into another expression.
        argument: Expression,
        /// The location of the spread element.
        loc: Option<SourceLocation>,
    },
}

/// An object property is a tuple of a key, value, and a tag representing what kind of
/// property it is.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Property {
    /// The key can be a computed expression, or an id reference.
    pub key: Expression,
    /// The value can be any sort of expression.
    pub value: Expression,
    /// The kind tells us if this is a getter, setter, or basic initializer.
    pub kind: PropertyKind,
    /// This tells us if the property was defined as a shorthand function expression.
    pub method: bool,
    /// This tells us if the key and value were exactly the same as an Identifier.
    pub shorthand: bool,
    /// This tells us if the key is more than just a basic literal or Identifier.
    pub computed: bool,
    /// This tells us where the property is defined.
    pub loc: Option<SourceLocation>,
}

/// An object property can be a getter, setter, or basic initializer.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum PropertyKind {
    /// This just means the value is initialized to the expression. This is the default.
    Init,
    /// This means the value is a function that gets called when you try to access
    /// the key in the object. This allows you to return a dynamic value at property
    /// access time.
    Get,
    /// This means the value is a function that gets called when you try to
    /// set the property in the object.
    Set,
}

/// An object expression property can be a property or a spread property.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ObjectExpressionProperty {
    /// A regular property being added to an object expression
    Property(Property),
    /// This allows you to spread into an object literal.
    SpreadElement(SpreadElement),
}

/// A pattern is any way you can destructure an object or array.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Pattern {
    /// This is a regular identifier pattern.
    Identifier {
        /// The identifier itself.
        name: String,
        /// The location of the identifier.
        loc: Option<SourceLocation>,
    },
    /// This allows you to destructure objects.
    ObjectPattern {
        /// The properties that are being destructured.
        properties: Vec<ObjectPatternProperty>,
        /// The location of the object pattern
        loc: Option<SourceLocation>,
    },
    /// This allows you to destructure arrays.
    ArrayPattern {
        /// The sub patterns in the destructured array.
        elements: Vec<Pattern>,
        /// The location of the pattern in source code.
        loc: Option<SourceLocation>,
    },
    /// This allows you to collect the "rest" of properties or elements
    /// in an array into a single parameter.
    /// This is only allowed within the Array or Object patterns.
    RestElement {
        /// The pattern (eg. identifier) that is being collected.
        argument: Box<Pattern>,
        /// The location of the pattern in code
        loc: Option<SourceLocation>,
    },
    /// This allows you to set a default value for a pattern.
    /// eg. const { x = 1 }
    AssignmentPattern {
        /// The pattern that you are setting a default for.
        /// It is a syntax error for the pattern to be a Rest pattern.
        left: Box<Pattern>,
        /// The value you set the default to.
        argument: Expression,
    },
}

/// This is a restricted version of a Property that only allows patterns as the value.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum ObjectPatternProperty {
    /// The regular property.
    #[serde(rename = "Property")]
    AssignmentProperty {
        /// The key can still be an id reference, or computed.
        key: Expression,
        /// The value however is now another pattern.
        value: Pattern,
    },
    /// The object pattern can be spread.
    RestElement {
        /// This is the pattern to be spread into.
        argument: Box<Pattern>,
        /// This is the source location of the spread property.
        loc: Option<SourceLocation>,
    },
}

/// A template literal element can either be the string between backticks and `${`
/// or the expression between `${` and `}`.
/// This is easier than trying to re-construct the order.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum TemplateLiteralElement {
    /// A TemplateElement is the strings between the interpolated expressions.
    TemplateElement(TemplateElement),
    /// The expressions that are interpolated into the final value of the string.
    Expression(Expression),
}

/// These operators take 1 operand, update the operands mathematical value in the background,
/// then return an updated version of the operand.
///
/// If the operator is in postfix position, it returns the old value of the operand.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum UpdateOperator {
    /// This will add 1 to the mathematical value of the operand. eg (a++ or ++a)
    Increment,
    /// This will subtract 1 from the mathematical value of the operand eg. (a-- or --a)
    Decrement,
}

/// These operators take 1 operand, and are a prefix of the operand.
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-unary-operators)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    /// Reverse the sign on the operand. This will do type coercion first.
    /// eg. (-1)
    Minus,
    /// Make the operand a positive number. This will do type coercion first.
    /// eg (+(-1) is 1)
    Plus,
    /// Logically reverse the operand. This will do type coercion first.
    /// eg. (!true is false)
    Not,
    /// Logcally reverse all the bits on the operand. This will do type coercion first.
    /// eg (~9 is -10) (the sign bit is also reversed)
    BitwiseNot,
    /// Check the internal type of the operand, and return a string that represents the type.
    /// eg (typeof {}) is 'object'
    Typeof,
    /// This operator will evaluate the operand, and then return undefined itself.
    /// This can be used for invoke a function epxression immediately for example.
    Void,
    /// This operator will remove a property from an object. It will return true when
    /// the property was successfully deleted, and false when it wasnt.
    Delete,
}

/// All the operators that have 2 arguments are merged into one big enum here for simplicity
/// sake.
///
/// - [Multiplicative Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-multiplicative-operators)
/// - [Additive Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-additive-operators)
/// - [Bitwise Shift Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-bitwise-shift-operators)
/// - [Relational Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-relational-operators)
/// - [Equality Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-equality-operators)
/// - [Bitwise Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-binary-bitwise-operators)
/// - [Logical Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-binary-logical-operators)
/// - [Exponentiation Operator](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-exp-operator)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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

/// Assignment operators are ones that signify a chnage to the left hand side of the expression.
///
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-assignment-operators)
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    /// The basic assignment statement. This changes the left hand side to become a
    /// copy of the right hand side. (eg. a = 1)
    Eq,
    /// This is shorthand for `lhs = lhs + rhs`. (eg a += 5).
    PlusEq,
    /// This is shorthand for `lhs = lhs - rhs`. (eg a -= 5).
    MinusEq,
    /// This is shorthand for `lhs = lhs * rhs`. (eg a *= 5).
    MultiplyEq,
    /// This is shorthand for `lhs = lhs / rhs`. (eg a /= 5).
    DivideEq,
    /// This is shorthand for `lhs = lhs % rhs`. (eg a %= 5).
    /// This is useful when the remainder of a division is more important than the division
    /// itself.
    ModEq,
    /// This is shorthand for `lhs = lhs << rhs`. (eg a <<= 5).
    /// This is useful when you want to shift all the bits of a variable
    /// without storing a copy of the variable.
    ShlEq,
    /// This is shorthand for `lhs = lhs >> rhs`. (eg a >>= 5).
    /// This is useful when you want to shift all the bits of a variable
    /// without storing a copy of the variable.
    ShrEq,
    /// This is shorthand for `lhs = lhs >>> rhs`. (eg a >>>= 5).
    /// The difference is that this will not preserve the minus sign of a number, like
    /// the >>= operation would.
    UnsignedShrEq,
    /// This is shorthand for `lhs = lhs | rhs`. (eg a |= 5).
    BitwiseOrEq,
    /// This is shorthand for `lhs = lhs ^ rhs`. (eg a ^= 5).
    BitwiseXorEq,
    /// This is shorthand for `lhs = lhs & rhs`. (eg a &= 5).
    BitwiseAndEq,
    /// The expoentation operator. This raises the left hand operand to the power of
    /// the right hand side. (eg 2 ** 4 is 2*2*2*2 or 16)
    ExponentiationEq,
}

impl Serialize for AssignmentOperator {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let string = match self {
            AssignmentOperator::Eq => "=",
            AssignmentOperator::PlusEq => "+=",
            AssignmentOperator::MinusEq => "-=",
            AssignmentOperator::MultiplyEq => "*=",
            AssignmentOperator::DivideEq => "/=",
            AssignmentOperator::ModEq => "%=",
            AssignmentOperator::ShlEq => "<<=",
            AssignmentOperator::ShrEq => ">>=",
            AssignmentOperator::UnsignedShrEq => ">>>=",
            AssignmentOperator::BitwiseOrEq => "|=",
            AssignmentOperator::BitwiseXorEq => "^=",
            AssignmentOperator::BitwiseAndEq => "&=",
            AssignmentOperator::ExponentiationEq => "**=",
        };
        serializer.serialize_str(string)
    }
}

struct AssignmentOperatorVisitor;

impl<'de> Visitor<'de> for AssignmentOperatorVisitor {
    type Value = AssignmentOperator;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("one of =")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match value {
            "=" => Ok(AssignmentOperator::Eq),
            "+=" => Ok(AssignmentOperator::PlusEq),
            "-=" => Ok(AssignmentOperator::MinusEq),
            "*=" => Ok(AssignmentOperator::MultiplyEq),
            "/=" => Ok(AssignmentOperator::DivideEq),
            "%=" => Ok(AssignmentOperator::ModEq),
            "<<=" => Ok(AssignmentOperator::ShlEq),
            ">>=" => Ok(AssignmentOperator::ShrEq),
            ">>>=" => Ok(AssignmentOperator::UnsignedShrEq),
            "|=" => Ok(AssignmentOperator::BitwiseOrEq),
            "^=" => Ok(AssignmentOperator::BitwiseAndEq),
            "&=" => Ok(AssignmentOperator::BitwiseAndEq),
            "**=" => Ok(AssignmentOperator::ExponentiationEq),
            _ => Err(E::custom(format!("{} is not an operator", value))),
        }
    }
}

impl<'de> Deserialize<'de> for AssignmentOperator {
    fn deserialize<D>(deserializer: D) -> Result<AssignmentOperator, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(AssignmentOperatorVisitor)
    }
}

/// All the operators that have 2 arguments are merged into one big enum here for simplicity
/// sake.
///
/// - [Logical Operators](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-binary-logical-operators)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    /// The logical or operator. This works on boolean values rather than numbers.
    /// (eg true || false is true)
    Or,
    /// The logical and operator. This works on boolean values instead of numbers.
    /// (eg true && false is false)
    And,
}

/// A JSX attribute is either a simple `key={value}` attribute, or a
/// spread of an object containing multiple attributes.
///
/// [Reference](https://facebook.github.io/jsx/)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum JsxAttribute {
    /// Spread an objects key value pairs into the JSX object as well.
    JsxSpreadAttribute {
        /// The expression could be typed more strictly into an ID Reference or an inline
        /// object, but for the sake of simplicity we reference the larger enum.
        expression: Expression,
    },
    /// A single `key={value}` pair. The value is optional, and if missing it means
    /// the existence of the key is more important than the value of the key.
    JsxAttribute {
        /// The key of the attribute.
        name: String,
        /// The optional value. If it is None, then it means the value is a boolean true.
        /// The absence of a key can mean false.
        value: Option<Expression>,
    },
}

/// This is a super call or object.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Super {
    /// The enum is just a hack to get "type: Super" in the estree output.
    #[serde(rename_all = "camelCase")]
    Super {
        /// The location of the super call.
        loc: Option<SourceLocation>,
    },
}

/// This is a super call or object.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum SuperExpression {
    /// This means the expression was a super call
    Super(Super),
    /// This means the expression was a regular expression
    Expression(Expression),
}

/// A function body is either a statement or a directive.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum FunctionBodyStatement {
    /// A statement.
    Statement(Statement),
}

/// This represents what the arrow function body can be
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ArrowFunctionBody {
    /// The arrow function body can be a list of statements
    FunctionBody(Vec<FunctionBodyStatement>),
    /// The arrow function can just return a single expression immediately
    Expression(Expression),
}

/// A statement is either a declaration (var, const, let, function, export) or an
/// instruction to the interpreter to evaluate an expression.
/// For the sake of simplicity, declarations will get merged into this struct as well.
///
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-statements-and-declarations)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Statement {
    /// An expression statement.
    ExpressionStatement {
        /// The expression that the statement contains.
        expression: Expression,
        /// The source location in code where this statement starts.
        loc: Option<SourceLocation>,
    },
    /// VariableDeclaration represents the statement that defines a variable, and possibly its
    /// value. eg "var array = []"
    VariableDeclaration {
        /// Multiple variables can be defined with a single "var", eg. "var x, y, z"
        declarations: Vec<VariableDeclarator>,
        /// This represents whether it was a "let", "const", or "var"
        kind: VariableDeclarationKind,
        /// The source location in code where this statement starts
        loc: Option<SourceLocation>,
    },
}

/// VariableDeclarator is a struct that has a pattern and an initializer.
/// The pattern can be an identifier, or an object destructuring pattern or array destructuring
/// pattern.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct VariableDeclarator {
    /// This is the "left hand side" of the declaration, eg "var x = []", "x" is the id.
    pub id: Pattern,
    /// This is the "right hand side", or "initial value" of the variable being declared.
    pub init: VariableDeclaratorInit,
}

/// This enum is internal, to maintain estree compatibility
/// Alternatively, we could write a custom serializer / deserializer to map "null" to an Option,
/// but this was easier in the short term.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum VariableDeclaratorInit {
    /// estree defines the init as being an expression or "null"
    #[serde(rename = "lowercase")]
    Null,
    /// The declarator is usually an expression.
    Expression(Expression),
}

/// This represents the different kinds of declarations that can occur, eg. let, const or var.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged, rename_all = "lowercase")]
pub enum VariableDeclarationKind {
    /// Var is a variable that is function scoped.
    Var,
    /// Let is a variable that can be re-assigned, but is block scoped.
    Let,
    /// Const is a variable that can't be re-assigned, but is not internally immutable, and is
    /// block scoped.
    Const,
}

/// This is the main entry point to the syntax tree. A program is a list of statements,
/// and statements include declarations.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Program {
    /// There is only one enum possible for program.
    #[serde(rename_all = "camelCase")]
    Program {
        /// The list of statements or declarations made by the source text.
        body: Vec<Statement>,
        /// This represents how the source is parsed. A module is parsed in strict mode, which
        /// disallows things in the parser level earlier on.
        source_type: SourceType,
        /// The location of the entire program.
        loc: Option<SourceLocation>,
    },
}

/// This enum represents whether or not the source code contains an ECMAScript module.
/// An ECMAScript module can have import and export declarations in it, and has some
/// other subtle behaviour differences.
///
/// [Reference](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-ecmascript-language-scripts-and-modules)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SourceType {
    /// The source text has no import or export declarations.
    Script,
    /// The source text has import or export declarations, and can be executed
    /// differently than a regular script.
    Module,
}
