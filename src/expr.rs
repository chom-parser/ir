use crate::{ty, Constant, Ids, Pattern};

pub enum Error<T: Ids> {
	UnexpectedChar(Box<Expr<T>>),
	UnexpectedToken(Box<Expr<T>>),
	UnexpectedNode(Box<Expr<T>>),
}

pub enum Var<T: Ids> {
	This,
	Defined(T::Var)
}

impl<T: Ids> Clone for Var<T> {
	fn clone(&self) -> Self {
		match self {
			Self::This => Self::This,
			Self::Defined(v) => Self::Defined(*v)
		}
	}
}

/// Expression.
pub enum Expr<T: Ids> {
	/// Literal value.
	Literal(Constant),

	/// Get the value of the given variable.
	/// 
	/// This moves the value.
	/// The caller ensures that the variable is not used anymore.
	Get(Var<T>),

	/// Get a structure/enum field of the given value.
	GetField(Box<Expr<T>>, ty::Ref, u32),

	/// Set the value of the given variable.
	Let(T::Var, bool, Box<Expr<T>>, Box<Expr<T>>),

	/// Create a new instance of the given type using the given arguments.
	New(ty::Ref, BuildArgs<T>),

	/// Construct an enum variant.
	///
	/// The first parameter is the enum type index,
	/// the second the variant index.
	Cons(ty::Ref, u32, BuildArgs<T>),

	/// Put the given value on the heap.
	Heap(Box<Expr<T>>),

	/// Error expression.
	Error(Error<T>),

	/// Pattern matching.
	Match {
		expr: Box<Expr<T>>,
		cases: Vec<MatchCase<T>>,
	},

	/// Unwrap the value matching the given pattern and evaluate the next expression.
	///
	/// `PatternUnwrap(p, v, e)` is equivalent to the following bit of Ocaml-like code:
	/// ```ocaml
	/// let p = v in e
	/// ```
	///
	/// The value is guaranteed to match the input pattern.
	LetMatch(Pattern<T>, Box<Expr<T>>, Box<Expr<T>>),

	/// Tail recursion.
	///
	/// This may be compiled into a while loop (e.g. in Rust):
	/// ```pseudo
	/// while condition {
	/// 	body
	/// }
	/// ```
	///
	/// This may also be compiled into a recursive function call (e.g. in OCaml):
	/// ```ocaml
	/// let rec f args = body in f args
	/// ```
	TailRecursion {
		label: T::Label,
		args: Vec<Var<T>>,
		body: Box<Expr<T>>,
	},

	/// Recurse on the given tail recursion loop.
	///
	/// The list of argument must match the list of the `TailRecursion` arguments.
	Recurse(T::Label, Vec<Var<T>>),

	Lexer(Var<T>, LexerExpr<T>),

	Stream(Var<T>, StreamExpr<T>),

	Stack(Var<T>, StackExpr<T>),

	/// Span-related expressions.
	Span(SpanExpr<T>),

	/// Write to the output.
	Write(String, Vec<Expr<T>>),

	/// Unreachable expression.
	Unreachable,
}

/// Lexer operation.
pub enum LexerExpr<T: Ids> {
	/// Peek a character from the source char stream.
	Peek,

	/// Parse the buffer content using the given token parser (given the index of the grammar terminal).
	Parse(u32),

	/// Creates an iterator from the characters of the buffer.
	Chars,

	/// Clear the lexer and then evaluate the given expression.
	Clear(Box<Expr<T>>),

	/// Consume the next character from the source,
	/// putting it in the buffer.
	/// Then evaluate the given expression.
	Consume(Box<Expr<T>>),
}

impl<T: Ids> LexerExpr<T> {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Clear(_) | Self::Consume(_) => true,
			_ => false,
		}
	}
}

pub enum StreamExpr<T: Ids> {
	/// Get the next token from the parser and evaluate the given expression.
	///
	/// `Pull(lexer, a, e)` corresponds to the following Ocaml-like bit of code:
	/// ```ocaml
	/// let (lexer, a) = pull lexer in e
	/// ```
	Pull(T::Var, Box<Expr<T>>),
}

pub enum StackExpr<T: Ids> {
	/// Push a value on the stack and evaluate the given expression.
	///
	/// `StackPush(stack, b, c, e)` is equivalent to
	/// ```ocaml
	/// let stack = push stack (b, c) in e
	/// ```
	Push(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>),

	/// Pop one value from the stack and evaluate the given expression.
	///
	/// `StackPop(a, b, c, e)` is equivalent to the following bit of Ocaml-like code:
	/// ```ocaml
	/// let (stack, (b, c)) = pop stack in e
	/// ```
	Pop(Option<T::Var>, Option<T::Var>, Box<Expr<T>>),
}

pub enum SpanExpr<T: Ids> {
	Locate(Box<Expr<T>>, Box<Expr<T>>),

	/// Create a new span from a position.
	FromPosition(Box<Expr<T>>),

	/// Return the first position after the given span.
	After(Box<Expr<T>>),

	/// Transpose an optional located value into a located optional value.
	///
	/// The second expression gives the default span to apply when
	/// the value is `none`.
	Transpose(Box<Expr<T>>, Box<Expr<T>>),

	/// Unwrap a located value into a pair (value, span),
	/// and then evaluate an expression.
	///
	/// `UnwrapOptLoc(a, b, c, d)` is equivalent to the
	/// following Ocaml-like bit of code:
	/// ```ocaml
	/// let (a, b) = Loc.into_pair c in d
	/// ```
	Unwrap(Option<T::Var>, Option<T::Var>, Box<Expr<T>>, Box<Expr<T>>),

	/// Compute the smallest span that includes both input span parameters.
	Merge(Box<Expr<T>>, Box<Expr<T>>),
}

impl<T: Ids> Expr<T> {
	/// Checks if the expression is "continued",
	/// that is when another expression is evaluated after the operation.
	/// Such expression can be generated with a preceding `return` or `break` statement
	/// in non-functional target languages.
	///
	/// The `Recurse` and `Unreachable` expressions are also considered to be continued.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Let(_, _, _, _) | Self::Match { .. } | Self::Recurse(_, _) | Self::Unreachable => {
				true
			}
			_ => false,
		}
	}
}

pub struct MatchCase<T: Ids> {
	pub pattern: Pattern<T>,
	pub expr: Expr<T>,
}

pub enum BuildArgs<T: Ids> {
	Tuple(Vec<Expr<T>>),
	Struct(Vec<Binding<T>>),
}

impl<T: Ids> BuildArgs<T> {
	pub fn is_empty(&self) -> bool {
		match self {
			Self::Tuple(args) => args.is_empty(),
			Self::Struct(bindings) => bindings.is_empty(),
		}
	}
}

pub struct Binding<T: Ids> {
	pub id: T::Field,
	pub expr: Expr<T>,
}
