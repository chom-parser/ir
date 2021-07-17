use derivative::Derivative;
use crate::{ty, Constant, Namespace, Pattern};

#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum Error<T: Namespace + ?Sized> {
	UnexpectedToken(Box<Expr<T>>),
	UnexpectedNode(Box<Expr<T>>),
}

// #[derive(Derivative)]
// #[derivative(Clone(bound=""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""))]
// pub enum Var<T: Namespace + ?Sized> {
// 	This,
// 	Defined(T::Var)
// }

// impl<T: Namespace + ?Sized> T::Var {
// 	pub fn eq_defined(&self, x: T::Var) -> bool {
// 		match self {
// 			Self::This => false,
// 			Self::Defined(y) => x == *y
// 		}
// 	}
// }

/// Expression.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum Expr<T: Namespace + ?Sized> {
	/// Literal value.
	Literal(Constant),

	/// Get the value of the given variable.
	/// 
	/// This moves the value.
	/// The caller ensures that the variable is not used anymore.
	Get(T::Var),

	/// Get a structure/enum field of the given value.
	GetField(T::Var, ty::Ref, u32),

	/// Create a reference to the given variable.
	Ref(T::Var),

	/// Create a reference to the given variable field.
	RefField(T::Var, u32),

	/// Declare a new variable initialized with the given expression,
	/// then evaluate the next expression.
	/// 
	/// The boolean specifies if the variable is mutable or not
	/// (if it can/will be updated using the `Update` expression).
	Let(T::Var, bool, Box<Expr<T>>, Box<Expr<T>>),

	/// Update the given variable.
	/// 
	/// It must have first been declared as mutable using `Let`.
	Update(T::Var, Box<Expr<T>>, Box<Expr<T>>),

	/// Create a new instance of the given type using the given arguments.
	New(ty::Ref, Vec<Expr<T>>),

	/// Construct an enum variant.
	///
	/// The first parameter is the enum type index,
	/// the second the variant index.
	Cons(ty::Ref, u32, Vec<Expr<T>>),

	/// Put the given value on the heap.
	Heap(Box<Expr<T>>),

	/// If-then-else.
	If(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>),

	/// Pattern matching.
	Match {
		expr: Box<Expr<T>>,
		cases: Vec<MatchCase<T>>,
	},

	/// Reference pattern matching.
	MatchRef {
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

	/// Call the given function with an optional object
	/// that will serve as `this`.
	Call(u32, Vec<Expr<T>>),

	/// Return from a function with the given values.
	/// 
	/// This is required to return multiple values at once.
	Return(Vec<Expr<T>>),

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
		args: Vec<(T::Var, bool)>,
		body: Box<Expr<T>>,
	},

	/// Recurse on the given tail recursion loop.
	///
	/// The list of argument must match the list of the `TailRecursion` arguments.
	Recurse(T::Label, Vec<T::Var>),

	Lexer(T::Var, LexerExpr<T>),

	Stream(T::Var, StreamExpr<T>),

	Stack(T::Var, StackExpr<T>),

	/// Span-related expressions.
	Span(SpanExpr<T>),

	/// Print to the error output.
	/// 
	/// This is only useful for debug.
	Print(String, Box<Expr<T>>),

	/// Write the given string to the output,
	/// and then evaluate the given expression.
	/// 
	/// Updates the output.
	Write(T::Var, String, Box<Expr<T>>),

	/// Format the given variable (second parameter) to
	/// the given output (first variable) using its defined
	/// debug formatting method.
	/// Then evaluate the given expression.
	/// 
	/// Updates the output.
	DebugFormat(T::Var, Box<Expr<T>>, Box<Expr<T>>),

	/// Check that the first given result value is
	/// not an error.
	/// 
	/// If it is, then immediatly return this value.
	/// If not, put it in the given variable and
	/// evaluate the next expression.
	Check(T::Var, Box<Expr<T>>, Box<Expr<T>>),

	/// Error value.
	Error(Error<T>),

	/// Unreachable expression.
	Unreachable,
}

impl<T: Namespace + ?Sized> Expr<T> {
	pub fn none() -> Self {
		Self::Cons(ty::Ref::Native(ty::Native::Option), 0, Vec::new())
	}

	pub fn some(value: Self) -> Self {
		Self::Cons(ty::Ref::Native(ty::Native::Option), 1, vec![value])
	}

	pub fn ok(value: Self) -> Self {
		Self::Cons(ty::Ref::Native(ty::Native::Result), 0, vec![value])
	}

	pub fn err(value: Self) -> Self {
		Self::Cons(ty::Ref::Native(ty::Native::Result), 1, vec![value])
	}

	pub fn heap(value: Self) -> Self {
		Self::Heap(Box::new(value))
	}

	pub fn locate(value: Self, span: Self) -> Self {
		Self::Span(SpanExpr::Locate(Box::new(value), Box::new(span)))
	}

	pub fn nowhere() -> Self {
		Self::New(ty::Ref::Native(ty::Native::Position), Vec::new())
	}

	pub fn empty_stack() -> Self {
		Self::New(ty::Ref::Native(ty::Native::Stack), Vec::new())
	}
}

/// Lexer operation.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum LexerExpr<T: Namespace + ?Sized> {
	/// Peek a character from the source char stream.
	/// 
	/// This returns a `Result` that is either an error
	/// if the lexer failed to peek the next character,
	/// or an `Result::Ok` with the character.
	Peek,

	/// Return that current span stored in the lexer.
	Span,

	/// Checks if the lexer is empty.
	IsEmpty,

	/// Creates an iterator from the characters of the buffer.
	Chars,

	/// Returns (a reference to) the buffer content.
	Buffer,

	/// Clear the lexer and then evaluate the given expression.
	Clear(Box<Expr<T>>),

	/// Consume the next character from the source,
	/// putting it in the buffer.
	/// 
	/// Returns either a `Result::Err` if the lexer failed
	/// to pull the next character,
	/// or evaluates the given expression.
	Consume(Box<Expr<T>>),
}

impl<T: Namespace + ?Sized> LexerExpr<T> {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Clear(_) | Self::Consume(_) => true,
			_ => false
		}
	}
}

#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum StreamExpr<T: Namespace + ?Sized> {
	/// Get the next token from the parser and evaluate the given expression.
	///
	/// `Pull(lexer, a, e)` corresponds to the following Ocaml-like bit of code:
	/// ```ocaml
	/// let (lexer, a) = pull lexer in e
	/// ```
	Pull(T::Var, Box<Expr<T>>),
}

#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum StackExpr<T: Namespace + ?Sized> {
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

#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum SpanExpr<T: Namespace + ?Sized> {
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

impl<T: Namespace + ?Sized> SpanExpr<T> {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Unwrap(_, _, _, _) => true,
			_ => false,
		}
	}
}

impl<T: Namespace + ?Sized> Expr<T> {
	/// Checks if the expression is "continued",
	/// that is when another expression is evaluated after the operation.
	/// Such expression can be generated with a preceding `return` or `break` statement
	/// in non-functional target languages.
	///
	/// The `Recurse` and `Unreachable` expressions are also considered to be continued.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Let(_, _, _, _) | Self::Update(_, _, _) | Self::Match { .. } | Self::LetMatch(_, _, _) | Self::Recurse(_, _) | Self::Unreachable => {
				true
			}
			Self::Lexer(_, e) => e.is_continued(),
			Self::Stream(_, _) => true,
			Self::Stack(_, _) => true,
			Self::Span(e) => e.is_continued(),
			_ => false,
		}
	}
}

#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub struct MatchCase<T: Namespace + ?Sized> {
	pub pattern: Pattern<T>,
	pub expr: Expr<T>,
}

// pub enum BuildArgs<T: Namespace + ?Sized> {
// 	Tuple(Vec<Expr<T>>),
// 	Struct(Vec<Binding<T>>),
// }

// impl<T: Namespace + ?Sized> BuildArgs<T> {
// 	pub fn empty() -> Self {
// 		BuildArgs::Tuple(Vec::new())
// 	}

// 	pub fn is_empty(&self) -> bool {
// 		match self {
// 			Self::Tuple(args) => args.is_empty(),
// 			Self::Struct(bindings) => bindings.is_empty(),
// 		}
// 	}
// }

// pub struct Binding<T: Namespace + ?Sized> {
// 	pub id: T::Field,
// 	pub expr: Expr<T>,
// }
