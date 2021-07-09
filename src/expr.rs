use derivative::Derivative;
use crate::{ty, Constant, Ids, Pattern};

pub enum Error<T: Ids> {
	UnexpectedToken(Box<Expr<T>>),
	UnexpectedNode(Box<Expr<T>>),
}

#[derive(Derivative)]
#[derivative(Clone, Copy, PartialEq, Eq)]
pub enum Var<T: Ids> {
	This,
	Defined(T::Var)
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
	GetField(T::Var, ty::Ref, u32),

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

	/// Call the given function with an optional object
	/// that will serve as `this`.
	Call(u32, Option<(T::Var, bool)>, Vec<Expr<T>>),

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

	/// Error value.
	Error(Error<T>),

	/// Unreachable expression.
	Unreachable,
}

impl<T: Ids> Expr<T> {
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
		Self::New(ty::Ref::Native(ty::Native::Span), Vec::new())
	}

	pub fn empty_stack() -> Self {
		Self::New(ty::Ref::Native(ty::Native::Stack), Vec::new())
	}
}

/// Lexer operation.
pub enum LexerExpr<T: Ids> {
	/// Peek a character from the source char stream.
	Peek,

	/// Return that current span stored in the lexer.
	Span,

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
			_ => false
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

impl<T: Ids> SpanExpr<T> {
	/// Checks if the operation is "continued",
	/// that is when an expression is evaluated after the operation.
	pub fn is_continued(&self) -> bool {
		match self {
			Self::Unwrap(_, _, _, _) => true,
			_ => false,
		}
	}
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

pub struct MatchCase<T: Ids> {
	pub pattern: Pattern<T>,
	pub expr: Expr<T>,
}

// pub enum BuildArgs<T: Ids> {
// 	Tuple(Vec<Expr<T>>),
// 	Struct(Vec<Binding<T>>),
// }

// impl<T: Ids> BuildArgs<T> {
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

// pub struct Binding<T: Ids> {
// 	pub id: T::Field,
// 	pub expr: Expr<T>,
// }
