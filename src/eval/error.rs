use std::fmt;
use crate::{
	Ident,
	ty
};

/// Error occuring during evaluation.
pub struct Error {
	desc: Desc
}

impl Error {
	pub fn new(d: Desc) -> Self {
		panic!("abort: {}", d);
		Self {
			desc: d
		}
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.desc.fmt(f)
	}
}

impl fmt::Debug for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.desc.fmt(f)
	}
}

/// Error description.
#[derive(Debug)]
pub enum Desc {
	/// IO error.
	IO(std::io::Error),

	/// No `this`.
	NoThis,

	/// Value has been moved and cannot be borrowed.
	ValueMoved,

	/// Value has already been moved.
	ValueAlreadyMoved,

	CannotMoveOut,

	/// Undefined variable.
	UnboundVariable(Ident),

	/// Attempted to get a struct/enum field from a value that
	/// is not a struct/enum variant.
	NotAnInstance,

	/// Type missmatch detected.
	IncompatibleType,

	/// Trying to get the field of a non structure type.
	GetFieldFromNonStruct,

	/// Undefined field.
	UndefinedField(u32),

	/// Trying to update non mutable variable.
	NotMutable,

	/// Trying to instanciate a non-struct type.
	NotAStructType(ty::Ref),

	/// Trying to instanciate a non-enum type.
	NotAnEnumType(ty::Ref),

	/// The number of given field at instanciation
	/// does not match the type definition.
	/// 
	/// The first parameter is the number of provided values.
	/// The second is the number of expected values.
	InvalidFieldCount(u32, u32),

	/// No match.
	NoMatch,

	/// Let match failed.
	PatternMissmatch(String, String),

	/// Trying to bind with a pattern union.
	BindingPatternUnion,

	/// Trying to construct a variant of a type that
	/// is not an enum.
	NotAnEnumVariant,

	/// Unimplemented function cannot be evaluated.
	UnimplementedFunction,

	/// Invalid number of arguments.
	/// 
	/// The first parameter is the number of provided values.
	/// The second is the number of expected values.
	InvalidNumberOfArguments(u32, u32),

	/// Unreachable label given to `recurse` expression.
	UnreachableLabel,

	/// The arguments given to `recurse` does not match the
	/// ones specified on the label.
	RecursionArgsMissmatch,

	/// `this` cannot be borrowed as mutable.
	ImmutableThis,

	/// `unreachable` expression reached.
	UnreachableReached,

	/// Attempted to pop an empty stack.
	EmptyStack,

	/// A lexer type has no lexer function.
	UndefinedLexerMethod,

	/// No lexer defined in the program.
	NoLexer,

	/// The value is not a stream.
	NotAStream
}

impl fmt::Display for Desc {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::IO(e) => write!(f, "IO error: {}", e),
			Self::NoThis => write!(f, "method called without an object (`this` is undefined)"),
			Self::ValueMoved => write!(f, "cannot borrow moved value"),
			Self::ValueAlreadyMoved => write!(f, "value has already been moved"),
			Self::CannotMoveOut => write!(f, "cannot move out of borrowed value"),
			Self::UnboundVariable(id) => write!(f, "unbound variable `{}`", id),
			Self::NotAnInstance => write!(f, "attempted to get a struct field from a non struct type instance"),
			Self::IncompatibleType => write!(f, "type missmatch"),
			Self::GetFieldFromNonStruct => write!(f, "attempted to get a struct field from an enum type instance"),
			Self::UndefinedField(_) => write!(f, "undefined field"),
			Self::NotMutable => write!(f, "attempted to override non mutable variable"),
			Self::NotAStructType(_) => write!(f, "attempted to instanciate a non struct type"),
			Self::NotAnEnumType(_) => write!(f, "attempted to instanciate a non enum type using a variant"),
			Self::InvalidFieldCount(expected, given) => write!(f, "invalid number of fields during instanciation: expected {}, found {}", expected, given),
			Self::NoMatch => write!(f, "no pattern matches the given value"),
			Self::PatternMissmatch(pattern, value) => write!(f, "let-match failed: the value `{}` does not match the pattern `{}`", value, pattern),
			Self::BindingPatternUnion => write!(f, "attempted to bind with a pattern uniont"),
			Self::NotAnEnumVariant => write!(f, "assumed that the value is an enum variant where it is not"),
			Self::UnimplementedFunction => write!(f, "unimplemented function called"),
			Self::InvalidNumberOfArguments(expected, given) => write!(f, "wrong number of arguments: expected {}, found {}", expected, given),
			Self::UnreachableLabel => write!(f, "label is unreachable"),
			Self::RecursionArgsMissmatch => write!(f, "recursion arguments do not match the definition at label"),
			Self::ImmutableThis => write!(f, "`this` cannot be borrowed as mutable"),
			Self::UnreachableReached => write!(f, "`unreachable` expression reached"),
			Self::EmptyStack => write!(f, "attempted to pop an empty stack"),
			Self::UndefinedLexerMethod => write!(f, "lexer type has no lexer function"),
			Self::NoLexer => write!(f, "no lexer defined in the program"),
			Self::NotAStream => write!(f, "attempted to pull from a non stream instance")
		}
	}
}

/// Error value.
#[derive(Debug)]
pub enum Value<'v> {
	/// IO error.
	IO(std::io::Error),

	/// Unexpected char.
	UnexpectedChar(Option<char>),

	/// Unexpected token.
	UnexpectedToken(Box<super::Value<'v>>),

	/// Unexpected node.
	UnexpectedNode(Box<super::Value<'v>>)
}

impl<'v> Value<'v> {
	pub fn name(&self) -> &str {
		match self {
			Self::IO(_) => "io",
			Self::UnexpectedChar(Some(c)) => {
				eprintln!("char: {}", c);
				"unexpected-char"
			},
			Self::UnexpectedChar(None) => "unexpected-eos",
			Self::UnexpectedToken(_) => "unexpected-token",
			Self::UnexpectedNode(_) => "unexpected-node"
		}
	}
}