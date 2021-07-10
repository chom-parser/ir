use crate::ty;

pub enum Error {
	IO(std::io::Error),

	/// No `this`.
	NoThis,

	/// Value has been moved and cannot be borrowed.
	ValueMoved,

	/// Value has already been moved.
	ValueAlreadyMoved,

	CannotMoveOut,

	/// Undefined variable.
	UnboundVariable,

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

	NoMatch,

	PatternMissmatch,

	BindingPatternUnion,

	NotAnEnumVariant,

	UnimplementedFunction,

	InvalidNumberOfArguments(u32, u32),

	UnreachableLabel,

	RecursionArgsMissmatch,

	ImmutableThis,

	UnreachableReached,

	EmptyStack,

	UndefinedLexerMethod
}

pub enum Value<'v> {
	IO(std::io::Error),
	UnexpectedToken(Box<super::Value<'v>>),
	UnexpectedNode(Box<super::Value<'v>>)
}