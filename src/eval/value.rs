use source_span::{
	Position,
	Span,
	Loc
};
use crate::{
	Namespace,
	Constant,
	ty,
	Pattern
};
use super::{
	Lexer,
	Stack,
	error,
	error::Desc as E,
	Error
};

/// Value.
pub enum Value<'v> {
	Constant(Constant),
	Instance(ty::Ref, InstanceData<'v>),
	Position(Position),
	Span(Span),
	Loc(Loc<Box<Value<'v>>>),
	Lexer(Lexer<'v>),
	Chars(std::vec::IntoIter<char>),
	Stack(Stack<'v>),
	Error(error::Value<'v>),
	Input(&'v mut dyn Iterator<Item=Result<char, std::io::Error>>),
	Output(&'v mut dyn std::io::Write),
	Opaque(String)
}

impl<'v> Value<'v> {
	pub fn option(opt: Option<Self>) -> Self {
		match opt {
			None => Self::none(),
			Some(value) => Self::some(value)
		}
	}

	pub fn none() -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Option), InstanceData::EnumVariant(0, Vec::new()))
	}

	pub fn some(value: Self) -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Option), InstanceData::EnumVariant(1, vec![MaybeMoved::new(value)]))
	}

	pub fn ok(value: Self) -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(0, vec![MaybeMoved::new(value)]))
	}

	pub fn err(value: Self) -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(1, vec![MaybeMoved::new(value)]))
	}

	/// Checks if the value is an `Result::Err` instance.
	pub fn is_err(&self) -> bool {
		match self {
			Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(1, _)) => true,
			_ => false
		}
	}

	/// Unwrap a value inside a `Result::Ok`.
	pub fn expect_ok(self) -> Result<Self, Error> {
		match self {
			Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(0, args)) => Ok(args.into_iter().next().unwrap().unwrap()?),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Constant(_) => true,
			Self::Instance(ty_ref, data) => {
				ty_ref.is_copiable() && data.is_copiable()
			},
			Self::Position(_) => true,
			Self::Span(_) => true,
			Self::Loc(l) => l.as_ref().is_copiable(),
			_ => false
		}
	}

	pub fn copy(&self) -> Result<Self, Error> {
		match self {
			Self::Constant(c) => Ok(Self::Constant(c.clone())),
			Self::Instance(ty_ref, data) => Ok(Self::Instance(*ty_ref, data.copy()?)),
			Self::Position(p) => Ok(Self::Position(*p)),
			Self::Span(s) => Ok(Self::Span(*s)),
			Self::Loc(l) => Ok(Self::Loc(Loc::new(Box::new(l.as_ref().copy()?), l.span()))),
			_ => Err(Error::new(E::CannotMoveOut))
		}
	}

	pub fn as_lexer(&self) -> Result<&Lexer<'v>, Error> {
		match self {
			Self::Lexer(l) => Ok(l),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn as_lexer_mut(&mut self) -> Result<&mut Lexer<'v>, Error> {
		match self {
			Self::Lexer(l) => Ok(l),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn as_stack_mut(&mut self) -> Result<&mut Stack<'v>, Error> {
		match self {
			Self::Stack(s) => Ok(s),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn as_output_mut(&mut self) -> Result<&mut dyn std::io::Write, Error> {
		match self {
			Self::Output(o) => Ok(o),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_char(self) -> Result<char, Error> {
		match self {
			Self::Constant(Constant::Char(c)) => Ok(c),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_string(self) -> Result<String, Error> {
		match self {
			Self::Constant(Constant::String(s)) => Ok(s),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_input(self) -> Result<&'v mut dyn Iterator<Item=std::io::Result<char>>, Error> {
		match self {
			Self::Input(o) => Ok(o),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_option(self) -> Result<Option<Self>, Error> {
		match self {
			Self::Instance(ty::Ref::Native(ty::Native::Option), InstanceData::EnumVariant(v, args)) => {
				Ok(match v {
					0 => None,
					1 => Some(args.into_iter().next().unwrap().unwrap()?),
					_ => panic!("invalid option")
				})
			},
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_result(self) -> Result<Result<Self, Self>, Error> {
		match self {
			Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(v, args)) => {
				let value = args.into_iter().next().unwrap().unwrap()?;
				Ok(match v {
					0 => Ok(value),
					1 => Err(value),
					_ => panic!("invalid option")
				})
			},
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_error(self) -> Result<error::Value<'v>, Error> {
		match self {
			Self::Error(e) => Ok(e),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_loc_error(self) -> Result<Loc<error::Value<'v>>, Error> {
		let loc = self.into_loc()?;
		loc.try_map(|v| v.into_error())
	}

	pub fn into_position(self) -> Result<Position, Error> {
		match self {
			Self::Position(p) => Ok(p),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_span(self) -> Result<Span, Error> {
		match self {
			Self::Span(s) => Ok(s),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_loc(self) -> Result<Loc<Self>, Error> {
		match self {
			Self::Loc(v) => Ok(v.map(|v| *v)),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn into_lexer(self) -> Result<Lexer<'v>, Error> {
		match self {
			Self::Lexer(l) => Ok(l),
			_ => Err(Error::new(E::IncompatibleType))
		}
	}

	pub fn matches<T: Namespace>(&self, pattern: &Pattern<T>) -> Result<bool, Error> {
		match (pattern, self) {
			(Pattern::Any | Pattern::Bind(_), _) => Ok(true),
			(Pattern::Literal(a), Self::Constant(b)) => Ok(a == b),
			(Pattern::Cons(ty_a, va, patterns), Value::Instance(ty_b, data)) => {
				if ty_a == ty_b {
					match data {
						InstanceData::EnumVariant(vb, args) => {
							if va == vb {
								if patterns.len() == args.len() {
									for (i, p) in patterns.iter().enumerate() {
										if !args[i].borrow()?.matches(p)? {
											return Ok(false)
										}
									}

									Ok(true)
								} else {
									Err(Error::new(E::InvalidFieldCount(args.len() as u32, patterns.len() as u32)))
								}
							} else {
								Ok(false)
							}
						},
						_ => Err(Error::new(E::NotAnEnumVariant))
					}
				} else {
					Err(Error::new(E::IncompatibleType))
				}
			},
			(Pattern::Or(patterns), _) => {
				for p in patterns {
					if self.matches(p)? {
						return Ok(true)
					}
				}

				Ok(false)
			},
			(_, _) => Ok(false)
		}
	}
}

pub enum InstanceData<'v> {
	EnumVariant(u32, Vec<MaybeMoved<'v>>),
	Struct(Vec<MaybeMoved<'v>>)
}

impl<'v> InstanceData<'v> {
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::EnumVariant(_, args) => args.iter().all(|a| a.is_copiable()),
			Self::Struct(bindings) => bindings.iter().all(|b| b.is_copiable())
		}
	}

	pub fn copy(&self) -> Result<Self, Error> {
		match self {
			Self::EnumVariant(v, args) => {
				let mut cargs = Vec::with_capacity(args.len());
				for a in args {
					cargs.push(MaybeMoved::new(a.borrow()?.copy()?))
				}
				Ok(Self::EnumVariant(*v, cargs))
			},
			Self::Struct(bindings) => {
				let mut cbindings = Vec::with_capacity(bindings.len());
				for b in bindings {
					cbindings.push(MaybeMoved::new(b.borrow()?.copy()?))
				}
				Ok(Self::Struct(cbindings))
			}
		}
	}
}

pub enum MaybeMoved<'v> {
	Here(Value<'v>),
	Moved
}

impl<'v> MaybeMoved<'v> {
	pub fn new(value: Value<'v>) -> Self {
		MaybeMoved::Here(value)
	}

	pub fn unwrap(self) -> Result<Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::new(E::ValueMoved))
		}
	}

	pub fn borrow(&self) -> Result<&Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::new(E::ValueMoved))
		}
	}

	pub fn borrow_mut(&mut self) -> Result<&mut Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::new(E::ValueMoved))
		}
	}

	pub fn copy_or_move(&mut self) -> Result<Value<'v>, Error> {
		match self {
			Self::Here(value) => {
				if value.is_copiable() {
					return Ok(value.copy()?)
				}
			},
			Self::Moved => return Err(Error::new(E::ValueAlreadyMoved))
		}

		let mut this = Self::Moved;
		std::mem::swap(self, &mut this);

		match this {
			Self::Here(value) => Ok(value),
			Self::Moved => unreachable!()
		}
	}

	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Here(value) => value.is_copiable(),
			Self::Moved => false
		}
	}
}

pub enum Borrowed<'a, 'v> {
	Const(&'a Value<'v>),
	Mut(&'a mut Value<'v>)
}

impl<'a, 'v> Borrowed<'a, 'v> {
	pub fn borrow(&self) -> &Value<'v> {
		match self {
			Self::Const(v) => v,
			Self::Mut(v) => v
		}
	}

	pub fn borrow_mut(&mut self) -> Result<&mut Value<'v>, Error> {
		match self {
			Self::Mut(v) => Ok(v),
			Self::Const(_) => Err(Error::new(E::ImmutableThis))
		}
	}
}