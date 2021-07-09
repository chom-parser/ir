use std::collections::HashMap;
use crate::{
	Ids,
	Context,
	Constant,
	ty,
	Expr,
	expr::{
		Var
	}
};

pub enum Error {
	/// No `this`.
	NoThis,

	/// Value has been moved and cannot be borrowed.
	ValueMoved,

	/// Value has already been moved.
	ValueAlreadyMoved,

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
	InvalidFieldCount(u32, u32)
}

#[derive(Clone)]
pub enum MaybeMovedValue {
	Here(Value),
	Moved
}

impl MaybeMovedValue {
	pub fn new(value: Value) -> Self {
		MaybeMovedValue::Here(value)
	}

	pub fn borrow(&self) -> Result<&Value, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::ValueMoved)
		}
	}

	pub fn borrow_mut(&mut self) -> Result<&mut Value, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::ValueMoved)
		}
	}

	pub fn copy_or_move(&mut self) -> Result<Value, Error> {
		match self {
			Self::Here(value) => {
				if value.is_copiable() {
					return Ok(value.clone())
				}
			},
			Self::Moved => return Err(Error::ValueAlreadyMoved)
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

pub struct Variable {
	mutable: bool,
	value: MaybeMovedValue
}

impl Variable {
	pub fn new(value: Value, mutable: bool) -> Self {
		Self {
			mutable,
			value: MaybeMovedValue::new(value)
		}
	}

	pub fn update(&mut self, new_value: Value) -> Result<(), Error> {
		if self.mutable {
			self.value = MaybeMovedValue::new(new_value);
			Ok(())
		} else {
			Err(Error::NotMutable)
		}
	}
}

pub struct Environement<'a, T: Ids> {
	context: &'a Context<T>,

	/// The current variables values.
	vars: HashMap<T::Var, Variable>,

	/// The current `Var::This`.
	this: Option<MaybeMovedValue>
}

impl<'a, T: Ids> Environement<'a, T> {
	pub fn eval(&mut self, e: &Expr<T>) -> Result<Value, Error> {
		match e {
			Expr::Literal(c) => Ok(Value::Constant(c.clone())),
			Expr::Get(v) => {
				match v {
					Var::This => Ok(self.this.as_mut().ok_or(Error::NoThis)?.copy_or_move()?),
					Var::Defined(x) => {
						match self.vars.get_mut(x) {
							Some(v) => Ok(v.value.copy_or_move()?),
							None => Err(Error::UnboundVariable)
						}
					}
				}
			},
			Expr::GetField(x, ty_ref, index) => {
				let index = *index;
				let value = self.vars.get_mut(x).ok_or(Error::UnboundVariable)?.value.borrow_mut()?;
				match value {
					Value::Instance(value_ty_ref, data) => {
						if value_ty_ref == ty_ref {
							match data {
								InstanceData::Struct(bindings) => {
									bindings.get_mut(index as usize).ok_or(Error::UndefinedField(index))?.copy_or_move()
								}
								_ => Err(Error::GetFieldFromNonStruct)
							}
						} else {
							Err(Error::IncompatibleType)
						}
					},
					_ => Err(Error::NotAnInstance)
				}
			},
			Expr::Let(x, mutable, e, next) => {
				let value = self.eval(e)?;
				self.vars.insert(*x, Variable::new(value, *mutable));
				self.eval(next)
			}
			Expr::Update(x, e, next) => {
				let value = self.eval(e)?;
				self.vars.get_mut(x).ok_or(Error::UnboundVariable)?.update(value)?;
				self.eval(next)
			},
			Expr::New(ty_ref, args) => {
				let ty = self.context.ty(*ty_ref).unwrap();
				let len = args.len() as u32;
				let expected_len = match ty.desc() {
					ty::Desc::Struct(strct) => strct.len(),
					ty::Desc::TupleStruct(args) => args.len() as u32,
					_ => return Err(Error::NotAStructType(*ty_ref))
				};

				if len == expected_len {
					let mut eargs = Vec::new();
					for a in args {
						eargs.push(MaybeMovedValue::new(self.eval(a)?));
						// TODO check types.
					}

					Ok(Value::Instance(*ty_ref, InstanceData::Struct(eargs)))
				} else {
					Err(Error::InvalidFieldCount(expected_len, len))
				}
			},
			Expr::Cons(ty_ref, index, args) => {
				let ty = self.context.ty(*ty_ref).unwrap();
				match ty.desc() {
					ty::Desc::Enum(enm) => {
						let variant = enm.variant(*index).expect("unknown variant");
						let len = args.len() as u32;
						let expected_len = variant.len();

						if len == expected_len {
							let mut eargs = Vec::new();
							for a in args {
								eargs.push(MaybeMovedValue::new(self.eval(a)?));
								// TODO check types.
							}
		
							Ok(Value::Instance(*ty_ref, InstanceData::EnumVariant(*index, eargs)))
						} else {
							Err(Error::InvalidFieldCount(expected_len, len))
						}
					},
					_ => Err(Error::NotAnEnumType(*ty_ref))
				}
			}
			Expr::Heap(e) => self.eval(e),
			Expr::Match { expr, cases } => {
				let value = self.eval(expr)?;

				// for case in cases {
				// 	// ...
				// }
				panic!("TODO")
			}
			_ => panic!("TODO")
		}
	}
}

/// Value.
#[derive(Clone)]
pub enum Value {
	Constant(Constant),
	Instance(ty::Ref, InstanceData)
}

impl Value {
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Constant(_) => true,
			Self::Instance(ty_ref, data) => {
				ty_ref.is_copiable() && data.is_copiable()
			}
		}
	}
}

#[derive(Clone)]
pub enum InstanceData {
	EnumVariant(u32, Vec<MaybeMovedValue>),
	Struct(Vec<MaybeMovedValue>)
}

impl InstanceData {
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::EnumVariant(_, args) => args.iter().all(|a| a.is_copiable()),
			Self::Struct(bindings) => bindings.iter().all(|b| b.is_copiable())
		}
	}
}