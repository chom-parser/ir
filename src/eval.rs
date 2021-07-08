use std::collections::HashMap;
use crate::{
	Ids,
	Constant,
	ty,
	Expr,
	expr::Var
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
	NotMutable
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

pub struct Environement<T: Ids> {
	/// The current variables values.
	vars: HashMap<T::Var, Variable>,

	/// The current `Var::This`.
	this: Option<MaybeMovedValue>
}

impl<T: Ids> Environement<T> {
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
								InstanceData::TupleStruct(args) => {
									args.get_mut(index as usize).ok_or(Error::UndefinedField(index))?.copy_or_move()
								}
								InstanceData::Struct(bindings) => {
									bindings.get_mut(index as usize).ok_or(Error::UndefinedField(index))?.value.copy_or_move()
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
	EnumVariant(u32, VariantData),
	Struct(Vec<Binding>),
	TupleStruct(Vec<MaybeMovedValue>)
}

impl InstanceData {
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::EnumVariant(_, data) => data.is_copiable(),
			Self::Struct(bindings) => bindings.iter().all(|b| b.is_copiable()),
			Self::TupleStruct(args) => args.iter().all(|a| a.is_copiable())
		}
	}
}

#[derive(Clone)]
pub enum VariantData {
	Tuple(Vec<MaybeMovedValue>),
	Struct(Vec<Binding>)
}

impl VariantData {
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Tuple(args) => args.iter().all(|a| a.is_copiable()),
			Self::Struct(bindings) => bindings.iter().all(|b| b.is_copiable())
		}
	}
}

#[derive(Clone)]
pub struct Binding {
	id: u32,
	value: MaybeMovedValue
}

impl Binding {
	pub fn is_copiable(&self) -> bool {
		self.value.is_copiable()
	}
}