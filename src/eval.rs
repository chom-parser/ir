use source_span::Loc;
use crate::{
	Namespace,
	Expr,
	expr::Var,
	ty
};

pub mod fmt;
pub mod value;
pub mod environment;
pub mod error;
mod lexer;
pub mod stream;
mod stack;

pub use value::Value;
pub use environment::Environment;
pub use error::Error;
pub use lexer::Lexer;
pub use stack::Stack;

use error::Desc as E;

pub enum Action<'e, T: Namespace> {
	/// Evaluate the given expression.
	Eval(&'e Expr<T>),

	/// Pop a value from the stack and bind it
	/// to the given variable.
	/// The boolean specifies if the variable is mutable.
	Bind(T::Var, bool)
}

pub struct Evaluator<'a, 'v, 'e, T: Namespace> {
	/// Variable environment.
	env: Environment<'a, 'v, 'e, T>,

	/// Actions stack.
	/// 
	/// When the stack is empty,
	/// the `values` stack must contains a single value:
	/// the result.
	actions: Vec<Action<'e, T>>,
	
	/// Values stack.
	values: Vec<Value<'v>>
}

impl<'a, 'v, 'e, T: Namespace> Evaluator<'a, 'v, 'e, T> {
	pub fn eval(&mut self, e: &'e Expr<T>) -> Result<Value<'v>, Error> {
		self.actions.push(Action::Eval(e));

		while let Some(action) = self.actions.pop() {
			self.step(action)?;
		}

		Ok(self.values.pop().unwrap())
	}

	fn error(&self, e: E) -> Error {
		Error::new(e)
	}

	fn err<X>(&self, e: E) -> Result<X, Error> {
		Err(self.error(e))
	}

	fn step(&mut self, action: Action<'e, T>) -> Result<(), Error> {
		match action {
			Action::Eval(e) => {
				match e {
					Expr::Literal(c) => {
						self.values.push(Value::Constant(c.clone()))
					}
					Expr::Get(v) => {
						self.values.push(match v {
							Var::This => self.env.this().ok_or_else(|| self.error(E::NoThis))?.borrow().copy()?,
							Var::Defined(x) => self.env.get_mut(*x)?.value_mut().copy_or_move()?
						})
					},
					Expr::GetField(x, ty_ref, index) => {
						let index = *index;
						let value = self.env.get_mut(*x)?.value_mut().borrow_mut()?;
						match value {
							Value::Instance(value_ty_ref, data) => {
								if value_ty_ref == ty_ref {
									match data {
										value::InstanceData::Struct(bindings) => {
											self.values.push(
												bindings.get_mut(index as usize).ok_or_else(|| Error::new(E::UndefinedField(index)))?.copy_or_move()?
											)
										}
										_ => return self.err(E::GetFieldFromNonStruct)
									}
								} else {
									return self.err(E::IncompatibleType)
								}
							},
							_ => return self.err(E::NotAnInstance)
						}
					},
					Expr::Let(x, mutable, e, next) => {
						self.actions.push(Action::Eval(next));
						self.actions.push(Action::Bind(*x, *mutable));
						self.actions.push(Action::Eval(e))
					}
					_ => panic!("TODO")
				}
			}
			Action::Bind(x, mutable) => {
				panic!("TODO")
			}
		}

		Ok(())
	}
}

/// Create a new lexer from the given source.
pub fn lexer<'a, 'v, 'e, T: Namespace, I>(env: &Environment<'a, 'v, 'e, T>, input: &'v mut I) -> Result<Value<'v>, Error> where I: Iterator<Item=std::io::Result<char>> {
	match env.context().lexer_types().next() {
		Some(ty_index) => {
			env.instanciate(ty::Ref::Defined(ty_index), Some(Value::Input(input)))
		},
		None => Err(Error::new(error::Desc::NoLexer))
	}
}

pub fn parse<'a, 'v, 'e, T: Namespace, P: Iterator<Item=std::io::Result<char>>>(
	env: &mut Environment<'a, 'v, 'e, T>,
	phrase: &'v mut P,
	parser: u32
) -> Result<Result<Value<'v>, Loc<error::Value<'v>>>, Error> {
	// let lexer = lexer(env, phrase)?;
	// let result = env.call(parser, None, vec![lexer])?;
	// Ok(match result.into_result()? {
	// 	Ok(v) => Ok(v),
	// 	Err(v) => Err(v.into_loc_error()?)
	// })
	panic!("TODO")
}