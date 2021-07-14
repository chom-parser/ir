use slab::Slab;
use source_span::{
	Position,
	Span,
	Loc
};
use crate::{
	Context,
	Namespace,
	Expr,
	expr::{
		self,
		Var
	},
	Pattern,
	ty,
	function
};

pub mod fmt;
pub mod value;
pub mod frame;
pub mod error;
mod lexer;
pub mod stream;
mod stack;

pub use value::Value;
pub use frame::Frame;
pub use error::Error;
pub use lexer::Lexer;
pub use stack::Stack;

use error::Desc as E;
use frame::Reference;

pub enum Action<'e, T: Namespace> {
	/// Evaluate the given expression.
	Eval(&'e Expr<T>),

	/// Pop a value from the stack and bind it
	/// to the given variable.
	/// The boolean specifies if the variable is mutable.
	Bind(T::Var, bool),

	/// Pop a value from the stack and update
	/// the given variable with it.
	Update(T::Var),

	/// Pop the given amount of values from the stack
	/// and use them to instanciate the given type.
	Instanciate(ty::Ref, usize),

	/// Pop the given amount of values from the stack
	/// and use them to instanciate the given type enum
	/// variant.
	Cons(ty::Ref, u32, usize),

	/// Pop a value from the stack and
	/// perform a pattern matching on it using the
	/// given cases.
	Match(&'e [expr::MatchCase<T>]),

	/// Pop a value from the stack, unwrap it
	/// according to the given pattern then
	/// evaluate the given expression.
	LetMatch(&'e Pattern<T>, &'e Expr<T>),

	/// Pop the given amount of values (last parameter) from
	/// the stack and use them to call the given function
	/// identified by its index (first parameter).
	/// The second parameter gives an optional `this` to the call
	/// if the function is a method.
	Call(u32, Option<Reference>, usize)
}

pub struct Evaluator<'a, 'v, 'e, T: Namespace> where 'a: 'e {
	/// Context.
	context: &'a Context<T>,

	/// Memory.
	memory: Slab<Value<'v>>,

	/// Call stack.
	stack: Vec<Frame<'e, T>>,

	/// Actions stack.
	/// 
	/// When the stack is empty,
	/// the `values` stack must contains a single value:
	/// the result.
	actions: Vec<Action<'e, T>>,
	
	/// Values stack.
	values: Vec<Value<'v>>
}

impl<'a, 'v, 'e, T: Namespace> Evaluator<'a, 'v, 'e, T> where 'a: 'e {
	pub fn new(context: &'a Context<T>) -> Self {
		Self {
			context,
			memory: Slab::new(),
			stack: vec![Frame::new(None)],
			actions: Vec::new(),
			values: Vec::new()
		}
	}

	pub fn frame(&self) -> &Frame<'e, T> {
		self.stack.last().unwrap()
	}

	pub fn frame_mut(&mut self) -> &mut Frame<'e, T> {
		self.stack.last_mut().unwrap()
	}

	pub fn context(&self) -> &'a Context<T> {
		self.context
	}

	fn exec(&mut self, action: Action<'e, T>) -> Result<Value<'v>, Error> {
		self.actions.push(action);

		while let Some(action) = self.actions.pop() {
			self.step(action)?;
		}

		Ok(self.values.pop().unwrap())
	}

	pub fn eval(&mut self, e: &'e Expr<T>) -> Result<Value<'v>, Error> {
		self.exec(Action::Eval(e))
	}

	fn call(&mut self, f_index: u32, this: Option<Reference>, args: Vec<Value<'v>>) -> Result<Value<'v>, Error> {
		let len = args.len();
		self.values.extend(args);
		self.exec(Action::Call(f_index, this, len))
	}

	pub fn instanciate<V>(&self, ty_ref: ty::Ref, args: V) -> Result<Value<'v>, Error> where V: IntoIterator<Item=Value<'v>> {
		match ty_ref {
			ty::Ref::Native(n) => {
				let args: Vec<_> = args.into_iter().collect();
				match n {
					ty::Native::Stack => {
						if args.is_empty() {
							Ok(Value::Stack(Stack::new()))
						} else {
							self.err(E::InvalidNumberOfArguments(0, args.len() as u32))
						}
					},
					ty::Native::Position => {
						if args.is_empty() {
							Ok(Value::Position(Position::default()))
						} else {
							self.err(E::InvalidNumberOfArguments(0, args.len() as u32))
						}
					}
					ty::Native::Span => {
						if args.is_empty() {
							Ok(Value::Span(Span::default()))
						} else {
							self.err(E::InvalidNumberOfArguments(0, args.len() as u32))
						}
					}
					_ => self.err(E::NotAStructType(ty_ref))
				}
			},
			ty::Ref::Defined(_) => {
				let ty = self.context.ty(ty_ref).unwrap();
				let expected_len = match ty.desc() {
					ty::Desc::Struct(strct) => strct.len(),
					ty::Desc::TupleStruct(args) => args.len() as u32,
					ty::Desc::Lexer => return {
						// A lexer is a bit special.
						let args: Vec<_> = args.into_iter().collect();
						if args.len() == 1 {
							let source = args.into_iter().next().unwrap().into_input()?;
							let lexer_method = ty.methods().iter().find(|f_index| {
								self.context.function(**f_index).unwrap().signature().is_lexer()
							});

							match lexer_method {
								Some(lexer_method) => Ok(Value::Lexer(Lexer::new(*lexer_method, source))),
								None => self.err(E::UndefinedLexerMethod)
							}
						} else {
							self.err(E::InvalidNumberOfArguments(1, args.len() as u32))
						}
					},
					_ => return self.err(E::NotAStructType(ty_ref))
				};

				let args: Vec<_> = args.into_iter().map(|v| value::MaybeMoved::new(v)).collect();
				let len = args.len() as u32;
				if len == expected_len {
					// TODO check types.
					// let mut eargs = Vec::new();
					// for a in args {
					// 	eargs.push(value::MaybeMoved::new(self.eval(a)?));
					// 	// TODO check types.
					// }

					Ok(Value::Instance(ty_ref, value::InstanceData::Struct(args)))
				} else {
					self.err(E::InvalidFieldCount(expected_len, len))
				}
			}
		}
	}

	fn bind(&mut self, x: T::Var, mutable: bool, value: Value<'v>) {
		let addr = self.memory.insert(value);
		self.frame_mut().bind(x, mutable, addr)
	}

	fn borrow(&self, x: Var<T>) -> Result<&Value<'v>, Error> {
		Ok(&self.memory[self.frame().borrow(x)?])
	}

	fn borrow_mut(&mut self, x: Var<T>) -> Result<&mut Value<'v>, Error> {
		let addr = self.frame().borrow_mut(x)?;
		Ok(&mut self.memory[addr])
	}

	fn reference_to(&self, x: Var<T>, mutable: bool) -> Result<Reference, Error> {
		let addr = if mutable {
			self.frame().borrow_mut(x)?
		} else {
			self.frame().borrow(x)?
		};

		Ok(Reference {
			addr,
			mutable
		})
	}

	fn take(&mut self, x: Var<T>) -> Result<Value<'v>, Error> {
		let addr = self.frame().borrow(x)?;
		if self.memory[addr].is_copiable() {
			Ok(self.memory[addr].copy()?)
		} else {
			match x {
				Var::Defined(x) => {
					self.frame_mut().take(x);
					Ok(self.memory.remove(addr))
				},
				Var::This => {
					Err(Error::new(E::CannotMoveOut))
				}
			}
		}
	}

	fn let_match(&mut self, value: Value<'v>, pattern: &Pattern<T>) -> Result<(), Error> {
		use fmt::ContextDisplay;
		match (pattern, value) {
			(Pattern::Any, _) => (),
			(Pattern::Bind(x), value) => { self.bind(*x, false, value); },
			(Pattern::Literal(a), Value::Constant(b)) => {
				if !a.matches(&b) {
					return self.err(E::PatternMissmatch(
						format!("{}", pattern.display_in(self.context())), 
						format!("{}", Value::Constant(b).display_in(self.context())), 
					))
				}
			},
			(Pattern::Cons(ty_a, va, patterns), Value::Instance(ty_b, data)) => {
				if *ty_a == ty_b {
					match data {
						value::InstanceData::EnumVariant(vb, args) => {
							let len = args.len() as u32;
							let expected_len = patterns.len() as u32;
							if *va == vb {
								if expected_len == len {
									for (i, a) in args.into_iter().enumerate() {
										self.let_match(a.unwrap()?, &patterns[i])?
									}
								} else {
									return self.err(E::InvalidFieldCount(len, expected_len))
								}
							} else {
								return self.err(E::PatternMissmatch(
									format!("{}", pattern.display_in(self.context())), 
									format!("{}", Value::Instance(ty_b, value::InstanceData::EnumVariant(vb, args)).display_in(self.context())), 
								))
							}
						},
						_ => return self.err(E::NotAnEnumVariant)
					}
				} else {
					return self.err(E::IncompatibleType)
				}
			},
			(Pattern::Or(patterns), _) => {
				if patterns.iter().any(Pattern::is_bound) {
					return self.err(E::BindingPatternUnion)
				}
			},
			(pattern, value) => return self.err(E::PatternMissmatch(
				format!("{}", pattern.display_in(self.context())), 
				format!("{}", value.display_in(self.context())), 
			))
		}

		Ok(())
	}

	fn error(&self, e: E) -> Error {
		Error::new(e)
	}

	fn err<X>(&self, e: E) -> Result<X, Error> {
		Err(self.error(e))
	}

	fn pop_value(&mut self) -> Value<'v> {
		self.values.pop().unwrap()
	}

	fn pop_values(&mut self, n: usize) -> Vec<Value<'v>> {
		let mut values = Vec::with_capacity(n);
		for _ in 0..n {
			values.push(self.values.pop().unwrap())
		}
		values.reverse();
		values
	}

	fn step(&mut self, action: Action<'e, T>) -> Result<(), Error> {
		match action {
			Action::Eval(e) => {
				match e {
					Expr::Literal(c) => {
						self.values.push(Value::Constant(c.clone()))
					}
					Expr::Get(x) => {
						let value = self.take(*x)?;
						self.values.push(value)
					},
					Expr::GetField(x, ty_ref, index) => {
						let index = *index;
						let value = self.borrow_mut(Var::Defined(*x))?;
						match value {
							Value::Instance(value_ty_ref, data) => {
								if value_ty_ref == ty_ref {
									match data {
										value::InstanceData::Struct(bindings) => {
											let value = bindings.get_mut(index as usize).ok_or_else(|| Error::new(E::UndefinedField(index)))?.copy_or_move()?;
											self.values.push(value)
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
					Expr::Update(x, e, next) => {
						self.actions.push(Action::Eval(next));
						self.actions.push(Action::Update(*x));
						self.actions.push(Action::Eval(e))
					}
					Expr::New(ty_ref, args) => {
						self.actions.push(Action::Instanciate(*ty_ref, args.len()));
						self.actions.extend(args.iter().map(Action::Eval));
					}
					Expr::Cons(ty_ref, index, args) => {
						self.actions.push(Action::Cons(*ty_ref, *index, args.len()));
						self.actions.extend(args.iter().map(Action::Eval));
					}
					Expr::Heap(e) => {
						self.actions.push(Action::Eval(e))
					}
					Expr::Match { expr, cases } => {
						self.actions.push(Action::Match(cases));
						self.actions.push(Action::Eval(expr))
					}
					Expr::LetMatch(pattern, expr, next) => {
						self.actions.push(Action::LetMatch(pattern, next));
						self.actions.push(Action::Eval(expr))
					}
					Expr::Call(f_index, this, args) => {
						let this = this.map(|(x, mutable)| self.reference_to(Var::Defined(x), mutable)).transpose()?;
						self.actions.push(Action::Call(*f_index, this, args.len()));
						self.actions.extend(args.iter().map(Action::Eval));
					}
					Expr::TailRecursion { label, args, body } => {
						self.frame_mut().begin_loop(*label, args.clone(), body);
						self.actions.push(Action::Eval(body))
					}
					Expr::Recurse(label, args) => {
						let body = self.frame_mut().continue_loop(*label, args)?;
						self.actions.push(Action::Eval(body))
					}
					_ => panic!("TODO")
				}
			}
			Action::Bind(x, mutable) => {
				let value = self.pop_value();
				self.bind(x, mutable, value)
			}
			Action::Update(x) => {
				let value = self.pop_value();
				*self.borrow_mut(Var::Defined(x))? = value
			}
			Action::Instanciate(ty_ref, n) => {
				let args = self.pop_values(n);
				let value = self.instanciate(ty_ref, args)?;
				self.values.push(value)
			}
			Action::Cons(ty_ref, index, n) => {
				let args = self.pop_values(n);
				let ty = self.context().ty(ty_ref).unwrap();
				match ty.desc() {
					ty::Desc::Enum(enm) => {
						let variant = enm.variant(index).expect("unknown variant");
						let len = args.len() as u32;
						let expected_len = variant.len();
						if len == expected_len {
							self.values.push(Value::Instance(
								ty_ref,
								value::InstanceData::EnumVariant(index, args.into_iter().map(value::MaybeMoved::new).collect())
							))
						} else {
							return self.err(E::InvalidFieldCount(expected_len, len))
						}
					}
					_ => return self.err(E::NotAnEnumType(ty_ref))
				}
			}
			Action::Match(cases) => {
				let value = self.pop_value();

				for case in cases {
					if value.matches(&case.pattern)? {
						self.let_match(value, &case.pattern)?;
						self.actions.push(Action::Eval(&case.expr));
						return Ok(())
					}
				}

				return self.err(E::NoMatch)
			}
			Action::LetMatch(pattern, next) => {
				let value = self.pop_value();
				self.let_match(value, pattern)?;
				self.actions.push(Action::Eval(next))
			}
			Action::Call(f_index, this, n) => {
				let args = self.pop_values(n);
				let f: &'e function::Function<T> = self.context.function(f_index).expect("unknown function");
				let len = args.len() as u32;

				match f.body() {
					Some(body) => {
						let expected_len = f.signature().arity();
						
						if expected_len == len {
							self.stack.push(Frame::new(this));

							for (i, a) in args.into_iter().enumerate() {
								let x = f.signature().arguments()[i];
								self.bind(x, false, a)
							}

							self.actions.push(Action::Eval(body))
						} else {
							return self.err(E::InvalidNumberOfArguments(expected_len, len))
						}
					},
					None => {
						match f.signature() {
							function::Signature::UndefinedChar(_, _) => {
								if len == 1 {
									let arg = args.into_iter().next().unwrap().into_option()?;
									let c = match arg {
										Some(v) => Some(v.into_char()?),
										None => None
									};
									self.values.push(Value::Error(error::Value::UnexpectedChar(c)));
								} else {
									return self.err(E::InvalidNumberOfArguments(1, len))
								}
							}
							function::Signature::ExternParser(_, _) => {
								if len == 1 {
									let arg = args.into_iter().next().unwrap().into_string()?;
									self.values.push(Value::Opaque(arg));
								} else {
									return self.err(E::InvalidNumberOfArguments(1, len))
								}
							},
							_ => return self.err(E::UnimplementedFunction)
						}
					}
				}
			}
		}

		Ok(())
	}
}

// /// Create a new lexer from the given source.
// pub fn lexer<'a, 'v, 'e, T: Namespace, I>(env: &Frame<'a, 'v, 'e, T>, input: &'v mut I) -> Result<Value<'v>, Error> where I: Iterator<Item=std::io::Result<char>> {
// 	match env.context().lexer_types().next() {
// 		Some(ty_index) => {
// 			env.instanciate(ty::Ref::Defined(ty_index), Some(Value::Input(input)))
// 		},
// 		None => Err(Error::new(error::Desc::NoLexer))
// 	}
// }

// pub fn parse<'a, 'v, 'e, T: Namespace, P: Iterator<Item=std::io::Result<char>>>(
// 	env: &mut Frame<'a, 'v, 'e, T>,
// 	phrase: &'v mut P,
// 	parser: u32
// ) -> Result<Result<Value<'v>, Loc<error::Value<'v>>>, Error> {
// 	// let lexer = lexer(env, phrase)?;
// 	// let result = env.call(parser, None, vec![lexer])?;
// 	// Ok(match result.into_result()? {
// 	// 	Ok(v) => Ok(v),
// 	// 	Err(v) => Err(v.into_loc_error()?)
// 	// })
// 	panic!("TODO")
// }