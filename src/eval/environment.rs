use std::collections::HashMap;
use source_span::Loc;
use crate::{
	Ids,
	Context,
	ty,
	Expr,
	expr::{
		self,
		Var,
		LexerExpr,
		StreamExpr,
		StackExpr,
		SpanExpr
	},
	Pattern
};
use super::{
	Value,
	value,
	Error,
	error,
	Lexer,
	Stream
};

pub struct Variable<'v> {
	mutable: bool,
	value: value::MaybeMoved<'v>
}

impl<'v> Variable<'v> {
	pub fn new(value: Value<'v>, mutable: bool) -> Self {
		Self {
			mutable,
			value: value::MaybeMoved::new(value)
		}
	}

	pub fn update(&mut self, new_value: Value<'v>) -> Result<(), Error> {
		if self.mutable {
			self.value = value::MaybeMoved::new(new_value);
			Ok(())
		} else {
			Err(Error::NotMutable)
		}
	}
}

pub struct Frame<'v, 'e, T: Ids> {
	label: T::Label,

	args: Vec<Var<T>>,

	expr: &'e Expr<T>,

	/// Variables local to the frame.
	vars: HashMap<T::Var, Variable<'v>>,
}

impl<'v, 'e, T: Ids> Frame<'v, 'e, T> {
	pub fn new(label: T::Label, args: Vec<Var<T>>, expr: &'e Expr<T>) -> Self {
		Self {
			label,
			args,
			expr,
			vars: HashMap::new()
		}
	}

	pub fn clear(&mut self) {
		self.vars.clear()
	}
}



pub struct Environement<'a, 'v, 'e, T: Ids> {
	/// Context.
	context: &'a Context<T>,

	/// The current variables values.
	vars: HashMap<T::Var, Variable<'v>>,

	/// The current `Var::This`.
	this: Option<value::Borrowed<'a, 'v>>,

	/// Stack.
	stack: Vec<Frame<'v, 'e, T>>
}

impl<'a, 'v, 'e, T: Ids> Environement<'a, 'v, 'e, T> {
	pub fn new(context: &'a Context<T>) -> Self {
		Self {
			context,
			vars: HashMap::new(),
			this: None,
			stack: Vec::new()
		}
	}

	pub fn set_this(&mut self, this: value::Borrowed<'a, 'v>) {
		self.this = Some(this)
	}

	pub fn get(&self, x: T::Var) -> Result<&Variable<'v>, Error> {
		for frame in self.stack.iter().rev() {
			if let Some(v) = frame.vars.get(&x) {
				return Ok(v)
			}
		}

		match self.vars.get(&x) {
			Some(v) => Ok(v),
			None => Err(Error::UnboundVariable)
		}
	}

	pub fn borrow(&self, x: Var<T>) -> Result<&Value<'v>, Error> {
		match x {
			Var::This => Ok(self.this.as_ref().ok_or(Error::NoThis)?.borrow()),
			Var::Defined(x) => self.get(x)?.value.borrow()
		}
	}

	pub fn get_mut(&mut self, x: T::Var) -> Result<&mut Variable<'v>, Error> {
		for frame in self.stack.iter_mut().rev() {
			if let Some(v) = frame.vars.get_mut(&x) {
				return Ok(v)
			}
		}

		match self.vars.get_mut(&x) {
			Some(v) => Ok(v),
			None => Err(Error::UnboundVariable)
		}
	}

	pub fn borrow_mut(&mut self, x: Var<T>) -> Result<&mut Value<'v>, Error> {
		match x {
			Var::This => self.this.as_mut().ok_or(Error::NoThis)?.borrow_mut(),
			Var::Defined(x) => self.get_mut(x)?.value.borrow_mut()
		}
	}

	pub fn bind(&mut self, x: T::Var, v: Variable<'v>) {
		if let Some(frame) = self.stack.last_mut() {
			frame.vars.insert(x, v);
		} else {
			self.vars.insert(x, v);
		}
	}

	pub fn eval(&mut self, e: &'e Expr<T>) -> Result<Value<'v>, Error> {
		match e {
			Expr::Literal(c) => Ok(Value::Constant(c.clone())),
			Expr::Get(v) => {
				match v {
					Var::This => self.this.as_ref().ok_or(Error::NoThis)?.borrow().copy(),
					Var::Defined(x) => self.get_mut(*x)?.value.copy_or_move()
				}
			},
			Expr::GetField(x, ty_ref, index) => {
				let index = *index;
				let value = self.get_mut(*x)?.value.borrow_mut()?;
				match value {
					Value::Instance(value_ty_ref, data) => {
						if value_ty_ref == ty_ref {
							match data {
								value::InstanceData::Struct(bindings) => {
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
				self.bind(*x, Variable::new(value, *mutable));
				self.eval(next)
			}
			Expr::Update(x, e, next) => {
				let value = self.eval(e)?;
				self.get_mut(*x)?.update(value)?;
				self.eval(next)
			},
			Expr::New(ty_ref, args) => {
				let ty = self.context.ty(*ty_ref).unwrap();
				let len = args.len() as u32;
				let expected_len = match ty.desc() {
					ty::Desc::Struct(strct) => strct.len(),
					ty::Desc::TupleStruct(args) => args.len() as u32,
					ty::Desc::Lexer => return {
						// A lexer is a bit special.
						if args.len() == 1 {
							let source = self.eval(&args[0])?.into_input()?;

							let lexer_method = ty.methods().iter().find(|f_index| {
								self.context.function(**f_index).unwrap().signature().is_lexer()
							});

							match lexer_method {
								Some(lexer_method) => Ok(Value::Lexer(Lexer::new(*lexer_method, source))),
								None => Err(Error::UndefinedLexerMethod)
							}
						} else {
							Err(Error::InvalidNumberOfArguments(1, args.len() as u32))
						}
					},
					_ => return Err(Error::NotAStructType(*ty_ref))
				};

				if len == expected_len {
					let mut eargs = Vec::new();
					for a in args {
						eargs.push(value::MaybeMoved::new(self.eval(a)?));
						// TODO check types.
					}

					Ok(Value::Instance(*ty_ref, value::InstanceData::Struct(eargs)))
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
								eargs.push(value::MaybeMoved::new(self.eval(a)?));
								// TODO check types.
							}
		
							Ok(Value::Instance(*ty_ref, value::InstanceData::EnumVariant(*index, eargs)))
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

				for case in cases {
					if value.matches(&case.pattern)? {
						self.let_match(value, &case.pattern)?;
						return self.eval(&case.expr)
					}
				}
				
				Err(Error::NoMatch)
			}
			Expr::LetMatch(pattern, expr, next) => {
				let value = self.eval(expr)?;
				self.let_match(value, pattern)?;
				self.eval(next)
			}
			Expr::Call(f_index, this, args) => {
				let f = self.context.function(*f_index).expect("unknown function");
				let body = f.body().ok_or(Error::UnimplementedFunction)?;

				let expected_len = f.signature().arity();
				let len = args.len() as u32;
				if expected_len == len {
					let mut env = Self::new(self.context);
					for (i, x) in f.signature().arguments().iter().enumerate() {
						env.vars.insert(*x, Variable::new(self.eval(&args[i])?, false));
					}

					env.this = match this {
						Some((x, false)) => {
							Some(value::Borrowed::Const(self.vars.get(&x).ok_or(Error::UnboundVariable)?.value.borrow()?))
						},
						Some((x, true)) => {
							Some(value::Borrowed::Mut(self.vars.get_mut(&x).ok_or(Error::UnboundVariable)?.value.borrow_mut()?))
						},
						None => None
					};

					env.eval(body)
				} else {
					Err(Error::InvalidNumberOfArguments(len, expected_len))
				}
			}
			Expr::TailRecursion { label, args, body } => {
				self.stack.push(Frame::new(*label, args.clone(), body));
				self.eval(body)
			}
			Expr::Recurse(label, args) => {
				loop {
					match self.stack.last() {
						Some(frame) => {
							if frame.label == *label {
								break
							}
						},
						None => return Err(Error::UnreachableLabel)
					}
				}

				let frame = self.stack.last_mut().unwrap();
				if frame.args == *args {
					frame.clear();
					let expr = frame.expr;
					self.eval(expr)
				} else {
					Err(Error::RecursionArgsMissmatch)
				}
			}
			Expr::Lexer(lexer, e) => {
				match e {
					LexerExpr::Peek => {
						let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
						Ok(lexer.peek())
					}
					LexerExpr::Span => {
						let lexer = self.borrow(*lexer)?.as_lexer()?;
						Ok(lexer.span())
					}
					LexerExpr::Chars => {
						let lexer = self.borrow(*lexer)?.as_lexer()?;
						Ok(lexer.chars())
					}
					LexerExpr::Buffer => {
						let lexer = self.borrow(*lexer)?.as_lexer()?;
						Ok(lexer.buffer())
					}
					LexerExpr::Clear(next) => {
						let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
						lexer.clear();
						self.eval(next)
					}
					LexerExpr::Consume(next) => {
						let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
						match lexer.consume() {
							Ok(()) => self.eval(next),
							Err(e) => Ok(Value::err(e))
						}
					}
				}
			}
			Expr::Stream(stream, e) => {
				match e {
					StreamExpr::Pull(x, next) => {
						let context = self.context;
						let value = Stream::pull(self.borrow_mut(*stream)?, context)?;
						self.bind(*x, Variable::new(value, false));
						self.eval(next)
					}
				}
			}
			Expr::Stack(stack, e) => {
				match e {
					StackExpr::Push(value, state, next) => {
						let value = self.eval(value)?;
						let state = self.eval(state)?;
						let stack = self.borrow_mut(*stack)?.as_stack_mut()?;
						stack.push(value, state);
						self.eval(next)
					}
					StackExpr::Pop(x, y, next) => {
						let stack = self.borrow_mut(*stack)?.as_stack_mut()?;
						let (vx, vy) = stack.pop()?;

						if let Some(x) = x {
							self.bind(*x, Variable::new(vx, false))
						}

						if let Some(y) = y {
							self.bind(*y, Variable::new(vy, false))
						}

						self.eval(next)
					}
				}
			}
			Expr::Span(e) => {
				match e {
					SpanExpr::Locate(expr, span) => {
						let value = self.eval(expr)?;
						let span = self.eval(span)?.into_span()?;
						Ok(Value::Loc(Loc::new(Box::new(value), span)))
					}
					SpanExpr::FromPosition(pos) => {
						let pos = self.eval(pos)?.into_position()?;
						Ok(Value::Span(pos.into()))
					}
					SpanExpr::After(span) => {
						let span = self.eval(span)?.into_span()?;
						Ok(Value::Position(span.end()))
					}
					SpanExpr::Transpose(loc_opt, default_span) => {
						let loc_opt = self.eval(loc_opt)?.into_option()?.map(Value::into_loc).transpose()?;
						let default_span = self.eval(default_span)?.into_span()?;
						Ok(Value::Loc(Loc::transposed(loc_opt, default_span).map(|v| Box::new(Value::option(v)))))
					}
					SpanExpr::Unwrap(x, y, loc, next) => {
						let (value, span) = self.eval(loc)?.into_loc()?.into_raw_parts();
						
						if let Some(x) = x {
							self.bind(*x, Variable::new(value, false))
						}

						if let Some(y) = y {
							self.bind(*y, Variable::new(Value::Span(span), false))
						}

						self.eval(next)
					}
					SpanExpr::Merge(a, b) => {
						let a = self.eval(a)?.into_span()?;
						let b = self.eval(b)?.into_span()?;
						Ok(Value::Span(a.union(b)))
					}
				}
			}
			Expr::Write(output, string, next) => {
				let output = self.borrow_mut(*output)?.as_output_mut()?;
				output.write_all(string.as_bytes()).map_err(Error::IO)?;
				self.eval(next)
			}
			Expr::Error(e) => {
				Ok(Value::Error(match e {
					expr::Error::UnexpectedToken(e) => {
						error::Value::UnexpectedToken(Box::new(self.eval(&e)?))
					}
					expr::Error::UnexpectedNode(e) => {
						error::Value::UnexpectedNode(Box::new(self.eval(&e)?))
					}
				}))
			}
			Expr::Unreachable => Err(Error::UnreachableReached)
		}
	}

	pub fn let_match(&mut self, value: Value<'v>, pattern: &Pattern<T>) -> Result<(), Error> {
		match (pattern, value) {
			(Pattern::Any, _) => (),
			(Pattern::Bind(x), value) => { self.bind(*x, Variable::new(value, false)); },
			(Pattern::Literal(a), Value::Constant(b)) => {
				if *a != b {
					return Err(Error::PatternMissmatch)
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
									return Err(Error::InvalidFieldCount(len, expected_len))
								}
							} else {
								return Err(Error::PatternMissmatch)
							}
						},
						_ => return Err(Error::NotAnEnumVariant)
					}
				} else {
					return Err(Error::IncompatibleType)
				}
			},
			(Pattern::Or(patterns), _) => {
				if patterns.iter().any(Pattern::is_bound) {
					return Err(Error::BindingPatternUnion)
				}
			},
			(_, _) => return Err(Error::PatternMissmatch)
		}

		Ok(())
	}
}