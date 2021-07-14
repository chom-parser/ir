use std::collections::HashMap;
use source_span::{
	Position,
	Span,
	Loc
};
use crate::{
	Namespace,
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
	Pattern,
	function
};
use super::{
	Value,
	value,
	Error,
	error,
	error::Desc as E,
	Lexer,
	Stack,
	stream
};

pub struct Environment<'a, 'v, 'e, T: Namespace> {
	/// Context.
	context: &'a Context<T>,

	/// The current variables values.
	vars: HashMap<T::Var, Variable<'v>>,

	/// The current `Var::This`.
	this: Option<value::Borrowed<'a, 'v>>,

	/// Stack.
	stack: Vec<Frame<'v, 'e, T>>
}

macro_rules! func_call {
	($self:ident, $f_index:ident, $this:ident, $args:ident) => {
		{
			let f = $self.context.function($f_index).expect("unknown function");
			let len = $args.len() as u32;

			match f.body() {
				Some(body) => {
					let expected_len = f.signature().arity();
					
					if expected_len == len {
						let mut env = Self::new($self.context);
						for (i, a) in $args.into_iter().enumerate() {
							let x = f.signature().arguments()[i];
							env.vars.insert(x, Variable::new(a, false));
						}

						env.this = $this;
						env.eval(body)
					} else {
						$self.err(E::InvalidNumberOfArguments(expected_len, len))
					}
				},
				None => {
					match f.signature() {
						function::Signature::UndefinedChar(_, _) => {
							if len == 1 {
								let arg = $args.into_iter().next().unwrap().into_option()?;
								let c = match arg {
									Some(v) => Some(v.into_char()?),
									None => None
								};
								Ok(Value::Error(error::Value::UnexpectedChar(c)))
							} else {
								$self.err(E::InvalidNumberOfArguments(1, len))
							}
						}
						function::Signature::ExternParser(_, _) => {
							if len == 1 {
								let arg = $args.into_iter().next().unwrap().into_string()?;
								Ok(Value::Opaque(arg))
							} else {
								$self.err(E::InvalidNumberOfArguments(1, len))
							}
						},
						_ => $self.err(E::UnimplementedFunction)
					}
				}
			}
		}
	};
}

impl<'a, 'v, 'e, T: Namespace> Environment<'a, 'v, 'e, T> {
	pub fn new(context: &'a Context<T>) -> Self {
		Self {
			context,
			vars: HashMap::new(),
			this: None,
			stack: Vec::new()
		}
	}

	fn error(&self, e: E) -> Error {
		Error::new(e)
	}

	fn err<X>(&self, e: E) -> Result<X, Error> {
		Err(self.error(e))
	}

	pub fn context(&self) -> &'a Context<T> {
		self.context
	}

	pub fn this(&self) -> Option<&value::Borrowed<'a, 'v>> {
		self.this.as_ref()
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
			None => self.err(E::UnboundVariable)
		}
	}

	pub fn borrow(&self, x: Var<T>) -> Result<&Value<'v>, Error> {
		match x {
			Var::This => Ok(self.this.as_ref().ok_or_else(|| self.error(E::NoThis))?.borrow()),
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
			None => Err(Error::new(E::UnboundVariable))
		}
	}

	pub fn borrow_mut(&mut self, x: Var<T>) -> Result<&mut Value<'v>, Error> {
		match x {
			Var::This => self.this.as_mut().ok_or_else(|| Error::new(E::NoThis))?.borrow_mut(),
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

	// pub fn call(&mut self, f_index: u32, this: Option<value::Borrowed<'a, 'v>>, args: Vec<Value<'v>>) -> Result<Value<'v>, Error> {
	// 	func_call!(self, f_index, this, args)
	// }

	// pub fn eval(&mut self, e: &'e Expr<T>) -> Result<Value<'v>, Error> {
	// 	match e {
	// 		Expr::Literal(c) => { eprintln!("literal");
	// 			Ok(Value::Constant(c.clone()))
	// 		},
	// 		Expr::Get(v) => { eprintln!("get");
	// 			match v {
	// 				Var::This => self.this.as_ref().ok_or_else(|| self.error(E::NoThis))?.borrow().copy(),
	// 				Var::Defined(x) => self.get_mut(*x)?.value.copy_or_move()
	// 			}
	// 		},
	// 		Expr::GetField(x, ty_ref, index) => { eprintln!("get-field");
	// 			let index = *index;
	// 			let value = self.get_mut(*x)?.value.borrow_mut()?;
	// 			match value {
	// 				Value::Instance(value_ty_ref, data) => {
	// 					if value_ty_ref == ty_ref {
	// 						match data {
	// 							value::InstanceData::Struct(bindings) => {
	// 								bindings.get_mut(index as usize).ok_or_else(|| Error::new(E::UndefinedField(index)))?.copy_or_move()
	// 							}
	// 							_ => self.err(E::GetFieldFromNonStruct)
	// 						}
	// 					} else {
	// 						self.err(E::IncompatibleType)
	// 					}
	// 				},
	// 				_ => self.err(E::NotAnInstance)
	// 			}
	// 		},
	// 		Expr::Let(x, mutable, e, next) => { eprintln!("let");
	// 			let value = self.eval(e)?;
	// 			self.bind(*x, Variable::new(value, *mutable));
	// 			self.eval(next)
	// 		}
	// 		Expr::Update(x, e, next) => { eprintln!("update");
	// 			let value = self.eval(e)?;
	// 			self.get_mut(*x)?.update(value)?;
	// 			self.eval(next)
	// 		},
	// 		Expr::New(ty_ref, args) => { eprintln!("new");
	// 			let mut eargs = Vec::with_capacity(args.len());
	// 			for a in args {
	// 				eargs.push(self.eval(a)?)
	// 			}

	// 			self.instanciate(*ty_ref, eargs)
	// 		},
	// 		Expr::Cons(ty_ref, index, args) => { eprintln!("cons");
	// 			let ty = self.context.ty(*ty_ref).unwrap();
	// 			match ty.desc() {
	// 				ty::Desc::Enum(enm) => {
	// 					let variant = enm.variant(*index).expect("unknown variant");
	// 					let len = args.len() as u32;
	// 					let expected_len = variant.len();

	// 					if len == expected_len {
	// 						let mut eargs = Vec::new();
	// 						for a in args {
	// 							eargs.push(value::MaybeMoved::new(self.eval(a)?));
	// 							// TODO check types.
	// 						}
		
	// 						Ok(Value::Instance(*ty_ref, value::InstanceData::EnumVariant(*index, eargs)))
	// 					} else {
	// 						self.err(E::InvalidFieldCount(expected_len, len))
	// 					}
	// 				},
	// 				_ => self.err(E::NotAnEnumType(*ty_ref))
	// 			}
	// 		}
	// 		Expr::Heap(e) => { eprintln!("heap");
	// 			self.eval(e)
	// 		},
	// 		Expr::Match { expr, cases } => { eprintln!("match");
	// 			let value = self.eval(expr)?;

	// 			for case in cases {
	// 				if value.matches(&case.pattern)? {
	// 					self.let_match(value, &case.pattern)?;
	// 					return self.eval(&case.expr)
	// 				}
	// 			}
				
	// 			self.err(E::NoMatch)
	// 		}
	// 		Expr::LetMatch(pattern, expr, next) => { eprintln!("let-match");
	// 			let value = self.eval(expr)?;
	// 			self.let_match(value, pattern)?;
	// 			self.eval(next)
	// 		}
	// 		Expr::Call(f_index, this, args) => { eprintln!("call");
	// 			let mut eargs = Vec::with_capacity(args.len());
	// 			for a in args {
	// 				eargs.push(self.eval(a)?)
	// 			}

	// 			let this = match this {
	// 				Some(x) => {
	// 					let f = self.context.function(*f_index).expect("unknown function");
	// 					if f.is_method_mut() {
	// 						Some(value::Borrowed::Mut(self.vars.get_mut(&x).ok_or_else(|| Error::new(E::UnboundVariable))?.value.borrow_mut()?))
	// 					} else {
	// 						Some(value::Borrowed::Const(self.vars.get(&x).ok_or_else(|| self.error(E::UnboundVariable))?.value.borrow()?))
	// 					}
	// 				}
	// 				None => None
	// 			};

	// 			let f_index = *f_index;
	// 			func_call!(self, f_index, this, eargs)
	// 		}
	// 		Expr::TailRecursion { label, args, body } => { eprintln!("loop");
	// 			self.stack.push(Frame::new(*label, args.clone(), body));
	// 			self.eval(body)
	// 		}
	// 		Expr::Recurse(label, args) => { eprintln!("continue");
	// 			loop {
	// 				match self.stack.last() {
	// 					Some(frame) => {
	// 						if frame.label == *label {
	// 							break
	// 						} else {
	// 							self.stack.pop();
	// 						}
	// 					},
	// 					None => return self.err(E::UnreachableLabel)
	// 				}
	// 			}

	// 			let frame = self.stack.last_mut().unwrap();
	// 			if frame.args == *args {
	// 				frame.clear();
	// 				let expr = frame.expr;
	// 				self.eval(expr)
	// 			} else {
	// 				self.err(E::RecursionArgsMissmatch)
	// 			}
	// 		}
	// 		Expr::Lexer(lexer, e) => { eprintln!("lexer");
	// 			match e {
	// 				LexerExpr::Peek => {
	// 					let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
	// 					Ok(lexer.peek())
	// 				}
	// 				LexerExpr::Span => {
	// 					let lexer = self.borrow(*lexer)?.as_lexer()?;
	// 					Ok(lexer.span())
	// 				}
	// 				LexerExpr::Chars => {
	// 					let lexer = self.borrow(*lexer)?.as_lexer()?;
	// 					Ok(lexer.chars())
	// 				}
	// 				LexerExpr::Buffer => {
	// 					let lexer = self.borrow(*lexer)?.as_lexer()?;
	// 					Ok(lexer.buffer())
	// 				}
	// 				LexerExpr::Clear(next) => {
	// 					let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
	// 					lexer.clear();
	// 					self.eval(next)
	// 				}
	// 				LexerExpr::Consume(next) => {
	// 					let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
	// 					match lexer.consume() {
	// 						Ok(()) => self.eval(next),
	// 						Err(e) => Ok(Value::err(e))
	// 					}
	// 				}
	// 			}
	// 		}
	// 		Expr::Stream(stream, e) => { eprintln!("stream");
	// 			match e {
	// 				StreamExpr::Pull(x, next) => {
	// 					let context = self.context;
	// 					let value = stream::pull(self.borrow_mut(*stream)?, context)?;
	// 					self.bind(*x, Variable::new(value, false));
	// 					self.eval(next)
	// 				}
	// 			}
	// 		}
	// 		Expr::Stack(stack, e) => { eprintln!("stack");
	// 			match e {
	// 				StackExpr::Push(value, state, next) => {
	// 					let value = self.eval(value)?;
	// 					let state = self.eval(state)?;
	// 					let stack = self.borrow_mut(*stack)?.as_stack_mut()?;
	// 					stack.push(value, state);
	// 					self.eval(next)
	// 				}
	// 				StackExpr::Pop(x, y, next) => {
	// 					let stack = self.borrow_mut(*stack)?.as_stack_mut()?;
	// 					let (vx, vy) = stack.pop()?;

	// 					if let Some(x) = x {
	// 						self.bind(*x, Variable::new(vx, false))
	// 					}

	// 					if let Some(y) = y {
	// 						self.bind(*y, Variable::new(vy, false))
	// 					}

	// 					self.eval(next)
	// 				}
	// 			}
	// 		}
	// 		Expr::Span(e) => { eprintln!("span");
	// 			match e {
	// 				SpanExpr::Locate(expr, span) => {
	// 					let value = self.eval(expr)?;
	// 					let span = self.eval(span)?.into_span()?;
	// 					Ok(Value::Loc(Loc::new(Box::new(value), span)))
	// 				}
	// 				SpanExpr::FromPosition(pos) => {
	// 					let pos = self.eval(pos)?.into_position()?;
	// 					Ok(Value::Span(pos.into()))
	// 				}
	// 				SpanExpr::After(span) => {
	// 					let span = self.eval(span)?.into_span()?;
	// 					Ok(Value::Position(span.end()))
	// 				}
	// 				SpanExpr::Transpose(loc_opt, default_span) => {
	// 					let loc_opt = self.eval(loc_opt)?.into_option()?.map(Value::into_loc).transpose()?;
	// 					let default_span = self.eval(default_span)?.into_span()?;
	// 					Ok(Value::Loc(Loc::transposed(loc_opt, default_span).map(|v| Box::new(Value::option(v)))))
	// 				}
	// 				SpanExpr::Unwrap(x, y, loc, next) => {
	// 					let (value, span) = self.eval(loc)?.into_loc()?.into_raw_parts();
						
	// 					if let Some(x) = x {
	// 						self.bind(*x, Variable::new(value, false))
	// 					}

	// 					if let Some(y) = y {
	// 						self.bind(*y, Variable::new(Value::Span(span), false))
	// 					}

	// 					self.eval(next)
	// 				}
	// 				SpanExpr::Merge(a, b) => {
	// 					let a = self.eval(a)?.into_span()?;
	// 					let b = self.eval(b)?.into_span()?;
	// 					Ok(Value::Span(a.union(b)))
	// 				}
	// 			}
	// 		}
	// 		Expr::Print(string, next) => { eprintln!("print");
	// 			eprintln!("{}", string);
	// 			self.eval(next)
	// 		}
	// 		Expr::Write(output, string, next) => {
	// 			let output = self.borrow_mut(*output)?.as_output_mut()?;
	// 			output.write_all(string.as_bytes()).map_err(|e| self.error(E::IO(e)))?;
	// 			self.eval(next)
	// 		}
	// 		Expr::Check(x, e, next) => { eprintln!("check");
	// 			let value = self.eval(e)?;
	// 			if value.is_err() {
	// 				Ok(value)
	// 			} else {
	// 				self.bind(*x, Variable::new(value.expect_ok()?, false));
	// 				self.eval(next)
	// 			}
	// 		}
	// 		Expr::Error(e) => { eprintln!("error");
	// 			Ok(Value::Error(match e {
	// 				expr::Error::UnexpectedToken(e) => {
	// 					error::Value::UnexpectedToken(Box::new(self.eval(&e)?))
	// 				}
	// 				expr::Error::UnexpectedNode(e) => {
	// 					error::Value::UnexpectedNode(Box::new(self.eval(&e)?))
	// 				}
	// 			}))
	// 		}
	// 		Expr::Unreachable => { eprintln!("unreachable");
	// 			self.err(E::UnreachableReached)
	// 		}
	// 	}
	// }

	pub fn let_match(&mut self, value: Value<'v>, pattern: &Pattern<T>) -> Result<(), Error> {
		use super::fmt::ContextDisplay;
		match (pattern, value) {
			(Pattern::Any, _) => (),
			(Pattern::Bind(x), value) => { self.bind(*x, Variable::new(value, false)); },
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
}

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
			Err(Error::new(E::NotMutable))
		}
	}

	pub fn value(&mut self) -> &value::MaybeMoved<'v> {
		&self.value
	}

	pub fn value_mut(&mut self) -> &mut value::MaybeMoved<'v> {
		&mut self.value
	}
}

pub struct Frame<'v, 'e, T: Namespace> {
	label: T::Label,

	args: Vec<Var<T>>,

	expr: &'e Expr<T>,

	/// Variables local to the frame.
	vars: HashMap<T::Var, Variable<'v>>,
}

impl<'v, 'e, T: Namespace> Frame<'v, 'e, T> {
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