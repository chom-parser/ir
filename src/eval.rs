use slab::Slab;
use source_span::{
	Position,
	Span,
	Loc
};
use crate::{
	Context,
	Namespace,
	Constant,
	Expr,
	expr::{
		self,
		LexerExpr,
		StreamExpr,
		StackExpr,
		SpanExpr
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

	/// Pop a boolean value from the stack and
	/// use it as condition to execute the if-then-else.
	If(&'e Expr<T>, &'e Expr<T>),

	/// Pop a value from the stack and
	/// perform a pattern matching on it using the
	/// given cases.
	Match(&'e [expr::MatchCase<T>]),

	/// Pop a reference value from the stack and
	/// perform a pattern matching on it using the
	/// given cases.
	MatchRef(&'e [expr::MatchCase<T>]),

	/// Pop a value from the stack, unwrap it
	/// according to the given pattern then
	/// evaluate the given expression.
	LetMatch(&'e Pattern<T>, &'e Expr<T>),

	/// Pop the given amount of values (last parameter) from
	/// the stack and use them to call the given function
	/// identified by its index (first parameter).
	Call(u32, usize),

	/// Return from a function call.
	Return,

	/// Pop two values from the stack (a pair (value, state)) and
	/// put them on the given stack.
	/// 
	/// The reference must be mutable.
	StackPush(Reference),

	/// Pop two values from the stack (a pair (value, span)) and
	/// wrap them as a located value.
	SpanLocate,

	/// Pop a position value from the stack and
	/// turn it into a span value. 
	SpanFromPosition,

	/// Pop a span value from the stack and
	/// return its after position.
	SpanAfter,

	/// Pop two values from the stack (a pair (loc_opt, default_span))
	/// and transpose the first value using the second
	/// as default span.
	SpanTranspose,

	/// Pop a span value from the stack unwraps it and
	/// bind the inner value to the first variable (if given)
	/// and the span to the second variable (if given).
	SpanUnwrap(Option<T::Var>, Option<T::Var>),

	/// Pop two span values from the stack and merge them.
	SpanMerge,

	/// Pop a result value from the stack.
	/// If it is a `Result::Err(err)` returns `Result::Err(err)`.
	/// If it is a `Result::Ok(value)`, binds
	/// `value` to the given variable and evaluates
	/// the given expression.
	Check(T::Var, &'e Expr<T>),

	/// Pop a `result(option(t), e)` value and transpose it
	/// into a `option(result(t, e))` value.
	ResultTranspose,

	/// Pop a `option(result(t, e))` value and transpose it
	/// into a `result(option(t), e)` value.
	OptionTranspose,

	/// Pop a token value and create an unexpected token error.
	ErrorUnexpectedToken,

	/// Pop a node value and create an unexpected node error.
	ErrorUnexpectedNode,

	DebugFormat
}

enum DebugFormatMethod {
	Ref,
	Call(u32),
	Opaque,
	Loc
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
			stack: vec![Frame::new()],
			actions: Vec::new(),
			values: Vec::new()
		}
	}

	pub fn instanciate_lexer<I: Iterator<Item=std::io::Result<char>>>(&mut self, input: &'v mut I) -> Result<Value<'v>, Error> {
		match self.context.lexer_types().next() {
			Some(ty_index) => {
				self.instanciate(ty::Ref::Defined(ty_index), Some(Value::Input(input)))
			},
			None => Err(Error::new(error::Desc::NoLexer))
		}
	}

	pub fn parse(&mut self, parser: u32, lexer: Value<'v>) -> Result<Result<Value<'v>, Loc<error::Value<'v>>>, Error> {
		let result = self.call(parser, vec![lexer])?;
		Ok(match result.into_result()? {
			Ok(v) => Ok(v),
			Err(v) => Err(v.into_loc_error()?)
		})
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

	pub fn store(&mut self, value: Value<'v>) -> Reference {
		let addr = self.memory.insert(value);
		Reference {
			mutable: true,
			addr,
			path: Vec::new()
		}
	}

	fn debug_format_method(&self, value: &Value<'v>) -> Result<DebugFormatMethod, Error> {
		use crate::eval::fmt::ContextDisplay;
		Ok(match value {
			Value::Reference(_) => {
				DebugFormatMethod::Ref
			}
			Value::Instance(ty_ref, _) => {
				match ty_ref {
					ty::Ref::Defined(_) => {
						match self.context.ty(*ty_ref).unwrap().desc() {
							ty::Desc::Opaque => DebugFormatMethod::Opaque,
							_ => {
								match self.context.debug_formatters_for(*ty_ref).next() {
									Some(index) => DebugFormatMethod::Call(index),
									_ => return self.err(E::UndefinedDebugFormatter(format!("{}", value.display_in(self.context()))))
								}
							}
						}
					}
					_ => return self.err(E::UndefinedDebugFormatter(format!("{}", value.display_in(self.context()))))
				}
			}
			Value::Loc(_) => DebugFormatMethod::Loc,
			Value::Opaque(_, _) => DebugFormatMethod::Opaque,
			_ => return self.err(E::UndefinedDebugFormatter(format!("{}", value.display_in(self.context()))))
		})
	}

	fn exec(&mut self) -> Result<Value<'v>, Error> {
		while let Some(action) = self.actions.pop() {
			self.step(action)?;
		}

		Ok(self.values.pop().unwrap())
	}

	pub fn eval(&mut self, e: &'e Expr<T>) -> Result<Value<'v>, Error> {
		self.actions.push(Action::Eval(e));
		self.exec()
	}

	pub fn call(&mut self, f_index: u32, args: Vec<Value<'v>>) -> Result<Value<'v>, Error> {
		let len = args.len();
		self.values.extend(args);
		self.actions.push(Action::Call(f_index, len));
		self.exec()
	}

	pub fn debug_format(&mut self, value: Value<'v>) -> Result<String, Error> {
		let output = Value::Output(value::Output::string());
		let r = self.store(value);
		self.compile_debug_format(output, r)?;
		let output = self.exec()?.into_output()?;
		Ok(output.into_string().unwrap())
	}

	fn compile_debug_format(&mut self, output: Value<'v>, mut value_ref: Reference) -> Result<(), Error> {
		loop {
			let method = self.debug_format_method(self.get(&value_ref)?)?;
			match method {
				DebugFormatMethod::Ref => {
					value_ref = self.get(&value_ref)?.as_reference()?.clone()
				}
				DebugFormatMethod::Call(index) => {
					self.values.push(output);
					self.values.push(Value::Reference(value_ref));
					self.actions.push(Action::Call(index, 2));
					break
				}
				DebugFormatMethod::Opaque => {
					let (ty_ref, inner) = self.get(&value_ref)?.as_opaque()?;
					let ty = self.context.ty(ty_ref).unwrap();
					let string = format!("{}({})", ty.id().ident(self.context.id()), inner);
					let mut output = output.into_output()?;
					output.write(&string).map_err(|e| self.error(E::IO(e)))?;
					self.values.push(Value::Output(output));
					break
				}
				DebugFormatMethod::Loc => {
					value_ref.loc_inner()
				}
			}
		}

		Ok(())
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

	fn borrow(&self, x: T::Var) -> Result<&Value<'v>, Error> {
		Ok(&self.memory[self.frame().borrow(self.context().id(), x)?])
	}

	// fn deref(&self, x: T::Var) -> Result<&Value<'v>, Error> {
	// 	let addr = match self.borrow(x)? {
	// 		Value::Reference(r) => r.addr,
	// 		_ => return self.err(E::IncompatibleType)
	// 	};

	// 	Ok(&self.memory[addr])
	// }

	fn borrow_mut(&mut self, x: T::Var) -> Result<&mut Value<'v>, Error> {
		let addr = self.frame().borrow_mut(self.context().id(), x)?;
		Ok(&mut self.memory[addr])
	}

	fn reference_to(&self, x: T::Var, mutable: bool) -> Result<Reference, Error> {
		let addr = if mutable {
			self.frame().borrow_mut(self.context().id(), x)?
		} else {
			self.frame().borrow(self.context().id(), x)?
		};

		Ok(Reference {
			addr,
			mutable,
			path: Vec::new()
		})
	}

	fn get(&self, r: &Reference) -> Result<&Value<'v>, Error> {
		let mut value = &self.memory[r.addr];

		for s in &r.path {
			match s {
				frame::Segment::LocInner => {
					value = value.as_loc()?.as_ref().as_ref();
				}
				frame::Segment::Field(i) => {
					use crate::eval::fmt::ContextDisplay;
					value = match value {
						Value::Instance(_, value::InstanceData::EnumVariant(_, args)) => args[*i as usize].borrow()?,
						Value::Instance(_, value::InstanceData::Struct(args)) => args[*i as usize].borrow()?,
						_ => panic!("invalid field ref on value: {}", value.display_in(self.context()))
					}
				}
			}
		}

		Ok(value)
	}

	fn get_mut(&mut self, r: &Reference) -> Result<&mut Value<'v>, Error> {
		let mut value = &mut self.memory[r.addr];

		for s in &r.path {
			match s {
				frame::Segment::LocInner => {
					value = value.as_loc_mut()?.as_mut().as_mut();
				}
				frame::Segment::Field(i) => {
					value = match value {
						Value::Instance(_, value::InstanceData::EnumVariant(_, args)) => args[*i as usize].borrow_mut()?,
						Value::Instance(_, value::InstanceData::Struct(args)) => args[*i as usize].borrow_mut()?,
						_ => panic!("invalid field ref")
					}
				}
			}
		}

		Ok(value)
	}

	// fn take_reference(&mut self, r: Reference) -> Result<Value<'v>, Error> {
	// 	if self.memory[r.addr].is_copiable() {
	// 		Ok(self.memory[r.addr].copy()?)
	// 	} else {
	// 		let ns = self.context().id();
	// 		self.frame_mut().take(ns, x)?;
	// 		Ok(self.memory.remove(r.addr))
	// 	}
	// }

	fn take(&mut self, x: T::Var) -> Result<Value<'v>, Error> {
		let addr = self.frame().borrow(self.context().id(), x).map_err(|e| e.into_already_moved())?;
		if self.memory[addr].is_copiable() {
			Ok(self.memory[addr].copy()?)
		} else {
			let ns = self.context().id();
			self.frame_mut().take(ns, x)?;
			Ok(self.memory.remove(addr))
		}
	}

	fn let_match_ref(&mut self, r: &Reference, pattern: &Pattern<T>) -> Result<(), Error> {
		let mut bindings = Vec::new();
		self.let_match_ref_bindings(&mut bindings, r, pattern)?;
		for (x, r) in bindings {
			self.bind(x, false, Value::Reference(r))
		}
		Ok(())
	}

	fn let_match_ref_bindings(&self, bindings: &mut Vec<(T::Var, Reference)>, r: &Reference, pattern: &Pattern<T>) -> Result<(), Error> {
		use fmt::ContextDisplay;

		let value = self.get(r)?;
		match (pattern, value) {
			(Pattern::Any, _) => (),
			(Pattern::Bind(x), _) => { bindings.push((*x, r.clone())) },
			(Pattern::Literal(a), Value::Constant(b)) => {
				if !a.matches(&b) {
					return self.err(E::PatternMissmatch(
						format!("{}", pattern.display_in(self.context())), 
						format!("{}", value.display_in(self.context())), 
					))
				}
			},
			(Pattern::Cons(ty_a, va, patterns), Value::Instance(ty_b, data)) => {
				if ty_a == ty_b {
					match data {
						value::InstanceData::EnumVariant(vb, args) => {
							let len = args.len() as u32;
							let expected_len = patterns.len() as u32;
							if va == vb {
								if expected_len == len {
									for (i, a) in args.into_iter().enumerate() {
										let mut a_ref = r.clone();
										a_ref.field(i as u32);
										self.let_match_ref_bindings(bindings, &a_ref, &patterns[i])?
									}
								} else {
									return self.err(E::InvalidFieldCount(len, expected_len))
								}
							} else {
								return self.err(E::PatternMissmatch(
									format!("{}", pattern.display_in(self.context())), 
									format!("{}", value.display_in(self.context())), 
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
		let value = self.values.pop().unwrap();
		// use fmt::ContextDisplay;
		value
	}

	/// Pop `n` values and return them in reverse order.
	fn pop_values(&mut self, n: usize) -> Vec<Value<'v>> {
		let mut values = Vec::with_capacity(n);
		for _ in 0..n {
			values.push(self.pop_value())
		}
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
						let value = self.borrow_mut(*x)?;
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
					Expr::Ref(x) => {
						let value = Value::Reference(self.reference_to(*x, false)?);
						self.values.push(value)
					}
					Expr::RefField(x, _, index) => {
						let mut r = self.take(*x)?.into_reference()?;
						r.field(*index);
						self.values.push(Value::Reference(r))
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
					Expr::If(condition, then_branch, else_branch) => {
						self.actions.push(Action::If(then_branch, else_branch));
						self.actions.push(Action::Eval(condition))
					}
					Expr::Match { expr, cases } => {
						self.actions.push(Action::Match(cases));
						self.actions.push(Action::Eval(expr))
					}
					Expr::MatchRef { expr, cases } => {
						self.actions.push(Action::MatchRef(cases));
						self.actions.push(Action::Eval(expr))
					}
					Expr::LetMatch(pattern, expr, next) => {
						self.actions.push(Action::LetMatch(pattern, next));
						self.actions.push(Action::Eval(expr))
					}
					Expr::Call(f_index, args) => {
						// let this = this.map(|(x, mutable)| self.reference_to(Var::Defined(x), mutable)).transpose()?;
						self.actions.push(Action::Call(*f_index, args.len()));
						self.actions.extend(args.iter().map(Action::Eval));
					}
					Expr::Return(args) => {
						self.actions.extend(args.iter().map(Action::Eval));
					}
					Expr::TailRecursion { label, args, body } => {
						let ns = self.context.id();
						self.frame_mut().begin_loop(ns, *label, args.clone(), body)?;
						self.actions.push(Action::Eval(body))
					}
					Expr::Recurse(label, args) => {
						let body = self.frame_mut().continue_loop(*label, args)?;
						self.actions.push(Action::Eval(body))
					}
					Expr::Lexer(lexer, e) => {
						match e {
							LexerExpr::Peek => {
								let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
								let value = lexer.peek();
								self.values.push(value)
							}
							LexerExpr::Span => {
								let lexer = self.borrow(*lexer)?.as_lexer()?;
								let value = lexer.span();
								self.values.push(value)
							}
							LexerExpr::Chars => {
								let lexer = self.borrow(*lexer)?.as_lexer()?;
								let value = lexer.chars();
								self.values.push(value)
							}
							LexerExpr::IsEmpty => {
								let lexer = self.borrow(*lexer)?.as_lexer()?;
								let value = lexer.is_empty();
								self.values.push(value)
							}
							LexerExpr::Buffer => {
								let lexer = self.borrow(*lexer)?.as_lexer()?;
								let value = lexer.buffer();
								self.values.push(value)
							}
							LexerExpr::Clear(next) => {
								let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
								lexer.clear();
								self.actions.push(Action::Eval(next))
							}
							LexerExpr::Consume(next) => {
								let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
								match lexer.consume() {
									Ok(()) => self.actions.push(Action::Eval(next)),
									Err(e) => self.values.push(Value::err(e))
								}
							}
						}
					}
					Expr::Stream(stream, e) => {
						match e {
							StreamExpr::Pull(x, next) => {
								enum StreamAction {
									CallMethod(u32),
									CharsNext
								}

								let action = match self.borrow(*stream)? {
									Value::Lexer(lexer) => {
										StreamAction::CallMethod(lexer.method())
									}
									Value::Chars(_) => {
										StreamAction::CharsNext
									},
									_ => return Err(Error::new(E::NotAStream))
								};

								self.actions.push(Action::Eval(next));
								self.actions.push(Action::Bind(*x, false));
								self.actions.push(Action::Update(*stream));
								match action {
									StreamAction::CallMethod(index) => {
										self.actions.push(Action::ResultTranspose);
										self.actions.push(Action::Call(index, 1));
										let stream = self.take(*stream)?;
										self.values.push(stream);
									}
									StreamAction::CharsNext => {
										let chars = self.borrow_mut(*stream)?.as_chars_mut()?;
										let value = match chars.next() {
											Some(c) => Value::some(Value::Constant(Constant::Char(c))),
											None => Value::none()
										};
										self.values.push(value)
									}
								}
							}
						}
					}
					Expr::Stack(stack, e) => {
						match e {
							StackExpr::Push(value, state, next) => {
								self.actions.push(Action::Eval(next));
								self.actions.push(Action::StackPush(self.reference_to(*stack, true)?));
								self.actions.push(Action::Eval(value));
								self.actions.push(Action::Eval(state))
							}
							StackExpr::Pop(x, y, next) => {
								let stack = self.borrow_mut(*stack)?.as_stack_mut()?;
								let (vx, vy) = stack.pop()?;

								if let Some(x) = x {
									self.bind(*x, false, vx)
								}

								if let Some(y) = y {
									self.bind(*y, false, vy)
								}

								self.actions.push(Action::Eval(next))
							}
						}
					}
					Expr::Span(e) => {
						match e {
							SpanExpr::Locate(expr, span) => {
								self.actions.push(Action::SpanLocate);
								self.actions.push(Action::Eval(expr));
								self.actions.push(Action::Eval(span))
							}
							SpanExpr::FromPosition(pos) => {
								self.actions.push(Action::SpanFromPosition);
								self.actions.push(Action::Eval(pos))
							}
							SpanExpr::After(span) => {
								self.actions.push(Action::SpanAfter);
								self.actions.push(Action::Eval(span))
							}
							SpanExpr::Transpose(loc_opt, default_span) => {
								self.actions.push(Action::SpanTranspose);
								self.actions.push(Action::Eval(loc_opt));
								self.actions.push(Action::Eval(default_span))
							}
							SpanExpr::Unwrap(x, y, loc, next) => {
								self.actions.push(Action::Eval(next));
								self.actions.push(Action::SpanUnwrap(*x, *y));
								self.actions.push(Action::Eval(loc))
							}
							SpanExpr::Merge(a, b) => {
								self.actions.push(Action::SpanMerge);
								self.actions.push(Action::Eval(a));
								self.actions.push(Action::Eval(b))
							}
						}
					}
					Expr::Print(string, next) => {
						eprintln!("{}", string);
						self.actions.push(Action::Eval(next))
					}
					Expr::Write(output, string, next) => {
						let output = self.borrow_mut(*output)?.as_output_mut()?;
						output.write(string).map_err(|e| self.error(E::IO(e)))?;
						self.actions.push(Action::Eval(next))
					}
					Expr::DebugFormat(output, value, next) => {
						self.actions.push(Action::Eval(next));
						self.actions.push(Action::Update(*output));
						self.actions.push(Action::DebugFormat);
						self.actions.push(Action::Eval(value));

						let output = self.take(*output)?;
						self.values.push(output);
					}
					Expr::Check(x, e, next) => {
						self.actions.push(Action::Check(*x, next));
						self.actions.push(Action::Eval(e))
					}
					Expr::Transpose(e) => {
						self.actions.push(Action::OptionTranspose);
						self.actions.push(Action::Eval(e))
					}
					Expr::Error(e) => {
						match e {
							expr::Error::UnexpectedToken(e) => {
								self.actions.push(Action::ErrorUnexpectedToken);
								self.actions.push(Action::Eval(e))
							}
							expr::Error::UnexpectedNode(e) => {
								self.actions.push(Action::ErrorUnexpectedNode);
								self.actions.push(Action::Eval(e))
							}
						}
					}
					Expr::Unreachable => {
						return self.err(E::UnreachableReached)
					}
				}
			}
			Action::Bind(x, mutable) => {
				let value = self.pop_value();
				self.bind(x, mutable, value)
			}
			Action::Update(x) => {
				let value = self.pop_value();
				self.bind(x, true, value)
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
			Action::If(then_branch, else_branch) => {
				let condition = self.pop_value().into_bool()?;
				if condition {
					self.actions.push(Action::Eval(then_branch))
				} else {
					self.actions.push(Action::Eval(else_branch))
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
			Action::MatchRef(cases) => {
				let r = self.pop_value().into_reference()?;

				let value = self.get(&r)?;
				for case in cases {
					if value.matches(&case.pattern)? {
						self.let_match_ref(&r, &case.pattern)?;
						self.actions.push(Action::Eval(&case.expr));
						return Ok(())
					}
				}

				use crate::eval::fmt::ContextDisplay;
				return self.err(E::NoMatchRef(format!("{}", value.display_in(self.context()))))
			}
			Action::LetMatch(pattern, next) => {
				let value = self.pop_value();
				self.let_match(value, pattern)?;
				self.actions.push(Action::Eval(next))
			}
			Action::Call(f_index, n) => {
				let args = self.pop_values(n);
				let f: &'e function::Function<T> = self.context.function(f_index).expect("unknown function");
				let len = args.len() as u32;

				match f.body() {
					Some(body) => {
						let expected_len = f.signature().arity();
						
						if expected_len == len {
							self.stack.push(Frame::new());

							for (i, a) in args.into_iter().enumerate() {
								let arg = &f.signature().arguments()[i];
								self.bind(arg.id(), arg.is_mutable(), a)
							}

							self.actions.push(Action::Return);
							self.actions.push(Action::Eval(body))
						} else {
							return self.err(E::InvalidNumberOfArguments(expected_len, len))
						}
					},
					None => {
						match f.signature().marker() {
							Some(function::Marker::UnexpectedChar) => {
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
							Some(function::Marker::ExternParser) => {
								if len == 1 {
									let ty_ref = f.signature().return_types()[0].ok_type().unwrap().instance_type_reference().unwrap();
									let arg = args.into_iter().next().unwrap().into_string()?;
									self.values.push(Value::Opaque(ty_ref, arg));
								} else {
									return self.err(E::InvalidNumberOfArguments(1, len))
								}
							},
							_ => return self.err(E::UnimplementedFunction)
						}
					}
				}
			}
			Action::Return => {
				self.stack.pop();
			}
			Action::StackPush(r) => {
				debug_assert!(r.mutable);
				let value = self.pop_value();
				let state = self.pop_value();
				let stack = self.get_mut(&r)?.as_stack_mut()?;
				stack.push(value, state)
			}
			Action::SpanLocate => {
				let value = self.pop_value();
				let span = self.pop_value().into_span()?;
				self.values.push(Value::Loc(Loc::new(Box::new(value), span)))
			}
			Action::SpanFromPosition => {
				let pos = self.pop_value().into_position()?;
				self.values.push(Value::Span(pos.into()))
			}
			Action::SpanAfter => {
				let span = self.pop_value().into_span()?;
				self.values.push(Value::Position(span.end()))
			}
			Action::SpanTranspose => {
				let loc_opt = self.pop_value().into_option()?.map(Value::into_loc).transpose()?;
				let default_span = self.pop_value().into_span()?;
				self.values.push(Value::Loc(Loc::transposed(loc_opt, default_span).map(|v| Box::new(Value::option(v)))))
			}
			Action::SpanUnwrap(x, y) => {
				let (value, span) = self.pop_value().into_loc()?.into_raw_parts();

				if let Some(x) = x {
					self.bind(x, false, value)
				}

				if let Some(y) = y {
					self.bind(y, false, Value::Span(span))
				}
			}
			Action::SpanMerge => {
				let a = self.pop_value().into_span()?;
				let b = self.pop_value().into_span()?;
				self.values.push(Value::Span(a.union(b)))
			}
			Action::Check(x, next) => {
				let value = self.pop_value();
				if value.is_err() {
					self.values.push(value)
				} else {
					self.bind(x, false, value.expect_ok()?);
					self.actions.push(Action::Eval(next))
				}
			}
			Action::ResultTranspose => {
				let value = self.pop_value();
				let transposed = match value.into_result()? {
					Ok(value_opt) => {
						match value_opt.into_option()? {
							Some(value) => Value::some(Value::ok(value)),
							None => Value::none()
						}
					}
					Err(e) => Value::some(Value::err(e))
				};
				self.values.push(transposed)
			}
			Action::OptionTranspose => {
				let value = self.pop_value();
				let transposed = match value.into_option()? {
					Some(result) => match result.into_result()? {
						Ok(value) => Value::ok(Value::some(value)),
						Err(e) => Value::err(e)
					}
					None => Value::ok(Value::none())
				};
				self.values.push(transposed)
			}
			Action::ErrorUnexpectedToken => {
				let e = self.pop_value();
				self.values.push(Value::Error(error::Value::UnexpectedToken(Box::new(e))))
			}
			Action::ErrorUnexpectedNode => {
				let e = self.pop_value();
				self.values.push(Value::Error(error::Value::UnexpectedNode(Box::new(e))))
			}
			Action::DebugFormat => {
				let value = self.pop_value();
				let output = self.pop_value();

				let value_ref = self.store(value);
				// let value_ref = self.reference_to(*x, false)?;
				self.compile_debug_format(output, value_ref)?
			}
		}

		Ok(())
	}
}