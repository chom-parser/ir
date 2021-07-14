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

pub struct Frame<'e, T: Namespace> {
	/// The current variables values.
	bindings: HashMap<T::Var, Binding>,

	/// The current `Var::This`.
	this: Option<Reference>,

	/// Loop stack.
	stack: Vec<LoopFrame<'e, T>>
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
						let mut evaluator = super::Evaluator::new(env);
						evaluator.eval(body)
						// env.eval(body)
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

impl<'e, T: Namespace> Frame<'e, T> {
	pub fn new(this: Option<Reference>) -> Self {
		Self {
			bindings: HashMap::new(),
			this,
			stack: Vec::new()
		}
	}

	fn error(&self, e: E) -> Error {
		Error::new(e)
	}

	fn err<X>(&self, e: E) -> Result<X, Error> {
		Err(self.error(e))
	}

	pub fn this(&self) -> Option<Reference> {
		self.this
	}

	pub fn set_this(&mut self, this: Reference) {
		self.this = Some(this)
	}

	pub fn get(&self, x: Var<T>) -> Result<Reference, Error> {
		match x {
			Var::This => self.this.ok_or_else(|| self.error(E::NoThis)),
			Var::Defined(x) => {
				for frame in self.stack.iter().rev() {
					if let Some(b) = frame.bindings.get(&x) {
						return b.bound()
					}
				}
		
				match self.bindings.get(&x) {
					Some(b) => b.bound(),
					None => self.err(E::UnboundVariable)
				}
			}
		}
	}

	pub fn take(&mut self, x: T::Var) -> Result<usize, Error> {
		for frame in self.stack.iter_mut().rev() {
			if let Some(b) = frame.bindings.get_mut(&x) {
				return Ok(b.take()?.addr)
			}
		}

		match self.bindings.get_mut(&x) {
			Some(b) => Ok(b.take()?.addr),
			None => self.err(E::UnboundVariable)
		}
	}

	pub fn borrow(&self, x: Var<T>) -> Result<usize, Error> {
		Ok(self.get(x)?.addr)
	}

	pub fn borrow_mut(&self, x: Var<T>) -> Result<usize, Error> {
		let r = self.get(x)?;
		if r.mutable {
			Ok(r.addr)
		} else {
			Err(Error::new(E::NotMutable))
		}
	}

	pub fn bind(&mut self, x: T::Var, mutable: bool, addr: usize) {
		if let Some(frame) = self.stack.last_mut() {
			frame.bindings.insert(x, Binding::new(mutable, addr));
		} else {
			self.bindings.insert(x, Binding::new(mutable, addr));
		}
	}

	pub fn begin_loop(&mut self, label: T::Label, args: Vec<Var<T>>, expr: &'e Expr<T>) {
		self.stack.push(LoopFrame::new(label, args, expr))
	}

	pub fn continue_loop(&mut self, label: T::Label, args: &[Var<T>]) -> Result<&'e Expr<T>, Error> {
		loop {
			match self.stack.last() {
				Some(frame) => {
					if frame.label == label {
						break
					} else {
						self.stack.pop();
					}
				},
				None => return self.err(E::UnreachableLabel)
			}
		}

		let frame = self.stack.last_mut().unwrap();
		if frame.args == *args {
			frame.clear();
			Ok(frame.expr)
		} else {
			self.err(E::RecursionArgsMissmatch)
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
	// 			self.stack.push(LoopFrame::new(*label, args.clone(), body));
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
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reference {
	pub mutable: bool,
	pub addr: usize
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Binding {
	Moved,
	Bound(Reference)
}

impl Binding {
	pub fn new(mutable: bool, addr: usize) -> Self {
		Self::Bound(Reference { mutable, addr })
	}

	pub fn bound(&self) -> Result<Reference, Error> {
		match self {
			Self::Moved => Err(Error::new(E::ValueMoved)),
			Self::Bound(r) => Ok(*r)
		}
	}

	pub fn take(&mut self) -> Result<Reference, Error> {
		match *self {
			Self::Moved => Err(Error::new(E::ValueAlreadyMoved)),
			Self::Bound(r) => {
				*self = Self::Moved;
				Ok(r)
			}
		}
	}
}

// pub struct Variable<'v> {
// 	mutable: bool,
// 	value: value::MaybeMoved<'v>
// }

// impl<'v> Variable<'v> {
// 	pub fn new(value: Value<'v>, mutable: bool) -> Self {
// 		Self {
// 			mutable,
// 			value: value::MaybeMoved::new(value)
// 		}
// 	}

// 	pub fn update(&mut self, new_value: Value<'v>) -> Result<(), Error> {
// 		if self.mutable {
// 			self.value = value::MaybeMoved::new(new_value);
// 			Ok(())
// 		} else {
// 			Err(Error::new(E::NotMutable))
// 		}
// 	}

// 	pub fn value(&mut self) -> &value::MaybeMoved<'v> {
// 		&self.value
// 	}

// 	pub fn value_mut(&mut self) -> &mut value::MaybeMoved<'v> {
// 		&mut self.value
// 	}
// }

pub struct LoopFrame<'e, T: Namespace> {
	label: T::Label,

	args: Vec<Var<T>>,

	expr: &'e Expr<T>,

	/// Variables local to the frame.
	bindings: HashMap<T::Var, Binding>,
}

impl<'e, T: Namespace> LoopFrame<'e, T> {
	pub fn new(label: T::Label, args: Vec<Var<T>>, expr: &'e Expr<T>) -> Self {
		Self {
			label,
			args,
			expr,
			bindings: HashMap::new()
		}
	}

	pub fn clear(&mut self) {
		self.bindings.clear()
	}
}