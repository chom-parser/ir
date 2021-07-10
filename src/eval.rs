use std::{
	collections::HashMap,
	iter::Peekable
};
use source_span::{
	Position,
	Span,
	Loc
};
use crate::{
	Ids,
	Context,
	Constant,
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

	EmptyStack
}

pub enum MaybeMovedValue<'v> {
	Here(Value<'v>),
	Moved
}

impl<'v> MaybeMovedValue<'v> {
	pub fn new(value: Value<'v>) -> Self {
		MaybeMovedValue::Here(value)
	}

	pub fn unwrap(self) -> Result<Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::ValueMoved)
		}
	}

	pub fn borrow(&self) -> Result<&Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::ValueMoved)
		}
	}

	pub fn borrow_mut(&mut self) -> Result<&mut Value<'v>, Error> {
		match self {
			Self::Here(v) => Ok(v),
			Self::Moved => Err(Error::ValueMoved)
		}
	}

	pub fn copy_or_move(&mut self) -> Result<Value<'v>, Error> {
		match self {
			Self::Here(value) => {
				if value.is_copiable() {
					return Ok(value.copy()?)
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

pub struct Variable<'v> {
	mutable: bool,
	value: MaybeMovedValue<'v>
}

impl<'v> Variable<'v> {
	pub fn new(value: Value<'v>, mutable: bool) -> Self {
		Self {
			mutable,
			value: MaybeMovedValue::new(value)
		}
	}

	pub fn update(&mut self, new_value: Value<'v>) -> Result<(), Error> {
		if self.mutable {
			self.value = MaybeMovedValue::new(new_value);
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
			Self::Const(_) => Err(Error::ImmutableThis)
		}
	}
}

pub struct Environement<'a, 'v, 'e, T: Ids> {
	/// Context.
	context: &'a Context<T>,

	/// The current variables values.
	vars: HashMap<T::Var, Variable<'v>>,

	/// The current `Var::This`.
	this: Option<Borrowed<'a, 'v>>,

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
							Some(Borrowed::Const(self.vars.get(&x).ok_or(Error::UnboundVariable)?.value.borrow()?))
						},
						Some((x, true)) => {
							Some(Borrowed::Mut(self.vars.get_mut(&x).ok_or(Error::UnboundVariable)?.value.borrow_mut()?))
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
					LexerExpr::Clear(next) => {
						let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
						lexer.clear();
						self.eval(next)
					}
					LexerExpr::Consume(next) => {
						let lexer = self.borrow_mut(*lexer)?.as_lexer_mut()?;
						lexer.consume();
						self.eval(next)
					}
				}
			}
			Expr::Stream(stream, e) => {
				match e {
					StreamExpr::Pull(x, next) => {
						let stream = self.borrow_mut(*stream)?.as_stream_mut()?;
						let value = stream.pull();
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
						ErrorValue::UnexpectedToken(Box::new(self.eval(&e)?))
					}
					expr::Error::UnexpectedNode(e) => {
						ErrorValue::UnexpectedNode(Box::new(self.eval(&e)?))
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
						InstanceData::EnumVariant(vb, args) => {
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

/// Value.
pub enum Value<'v> {
	Constant(Constant),
	Instance(ty::Ref, InstanceData<'v>),
	Position(Position),
	Span(Span),
	Loc(Loc<Box<Value<'v>>>),
	Lexer(Lexer<'v>),
	Stream(Stream<'v>),
	Stack(Stack<'v>),
	Error(ErrorValue<'v>),
	Output(Box<dyn 'v + std::io::Write>)
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
		Self::Instance(ty::Ref::Native(ty::Native::Option), InstanceData::EnumVariant(1, vec![MaybeMovedValue::new(value)]))
	}

	pub fn ok(value: Self) -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(0, vec![MaybeMovedValue::new(value)]))
	}

	pub fn err(value: Self) -> Self {
		Self::Instance(ty::Ref::Native(ty::Native::Result), InstanceData::EnumVariant(1, vec![MaybeMovedValue::new(value)]))
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
			Self::Lexer(_) => false,
			Self::Stream(_) => false,
			Self::Stack(_) => false,
			Self::Error(_) => false,
			Self::Output(_) => false
		}
	}

	pub fn copy(&self) -> Result<Self, Error> {
		match self {
			Self::Constant(c) => Ok(Self::Constant(c.clone())),
			Self::Instance(ty_ref, data) => Ok(Self::Instance(*ty_ref, data.copy()?)),
			Self::Position(p) => Ok(Self::Position(*p)),
			Self::Span(s) => Ok(Self::Span(*s)),
			Self::Loc(l) => Ok(Self::Loc(Loc::new(Box::new(l.as_ref().copy()?), l.span()))),
			Self::Lexer(_) => Err(Error::CannotMoveOut),
			Self::Stream(_) => Err(Error::CannotMoveOut),
			Self::Stack(_) => Err(Error::CannotMoveOut),
			Self::Error(_) => Err(Error::CannotMoveOut),
			Self::Output(_) => Err(Error::CannotMoveOut)
		}
	}

	pub fn as_lexer(&self) -> Result<&Lexer<'v>, Error> {
		match self {
			Self::Lexer(l) => Ok(l),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn as_lexer_mut(&mut self) -> Result<&mut Lexer<'v>, Error> {
		match self {
			Self::Lexer(l) => Ok(l),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn as_stream_mut(&mut self) -> Result<&mut Stream<'v>, Error> {
		match self {
			Self::Stream(s) => Ok(s),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn as_stack_mut(&mut self) -> Result<&mut Stack<'v>, Error> {
		match self {
			Self::Stack(s) => Ok(s),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn as_output_mut(&mut self) -> Result<&mut (dyn 'v + std::io::Write), Error> {
		match self {
			Self::Output(o) => Ok(o),
			_ => Err(Error::IncompatibleType)
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
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn into_position(self) -> Result<Position, Error> {
		match self {
			Self::Position(p) => Ok(p),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn into_span(self) -> Result<Span, Error> {
		match self {
			Self::Span(s) => Ok(s),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn into_loc(self) -> Result<Loc<Self>, Error> {
		match self {
			Self::Loc(v) => Ok(v.map(|v| *v)),
			_ => Err(Error::IncompatibleType)
		}
	}

	pub fn matches<T: Ids>(&self, pattern: &Pattern<T>) -> Result<bool, Error> {
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
									Err(Error::InvalidFieldCount(args.len() as u32, patterns.len() as u32))
								}
							} else {
								Ok(false)
							}
						},
						_ => Err(Error::NotAnEnumVariant)
					}
				} else {
					Err(Error::IncompatibleType)
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
	EnumVariant(u32, Vec<MaybeMovedValue<'v>>),
	Struct(Vec<MaybeMovedValue<'v>>)
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
					cargs.push(MaybeMovedValue::new(a.borrow()?.copy()?))
				}
				Ok(Self::EnumVariant(*v, cargs))
			},
			Self::Struct(bindings) => {
				let mut cbindings = Vec::with_capacity(bindings.len());
				for b in bindings {
					cbindings.push(MaybeMovedValue::new(b.borrow()?.copy()?))
				}
				Ok(Self::Struct(cbindings))
			}
		}
	}
}

pub struct Lexer<'v> {
	source: Peekable<Box<dyn 'v + Iterator<Item=Result<char, std::io::Error>>>>,
	buffer: String,
	span: Span
}

impl<'v> Lexer<'v> {
	pub fn peek(&mut self) -> Value<'v> {
		match self.source.peek() {
			Some(Ok(c)) => Value::ok(Value::some(Value::Constant(Constant::Char(*c)))),
			None => Value::ok(Value::none()),
			Some(Err(_)) => Value::err(Value::Error(ErrorValue::IO(self.source.next().unwrap().expect_err("expected io error"))))
		}
	}

	pub fn span(&self) -> Value<'v> {
		Value::Span(self.span)
	}

	pub fn chars(&self) -> Value<'v> {
		// Note: We need to copy the buffer here since
		// we cannot know statically if the caller will
		// respect the lifetime of the buffer using the returned stream.
		Value::Stream(Stream::Chars(self.buffer.chars().collect::<Vec<_>>().into_iter()))
	}

	pub fn clear(&mut self) {
		self.buffer.clear();
		self.span.clear();
	}

	pub fn consume(&mut self) -> Value<'v> {
		match self.source.peek() {
			Some(Ok(_)) | None => panic!("TODO"),
			Some(Err(_)) => Value::err(Value::Error(ErrorValue::IO(self.source.next().unwrap().expect_err("expected io error"))))
		}
	}
}

pub enum Stream<'v> {
	/// Lexer.
	Lexer(Lexer<'v>),

	/// Buffer characters.
	Chars(std::vec::IntoIter<char>)
}

impl<'v> Stream<'v> {
	pub fn pull(&mut self) -> Value<'v> {
		panic!("TODO")
	}
}

pub struct Stack<'v> {
	inner: Vec<(Value<'v>, Value<'v>)>
}

impl<'v> Stack<'v> {
	pub fn push(&mut self, value: Value<'v>, state: Value<'v>) {
		self.inner.push((value, state))
	}

	pub fn pop(&mut self) -> Result<(Value<'v>, Value<'v>), Error> {
		self.inner.pop().ok_or(Error::EmptyStack)
	}
}

pub enum ErrorValue<'v> {
	IO(std::io::Error),
	UnexpectedToken(Box<Value<'v>>),
	UnexpectedNode(Box<Value<'v>>)
}