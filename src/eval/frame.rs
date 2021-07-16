use std::collections::HashMap;
use crate::{
	Namespace,
	Expr,
	expr::Var
};
use super::{
	Error,
	error::Desc as E,
};

pub struct Frame<'e, T: Namespace> {
	/// The current variables values.
	bindings: HashMap<T::Var, Binding>,

	/// The current `Var::This`.
	this: Option<Reference>,

	/// Loop stack.
	stack: Vec<LoopFrame<'e, T>>
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

	pub fn this(&self) -> Option<&Reference> {
		self.this.as_ref()
	}

	pub fn set_this(&mut self, this: Reference) {
		self.this = Some(this)
	}

	pub fn get(&self, ns: &T, x: Var<T>) -> Result<&Reference, Error> {
		match x {
			Var::This => self.this.as_ref().ok_or_else(|| self.error(E::NoThis)),
			Var::Defined(x) => {
				match self.stack.last() {
					Some(frame) => {
						if let Some(b) = frame.bindings.get(&x) {
							return b.bound()
						}
					}
					None => {
						if let Some(b) = self.bindings.get(&x) {
							return b.bound()
						}
					}
				}
		
				self.err(E::UnboundVariable(ns.var_ident(x)))
			}
		}
	}

	pub fn take(&mut self, ns: &T, x: T::Var) -> Result<usize, Error> {
		match self.stack.last_mut() {
			Some(frame) => {
				if let Some(b) = frame.bindings.get_mut(&x) {
					return Ok(b.take()?.addr)
				}
			}
			None => {
				if let Some(b) = self.bindings.get_mut(&x) {
					return Ok(b.take()?.addr)
				}
			}
		}

		self.err(E::UnboundVariable(ns.var_ident(x)))
	}

	pub fn borrow(&self, ns: &T, x: Var<T>) -> Result<usize, Error> {
		Ok(self.get(ns, x)?.addr)
	}

	pub fn borrow_mut(&self, ns: &T, x: Var<T>) -> Result<usize, Error> {
		let r = self.get(ns, x)?;
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

	pub fn begin_loop(&mut self, ns: &T, label: T::Label, args: Vec<(Var<T>, bool)>, expr: &'e Expr<T>) -> Result<(), Error> {
		let mut frame = LoopFrame::new(label, args.iter().map(|(x, _)| *x).collect(), expr);

		for (a, mutable) in args {
			if let Var::Defined(x) = a {
				let addr = self.take(ns, x)?;
				frame.bindings.insert(x, Binding::new(
					mutable,
					addr
				));
			}
		}

		self.stack.push(frame);
		Ok(())
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
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Reference {
	pub mutable: bool,
	pub addr: usize,
	pub path: Vec<Segment>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Segment {
	LocInner
}

impl Reference {
	pub fn loc_inner(&mut self) {
		self.path.push(Segment::LocInner)
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Binding {
	Moved(Reference),
	Bound(Reference)
}

impl Binding {
	pub fn new(mutable: bool, addr: usize) -> Self {
		Self::Bound(Reference { mutable, addr, path: Vec::new() })
	}

	pub fn bound(&self) -> Result<&Reference, Error> {
		match self {
			Self::Moved(_) => Err(Error::new(E::ValueMoved)),
			Self::Bound(r) => Ok(r)
		}
	}

	pub fn reference(&self) -> &Reference {
		match self {
			Self::Moved(r) => r,
			Self::Bound(r) => r
		}
	}

	pub fn take(&mut self) -> Result<Reference, Error> {
		let r = match self {
			Self::Moved(_) => return Err(Error::new(E::ValueAlreadyMoved)),
			Self::Bound(r) => r.clone()
		};

		let mut moved = Self::Moved(r.clone());
		std::mem::swap(self, &mut moved);

		Ok(r)
	}
}

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
		let args = &self.args;
		self.bindings.retain(|x, _| args.iter().any(|y| y.eq_defined(*x)))
	}
}