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