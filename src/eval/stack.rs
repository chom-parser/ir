use super::{
	Value,
	Error,
	error::Desc as E
};

#[derive(Debug)]
pub struct Stack<'v> {
	inner: Vec<(Value<'v>, Value<'v>)>
}

impl<'v> Stack<'v> {
	pub fn new() -> Self {
		Self {
			inner: Vec::new()
		}
	}

	pub fn push(&mut self, value: Value<'v>, state: Value<'v>) {
		self.inner.push((value, state))
	}

	pub fn pop(&mut self) -> Result<(Value<'v>, Value<'v>), Error> {
		self.inner.pop().ok_or_else(|| Error::new(E::EmptyStack))
	}
}