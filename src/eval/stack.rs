use super::{
	Value,
	Error
};

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