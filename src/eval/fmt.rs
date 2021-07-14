use std::fmt;
use crate::{
	Context,
	Namespace
};

pub trait ContextDisplay<N: Namespace> {
	fn fmt(&self, context: &Context<N>, f: &mut fmt::Formatter) -> fmt::Result;

	fn display_in<'c>(&self, context: &'c Context<N>) -> DisplayIn<'_, 'c, Self, N> {
		DisplayIn(self, context)
	}
}

pub struct DisplayIn<'a, 'c, T: ?Sized, N: Namespace>(&'a T, &'c Context<N>);

impl<'a, 'c, T: ?Sized, N: Namespace> fmt::Display for DisplayIn<'a, 'c, T, N> where T: ContextDisplay<N> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.0.fmt(self.1, f)
	}
}