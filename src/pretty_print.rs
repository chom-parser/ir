use crate::{
	Namespace,
	Context
};

use std::fmt;

mod constant;
mod expr;

pub trait PrettyPrint<T: Namespace> {
	fn fmt(&self, f: &mut PrettyPrinter<T>) -> fmt::Result;

	fn pretty_print<'a>(&'a self, context: &'a Context<T>, tab: &'a str) -> PrettyPrinted<'a, T, Self> {
		PrettyPrinted {
			context,
			tab,
			value: self
		}
	}
}

pub struct PrettyPrinted<'a, C: Namespace, T: PrettyPrint<C> + ?Sized> {
	context: &'a Context<C>,
	tab: &'a str,
	value: &'a T
}

impl<'a, C: Namespace, T: PrettyPrint<C> + ?Sized> fmt::Display for PrettyPrinted<'a, C, T> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut ppf = PrettyPrinter::new(f, self.context, self.tab);
		self.value.fmt(&mut ppf)
	}
}

pub struct PrettyPrinter<'a, 'f, T: Namespace> {
	context: &'a Context<T>,
	f: &'a mut fmt::Formatter<'f>,
	tab: &'a str,
	depth: u32
}

impl<'a, 'f, T: Namespace> PrettyPrinter<'a, 'f, T> {
	pub fn new(f: &'a mut fmt::Formatter<'f>, context: &'a Context<T>, tab: &'a str) -> Self {
		Self {
			context,
			f,
			tab,
			depth: 0
		}
	}

	pub fn context(&self) -> &'a Context<T> {
		self.context
	}

	pub fn write(&mut self, s: &str) -> fmt::Result {
		use fmt::Display;
		s.fmt(self.f)
	}

	pub fn write_u32(&mut self, i: u32) -> fmt::Result {
		use fmt::Display;
		i.fmt(self.f)
	}

	pub fn write_char(&mut self, c: char) -> fmt::Result {
		write!(self.f, "'{}'", c)
	}

	pub fn write_str(&mut self, s: &str) -> fmt::Result {
		write!(self.f, "\"{}\"", s)
	}

	pub fn begin(&mut self) -> fmt::Result {
		self.depth += 1;
		write!(self.f, "\n")?;

		for _ in 0..self.depth {
			write!(self.f, "{}", self.tab)?
		}

		Ok(())
	}

	pub fn end(&mut self) -> fmt::Result {
		self.depth -= 1;
		write!(self.f, "\n")
	}
}