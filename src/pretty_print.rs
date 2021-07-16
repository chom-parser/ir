use crate::{
	Namespace,
	Context
};

use std::fmt;

mod constant;
mod ty;
mod expr;
mod pattern;
mod module;
mod function;

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

	pub fn write_bool(&mut self, b: bool) -> fmt::Result {
		use fmt::Display;
		b.fmt(self.f)
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

	pub fn write_var_id(&mut self, x: T::Var) -> fmt::Result {
		self.write(self.context.id().var_ident(x).as_str())
	}

	pub fn write_module_id(&mut self, m: T::Module) -> fmt::Result {
		self.write(self.context.id().module_ident(m).as_str())
	}

	pub fn write_function_id(&mut self, m: T::Function) -> fmt::Result {
		self.write(self.context.id().function_ident(m).as_str())
	}

	pub fn write_ty_param(&mut self, p: T::Param) -> fmt::Result {
		self.write(self.context.id().param_ident(p).as_str())
	}

	pub fn write_field(&mut self, f: T::Field) -> fmt::Result {
		self.write(self.context.id().field_ident(f).as_str())
	}

	pub fn begin(&mut self) -> fmt::Result {
		self.depth += 1;
		self.sep()
	}

	pub fn sep(&mut self) -> fmt::Result {
		write!(self.f, "\n")?;
		for _ in 0..self.depth {
			write!(self.f, "{}", self.tab)?
		}

		Ok(())
	}

	pub fn end(&mut self) -> fmt::Result {
		self.depth -= 1;
		self.sep()
	}
}