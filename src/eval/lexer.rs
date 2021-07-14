use std::{
	iter::Peekable,
	fmt
};
use source_span::Span;
use crate::Constant;
use super::{
	Value,
	error
};

pub struct Lexer<'v> {
	lexer_method: u32,
	source: Peekable<&'v mut dyn Iterator<Item=Result<char, std::io::Error>>>,
	buffer: String,
	span: Span,
	metrics: source_span::DefaultMetrics
}

impl<'v> Lexer<'v> {
	pub fn new(lexer_method: u32, source: &'v mut dyn Iterator<Item=Result<char, std::io::Error>>) -> Self {
		Self {
			lexer_method,
			source: source.peekable(),
			buffer: String::new(),
			span: Span::default(),
			metrics: source_span::DEFAULT_METRICS
		}
	}

	pub fn method(&self) -> u32 {
		self.lexer_method
	}

	pub fn peek(&mut self) -> Value<'v> {
		match self.source.peek() {
			Some(Ok(c)) => Value::ok(Value::some(Value::Constant(Constant::Char(*c)))),
			None => Value::ok(Value::none()),
			Some(Err(_)) => Value::err(Value::Error(error::Value::IO(self.source.next().unwrap().expect_err("expected io error"))))
		}
	}

	pub fn span(&self) -> Value<'v> {
		Value::Span(self.span)
	}

	pub fn chars(&self) -> Value<'v> {
		// Note: We need to copy the buffer here since
		// we cannot know statically if the caller will
		// respect the lifetime of the buffer using the returned stream.
		Value::Chars(self.buffer.chars().collect::<Vec<_>>().into_iter())
	}

	pub fn buffer(&self) -> Value<'v> {
		Value::Constant(Constant::String(self.buffer.clone()))
	}

	pub fn clear(&mut self) {
		self.buffer.clear();
		self.span.clear();
	}

	pub fn consume(&mut self) -> Result<(), Value<'v>> {
		match self.source.next() {
			None => Ok(()),
			Some(Ok(c)) => {
				self.buffer.push(c);
				self.span.push(c, &self.metrics);
				Ok(())
			},
			Some(Err(e)) => Err(Value::Error(error::Value::IO(e)))
		}
	}
}

impl<'v> fmt::Debug for Lexer<'v> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Lexer(buffer={}, span={})", self.buffer, self.span)
	}
}