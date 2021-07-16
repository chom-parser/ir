use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Constant {
	Unit,
	Bool(bool),
	Int(u32),
	Char(char),
	CharRange(char, char),
	String(String)
}

impl Constant {
	pub fn matches(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::CharRange(a, b), Self::Char(c)) => c >= a && c <= b,
			_ => self == other
		}
	}
}

impl fmt::Display for Constant {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Unit => write!(f, "()"),
			Self::Bool(b) => write!(f, "{}", b),
			Self::Int(i) => write!(f, "{}", i),
			Self::Char(c) => write!(f, "'{}'", c),
			Self::CharRange(a, b) => write!(f, "'{}'..'{}'", a, b),
			Self::String(s) => write!(f, "\"{}\"", s)
		}
	}
}