use std::fmt;
use crate::{
	Namespace,
	Constant
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Constant {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Unit => ppf.write("()"),
			Self::Int(i) => ppf.write_u32(*i),
			Self::Char(c) => ppf.write_char(*c),
			Self::CharRange(a, b) => {
				ppf.write_char(*a)?;
				ppf.write("..")?;
				ppf.write_char(*b)
			},
			Self::String(s) => ppf.write_str(s.as_str())
		}
	}
}