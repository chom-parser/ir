use std::fmt;
use crate::{
	Namespace,
	Function,
	function
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Function<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self.owner() {
			function::Owner::Type(ty_ref) => {
				ppf.write("method ")?;
				ty_ref.fmt(ppf)?;
				ppf.write(".")?;
			}
			function::Owner::Module(_) => {
				ppf.write("function ")?;
			}
		}
		
		self.signature().fmt(ppf)?;

		match self.body() {
			Some(body) => {
				ppf.write(" {")?;
				ppf.begin()?;
				body.fmt(ppf)?;
				ppf.end()?;
				ppf.write("}")?
			},
			None => ppf.write(" unimplemented")?
		}

		ppf.sep()
	}
}

impl<T: Namespace> PrettyPrint<T> for function::Signature<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::UndefinedChar(x, error_ty) => {
				ppf.write("undefined-char(")?;
				ppf.write_var_id(*x)?;
				ppf.write(") -> ")?;
				error_ty.fmt(ppf)
			}
			Self::ExternParser(x, id) => {
				ppf.write("parser-extern-")?;
				ppf.write(id.as_str())?;
				ppf.write("(")?;
				ppf.write_var_id(*x)?;
				ppf.write(") -> ")?;
				ppf.write(id.as_str())
			}
			Self::Lexer(token_ty, error_ty) => {
				ppf.write("next-token() -> result(")?;
				token_ty.fmt(ppf)?;
				ppf.write(", ")?;
				error_ty.fmt(ppf)?;
				ppf.write(")")
			}
			Self::Parser(x, token_ty, error_ty) => {
				ppf.write("parse(")?;
				ppf.write_var_id(*x)?;
				ppf.write(": stream(")?;
				token_ty.fmt(ppf)?;
				ppf.write(", ")?;
				error_ty.fmt(ppf)?;
				ppf.write(") -> result(self, ")?;
				error_ty.fmt(ppf)?;
				ppf.write(")")
			}
		}
	}
}

pub fn function_id<T: Namespace>(ppf: &mut PrettyPrinter<T>, sig: &function::Signature<T>) -> fmt::Result {
	match sig {
		function::Signature::UndefinedChar(_, _) => {
			ppf.write("undefined-char")
		}
		function::Signature::ExternParser(_, id) => {
			ppf.write("parser-extern-")?;
			ppf.write(id.as_str())
		}
		function::Signature::Lexer(_, _) => {
			ppf.write("next-token")
		}
		function::Signature::Parser(_, _, _) => {
			ppf.write("parse")
		}
	}
}