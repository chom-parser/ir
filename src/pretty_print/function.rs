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
				if self.signature().mutates() {
					ppf.write("method-mut ")?;
				} else {
					ppf.write("method ")?;
				}
				
				if let Some(marker) = self.signature().marker() {
					marker.fmt(ppf)?;
					ppf.write(" ")?;
				}

				ty_ref.fmt(ppf)?;
				ppf.write(".")?;
			}
			function::Owner::Module(_) => {
				ppf.write("function ")?;
				
				if let Some(marker) = self.signature().marker() {
					marker.fmt(ppf)?;
					ppf.write(" ")?;
				}
			}
		}

		ppf.write_function_id(self.id())?;
		
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
		ppf.write("(")?;
		for (i, a) in self.arguments().iter().enumerate() {
			if i > 0 {
				ppf.write(", ")?;
			}
			a.fmt(ppf)?;
		}
		ppf.write(") -> ")?;
		for (i, ty) in self.return_types().iter().enumerate() {
			if i > 0 {
				ppf.write(", ")?;
			}
			ty.fmt(ppf)?;
		}
		Ok(())
	}
}

impl<T: Namespace> PrettyPrint<T> for function::Arg<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		if self.is_mutable() {
			ppf.write("mut ")?;
		}
		ppf.write_var_id(self.id())?;
		ppf.write(": ")?;
		self.ty().fmt(ppf)
	}
}

impl<T: Namespace> PrettyPrint<T> for function::Marker {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::UndefinedChar => ppf.write("[undefined-char]"),
			Self::ExternParser => ppf.write("[extern-parser]"),
			Self::Lexer => ppf.write("[lexer]"),
			Self::Parser => ppf.write("[parser]"),
			Self::DebugFormat => ppf.write("[debug-format]")
		}
	}
}