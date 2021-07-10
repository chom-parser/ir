use std::fmt;
use crate::{
	Namespace,
	Context,
	Expr,
	expr::Var
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Var<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::This => ppf.write("this"),
			Self::Defined(x) => {
				let id = ppf.context().id().var_ident(*x);
				ppf.write(id.as_str())
			}
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for Expr<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Literal(c) => c.fmt(ppf),
			Self::Get(x) => x.fmt(ppf),
			Self::GetField(x, ty_ref, index) => {
				panic!("TODO")
			},
			_ => panic!("TODO")
		}
	}
}