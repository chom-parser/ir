use std::fmt;
use crate::{
	Namespace,
	Pattern,
	ty
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Pattern<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Any => ppf.write("_"),
			Self::Bind(x) => ppf.write(ppf.context().id().var_ident(*x).as_str()),
			Self::Cons(ty_ref, index, args) => {
				let ty = ppf.context().ty(*ty_ref).unwrap();
				ty_ref.fmt(ppf)?;
				ppf.write(".")?;
				match ty.desc() {
					ty::Desc::Enum(enm) => {
						let v = enm.variant(*index).expect("undefined variant");
						super::ty::variant_id(ppf, v)?;
					}
					_ => panic!("cannot pretty-print variant construction on non-enum type")
				}
				if !args.is_empty() {
					ppf.write("(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							ppf.write(", ")?;
						}
						a.fmt(ppf)?;
					}
					ppf.write(")")?;
				}
				Ok(())
			}
			Self::Literal(c) => c.fmt(ppf),
			Self::Or(patterns) => {
				for (i, p) in patterns.iter().enumerate() {
					if i > 0 {
						ppf.write(" | ")?;
					}

					p.fmt(ppf)?;
				}
				Ok(())
			}
		}
	}
}