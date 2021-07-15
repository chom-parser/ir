use std::fmt;
use crate::{
	Namespace,
	Type,
	ty
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Type<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self.desc() {
			ty::Desc::Opaque => ppf.write("opaque ")?,
			ty::Desc::Lexer => ppf.write("lexer ")?,
			ty::Desc::Enum(_) => ppf.write("enum ")?,
			ty::Desc::Struct(_) => ppf.write("struct ")?,
			ty::Desc::TupleStruct(_) => ppf.write("tuple ")?
		}

		self.id().fmt(ppf)?;

		match self.desc() {
			ty::Desc::Opaque => (),
			ty::Desc::Lexer => (),
			ty::Desc::Enum(enm) => {
				ppf.write("{")?;
				ppf.begin()?;
				for (i, variant) in enm.variants().iter().enumerate() {
					if i > 0 {
						ppf.write(",")?;
						ppf.sep()?;
					}
					variant.fmt(ppf)?;
				}
				ppf.end()?;
				ppf.write("}")?;
			}
			ty::Desc::Struct(strct) => {
				ppf.write("{")?;
				ppf.begin()?;
				for (i, field) in strct.fields().iter().enumerate() {
					if i > 0 {
						ppf.write(",")?;
						ppf.sep()?;
					}
					field.fmt(ppf)?;
				}
				ppf.end()?;
				ppf.write("}")?;
			}
			ty::Desc::TupleStruct(args) => {
				ppf.write("(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(",")?;
					}
					a.fmt(ppf)?;
				}
				ppf.write(")")?;
			}
		}

		ppf.sep()?;

		for f_index in self.methods() {
			let f = ppf.context().function(*f_index).unwrap();
			f.fmt(ppf)?;
		}

		Ok(())
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Param<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Native(n) => n.fmt(ppf),
			Self::Defined(p) => ppf.write_ty_param(*p)
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::NativeParam {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Value => ppf.write("value"),
			Self::Error => ppf.write("error")
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Expr<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			ty::Expr::Var(p) => p.fmt(ppf),
			ty::Expr::Instance(ty_ref, args) => {
				ty_ref.fmt(ppf)?;
				if !args.is_empty() {
					ppf.write("(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							ppf.write(",")?;
						}
						a.fmt(ppf)?;
					}
					ppf.write(")")?;
				}
				Ok(())
			}
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Field<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		ppf.write_field(self.id)?;
		ppf.write(": ")?;
		self.ty.fmt(ppf)
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Id<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			ty::Id::Native(n) => n.fmt(ppf),
			ty::Id::Defined(i) => ppf.write(ppf.context().id().type_ident(*i).as_str())
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Ref {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		let ty = ppf.context().ty(*self).unwrap();
		ty.id().fmt(ppf)
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Native {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		ppf.write(self.ident().as_str())
	}
}

pub fn variant_id<T: Namespace>(ppf: &mut PrettyPrinter<T>, variant: &ty::Variant<T>) -> fmt::Result {
	match variant {
		ty::Variant::Native(n) => native_variant_id(ppf, n),
		ty::Variant::Defined(v, _) => {
			ppf.write(ppf.context().id().variant_ident(*v).as_str())
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for ty::Variant<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		variant_id(ppf, self)?;
		if let Some(args) = self.tuple_parameters() {
			ppf.write("(")?;
			for (i, a) in args.iter().enumerate() {
				if i > 0 {
					ppf.write(", ")?;
				}
				a.fmt(ppf)?;
			}
			ppf.write(")")
		} else if let Some(strct) = self.struct_parameter() {
			ppf.write("(")?;
			for (i, f) in strct.fields().iter().enumerate() {
				if i > 0 {
					ppf.write(", ")?;
				}
				f.ty.fmt(ppf)?;
			}
			ppf.write(")")
		} else {
			Ok(())
		}
	}
}

pub fn native_variant_id<T: Namespace>(ppf: &mut PrettyPrinter<T>, n: &ty::NativeVariant) -> fmt::Result {
	match n {
		ty::NativeVariant::None => ppf.write("none"),
		ty::NativeVariant::Some => ppf.write("some"),
		ty::NativeVariant::Ok => ppf.write("ok"),
		ty::NativeVariant::Err => ppf.write("err")
	}
}