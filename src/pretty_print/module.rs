use std::fmt;
use crate::{
	Namespace,
	Module
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Module<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		for ty_ref in self.types() {
			let ty = ppf.context().ty(ty_ref).unwrap();
			ty.fmt(ppf)?;
		}

		for f_index in self.functions() {
			let f = ppf.context().function(f_index).unwrap();
			f.fmt(ppf)?;
		}

		for m_index in self.modules() {
			let m = ppf.context().module(m_index).unwrap();
			if m.is_extern() {
				ppf.write("extern ")?;
			}
			ppf.write("module ")?;
			ppf.write_module_id(*m.id().as_nammed().unwrap())?;
			ppf.write(" {")?;
			ppf.begin()?;
			m.fmt(ppf)?;
			ppf.end()?;
			ppf.write("}")?;
			ppf.sep()?;
		}

		Ok(())
	}
}