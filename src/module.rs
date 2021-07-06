use super::{ty, Ids, Routine};
use std::collections::HashSet;

/// Module.
pub struct Module<T: Ids> {
	/// Index of the module in `Context::modules`.
	index: u32,

	/// Parent module.
	parent: Option<u32>,

	/// Module id.
	id: Id<T>,

	/// Module roles.
	roles: HashSet<Role>,

	/// Types defined in the module.
	types: Vec<ty::Ref>,

	/// Sub-modules.
	modules: Vec<u32>,

	/// Defined routines.
	routines: Vec<Routine>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Role {
	Extern,
	Ast,
	Lexer,
	ParserRoot,
	ParserSubmodule,
}

impl<T: Ids> Module<T> {
	pub fn root() -> Self {
		Self {
			index: 0,
			parent: None,
			id: Id::Root,
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			routines: Vec::new(),
		}
	}

	pub fn new(index: u32, parent: u32, name: T::Module) -> Self {
		Self {
			index,
			parent: Some(parent),
			id: Id::Named(name),
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			routines: Vec::new(),
		}
	}

	pub fn index(&self) -> u32 {
		self.index
	}

	pub fn parent(&self) -> Option<u32> {
		self.parent
	}

	/// Module id.
	pub fn id(&self) -> &Id<T> {
		&self.id
	}

	pub fn roles(&self) -> impl '_ + Iterator<Item = Role> {
		self.roles.iter().cloned()
	}

	pub fn is_extern(&self) -> bool {
		self.roles.contains(&Role::Extern)
	}

	pub fn types(&self) -> impl '_ + Iterator<Item = ty::Ref> {
		self.types.iter().cloned()
	}

	pub fn modules(&self) -> impl '_ + Iterator<Item = u32> {
		self.modules.iter().cloned()
	}

	pub fn routines(&self) -> impl '_ + Iterator<Item = &'_ Routine> {
		self.routines.iter()
	}

	pub fn add_role(&mut self, role: Role) {
		self.roles.insert(role);
	}

	pub fn add_type(&mut self, ty: ty::Ref) {
		self.types.push(ty)
	}

	pub fn add_module(&mut self, m: u32) {
		self.modules.push(m)
	}

	pub fn add_routine(&mut self, routine: Routine) {
		self.routines.push(routine)
	}
}

pub enum Id<T: Ids> {
	/// Root module.
	///
	/// In Rust this is translated into `crate`.
	Root,

	/// Named module.
	Named(T::Module),
}

impl<T: Ids> Id<T> {
	pub fn as_nammed(&self) -> Option<&T::Module> {
		match self {
			Self::Named(id) => Some(id),
			_ => None,
		}
	}
}

pub struct Path<'a, T: Ids> {
	parent: Option<Box<Path<'a, T>>>,
	id: Option<&'a Id<T>>,
}

// impl<'a, T: Ids> Path<'a, T> {
// 	pub(crate) fn new(parent: Option<Path<'a, T>>, id: &'a Id<T>) -> Self {
// 		Self {
// 			parent: parent.map(Box::new),
// 			id: Some(id),
// 		}
// 	}
// }

impl<'a, T: Ids> Iterator for Path<'a, T> {
	type Item = &'a Id<T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.parent.as_mut().map(|p| p.next()).flatten() {
			Some(id) => Some(id),
			None => self.id.take(),
		}
	}
}
