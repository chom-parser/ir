use super::{ty, Ids};
use std::collections::HashSet;

/// Module.
pub struct Module<T: Ids> {
	/// Index of the module in `Context::modules`.
	index: Option<u32>,

	/// Parent module.
	parent: Option<u32>,

	/// Module's id.
	id: Id<T>,

	/// Module's roles.
	roles: HashSet<Role>,

	/// Types defined in the module.
	types: Vec<ty::Ref>,

	/// Sub-modules.
	modules: Vec<u32>,

	/// Module's functions.
	functions: Vec<u32>,
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
			index: None,
			parent: None,
			id: Id::Root,
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			functions: Vec::new(),
		}
	}

	pub fn new(parent: u32, name: T::Module) -> Self {
		Self {
			index: None,
			parent: Some(parent),
			id: Id::Named(name),
			roles: HashSet::new(),
			types: Vec::new(),
			modules: Vec::new(),
			functions: Vec::new(),
		}
	}

	pub fn index(&self) -> Option<u32> {
		self.index
	}

	pub(crate) fn set_index(&mut self, i: u32) {
		self.index = Some(i)
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

	pub fn functions(&self) -> impl '_ + Iterator<Item = u32> {
		self.functions.iter().cloned()
	}

	pub fn add_role(&mut self, role: Role) {
		self.roles.insert(role);
	}

	pub(crate) fn add_type(&mut self, ty: ty::Ref) {
		self.types.push(ty)
	}

	pub(crate) fn add_module(&mut self, m: u32) {
		self.modules.push(m)
	}

	pub(crate) fn add_function(&mut self, f: u32) {
		self.functions.push(f)
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