use super::{ty, Namespace};

/// Module.
pub struct Module<T: Namespace + ?Sized> {
	/// Index of the module in `Context::modules`.
	index: Option<u32>,

	/// Parent module.
	parent: Option<u32>,

	/// Module's id.
	id: Id<T>,

	/// Is the module extern?
	/// 
	/// If `true`, code generators may choose not to render it.
	is_extern: bool,

	/// Types defined in the module.
	types: Vec<ty::Ref>,

	/// Sub-modules.
	modules: Vec<u32>,

	/// Module's functions.
	functions: Vec<u32>,
}

impl<T: Namespace + ?Sized> Module<T> {
	pub fn root() -> Self {
		Self {
			index: None,
			parent: None,
			id: Id::Root,
			is_extern: false,
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
			is_extern: false,
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

	pub fn is_extern(&self) -> bool {
		self.is_extern
	}

	pub fn set_extern(&mut self, e: bool) {
		self.is_extern = e
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

pub enum Id<T: Namespace + ?Sized> {
	/// Root module.
	///
	/// In Rust this is translated into `crate`.
	Root,

	/// Named module.
	Named(T::Module),
}

impl<T: Namespace + ?Sized> Id<T> {
	pub fn as_nammed(&self) -> Option<&T::Module> {
		match self {
			Self::Named(id) => Some(id),
			_ => None,
		}
	}
}