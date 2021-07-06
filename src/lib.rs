use std::{
	hash::Hash
};

pub mod constant;
pub mod expr;
pub mod ident;
pub mod path;
pub mod module;
pub mod pattern;
pub mod function;
pub mod ty;

pub use constant::Constant;
pub use expr::Expr;
pub use ident::Ident;
pub use path::Path;
pub use module::Module;
pub use pattern::Pattern;
pub use function::Function;
pub use ty::Type;

pub struct Context<T: Ids> {
	ids: T,

	functions: Vec<Function<T>>,
	modules: Vec<Module<T>>,
	types: Vec<Type<T>>,

	native_types: NativeTypes<T>,

	extern_module: u32
}

pub struct NativeTypes<T: Ids> {
	unit: Type<T>,
	option: Type<T>,
	list: Type<T>,
	stack: Type<T>,
	heap: Type<T>,
	lexer: Type<T>,
	loc: Type<T>
}

impl<T: Ids> NativeTypes<T> {
	pub fn new() -> Self {
		use ty::Native;
		Self {
			unit: Type::native(Native::Unit),
			option: Type::native(Native::Option),
			list: Type::native(Native::List),
			stack: Type::native(Native::Stack),
			heap: Type::native(Native::Heap),
			lexer: Type::native(Native::Lexer),
			loc: Type::native(Native::Loc),
		}
	}

	pub fn get(&self, n: ty::Native) -> &Type<T> {
		use ty::Native;
		match n {
			Native::Unit => &self.unit,
			Native::Option => &self.option,
			Native::List => &self.list,
			Native::Stack => &self.stack,
			Native::Heap => &self.heap,
			Native::Lexer => &self.lexer,
			Native::Loc => &self.loc
		}
	}

	pub fn get_mut(&mut self, n: ty::Native) -> &mut Type<T> {
		use ty::Native;
		match n {
			Native::Unit => &mut self.unit,
			Native::Option => &mut self.option,
			Native::List => &mut self.list,
			Native::Stack => &mut self.stack,
			Native::Heap => &mut self.heap,
			Native::Lexer => &mut self.lexer,
			Native::Loc => &mut self.loc
		}
	}
}

impl<T: Ids> Context<T> {
	pub fn new(ids: T) -> Self {
		let mut context = Self {
			ids,
			functions: Vec::new(),
			modules: Vec::new(),
			types: Vec::new(),
			native_types: NativeTypes::new(),
			extern_module: 0
		};

		context.add_module(Module::root());
		context
	}

	pub fn id(&self) -> &T {
		&self.ids
	}

	pub fn module(&self, index: u32) -> Option<&Module<T>> {
		self.modules.get(index as usize)
	}

	pub fn ty(&self, ty: ty::Ref) -> Option<&Type<T>> {
		match ty {
			ty::Ref::Native(n) => Some(self.native_types.get(n)),
			ty::Ref::Defined(i) => self.types.get(i as usize)
		}
	}

	pub fn ty_mut(&mut self, ty: ty::Ref) -> Option<&mut Type<T>> {
		match ty {
			ty::Ref::Native(n) => Some(self.native_types.get_mut(n)),
			ty::Ref::Defined(i) => self.types.get_mut(i as usize)
		}
	}

	pub fn function(&self, index: u32) -> Option<&Function<T>> {
		self.functions.get(index as usize)
	}

	pub fn module_path(&self, index: u32) -> Option<Path<'_, T>> {
		self.module(index)
			.map(|m| Path::new(m.parent().map(|p| self.module_path(p).unwrap()), path::Segment::Module(m.id())))
	}

	pub fn type_path(&self, r: ty::Ref) -> Option<Path<'_, T>> {
		self.ty(r).map(|ty| {
			Path::new(ty.module().map(|m| self.module_path(m)).flatten(), path::Segment::Type(ty.id()))
		})
	}

	pub fn function_path(&self, index: u32) -> Option<Path<'_, T>> {
		self.function(index).map(|f| {
			let owner_path = match f.owner() {
				function::Owner::Module(m) => self.module_path(m),
				function::Owner::Type(r) => self.type_path(r)
			};

			Path::new(owner_path, path::Segment::Function(f.signature()))
		})
	}

	pub fn extern_module_path(&self) -> Path<'_, T> {
		self.module_path(self.extern_module).unwrap()
	}
	
	/// Add a new module.
	pub fn add_module(&mut self, mut m: Module<T>) -> u32 {
		let i = self.modules.len() as u32;
		m.set_index(i);
		if let Some(parent) = m.parent() {
			self.modules[parent as usize].add_module(i)
		}
		if m.is_extern() {
			self.extern_module = i
		}
		self.modules.push(m);
		i
	}

	pub fn add_function(&mut self, f: Function<T>) -> u32 {
		let i = self.functions.len() as u32;
		match f.owner() {
			function::Owner::Module(m) => self.modules[m as usize].add_function(i),
			function::Owner::Type(t) => self.ty_mut(t).unwrap().add_method(i)
		}
		self.functions.push(f);
		i
	}

	pub fn add_type(&mut self, t: Type<T>) -> u32 {
		let i = self.types.len() as u32;
		self.modules[t.module().unwrap() as usize].add_type(ty::Ref::Defined(i));
		self.types.push(t);
		i
	}
}

/// Types used as identifiers.
pub trait Ids {
	/// Const variable identifier type.
	type Var: Copy + PartialEq + Eq + Hash;

	/// Module identifier type.
	type Module: Copy;

	/// Type identifier type.
	type Type: Copy;

	/// Type parameter identifier type.
	type Param: Copy;

	/// Enum variant identifier type.
	type Variant: Copy;

	/// Struct field identifier type.
	type Field: Copy;

	/// Label identifier type.
	type Label: Copy + PartialEq + Eq + Hash;

	/// Returns the identifier associated to the given variable.
	fn var_ident(&self, v: Self::Var) -> Ident;

	fn module_ident(&self, m: Self::Module) -> Ident;

	fn type_ident(&self, t: Self::Type) -> Ident;

	fn param_ident(&self, p: Self::Param) -> Ident;

	fn variant_ident(&self, v: Self::Variant) -> Ident;

	fn field_ident(&self, f: Self::Field) -> Ident;

	fn label_ident(&self, l: Self::Label) -> Ident;
}