use std::{
	hash::Hash,
	collections::HashMap
};

pub mod constant;
pub mod expr;
pub mod ident;
pub mod module;
pub mod pattern;
pub mod routine;
pub mod ty;

pub use constant::Constant;
pub use expr::Expr;
pub use ident::Ident;
pub use module::Module;
pub use pattern::Pattern;
pub use routine::Routine;
pub use ty::Type;

pub struct Context<T: Ids> {
	ids: T,
	// reverse_ids: ReverseContextIds<T>
}

impl<T: Ids> Context<T> {
	pub fn id(&self) -> &T {
		&self.ids
	}

	pub fn ty(&self, ty: ty::Ref) -> Option<&Type<T>> {
		panic!("TODO")
	}

	pub fn function_path(&self, f: u32) -> Option<module::Path<'_, T>> {
		panic!("TODO")
	}

	pub fn module_path(&self, m: u32) -> Option<module::Path<'_, T>> {
		panic!("TODO")
	}

	pub fn extern_module_path(&self) -> module::Path<'_, T> {
		panic!("TODO")
	}
}

// pub struct ContextIds<T: Ids> {
// 	vars: HashMap<T::Var, Ident>,
// 	modules: HashMap<T::Module, Ident>,
// 	types: HashMap<T::Type, Ident>,
// 	params: HashMap<T::Param, Ident>,
// 	variants: HashMap<T::Variant, Ident>,
// 	fields: HashMap<T::Field, Ident>,
// 	labels: HashMap<T::Label, Ident>
// }

// impl<T: Ids> ContextIds<T> {
// 	pub fn var(&self, id: T::Var) -> Option<&Ident> {
// 		self.vars.get(&id)
// 	}
// }

// pub struct ReverseContextIds<T: Ids> {
// 	vars: HashMap<Ident, T::Var>,
// 	modules: HashMap<Ident, T::Module>,
// 	types: HashMap<Ident, T::Type>,
// 	params: HashMap<Ident, T::Param>,
// 	variants: HashMap<Ident, T::Variant>,
// 	fields: HashMap<Ident, T::Field>,
// 	labels: HashMap<Ident, T::Label>
// }

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