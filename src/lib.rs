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

pub mod eval;
mod pretty_print;

pub use constant::Constant;
pub use expr::Expr;
pub use ident::Ident;
pub use path::Path;
pub use module::Module;
pub use pattern::Pattern;
pub use function::Function;
pub use ty::Type;
pub use pretty_print::*;

#[derive(Debug)]
pub struct InvalidTypeExpr;

pub struct Context<T: Namespace> {
	ids: T,

	functions: Vec<Function<T>>,
	modules: Vec<Module<T>>,
	types: Vec<Type<T>>,

	native_types: NativeTypes<T>,
}

pub struct NativeTypes<T: Namespace + ?Sized> {
	unit: Type<T>,
	chr: Type<T>,
	string: Type<T>,
	option: Type<T>,
	result: Type<T>,
	list: Type<T>,
	stack: Type<T>,
	heap: Type<T>,
	position: Type<T>,
	span: Type<T>,
	loc: Type<T>,
	stream: Type<T>
}

impl<T: Namespace + ?Sized> NativeTypes<T> {
	pub fn new() -> Self {
		use ty::Native;
		Self {
			unit: Type::native(Native::Unit),
			chr: Type::native(Native::Char),
			string: Type::native(Native::String),
			option: Type::native(Native::Option),
			result: Type::native(Native::Result),
			list: Type::native(Native::List),
			stack: Type::native(Native::Stack),
			heap: Type::native(Native::Heap),
			position: Type::native(Native::Position),
			span: Type::native(Native::Span),
			loc: Type::native(Native::Loc),
			stream: Type::native(Native::Stream)
		}
	}

	pub fn get(&self, n: ty::Native) -> &Type<T> {
		use ty::Native;
		match n {
			Native::Unit => &self.unit,
			Native::String => &self.string,
			Native::Char => &self.chr,
			Native::Option => &self.option,
			Native::Result => &self.result,
			Native::List => &self.list,
			Native::Stack => &self.stack,
			Native::Heap => &self.heap,
			Native::Position => &self.position,
			Native::Span => &self.span,
			Native::Loc => &self.loc,
			Native::Stream => &self.stream
		}
	}

	pub fn get_mut(&mut self, n: ty::Native) -> &mut Type<T> {
		use ty::Native;
		match n {
			Native::Unit => &mut self.unit,
			Native::Char => &mut self.chr,
			Native::String => &mut self.string,
			Native::Option => &mut self.option,
			Native::Result => &mut self.result,
			Native::List => &mut self.list,
			Native::Stack => &mut self.stack,
			Native::Heap => &mut self.heap,
			Native::Position => &mut self.position,
			Native::Span => &mut self.span,
			Native::Loc => &mut self.loc,
			Native::Stream => &mut self.stream
		}
	}
}

impl<T: Namespace> Context<T> {
	pub fn new(ids: T) -> Self where T: Sized {
		let mut context = Self {
			ids,
			functions: Vec::new(),
			modules: Vec::new(),
			types: Vec::new(),
			native_types: NativeTypes::new()
		};

		context.add_module(Module::root());
		context
	}

	pub fn id(&self) -> &T {
		&self.ids
	}

	pub fn id_mut(&mut self) -> &mut T {
		&mut self.ids
	}

	pub fn root_module(&self) -> &Module<T> {
		&self.modules[0]
	}

	pub fn module(&self, index: u32) -> Option<&Module<T>> {
		self.modules.get(index as usize)
	}

	pub fn module_mut(&mut self, index: u32) -> Option<&mut Module<T>> {
		self.modules.get_mut(index as usize)
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

	pub fn ty_ref_by_id(&self, id: ty::Id<T>) -> Option<ty::Ref> {
		match id {
			ty::Id::Defined(t) => {
				for (i, ty) in self.types.iter().enumerate() {
					match ty.id() {
						ty::Id::Defined(u) if u == t => return Some(ty::Ref::Defined(i as u32)),
						_ => ()
					}
				}

				None
			},
			ty::Id::Native(n) => {
				let ty_ref = ty::Ref::Native(n);
				if self.ty(ty_ref).is_some() {
					Some(ty_ref)
				} else {
					None
				}
			}
		}
	}

	pub fn ty_ref_by_ident(&self, ident: &Ident) -> Option<ty::Ref> {
		for (i, ty) in self.types.iter().enumerate() {
			match ty.id() {
				ty::Id::Defined(u) if self.ids.type_ident(u) == *ident => return Some(ty::Ref::Defined(i as u32)),
				_ => ()
			}
		}

		ty::Native::from_ident(ident).map(|n| {
			let ty_ref = ty::Ref::Native(n);
			if self.ty(ty_ref).is_some() {
				Some(ty_ref)
			} else {
				None
			}
		}).flatten()
	}

	pub fn parse_ty_expr(&self, string: &str) -> Result<Option<ty::Expr<T>>, InvalidTypeExpr> {
		#[derive(Default)]
		struct Node<T: Namespace + ?Sized> {
			id: String,
			args: Vec<ty::Expr<T>>
		}

		impl<T: Namespace + ?Sized> Node<T> {
			fn new() -> Self {
				Self {
					id: String::new(),
					args: Vec::new()
				}
			}
		}
		
		let mut stack = Vec::new();
		stack.push(Node::new());

		let mut chars = string.chars();
		while let Some(c) = chars.next() {
			match c {
				c if c.is_whitespace() => (),
				'(' => {
					stack.push(Node::new())
				},
				',' | ')' => {
					let node = stack.pop().ok_or(InvalidTypeExpr)?;
					let id = Ident::new(node.id).map_err(|_| InvalidTypeExpr)?;
					match self.ty_ref_by_ident(&id) {
						Some(ty_ref) => {
							let ty_expr = ty::Expr::Instance(ty_ref, node.args);
							stack.last_mut().ok_or(InvalidTypeExpr)?.args.push(ty_expr);
							if c == ',' {
								stack.push(Node::new())
							}
						},
						None => return Ok(None)
					}
				}
				c => stack.last_mut().ok_or(InvalidTypeExpr)?.id.push(c)
			}
		}

		let node = stack.pop().ok_or(InvalidTypeExpr)?;
		let id = Ident::new(node.id).map_err(|_| InvalidTypeExpr)?;
		let args = node.args;
		Ok(self.ty_ref_by_ident(&id).map(|ty_ref| ty::Expr::Instance(ty_ref, args)))
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
	
	/// Add a new module.
	pub fn add_module(&mut self, mut m: Module<T>) -> u32 {
		let i = self.modules.len() as u32;
		m.set_index(i);
		if let Some(parent) = m.parent() {
			self.modules[parent as usize].add_module(i)
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

	pub fn lexer_types(&self) -> impl '_ + Iterator<Item=u32> {
		self.types.iter().enumerate().filter(|(_, ty)| ty.is_lexer()).map(|(i, _)| i as u32)
	}

	pub fn parsers_for<'a>(&'a self, ty: &'a ty::Expr<T>) -> impl 'a + Iterator<Item=u32> {
		self.functions.iter().enumerate().filter(move |(_, f)| f.is_parser_for(ty)).map(|(i, _)| i as u32)
	}
}

/// Types used as identifiers.
pub trait Namespace {
	/// Const variable identifier type.
	type Var: Copy + PartialEq + Eq + Hash;

	/// Module identifier type.
	type Module: Copy;

	/// Type identifier type.
	type Type: Copy + PartialEq + Eq;

	/// Type parameter identifier type.
	type Param: Copy  + PartialEq + Eq;

	/// Enum variant identifier type.
	type Variant: Copy;

	/// Struct field identifier type.
	type Field: Copy;

	/// Label identifier type.
	type Label: Copy + PartialEq + Eq + Hash;

	/// Function identifier type.
	type Function: Copy;

	/// Returns the identifier associated to the given variable.
	fn var_ident(&self, v: Self::Var) -> Ident;

	fn module_ident(&self, m: Self::Module) -> Ident;

	fn type_ident(&self, t: Self::Type) -> Ident;

	fn param_ident(&self, p: Self::Param) -> Ident;

	fn variant_ident(&self, v: Self::Variant) -> Ident;

	fn field_ident(&self, f: Self::Field) -> Ident;

	fn label_ident(&self, l: Self::Label) -> Ident;

	fn function_ident(&self, f: Self::Function) -> Ident;
}

// pub trait ReverseNamespace: Namespace {
// 	fn ty(&self, id: Ident) -> Self::Type;

// 	fn param(&self, id: Ident) -> Self::Param;
// }