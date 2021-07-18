use crate::{
	Namespace,
	ty,
	Expr
};

#[derive(Clone, Copy)]
pub enum Owner {
	/// The function is defined in the given module.
	Module(u32),

	/// The function is a method of the given type.
	Type(ty::Ref)
}

pub struct Arg<T: Namespace + ?Sized> {
	id: T::Var,
	mutable: bool,
	ty: ty::Expr<T>
}

impl<T: Namespace + ?Sized> Arg<T> {
	pub fn new(id: T::Var, mutable: bool, ty: ty::Expr<T>) -> Self {
		Self {
			id, mutable, ty
		}
	}

	pub fn id(&self) -> T::Var {
		self.id
	}

	pub fn is_mutable(&self) -> bool {
		self.mutable
	}

	pub fn ty(&self) -> &ty::Expr<T> {
		&self.ty
	}
}

/// Function signature.
pub struct Signature<T: Namespace + ?Sized> {
	/// Signature marker (indicates the role of the function).
	marker: Option<Marker>,

	/// If it borrows `this` mutably.
	mutates: bool,

	/// Arguments.
	args: Vec<Arg<T>>,

	/// Return types.
	/// 
	/// Yes, a function may return multiple values.
	return_tys: Vec<ty::Expr<T>>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Marker {
	ExternParser,
	UnexpectedChar,
	Parser,
	Lexer,
	DebugFormat
}

impl Marker {
	pub fn is_lexer(&self) -> bool {
		match self {
			Self::Lexer => true,
			_ => false
		}
	}

	pub fn is_parser(&self) -> bool {
		match self {
			Self::Parser => true,
			_ => false
		}
	}

	pub fn is_extern_parser(&self) -> bool {
		match self {
			Self::ExternParser => true,
			_ => false
		}
	}

	pub fn is_unexpected_char_constructor(&self) -> bool {
		match self {
			Self::UnexpectedChar => true,
			_ => false
		}
	}

	pub fn is_debug_formatter(&self) -> bool {
		match self {
			Self::DebugFormat => true,
			_ => false
		}
	}
}

impl<T: Namespace + ?Sized> Signature<T> {
	pub fn extern_parser(x: T::Var, target_ty: ty::Expr<T>, error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::ExternParser),
			mutates: false,
			args: vec![Arg::new(
				x,
				false,
				ty::Expr::string()
			)],
			return_tys: vec![ty::Expr::result(target_ty, error_ty)]
		}
	}

	pub fn unexpected_char_constructor(x: T::Var, error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::UnexpectedChar),
			mutates: false,
			args: vec![Arg::new(
				x,
				false,
				ty::Expr::option(ty::Expr::char())
			)],
			return_tys: vec![error_ty]
		}
	}

	pub fn lexer(this: T::Var, lexer_ty: ty::Expr<T>, token_ty: ty::Expr<T>, lexer_error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::Lexer),
			mutates: true,
			args: vec![Arg::new(
				this,
				true,
				lexer_ty.clone()
			)],
			return_tys: vec![
				lexer_ty,
				ty::Expr::result(ty::Expr::option(token_ty), lexer_error_ty)
			]
		}
	}

	pub fn parser(lexer: T::Var, token_ty: ty::Expr<T>, lexer_error_ty: ty::Expr<T>, target_ty: ty::Expr<T>, error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::Parser),
			mutates: false,
			args: vec![Arg::new(
				lexer,
				true,
				ty::Expr::stream(ty::Expr::result(token_ty, lexer_error_ty))
			)],
			return_tys: vec![ty::Expr::result(target_ty, error_ty)]
		}
	}

	pub fn debug_format(this: T::Var, ty: ty::Expr<T>, output: T::Var) -> Self {
		Self {
			marker: Some(Marker::DebugFormat),
			mutates: false,
			args: vec![
				Arg::new(
					this,
					false,
					ty::Expr::reference(ty)
				),
				Arg::new(
					output,
					true,
					ty::Expr::output()
				)
			],
			return_tys: vec![ty::Expr::output()]
		}
	}

	pub fn marker(&self) -> Option<Marker> {
		self.marker
	}

	pub fn arguments(&self) -> &[Arg<T>] {
		&self.args
	}

	pub fn return_types(&self) -> &[ty::Expr<T>] {
		&self.return_tys
	}

	pub fn arity(&self) -> u32 {
		self.args.len() as u32
	}

	pub fn is_lexer(&self) -> bool {
		self.marker.map(|m| m.is_lexer()).unwrap_or(false)
	}

	pub fn is_parser_for(&self, t: &ty::Expr<T>) -> bool {
		self.marker.map(|m| m.is_parser()).unwrap_or(false) &&
		self.return_tys[0].ok_type().unwrap() == t
	}

	pub fn is_debug_formatter(&self) -> bool {
		self.marker.map(|m| m.is_debug_formatter()).unwrap_or(false)
	}

	pub fn mutates(&self) -> bool {
		self.mutates
	}
}

pub struct Function<T: Namespace + ?Sized> {
	owner: Owner,
	id: T::Function,
	signature: Signature<T>,
	body: Option<Expr<T>>
}

impl<T: Namespace + ?Sized> Function<T> {
	pub fn new(owner: Owner, id: T::Function, signature: Signature<T>, body: Option<Expr<T>>) -> Self {
		Self {
			owner,
			id,
			signature,
			body
		}
	}

	pub fn owner(&self) -> Owner {
		self.owner
	}

	pub fn id(&self) -> T::Function {
		self.id
	}

	pub fn is_type_function(&self) -> bool {
		match self.owner {
			Owner::Type(_) => true,
			Owner::Module(_) => false
		}
	}

	pub fn signature(&self) -> &Signature<T> {
		&self.signature
	}

	/// Returns the name of the variable that can be used as
	/// a `this` (or `self`) variable during code generation.
	/// 
	/// It is the first variable of a function owned by a type
	/// whose type is the same as its owner.
	pub fn this(&self) -> Option<T::Var> {
		match self.owner {
			Owner::Type(ty_ref) => {
				if self.signature.arguments().is_empty() {
					None
				} else {
					let first = &self.signature.arguments()[0];
					match first.ty().referenced_type() {
						Some(ty) => match ty.as_instance() {
							Some((first_ty_ref, args)) => {
								if ty_ref == first_ty_ref && args.iter().all(|a| a.is_var()) {
									Some(first.id())
								} else {
									None
								}
							},
							None => None
						}
						None => None
					}
				}
			},
			_ => None
		}
	}

	pub fn body(&self) -> Option<&Expr<T>> {
		self.body.as_ref()
	}

	pub fn is_lexer(&self) -> bool {
		self.signature().is_lexer()
	}

	pub fn is_parser_for(&self, t: &ty::Expr<T>) -> bool {
		self.signature().is_parser_for(t)
	}

	pub fn is_debug_formatter_for(&self, t: ty::Ref) -> bool {
		self.signature().is_debug_formatter() &&
		match &self.owner {
			Owner::Type(o) => *o == t,
			_ => false
		}
	}
}