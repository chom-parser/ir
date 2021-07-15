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

	/// Return type.
	return_ty: ty::Expr<T>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Marker {
	ExternParser,
	UndefinedChar,
	Parser,
	Lexer
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

	pub fn is_undefined_char_constructor(&self) -> bool {
		match self {
			Self::UndefinedChar => true,
			_ => false
		}
	}
}

// /// Function signature.
// pub enum Signature<T: Namespace + ?Sized> {
// 	/// Extern parser used by the lexer to process tokens.
// 	/// 
// 	/// The given `Ident` is a custom name used to build
// 	/// the function's name.
// 	ExternParser(T::Var, Ident),

// 	/// Undefined character function.
// 	/// 
// 	/// The given type is the returned error type.
// 	UndefinedChar(T::Var, ty::Expr<T>),

// 	/// Parser function.
// 	/// 
// 	/// The first given type is the token type,
// 	/// the second is the returned type.
// 	Parser(T::Var, ty::Expr<T>, ty::Expr<T>),

// 	/// Lexer function.
// 	/// 
// 	/// The first type is the token type.
// 	/// The second type is the lexing error type.
// 	Lexer(ty::Expr<T>, ty::Expr<T>)
// }

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
			return_ty: ty::Expr::result(target_ty, error_ty)
		}
	}

	pub fn undefined_char_constructor(x: T::Var, error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::UndefinedChar),
			mutates: false,
			args: vec![Arg::new(
				x,
				false,
				ty::Expr::option(ty::Expr::char())
			)],
			return_ty: error_ty
		}
	}

	pub fn lexer(token_ty: ty::Expr<T>, lexer_error_ty: ty::Expr<T>) -> Self {
		Self {
			marker: Some(Marker::Lexer),
			mutates: true,
			args: vec![],
			return_ty: ty::Expr::result(token_ty, lexer_error_ty)
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
			return_ty: ty::Expr::result(target_ty, error_ty)
		}
	}

	pub fn marker(&self) -> Option<&Marker> {
		self.marker.as_ref()
	}

	pub fn arguments(&self) -> &[Arg<T>] {
		&self.args
	}

	pub fn return_type(&self) -> &ty::Expr<T> {
		&self.return_ty
	}

	pub fn arity(&self) -> u32 {
		self.args.len() as u32
	}

	pub fn is_lexer(&self) -> bool {
		self.marker.map(|m| m.is_lexer()).unwrap_or(false)
	}

	pub fn is_parser_for(&self, t: &ty::Expr<T>) -> bool {
		self.marker.map(|m| m.is_parser()).unwrap_or(false) &&
		self.return_ty.ok_type().unwrap() == t
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

	pub fn is_method(&self) -> bool {
		match self.owner {
			Owner::Type(_) => true,
			Owner::Module(_) => false
		}
	}

	pub fn is_method_mut(&self) -> bool {
		self.is_method() && self.signature.mutates()
	}

	pub fn signature(&self) -> &Signature<T> {
		&self.signature
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
}