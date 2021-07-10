use crate::{
	Ids,
	Ident,
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

/// Function signature.
pub enum Signature<T: Ids> {
	/// Extern parser used by the lexer to process tokens.
	/// 
	/// The given `Ident` is a custom name used to build
	/// the function's name.
	ExternParser(T::Var, Ident),

	/// Undefined character function.
	/// 
	/// The given type is the returned error type.
	UndefinedChar(T::Var, ty::Expr<T>),

	/// Parser function.
	/// 
	/// The first given type is the token type,
	/// the second is the returned type.
	Parser(T::Var, ty::Expr<T>, ty::Expr<T>),

	/// Lexer function.
	/// 
	/// The first type is the token type.
	/// The second type is the lexing error type.
	Lexer(ty::Expr<T>, ty::Expr<T>)
}

impl<T: Ids> Signature<T> {
	pub fn arguments(&self) -> &[T::Var] {
		match self {
			Self::ExternParser(x, _) => std::slice::from_ref(x),
			Self::UndefinedChar(x, _) => std::slice::from_ref(x),
			Self::Parser(x, _, _) => std::slice::from_ref(x),
			Self::Lexer(_, _) => &[]
		}
	}

	pub fn arity(&self) -> u32 {
		self.arguments().len() as u32
	}

	pub fn is_lexer(&self) -> bool {
		match self {
			Self::Lexer(_, _) => true,
			_ => false
		}
	}
}

pub struct Function<T: Ids> {
	owner: Owner,
	signature: Signature<T>,
	body: Option<Expr<T>>
}

impl<T: Ids> Function<T> {
	pub fn new(owner: Owner, signature: Signature<T>, body: Option<Expr<T>>) -> Self {
		Self {
			owner,
			signature,
			body
		}
	}

	pub fn owner(&self) -> Owner {
		self.owner
	}

	pub fn is_method(&self) -> bool {
		match self.owner {
			Owner::Type(_) => true,
			Owner::Module(_) => false
		}
	}

	pub fn signature(&self) -> &Signature<T> {
		&self.signature
	}

	pub fn body(&self) -> Option<&Expr<T>> {
		self.body.as_ref()
	}
}