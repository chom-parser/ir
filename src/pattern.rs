use crate::{expr, ty, Constant, Expr, Ids};

/// Pattern.
pub enum Pattern<T: Ids> {
	/// Matches any value.
	Any,

	/// Matches any value and bind to the given ident.
	Bind(T::Var),

	/// Literal value.
	Literal(Constant),

	/// Enum variant.
	///
	/// The first parameter is the type id.
	/// The second is the variant index.
	/// The third is the inner pattern if any.
	Cons(ty::Ref, u32, ConsArgs<T>),

	/// Union.
	Or(Vec<Pattern<T>>),
}

impl<T: Ids> Clone for Pattern<T>
where
	T::Var: Clone,
	T::Field: Clone,
{
	fn clone(&self) -> Self {
		match self {
			Self::Any => Self::Any,
			Self::Bind(v) => Self::Bind(v.clone()),
			Self::Literal(l) => Self::Literal(l.clone()),
			Self::Cons(ty, v, args) => Self::Cons(ty.clone(), *v, args.clone()),
			Self::Or(p) => Self::Or(p.clone()),
		}
	}
}

pub enum ConsArgs<T: Ids> {
	Tuple(Vec<Pattern<T>>),
	Struct(Vec<Binding<T>>),
}

impl<T: Ids> Clone for ConsArgs<T>
where
	T::Var: Clone,
	T::Field: Clone,
{
	fn clone(&self) -> Self {
		match self {
			Self::Tuple(args) => Self::Tuple(args.clone()),
			Self::Struct(bindings) => Self::Struct(bindings.clone()),
		}
	}
}

impl<T: Ids> ConsArgs<T> {
	pub fn is_empty(&self) -> bool {
		match self {
			Self::Tuple(args) => args.is_empty(),
			Self::Struct(bindings) => bindings.is_empty(),
		}
	}
}

pub struct Binding<T: Ids> {
	pub id: T::Field,
	pub pattern: Pattern<T>,
}

impl<T: Ids> Clone for Binding<T>
where
	T::Var: Clone,
	T::Field: Clone,
{
	fn clone(&self) -> Self {
		Self {
			id: self.id.clone(),
			pattern: self.pattern.clone(),
		}
	}
}

impl<T: Ids> Pattern<T> {
	pub fn none() -> Self {
		Self::Cons(
			ty::Ref::Native(ty::Native::Option),
			0,
			ConsArgs::Tuple(Vec::new())
		)
	}

	pub fn some(pattern: Self) -> Self {
		Self::Cons(
			ty::Ref::Native(ty::Native::Option),
			1,
			ConsArgs::Tuple(vec![pattern])
		)
	}

	pub fn is_union(&self) -> bool {
		match self {
			Self::Or(_) => true,
			_ => false,
		}
	}

	pub fn is_bound(&self) -> bool {
		match self {
			Self::Any => false,
			Self::Bind(_) => true,
			Self::Literal(_) => false,
			Self::Cons(_, _, args) => match args {
				ConsArgs::Tuple(args) => args.iter().any(|a| a.is_bound()),
				ConsArgs::Struct(bindings) => bindings.iter().any(|b| b.pattern.is_bound()),
			},
			Self::Or(_) => false,
		}
	}

	pub fn bind_any<F>(&self, f: F) -> Self
	where
		F: Copy + Fn() -> T::Var,
		T::Var: Clone,
		T::Field: Clone,
	{
		match self {
			Self::Any => Self::Bind(f()),
			Self::Bind(id) => Self::Bind(id.clone()),
			Self::Cons(ty, v, args) => {
				let args = match args {
					ConsArgs::Tuple(args) => {
						ConsArgs::Tuple(args.iter().map(|p| p.bind_any(f)).collect())
					}
					ConsArgs::Struct(bindings) => ConsArgs::Struct(
						bindings
							.iter()
							.map(|b| Binding {
								id: b.id.clone(),
								pattern: b.pattern.bind_any(f),
							})
							.collect(),
					),
				};
				Self::Cons(*ty, *v, args)
			}
			Self::Literal(c) => Self::Literal(c.clone()),
			Self::Or(patterns) => Self::Or(patterns.iter().map(|p| p.bind_any(f)).collect()),
		}
	}

	pub fn as_expr<F>(&self, f: F) -> Expr<T>
	where
		F: Copy + Fn(Option<T::Var>) -> Expr<T>,
		T::Var: Clone,
		T::Field: Clone,
	{
		match self {
			Self::Any => f(None),
			Self::Bind(id) => f(Some(id.clone())),
			Self::Cons(ty, v, args) => {
				let args = match args {
					ConsArgs::Tuple(args) => {
						expr::BuildArgs::Tuple(args.iter().map(|p| p.as_expr(f)).collect())
					}
					ConsArgs::Struct(bindings) => expr::BuildArgs::Struct(
						bindings
							.iter()
							.map(|b| expr::Binding {
								id: b.id.clone(),
								expr: b.pattern.as_expr(f),
							})
							.collect(),
					),
				};
				Expr::Cons(*ty, *v, args)
			}
			Self::Literal(c) => Expr::Literal(c.clone()),
			Self::Or(_) => panic!("union pattern cannot be made into an expression"),
		}
	}
}
