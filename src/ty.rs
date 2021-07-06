use crate::Ids;

/// Type definition.
pub struct Type<T: Ids> {
	/// Index of the containing module.
	module: Option<u32>,

	/// Identifier.
	id: Id<T>,

	/// Parameters.
	parameters: Vec<T::Param>,

	/// Description.
	desc: Desc<T>,

	/// Methods.
	methods: Vec<u32>
}

impl<T: Ids> Type<T> {
	/// Creates a new opaque type from a grammar extern type.
	pub fn opaque(module: u32, id: T::Type) -> Self {
		Self {
			module: Some(module),
			id: Id::Defined(id),
			parameters: Vec::new(),
			desc: Desc::Opaque,
			methods: Vec::new()
		}
	}

	pub fn new(module: u32, id: T::Type, desc: Desc<T>) -> Self {
		Self {
			module: Some(module),
			id: Id::Defined(id),
			parameters: Vec::new(),
			desc,
			methods: Vec::new()
		}
	}

	pub(crate) fn native(id: Native) -> Self {
		let desc = match id {
			Native::Option => {
				let mut enm = Enum::new();
				enm.add_variant(Variant::Native(NativeVariant::None));
				enm.add_variant(Variant::Native(NativeVariant::Some));
				Desc::Enum(enm)
			},
			_ => Desc::Opaque
		};

		Self {
			module: None,
			id: Id::Native(id),
			parameters: Vec::new(),
			desc,
			methods: Vec::new()
		}
	}

	pub fn module(&self) -> Option<u32> {
		self.module
	}

	pub fn id(&self) -> Id<T> {
		self.id
	}

	pub fn parameters(&self) -> &[T::Param] {
		&self.parameters
	}

	pub fn desc(&self) -> &Desc<T> {
		&self.desc
	}

	pub fn set_desc(&mut self, desc: Desc<T>) {
		self.desc = desc
	}

	pub fn as_enum(&self) -> Option<&Enum<T>> {
		match &self.desc {
			Desc::Enum(enm) => Some(enm),
			_ => None,
		}
	}

	pub(crate) fn add_method(&mut self, f: u32) {
		self.methods.push(f)
	}
}

pub enum Id<T: Ids> {
	Native(Native),
	Defined(T::Type)
}

impl<T: Ids> Clone for Id<T> {
	fn clone(&self) -> Self {
		match self {
			Self::Native(n) => Self::Native(*n),
			Self::Defined(d) => Self::Defined(*d)
		}
	}
}

impl<T: Ids> Copy for Id<T> {}

/// Type reference.
#[derive(Clone, Copy)]
pub enum Ref {
	/// Native type provided by the target language.
	Native(Native),

	/// Defined type.
	Defined(u32),
}

pub enum Desc<T: Ids> {
	Opaque,
	Enum(Enum<T>),
	Struct(Struct<T>),
	TupleStruct(Vec<Expr>),
}

/// Enumerator type.
pub struct Enum<T: Ids> {
	variants: Vec<Variant<T>>,
}

impl<T: Ids> Enum<T> {
	pub fn new() -> Self {
		Self {
			variants: Vec::new(),
		}
	}

	pub fn len(&self) -> u32 {
		self.variants.len() as u32
	}

	pub fn is_empty(&self) -> bool {
		self.variants.is_empty()
	}

	pub fn variant(&self, index: u32) -> Option<&Variant<T>> {
		self.variants.get(index as usize)
	}

	pub fn variants(&self) -> &[Variant<T>] {
		&self.variants
	}

	pub fn add_variant(&mut self, v: Variant<T>) -> u32 {
		let i = self.variants.len() as u32;
		self.variants.push(v);
		i
	}

	pub fn not_empty(self) -> Option<Self> {
		if self.is_empty() {
			None
		} else {
			Some(self)
		}
	}
}

/// Enum variant.
pub enum Variant<T: Ids> {
	/// Baked-in variant.
	Native(NativeVariant),

	/// Defined variant.
	Defined(T::Variant, VariantDesc<T>),
}

impl<T: Ids> Variant<T> {
	pub fn is_empty(&self) -> bool {
		match self {
			Self::Native(t) => t.parameters().is_empty(),
			Self::Defined(_, desc) => desc.is_empty(),
		}
	}

	pub fn tuple_parameters(&self) -> Option<&[Expr]> {
		match self {
			Self::Native(t) => Some(t.parameters()),
			Self::Defined(_, VariantDesc::Tuple(params)) => Some(params.as_ref()),
			_ => None,
		}
	}

	pub fn struct_parameter(&self) -> Option<&Struct<T>> {
		match self {
			Self::Defined(_, VariantDesc::Struct(s)) => Some(s),
			_ => None,
		}
	}
}

pub enum VariantDesc<T: Ids> {
	/// The variant contains untagged parameters.
	///
	/// The given list is not empty.
	Tuple(Vec<Expr>),

	/// The variant contains only tagged parameters,
	/// defining a structure.
	Struct(Struct<T>),
}

impl<T: Ids> VariantDesc<T> {
	pub fn len(&self) -> u32 {
		match self {
			Self::Tuple(args) => args.len() as u32,
			Self::Struct(s) => s.len(),
		}
	}

	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}
}

/// Structure type.
pub struct Struct<T: Ids> {
	fields: Vec<Field<T>>,
}

impl<T: Ids> Struct<T> {
	pub fn new() -> Self {
		Self { fields: Vec::new() }
	}

	pub fn len(&self) -> u32 {
		self.fields.len() as u32
	}

	pub fn is_empty(&self) -> bool {
		self.fields.is_empty()
	}

	pub fn fields(&self) -> &[Field<T>] {
		&self.fields
	}

	pub fn add_field(&mut self, id: T::Field, ty: Expr) -> u32 {
		let field = Field { id, ty };
		let i = self.fields.len() as u32;
		self.fields.push(field);
		i
	}
}

pub struct Field<T: Ids> {
	pub id: T::Field,
	pub ty: Expr,
}

pub struct Instance {
	pub ty: Ref,
	pub args: Vec<Instance>
}

/// Type expression.
#[derive(Clone)]
pub enum Expr {
	/// Type variable.
	Var(u32),

	/// Type instance.
	Instance(Ref, Vec<Expr>),
}

/// Native type provided by the target language.
#[derive(Clone, Copy)]
pub enum Native {
	/// Unit type.
	Unit,

	/// On-heap type.
	///
	/// In Rust, this will generally be translated into `Box`.
	Heap,

	/// Optional type.
	Option,

	/// List type.
	///
	/// In Rust, this will generally be translated into `Vec`.
	List,

	/// Located type.
	///
	/// In Rust, this will generally be translated into `::source_span::Loc<T>`.
	Loc,

	/// Lexer type.
	Lexer,

	/// Stack.
	Stack,
}

/// Native enum variants.
#[derive(Clone, Copy)]
pub enum NativeVariant {
	Some,
	None,
	Ok,
	Err,
}

impl NativeVariant {
	pub fn parameters(&self) -> &[Expr] {
		match self {
			Self::Some => &[Expr::Var(0)],
			Self::Ok => &[Expr::Var(0)],
			Self::Err => &[Expr::Var(1)],
			_ => &[],
		}
	}
}
