use derivative::Derivative;
use crate::Ids;

#[derive(Derivative)]
#[derivative(Clone, Copy)]
pub enum Param<T: Ids> {
	Defined(T::Param),
	Native(NativeParam)
}

#[derive(Clone, Copy, Debug)]
pub enum NativeParam {
	/// Value type parameter.
	Value,

	/// Error type parameter.
	Error
}

/// Type definition.
pub struct Type<T: Ids> {
	/// Index of the containing module.
	module: Option<u32>,

	/// Identifier.
	id: Id<T>,

	/// Parameters.
	parameters: Vec<Param<T>>,

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
			Native::Result => {
				let mut enm = Enum::new();
				enm.add_variant(Variant::Native(NativeVariant::Ok));
				enm.add_variant(Variant::Native(NativeVariant::Err));
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

	pub fn parameters(&self) -> &[Param<T>] {
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

	pub fn methods(&self) -> &[u32] {
		&self.methods
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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Ref {
	/// Native type provided by the target language.
	Native(Native),

	/// Defined type.
	Defined(u32),
}

impl Ref {
	/// Checks if an instance of the given type may be copied
	/// iff its parameter instances are can also be copied.
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Native(n) => n.is_copiable(),
			Self::Defined(_) => false
		}
	}
}

pub enum Desc<T: Ids> {
	Opaque,
	Enum(Enum<T>),
	Struct(Struct<T>),
	TupleStruct(Vec<Expr<T>>),
	Lexer
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
			Self::Native(t) => t.parameters::<T>().is_empty(),
			Self::Defined(_, desc) => desc.is_empty(),
		}
	}

	pub fn tuple_parameters(&self) -> Option<&[Expr<T>]> {
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
	Tuple(Vec<Expr<T>>),

	/// The variant contains only tagged parameters,
	/// defining a structure.
	Struct(Struct<T>),
}

impl<T: Ids> VariantDesc<T> {
	pub fn empty() -> Self {
		Self::Tuple(Vec::new())
	}
	
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

	pub fn add_field(&mut self, id: T::Field, ty: Expr<T>) -> u32 {
		let field = Field { id, ty };
		let i = self.fields.len() as u32;
		self.fields.push(field);
		i
	}
}

pub struct Field<T: Ids> {
	pub id: T::Field,
	pub ty: Expr<T>,
}

// pub struct Instance {
// 	pub ty: Ref,
// 	pub args: Vec<Instance>
// }

/// Type expression.
#[derive(Derivative)]
#[derivative(Clone)]
pub enum Expr<T: Ids> {
	/// Type variable.
	Var(Param<T>),

	/// Type instance.
	Instance(Ref, Vec<Expr<T>>),
}

impl<T: Ids> Expr<T> {
	pub fn as_reference(&self) -> Option<Ref> {
		match self {
			Self::Instance(r, _) => Some(*r),
			_ => None
		}
	}

	pub fn heap(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Heap), vec![e])
	}

	pub fn locate(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Loc), vec![e])
	}
}

/// Native type provided by the target language.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Native {
	/// Unit type.
	Unit,

	/// On-heap type.
	///
	/// In Rust, this will generally be translated into `Box`.
	Heap,

	/// Optional type.
	Option,

	/// Result type.
	Result,

	/// List type.
	///
	/// In Rust, this will generally be translated into `Vec`.
	List,

	/// Position.
	Position,

	/// Span.
	Span,

	/// Located type.
	///
	/// In Rust, this will generally be translated into `::source_span::Loc<T>`.
	Loc,

	/// Stack.
	Stack,
}

impl Native {
	/// Checks if an instance of the given type may be copied
	/// iff its parameter instances are can also be copied.
	pub fn is_copiable(&self) -> bool {
		match self {
			Self::Unit | Self::Option | Self::Result | Self::Position | Self::Span => true,
			_ => false
		}
	}
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
	pub fn parameters<T: Ids>(&self) -> &[Expr<T>] {
		match self {
			Self::Some => &[Expr::Var(Param::Native(NativeParam::Value))],
			Self::Ok => &[Expr::Var(Param::Native(NativeParam::Value))],
			Self::Err => &[Expr::Var(Param::Native(NativeParam::Error))],
			_ => &[],
		}
	}
}
