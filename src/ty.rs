use derivative::Derivative;
use crate::{
	Ident,
	Namespace
};

#[derive(Derivative)]
#[derivative(Clone(bound=""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""))]
pub enum Param<T: Namespace + ?Sized> {
	Defined(T::Param),
	Native(NativeParam)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NativeParam {
	/// Value type parameter.
	Value,

	/// Error type parameter.
	Error
}

/// Type definition.
pub struct Type<T: Namespace + ?Sized> {
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

impl<T: Namespace + ?Sized> Type<T> {
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

	pub fn is_lexer(&self) -> bool {
		self.desc.is_lexer()
	}
}

pub enum Id<T: Namespace + ?Sized> {
	Native(Native),
	Defined(T::Type)
}

impl<T: Namespace + ?Sized> Id<T> {
	pub fn ident(&self, ns: &T) -> Ident {
		match self {
			Self::Native(n) => n.ident(),
			Self::Defined(i) => ns.type_ident(*i)
		}
	}
}

impl<T: Namespace + ?Sized> Clone for Id<T> {
	fn clone(&self) -> Self {
		match self {
			Self::Native(n) => Self::Native(*n),
			Self::Defined(d) => Self::Defined(*d)
		}
	}
}

impl<T: Namespace + ?Sized> Copy for Id<T> {}

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

pub enum Desc<T: Namespace + ?Sized> {
	Opaque,
	Enum(Enum<T>),
	Struct(Struct<T>),
	TupleStruct(Vec<Expr<T>>),
	Lexer
}

impl<T: Namespace + ?Sized> Desc<T> {
	pub fn is_lexer(&self) -> bool {
		match self {
			Self::Lexer => true,
			_ => false
		}
	}
}

/// Enumerator type.
pub struct Enum<T: Namespace + ?Sized> {
	variants: Vec<Variant<T>>,
}

impl<T: Namespace + ?Sized> Enum<T> {
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
pub enum Variant<T: Namespace + ?Sized> {
	/// Baked-in variant.
	Native(NativeVariant),

	/// Defined variant.
	Defined(T::Variant, VariantDesc<T>),
}

impl<T: Namespace + ?Sized> Variant<T> {
	pub fn len(&self) -> u32 {
		match self {
			Self::Native(t) => t.parameters::<T>().len() as u32,
			Self::Defined(_, desc) => desc.len(),
		}
	}

	pub fn is_empty(&self) -> bool {
		match self {
			Self::Native(t) => t.parameters::<T>().is_empty(),
			Self::Defined(_, desc) => desc.is_empty(),
		}
	}

	pub fn ident(&self, ns: &T) -> Ident {
		match self {
			Self::Native(n) => n.ident(),
			Self::Defined(i, _) => ns.variant_ident(*i)
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

pub enum VariantDesc<T: Namespace + ?Sized> {
	/// The variant contains untagged parameters.
	///
	/// The given list is not empty.
	Tuple(Vec<Expr<T>>),

	/// The variant contains only tagged parameters,
	/// defining a structure.
	Struct(Struct<T>),
}

impl<T: Namespace + ?Sized> VariantDesc<T> {
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
pub struct Struct<T: Namespace + ?Sized> {
	fields: Vec<Field<T>>,
}

impl<T: Namespace + ?Sized> Struct<T> {
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

pub struct Field<T: Namespace + ?Sized> {
	pub id: T::Field,
	pub ty: Expr<T>,
}

// pub struct Instance {
// 	pub ty: Ref,
// 	pub args: Vec<Instance>
// }

/// Type expression.
#[derive(Derivative)]
#[derivative(Clone(bound=""))]
pub enum Expr<T: Namespace + ?Sized> {
	/// Type variable.
	Var(Param<T>),

	/// Type instance.
	Instance(Ref, Vec<Expr<T>>),
}

impl<T: Namespace + ?Sized> PartialEq for Expr<T> {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Var(x), Self::Var(y)) => x == y,
			(Self::Instance(a, args), Self::Instance(b, brgs)) => {
				a == b && args == brgs
			},
			_ => false
		}
	}
}

impl<T: Namespace + ?Sized> Eq for Expr<T> {}

impl<T: Namespace + ?Sized> Expr<T> {
	pub fn as_reference(&self) -> Option<Ref> {
		match self {
			Self::Instance(r, _) => Some(*r),
			_ => None
		}
	}

	pub fn char() -> Self {
		Self::Instance(Ref::Native(Native::Char), Vec::new())
	}

	pub fn string() -> Self {
		Self::Instance(Ref::Native(Native::String), Vec::new())
	}

	pub fn option(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Option), vec![e])
	}

	pub fn result(a: Expr<T>, b: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Result), vec![a, b])
	}

	pub fn heap(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Heap), vec![e])
	}

	pub fn locate(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Loc), vec![e])
	}

	pub fn stream(e: Expr<T>) -> Self {
		Self::Instance(Ref::Native(Native::Stream), vec![e])
	}

	/// If this type expression is a `result(a, b)`,
	/// returns `(a, b)`.
	pub fn as_result_type(&self) -> Option<(&Self, &Self)> {
		match self {
			Self::Instance(Ref::Native(Native::Result), args) => Some((args.get(0).unwrap(), args.get(1).unwrap())),
			_ => None
		}
	}

	/// If this type expression is a `result(a, b)`,
	/// returns the `a` type.
	pub fn ok_type(&self) -> Option<&Self> {
		self.as_result_type().map(|(a, _)| a)
	}

	/// If this type expression is a `result(a, b)`,
	/// returns the `b` type.
	pub fn err_type(&self) -> Option<&Self> {
		self.as_result_type().map(|(_, b)| b)
	}
}

/// Native type provided by the target language.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Native {
	/// Unit type.
	Unit,

	/// Character.
	Char,

	/// String (or string reference).
	String,

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

	/// Stream type.
	Stream
}

impl Native {
	pub fn from_ident(id: &Ident) -> Option<Self> {
		match id.as_str() {
			"unit" => Some(Self::Unit),
			"char" => Some(Self::Char),
			"string" => Some(Self::String),
			"heap" => Some(Self::Heap),
			"option" => Some(Self::Option),
			"result" => Some(Self::Result),
			"list" => Some(Self::List),
			"position" => Some(Self::Position),
			"span" => Some(Self::Span),
			"loc" => Some(Self::Loc),
			"stack" => Some(Self::Stack),
			"stream" => Some(Self::Stream),
			_ => None
		}
	}

	pub fn ident(&self) -> Ident {
		match self {
			Self::Unit => Ident::new("unit").unwrap(),
			Self::Char => Ident::new("char").unwrap(),
			Self::String => Ident::new("string").unwrap(),
			Self::Heap => Ident::new("heap").unwrap(),
			Self::Option => Ident::new("option").unwrap(),
			Self::Result => Ident::new("result").unwrap(),
			Self::List => Ident::new("list").unwrap(),
			Self::Position => Ident::new("position").unwrap(),
			Self::Span => Ident::new("span").unwrap(),
			Self::Loc => Ident::new("loc").unwrap(),
			Self::Stack => Ident::new("stack").unwrap(),
			Self::Stream => Ident::new("stream").unwrap(),
		}
	}

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
	pub fn from_ident(id: &Ident) -> Option<Self> {
		match id.as_str() {
			"some" => Some(Self::Some),
			"none" => Some(Self::None),
			"ok" => Some(Self::Ok),
			"err" => Some(Self::Err),
			_ => None
		}
	}

	pub fn ident(&self) -> Ident {
		match self {
			Self::Some => Ident::new("some").unwrap(),
			Self::None => Ident::new("none").unwrap(),
			Self::Ok => Ident::new("ok").unwrap(),
			Self::Err => Ident::new("err").unwrap(),
		}
	}

	pub fn parameters<T: Namespace + ?Sized>(&self) -> &[Expr<T>] {
		match self {
			Self::Some => &[Expr::Var(Param::Native(NativeParam::Value))],
			Self::Ok => &[Expr::Var(Param::Native(NativeParam::Value))],
			Self::Err => &[Expr::Var(Param::Native(NativeParam::Error))],
			_ => &[],
		}
	}
}
