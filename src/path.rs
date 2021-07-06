use crate::{
	module,
	ty,
	function,
	Ids
};

pub enum Segment<'a, T: Ids> {
	Module(&'a module::Id<T>),
	Type(ty::Id<T>),
	Function(&'a function::Signature)
}

pub struct Path<'a, T: Ids> {
	parent: Option<Box<Path<'a, T>>>,
	segment: Option<Segment<'a, T>>,
}

impl<'a, T: Ids> Path<'a, T> {
	pub(crate) fn new(parent: Option<Path<'a, T>>, segment: Segment<'a, T>) -> Self {
		Self {
			parent: parent.map(Box::new),
			segment: Some(segment)
		}
	}
}

impl<'a, T: Ids> Iterator for Path<'a, T> {
	type Item = Segment<'a, T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.parent.as_mut().map(|p| p.next()).flatten() {
			Some(id) => Some(id),
			None => self.segment.take(),
		}
	}
}
