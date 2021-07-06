use std::{
	cmp::Ordering,
	convert::TryFrom,
	fmt,
	hash::{Hash, Hasher},
};

/// Normalized identifier.
#[derive(Clone, Eq, Debug)]
pub struct Ident {
	normalized: String,
	prefered: Option<String>,
}

impl PartialEq for Ident {
	fn eq(&self, other: &Ident) -> bool {
		self.normalized.eq(&other.normalized)
	}
}

impl PartialOrd for Ident {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.normalized.partial_cmp(&other.normalized)
	}
}

impl Ord for Ident {
	fn cmp(&self, other: &Self) -> Ordering {
		self.normalized.cmp(&other.normalized)
	}
}

impl Hash for Ident {
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.normalized.hash(h)
	}
}

#[derive(Debug)]
pub struct InvalidIdent;

fn is_word_start(prev: Option<char>, c: char, next: Option<char>) -> bool {
	c.is_uppercase()
		&& prev.map(|p| p.is_lowercase()).unwrap_or(false)
		&& next.map(|p| p.is_lowercase()).unwrap_or(false)
}

fn normalize(id: &str) -> Result<String, InvalidIdent> {
	let mut result = String::new();
	let mut prev = None;
	let mut boundary = true;
	let mut separated = true;
	let mut chars = id.chars().peekable();

	while let Some(c) = chars.next() {
		match c {
			'_' | ' ' | '-' => {
				if !boundary {
					boundary = true;
					separated = false
				}
			}
			c => {
				if is_word_start(prev, c, chars.peek().cloned()) {
					boundary = true
				}

				if boundary && !separated {
					result.push('_');
					separated = true
				}

				result.push(c.to_lowercase().next().unwrap());
				boundary = false
			}
		}

		prev = Some(c);
	}

	if result.is_empty() {
		return Err(InvalidIdent);
	}

	Ok(result)
}

impl TryFrom<String> for Ident {
	type Error = InvalidIdent;

	fn try_from(id: String) -> Result<Self, Self::Error> {
		Ok(Self {
			normalized: normalize(&id)?,
			prefered: Some(id),
		})
	}
}

impl Ident {
	pub fn new<S: AsRef<str>>(id: S) -> Result<Self, InvalidIdent> {
		Ok(Self {
			normalized: normalize(id.as_ref())?,
			prefered: None,
		})
	}

	pub fn as_str(&self) -> &str {
		&self.normalized
	}

	pub fn to_snake_case(&self) -> String {
		self.normalized.clone()
	}

	pub fn to_caml_case(&self) -> String {
		use itertools::Itertools;
		self.normalized
			.split('_')
			.map(|segment| {
				let c = segment.chars().next().unwrap(); // segment is never empty.
				let (_, rest) = segment.split_at(c.len_utf8());
				IntoIterator::into_iter([
					c.to_uppercase().next().unwrap().to_string(),
					rest.to_string(),
				])
			})
			.flatten()
			.join("")
	}

	pub fn push(&mut self, id: &str) {
		if let Ok(id) = normalize(id) {
			self.normalized.push('_');
			self.normalized.push_str(&id);
			self.prefered = None
		}
	}

	pub fn push_ident(&mut self, id: &Ident) {
		self.normalized.push('_');
		self.normalized.push_str(&id.normalized);
		self.prefered = None
	}
}

impl fmt::Display for Ident {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self.prefered {
			Some(id) => id.fmt(f),
			None => self.normalized.fmt(f),
		}
	}
}
