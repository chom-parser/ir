use source_span::Loc;
use crate::{
	Namespace,
	ty
};

pub mod value;
pub mod environment;
pub mod error;
mod lexer;
pub mod stream;
mod stack;

pub use value::Value;
pub use environment::Environment;
pub use error::Error;
pub use lexer::Lexer;
pub use stack::Stack;

/// Create a new lexer from the given source.
pub fn lexer<'a, 'v, 'e, T: Namespace, I>(env: &Environment<'a, 'v, 'e, T>, input: &'v mut I) -> Result<Value<'v>, Error> where I: Iterator<Item=std::io::Result<char>> {
	match env.context().lexer_types().next() {
		Some(ty_index) => {
			env.instanciate(ty::Ref::Defined(ty_index), Some(Value::Input(input)))
		},
		None => Err(Error::new(error::Desc::NoLexer))
	}
}

pub fn parse<'a, 'v, 'e, T: Namespace, P: Iterator<Item=std::io::Result<char>>>(
	env: &mut Environment<'a, 'v, 'e, T>,
	phrase: &'v mut P,
	parser: u32
) -> Result<Result<Value<'v>, Loc<error::Value<'v>>>, Error> {
	let lexer = lexer(env, phrase)?;
	let result = env.call(parser, None, vec![lexer])?;
	Ok(match result.into_result()? {
		Ok(v) => Ok(v),
		Err(v) => Err(v.into_loc_error()?)
	})
}