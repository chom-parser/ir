use crate::{
	Namespace,
	Context,
	Constant
};
use super::{
	Value,
	value,
	Environment,
	Error,
	error::Desc as E
};

// pub enum Stream<'v> {
// 	/// Lexer.
// 	Lexer(Lexer<'v>),

// 	/// Buffer characters.
// 	Chars(std::vec::IntoIter<char>)
// }

pub fn pull<'v, T: Namespace>(value: &mut Value<'v>, context: &Context<T>) -> Result<Value<'v>, Error> {
	match value {
		Value::Lexer(lexer) => {
			let f = context.function(lexer.method()).unwrap();
			let body = f.body().ok_or_else(|| Error::new(E::UnimplementedFunction))?;
			let mut env = Environment::new(context);
			env.set_this(value::Borrowed::Mut(value));
			env.eval(body)
		}
		Value::Chars(chars) => {
			Ok(match chars.next() {
				Some(c) => Value::some(Value::Constant(Constant::Char(c))),
				None => Value::none()
			})
		},
		_ => Err(Error::new(E::NotAStream))
	}
}