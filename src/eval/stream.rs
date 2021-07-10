use crate::{
	Ids,
	Context,
	Constant
};
use super::{
	Value,
	value,
	Environement,
	Lexer,
	Error
};

pub enum Stream<'v> {
	/// Lexer.
	Lexer(Lexer<'v>),

	/// Buffer characters.
	Chars(std::vec::IntoIter<char>)
}

impl<'v> Stream<'v> {
	pub fn pull<T: Ids>(value: &mut Value<'v>, context: &Context<T>) -> Result<Value<'v>, Error> {
		let stream = value.as_stream_mut()?;
		match stream {
			Self::Lexer(lexer) => {
				let f = context.function(lexer.method()).unwrap();
				let body = f.body().ok_or(Error::UnimplementedFunction)?;
				let mut env = Environement::new(context);
				env.set_this(value::Borrowed::Mut(value));
				env.eval(body)
			}
			Self::Chars(chars) => {
				Ok(match chars.next() {
					Some(c) => Value::some(Value::Constant(Constant::Char(c))),
					None => Value::none()
				})
			}
		}
	}
}