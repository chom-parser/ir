#[derive(Clone)]
pub enum Constant {
	Int(u32),
	Char(char),
	CharRange(char, char),
}
