#[derive(Clone, PartialEq, Eq)]
pub enum Constant {
	Int(u32),
	Char(char),
	CharRange(char, char),
}
