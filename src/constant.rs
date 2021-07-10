#[derive(Clone, PartialEq, Eq)]
pub enum Constant {
	Unit,
	Int(u32),
	Char(char),
	CharRange(char, char),
	String(String)
}
