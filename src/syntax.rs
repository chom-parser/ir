pub mod ast;
pub mod lexer;
pub mod parser;

pub mod glue {
    pub enum Error {
        // ...
    }

    pub fn unexpected(c: Option<char>) -> Error {
        panic!("TODO")
    }

    pub type Int = u32;
    pub type Char = char;
    pub type String = std::string::String;

    pub fn int(s: &str) -> Result<Int, Error> {
        unimplemented!()
    }

    pub fn char(s: &str) -> Result<Char, Error> {
        unimplemented!()
    }

    pub fn string(s: &str) -> Result<String, Error> {
        unimplemented!()
    }
}
