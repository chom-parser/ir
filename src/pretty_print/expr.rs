use std::fmt;
use crate::{
	Namespace,
	Expr,
	expr::{
		Var,
		LexerExpr,
		StreamExpr,
		StackExpr,
		SpanExpr,
		Error
	},
	ty
};
use super::{
	PrettyPrint,
	PrettyPrinter
};

impl<T: Namespace> PrettyPrint<T> for Var<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::This => ppf.write("this"),
			Self::Defined(x) => {
				let id = ppf.context().id().var_ident(*x);
				ppf.write(id.as_str())
			}
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for Expr<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Literal(c) => c.fmt(ppf),
			Self::Get(x) => x.fmt(ppf),
			Self::GetField(x, ty_ref, index) => {
				ppf.write(ppf.context().id().var_ident(*x).as_str())?;
				ppf.write(".")?;
				let ty = ppf.context().ty(*ty_ref).unwrap();
				match ty.desc() {
					ty::Desc::Struct(strct) => {
						let f = strct.fields().get(*index as usize).expect("undefined field");
						let id = ppf.context().id().field_ident(f.id);
						ppf.write(id.as_str())
					}
					ty::Desc::TupleStruct(_) => {
						ppf.write_u32(*index)
					},
					_ => panic!("cannot pretty-print field access on non-struct type")
				}
			},
			Self::Let(x, mutable, e, next) => {
				ppf.write("let ")?;
				if *mutable {
					ppf.write("mut ")?;
				}
				ppf.write(ppf.context().id().var_ident(*x).as_str())?;
				ppf.write(" = ")?;
				e.fmt(ppf)?;
				ppf.write(" in ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Self::Update(x, e, next) => {
				ppf.write("set ")?;
				ppf.write(ppf.context().id().var_ident(*x).as_str())?;
				ppf.write(" = ")?;
				e.fmt(ppf)?;
				ppf.write(" in ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Self::New(ty_ref, args) => {
				ppf.write("new ")?;
				ty_ref.fmt(ppf)?;
				if !args.is_empty() {
					ppf.write("(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							ppf.write(", ")?;
						}
						a.fmt(ppf)?;
					}
					ppf.write(")")?;
				}
				Ok(())
			}
			Self::Cons(ty_ref, index, args) => {
				let ty = ppf.context().ty(*ty_ref).unwrap();
				ty_ref.fmt(ppf)?;
				ppf.write(".")?;
				match ty.desc() {
					ty::Desc::Enum(enm) => {
						let v = enm.variant(*index).expect("undefined variant");
						super::ty::variant_id(ppf, v)?;
					}
					_ => panic!("cannot pretty-print variant construction on non-enum type")
				}
				if !args.is_empty() {
					ppf.write("(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							ppf.write(", ")?;
						}
						a.fmt(ppf)?;
					}
					ppf.write(")")?;
				}
				Ok(())
			}
			Self::Heap(e) => {
				ppf.write("heap")?;
				e.fmt(ppf)
			}
			Self::Match { expr, cases } => {
				ppf.write("match ")?;
				expr.fmt(ppf)?;
				ppf.write(" {")?;
				ppf.begin()?;
				for case in cases {
					case.pattern.fmt(ppf)?;
					ppf.write(" => {")?;
					ppf.begin()?;
					case.expr.fmt(ppf)?;
					ppf.end()?;
					ppf.write("}")?;
					ppf.sep()?;
				}
				ppf.end()?;
				ppf.write("}")?;
				ppf.sep()
			}
			Self::LetMatch(pattern, e, next) => {
				ppf.write("let-match ")?;
				pattern.fmt(ppf)?;
				ppf.write(" = ")?;
				e.fmt(ppf)?;
				ppf.write(" in ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Self::Call(index, this, args) => {
				ppf.write("call ")?;
				let f = ppf.context().function(*index).unwrap();
				if let Some((x, mutable)) = this {
					if *mutable {
						ppf.write("mut ")?;
					}

					ppf.write(ppf.context().id().var_ident(*x).as_str())?;
					ppf.write(".")?;
				}
				super::function::function_id(ppf, f.signature())?;
				ppf.write("(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(", ")?;
					}
					a.fmt(ppf)?;
				}
				ppf.write(")")
			}
			Self::TailRecursion { label, args, body } => {
				ppf.write("loop ")?;
				ppf.write(ppf.context().id().label_ident(*label).as_str())?;
				ppf.write(" (")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(", ")?;
					}
					a.fmt(ppf)?;
				}
				ppf.write(") {")?;
				ppf.begin()?;
				body.fmt(ppf)?;
				ppf.end()?;
				ppf.write("}")?;
				ppf.sep()
			}
			Self::Recurse(label, args) => {
				ppf.write("continue ")?;
				ppf.write(ppf.context().id().label_ident(*label).as_str())?;
				ppf.write(" (")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(", ")?;
					}
					a.fmt(ppf)?;
				}
				ppf.write(")")
			}
			Self::Lexer(lexer, e) => {
				ppf.write("lexer ")?;
				lexer.fmt(ppf)?;
				ppf.write(" ")?;
				match e {
					LexerExpr::Peek => ppf.write("peek"),
					LexerExpr::Span => ppf.write("span"),
					LexerExpr::Chars => ppf.write("chars"),
					LexerExpr::Buffer => ppf.write("buffer"),
					LexerExpr::Clear(next) => {
						ppf.write("clear in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
					LexerExpr::Consume(next) => {
						ppf.write("consume in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
				}
			}
			Self::Stream(stream, e) => {
				ppf.write("stream ")?;
				stream.fmt(ppf)?;
				ppf.write(" ")?;
				match e {
					StreamExpr::Pull(x, next) => {
						ppf.write("pull ")?;
						ppf.write(ppf.context().id().var_ident(*x).as_str())?;
						ppf.write(" in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
				}
			}
			Self::Stack(stack, e) => {
				ppf.write("stack ")?;
				stack.fmt(ppf)?;
				ppf.write(" ")?;
				match e {
					StackExpr::Push(a, b, next) => {
						ppf.write("push (")?;
						a.fmt(ppf)?;
						ppf.write(", ")?;
						b.fmt(ppf)?;
						ppf.write(") in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
					StackExpr::Pop(a, b, next) => {
						ppf.write("pop (")?;
						match a {
							Some(a) => ppf.write(ppf.context().id().var_ident(*a).as_str())?,
							None => ppf.write("_")?
						}
						ppf.write(", ")?;
						match b {
							Some(b) => ppf.write(ppf.context().id().var_ident(*b).as_str())?,
							None => ppf.write("_")?
						}
						ppf.write(") in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
				}
			}
			Self::Span(e) => {
				ppf.write("span ")?;
				match e {
					SpanExpr::Locate(e, span) => {
						ppf.write("locate ")?;
						e.fmt(ppf)?;
						ppf.write(" with ")?;
						span.fmt(ppf)
					}
					SpanExpr::FromPosition(e) => {
						ppf.write("from-position ")?;
						e.fmt(ppf)
					}
					SpanExpr::After(e) => {
						ppf.write("after ")?;
						e.fmt(ppf)
					}
					SpanExpr::Transpose(a, b) => {
						ppf.write("transpose ")?;
						a.fmt(ppf)?;
						ppf.write(" default ")?;
						b.fmt(ppf)
					}
					SpanExpr::Unwrap(a, b, e, next) => {
						ppf.write("unwrap ")?;
						e.fmt(ppf)?;
						ppf.write(" into ")?;
						match a {
							Some(a) => ppf.write(ppf.context().id().var_ident(*a).as_str())?,
							None => ppf.write("_")?
						}
						ppf.write(", ")?;
						match b {
							Some(b) => ppf.write(ppf.context().id().var_ident(*b).as_str())?,
							None => ppf.write("_")?
						}
						ppf.write(") in ")?;
						ppf.sep()?;
						next.fmt(ppf)
					}
					SpanExpr::Merge(a, b) => {
						ppf.write("merge (")?;
						a.fmt(ppf)?;
						ppf.write(", ")?;
						b.fmt(ppf)?;
						ppf.write(")")
					}
				}
			}
			Expr::Print(s, next) => {
				ppf.write("print ")?;
				ppf.write_str(s)?;
				ppf.write(" then ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Expr::Write(x, s, next) => {
				ppf.write("write ")?;
				ppf.write_str(s)?;
				ppf.write(" to ")?;
				x.fmt(ppf)?;
				ppf.write(" then ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Expr::Check(x, e, next) => {
				ppf.write("check ")?;
				ppf.write(ppf.context().id().var_ident(*x).as_str())?;
				ppf.write(" = ")?;
				e.fmt(ppf)?;
				ppf.write(" in ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Expr::Error(e) => e.fmt(ppf),
			Expr::Unreachable => ppf.write("unreachable")
		}
	}
}

impl<T: Namespace> PrettyPrint<T> for Error<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::UnexpectedNode(e) => {
				ppf.write("unexpected-node ")?;
				e.fmt(ppf)
			}
			Self::UnexpectedToken(e) => {
				ppf.write("unexpected-token ")?;
				e.fmt(ppf)
			}
		}
	}
}