use std::fmt;
use crate::{
	Namespace,
	Expr,
	expr::{
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

impl<T: Namespace> PrettyPrint<T> for Expr<T> {
	fn fmt(&self, ppf: &mut PrettyPrinter<T>) -> fmt::Result {
		match self {
			Self::Literal(c) => c.fmt(ppf),
			Self::Get(x) => ppf.write_var_id(*x),
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
			Self::Ref(x) => {
				ppf.write("&")?;
				ppf.write_var_id(*x)
			}
			Self::RefField(x, index) => {
				ppf.write("&")?;
				ppf.write_var_id(*x)?;
				ppf.write(".")?;
				ppf.write_u32(*index)
			}
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
			Self::If(condition, then_branch, else_branch) => {
				ppf.write("if ")?;
				condition.fmt(ppf)?;
				ppf.write(" {")?;
				ppf.begin()?;
				then_branch.fmt(ppf)?;
				ppf.end()?;
				ppf.write("} else {")?;
				ppf.begin()?;
				else_branch.fmt(ppf)?;
				ppf.end()?;
				ppf.write("}")?;
				ppf.sep()
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
			Self::MatchRef { expr, cases } => {
				ppf.write("match-ref ")?;
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
			Self::Call(index, args) => {
				ppf.write("call ")?;
				let f = ppf.context().function(*index).unwrap();
				ppf.write_function_id(f.id())?;
				ppf.write("(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(", ")?;
					}
					a.fmt(ppf)?;
				}
				ppf.write(")")
			}
			Self::Return(args) => {
				ppf.write("return(")?;
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
				for (i, (a, mutable)) in args.iter().enumerate() {
					if i > 0 {
						ppf.write(", ")?;
					}
					if *mutable {
						ppf.write("mut ")?;
					}
					ppf.write_var_id(*a)?;
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
					ppf.write_var_id(*a)?;
				}
				ppf.write(")")
			}
			Self::Lexer(lexer, e) => {
				ppf.write("lexer ")?;
				ppf.write_var_id(*lexer)?;
				ppf.write(" ")?;
				match e {
					LexerExpr::Peek => ppf.write("peek"),
					LexerExpr::Span => ppf.write("span"),
					LexerExpr::IsEmpty => ppf.write("empty?"),
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
				ppf.write_var_id(*stream)?;
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
				ppf.write_var_id(*stack)?;
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
				ppf.write_var_id(*x)?;
				ppf.write(" then ")?;
				ppf.sep()?;
				next.fmt(ppf)
			}
			Expr::DebugFormat(x, e, next) => {
				ppf.write("debug-format ")?;
				e.fmt(ppf)?;
				ppf.write(" to ")?;
				ppf.write_var_id(*x)?;
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