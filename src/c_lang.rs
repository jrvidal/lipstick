use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Program(pub Vec<Item>);

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for decl in &self.0 {
            decl.fmt(f)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Item {
    Function(Signature, Block),
    Include(bool, String),
    StructTypedef(String),
    Struct(String, Vec<Declaration>),
    UnionTypedef(String),
    Union(String, Vec<Declaration>),
    Ifndef(String, String),
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Function(sig, block) => {
                sig.fmt(f)?;
                block.fmt(f)?;
            }
            Item::Include(root, header) => {
                f.write_str("#include ")?;
                if *root {
                    writeln!(f, "<{}.h>", header)?;
                } else {
                    writeln!(f, "\"{}.h\"", header)?;
                }
            }
            Item::StructTypedef(name) => {
                writeln!(f, "typedef struct {name} {name};", name = name)?;
            }
            Item::Struct(name, fields) => {
                writeln!(f, "struct {} {{", name)?;
                for Declaration(ty, decl) in fields {
                    writeln!(f, "  {ty} {decl};", ty = ty, decl = decl)?;
                }
                f.write_str("};\n")?;
            }
            Item::UnionTypedef(name) => {
                writeln!(f, "typedef union {name} {name};", name = name)?;
            }
            Item::Union(name, fields) => {
                writeln!(f, "union {} {{", name)?;
                for Declaration(ty, decl) in fields {
                    writeln!(f, "  {ty} {decl};", ty = ty, decl = decl)?;
                }
                f.write_str("};\n")?;
            }
            Item::Ifndef(name, value) => {
                writeln!(f, "#ifndef {}", name)?;
                writeln!(f, "#define {} {}", name, value)?;
                f.write_str("#endif\n")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Signature {
    pub ret: Type,
    pub name: String,
    pub args: Vec<(String, Declarator)>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ret.fmt(f)?;
        f.write_str(" ")?;
        f.write_str(&self.name)?;
        f.write_str("(")?;
        let len = self.args.len();
        for (i, (ty, decl)) in self.args.iter().enumerate() {
            f.write_str(ty)?;
            f.write_str(" ")?;
            decl.fmt(f)?;
            if i < len - 1 {
                f.write_str(", ")?;
            }
        }
        f.write_str(") ")
    }
}

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>);

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{\n")?;
        for stmt in &self.0 {
            f.write_str("  ")?;
            stmt.fmt(f)?;
        }
        f.write_str("}\n")
    }
}

#[derive(Debug)]
pub enum Stmt {
    Declaration(Declaration, Option<Expr>),
    Assignment(Expr, Expr),
    AssignmentOp(Expr, BinOp, Expr),
    Block(Block),
    Return(Option<Expr>),
    Expr(Expr),
    If {
        test: Expr,
        then: Block,
        alts: Vec<(Expr, Block)>,
        tail: Option<Block>,
    },
    While {
        test: Expr,
        block: Block,
    },
    For {
        ident: String,
        inclusive: bool,
        range: (Expr, Expr),
        block: Block,
    },
    Labeled(String, Box<Stmt>),
    Break(Option<String>),
    Continue(Option<String>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Declaration(Declaration(ty, decl), value) => {
                write!(f, "{} {}", ty, decl)?;
                if let Some(value) = value {
                    write!(f, " = {}", value)?;
                }
                f.write_str(";\n")
            }
            Stmt::Assignment(left, right) => writeln!(f, "{} = {};", left, right),
            Stmt::AssignmentOp(left, op, right) => writeln!(f, "{} {} {};", left, op, right),
            Stmt::Block(block) => block.fmt(f),
            Stmt::Return(expr) => {
                f.write_str("return")?;
                if let Some(expr) = expr {
                    f.write_str(" ")?;
                    expr.fmt(f)?;
                }
                writeln!(f, ";")
            }
            Stmt::Expr(expr) => writeln!(f, "{};", expr),
            Stmt::If {
                test,
                then,
                alts,
                tail,
            } => {
                f.write_str("if (")?;
                test.fmt(f)?;
                f.write_str(") ")?;
                then.fmt(f)?;
                for (expr, block) in alts {
                    f.write_str(" else if (")?;
                    expr.fmt(f)?;
                    f.write_str(") ")?;
                    block.fmt(f)?;
                }
                if let Some(tail) = tail {
                    f.write_str(" else ")?;
                    tail.fmt(f)?;
                }
                Ok(())
            }
            Stmt::While { test, block } => {
                f.write_str("while (")?;
                test.fmt(f)?;
                f.write_str(") ")?;
                block.fmt(f)
            }
            Stmt::Labeled(label, stmt) => {
                f.write_str(&label)?;
                f.write_str(": ")?;
                stmt.fmt(f)
            }
            Stmt::Break(label) => {
                f.write_str("break")?;
                if let Some(label) = label {
                    write!(f, " {}", label)?;
                }
                f.write_str(";\n")
            }
            Stmt::Continue(label) => {
                f.write_str("continue")?;
                if let Some(label) = label {
                    write!(f, " {}", label)?;
                }
                f.write_str(";\n")
            }
            Stmt::For {
                ident,
                inclusive,
                range,
                block,
            } => {
                let op = if *inclusive { "<=" } else { "<" };
                writeln!(
                    f,
                    "for (usize {ident} = {start}; {ident} {op} {end}; {ident}++) ",
                    ident = ident,
                    op = op,
                    start = range.0,
                    end = range.1
                )?;
                block.fmt(f)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i64),
    String(String),
    Boolean(bool),
    Char(char),
    Float(f64),
    Variable(String),
    Deref(Box<Expr>),
    Ref(Box<Expr>),
    Not(Box<Expr>),
    Neg(Box<Expr>),
    Field(Box<Expr>, String),
    Cast(Type, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    StructInit(Vec<(String, Expr)>),
    ArrayInit(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Paren(Box<Expr>),
}

impl Expr {
    pub fn fallback() -> Self {
        Expr::Integer(0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    RemEq,
    BitXorEq,
    BitAndEq,
    BitOrEq,
    ShlEq,
    ShrEq,
}

impl<'a> From<&'a syn::BinOp> for BinOp {
    fn from(op: &'a syn::BinOp) -> Self {
        match op {
            syn::BinOp::Add(_) => BinOp::Add,
            syn::BinOp::Sub(_) => BinOp::Sub,
            syn::BinOp::Mul(_) => BinOp::Mul,
            syn::BinOp::Div(_) => BinOp::Div,
            syn::BinOp::Rem(_) => BinOp::Rem,
            syn::BinOp::And(_) => BinOp::And,
            syn::BinOp::Or(_) => BinOp::Or,
            syn::BinOp::BitXor(_) => BinOp::BitXor,
            syn::BinOp::BitAnd(_) => BinOp::BitAnd,
            syn::BinOp::BitOr(_) => BinOp::BitOr,
            syn::BinOp::Shl(_) => BinOp::Shl,
            syn::BinOp::Shr(_) => BinOp::Shr,
            syn::BinOp::Eq(_) => BinOp::Eq,
            syn::BinOp::Lt(_) => BinOp::Lt,
            syn::BinOp::Le(_) => BinOp::Le,
            syn::BinOp::Ne(_) => BinOp::Ne,
            syn::BinOp::Ge(_) => BinOp::Ge,
            syn::BinOp::Gt(_) => BinOp::Gt,
            syn::BinOp::AddEq(_) => BinOp::AddEq,
            syn::BinOp::SubEq(_) => BinOp::SubEq,
            syn::BinOp::MulEq(_) => BinOp::MulEq,
            syn::BinOp::DivEq(_) => BinOp::DivEq,
            syn::BinOp::RemEq(_) => BinOp::RemEq,
            syn::BinOp::BitXorEq(_) => BinOp::BitXorEq,
            syn::BinOp::BitAndEq(_) => BinOp::BitAndEq,
            syn::BinOp::BitOrEq(_) => BinOp::BitOrEq,
            syn::BinOp::ShlEq(_) => BinOp::ShlEq,
            syn::BinOp::ShrEq(_) => BinOp::ShrEq,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::BitXor => "^",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Eq => "==",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Ne => "!=",
            BinOp::Ge => ">",
            BinOp::Gt => ">=",
            BinOp::AddEq => "+=",
            BinOp::SubEq => "-=",
            BinOp::MulEq => "*=",
            BinOp::DivEq => "/=",
            BinOp::RemEq => "%=",
            BinOp::BitXorEq => "^=",
            BinOp::BitAndEq => "&=",
            BinOp::BitOrEq => "|=",
            BinOp::ShlEq => "<<=",
            BinOp::ShrEq => ">>=",
        };

        f.write_str(s)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(n) => n.fmt(f),
            Expr::String(s) => write!(f, "{:?}", s),
            Expr::Boolean(b) => b.fmt(f),
            Expr::Char(c) => write!(f, "'{}'", c),
            Expr::Float(x) => x.fmt(f),
            Expr::Variable(ident) => ident.fmt(f),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::ArrayInit(elems) => {
                f.write_str("{")?;
                for elem in elems {
                    write!(f, "{},", elem)?;
                }
                f.write_str("}")
            }
            Expr::StructInit(fields) => {
                f.write_str("{")?;
                for (name, expr) in fields {
                    write!(f, ".{} = {},", name, expr)?;
                }
                f.write_str("}")
            }
            Expr::Call(fun, args) => {
                fun.fmt(f)?;
                f.write_str("(")?;

                let len = args.len();
                for (i, arg) in args.iter().enumerate() {
                    arg.fmt(f)?;
                    if i < len - 1 {
                        f.write_str(", ")?;
                    }
                }

                f.write_str(")")
            }
            Expr::Deref(expr) => {
                f.write_str("*")?;
                expr.fmt(f)
            }
            Expr::Not(expr) => {
                f.write_str("!")?;
                expr.fmt(f)
            }
            Expr::Neg(expr) => {
                f.write_str("~")?;
                expr.fmt(f)
            }
            Expr::Field(expr, field) => {
                // TODO: this should be handled in codegen proper
                match &**expr {
                    Expr::Variable(ident) => ident.fmt(f)?,
                    _ => write!(f, "({})", expr)?,
                }
                f.write_str(".")?;
                f.write_str(field)
            }
            Expr::Paren(expr) => write!(f, "({})", expr),
            Expr::Ternary(test, then, else_) => write!(f, "{} ? {} : {}", test, then, else_),
            Expr::Cast(ty, expr) => {
                write!(f, "({}) {}", ty, expr)
            }
            Expr::Binary(right, op, left) => {
                // TODO: precedence
                write!(f, "{} {} {}", right, op, left)
            }
            Expr::Index(expr, index) => {
                write!(f, "{}[{}]", expr, index)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ident: String,
    pub pointer: u8,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.ident)?;
        for _ in 0..self.pointer {
            f.write_str("*")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Declaration(pub String, pub Declarator);

impl Declaration {
    pub fn fallback() -> Self {
        Declaration("u32".to_string(), Declarator::fallback())
    }
}

impl From<(String, Declarator)> for Declaration {
    fn from((ident, decl): (String, Declarator)) -> Self {
        Declaration(ident, decl)
    }
}

#[derive(Debug)]
pub struct Declarator {
    pub pointer: u8,
    pub ddecl: DirectDeclarator,
}

impl Declarator {
    pub fn fallback() -> Self {
        Declarator {
            pointer: 0,
            ddecl: DirectDeclarator::Ident("fallback__".to_string()),
        }
    }
}

#[derive(Debug)]
pub enum DirectDeclarator {
    Ident(String),
    Paren(Box<Declarator>),
    Array(Box<Self>, Expr),
    Function,
}

impl Display for Declarator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Declarator { pointer, ddecl } = self;

        for _ in 0..*pointer {
            f.write_str("*")?;
        }

        write!(f, "{}", ddecl)
    }
}

impl Display for DirectDeclarator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DirectDeclarator::Ident(ident) => f.write_str(ident)?,
            DirectDeclarator::Paren(decl) => {
                write!(f, "({})", decl)?;
            }
            DirectDeclarator::Array(decl, len) => {
                write!(f, "{}[{}]", decl, len)?;
            }
            DirectDeclarator::Function => todo!(),
        }

        Ok(())
    }
}
