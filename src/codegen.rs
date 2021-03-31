use crate::ast;
use std::fmt::{self, Display};

pub fn transform(program: ast::Program) -> Program {
    let mut decls = vec![];

    for item in &program.0 {
        match item {
            ast::Item::Use(root, header) => {
                decls.push(Declaration::Include(*root, header.clone()));
            }
            _ => continue,
        }
    }

    decls.extend(prelude());

    for item in program.0 {
        match item {
            ast::Item::Use { .. } => continue,
            ast::Item::Function(sig, name, block) => {
                decls.push(Declaration::Function((name, sig).into(), block.into()));
            }
            ast::Item::StructDecl(name, fields) => {
                decls.push(Declaration::StructTypedef(name.clone()));
                decls.push(Declaration::Struct(
                    name,
                    fields.into_iter().map(From::from).collect(),
                ))
            }
        }
    }

    Program(decls)
}

#[derive(Debug)]
pub struct Program(pub Vec<Declaration>);

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for decl in &self.0 {
            decl.fmt(f)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function(Signature, Block),
    Include(bool, String),
    StructTypedef(String),
    Struct(String, Vec<Variable>),
    Ifndef(String, String),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::Function(sig, block) => {
                sig.fmt(f)?;
                block.fmt(f)?;
            }
            Declaration::Include(root, header) => {
                f.write_str("#include ")?;
                if *root {
                    writeln!(f, "<{}.h>", header)?;
                } else {
                    writeln!(f, "\"{}.h\"", header)?;
                }
            }
            Declaration::StructTypedef(name) => {
                writeln!(f, "typedef struct {name} {name};", name = name)?;
            }
            Declaration::Struct(name, fields) => {
                writeln!(f, "struct {} {{", name)?;
                for Variable(ty, decl) in fields {
                    writeln!(f, "  {ty} {decl};", ty = ty, decl = decl)?;
                }
                f.write_str("};\n")?;
            }
            Declaration::Ifndef(name, value) => {
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
    ret: Type,
    name: String,
    args: Vec<(String, Declarator)>,
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

impl From<(String, ast::Signature)> for Signature {
    fn from((name, sig): (String, ast::Signature)) -> Self {
        let ret = sig.ret.map(From::from).unwrap_or_else(|| Type {
            ident: "void".to_string(),
            pointer: 0,
        });

        let args: Vec<_> = sig
            .args
            .into_iter()
            .map(Variable::from)
            .map(|var| (var.0, var.1))
            .collect();

        Signature { name, ret, args }
    }
}

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>);

impl From<ast::Block> for Block {
    fn from(block: ast::Block) -> Self {
        Block(block.statements.into_iter().map(From::from).collect())
    }
}

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
    Declaration(Variable, Option<Expr>),
    Assignment(Expr, Expr),
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
    Labeled(String, Box<Stmt>),
    Break(Option<String>),
    Continue(Option<String>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Declaration(Variable(ty, decl), value) => {
                write!(f, "{} {}", ty, decl)?;
                if let Some(value) = value {
                    write!(f, " = {}", value)?;
                }
                f.write_str(";\n")
            }
            Stmt::Assignment(ident, expr) => writeln!(f, "{} = {};", ident, expr),
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
        }
    }
}

impl From<ast::Stmt> for Stmt {
    fn from(stmt: ast::Stmt) -> Self {
        match stmt {
            ast::Stmt::Declaration(name, ty, expr) => {
                Stmt::Declaration((name, ty).into(), expr.map(From::from))
            }
            ast::Stmt::Assignment(place, expr) => Stmt::Assignment(place.into(), expr.into()),
            ast::Stmt::Return(expr) => Stmt::Return(expr.map(From::from)),
            ast::Stmt::Expr(expr) => Stmt::Expr(expr.into()),
            ast::Stmt::Block(block) => Stmt::Block(block.into()),
            ast::Stmt::If(if_stmt) => Stmt::If {
                test: if_stmt.test.into(),
                then: if_stmt.then.into(),
                alts: if_stmt
                    .alts
                    .into_iter()
                    .map(|(expr, block)| (expr.into(), block.into()))
                    .collect(),
                tail: if_stmt.tail.map(From::from),
            },
            ast::Stmt::While(test, block) => Stmt::While {
                test: test.into(),
                block: block.into(),
            },
            ast::Stmt::Labeled(label, stmt) => Stmt::Labeled(label, Box::new((*stmt).into())),
            ast::Stmt::Break(label) => Stmt::Break(label),
            ast::Stmt::Continue(label) => Stmt::Continue(label),
            ast::Stmt::UntypedDeclaration => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    String(String),
    Boolean(bool),
    Variable(String),
    Ref(Box<Expr>),
    Initializer(Vec<(String, Expr)>),
    Call(Box<Expr>, Vec<Expr>),
    Deref(Box<Expr>),
    Field(Box<Expr>, String),
}

impl From<ast::Expr> for Expr {
    fn from(expr: ast::Expr) -> Self {
        match expr {
            ast::Expr::Integer(n) => Expr::Integer(n),
            ast::Expr::String(s) => Expr::String(s),
            ast::Expr::Boolean(b) => Expr::Boolean(b),
            ast::Expr::Variable(v) => Expr::Variable(v),
            ast::Expr::StructInit(_, fields) => Expr::Initializer(
                fields
                    .into_iter()
                    .map(|(name, expr)| (name, expr.into()))
                    .collect(),
            ),
            ast::Expr::Ref(expr) => Expr::Ref(Box::new((*expr).into())),
            ast::Expr::Call(fun, args) => Expr::Call(
                Box::new((*fun).into()),
                args.into_iter().map(From::from).collect(),
            ),
            ast::Expr::Deref(expr) => Expr::Deref(Box::new((*expr).into())),
            ast::Expr::Field(expr, field) => Expr::Field(Box::new((*expr).into()), field),
            ast::Expr::Cast(_, _) => todo!(),
            ast::Expr::Index(_, _) => todo!(),
            ast::Expr::MethodCall => unreachable!(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(n) => n.fmt(f),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Boolean(b) => b.fmt(f),
            Expr::Variable(ident) => ident.fmt(f),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::Initializer(fields) => {
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
            Expr::Field(expr, field) => {
                // TODO: this should be handled in codegen proper
                match &**expr {
                    Expr::Variable(ident) => ident.fmt(f)?,
                    _ => write!(f, "({})", expr)?,
                }
                f.write_str(".")?;
                f.write_str(field)
            }
        }
    }
}

#[derive(Debug)]
pub struct Type {
    ident: String,
    pointer: u8,
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

impl From<ast::Type> for Type {
    fn from(ty: ast::Type) -> Self {
        match ty {
            ast::Type::Ident(ident) => Type { ident, pointer: 0 },
            ast::Type::Ref(ty) => {
                let mut ret: Self = (*ty).into();
                ret.pointer += 1;
                ret
            }
            ast::Type::Fun(_, _) => todo!(),
            ast::Type::Array(_, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct Variable(pub String, pub Declarator);

impl From<(String, Declarator)> for Variable {
    fn from((ident, decl): (String, Declarator)) -> Self {
        Variable(ident, decl)
    }
}

#[derive(Debug)]
pub struct Declarator {
    pointer: u8,
    decl: DirectDeclarator,
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
        let Declarator { pointer, decl } = self;

        for _ in 0..*pointer {
            f.write_str("*")?;
        }

        match decl {
            DirectDeclarator::Ident(ident) => f.write_str(ident)?,
            DirectDeclarator::Paren(decl) => {
                write!(f, "({})", decl)?;
            }
            DirectDeclarator::Array(_, _) => todo!(),
            DirectDeclarator::Function => todo!(),
        }

        Ok(())
    }
}

impl From<(String, ast::Type)> for Variable {
    fn from((ident, ty): (String, ast::Type)) -> Self {
        match ty {
            ast::Type::Ident(path) => {
                let decl = {
                    Declarator {
                        pointer: 0,
                        decl: DirectDeclarator::Ident(ident),
                    }
                };
                (path, decl).into()
            }
            ast::Type::Ref(ty) => {
                let mut variable: Variable = (ident, *ty).into();
                variable.1.pointer += 1;
                variable
            }
            ast::Type::Fun(_, _) => todo!(),
            ast::Type::Array(_, _) => todo!(),
        }
    }
}

fn prelude() -> Vec<Declaration> {
    let mut ret = vec![
        Declaration::Include(true, "stdint".to_string()),
        Declaration::Include(true, "stdbool".to_string()),
    ];

    for (rust, c) in &[
        ("u32", "uint32_t"),
        ("i32", "int32_t"),
        ("u8", "uint8_t"),
        ("i8", "int8_t"),
        ("usize", "uintptr_t"),
        ("isize", "intptr_t"),
    ] {
        ret.push(Declaration::Ifndef(rust.to_string(), c.to_string()));
    }
    ret
}
