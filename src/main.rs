#[macro_use]
extern crate lalrpop_util;

use std::fmt::Write;

#[derive(Debug)]
pub struct Program(Vec<Item>);

#[derive(Debug)]
pub enum Item {
    Use(bool, String),
    Function(Signature, String, Block),
    StructDecl(String, Vec<(String, Type)>),
}

#[derive(Debug)]
pub struct Signature {
    args: Vec<(String, Type)>,
    ret: Option<Type>,
}

#[derive(Debug)]
pub struct Block {
    statements: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Declaration(String, Type, Option<Expr>),
    Assignment(String, Expr),
    Return(Option<Expr>),
    Expr(Expr),
    Block(Block),
    If(IfStmt),
    // Switch,
    // While,
    // DoWhile,
    // For,
    // Goto,
    // Continue,
    // Break,
}

#[derive(Debug)]
pub struct IfStmt {
    test: Expr,
    then: Block,
    alts: Vec<(Expr, Block)>,
    tail: Option<Block>,
}

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    String(String),
    Boolean(bool),
    Variable(String),
    StructInit(String, Vec<(String, Expr)>),
    Paren(Box<Expr>),
    Ref(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Cast(Box<Expr>, Type),
}

#[derive(Debug)]
pub enum Type {
    Path(String),
    Ref(Box<Type>),
    Fun(Vec<Type>, Option<Box<Type>>),
}

lalrpop_mod!(pub ast);

macro_rules! writeln {
  ($dst:expr, $($arg:tt)*) => {
    std::writeln!($dst, $($arg)*).unwrap()
  };
}

macro_rules! write {
  ($dst:expr, $($arg:tt)*) => {
    std::write!($dst, $($arg)*).unwrap()
  };
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file_name = std::env::args().skip(1).next().expect("No file given");
    let source = std::fs::read_to_string(&file_name).expect("Unable to read file");
    let program = ast::ProgramParser::new()
        .parse(&source)
        .map_err(|e| format!("{:?}", e))?;
    println!("{}", generate(program.0));
    Ok(())
}

#[test]
fn testing() {
    let result = ast::ProgramParser::new().parse(
        r#"
    struct Foo {
      x: u32,
      y: &A
    }


    fn foobar(x: u32) -> void {
      let x: int = 7;
      x = y;
      let z: Foo = Foo { y: 7 };
      return _z;
    }
  "#,
    );

    panic!("{:?}", result);
}

fn generate(items: Vec<Item>) -> String {
    let mut ret = String::new();

    for item in &items {
        let (root, path) = match item {
            Item::Use(root, path) => (root, path),
            _ => continue,
        };

        write!(ret, "#include ");

        if *root {
            writeln!(ret, "<{}.h>", path);
        } else {
            writeln!(ret, "\"{}\".h", path);
        }
    }

    writeln!(ret, "");
    ret.push_str(&prelude());
    writeln!(ret, "");

    for item in &items {
        match item {
            Item::Use { .. } => continue,
            Item::Function(sig, name, block) => generate_fun(&mut ret, sig, name, block),
            Item::StructDecl(name, fields) => {
                writeln!(ret, "typedef struct {} {{", name);
                for (field_name, ty) in fields {
                    writeln!(ret, "{} {};", generate_ty(ty), field_name);
                }
                writeln!(ret, "}} {};", name);
            }
        }
        writeln!(ret, "");
    }

    ret
}

fn prelude() -> String {
    let mut ret = "#include <stdint.h>\n".to_string();

    for (rust, c) in &[
        ("u32", "uint32_t"),
        ("i32", "int32_t"),
        ("u8", "uint8_t"),
        ("i8", "int8_t"),
        ("usize", "uintptr_t"),
        ("isize", "intptr_t"),
    ] {
        writeln!(ret, "#ifndef {}", rust);
        writeln!(ret, "#define {} {}", rust, c);
        writeln!(ret, "#endif");
    }

    ret
}

fn generate_fun(ret: &mut String, sig: &Signature, name: &str, block: &Block) {
    let Signature { args, ret: ret_ty } = sig;

    match ret_ty {
        Some(ty) => write!(ret, "{}", generate_ty(ty)),
        None => ret.push_str("void"),
    }

    ret.push_str(" ");
    ret.push_str(name);
    ret.push_str("(");

    for (arg_name, arg_ty) in args {
        ret.push_str(&generate_ty(arg_ty));
        ret.push_str(" ");
        ret.push_str(arg_name);
    }

    ret.push_str(") {\n");

    for stmt in &block.statements {
        generate_stmt(ret, stmt);
    }

    ret.push_str("}");
}

fn generate_stmt(ret: &mut String, stmt: &Stmt) {
    match stmt {
        Stmt::Declaration(name, ty, expr) => {
            write!(ret, "{} {}", generate_ty(ty), name);
            if let Some(expr) = expr {
                ret.push_str(" = ");
                generate_expr(ret, expr);
            }
            writeln!(ret, ";");
        }
        Stmt::Expr(expr) => {
            generate_expr(ret, expr);
            writeln!(ret, ";");
        }
        Stmt::Assignment(name, expr) => {
            write!(ret, "{} = ", name);
            generate_expr(ret, expr);
            writeln!(ret, ";");
        }
        Stmt::Return(expr) => {
            write!(ret, "return{}", if expr.is_some() { " " } else { "" });
            if let Some(expr) = expr {
                generate_expr(ret, expr);
            }
            writeln!(ret, ";");
        }
        Stmt::Block(block) => {
            ret.push_str("{");
            for stmt in &block.statements {
                generate_stmt(ret, stmt);
            }
            ret.push_str("}\n");
        }
        Stmt::If(if_stmt) => todo!(),
    }
}

fn generate_expr(ret: &mut String, expr: &Expr) {
    match expr {
        Expr::Integer(n) => {
            write!(ret, "{}", n);
        }
        Expr::Variable(var) => {
            ret.push_str(var);
        }
        Expr::Boolean(val) => ret.push_str(if *val { "1" } else { "0" }),
        Expr::Call(fun, args) => {
            ret.push_str("(");
            generate_expr(ret, fun);
            ret.push_str(")(");

            args.iter().for_each(|arg| {
                generate_expr(ret, arg);
                ret.push_str(",");
            });

            if !args.is_empty() {
                let _ = ret.pop();
            }

            ret.push_str(")");
        }
        Expr::Paren(expr) => {
            ret.push_str("(");
            generate_expr(ret, expr);
            ret.push_str(")");
        }
        Expr::Ref(expr) => {
            ret.push_str("&");
            generate_expr(ret, expr);
        }
        Expr::String(s) => {
            write!(ret, "{}", s);
        }
        Expr::Cast(expr, ty) => {
            write!(ret, "({}) ", generate_ty(ty));
            generate_expr(ret, expr);
        }
        Expr::StructInit(_name, fields) => {
            ret.push_str("{");
            for (name, val) in fields {
                write!(ret, ".{} = ", name);
                generate_expr(ret, val);
                ret.push_str(",");
            }
            ret.push_str("}");
        }
    }
}

fn generate_ty(ty: &Type) -> String {
    match ty {
        Type::Path(path) => path.to_string(),
        Type::Ref(ty) => format!("{}*", generate_ty(ty)),
        Type::Fun { .. } => todo!(),
    }
}
