#[derive(Debug)]
pub struct Program(pub Vec<Item>);

#[derive(Debug)]
pub enum Item {
    Use(bool, String),
    Function(Signature, String, Block),
    StructDecl(String, Vec<(String, Type)>),
}

#[derive(Debug)]
pub struct Signature {
    pub args: Vec<(String, Type)>,
    pub ret: Option<Type>,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Declaration(String, Type, Option<Expr>),
    UntypedDeclaration,
    Assignment(Expr, Expr),
    Return(Option<Expr>),
    Expr(Expr),
    Block(Block),
    If(IfStmt),
    While(Expr, Block),
    Labeled(String, Box<Stmt>),
    Break(Option<String>),
    Continue(Option<String>),
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
    pub test: Expr,
    pub then: Block,
    pub alts: Vec<(Expr, Block)>,
    pub tail: Option<Block>,
}

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    String(String),
    Boolean(bool),
    Variable(String),
    StructInit(String, Vec<(String, Expr)>),
    Ref(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Cast(Box<Expr>, Type),
    Field(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),
    MethodCall,
}

impl Expr {
    pub fn is_place(&self) -> bool {
        match self {
            Expr::Integer(_)
            | Expr::String(_)
            | Expr::Boolean(_)
            | Expr::StructInit(_, _)
            | Expr::Ref(_)
            | Expr::MethodCall
            | Expr::Cast(_, _)
            | Expr::Call(_, _) => false,
            Expr::Variable(_) => true,
            Expr::Field(expr, _) | Expr::Index(expr, _) | Expr::Deref(expr) => expr.is_place(),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Ident(String),
    Ref(Box<Type>),
    Fun(Vec<Type>, Option<Box<Type>>),
    Array(Box<Type>, Box<Expr>),
}

lalrpop_mod!(pub grammar);

pub mod visit {
    use super::*;

    macro_rules! generate_visitor {
        ($trait_name:ident, $node:ident, $visitor:ident, [
            $($fn_name:ident, $ty:path, $imp:block ;)*
        ]) => {
            pub trait $trait_name<'a>: Sized {
                $(
                    fn $fn_name(&mut self, i: &'a $ty) {
                        $fn_name(self, i);
                    }
                )*
            }

            $(
                pub fn $fn_name<'a, V: $trait_name<'a>>($visitor: &mut V, $node: &'a $ty) $imp
            )*

        }
    }

    generate_visitor![Visit, node, visitor, [
        visit_program, Program, {
            for item in &node.0 {
                match item {
                    Item::Function(sig, _ident, block) => {
                        visitor.visit_signature(sig);
                        visitor.visit_block(block);
                    },
                    Item::StructDecl(_name, fields) => {
                        for (_name, ty) in fields {
                            visitor.visit_type(ty);
                        }
                    },
                    Item::Use { .. } => {}
                }
            }
        };
        visit_signature, Signature, {
            for (_name, ty) in &node.args {
                visitor.visit_type(ty);
            }
            if let Some(ret) = node.ret.as_ref() {
                visitor.visit_type(ret);
            }
        };
        visit_block, Block, {
            for stmt in &node.statements {
                visitor.visit_statement(stmt);
            }
        };
        visit_type, Type, {
            match node {
                Type::Ident(_) => {},
                Type::Ref(ty) => visitor.visit_type(ty),
                Type::Fun(args, ret) => {
                    for arg in args {
                        visitor.visit_type(arg);
                    }
                    if let Some(ret) = ret {
                        visitor.visit_type(ret);
                    }
                },
                Type::Array(ty, n) => {
                    visitor.visit_type(ty);
                    visitor.visit_expr(n);
                },
            }
        };
        visit_statement, Stmt, {
            match node {
                Stmt::Declaration(_, ty, expr) => {
                    visitor.visit_type(ty);
                    if let Some(expr) = expr {
                        visitor.visit_expr(expr);
                    }
                }
                Stmt::Assignment(place, expr) => {
                    visitor.visit_expr(place);
                    visitor.visit_expr(expr);
                }
                Stmt::Return(None) => {}
                Stmt::Return(Some(expr)) => {
                    visitor.visit_expr(expr);
                }
                Stmt::Expr(expr) => {
                    visitor.visit_expr(expr);
                }
                Stmt::Block(block) => {
                    visitor.visit_block(block);
                }
                Stmt::If(if_stmt) => {
                    visitor.visit_expr(&if_stmt.test);
                    visitor.visit_block(&if_stmt.then);
                    for (alt_test, alt_block) in &if_stmt.alts {
                        visitor.visit_expr(alt_test);
                        visitor.visit_block(alt_block);
                    }
                    if let Some(tail) = if_stmt.tail.as_ref() {
                        visitor.visit_block(tail);
                    }

                }
                Stmt::While(test, block) => {
                    visitor.visit_expr(test);
                    visitor.visit_block(block);
                }
                Stmt::Labeled(_label, stmt) => {
                    visitor.visit_statement(stmt);
                }
                Stmt::Break(_) |
                Stmt::Continue(_) |
                Stmt::UntypedDeclaration => {}
            }
        };
        visit_expr, Expr, {
            match node {
                Expr::StructInit(_, fields) => {
                    for (_name, value) in fields {
                        visitor.visit_expr(value);
                    }
                }
                Expr::Ref(expr) => visitor.visit_expr(expr),
                Expr::Call(fun, args) => {
                    visitor.visit_expr(fun);
                    for arg in args {
                        visitor.visit_expr(arg);
                    }
                }
                Expr::Cast(expr, ty) => {
                    visitor.visit_expr(expr);
                    visitor.visit_type(ty);
                }
                Expr::Field(this, _) => {
                    visitor.visit_expr(this);
                }
                Expr::Index(arr, index) => {
                    visitor.visit_expr(arr);
                    visitor.visit_expr(index);
                }
                Expr::Deref(expr) => {
                    visitor.visit_expr(expr);
                }
                Expr::Integer(_) |
                Expr::String(_) |
                Expr::Boolean(_) |
                Expr::Variable(_) |
                Expr::MethodCall => {}
            }
        };
    ]];
}
