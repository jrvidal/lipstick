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
    Loop(Block),
    For {
        var: String,
        range: (Expr, Expr),
        inclusive: bool,
        block: Block,
    },
    Match {
        value: Expr,
        arms: Vec<(Expr, Block)>,
        catch_all: Option<Block>,
    }, // Goto,
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
    Float(f64),
    String(String),
    Boolean(bool),
    Variable(String),
    Char(char),
    // TODO: bytestrings, byte literals
    StructInit(String, Vec<(String, Expr)>),
    Ref(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Cast(Box<Expr>, Type),
    Field(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),
    Operator(Box<Expr>, BinaryOp, Box<Expr>),
    Not(Box<Expr>),
    MethodCall,
    // TODO: if ternary
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    Xor,
    LShift,
    RShift,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
    Equal,
    NotEqual,
    Gt,
    Lt,
    Geq,
    Leq,
    And,
    Or,
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
            | Expr::Float(_)
            | Expr::Cast(_, _)
            | Expr::Char(_)
            | Expr::Operator(_, _, _)
            | Expr::Not(_)
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
                visitor.visit_item(item);
            }
        };
        visit_item, Item, {
                match node {
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
                visitor.visit_stmt(stmt);
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
        visit_stmt, Stmt, {
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
                Stmt::Block(block)
                | Stmt::Loop(block) => {
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
                    visitor.visit_stmt(stmt);
                }
                Stmt::Match { value, arms, catch_all } => {
                    visitor.visit_expr(value);
                    for (expr, block) in arms {
                        visitor.visit_expr(expr);
                        visitor.visit_block(block);
                    }
                    catch_all.iter().for_each(|block| visitor.visit_block(block));
                }
                Stmt::For {
                    var: _var,
                    range,
                    block,
                    inclusive: _inclusive
                } => {
                    visitor.visit_expr(&range.0);
                    visitor.visit_expr(&range.1);
                    visitor.visit_block(block);
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
                Expr::Ref(expr) | Expr::Deref(expr) | Expr::Not(expr) => {
                    visitor.visit_expr(expr);
                }
                Expr::Operator(left, _, right) => {
                    visitor.visit_expr(left);
                    visitor.visit_expr(right);
                }
                Expr::Integer(_) |
                Expr::Float(_) |
                Expr::Char(_) |
                Expr::String(_) |
                Expr::Boolean(_) |
                Expr::Variable(_) |
                Expr::MethodCall => {}
            }
        };
    ]];
}
