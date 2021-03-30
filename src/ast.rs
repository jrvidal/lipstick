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
    Array(Box<Type>, String),
}

lalrpop_mod!(pub grammar);
