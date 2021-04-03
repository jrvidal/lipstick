use std::collections::HashSet;

use crate::ast::{
    visit::{self, Visit},
    Expr, Item, Program, Stmt,
};

#[derive(Default)]
struct CollectVisitor<'a> {
    exprs: Vec<&'a Expr>,
    stmts: Vec<&'a Stmt>,
    items: Vec<&'a Item>,
}

impl<'a> Visit<'a> for CollectVisitor<'a> {
    fn visit_item(&mut self, i: &'a Item) {
        self.items.push(i);
        visit::visit_item(self, i);
    }
    fn visit_expr(&mut self, i: &'a Expr) {
        self.exprs.push(i);
        visit::visit_expr(self, i);
    }

    fn visit_stmt(&mut self, i: &'a Stmt) {
        self.stmts.push(i);
        visit::visit_stmt(self, i);
    }
}

#[derive(Default)]
pub struct Context<'a> {
    struct_index: HashSet<&'a str>,
}

pub fn check(program: &Program) -> Result<Context, Box<dyn std::error::Error>> {
    let mut ctx = Context::default();
    let mut visitor = CollectVisitor::default();
    visitor.visit_program(program);

    let method_calls: Vec<_> = visitor
        .exprs
        .iter()
        .copied()
        .filter(|expr| match expr {
            Expr::MethodCall => true,
            _ => false,
        })
        .collect();
    let untyped_decls: Vec<_> = visitor
        .stmts
        .iter()
        .copied()
        .filter(|stmt| match stmt {
            Stmt::UntypedDeclaration => true,
            _ => false,
        })
        .collect();

    if !method_calls.is_empty() {
        Err("Unexpected method call")?
    }

    if !untyped_decls.is_empty() {
        Err("Unexpected untyped declaration")?
    }

    ctx.struct_index = visitor
        .items
        .iter()
        .copied()
        .filter_map(|item| match item {
            Item::Use(_, _) | Item::Function(_, _, _) => None,
            Item::StructDecl(name, _) => Some(&name[..]),
        })
        .collect();

    Ok(ctx)
}
