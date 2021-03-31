use crate::ast::{
    visit::{self, Visit},
    Expr, Program, Stmt,
};

struct Visitor {
    method_call: bool,
    untyped_declaration: bool,
}

impl<'a> Visit<'a> for Visitor {
    fn visit_expr(&mut self, i: &'a Expr) {
        if self.method_call {
            return;
        }

        if let Expr::MethodCall = i {
            self.method_call = true;
        } else {
            visit::visit_expr(self, i);
        }
    }

    fn visit_statement(&mut self, i: &'a Stmt) {
        if self.untyped_declaration {
            return;
        }

        if let Stmt::UntypedDeclaration = i {
            self.untyped_declaration = true;
        } else {
            visit::visit_statement(self, i);
        }
    }
}

pub fn check(program: &Program) -> Result<(), Box<dyn std::error::Error>> {
    let mut visitor = Visitor {
        method_call: false,
        untyped_declaration: false,
    };
    visitor.visit_program(program);

    if visitor.method_call {
        Err("Unexpected method call")?
    }

    Ok(())
}
