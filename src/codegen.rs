use proc_macro2::{Span, TokenTree};
use std::fmt::{self, Display};
use syn::spanned::Spanned;

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

#[derive(Debug)]
pub struct CompilationError {
    pub diagnostics: Vec<Error>,
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.diagnostics {
            writeln!(f, "Error: {} ({:?})", error.msg, error.span)?;
        }

        Ok(())
    }
}

impl std::error::Error for CompilationError {}

#[derive(Default)]
struct Context {
    errors: Vec<Error>,
}

impl Context {
    fn transform(mut self, file: &syn::File) -> std::result::Result<Program, CompilationError> {
        let syn::File {
            shebang,
            attrs,
            items,
        } = file;

        self.fail_opt(shebang, "Shebang is not supported");
        self.fail_attrs(attrs);

        let mut decls = vec![];
        decls.extend(prelude());

        for item in items {
            self.transform_item(item, &mut decls);
        }
        if self.errors.is_empty() {
            Ok(Program(decls))
        } else {
            Err(CompilationError {
                diagnostics: self.errors,
            })
        }
    }

    fn transform_item(&mut self, item: &syn::Item, out: &mut Vec<Item>) {
        let mut fail = |msg: &str| self.fail(item, &format!("{} are not supported", msg));
        match item {
            syn::Item::Const(_) => fail("Const items"),
            syn::Item::Enum(_) => fail("Enums"),
            syn::Item::ExternCrate(_) => fail("Extern crates"),
            syn::Item::Fn(item) => self.transform_fn(item, out),
            syn::Item::ForeignMod(_) => fail("Extern modules"),
            syn::Item::Impl(_) => fail("Impl blocks"),
            syn::Item::Macro(syn::ItemMacro {
                attrs,
                ident,
                mac,
                semi_token: _,
            }) => {
                self.fail_attrs(attrs);
                self.fail_opt(ident, "macro_rules-style macros are not supported");
                let syn::Macro {
                    path,
                    bang_token: _,
                    delimiter: _,
                    tokens,
                } = mac;
                let ident = match path.get_ident() {
                    Some(ident) => ident.to_string(),
                    _ => {
                        self.fail(path, "Paths are not supported");
                        return;
                    }
                };

                if ident != "include" {
                    self.fail(item, "Unsupported macro");
                    return;
                }

                const USAGE: &str =
                    "include! can be invoked as `include!(file)` or `include!(<header>)`";

                let tokens: Vec<_> = tokens.clone().into_iter().collect();
                if tokens.len() != 1 && tokens.len() != 3 {
                    self.fail(item, USAGE);
                    return;
                }

                let header_token = if tokens.len() == 1 {
                    &tokens[0]
                } else {
                    &tokens[1]
                };

                let header = match header_token {
                    TokenTree::Ident(ident) => ident.to_string(),
                    _ => {
                        self.fail(item, USAGE);
                        return;
                    }
                };

                let is_char = |tt: &TokenTree, c| match tt {
                    TokenTree::Punct(punct) => punct.as_char() == c,
                    _ => false,
                };

                if tokens.len() == 3 {
                    if is_char(&tokens[0], '<') && is_char(&tokens[2], '>') {
                        // OK
                    } else {
                        self.fail(item, USAGE);
                        return;
                    }
                }

                out.push(Item::Include(tokens.len() == 3, header));
            }
            syn::Item::Macro2(_) => fail("Macros"),
            syn::Item::Mod(_) => fail("Modules"),
            syn::Item::Static(_) => fail("Static items"),
            syn::Item::Struct(syn::ItemStruct {
                attrs,
                vis,
                struct_token: _,
                ident,
                generics,
                fields: item_fields,
                semi_token: _,
            }) => {
                self.fail_attrs(attrs);
                self.fail_vis(vis);
                self.fail_opt(
                    &generics.lt_token.as_ref().map(|_| generics),
                    "Generics are not supported",
                );
                self.fail_opt(
                    &generics.where_clause.as_ref(),
                    "Generics are not supported",
                );

                let mut fields = vec![];

                for syn::Field {
                    attrs,
                    vis,
                    ident: field_ident,
                    colon_token: _,
                    ty,
                } in item_fields
                {
                    self.fail_attrs(attrs);
                    self.fail_vis(vis);
                    if let Some(field_ident) = field_ident {
                        fields.push(
                            self.transform_declarator(field_ident.to_string(), ty)
                                .into(),
                        );
                    } else {
                        self.fail(item_fields, "Tuple structs are not supported");
                    }
                }

                if item_fields.is_empty() {
                    self.fail(item, "Unit structs are not supported");
                }

                out.push(Item::StructTypedef(ident.to_string()));
                out.push(Item::Struct(ident.to_string(), fields));
            }
            syn::Item::Trait(_) => fail("Traits"),
            syn::Item::TraitAlias(_) => fail("Trait aliases"),
            syn::Item::Type(_) => fail("Type aliases"),
            syn::Item::Union(_) => fail("Unions"),
            syn::Item::Use(_) => { /*TODO*/ }
            syn::Item::Verbatim(_) => self.fail(item, "Unexpected tokens"),
            syn::Item::__TestExhaustive(_) => unreachable!(),
        }
    }

    fn transform_fn(&mut self, item: &syn::ItemFn, out: &mut Vec<Item>) {
        let syn::ItemFn {
            attrs,
            vis,
            sig,
            block: item_block,
        } = item;
        self.fail_attrs(attrs);
        self.fail_vis(vis);

        let syn::Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token: _,
            ident,
            generics,
            paren_token: _,
            inputs,
            variadic,
            output,
        } = sig;
        self.fail_opt(constness, "Const functions are not supported");
        self.fail_opt(asyncness, "Async functions are not supported");
        self.fail_opt(unsafety, "Unsafe functions are not supported");
        self.fail_opt(abi, "`extern` functions are not supported");
        self.fail_opt(
            &generics.lt_token.as_ref().map(|_| generics),
            "Generics are not supported",
        );
        self.fail_opt(variadic, "Variadic functions are not supported");

        let ret = match output {
            syn::ReturnType::Default => Type {
                ident: "void".to_string(),
                pointer: 0,
            },
            syn::ReturnType::Type(_, ty) => self.transform_type(ty),
        };

        let mut args = vec![];

        for fn_arg in inputs {
            match fn_arg {
                syn::FnArg::Receiver(_) => self.fail(fn_arg, "Methods are not supported"),
                syn::FnArg::Typed(pat_type) => {
                    args.push(self.transform_variable_declarator(pat_type));
                }
            }
        }

        let signature = Signature {
            ret,
            name: ident.to_string(),
            args,
        };

        let block = self.transform_expr_block(item_block);

        out.push(Item::Function(signature, block));
    }

    fn transform_type(&mut self, ty: &syn::Type) -> Type {
        let mut fail = |msg: &str| self.fail(ty, &format!("{} is not supported", msg));

        match ty {
            syn::Type::Array(_) => { /* TODO: SUPPORT? */ }
            syn::Type::BareFn(_) => { /* TODO: SUPPORT? */ }
            syn::Type::Group(group) => return self.transform_type(&*group.elem),
            syn::Type::ImplTrait(_) => fail("Impl trait"),
            syn::Type::Infer(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Macro(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Never(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Paren(paren) => return self.transform_type(&*paren.elem),
            syn::Type::Path(type_path) => {
                self.fail_opt(
                    &type_path.qself.as_ref().map(|q| &*q.ty),
                    "Explicit self types are not supported",
                );
                if let Some(ident) = type_path.path.get_ident() {
                    return Type {
                        ident: ident.to_string(),
                        pointer: 0,
                    };
                } else {
                    self.fail(type_path, "Paths are not supported");
                }
            }
            syn::Type::Ptr(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Reference(ty_ref) => {
                self.fail_opt(&ty_ref.lifetime, "Explicit lifetimes are not supported");
                self.fail_opt(&ty_ref.mutability, "Mutable references are not supported");
                let mut ret = self.transform_type(&*ty_ref.elem);
                ret.pointer += 1;
                return ret;
            }
            syn::Type::Slice(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::TraitObject(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Tuple(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Verbatim(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::__TestExhaustive(_) => unreachable!(),
        }

        // Catch-all
        Type {
            ident: "void".to_string(),
            pointer: 0,
        }
    }

    fn transform_variable_declarator(&mut self, pat_type: &syn::PatType) -> (String, Declarator) {
        self.fail_attrs(&pat_type.attrs);
        let ident = match &*pat_type.pat {
            syn::Pat::Ident(pat_ident) => {
                self.fail_attrs(&pat_ident.attrs);
                self.fail_opt(&pat_ident.by_ref, "By-ref patterns are not supported");
                self.fail_opt(&pat_ident.mutability, "Mutable patterns are not supported");
                self.fail_opt(
                    &pat_ident.subpat.as_ref().map(|(_, sub)| &*sub),
                    "Subpatterns are not supported",
                );
                pat_ident.ident.to_string()
            }
            _ => {
                self.fail(pat_type, "Patterns are not supported");
                "u32".to_string()
            }
        };
        self.transform_declarator(ident, &*pat_type.ty)
    }

    fn transform_declarator(&mut self, ident: String, ty: &syn::Type) -> (String, Declarator) {
        let mut fail = |msg: &str| self.fail(ty, &format!("{} is not supported", msg));

        match ty {
            syn::Type::Array(_) => { /* TODO: SUPPORT? */ }
            syn::Type::BareFn(_) => { /* TODO: SUPPORT? */ }
            syn::Type::Group(group) => return self.transform_declarator(ident, &*group.elem),
            syn::Type::ImplTrait(_) => fail("Impl trait"),
            syn::Type::Infer(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Macro(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Never(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Paren(paren) => return self.transform_declarator(ident, &*paren.elem),
            syn::Type::Path(type_path) => {
                self.fail_opt(
                    &type_path.qself.as_ref().map(|q| &*q.ty),
                    "Explicit self types are not supported",
                );
                if let Some(ty_ident) = type_path.path.get_ident() {
                    let decl = Declarator {
                        pointer: 0,
                        ddecl: DirectDeclarator::Ident(ident),
                    };
                    return (ty_ident.to_string(), decl);
                } else {
                    self.fail(type_path, "Paths are not supported");
                }
            }
            syn::Type::Ptr(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Reference(ty_ref) => {
                self.fail_opt(&ty_ref.lifetime, "Explicit lifetimes are not supported");
                self.fail_opt(&ty_ref.mutability, "Mutable references are not supported");
                let mut ret = self.transform_declarator(ident, &*ty_ref.elem);
                ret.1.pointer += 1;
                return ret;
            }
            syn::Type::Slice(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::TraitObject(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Tuple(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::Verbatim(_) => { /* TODO: UNSUPPORTED */ }
            syn::Type::__TestExhaustive(_) => unreachable!(),
        }

        let declaration = Declaration::fallback();
        (declaration.0, declaration.1)
    }

    fn transform_stmt_block(&mut self, block: &syn::Block) -> Block {
        self.transform_block(block, false)
    }

    fn transform_expr_block(&mut self, block: &syn::Block) -> Block {
        self.transform_block(block, true)
    }

    fn transform_block(&mut self, block: &syn::Block, expression: bool) -> Block {
        let mut statements = vec![];
        for stmt in block.stmts.iter() {
            self.transform_stmt(stmt, expression, &mut statements);
        }
        Block(statements)
    }

    fn transform_stmt(&mut self, stmt: &syn::Stmt, expression: bool, out: &mut Vec<Stmt>) {
        match stmt {
            syn::Stmt::Local(syn::Local {
                attrs,
                let_token: _,
                pat,
                init,
                semi_token: _,
            }) => {
                self.fail_attrs(attrs);
                let expr = init.as_ref().map(|(_, expr)| self.transform_expr(&*expr));

                let declaration = match pat {
                    syn::Pat::Ident(_) => {
                        self.fail(stmt, "All local variables should have a type");
                        Declaration::fallback()
                    }
                    syn::Pat::Type(pat_type) => self.transform_variable_declarator(pat_type).into(),
                    _ => {
                        self.fail(pat, "Patterns are not supported");
                        Declaration::fallback()
                    }
                };

                out.push(Stmt::Declaration(declaration, expr));
            }
            syn::Stmt::Item(_) => self.fail(stmt, "Nested items are not supported"),
            syn::Stmt::Expr(expr) => match expr {
                syn::Expr::Block(_)
                | syn::Expr::While(_)
                | syn::Expr::If(_)
                | syn::Expr::Match(_)
                | syn::Expr::ForLoop(_)
                | syn::Expr::Loop(_) => self.transform_expr_stmt(expr, out),
                _ if expression => {
                    out.push(Stmt::Return(Some(self.transform_expr(expr))));
                }
                _ => self.fail(stmt, "Blocks as expressions are not supported"),
            },
            syn::Stmt::Semi(expr, _) => self.transform_expr_stmt(expr, out),
        }
    }

    fn transform_expr_stmt(&mut self, expr: &syn::Expr, out: &mut Vec<Stmt>) {
        let state = self.state();

        macro_rules! push {
            ($self:expr, $state:expr, $out:expr; $item:expr) => {
                if !$self.failed($state) {
                    $out.push($item);
                }
            };
        }
        match expr {
            syn::Expr::Assign(syn::ExprAssign {
                attrs,
                left,
                eq_token: _,
                right,
            }) => {
                self.fail_attrs(attrs);
                // TODO: are all right exprs valid?
                out.push(Stmt::Assignment(
                    self.transform_expr(&*left),
                    self.transform_expr(&*right),
                ));
            }
            syn::Expr::AssignOp(syn::ExprAssignOp {
                attrs,
                left,
                op,
                right,
            }) => {
                self.fail_attrs(attrs);
                out.push(Stmt::AssignmentOp(
                    self.transform_expr(left),
                    op.into(),
                    self.transform_expr(right),
                ));
            }
            syn::Expr::Block(syn::ExprBlock {
                attrs,
                label,
                block: expr_block,
            }) => {
                self.fail_attrs(attrs);
                let block = self.transform_stmt_block(expr_block);
                let stmt = Stmt::Block(block);
                match label {
                    Some(label) => {
                        push![self, state, out; Stmt::Labeled(label.name.to_string(), stmt.into())];
                    }
                    None => {
                        push![self, state, out; stmt];
                    }
                }
            }
            syn::Expr::Break(syn::ExprBreak {
                attrs,
                break_token: _,
                label,
                expr,
            }) => {
                self.fail_attrs(attrs);
                self.fail_opt(expr, "Break values are not supported");
                push![self, state, out; Stmt::Break(label.as_ref().map(|lf| lf.to_string()))];
            }
            syn::Expr::Continue(syn::ExprContinue {
                attrs,
                continue_token: _,
                label,
            }) => {
                self.fail_attrs(attrs);
                out.push(Stmt::Continue(
                    label.as_ref().map(|lf| lf.ident.to_string()),
                ));
            }
            syn::Expr::ForLoop(syn::ExprForLoop {
                attrs,
                label,
                for_token: _,
                pat,
                in_token: _,
                expr: range_expr,
                body,
            }) => {
                self.fail_attrs(attrs);
                let ident = match pat {
                    syn::Pat::Ident(pat) => pat.ident.to_string(),
                    _ => {
                        self.fail(pat, "Patterns are not supported");
                        return;
                    }
                };

                let (from, limits, to) = match &**range_expr {
                    syn::Expr::Range(syn::ExprRange {
                        attrs,
                        from,
                        limits,
                        to,
                    }) => {
                        self.fail_attrs(attrs);
                        (from, limits, to)
                    }
                    _ => {
                        self.fail(range_expr, "For loops only support range iteration");
                        return;
                    }
                };

                let inclusive = std::matches!(limits, syn::RangeLimits::Closed(_));

                let (start, end) = match (from, to) {
                    (Some(from), Some(to)) => (self.transform_expr(from), self.transform_expr(to)),
                    _ => {
                        self.fail(
                            expr,
                            "For loops only support range iteration with declared start and end",
                        );
                        return;
                    }
                };

                let for_loop = Stmt::For {
                    ident,
                    inclusive,
                    range: (start, end),
                    block: self.transform_stmt_block(body),
                };

                if let Some(label) = label {
                    out.push(Stmt::Labeled(label.name.to_string(), for_loop.into()));
                } else {
                    out.push(for_loop);
                }
            }
            syn::Expr::If(syn::ExprIf {
                attrs,
                if_token: _,
                cond,
                then_branch,
                else_branch,
            }) => {
                self.fail_attrs(attrs);
                let test = self.transform_expr(cond);
                let then = self.transform_stmt_block(then_branch);
                let mut alts = vec![];
                let mut tail = None;

                let mut opt_branch = else_branch.as_ref().map(|(_, b)| &**b);
                while let Some(branch) = opt_branch {
                    match branch {
                        syn::Expr::If(expr) => {
                            alts.push((
                                self.transform_expr(&*expr.cond),
                                self.transform_stmt_block(&expr.then_branch),
                            ));
                            opt_branch = expr.else_branch.as_ref().map(|(_, b)| &**b);
                        }
                        syn::Expr::Block(expr) => {
                            tail = Some(self.transform_stmt_block(&expr.block));
                            break;
                        }
                        _ => unreachable!(),
                    }
                }

                push![self, state, out ;
                    Stmt::If {
                        test,
                        then,
                        alts,
                        tail
                    }
                ];
            }
            syn::Expr::Loop(syn::ExprLoop {
                attrs,
                label,
                loop_token: _,
                body,
            }) => {
                self.fail_attrs(attrs);

                let block = self.transform_stmt_block(body);
                let while_stmt = Stmt::While {
                    test: Expr::Integer(1),
                    block,
                };

                if let Some(label) = label {
                    push![self, state, out ; Stmt::Labeled(label.name.to_string(), while_stmt.into())];
                } else {
                    push![self, state, out ; while_stmt];
                }
            }
            syn::Expr::Match(_) => {}
            syn::Expr::Return(syn::ExprReturn {
                attrs,
                return_token: _,
                expr: syn_expr,
            }) => {
                self.fail_attrs(attrs);
                let expr = syn_expr.as_ref().map(|expr| self.transform_expr(expr));

                push![self, state, out ; Stmt::Return(expr)]
            }
            syn::Expr::While(syn::ExprWhile {
                attrs,
                label,
                while_token: _,
                cond,
                body,
            }) => {
                self.fail_attrs(attrs);

                let test = self.transform_expr(cond);
                let block = self.transform_stmt_block(body);
                let while_stmt = Stmt::While { test, block };

                if let Some(label) = label {
                    push![self, state, out ; Stmt::Labeled(label.name.to_string(), while_stmt.into())];
                } else {
                    push![self, state, out ; while_stmt];
                }
            }

            syn::Expr::Group(_) => {}
            syn::Expr::Paren(_) => {}

            syn::Expr::Array(_)
            | syn::Expr::Async(_)
            | syn::Expr::Await(_)
            | syn::Expr::Binary(_)
            | syn::Expr::Box(_)
            | syn::Expr::Call(_)
            | syn::Expr::Cast(_)
            | syn::Expr::Closure(_)
            | syn::Expr::Field(_)
            | syn::Expr::Index(_)
            | syn::Expr::Let(_)
            | syn::Expr::Lit(_)
            | syn::Expr::Macro(_)
            | syn::Expr::MethodCall(_)
            | syn::Expr::Path(_)
            | syn::Expr::Range(_)
            | syn::Expr::Reference(_)
            | syn::Expr::Repeat(_)
            | syn::Expr::Struct(_)
            | syn::Expr::Try(_)
            | syn::Expr::TryBlock(_)
            | syn::Expr::Tuple(_)
            | syn::Expr::Type(_)
            | syn::Expr::Unary(_)
            | syn::Expr::Unsafe(_)
            | syn::Expr::Verbatim(_)
            | syn::Expr::Yield(_) => {
                out.push(Stmt::Expr(self.transform_expr(expr)));
            }
            syn::Expr::__TestExhaustive(_) => unreachable!(),
        }
    }
    fn transform_expr(&mut self, expr: &syn::Expr) -> Expr {
        match expr {
            syn::Expr::Array(_) => todo!(),
            syn::Expr::AssignOp(_) => todo!(),
            syn::Expr::Async(_) => todo!(),
            syn::Expr::Await(_) => self.fail(expr, "Await expressions are not supported"),
            syn::Expr::Binary(syn::ExprBinary {
                attrs,
                left,
                op,
                right,
            }) => {
                self.fail_attrs(attrs);
                let bin_op = BinOp::from(op);
                return Expr::Binary(
                    self.transform_expr(left).into(),
                    bin_op,
                    self.transform_expr(right).into(),
                );
            }
            syn::Expr::Box(_) => { /* TODO: UNSUPPORTED */ }
            syn::Expr::Call(syn::ExprCall {
                attrs,
                func,
                paren_token: _,
                args,
            }) => {
                self.fail_attrs(attrs);
                return Expr::Call(
                    self.transform_expr(&*func).into(),
                    args.iter().map(|arg| self.transform_expr(arg)).collect(),
                );
            }
            syn::Expr::Cast(syn::ExprCast {
                attrs,
                expr,
                as_token: _,
                ty,
            }) => {
                self.fail_attrs(attrs);
                return Expr::Cast(self.transform_type(ty), self.transform_expr(expr).into());
            }
            syn::Expr::Closure(_) => todo!(),
            syn::Expr::Field(syn::ExprField {
                attrs,
                base,
                dot_token: _,
                member,
            }) => {
                self.fail_attrs(attrs);
                let ident = match member {
                    syn::Member::Named(field) => field.to_string(),
                    syn::Member::Unnamed(_) => {
                        self.fail(expr, "Tuple structs are not supported");
                        return Expr::fallback();
                    }
                };
                // TODO: parens?
                return Expr::Field(self.transform_expr(&*base).into(), ident);
            }
            syn::Expr::Group(_) => todo!(),
            syn::Expr::If(syn::ExprIf {
                attrs,
                if_token: _,
                cond,
                then_branch,
                else_branch,
            }) => {
                self.fail_attrs(attrs);
                let then_expr = match then_branch.stmts.get(0) {
                    Some(syn::Stmt::Expr(expr)) if then_branch.stmts.len() == 1 => expr,
                    _ => {
                        self.fail(expr, "Invalid ternary");
                        return Expr::fallback();
                    }
                };

                let else_expr = match else_branch
                    .as_ref()
                    .map(|(_, b)| &**b)
                    .and_then(|expr| match expr {
                        syn::Expr::Block(b) => Some(&b.block),
                        _ => None,
                    })
                    .filter(|block| block.stmts.len() == 1)
                    .map(|block| &block.stmts[0])
                    .and_then(|stmt| match stmt {
                        syn::Stmt::Expr(expr) => Some(expr),
                        _ => None,
                    }) {
                    Some(expr) => expr,
                    _ => {
                        self.fail(expr, "Invalid ternary");
                        return Expr::fallback();
                    }
                };

                return Expr::Ternary(
                    self.transform_expr(cond).into(),
                    self.transform_expr(then_expr).into(),
                    self.transform_expr(else_expr).into(),
                );
            }
            syn::Expr::Index(syn::ExprIndex {
                attrs,
                expr,
                bracket_token: _,
                index,
            }) => {
                self.fail_attrs(attrs);
                return Expr::Index(
                    self.transform_expr(&*expr).into(),
                    self.transform_expr(&*index).into(),
                );
            }
            syn::Expr::Let(_) => todo!(),
            syn::Expr::Lit(expr_lit) => {
                self.fail_attrs(&expr_lit.attrs);
                match &expr_lit.lit {
                    syn::Lit::Int(lit) => match lit.base10_parse() {
                        Ok(n) => return Expr::Integer(n),
                        Err(e) => self.fail(lit, &format!("Unable to parse integer: {}", e)),
                    },
                    syn::Lit::Float(lit) => match lit.base10_parse() {
                        Ok(x) => return Expr::Float(x),
                        Err(e) => self.fail(lit, &format!("Unable to parse float: {}", e)),
                    },
                    syn::Lit::Str(lit) => return Expr::String(lit.value()),
                    syn::Lit::Bool(lit) => return Expr::Boolean(lit.value()),
                    syn::Lit::Char(lit) => return Expr::Char(lit.value()),
                    syn::Lit::ByteStr(_) => self.fail(expr, "Byte strings are not supported"),
                    syn::Lit::Byte(_) => self.fail(expr, "Byte literals are not supported"),
                    syn::Lit::Verbatim(_) => self.fail(expr, "Unsupported expr"),
                }
            }
            syn::Expr::Macro(_) => todo!(),
            syn::Expr::MethodCall(_) => {
                self.fail(expr, "Method calls are not supported");
            }
            syn::Expr::Paren(expr) => {
                if !self.fail_attrs(&expr.attrs) {
                    return Expr::Paren(self.transform_expr(&expr.expr).into());
                }
            }
            syn::Expr::Path(expr) => match expr.path.get_ident() {
                Some(ident) => return Expr::Variable(ident.to_string()),
                None => {
                    self.fail(expr, "Paths are not supported");
                }
            },
            syn::Expr::Range(_) => todo!(),
            syn::Expr::Reference(syn::ExprReference {
                attrs,
                and_token: _,
                raw: _,
                mutability,
                expr,
            }) => {
                let state = self.state();
                self.fail_attrs(attrs);
                self.fail_opt(mutability, "Mutable references are not supported");
                return if self.failed(state) {
                    Expr::fallback()
                } else {
                    Expr::Ref(self.transform_expr(expr).into())
                };
            }
            syn::Expr::Repeat(_) => todo!(),
            syn::Expr::Struct(syn::ExprStruct {
                attrs,
                path,
                brace_token: _,
                fields: struct_fields,
                dot2_token: _,
                rest,
            }) => {
                self.fail_attrs(attrs);
                self.fail_opt(rest, "Rest functional updates are not supported");

                let ident = match path.get_ident() {
                    Some(ident) => ident.to_string(),
                    _ => {
                        self.fail(expr, "Paths are not supported");
                        return Expr::fallback();
                    }
                };

                let mut fields = vec![];

                for field in struct_fields {
                    let syn::FieldValue {
                        attrs,
                        member,
                        colon_token: _,
                        expr,
                    } = field;
                    self.fail_attrs(attrs);

                    let field_ident = match member {
                        syn::Member::Named(ident) => ident.to_string(),
                        syn::Member::Unnamed(_) => {
                            self.fail(member, "Unnamed fields are not supported");
                            return Expr::fallback();
                        }
                    };

                    fields.push((field_ident, self.transform_expr(expr)));
                }

                return Expr::Cast(Type { pointer: 0, ident }, Expr::StructInit(fields).into());
            }
            syn::Expr::Try(_) => todo!(),
            syn::Expr::TryBlock(_) => todo!(),
            syn::Expr::Tuple(_) => todo!(),
            syn::Expr::Type(_) => todo!(),
            syn::Expr::Unary(syn::ExprUnary { attrs, op, expr }) => {
                self.fail_attrs(attrs);
                let expr = self.transform_expr(expr);
                return match op {
                    syn::UnOp::Deref(_) => Expr::Deref(expr.into()),
                    syn::UnOp::Not(_) => Expr::Not(expr.into()),
                    syn::UnOp::Neg(_) => Expr::Neg(expr.into()),
                };
            }
            syn::Expr::Unsafe(_) => todo!(),
            syn::Expr::Verbatim(_) => todo!(),
            syn::Expr::Yield(_) => todo!(),
            syn::Expr::__TestExhaustive(_) => unreachable!(),

            syn::Expr::Assign(_)
            | syn::Expr::Block(_)
            | syn::Expr::ForLoop(_)
            | syn::Expr::Return(_)
            | syn::Expr::Continue(_)
            | syn::Expr::Break(_)
            | syn::Expr::Match(_)
            | syn::Expr::Loop(_)
            | syn::Expr::While(_) => {
                self.fail(expr, "Statement cannot be used as expression");
            }
        }

        Expr::fallback()
    }

    fn fail<S: Spanned>(&mut self, item: S, msg: &str) {
        self.errors.push(Error {
            span: item.span(),
            msg: msg.to_string(),
        });
    }

    fn fail_opt<'a, S: 'a>(&mut self, item: &'a Option<S>, msg: &str) -> bool
    where
        &'a S: Spanned,
    {
        if let Some(item) = item {
            self.fail(item, msg);
            true
        } else {
            false
        }
    }

    fn fail_iter<'a, S: 'a, T: IntoIterator<Item = &'a S>>(&mut self, item: T, msg: &str) -> bool
    where
        &'a S: Spanned,
    {
        let mut failed = false;
        for it in item {
            self.fail(it, msg);
            failed = true;
        }

        failed
    }

    fn fail_attrs(&mut self, attrs: &[syn::Attribute]) -> bool {
        self.fail_iter(attrs.iter(), "Attributes are not supported")
    }

    fn fail_vis(&mut self, vis: &syn::Visibility) {
        if let syn::Visibility::Inherited = vis {
            return;
        } else {
            self.fail(vis, "Visibility modifiers are not supported");
        }
    }

    fn state(&self) -> usize {
        self.errors.len()
    }

    fn failed(&self, state: usize) -> bool {
        self.errors.len() != state
    }
}

pub fn transform(file: &syn::File) -> std::result::Result<Program, CompilationError> {
    let ctx = Context::default();
    ctx.transform(file)
}

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

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    String(String),
    Boolean(bool),
    Variable(String),
    Char(char),
    Float(f64),
    Ref(Box<Expr>),
    StructInit(Vec<(String, Expr)>),
    Call(Box<Expr>, Vec<Expr>),
    Deref(Box<Expr>),
    Not(Box<Expr>),
    Neg(Box<Expr>),
    Field(Box<Expr>, String),
    Paren(Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Cast(Type, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn fallback() -> Self {
        Expr::Integer(0)
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Declaration(pub String, pub Declarator);

impl Declaration {
    fn fallback() -> Self {
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
    pointer: u8,
    ddecl: DirectDeclarator,
}

impl Declarator {
    fn fallback() -> Self {
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

        match ddecl {
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

fn prelude() -> Vec<Item> {
    let mut ret = vec![
        Item::Include(true, "stdint".to_string()),
        Item::Include(true, "stdbool".to_string()),
    ];

    for (rust, c) in &[
        // TODO: Well, this is wishful thinking at best... Maybe add some static assertions?
        ("f32", "float"),
        ("f64", "double"),
        ("u32", "uint32_t"),
        ("i32", "int32_t"),
        ("u8", "uint8_t"),
        ("i8", "int8_t"),
        ("usize", "uintptr_t"),
        ("isize", "intptr_t"),
    ] {
        ret.push(Item::Ifndef(rust.to_string(), c.to_string()));
    }
    ret
}
