use proc_macro2::Span;
use proc_macro2::TokenTree;
use syn::spanned::Spanned;

use super::{CompilationError as Error, CompilationErrors};
use crate::c_lang::{
    BinOp, Block, Declaration, Declarator, DirectDeclarator, Expr, Item, Program, Signature, Stmt,
};
use crate::typecheck::Builtins;
use crate::typecheck::TypeInfo;

macro_rules! unsupported {
    ($msg:literal 1) => {
        std::concat!($msg, " is not supported")
    };
    ($msg:literal) => {
        std::concat!($msg, " are not supported")
    };
}

struct Context<'a> {
    errors: Vec<Error>,
    type_info: TypeInfo<'a>,
}

impl<'a> Context<'a> {
    fn transform(mut self, file: &syn::File) -> std::result::Result<Program, CompilationErrors> {
        let syn::File {
            shebang,
            attrs,
            items,
        } = file;

        self.fail_opt(shebang, unsupported!["Shebang" 1]);
        self.fail_attrs(attrs);

        let mut decls = vec![];
        decls.extend(prelude(&self.type_info.builtins));
        let mut suffix = vec![];

        let mut use_decls = vec![];

        for item in items {
            self.transform_item(item, &mut decls, &mut suffix, &mut use_decls);
        }

        if use_decls.len() > 0 {
            let last = use_decls.last().unwrap().clone();
            let span = use_decls[0].join(last).unwrap();

            self.errors.insert(
                0,
                Error {
                    span,
                    msg: unsupported!["\"use\" declarations"].to_string(),
                },
            );
        }

        decls.extend(suffix);

        if self.errors.is_empty() {
            Ok(Program(decls))
        } else {
            Err(CompilationErrors {
                errors: self.errors,
            })
        }
    }

    fn transform_item(
        &mut self,
        item: &syn::Item,
        prefix: &mut Vec<Item>,
        out: &mut Vec<Item>,
        use_decls: &mut Vec<Span>,
    ) {
        let mut fail = |msg: &str| self.fail(item, &format!("{} are not supported", msg));

        match item {
            syn::Item::Const(_) => fail("Const items"),
            syn::Item::Enum(_) => fail("Enums"),
            syn::Item::ExternCrate(_) => fail("Extern crates"),
            syn::Item::Fn(item) => self.transform_fn(item, prefix, out),
            syn::Item::ForeignMod(_) => fail("Extern modules"),
            syn::Item::Impl(_) => fail("Impl blocks"),
            syn::Item::Macro(syn::ItemMacro {
                attrs,
                ident,
                mac,
                semi_token: _,
            }) => {
                self.fail_attrs(attrs);

                if self.fail_opt(
                    &ident.as_ref().map(|_| item),
                    unsupported!["macro_rules-style macros"],
                ) {
                    return;
                }

                let syn::Macro {
                    path,
                    bang_token: _,
                    delimiter: _,
                    tokens,
                } = mac;

                let ident = match self.path_to_ident(path) {
                    Some(ident) => ident,
                    _ => return,
                };

                if ident != "include" {
                    self.fail(item, "Unsupported macro");
                    return;
                }

                const USAGE: &str =
                    "include! can be invoked as `include!(file)` or `include!(<header>)`";

                let mut tokens: Vec<_> = tokens.clone().into_iter().collect();

                if tokens.len() == 1 {
                    let header = match &tokens[0] {
                        TokenTree::Ident(ident) => ident.to_string(),
                        _ => {
                            self.fail(item, USAGE);
                            return;
                        }
                    };

                    prefix.push(Item::Include(false, header));
                    return;
                }

                let is_char = |tt: &TokenTree, c| match tt {
                    TokenTree::Punct(punct) => punct.as_char() == c,
                    _ => false,
                };

                if tokens.len() < 3
                    || !(is_char(&tokens[0], '<')
                        && tokens.last().map(|tok| is_char(tok, '>')).unwrap_or(false))
                {
                    self.fail(item, USAGE);
                    return;
                }

                let _ = tokens.pop();
                let _ = tokens.remove(0);

                let mut component = true;

                let mut path = String::new();

                for comp in &tokens {
                    match (comp, component) {
                        (TokenTree::Ident(ident), true) => {
                            path += &ident.to_string();
                            component = false;
                        }
                        (TokenTree::Punct(punct), false) if punct.as_char() == '/' => {
                            path += "/";
                            component = true;
                        }
                        _ => {
                            self.fail(item, USAGE);
                            return;
                        }
                    }
                }

                if component {
                    self.fail(item, USAGE);
                    return;
                }

                prefix.push(Item::Include(true, path))
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
                let named_fields = match item_fields {
                    syn::Fields::Named(named) => Some(named),
                    syn::Fields::Unnamed(_) => {
                        self.fail(item_fields, unsupported!["Tuple structs"]);
                        None
                    }
                    syn::Fields::Unit => {
                        self.fail(item, unsupported!["Unit structs"]);
                        None
                    }
                };

                let fields = self.transform_composite(attrs, vis, generics, named_fields);

                if item_fields.is_empty() && named_fields.is_some() {
                    self.fail(item, unsupported!["Empty structs"]);
                }

                prefix.push(Item::StructTypedef(ident.to_string()));
                out.push(Item::Struct(ident.to_string(), fields));
            }
            syn::Item::Trait(_) => fail("Traits"),
            syn::Item::TraitAlias(_) => fail("Trait aliases"),
            syn::Item::Type(_) => fail("Type aliases"),
            syn::Item::Union(syn::ItemUnion {
                attrs,
                vis,
                union_token: _,
                ident,
                generics,
                fields: item_fields,
            }) => {
                let fields = self.transform_composite(attrs, vis, generics, Some(item_fields));
                prefix.push(Item::UnionTypedef(ident.to_string()));
                out.push(Item::Union(ident.to_string(), fields));
            }
            syn::Item::Use(_) => {
                use_decls.push(item.span());
            }
            syn::Item::Verbatim(_) => self.fail(item, "Unexpected tokens"),
            syn::Item::__TestExhaustive(_) => unreachable!(),
        }
    }

    fn transform_composite(
        &mut self,
        attrs: &[syn::Attribute],
        vis: &syn::Visibility,
        generics: &syn::Generics,
        item_fields: Option<&syn::FieldsNamed>,
    ) -> Vec<Declaration> {
        self.fail_attrs(attrs);
        self.fail_vis(vis);
        self.fail_opt(
            &generics.lt_token.as_ref().map(|_| generics),
            unsupported!["Generics"],
        );

        let mut fields = vec![];

        let item_fields = if let Some(item_fields) = item_fields {
            item_fields
        } else {
            return fields;
        };

        for syn::Field {
            attrs,
            vis,
            ident: field_ident,
            colon_token: _,
            ty,
        } in &item_fields.named
        {
            self.fail_attrs(attrs);
            self.fail_vis(vis);

            fields.push(
                self.transform_declarator(
                    field_ident
                        .as_ref()
                        .expect("Unexpected unnamed field")
                        .to_string()
                        .into(),
                    ty,
                )
                .into(),
            );
        }

        fields
    }

    fn transform_fn(&mut self, item: &syn::ItemFn, prefix: &mut Vec<Item>, out: &mut Vec<Item>) {
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
        self.fail_opt(constness, unsupported!["Const functions"]);
        self.fail_opt(asyncness, unsupported!["Async functions"]);
        self.fail_opt(unsafety, unsupported!["Unsafe functions"]);
        self.fail_opt(abi, unsupported!["`extern` functions"]);
        self.fail_opt(
            &generics.lt_token.as_ref().map(|_| generics),
            unsupported!["Generics"],
        );
        self.fail_opt(variadic, unsupported!["Variadic functions"]);

        let mut noreturn = false;
        let mut ret = Declaration(
            "void".to_string(),
            Declarator {
                pointer: 0,
                ddecl: DirectDeclarator::Abstract,
            },
        );

        if let syn::ReturnType::Type(_, ty) = output {
            match &**ty {
                syn::Type::Never(_) => {
                    noreturn = true;
                }
                _ => ret = self.transform_declarator(None, ty).into(),
            }
        }

        let mut args = vec![];

        for fn_arg in inputs {
            match fn_arg {
                syn::FnArg::Receiver(_) => self.fail(fn_arg, unsupported!["Methods"]),
                syn::FnArg::Typed(pat_type) => {
                    args.push(self.transform_variable_declarator(pat_type));
                }
            }
        }

        let signature = Signature {
            noreturn,
            ret,
            name: ident.to_string(),
            args,
        };

        let block = self.transform_expr_block(item_block);

        prefix.push(Item::FunctionDeclaration(signature.clone()));
        out.push(Item::Function(signature, block));
    }

    fn transform_variable_declarator(&mut self, pat_type: &syn::PatType) -> (String, Declarator) {
        self.fail_attrs(&pat_type.attrs);
        let ident = match &*pat_type.pat {
            syn::Pat::Ident(pat_ident) => {
                self.fail_attrs(&pat_ident.attrs);
                self.fail_opt(&pat_ident.by_ref, unsupported!["By-ref patterns"]);
                self.fail_opt(&pat_ident.mutability, unsupported!["Mutable patterns"]);
                self.fail_opt(
                    &pat_ident.subpat.as_ref().map(|(_, sub)| &*sub),
                    unsupported!["Subpatterns"],
                );
                pat_ident.ident.to_string()
            }
            _ => {
                self.fail(pat_type, unsupported!["Patterns"]);
                "u32".to_string()
            }
        };
        self.transform_declarator(Some(ident), &*pat_type.ty)
    }

    fn transform_declarator(
        &mut self,
        ident: Option<String>,
        ty: &syn::Type,
    ) -> (String, Declarator) {
        let mut declarator = Declarator {
            pointer: 0,
            ddecl: ident
                .map(DirectDeclarator::Ident)
                .unwrap_or(DirectDeclarator::Abstract),
        };

        let mut ty = ty;

        loop {
            match ty {
                syn::Type::Array(syn::TypeArray {
                    bracket_token: _,
                    elem,
                    semi_token: _,
                    len,
                }) => {
                    let length = self.transform_expr(len);
                    declarator = if declarator.pointer == 0 {
                        declarator.ddecl = DirectDeclarator::Array(declarator.ddecl.into(), length);
                        declarator
                    } else {
                        let paren = DirectDeclarator::Paren(declarator.into());
                        Declarator {
                            pointer: 0,
                            ddecl: DirectDeclarator::Array(paren.into(), length),
                        }
                    };
                    ty = elem;
                    continue;
                }
                syn::Type::BareFn(syn::TypeBareFn {
                    lifetimes,
                    unsafety,
                    abi,
                    fn_token: _,
                    paren_token: _,
                    inputs,
                    variadic,
                    output,
                }) => {
                    self.fail_opt(lifetimes, unsupported!["Lifetimes"]);
                    self.fail_opt(unsafety, unsupported!["Unsafe fn types"]);
                    self.fail_opt(abi, unsupported!["Explicit ABIs"]);
                    // TODO: support?
                    self.fail_opt(variadic, unsupported!["Variadic functions"]);
                    let args: Vec<_> = inputs
                        .iter()
                        .map(|input| {
                            self.fail_attrs(&input.attrs);
                            // TODO: do named args have any consequence? Should names be preserved
                            let _ = &input.name;
                            self.transform_declarator(None, &input.ty).into()
                        })
                        .collect();
                    declarator.pointer += 1;
                    let ddecl = DirectDeclarator::Paren(declarator.into());
                    declarator = Declarator {
                        pointer: 0,
                        ddecl: DirectDeclarator::Function(ddecl.into(), args),
                    };
                    ty = match output {
                        syn::ReturnType::Type(_, ty) => ty,
                        syn::ReturnType::Default => {
                            return ("void".to_string(), declarator);
                        }
                    };
                    continue;
                }
                syn::Type::Group(syn::TypeGroup {
                    group_token: _,
                    elem,
                }) => {
                    ty = elem;
                    continue;
                }
                syn::Type::ImplTrait(_) => self.fail(ty, unsupported!["Impl trait" 1]),
                syn::Type::Infer(_) => self.fail(ty, unsupported!["The inferred type `_`" 1]),
                syn::Type::Macro(_) => self.fail(ty, unsupported!["Macros in type position"]),
                syn::Type::Never(_) => self.fail(
                    ty,
                    "The never type `!` is only supported as return type of a function",
                ),
                syn::Type::Paren(syn::TypeParen {
                    paren_token: _,
                    elem,
                }) => {
                    ty = &*elem;
                    continue;
                }
                syn::Type::Path(type_path) => {
                    self.fail_opt(
                        &type_path.qself.as_ref().map(|q| &*q.ty),
                        unsupported!["Explicit self types"],
                    );
                    if let Some(ty_ident) = self.path_to_ident(&type_path.path) {
                        return (ty_ident, declarator);
                    }
                }
                syn::Type::Ptr(_) => {
                    self.fail(ty, "Raw pointer syntax is not supported. Use references.")
                }
                syn::Type::Reference(ty_ref) => {
                    self.fail_opt(&ty_ref.lifetime, unsupported!["Explicit lifetimes"]);
                    self.fail_opt(&ty_ref.mutability, unsupported!["Mutable references"]);

                    declarator.pointer += 1;
                    ty = &*ty_ref.elem;
                    continue;
                }
                syn::Type::Slice(_) => self.fail(ty, unsupported!["Slices"]),
                syn::Type::TraitObject(_) => self.fail(ty, unsupported!["Trait objects"]),
                syn::Type::Tuple(_) => self.fail(ty, unsupported!["Tuples"]),
                syn::Type::Verbatim(_) => self.fail(ty, "Unexpected type"),
                syn::Type::__TestExhaustive(_) => unreachable!(),
            }
            break;
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
                        self.fail(pat, unsupported!["Patterns"]);
                        Declaration::fallback()
                    }
                };

                out.push(Stmt::Declaration(declaration, expr));
            }
            syn::Stmt::Item(_) => self.fail(stmt, unsupported!["Nested items"]),
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
                _ => self.fail(stmt, unsupported!["Blocks as expressions"]),
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
                        push![self, state, out; Stmt::Labeled(label.name.ident.to_string(), stmt.into())];
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
                self.fail_opt(expr, unsupported!["Break values"]);
                push![self, state, out; Stmt::Break(label.as_ref().map(|lf| lf.ident.to_string()))];
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
                        self.fail(pat, unsupported!["Patterns"]);
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
                        self.fail(range_expr, "`for` loops only support range iteration");
                        return;
                    }
                };

                let inclusive = std::matches!(limits, syn::RangeLimits::Closed(_));

                // TODO: infer variable type?
                let (start, end) = match (from, to) {
                    (Some(from), Some(to)) => (self.transform_expr(from), self.transform_expr(to)),
                    _ => {
                        self.fail(
                            expr,
                            "`for` loops only support range iteration with declared start and end",
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
                    out.push(Stmt::Labeled(label.name.ident.to_string(), for_loop.into()));
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
                    push![self, state, out ; Stmt::Labeled(label.name.ident.to_string(), while_stmt.into())];
                } else {
                    push![self, state, out ; while_stmt];
                }
            }
            syn::Expr::Match(syn::ExprMatch {
                attrs,
                match_token: _,
                expr,
                brace_token: _,
                arms,
            }) => {
                self.fail_attrs(attrs);
                let expr = self.transform_expr(expr);
                let mut cases = vec![];
                let mut catch = None;

                for (
                    i,
                    syn::Arm {
                        attrs,
                        pat,
                        guard,
                        fat_arrow_token: _,
                        body,
                        comma: _,
                    },
                ) in arms.iter().enumerate()
                {
                    self.fail_attrs(attrs);
                    self.fail_opt(
                        &guard.as_ref().map(|(if_, _)| if_),
                        unsupported!["Match guards"],
                    );

                    let block = if let syn::Expr::Block(body) = &**body {
                        self.transform_block(&body.block, false)
                    } else {
                        self.fail(
                            body,
                            "Match expressions only support blocks as bodies in each arm",
                        );
                        continue;
                    };

                    let last_arm = i == arms.len() - 1;

                    match pat {
                        syn::Pat::Ident(pat) => {
                            self.fail_attrs(&pat.attrs);
                            cases.push((Expr::Variable(pat.ident.to_string()), block));
                        }
                        syn::Pat::Lit(pat) => {
                            self.fail_attrs(&pat.attrs);
                            cases.push((self.transform_expr(&pat.expr), block));
                        }
                        syn::Pat::Wild(pat) if catch.is_none() && last_arm => {
                            self.fail_attrs(&pat.attrs);
                            catch = Some(block);
                        }
                        syn::Pat::Wild(_) => {
                            self.fail(
                                pat,
                                "Wildcard pattern can only be used as the last arm of a `match`",
                            );
                            continue;
                        }
                        _ => {
                            self.fail(
                                pat,
                                "Match expressions only support variables and literals as patterns",
                            );
                            continue;
                        }
                    }
                }

                out.push(Stmt::Switch {
                    value: expr,
                    cases,
                    catch,
                });
            }
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
                    push![self, state, out ; Stmt::Labeled(label.name.ident.to_string(), while_stmt.into())];
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
        let mut ret = self.transform_expr_raw(expr);
        ret.fix_precedence();
        ret
    }

    fn transform_expr_raw(&mut self, expr: &syn::Expr) -> Expr {
        match expr {
            syn::Expr::Array(syn::ExprArray {
                attrs,
                bracket_token: _,
                elems,
            }) => {
                self.fail_attrs(attrs);
                return Expr::ArrayInit(
                    elems.iter().map(|expr| self.transform_expr(expr)).collect(),
                );
            }
            syn::Expr::Async(_) => self.fail(expr, unsupported!["Async blocks"]),
            syn::Expr::Await(_) => self.fail(expr, unsupported!["Await expressions"]),
            syn::Expr::Binary(syn::ExprBinary {
                attrs,
                left,
                op,
                right,
            }) => {
                self.fail_attrs(attrs);
                return Expr::Binary(
                    self.transform_expr(left).into(),
                    BinOp::from(op),
                    self.transform_expr(right).into(),
                );
            }
            syn::Expr::Box(_) => self.fail(expr, unsupported!["Box expressions"]),
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
                return Expr::Cast(
                    Box::new(self.transform_declarator(None, ty).into()),
                    self.transform_expr(expr).into(),
                );
            }
            syn::Expr::Closure(_) => self.fail(expr, unsupported!["Closures"]),
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
                        self.fail(expr, unsupported!["Tuple structs"]);
                        return Expr::fallback();
                    }
                };
                let deref = self.type_info.needs_deref(&*base, &ident);
                return Expr::Field {
                    base: self.transform_expr(&*base).into(),
                    field: ident,
                    deref,
                };
            }
            syn::Expr::Group(expr) => {
                self.fail_attrs(&expr.attrs);
                return self.transform_expr(&*expr.expr);
            }
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
                        self.fail(expr, "Invalid ternary expression");
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
                        self.fail(expr, "Invalid ternary expression");
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
            syn::Expr::Let(_) => self.fail(expr, unsupported!["Let guards"]),
            syn::Expr::Lit(expr_lit) => {
                self.fail_attrs(&expr_lit.attrs);
                return self.transform_lit(&expr_lit.lit);
            }
            syn::Expr::Macro(_) => self.fail(expr, unsupported!["Macro expressions"]),
            syn::Expr::MethodCall(_) => self.fail(expr, unsupported!["Method calls"]),
            syn::Expr::Paren(expr) => {
                self.fail_attrs(&expr.attrs);
                return Expr::Paren(self.transform_expr(&expr.expr).into());
            }
            syn::Expr::Path(expr) => {
                if let Some(ident) = self.path_to_ident(&expr.path) {
                    return Expr::Variable(ident);
                }
            }
            syn::Expr::Range(_) => self.fail(expr, unsupported!["Range expressions"]),
            syn::Expr::Reference(syn::ExprReference {
                attrs,
                and_token: _,
                raw: _,
                mutability,
                expr,
            }) => {
                let state = self.state();
                self.fail_attrs(attrs);
                self.fail_opt(mutability, unsupported!["Mutable references"]);
                return if self.failed(state) {
                    Expr::fallback()
                } else {
                    Expr::Ref(self.transform_expr(expr).into())
                };
            }
            syn::Expr::Repeat(syn::ExprRepeat {
                attrs,
                bracket_token: _,
                expr: repeat_expr,
                semi_token: _,
                len,
            }) => {
                self.fail_attrs(attrs);
                let len_expr = self.transform_expr(len);
                if let Expr::Integer(n) = len_expr {
                    let repeat = self.transform_expr(repeat_expr);
                    return Expr::ArrayInit(
                        std::iter::repeat(0)
                            .take(n as usize)
                            .map(|_| repeat.clone())
                            .collect(),
                    );
                } else {
                    self.fail(
                        len,
                        "Array repeat expressions only support integer constants",
                    );
                }
            }
            syn::Expr::Struct(syn::ExprStruct {
                attrs,
                path,
                brace_token: _,
                fields: struct_fields,
                dot2_token: _,
                rest,
            }) => {
                self.fail_attrs(attrs);
                self.fail_opt(rest, unsupported!["Rest functional updates"]);

                let ident = match self.path_to_ident(path) {
                    Some(ident) => ident,
                    _ => return Expr::fallback(),
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
                            self.fail(member, unsupported!["Unnamed fields"]);
                            return Expr::fallback();
                        }
                    };

                    fields.push((field_ident, self.transform_expr(expr)));
                }

                return Expr::Cast(
                    Declaration(
                        ident,
                        Declarator {
                            pointer: 0,
                            ddecl: DirectDeclarator::Abstract,
                        },
                    )
                    .into(),
                    Expr::StructInit(fields).into(),
                );
            }
            syn::Expr::Try(_) => self.fail(expr, unsupported!["Try operator" 1]),
            syn::Expr::TryBlock(_) => self.fail(expr, unsupported!["Try blocks"]),
            syn::Expr::Tuple(_) => self.fail(expr, unsupported!["Tuples"]),
            syn::Expr::Type(_) => self.fail(expr, unsupported!["Type ascription" 1]),
            syn::Expr::Unary(syn::ExprUnary { attrs, op, expr }) => {
                self.fail_attrs(attrs);
                let expr = self.transform_expr(expr);
                return match op {
                    syn::UnOp::Deref(_) => Expr::Deref(expr.into()),
                    syn::UnOp::Not(_) => Expr::Not(expr.into()),
                    syn::UnOp::Neg(_) => Expr::Neg(expr.into()),
                };
            }
            syn::Expr::Unsafe(_) => self.fail(expr, unsupported!["Unsafe blocks"]),
            syn::Expr::Verbatim(_) => self.fail(expr, "Unexpected expression"),
            syn::Expr::Yield(_) => self.fail(expr, unsupported!["Yield" 1]),
            syn::Expr::__TestExhaustive(_) => unreachable!(),

            syn::Expr::Assign(_)
            | syn::Expr::AssignOp(_)
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

    fn transform_lit(&mut self, lit: &syn::Lit) -> Expr {
        match lit {
            // TODO: C suffixes? Preserve octal/hexadecimal?
            syn::Lit::Int(lit) => match lit.base10_parse() {
                Ok(n) => return Expr::Integer(n),
                Err(e) => self.fail(lit, &format!("Unable to parse integer: {}", e)),
            },
            // TODO: C suffixes
            syn::Lit::Float(lit) => match lit.base10_parse() {
                Ok(x) => return Expr::Float(x),
                Err(e) => self.fail(lit, &format!("Unable to parse float: {}", e)),
            },
            syn::Lit::Str(lit) => return Expr::String(lit.value()),
            syn::Lit::Bool(lit) => return Expr::Boolean(lit.value()),
            syn::Lit::Char(lit) => return Expr::Char(lit.value()),
            // TODO: support
            syn::Lit::ByteStr(_) => self.fail(lit, unsupported!["Byte strings"]),
            // TODO: support
            syn::Lit::Byte(_) => self.fail(lit, unsupported!["Byte literals"]),
            syn::Lit::Verbatim(_) => self.fail(lit, "Unsupported literal"),
        }
        Expr::fallback()
    }

    fn path_to_ident(&mut self, path: &syn::Path) -> Option<String> {
        if let Some(ident) = path.get_ident() {
            return Some(ident.to_string());
        }

        for segment in &path.segments {
            if !std::matches!(segment.arguments, syn::PathArguments::None) {
                self.fail(path, unsupported!["Generics"]);
                return None;
            }
        }

        self.fail(path, unsupported!["Paths"]);
        None
    }

    fn fail<S: Spanned>(&mut self, item: S, msg: &str) {
        self.errors.push(Error {
            span: item.span(),
            msg: msg.to_string(),
        });
    }

    fn fail_opt<'item, S: 'item>(&mut self, item: &'item Option<S>, msg: &str) -> bool
    where
        &'item S: Spanned,
    {
        if let Some(item) = item {
            self.fail(item, msg);
            true
        } else {
            false
        }
    }

    fn fail_iter<'item, S: 'item, T: IntoIterator<Item = &'item S>>(
        &mut self,
        item: T,
        msg: &str,
    ) -> bool
    where
        &'item S: Spanned,
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

pub(crate) fn transform(
    file: &syn::File,
    type_info: TypeInfo<'_>,
) -> std::result::Result<Program, CompilationErrors> {
    let ctx = Context {
        errors: vec![],
        type_info,
    };

    ctx.transform(file)
}

fn prelude(builtins: &Builtins) -> Vec<Item> {
    let mut ret = vec![];

    let stdint = builtins.u64
        || builtins.i64
        || builtins.u32
        || builtins.i32
        || builtins.u16
        || builtins.i16
        || builtins.u8
        || builtins.i8
        || builtins.usize
        || builtins.isize;

    let stdbool = builtins.bool;

    if stdint {
        ret.push(Item::Include(true, "stdint".to_string()));
    }

    if stdbool {
        ret.push(Item::Include(true, "stdbool".to_string()));
    }

    for (rust, c, should_include) in &[
        // TODO: Well, this is wishful thinking at best... Maybe add some static assertions?
        ("f32", "float", builtins.f32),
        ("f64", "double", builtins.f64),
        ("u64", "uint64_t", builtins.u64),
        ("i64", "int64_t", builtins.i64),
        ("u32", "uint32_t", builtins.u32),
        ("i32", "int32_t", builtins.i32),
        ("u16", "uint16_t", builtins.u16),
        ("i16", "int16_t", builtins.i16),
        ("u8", "uint8_t", builtins.u8),
        ("i8", "int8_t", builtins.i8),
        ("usize", "uintptr_t", builtins.usize),
        ("isize", "intptr_t", builtins.isize),
    ] {
        if *should_include {
            ret.push(Item::Ifndef(rust.to_string(), c.to_string()));
        }
    }
    ret
}
