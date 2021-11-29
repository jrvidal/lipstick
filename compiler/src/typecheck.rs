use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use syn::{
    spanned::Spanned,
    visit::{self, Visit},
    Block, Expr,
};

use super::{CompilationError as Error, CompilationErrors};

struct StringInterner(std::cell::RefCell<string_interner::StringInterner<TypeId>>);

impl StringInterner {
    fn new() -> Self {
        Self(std::cell::RefCell::new(
            string_interner::StringInterner::new(),
        ))
    }

    fn get_or_intern(&self, string: impl AsRef<str>) -> TypeId {
        self.0.borrow_mut().get_or_intern(string)
    }

    fn resolve(&self, sym: TypeId) -> Option<String> {
        self.0.borrow().resolve(sym).map(|s| s.to_string())
    }
}

pub(crate) struct Ref<'a, T>(&'a T);

impl<'a, T> From<&'a T> for Ref<'a, T> {
    fn from(ref_: &'a T) -> Self {
        Self(ref_)
    }
}

impl<'a, T> Ref<'a, T> {
    fn as_ptr(&self) -> *const T {
        self.0 as *const _
    }
}

impl<'a, T> Hash for Ref<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> PartialEq for Ref<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'a, T> Eq for Ref<'a, T> {}
impl<'a, T> Copy for Ref<'a, T> {}

pub(crate) type Scope<'a> = Ref<'a, Block>;

#[derive(Default)]
struct ScopeInfo<'a> {
    parent: Option<Scope<'a>>,
    vars: HashMap<String, Type>,
}

pub(crate) struct TypeInfo<'a> {
    cache: HashMap<Ref<'a, Expr>, Type>,
    exprs: HashMap<Ref<'a, Expr>, Scope<'a>>,
    scopes: HashMap<Scope<'a>, ScopeInfo<'a>>,
    declared_types: HashMap<TypeId, DeclaredType>,
    type_names: StringInterner,
}

impl<'a> TypeInfo<'a> {
    pub(crate) fn needs_deref(&self, expr: &'a Expr, field: &str) -> bool {
        let ty = self.type_of(expr);
        if let Type::Ref(ty) = ty {
            Some(*ty)
        } else {
            None
        }
        .and_then(|ty| {
            if let Type::Named(type_id) = ty {
                Some(type_id)
            } else {
                None
            }
        })
        .and_then(|type_id| self.declared_types.get(&type_id))
        .map(|declared| match declared {
            DeclaredType::Composite(fields) => fields.contains_key(field),
            _ => false,
        })
        .unwrap_or(false)
    }

    fn type_of(&self, expr: &'a Expr) -> Type {
        self.cache.get(&expr.into()).cloned().unwrap_or_else(|| {
            let ty = self.compute_type_of(expr);
            // TODO: use cache
            ty
        })
    }

    fn compute_type_of(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Index(expr) => {
                let array_type = self.type_of(&*expr.expr);
                match array_type {
                    Type::Array(ty) => *ty,
                    _ => Type::Unknown,
                }
            }
            Expr::Field(expr) => {
                let type_id = match self.type_of(&*expr.base) {
                    Type::Named(type_id) => type_id,
                    Type::Ref(ty) => match *ty {
                        Type::Named(type_id) => type_id,
                        _ => return Type::Unknown,
                    },
                    _ => return Type::Unknown,
                };

                let field = match &expr.member {
                    syn::Member::Named(ident) => ident,
                    _ => return Type::Unknown,
                };

                match self.declared_types.get(&type_id) {
                    Some(DeclaredType::Composite(fields)) => {
                        fields.get(&field.to_string()).cloned()
                    }
                    Some(DeclaredType::Function(_)) | None => None,
                }
                .unwrap_or(Type::Unknown)
            }
            Expr::Path(expr_path) => {
                let ident = match expr_path.path.get_ident() {
                    Some(ident) => ident.to_string(),
                    _ => return Type::Unknown,
                };

                let mut maybe_scope = self.exprs.get(&expr.into());

                while let Some(scope) = maybe_scope {
                    let info = match self.scopes.get(scope) {
                        Some(info) => info,
                        _ => break,
                    };

                    if let Some(ty) = info.vars.get(&ident) {
                        return ty.clone();
                    }

                    maybe_scope = info.parent.as_ref();
                }

                Type::Unknown
            }
            Expr::Call(expr) => match self.type_of(&*expr.func) {
                Type::Function(ty) => *ty,
                _ => Type::Unknown,
            },
            Expr::Cast(expr) => assign_type(&self.type_names, &*expr.ty),
            Expr::Group(expr) => self.type_of(&*expr.expr),
            Expr::Paren(expr) => self.type_of(&*expr.expr),
            Expr::Reference(expr) => Type::Ref(self.type_of(&*expr.expr).into()),
            Expr::Unary(expr) => match expr.op {
                syn::UnOp::Deref(_) => {
                    let ty = self.type_of(&*expr.expr);
                    match ty {
                        Type::Ref(ty) => *ty,
                        _ => Type::Unknown,
                    }
                }
                _ => Type::Irrelevant,
            },

            Expr::Block(_) => Type::Irrelevant,
            Expr::Array(_)
            | Expr::Assign(_)
            | Expr::AssignOp(_)
            | Expr::Async(_)
            | Expr::Await(_)
            | Expr::Binary(_)
            | Expr::Box(_)
            | Expr::Break(_)
            | Expr::Closure(_)
            | Expr::Continue(_)
            | Expr::ForLoop(_)
            | Expr::If(_)
            | Expr::Let(_)
            | Expr::Lit(_)
            | Expr::Loop(_)
            | Expr::Macro(_)
            | Expr::Match(_)
            | Expr::MethodCall(_)
            | Expr::Range(_)
            | Expr::Repeat(_)
            | Expr::Return(_)
            | Expr::Struct(_)
            | Expr::Try(_)
            | Expr::TryBlock(_)
            | Expr::Tuple(_)
            | Expr::Type(_)
            | Expr::Unsafe(_)
            | Expr::Verbatim(_)
            | Expr::While(_)
            | Expr::Yield(_) => Type::Irrelevant,
            Expr::__TestExhaustive(_) => unreachable!(),
        }
    }
}

impl Default for TypeInfo<'_> {
    fn default() -> Self {
        TypeInfo {
            cache: Default::default(),
            exprs: Default::default(),
            scopes: Default::default(),
            declared_types: Default::default(),
            type_names: StringInterner::new(),
        }
    }
}

pub(crate) fn check<'a>(file: &'a syn::File) -> Result<TypeInfo<'a>, CompilationErrors> {
    let mut info: TypeInfo = Default::default();
    let mut seen = HashSet::new();
    let mut errors = vec![];

    for item in &file.items {
        let (ident, fields) = match item {
            syn::Item::Union(item) => (&item.ident, &item.fields),
            syn::Item::Struct(item) => {
                let fields = if let syn::Fields::Named(fields) = &item.fields {
                    fields
                } else {
                    continue;
                };

                (&item.ident, fields)
            }
            syn::Item::Fn(item) => {
                let ident = &item.sig.ident;
                let ty = match &item.sig.output {
                    syn::ReturnType::Default => Type::Irrelevant,
                    syn::ReturnType::Type(_, ty) => assign_type(&info.type_names, ty),
                };

                let type_id = info.type_names.get_or_intern(ident.to_string());
                if !seen.insert(type_id) {
                    errors.push(Error {
                        msg: format!("Repeated declaration for {}", ident),
                        span: item.span(),
                    });
                }
                info.declared_types
                    .insert(type_id, DeclaredType::Function(ty));
                continue;
            }
            _ => continue,
        };

        let field_types = fields
            .named
            .iter()
            .filter_map(|field| {
                let ident = field.ident.as_ref()?;
                let ty = assign_type(&info.type_names, &field.ty);
                Some((ident.to_string(), ty))
            })
            .collect();
        let type_id = info.type_names.get_or_intern(ident.to_string());
        if !seen.insert(type_id) {
            errors.push(Error {
                msg: format!("Repeated declaration for {}", ident),
                span: item.span(),
            });
        }
        info.declared_types
            .insert(type_id, DeclaredType::Composite(field_types));
    }

    let global_scope: &'static syn::Block = Box::leak(
        syn::Block {
            brace_token: syn::token::Brace {
                span: proc_macro2::Span::call_site(),
            },
            stmts: vec![],
        }
        .into(),
    );

    let mut global_scope_info: ScopeInfo = Default::default();

    for (type_id, declared_type) in &info.declared_types {
        let fun = match declared_type {
            DeclaredType::Function(ty) => ty,
            _ => continue,
        };
        let fun_name = info.type_names.resolve(*type_id).unwrap();
        global_scope_info
            .vars
            .insert(fun_name, Type::Function(fun.clone().into()));
    }

    info.scopes.insert(global_scope.into(), global_scope_info);
    let mut visitor = TypeInfoVisitor {
        stack: vec![global_scope],
        exprs: &mut info.exprs,
        scopes: &mut info.scopes,
        type_names: &info.type_names,
    };

    visitor.visit_file(file);

    if errors.is_empty() {
        Ok(info)
    } else {
        Err(CompilationErrors { errors })
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeId(usize);

impl string_interner::Symbol for TypeId {
    fn try_from_usize(index: usize) -> Option<Self> {
        Some(Self(index))
    }

    fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
enum DeclaredType {
    Composite(HashMap<String, Type>),
    Function(Type),
}

#[derive(Clone, Debug)]
pub(crate) enum Type {
    Named(TypeId),
    Array(Box<Type>),
    Ref(Box<Type>),
    Function(Box<Type>),
    Irrelevant,
    Unknown,
}

struct TypeInfoVisitor<'ast, 'b> {
    stack: Vec<&'ast Block>,
    exprs: &'b mut HashMap<Ref<'ast, Expr>, Scope<'ast>>,
    scopes: &'b mut HashMap<Scope<'ast>, ScopeInfo<'ast>>,
    // items: HashMap<String, Type>,
    type_names: &'b StringInterner,
}

impl<'ast, 'b> Visit<'ast> for TypeInfoVisitor<'ast, 'b> {
    fn visit_block(&mut self, i: &'ast Block) {
        let scope = i.into();
        let mut scope_info = self.scopes.entry(scope).or_insert_with(Default::default);
        scope_info.parent = self.stack.last().map(|&block| block.into());
        self.stack.push(i);
        visit::visit_block(self, i);
        let _ = self.stack.pop();
    }

    fn visit_expr(&mut self, i: &'ast Expr) {
        if let Some(block) = self.stack.last() {
            self.exprs.insert(i.into(), (*block).into());
        }

        visit::visit_expr(self, i);
    }

    fn visit_local(&mut self, i: &'ast syn::Local) {
        loop {
            let block = if let Some(block) = self.stack.last() {
                block
            } else {
                break;
            };

            let scope = (*block).into();
            let (ident, ty) = match &i.pat {
                syn::Pat::Type(pat) => match &*pat.pat {
                    syn::Pat::Ident(ident) => (&ident.ident, &*pat.ty),
                    _ => break,
                },
                _ => break,
            };
            let type_ = assign_type(&mut self.type_names, ty);
            let scope_info = self.scopes.entry(scope).or_insert_with(Default::default);
            scope_info.vars.insert(ident.to_string(), type_);
            break;
        }

        visit::visit_local(self, i);
    }
}

fn assign_type(type_names: &StringInterner, ty: &syn::Type) -> Type {
    match ty {
        syn::Type::Path(ty) => {
            if let Some(ident) = ty.path.get_ident() {
                let type_id = type_names.get_or_intern(ident.to_string());
                Type::Named(type_id)
            } else {
                Type::Irrelevant
            }
        }
        syn::Type::Array(ty) => Type::Array(assign_type(type_names, &ty.elem).into()),
        syn::Type::BareFn(ty) => match &ty.output {
            syn::ReturnType::Default => Type::Irrelevant,
            syn::ReturnType::Type(_, ty) => Type::Function(assign_type(type_names, &*ty).into()),
        },
        syn::Type::Group(ty) => assign_type(type_names, &ty.elem),
        syn::Type::Paren(ty) => assign_type(type_names, &ty.elem),
        syn::Type::Reference(ty) => Type::Ref(assign_type(type_names, &ty.elem).into()),
        syn::Type::ImplTrait(_)
        | syn::Type::Infer(_)
        | syn::Type::Macro(_)
        | syn::Type::Never(_)
        | syn::Type::Ptr(_)
        | syn::Type::TraitObject(_)
        | syn::Type::Tuple(_)
        | syn::Type::Slice(_)
        | syn::Type::Verbatim(_) => Type::Irrelevant,
        syn::Type::__TestExhaustive(_) => unreachable!(),
    }
}
