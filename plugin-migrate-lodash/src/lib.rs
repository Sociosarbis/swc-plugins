use std::collections::HashSet;

use swc_core::common::util::take::Take;
use swc_core::common::Span;
use swc_core::ecma::ast::{ImportNamedSpecifier, JSXExpr};
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};
use swc_core::{
    atoms::Atom,
    ecma::{
        ast::{
            CallExpr, Callee, Expr, ExprOrSpread, Ident, IdentName, ImportDecl,
            ImportDefaultSpecifier, ImportSpecifier, ImportStarAsSpecifier, KeyValueProp,
            MemberProp, ModuleDecl, ModuleItem, Program, PropOrSpread,
        },
        transforms::testing::test_inline,
        visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
    },
};
use swc_ecma_parser::{Syntax, TsSyntax};

#[derive(Default)]
pub struct TransformVisitor {
    lodash_name: String,
    used_apis: HashSet<String>,
    static_expr: Option<Box<Expr>>,
    find_stopped: bool,
}

static LODASH_NAME: &'static str = "lodash";
static LODASH_ES_NAME: &'static str = "lodash-es";
static VALUE_PROP: &'static str = "value";

impl TransformVisitor {
    fn take_static_expr(&mut self) -> Box<Expr> {
        self.static_expr.take().unwrap()
    }

    fn find_static_lodash_call(&mut self, node: &mut swc_core::ecma::ast::CallExpr) {
        self.find_stopped = false;
        match &mut node.callee {
            Callee::Expr(expr) => match expr.as_mut() {
                Expr::Member(expr) => match expr.obj.as_mut() {
                    Expr::Call(obj_call) => {
                        if self.static_expr.is_none() {
                            self.find_static_lodash_call(obj_call);
                            if self.static_expr.is_some() {
                                match &mut expr.prop {
                                    MemberProp::Ident(IdentName { span, sym }) => {
                                        if sym == VALUE_PROP {
                                            self.find_stopped = true;
                                        } else {
                                            if self.find_stopped {
                                                self.find_stopped = false;
                                                expr.obj = self.take_static_expr()
                                            } else {
                                                self.used_apis.insert(sym.to_string());
                                                let mut args = node.args.clone();
                                                args.insert(
                                                    0,
                                                    ExprOrSpread {
                                                        spread: None,
                                                        expr: self.take_static_expr(),
                                                    },
                                                );
                                                self.static_expr =
                                                    Some(Box::new(Expr::Call(CallExpr {
                                                        span: node.span,
                                                        callee: Callee::Expr(Box::new(
                                                            Expr::Ident(Ident::new_no_ctxt(
                                                                sym.clone(),
                                                                *span,
                                                            )),
                                                        )),
                                                        args,
                                                        ..CallExpr::default()
                                                    })));
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {}
                },
                // _([])
                Expr::Ident(Ident { sym, .. })
                    if *sym == self.lodash_name && !node.args.is_empty() =>
                {
                    self.static_expr = Some(node.args[0].expr.clone());
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn update_call(&mut self, node: &mut Box<swc_core::ecma::ast::Expr>) {
        if self.static_expr.is_some() {
            assert_eq!(self.find_stopped, true, "must ends with .value()");
            *node = self.take_static_expr();
        }
    }
}

impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html
    fn visit_mut_module(&mut self, node: &mut swc_core::ecma::ast::Module) {
        let lodash_imports: Vec<_> = node
            .body
            .iter()
            .filter_map(|m| {
                match m {
                    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                        specifiers,
                        src,
                        ..
                    })) if src.value == LODASH_NAME => {
                        if self.lodash_name.is_empty() {
                            specifiers.iter().find(|s| match s {
                                ImportSpecifier::Default(ImportDefaultSpecifier {
                                    local, ..
                                })
                                | ImportSpecifier::Namespace(ImportStarAsSpecifier {
                                    local, ..
                                }) => {
                                    self.lodash_name = local.sym.to_string();
                                    true
                                }
                                ImportSpecifier::Named(ImportNamedSpecifier { local, .. }) => {
                                    self.used_apis.insert(local.sym.to_string());
                                    false
                                }
                            });
                        }
                        return Some(m.as_module_decl().unwrap().as_import().unwrap());
                    }
                    _ => {}
                }
                None
            })
            .collect();
        if lodash_imports.is_empty() {
            return;
        }
        node.body.visit_mut_children_with(self);
        let mut met = false;
        node.body.retain_mut(|m| match m {
            ModuleItem::ModuleDecl(ModuleDecl::Import(import_decl))
                if import_decl.src.value == LODASH_NAME
                    || import_decl.src.value == LODASH_ES_NAME =>
            {
                if !met && !self.used_apis.is_empty() {
                    met = true;
                    import_decl.src.value = LODASH_ES_NAME.into();
                    import_decl.src.raw = None;
                    let mut apis: Vec<&String> = self.used_apis.iter().collect();
                    apis.sort();
                    import_decl.specifiers = apis
                        .iter()
                        .map(|name| {
                            ImportSpecifier::Named(ImportNamedSpecifier {
                                span: Span::dummy(),
                                local: Ident::new_no_ctxt(Atom::new(name.as_str()), Span::dummy()),
                                imported: None,
                                is_type_only: false,
                            })
                        })
                        .collect();
                    true
                } else {
                    false
                }
            }
            _ => true,
        });
    }

    fn visit_mut_var_declarator(&mut self, node: &mut swc_core::ecma::ast::VarDeclarator) {
        if let Some(expr) = &mut node.init {
            if expr.is_call() {
                self.find_static_lodash_call(expr.as_mut_call().unwrap());
                self.update_call(expr);
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_expr_stmt(&mut self, node: &mut swc_core::ecma::ast::ExprStmt) {
        if node.expr.is_call() {
            self.find_static_lodash_call(node.expr.as_mut_call().unwrap());
            self.update_call(&mut node.expr);
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_jsx_expr_container(&mut self, node: &mut swc_core::ecma::ast::JSXExprContainer) {
        if let JSXExpr::Expr(expr) = &mut node.expr {
            if expr.is_call() {
                self.find_static_lodash_call(expr.as_mut_call().unwrap());
                self.update_call(expr);
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_expr(&mut self, node: &mut swc_core::ecma::ast::Expr) {
        match node {
            Expr::Call(call_expr) => {
                match &mut call_expr.callee {
                    Callee::Expr(expr) => match expr.as_mut() {
                        Expr::Member(expr) => match expr.obj.as_mut() {
                            Expr::Call(inner_call_expr) => {
                                if inner_call_expr.callee.is_expr() {
                                    self.find_static_lodash_call(inner_call_expr);
                                    self.update_call(inner_call_expr.callee.as_mut_expr().unwrap());
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                }
                for ExprOrSpread { expr, .. } in &mut call_expr.args {
                    if expr.is_call() {
                        self.find_static_lodash_call(expr.as_mut_call().unwrap());
                        self.update_call(expr);
                    }
                }
            }
            Expr::Object(obj_expr) => {
                for prop in &mut obj_expr.props {
                    match prop {
                        PropOrSpread::Prop(p) => {
                            if let Some(KeyValueProp { value, .. }) = p.as_mut_key_value() {
                                if value.is_call() {
                                    self.find_static_lodash_call(value.as_mut_call().unwrap());
                                    self.update_call(value);
                                }
                            }
                        }
                        PropOrSpread::Spread(s) => {
                            if s.expr.is_call() {
                                self.find_static_lodash_call(s.expr.as_mut_call().unwrap());
                                self.update_call(&mut s.expr);
                            }
                        }
                    }
                }
            }
            Expr::Array(arr_expr) => {
                for elem in &mut arr_expr.elems {
                    if let Some(ExprOrSpread { expr, .. }) = elem {
                        if expr.is_call() {
                            self.find_static_lodash_call(expr.as_mut_call().unwrap());
                            self.update_call(expr);
                        }
                    }
                }
            }
            Expr::Assign(assgin_expr) => {
                if assgin_expr.right.is_call() {
                    self.find_static_lodash_call(assgin_expr.right.as_mut_call().unwrap());
                    self.update_call(&mut assgin_expr.right);
                }
            }
            Expr::Member(mem_expr) => match mem_expr.obj.as_mut() {
                Expr::Ident(ident) if ident.sym == self.lodash_name => {
                    if let MemberProp::Ident(IdentName { sym, span }) = &mem_expr.prop {
                        self.used_apis.insert(sym.to_string());
                        *node = Expr::Ident(Ident::new_no_ctxt(sym.clone(), *span));
                        return;
                    }
                }
                _ => {}
            },
            _ => {}
        }
        node.visit_mut_children_with(self);
    }
}

/// An example plugin function with macro support.
/// `plugin_transform` macro interop pointers into deserialized structs, as well
/// as returning ptr back to host.
///
/// It is possible to opt out from macro by writing transform fn manually
/// if plugin need to handle low-level ptr directly via
/// `__transform_plugin_process_impl(
///     ast_ptr: *const u8, ast_ptr_len: i32,
///     unresolved_mark: u32, should_enable_comments_proxy: i32) ->
///     i32 /*  0 for success, fail otherwise.
///             Note this is only for internal pointer interop result,
///             not actual transform result */`
///
/// This requires manual handling of serialization / deserialization from ptrs.
/// Refer swc_plugin_macro to see how does it work internally.
#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor::default()))
}

// An example to test plugin transform.
// Recommended strategy to test plugin's transform is verify
// the Visitor's behavior, instead of trying to run `process_transform` with mocks
// unless explicitly required to do so.
test_inline!(
    Syntax::Typescript(TsSyntax {
        tsx: true,
        ..Default::default()
    }),
    |_| as_folder(TransformVisitor::default()),
    boo,
    // Input codes
    r#"
    import _ from "lodash";
    var a = [_([]).filter((i) => _.isFinite(i.label)).mapValues((i) => i.key).value()]
    _.isNil;
    const a = <>{_([]).map((i) => i.label).value()}</>
    "#,
    // Output codes after transformed with plugin
    r#"import { filter, isFinite, isNil, map, mapValues } from "lodash-es";
    var a = [
        mapValues(filter([], (i)=>isFinite(i.label)), (i)=>i.key)
    ]
    isNil
    const a = <>{map([], (i)=>i.label)}</>;
    "#
);
