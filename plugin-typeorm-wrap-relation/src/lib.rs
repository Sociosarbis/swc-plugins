use swc_core::ecma::ast::{ImportPhase, Str};
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};
use swc_core::{
    common::{util::take::Take, Span},
    ecma::{
        ast::{
            Ident, ImportDecl, ImportNamedSpecifier, ImportSpecifier, ModuleDecl, ModuleItem,
            Program, TsEntityName, TsType, TsTypeParamInstantiation, TsTypeRef,
        },
        transforms::testing::test_inline,
        visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
    },
};
use swc_ecma_parser::{Syntax, TsSyntax};

#[derive(Default)]
pub struct TransformVisitor {
    need_relation_type: bool,
}

static TYPEORM_NAME: &'static str = "typeorm";
static RELATION_TYPE_NAME: &'static str = "Relation";

impl TransformVisitor {
    fn build_import_specifier(&self, is_type_only: bool) -> ImportSpecifier {
        ImportSpecifier::Named(ImportNamedSpecifier {
            span: Span::dummy(),
            local: Ident::new_no_ctxt(RELATION_TYPE_NAME.into(), Span::dummy()),
            imported: None,
            is_type_only: is_type_only,
        })
    }
}

impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html
    fn visit_mut_module_items(&mut self, node: &mut Vec<swc_core::ecma::ast::ModuleItem>) {
        for item in node.iter_mut() {
            item.visit_mut_children_with(self);
        }
        if self.need_relation_type {
            let mut typeorm_import: Option<&mut ImportDecl> = None;
            for item in node.iter_mut() {
                match item {
                    ModuleItem::ModuleDecl(ModuleDecl::Import(decl))
                        if decl.src.value == TYPEORM_NAME =>
                    {
                        for item in decl.specifiers.iter_mut() {
                            match item {
                                ImportSpecifier::Named(ImportNamedSpecifier {
                                    local,
                                    is_type_only,
                                    ..
                                }) if local.sym == RELATION_TYPE_NAME => {
                                    if !decl.type_only && !*is_type_only {
                                        *is_type_only = true;
                                    }
                                    return;
                                }
                                _ => {}
                            };
                        }
                        if typeorm_import.is_none() {
                            typeorm_import.replace(decl);
                        }
                    }
                    _ => {}
                }
            }
            if let Some(import) = typeorm_import {
                import
                    .specifiers
                    .push(self.build_import_specifier(if import.type_only { false } else { true }));
            } else {
                node.insert(
                    0,
                    ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                        span: Span::dummy(),
                        specifiers: vec![self.build_import_specifier(false)],
                        src: Box::new(Str::from(TYPEORM_NAME)),
                        type_only: true,
                        with: None,
                        phase: ImportPhase::Source,
                    })),
                );
            }
        }
    }

    fn visit_mut_class_prop(&mut self, node: &mut swc_core::ecma::ast::ClassProp) {
        if !node.decorators.is_empty() {
            if let Some(ann) = &mut node.type_ann {
                if let TsType::TsTypeRef(TsTypeRef {
                    type_name: name,
                    type_params: params,
                    span,
                }) = ann.type_ann.as_ref()
                {
                    if params.is_none() && name.is_ident() {
                        ann.type_ann = Box::new(TsType::TsTypeRef(TsTypeRef {
                            span: *span,
                            type_name: TsEntityName::Ident(Ident::new_private(
                                RELATION_TYPE_NAME.into(),
                                *span,
                            )),
                            type_params: Some(Box::new(TsTypeParamInstantiation {
                                span: *span,
                                params: vec![Box::new(TsType::TsTypeRef(TsTypeRef {
                                    span: *span,
                                    type_name: name.clone(),
                                    type_params: None,
                                }))],
                            })),
                        }));
                        self.need_relation_type = true;
                    }
                }
            }
        }
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
        decorators: true,
        ..TsSyntax::default()
    }),
    |_| as_folder(TransformVisitor::default()),
    boo,
    // Input codes
    r#"
    import { B } from './b';
    import { Entity, ManyToOne } from 'typeorm';
    
    @Entity()
    class A {
        @ManyToOne(() => B)
        b: B;
    };"#,
    // Output codes after transformed with plugin
    r#"
    import { B } from './b';
    import { Entity, ManyToOne, type Relation } from 'typeorm';
    @Entity()
    class A {
        @ManyToOne(() => B)
        b: Relation<B>;
    };"#
);
