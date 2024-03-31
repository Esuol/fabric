use std::iter::FromIterator;

use proc_macro::TokenStream;
use quote::{__private::Span, quote};
use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    token::{Brace, Paren},
    Abi, AngleBracketedGenericArguments, BareFnArg, Binding, Block, Expr, ExprCall, ExprCast,
    ExprField, ExprParen, ExprPath, ExprReference, ExprStruct, ExprUnary, ExprUnsafe, Field,
    FieldValue, FnArg, GenericArgument, GenericParam, Generics, Ident, ImplItem, ImplItemMethod,
    Item, ItemFn, ItemImpl, ItemTrait, LitStr, Member, Pat, PatPath, PatType, Path, PathArguments,
    PathSegment, Receiver, ReturnType, Signature, Stmt, Token, TraitBound, TraitBoundModifier,
    TraitItem, TraitItemMethod, Type, TypeBareFn, TypeParam, TypeParamBound, TypePath, TypePtr,
    TypeTraitObject, UnOp, VisPublic, VisRestricted, Visibility,
};

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

fn punctuated<A, B>(iter: impl IntoIterator<Item = A>) -> Punctuated<A, B>
where
    Punctuated<A, B>: FromIterator<A>,
{
    Punctuated::from_iter(iter)
}

fn generics_argument(
    args: impl IntoIterator<Item = GenericArgument>,
) -> AngleBracketedGenericArguments {
    AngleBracketedGenericArguments {
        lt_token: Token![<](Span::call_site()),
        args: punctuated(args),
        colon2_token: None,
        gt_token: Token![>](Span::call_site()),
    }
}

fn segment(ident: Ident, arguments: Option<AngleBracketedGenericArguments>) -> PathSegment {
    PathSegment {
        ident,
        arguments: if let Some(arguments) = arguments {
            PathArguments::AngleBracketed(arguments)
        } else {
            PathArguments::None
        },
    }
}

fn path(segments: impl IntoIterator<Item = PathSegment>) -> Path {
    Path {
        leading_colon: None,
        segments: punctuated(segments),
    }
}

fn path_type(segments: impl IntoIterator<Item = PathSegment>) -> TypePath {
    TypePath {
        qself: None,
        path: path(segments),
    }
}

fn pointer_type(mutability: Option<Token![mut]>, ty: Type) -> TypePtr {
    TypePtr {
        star_token: Token![*]([Span::call_site(); 1]),
        const_token: if mutability.is_none() {
            Some(Token![const](Span::call_site()))
        } else {
            None
        },
        mutability,
        elem: Box::new(ty),
    }
}

fn map_type(input: &Type) -> Type {
    match input {
        Type::Reference(reference) => match &*reference.elem {
            Type::Path(path) => {
                if let Some(seg) = path.path.segments.last() {
                    match &seg.ident.to_string() as &str {
                        "CStr" => {
                            return Type::Ptr(pointer_type(
                                reference.mutability.clone(),
                                Type::Path(path_type(vec![
                                    segment(ident("std"), None),
                                    segment(ident("os"), None),
                                    segment(ident("raw"), None),
                                    segment(ident("c_char"), None),
                                ])),
                            ));
                        }
                        _ => {}
                    }
                }
            }

            Type::TraitObject(_) => {
                return Type::Ptr(pointer_type(
                    reference.mutability.clone(),
                    Type::Path(path_type(vec![
                        segment(ident("std"), None),
                        segment(ident("ffi"), None),
                        segment(ident("c_void"), None),
                    ])),
                ))
            }

            _ => {}
        },

        Type::Path(path) => {
            if let Some(seg) = path.path.segments.last() {
                match &seg.ident.to_string() as &str {
                    "Box" => {
                        let args = match &seg.arguments {
                            PathArguments::AngleBracketed(args) => args,
                            other => panic!("{:?}", other),
                        };

                        let arg = match &args.args[0] {
                            GenericArgument::Type(arg) => arg,
                            other => panic!("{:?}", other),
                        };

                        match arg {
                            Type::TraitObject(_) => {
                                return Type::Ptr(pointer_type(
                                    Some(Token![mut](Span::call_site())),
                                    Type::Path(path_type(vec![
                                        segment(ident("std"), None),
                                        segment(ident("ffi"), None),
                                        segment(ident("c_void"), None),
                                    ])),
                                ));
                            }

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }

        _ => {}
    }

    input.clone()
}

fn map_input(input: Expr, ty: &Type) -> Expr {
    match ty {
        Type::Reference(reference) => match &*reference.elem {
            Type::Path(pat) => {
                if let Some(seg) = pat.path.segments.last() {
                    if seg.ident.to_string() == "CStr" {
                        return Expr::Call(ExprCall {
                            attrs: Vec::new(),
                            func: Box::new(Expr::Field(ExprField {
                                attrs: Vec::new(),
                                base: Box::new(input),
                                dot_token: Token![.](Span::call_site()),
                                member: Member::Named(ident("as_ptr")),
                            })),
                            paren_token: Paren(Span::call_site()),
                            args: Punctuated::new(),
                        });
                    }
                }
            }

            Type::TraitObject(obj) => {
                let bound = match &obj.bounds[0] {
                    TypeParamBound::Trait(bound) => bound,
                    other => panic!("{:?}", other),
                };

                let name = match bound.path.segments.last() {
                    Some(segment) => &segment.ident,
                    None => panic!(),
                };

                let vtable_name = Ident::new(&format!("I{}", name), Span::call_site());
                let class_name = Ident::new(&format!("C{}", name), Span::call_site());
                let ty = ty.clone();

                // TODO: Move the static vtable somewhere it can be shared
                // between all invocations so they do not bloat the binary
                return Expr::Verbatim(quote! {{
                    static VTABLE: #vtable_name = #name::vtable::<#ty, _>();

                    let instance = Box::new(#class_name {
                        vtable: &VTABLE as *const #vtable_name,
                        instance: #input
                    });

                    let ptr = Box::into_raw(instance);
                    log::trace!(concat!("into_raw ", stringify!(#class_name), " {:?}"), ptr);
                    ptr as *mut std::ffi::c_void
                }});
            }

            _ => {}
        },

        Type::Path(pat) => {
            if let Some(seg) = pat.path.segments.last() {
                match &seg.ident.to_string() as &str {
                    "Box" => {
                        let args = match &seg.arguments {
                            PathArguments::AngleBracketed(args) => args,
                            other => panic!("{:?}", other),
                        };

                        if let GenericArgument::Type(Type::TraitObject(obj)) = &args.args[0] {
                            let bound = match &obj.bounds[0] {
                                TypeParamBound::Trait(bound) => bound,
                                other => panic!("{:?}", other),
                            };

                            let name = match bound.path.segments.last() {
                                Some(segment) => &segment.ident,
                                None => panic!(),
                            };

                            let vtable_name = Ident::new(&format!("I{}", name), Span::call_site());
                            let class_name = Ident::new(&format!("C{}", name), Span::call_site());
                            let ty = ty.clone();

                            return Expr::Verbatim(quote! {{
                                static VTABLE: #vtable_name = #name::vtable::<#ty, _>();
                                let instance = Box::new(#class_name {
                                    vtable: &VTABLE as *const #vtable_name,
                                    instance: #input
                                });

                                let ptr = Box::into_raw(instance);
                                log::trace!(concat!("into_raw ", stringify!(#class_name), " {:?}"), ptr);
                                ptr as *mut std::ffi::c_void
                            }});
                        }
                    }
                    _ => {}
                }
            }
        }

        _ => {}
    }

    input
}

fn map_output(input: Expr, ty: &Type) -> Expr {
    match ty {
        Type::Reference(reference) => match &*reference.elem {
            Type::Path(pat) => {
                if let Some(seg) = pat.path.segments.last() {
                    match &seg.ident.to_string() as &str {
                        "CStr" => {
                            return Expr::Call(ExprCall {
                                attrs: Vec::new(),
                                func: Box::new(Expr::Path(ExprPath {
                                    attrs: Vec::new(),
                                    qself: None,
                                    path: path(vec![
                                        segment(ident("std"), None),
                                        segment(ident("ffi"), None),
                                        segment(ident("CStr"), None),
                                        segment(ident("from_ptr"), None),
                                    ]),
                                })),
                                paren_token: Paren(Span::call_site()),
                                args: punctuated(vec![input]),
                            });
                        }
                        _ => {}
                    }
                }
            }

            Type::TraitObject(obj) => {
                return Expr::Verbatim(quote! {
                    &mut crate::foreign::Foreign::<#obj>::with(#input)
                })
            }

            _ => {}
        },

        Type::Path(pat) => {
            if let Some(seg) = pat.path.segments.last() {
                match &seg.ident.to_string() as &str {
                    "Box" => {
                        let args = match &seg.arguments {
                            PathArguments::AngleBracketed(args) => args,
                            other => panic!("{:?}", other),
                        };

                        let arg = match &args.args[0] {
                            GenericArgument::Type(arg) => arg,
                            other => panic!("{:?}", other),
                        };

                        match arg {
                            Type::TraitObject(obj) => {
                                return Expr::Verbatim(quote! {
                                    Box::new(crate::foreign::Foreign::<#obj>::with(#input))
                                });
                            }

                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }

        _ => {}
    }

    input
}

fn map_self_output(input: &Receiver, class_name: &Ident) -> Expr {
    if input.reference.is_some() {
        Expr::Reference(ExprReference {
            attrs: Vec::new(),
            and_token: Token![&](Span::call_site()),
            raw: Default::default(),
            mutability: if input.mutability.is_some() {
                Some(Token![mut](Span::call_site()))
            } else {
                None
            },
            expr: Box::new(Expr::Field(ExprField {
                attrs: Vec::new(),
                base: Box::new(Expr::Paren(ExprParen {
                    attrs: Vec::new(),
                    paren_token: Paren(Span::call_site()),
                    expr: Box::new(Expr::Unary(ExprUnary {
                        attrs: Vec::new(),
                        op: UnOp::Deref(Token![*](Span::call_site())),
                        expr: Box::new(Expr::Paren(ExprParen {
                            attrs: Vec::new(),
                            paren_token: Paren(Span::call_site()),
                            expr: Box::new(Expr::Cast(ExprCast {
                                attrs: Vec::new(),
                                expr: Box::new(Expr::Path(ExprPath {
                                    attrs: Vec::new(),
                                    qself: None,
                                    path: path(vec![segment(ident("this"), None)]),
                                })),
                                as_token: Token![as](Span::call_site()),
                                ty: Box::new(Type::Ptr(pointer_type(
                                    input.mutability.clone(),
                                    Type::Path(path_type(vec![segment(
                                        class_name.clone(),
                                        Some(generics_argument(vec![GenericArgument::Type(
                                            Type::Path(TypePath {
                                                qself: None,
                                                path: path(vec![segment(ident("P"), None)]),
                                            }),
                                        )])),
                                    )])),
                                ))),
                            })),
                        })),
                    })),
                })),
                dot_token: Token![.](Span::call_site()),
                member: Member::Named(ident("instance")),
            })),
        })
    } else {
        Expr::Verbatim(quote! {
            *Box::from_raw(this as *mut #class_name<P>).instance
        })
    }
}
