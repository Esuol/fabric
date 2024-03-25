
use std::iter::FromIterator;

use proc_macro::{Span, TokenStream};
use quote::{__private::Span, quote}
use syn::{
  parse_macro_input,
  punctuated::Punctuated,
  token::{Brace, Paren},
  Abi, AngleBracketedGenericArguments, BareFnArg,  Block, Expr, ExprCall, ExprCast,
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

fn punctuated<A,B>(iter: impl IntoIterator<Item=A>) -> Punctuated<A,B>
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