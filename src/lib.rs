//! A syntactic for loop.
//!
//! For example, the following takes the sum of the bit-length of four integer
//! types:
//! ```
//! # use syntactic_for::syntactic_for;
//! let sum = syntactic_for!{ ty in [ u8, u16, u32, u64 ] {
//!     [$( <$ty>::BITS ),*].into_iter().sum::<u32>()
//! }};
//! assert_eq!(sum, 120);
//! ```
//!
//! # Usage
//!
//! The syntax is as follows:
//! ```ignore
//! syntactic_for!{ IDENTIFIER in [ EXPRESSION, EXPRESSION, ... ] {
//!     BODY
//! }}
//! ```
//! where `BODY` works similarly to `macro_rules!`, that is:
//! `$($IDENTIFIER)SEPARATOR*` will expand and substitute `IDENTIFIER` with
//! each `EXPRESSION`, separating the expansions with `SEPARATOR`.
//!
//! `SEPARATOR` can be any non-`*` punctuation.  Hence, the example from above
//! could also be written without an iterator:
//! ```
//! # use syntactic_for::syntactic_for;
//! # let sum =
//! # syntactic_for!{ ty in [ u8, u16, u32, u64 ] {
//! $( <$ty>::BITS )+*
//! # }};
//! # assert_eq!(sum, 120);
//! ```
//!
//! # Examples
//!
//! ## Loop unrolling
//!
//! Sum the elements of an array with
//! [loop unrolling](https://en.wikipedia.org/wiki/Loop_unrolling):
//! ```
//! # use syntactic_for::syntactic_for;
//! let array = b"oh my, I am getting summed!";
//! let mut acc = 0u32;
//! let mut i = 0;
//! while i <= array.len()-4 {
//!     syntactic_for!{ offset in [ 0, 1, 2, 3 ] {$(
//!         acc += array[i + $offset] as u32;
//!     )*}}
//!     i += 4;
//! }
//! for j in i..array.len() {
//!     acc += array[j] as u32;
//! }
//! assert_eq!(acc, 2366);
//! ```
//!
//! ## Matching
//!
//! Find the maximum value of an integer type of the given bit size:
//! ```
//! # use syntactic_for::syntactic_for;
//! # let bit_size = 16;
//! let max_size = syntactic_for!{ ty in [ u8, u16, u32, u64, u128 ] {
//!     match bit_size {
//!         $(<$ty>::BITS => <$ty>::MAX as u128,)*
//!         other => panic!("No integer of size {other}"),
//!     }
//! }};
//! # assert_eq!(max_size, u16::MAX as u128)
//! ```
//!
//! ## `impl` blocks
//!
//! Implement a trait for a set of types:
//! ```
//! # use syntactic_for::syntactic_for;
//! # trait MyTrait {}
//! syntactic_for!{ ty in [ u8, u16, u32, u64, u128 ] {$(
//!     impl MyTrait for $ty {
//!         // snip.
//!     }
//! )*}}
//! ```
//!
//! ## Custom syntactic loop
//!
//! A useful design pattern is to define a custom macro that expands to a
//! syntactic loop over a given set of expressions:
//! ```
//! # struct CustomType1;
//! # struct CustomType2;
//! #[doc(hidden)]
//! pub extern crate syntactic_for;
//!
//! #[macro_export]
//! macro_rules! for_each_custom_type {
//!     ($ident:ident { $($tt:tt)* }) => {
//!         $crate::syntactic_for::syntactic_for! { $ident in [
//!             $crate::CustomType1,
//!             $crate::CustomType2,
//!             // etc.
//!         ] { $($tt)* } }
//!     }
//! }
//! ```
//!
//! For example, a library could expose `for_each_custom_type` as a way of
//! letting its users write syntactic loops over a set of types defined in the
//! library.  Then, it becomes possible to add types to that loop inside the
//! library, whithout requiring any change on the user's end:
//!
//! ```
//! # struct CustomType1;
//! # impl CustomType1 { fn parse(i: &str) -> Result<(), ()> { Err(()) }}
//! # struct CustomType2;
//! # impl CustomType2 { fn parse(i: &str) -> Result<(), ()> { Ok(()) }}
//! # pub extern crate syntactic_for;
//! # #[macro_export]
//! # macro_rules! for_each_custom_type {
//! #     ($ident:ident { $($tt:tt)* }) => {
//! #         ::syntactic_for::syntactic_for! { $ident in [
//! #             CustomType1,
//! #             CustomType2,
//! #             // etc.
//! #         ] { $($tt)* } }
//! #     }
//! # }
//! # mod my_library { pub use for_each_custom_type; }
//! // Try and parse each library type in succession, stopping at the first
//! // success:
//! fn can_parse(input: &str) -> bool {
//!     my_library::for_each_custom_type! { ty {
//!         $(if let Ok(parsed) = <$ty>::parse(input) {
//!             return true;
//!         })*
//!     }}
//!     return false;
//! }
//! # assert_eq!(can_parse("foo"), true);
//! ```
use proc_macro2::{TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    braced, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, Expr, Ident, Token,
};

struct SyntacticFor {
    ident: Ident,
    _in_token: Token![in],
    _bracket_token: token::Bracket,
    exprs: Punctuated<Expr, Token![,]>,
    _brace_token: token::Brace,
    body: TokenStream,
}

impl Parse for SyntacticFor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let brackets;
        let braces;
        Ok(SyntacticFor {
            ident: input.parse()?,
            _in_token: input.parse()?,
            _bracket_token: bracketed!(brackets in input),
            exprs: Punctuated::parse_terminated(&brackets)?,
            _brace_token: braced!(braces in input),
            body: braces.parse()?,
        })
    }
}

fn subs_group<'a, S, IntoIter>(
    pattern: &Ident,
    subs: &'a S,
    tokens: TokenStream,
) -> syn::Result<TokenStream>
where
    &'a S: IntoIterator<IntoIter = IntoIter> + Clone + 'a,
    IntoIter: ExactSizeIterator,
    <IntoIter as Iterator>::Item: ToTokens,
{
    let mut output = TokenStream::new();
    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == '$' => match tokens.next() {
                Some(TokenTree::Group(group)) => {
                    let separator = match tokens.peek() {
                        Some(TokenTree::Punct(punct)) if punct.as_char() == '*' => None,
                        Some(TokenTree::Punct(_)) => {
                            if let TokenTree::Punct(punct) = tokens.next().unwrap() {
                                Some(punct)
                            } else {
                                unreachable!()
                            }
                        }
                        Some(token) => {
                            return Err(syn::Error::new_spanned(
                                token,
                                format!("expected punctuation or `*`, found {}", token),
                            ))
                        }
                        None => panic!("unexpected end of stream after group"),
                    };
                    match tokens.next() {
                        Some(TokenTree::Punct(punct)) if punct.as_char() == '*' => {}
                        Some(token) => {
                            return Err(syn::Error::new_spanned(
                                &token,
                                format!("expected `*`, found {}", token),
                            ))
                        }
                        None => panic!("unexpected end of stream after group"),
                    }

                    let subs = subs.into_iter();
                    let len = subs.len();
                    for (i, sub) in subs.enumerate() {
                        output.extend(subs_ident(pattern, &sub, group.stream())?);
                        if i + 1 < len {
                            if let Some(separator) = &separator {
                                output.append(separator.clone());
                            }
                        }
                    }
                }
                Some(token) => {
                    return Err(syn::Error::new_spanned(
                        &token,
                        format!("expected group after `$`, found `{}`", token),
                    ))
                }
                None => {
                    panic!("unexpected end of stream after `$`")
                }
            },
            TokenTree::Group(group) => {
                output.append(proc_macro2::Group::new(
                    group.delimiter(),
                    subs_group(pattern, subs, group.stream())?,
                ));
            }
            token => output.append(token),
        }
    }
    Ok(output)
}

fn subs_ident<'a, I>(pattern: &Ident, sub: &'a I, tokens: TokenStream) -> syn::Result<TokenStream>
where
    &'a I: ToTokens,
{
    let mut output = TokenStream::new();
    let mut tokens = tokens.into_iter();
    while let Some(token) = tokens.next() {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == '$' => match tokens.next() {
                Some(TokenTree::Ident(ident)) if &ident == pattern => {
                    sub.to_tokens(&mut output);
                }
                Some(token) => {
                    return Err(syn::Error::new_spanned(
                        &token,
                        format!("expected `{}` after `$`, found `{}`", pattern, token),
                    ))
                }
                None => {
                    panic!("unexpected end of stream after `$`")
                }
            },
            TokenTree::Group(group) => {
                output.append(proc_macro2::Group::new(
                    group.delimiter(),
                    subs_ident(pattern, sub, group.stream())?,
                ));
            }
            token => output.append(token),
        }
    }
    Ok(output)
}

/// Iterate over a list of (syntactic) expressions.
///
/// For details, see [top level documentation][crate].
#[proc_macro]
pub fn syntactic_for(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let SyntacticFor {
        ident, exprs, body, ..
    } = parse_macro_input!(input as SyntacticFor);

    subs_group(&ident, &exprs, body)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
