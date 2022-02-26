use proc_macro::TokenStream;
use std::collections::{HashMap, VecDeque};
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, AttributeArgs, FnArg, ItemFn, ReturnType, Type, Token, Path, Pat, TypeImplTrait, Stmt, NestedMeta, Meta, PathSegment, PathArguments};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::parse_quote::ParseQuote;
use syn::{Expr};
use syn::spanned::Spanned;

fn recurse(expr: &mut Expr) {
    match expr {
        Expr::If(expr) => {
            if let Some(s) = &mut expr.else_branch {
                recurse(s.1.as_mut());
            }
            for stmt in &mut expr.then_branch.stmts {
                if let syn::Stmt::Expr(expr) = stmt {
                    recurse(expr);
                }
            }
        },
        Expr::Match(expr) => {
            for arm in &mut expr.arms {
                recurse(&mut arm.body);
            }
        },
        Expr::Loop(expr) => {
            for stmt in &mut expr.body.stmts {
                if let syn::Stmt::Expr(expr) = stmt {
                    recurse(expr);
                }
            }
        },
        Expr::ForLoop(expr) => {
            for stmt in &mut expr.body.stmts {
                if let syn::Stmt::Expr(expr) = stmt {
                    recurse(expr);
                }
            }
        },
        _ => {
            let tk = quote!{#expr.into()};
            let rt = match syn::parse::<syn::Expr>(TokenStream::from(tk)) {
                Ok(syntax_tree) => syntax_tree,
                Err(err) => panic!("{}", err),
            };

            *expr = rt;
        }
    }
}

fn wrap_return_type(func: &mut ItemFn) {
    for stmt in &mut func.block.stmts {
        let stm_str = stmt.to_token_stream().to_string();
        let is_return =  !stm_str.ends_with(";") || stm_str.starts_with("return");

        if !is_return {
            continue;
        }

        if let syn::Stmt::Expr(expr) = stmt {
            recurse(expr);
        }
    }
}

struct Variants {
    variants: Punctuated<Type, Token![|]>
}

impl Parse for Variants {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let variants: Punctuated<Type, Token![|]> = Punctuated::parse(&input)?;
        Ok(Self{variants})
    }
}

#[proc_macro_attribute]
pub fn ano_enum(_: TokenStream, item: TokenStream) -> TokenStream {
    let mut func = parse_macro_input!(item as ItemFn);

    let (e, vars_map) = match make_arg(&mut func) {
        Ok((enums, vars, vars_map)) => {
            let mut stmt: VecDeque<Stmt> = func.block.stmts.clone().into();
            for var in vars {
                let tk: proc_macro2::TokenStream = var.into();
                let rt = match syn::parse::<Stmt>(TokenStream::from(tk)) {
                    Ok(syntax_tree) => syntax_tree,
                    Err(err) => panic!("{}", err),
                };
                stmt.push_front(rt);
            }

            func.block.stmts = stmt.into();

            (quote!{
                #(#enums)*
            }, vars_map)
        }
        Err(e) => panic!("{}", e)
    };

    make_match(&mut func, vars_map);

    let enum_def =  match make_return_type(&mut func) {
        Ok((var_tokens, impls, enum_name, gens)) => {
            quote!{
                #[allow(non_camel_case_types)]
                pub enum #enum_name #gens {
                    #(#var_tokens),*
                }

                #(#impls)*
            }
        }
        Err(e) => quote!{}
    };

    wrap_return_type(&mut func);


    let out = quote!{
        #enum_def
        #e
        #func
    };
    //panic!("{}", out.to_token_stream().to_string());
    // let args = parse_macro_input!(args as AttributeArgs);
    //
    // for arg in args {
    //     match arg {
    //         NestedMeta::Meta(m) => {
    //             match m {
    //                 Meta::Path(ml) => {
    //                     panic!("{}", ml.to_token_stream().to_string());
    //                 }
    //                 _ => {}
    //             }
    //         }
    //         _ => {}
    //     }
    // }

    out.into()
}

fn make_arg(func: &mut ItemFn) -> Result<(Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>, HashMap<Ident, (Ident, Variants)>), syn::Error> {
    let mut enums = vec![];
    let mut vars = vec![];
    let mut vars_map = HashMap::new();

    for input in &mut func.sig.inputs {
        if let FnArg::Typed(tp) = input {
            if let Type::Macro(tm) = tp.ty.as_mut() {
                if let Some(id) =  tm.mac.path.get_ident() {
                    if id == "ano" {
                        if let Pat::Ident(var_name) = tp.pat.as_mut() {
                            let enum_name = format_ident!("{}_{}", func.sig.ident, var_name.ident);

                            let variants: Variants = tm.mac.parse_body().unwrap();

                            if variants.variants.is_empty() {
                                return Err(syn::Error::new(func.sig.output.span(), "no variants"));
                            }

                            let mut var_tokens = vec![];
                            let mut impls = vec![];

                            for variant in &variants.variants {

                                if let Type::Path(tp) = variant {
                                    let variant_name = tp.path.segments.first().unwrap().ident.clone();
                                    var_tokens.push(quote!{
                                        #variant_name(#variant)
                                    });

                                    impls.push(quote!{
                                        impl From<#variant> for #enum_name {
                                            fn from(v: #variant) -> Self {
                                                Self::#variant_name(v)
                                            }
                                        }
                                    });
                                }
                            }

                            enums.push(quote!{
                                #[allow(non_camel_case_types)]
                                enum #enum_name {
                                    #(#var_tokens),*
                                }

                                #(#impls)*
                            });

                            let tk: proc_macro2::TokenStream = quote!{impl Into<#enum_name>}.into();
                            let rt = match syn::parse::<TypeImplTrait>(TokenStream::from(tk)) {
                                Ok(syntax_tree) => syntax_tree,
                                Err(err) => panic!("{}", err),
                            };

                            tp.ty = Box::new(Type::ImplTrait(rt));

                            let old = var_name.ident.clone();
                            var_name.ident = format_ident!("_{}", var_name.ident);
                            let n = &var_name.ident;
                            vars.push(quote!{
                                let #old: #enum_name  = #n.into();
                            });
                            vars_map.insert(old.clone(), (enum_name, variants));
                        }
                    }
                }
            }
        }
    }

    Ok((enums, vars, vars_map))
}

fn make_match(func: &mut ItemFn, vars_map: HashMap<Ident, (Ident, Variants)>) {
    for stmt in &mut func.block.stmts {
        if let syn::Stmt::Expr(expr) = stmt {
            match expr {
                Expr::Match(em) => {
                    if let Expr::Macro(mm) = em.expr.as_mut() {
                        if let Some(id) = mm.mac.path.get_ident() {
                            if id != "ano" {
                                break;
                            }
                            let ii: Ident = mm.mac.parse_body().unwrap();

                            let tk: proc_macro2::TokenStream = quote!{#ii}.into();
                            let rt = match syn::parse::<Expr>(TokenStream::from(tk)) {
                                Ok(syntax_tree) => syntax_tree,
                                Err(err) => panic!("{}", err),
                            };
                            em.expr = Box::new(rt);
                        }
                    }

                    for arm in &mut em.arms {
                        if let Pat::Path(path) = &mut arm.pat {
                            panic!("pp: {:#?}", path.to_token_stream());
                            // if let Some(last) = path.path.segments.last().as_mut() {
                            //
                            // }
                        }
                        if let Pat::TupleStruct(ts) = &mut arm.pat {
                            if let Some(tt) = vars_map.get(&ts.path.segments.first().unwrap().ident) {
                                let mut seg: Punctuated<Ident, Token![::]> = Punctuated::new();
                                for (a, b) in ts.path.segments.iter().enumerate() {
                                    if a > 0 {
                                        seg.push(b.ident.clone());
                                    }
                                }
                                let mut exists  = false;

                                for variant in &tt.1.variants {
                                    if let Type::Path(tp) = variant {
                                        if tp.path.segments.first().unwrap().ident == seg.to_token_stream().to_string() {
                                            exists = true;
                                            break;
                                        }
                                    }
                                }

                                if !exists {
                                    panic!("unknown match type: `{}`", seg.to_token_stream().to_string());
                                }

                                let name = tt.0.clone();
                                let tk: proc_macro2::TokenStream = quote!{#name::#seg}.into();
                                let rt = match syn::parse::<Path>(TokenStream::from(tk)) {
                                    Ok(syntax_tree) => syntax_tree,
                                    Err(err) => panic!("{}", err),
                                };

                                ts.path = rt;
                            } else {
                                //todo
                                let mut seg: Punctuated<PathSegment, Token![::]> = Punctuated::new();
                                for (a, b) in ts.path.segments.iter().enumerate() {
                                    if a > 0 {
                                        seg.push(b.clone());
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

fn make_return_type(func: &mut ItemFn) -> Result<(Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>, Ident, proc_macro2::TokenStream), syn::Error> {
    let macro_output = match func.sig.output.clone() {
        ReturnType::Type(_, t) => *t,
        _ => return Err(syn::Error::new(func.sig.output.span(), "no return type"))
    };

    let variants: Variants = match macro_output {
        Type::Macro(m) => {
            m.mac.parse_body().unwrap()
        },
        _ => return Err(syn::Error::new(func.sig.output.span(), "no variants"))
    };

    if variants.variants.is_empty() {
        return Err(syn::Error::new(func.sig.output.span(), "no variants"));
    }

    let mut name = func.sig.ident.to_string();

    let enum_name = format_ident!("{}", name);

    let mut var_tokens = vec![];
    let mut impls = vec![];
    let mut generics = vec![];
    let mut types = vec![];

    for variant in &variants.variants {
        if let Type::Path(tp) = &variant {
            if tp.path.segments.len() != 1 {
                panic!("only 1 segment allowed");
            }

            let t = tp.path.segments.first().unwrap().ident.clone();
            types.push(t);
            match tp.path.segments.first().unwrap().arguments.clone() {
                PathArguments::AngleBracketed(ga) => {
                    generics.push(Some(ga.args.to_token_stream()));
                },
                _ => {
                    generics.push(None);
                }
            };

        }
    }

    let gens =  if generics.is_empty() {
        quote!{}
    } else {
        let g: Vec<proc_macro2::TokenStream> = generics.into_iter().filter(|a| a.is_some()).map(|a| a.unwrap()).collect();
        quote!{< #(#g),* >}
    };

    for (i, variant) in variants.variants.iter().enumerate() {
        if let Type::Path(_) = &variant {
            let t = types[i].clone();
            var_tokens.push(quote!{
                #t(#variant)
            });

            impls.push(quote!{
                impl #gens From<#variant> for #enum_name #gens {
                    fn from(v: #variant) -> Self {
                        Self::#t(v)
                    }
                }
            });
        }
    }

    let tk: proc_macro2::TokenStream = quote!{-> #enum_name #gens}.into();
    //panic!("{}", gens.to_string());
    let rt = match syn::parse::<ReturnType>(TokenStream::from(tk)) {
        Ok(syntax_tree) => syntax_tree,
        Err(err) => panic!("{}", err),
    };

    //let rt = parse_macro_input!(tk as ReturnType);
    func.sig.output = rt;

    Ok((var_tokens, impls, enum_name, gens))
}