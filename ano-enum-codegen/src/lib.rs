use proc_macro::TokenStream;
use std::collections::{HashMap, VecDeque};
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{parse_macro_input, FnArg, ItemFn, ReturnType, Type, Token, Path, Pat, TypeImplTrait, Stmt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::parse_quote::ParseQuote;
use syn::{Expr};
use syn::spanned::Spanned;

#[proc_macro]
pub fn ano(input: TokenStream) -> TokenStream {
    input
}

fn wrap_return_type(func: &mut ItemFn) {
    for stmt in &mut func.block.stmts {
        let stm_str = stmt.to_token_stream().to_string();
        let is_return =  !stm_str.ends_with(";") || stm_str.starts_with("return");

        if !is_return {
            continue;
        }

        if let syn::Stmt::Expr(expr) = stmt {
            match expr {
                Expr::Match(em) => {
                    for arm in &mut em.arms {
                        let arm_expr = &mut arm.body;
                        let tk: proc_macro2::TokenStream = quote!{{
                            let result = #arm_expr;
                            result.into()
                        }}.into();
                        let rt = match syn::parse::<Expr>(TokenStream::from(tk)) {
                            Ok(syntax_tree) => syntax_tree,
                            Err(err) => panic!("{}", err),
                        };
                        arm.body = Box::new(rt);
                    }
                }
                _ => {
                    let tk: proc_macro2::TokenStream = quote!{{
                        let result = #expr;
                        result.into()
                    }}.into();
                    let rt = match syn::parse::<Expr>(TokenStream::from(tk)) {
                        Ok(syntax_tree) => syntax_tree,
                        Err(err) => panic!("{}", err),
                    };
                    *expr = rt;
                }
            }
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
        Err(_e) => panic!("Failed to process arguments")
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
        Err(_e) => quote!{}
    };

    wrap_return_type(&mut func);

    let out = quote!{
        #enum_def
        #e
        #func
    };

    out.into()
}

fn make_arg(func: &mut ItemFn) -> Result<(Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>, HashMap<Ident, (Ident, Variants)>), syn::Error> {
    let mut enums = vec![];
    let mut vars = vec![];
    let mut vars_map = HashMap::new();

    // Get the generic parameters from the function
    let generics = &func.sig.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    for input in &mut func.sig.inputs {
        if let FnArg::Typed(tp) = input {
            if let Type::Macro(tm) = tp.ty.as_mut() {
                if let Some(id) = tm.mac.path.get_ident() {
                    if id == "ano" {
                        if let Pat::Ident(var_name) = tp.pat.as_mut() {
                            let enum_name = format_ident!("{}", var_name.ident);

                            let variants: Variants = tm.mac.parse_body().unwrap();

                            if variants.variants.is_empty() {
                                return Err(syn::Error::new(func.sig.output.span(), "no variants"));
                            }

                            let mut var_tokens = vec![];
                            let mut impls = vec![];

                            for variant in &variants.variants {
                                match variant {
                                    Type::Macro(tm) if tm.mac.path.get_ident().map_or(false, |id| id == "ano") => {
                                        return Err(syn::Error::new(tm.mac.span(), "Nested ano! enums are not supported. Please avoid nesting ano! macros."));
                                    }
                                    Type::Path(tp) => {
                                        let variant_name = tp.path.segments.first().unwrap().ident.clone();
                                        var_tokens.push(quote!{
                                            #variant_name(#variant)
                                        });

                                        impls.push(quote!{
                                            impl #impl_generics From<#variant> for #enum_name #ty_generics #where_clause {
                                                fn from(v: #variant) -> Self {
                                                    Self::#variant_name(v)
                                                }
                                            }
                                        });
                                    }
                                    _ => {}
                                }
                            }

                            enums.push(quote!{
                                #[allow(non_camel_case_types)]
                                pub enum #enum_name #ty_generics #where_clause {
                                    #(#var_tokens),*
                                }

                                #(#impls)*
                            });

                            let tk: proc_macro2::TokenStream = quote!{impl Into<#enum_name #ty_generics>}.into();
                            let rt = match syn::parse::<TypeImplTrait>(TokenStream::from(tk)) {
                                Ok(syntax_tree) => syntax_tree,
                                Err(err) => panic!("{}", err),
                            };

                            tp.ty = Box::new(Type::ImplTrait(rt));

                            let old = var_name.ident.clone();
                            var_name.ident = format_ident!("_{}", var_name.ident);
                            let n = &var_name.ident;
                            vars.push(quote!{
                                let #old: #enum_name #ty_generics = #n.into();
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
                            let first_segment = path.path.segments.first().cloned();
                            if let Some(first) = first_segment {
                                if let Some((_enum_name, variants)) = vars_map.get(&first.ident) {
                                    let mut seg: Punctuated<Ident, Token![::]> = Punctuated::new();
                                    for (a, b) in path.path.segments.iter().enumerate() {
                                        if a > 0 {
                                            seg.push(b.ident.clone());
                                        }
                                    }

                                    let mut exists = false;
                                    for variant in &variants.variants {
                                        match variant {
                                            Type::Macro(tm) if tm.mac.path.get_ident().map_or(false, |id| id == "ano") => {
                                                if seg.to_token_stream().to_string() == "ano" {
                                                    exists = true;
                                                    break;
                                                }
                                            }
                                            Type::Path(tp) => {
                                                if tp.path.segments.first().unwrap().ident == seg.to_token_stream().to_string() {
                                                    exists = true;
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    if !exists {
                                        panic!("unknown match type: `{}`", seg.to_token_stream().to_string());
                                    }

                                    let tk: proc_macro2::TokenStream = quote!{#first.ident::#seg}.into();
                                    let rt = match syn::parse::<Path>(TokenStream::from(tk)) {
                                        Ok(syntax_tree) => syntax_tree,
                                        Err(err) => panic!("{}", err),
                                    };

                                    path.path = rt;

                                    // Handle nested match expressions
                                    if let Expr::Match(nested_match) = &mut *arm.body {
                                        if let Expr::Macro(nested_macro) = nested_match.expr.as_mut() {
                                            if let Some(nested_id) = nested_macro.mac.path.get_ident() {
                                                if nested_id == "ano" {
                                                    let nested_var: Ident = nested_macro.mac.parse_body().unwrap();
                                                    
                                                    let tk: proc_macro2::TokenStream = quote!{#nested_var}.into();
                                                    let rt = match syn::parse::<Expr>(TokenStream::from(tk)) {
                                                        Ok(syntax_tree) => syntax_tree,
                                                        Err(err) => panic!("{}", err),
                                                    };
                                                    nested_match.expr = Box::new(rt);

                                                    for nested_arm in &mut nested_match.arms {
                                                        if let Pat::Path(nested_path) = &mut nested_arm.pat {
                                                            let mut nested_seg: Punctuated<Ident, Token![::]> = Punctuated::new();
                                                            for (a, b) in nested_path.path.segments.iter().enumerate() {
                                                                if a > 0 {
                                                                    nested_seg.push(b.ident.clone());
                                                                }
                                                            }

                                                            let tk: proc_macro2::TokenStream = quote!{#first.ident::ano::#nested_seg}.into();
                                                            let rt = match syn::parse::<Path>(TokenStream::from(tk)) {
                                                                Ok(syntax_tree) => syntax_tree,
                                                                Err(err) => panic!("{}", err),
                                                            };

                                                            nested_path.path = rt;
                                                        }
                                                    }
                                                }
                                            }
                                        }
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

    let name = func.sig.ident.to_string();
    let enum_name = format_ident!("{}", name);

    // Get the generic parameters from the function
    let generics = &func.sig.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut var_tokens = vec![];
    let mut impls = vec![];
    let mut types = vec![];

    for variant in &variants.variants {
        if let Type::Path(tp) = &variant {
            if tp.path.segments.len() != 1 {
                panic!("only 1 segment allowed");
            }

            let t = tp.path.segments.first().unwrap().ident.clone();
            types.push(t);
        }
    }

    for (i, variant) in variants.variants.iter().enumerate() {
        if let Type::Path(_) = &variant {
            let t = types[i].clone();
            var_tokens.push(quote!{
                #t(#variant)
            });

            impls.push(quote!{
                impl #impl_generics From<#variant> for #enum_name #ty_generics #where_clause {
                    fn from(v: #variant) -> Self {
                        Self::#t(v)
                    }
                }
            });
        }
    }

    let tk: proc_macro2::TokenStream = quote!{-> #enum_name #ty_generics}.into();
    let rt = match syn::parse::<ReturnType>(TokenStream::from(tk)) {
        Ok(syntax_tree) => syntax_tree,
        Err(err) => panic!("{}", err),
    };

    func.sig.output = rt;

    Ok((var_tokens, impls, enum_name, ty_generics.to_token_stream()))
}