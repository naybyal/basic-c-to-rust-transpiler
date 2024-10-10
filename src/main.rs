extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use syn::{Expr, parse_quote};
use quote::quote;

#[derive(Parser)]
#[grammar = "c_grammar.pest"] // Point this to your correct .pest grammar file
struct CParser;

fn main() {
    let c_code = r#"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            int result = add(5, 10);
            printf("Result: %d\n", result);
            return 0;
        }
    "#;

    let parse_result = CParser::parse(Rule::program, c_code).expect("Failed to parse C code");
    let rust_code = transpile_c_to_rust(parse_result);
    println!("Generated Rust code:\n{}", rust_code);
}

fn transpile_c_to_rust(parse_result: pest::iterators::Pairs<Rule>) -> String {
    let mut rust_functions = Vec::new();
    let mut rust_main = None;

    for pair in parse_result {
        if let Rule::function = pair.as_rule() {
            let inner_pair = pair.into_inner();
            let func_name = inner_pair.clone().nth(1).unwrap().as_str();
            if func_name == "main" {
                rust_main = Some(transpile_function(inner_pair));
            } else {
                rust_functions.push(transpile_function(inner_pair));
            }
        }
    }

    let rust_code = quote! {
        #(#rust_functions)*

        fn main() {
            #rust_main
        }
    };
    rust_code.to_string()
}

fn transpile_function(mut inner_pair: pest::iterators::Pairs<Rule>) -> syn::ItemFn {
    let return_type = transpile_type_specifier(inner_pair.next().unwrap());
    let function_name = inner_pair.next().unwrap().as_str();
    let parameters = transpile_parameter_list(inner_pair.next().unwrap());
    let body = transpile_statement_list(inner_pair.next().unwrap());

    let ident = syn::Ident::new(function_name, proc_macro2::Span::call_site());
    let rust_fn: syn::ItemFn = parse_quote! {
        #return_type #ident(#(#parameters),*) {
            #body
        }
    };
    rust_fn
}

fn transpile_parameter_list(pair: pest::iterators::Pair<Rule>) -> Vec<syn::FnArg> {
    pair.into_inner().map(|param| {
        let mut param_inner = param.into_inner();
        let type_specifier = transpile_type_specifier(param_inner.next().unwrap());
        let ident = syn::Ident::new(param_inner.next().unwrap().as_str(), proc_macro2::Span::call_site());
        parse_quote! { #ident: #type_specifier }
    }).collect()
}

fn transpile_statement_list(pair: pest::iterators::Pair<Rule>) -> syn::Block {
    let statements = pair.into_inner().map(transpile_statement).collect::<Vec<syn::Stmt>>();
    parse_quote! { { #(#statements)* } }
}

fn transpile_statement(pair: pest::iterators::Pair<Rule>) -> syn::Stmt {
    match pair.as_rule() {
        Rule::return_statement => {
            let expr = pair.into_inner().next();
            let rust_expr = if let Some(expr) = expr {
                transpile_expression(expr)
            } else {
                parse_quote! { None }
            };
            parse_quote! { return #rust_expr; }
        }
        Rule::declaration => {
            let mut decl_inner = pair.into_inner();
            let type_specifier = transpile_type_specifier(decl_inner.next().unwrap());
            let idents = decl_inner.map(|ident| {
                syn::Ident::new(ident.as_str(), proc_macro2::Span::call_site())
            }).collect::<Vec<syn::Ident>>();
            parse_quote! { let #(#idents),*: #type_specifier; }
        }
        Rule::assignment => {
            let mut assign_inner = pair.into_inner();
            let ident = syn::Ident::new(assign_inner.next().unwrap().as_str(), proc_macro2::Span::call_site());
            let expr = transpile_expression(assign_inner.next().unwrap());
            parse_quote! { #ident = #expr; }
        }
        Rule::if_statement => {
            let mut if_inner = pair.into_inner();
            let cond = transpile_expression(if_inner.next().unwrap());
            let then_block = transpile_statement_list(if_inner.next().unwrap());
            let else_block = if_inner.next().map(|else_pair| transpile_statement_list(else_pair));
            if let Some(else_block) = else_block {
                parse_quote! { if #cond { #then_block } else { #else_block } }
            } else {
                parse_quote! { if #cond { #then_block } }
            }
        }
        _ => unimplemented!("This statement is not yet supported."),
    }
}

fn transpile_expression(pair: pest::iterators::Pair<Rule>) -> Expr {
    let primary = |pair| transpile_primary(pair);

    let infix = |lhs: Expr, op: pest::iterators::Pair<Rule>, rhs: Expr| {
        let expr = match op.as_str() {
            "+" => quote! { #lhs + #rhs },
            "-" => quote! { #lhs - #rhs },
            "*" => quote! { #lhs * #rhs },
            "/" => quote! { #lhs / #rhs },
            "%" => quote! { #lhs % #rhs },
            "&&" => quote! { #lhs && #rhs },
            "||" => quote! { #lhs || #rhs },
            "<" => quote! { #lhs < #rhs },
            ">" => quote! { #lhs > #rhs },
            "<=" => quote! { #lhs <= #rhs },
            ">=" => quote! { #lhs >= #rhs },
            "==" => quote! { #lhs == #rhs },
            "!=" => quote! { #lhs != #rhs },
            _ => unreachable!(),
        };
        syn::parse2(expr).unwrap()
    };

    let prefix = |op: pest::iterators::Pair<Rule>, rhs: Expr| {
        let expr = match op.as_str() {
            "+" => quote! { #rhs },
            "-" => quote! { -#rhs },
            "!" => quote! { !#rhs },
            "~" => quote! { !#rhs },
            _ => unreachable!(),
        };
        syn::parse2(expr).unwrap()
    };

    let postfix = |lhs: Expr, op: pest::iterators::Pair<Rule>| {
        match op.as_rule() {
            Rule::array_access => {
                let index = transpile_expression(op.into_inner().next().unwrap());
                parse_quote! { #lhs[#index] }
            }
            _ => unreachable!(),
        }
    };

    let parser = PrattParser::new()
        .op(Op::prefix(Rule::unary_op))
        .op(Op::infix(Rule::binary_op, Assoc::Left))
        .op(Op::postfix(Rule::array_access));

    parser.map_primary(primary).map_prefix(prefix).map_infix(infix).map_postfix(postfix).parse(pair.into_inner())
}


fn transpile_primary(pair: pest::iterators::Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::ident => {
            let ident = syn::Ident::new(pair.as_str(), proc_macro2::Span::call_site());
            parse_quote! { #ident }
        }
        Rule::number => {
            let num: i32 = pair.as_str().parse().unwrap();
            parse_quote! { #num }
        }
        Rule::function_call => {
            let mut func_inner = pair.into_inner();
            let function_name = func_inner.next().unwrap().as_str();
            let args = func_inner.map(transpile_expression).collect::<Vec<Expr>>();
            parse_quote! { #function_name(#(#args),*) }
        }
        _ => unreachable!(),
    }
}

fn transpile_type_specifier(pair: pest::iterators::Pair<Rule>) -> syn::Type {
    match pair.as_str() {
        "int" => parse_quote! { i32 },
        "void" => parse_quote! { () },
        "float" => parse_quote! { f32 },
        "char" => parse_quote! { char },
        "double" => parse_quote! { f64 },
        _ => unimplemented!("Unsupported type specifier."),
    }
}