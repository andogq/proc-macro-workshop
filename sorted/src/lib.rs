use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{parse, visit_mut::VisitMut, Item, ItemFn, PatIdent, PatTupleStruct, Path, PathSegment};

fn stringify_path(p: &Path) -> String {
    p.segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn check_sorted_idents(paths: Vec<Path>) -> Result<(), syn::Error> {
    let paths = paths.iter().filter_map(|path| {
        if let Some(PathSegment { ident, .. }) = path.segments.last() {
            Some((path, ident))
        } else {
            None
        }
    });

    for ((_, a), (b_full, b)) in paths.clone().zip(paths.clone().skip(1)) {
        if a > b {
            // b is out of place, find it's correct location
            return Err(syn::Error::new_spanned(
                b_full,
                if let Some((correct_place, _)) = paths.clone().find(|(_, var)| var > &b) {
                    format!(
                        "{} should sort before {}",
                        stringify_path(b_full),
                        stringify_path(correct_place)
                    )
                } else {
                    // Shouldn't get here....
                    format!("{} is out of order, but cannot find where it should go", b)
                },
            ));
        }
    }

    Ok(())
}

fn sorted_macro(raw_input: TokenStream) -> Result<TokenStream, syn::Error> {
    match parse::<Item>(raw_input) {
        Ok(Item::Enum(attached_enum)) => {
            match check_sorted_idents(
                attached_enum
                    .variants
                    .iter()
                    .map(|variant| Path::from(variant.ident.clone()))
                    .collect(),
            ) {
                Ok(_) => Ok(TokenStream::new()),
                Err(e) => Err(e),
            }
        }
        Ok(_) => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
        Err(e) => Err(e),
    }
}

struct SortedMatch {
    pub errors: Vec<syn::Error>,
}
impl SortedMatch {
    pub fn new() -> Self {
        SortedMatch { errors: Vec::new() }
    }
}

impl VisitMut for SortedMatch {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        // Check if #[sorted] attribute is present
        let found_sorted = node
            .attrs
            .iter()
            .enumerate()
            .find(|(_, attr)| attr.path.is_ident("sorted"))
            .map(|(i, _)| i);

        if let Some(sorted_attr_i) = found_sorted {
            // Remove the attribute
            node.attrs.swap_remove(sorted_attr_i);

            // Check if arms are sorted
            let mut arm_idents = Vec::new();

            for (i, arm) in node.arms.iter().enumerate() {
                match &arm.pat {
                    syn::Pat::TupleStruct(PatTupleStruct { path, .. }) => {
                        arm_idents.push(path.clone());
                    }
                    syn::Pat::Ident(PatIdent { ident, .. }) => {
                        arm_idents.push(Path::from(ident.clone()));
                    }
                    syn::Pat::Wild(_) => {
                        if i != node.arms.len() - 1 {
                            self.errors
                                .push(syn::Error::new_spanned(arm.pat.to_token_stream(), "helo"));

                            return;
                        }
                    }
                    _ => {
                        self.errors.push(syn::Error::new_spanned(
                            arm.pat.to_token_stream(),
                            "unsupported by #[sorted]",
                        ));

                        return;
                    }
                }
            }

            if let Err(e) = check_sorted_idents(arm_idents) {
                self.errors.push(e);
                return;
            }
        }

        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;

    // Always emit the original enum
    let mut output = input.clone();

    output.extend(match sorted_macro(input) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error().into(),
    });

    output
}

fn check_macro(input: TokenStream) -> Result<TokenStream, syn::Error> {
    match parse::<ItemFn>(input) {
        Ok(mut function) => {
            // Search for match statement with #[sorted] attribute
            let mut visitor = SortedMatch::new();
            visitor.visit_item_fn_mut(&mut function);

            let mut output = quote!(#function);

            if let Some(error) = visitor.errors.into_iter().reduce(|mut acc, e| {
                acc.combine(e);
                acc
            }) {
                output.extend::<proc_macro2::TokenStream>(error.to_compile_error());
            }

            Ok(output.into())
        }
        Err(e) => Err(e),
    }
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;

    match check_macro(input.clone()) {
        Ok(ts) => ts,
        Err(e) => {
            let mut output = input;
            output.extend::<TokenStream>(e.to_compile_error().into());

            output
        }
    }
}
