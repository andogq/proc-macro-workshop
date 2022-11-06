use quote::TokenStreamExt;

struct Seq {
    ident: syn::Ident,
    start: usize,
    end: usize,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        input.parse::<syn::Token![in]>()?;
        let start: syn::LitInt = input.parse()?;
        input.parse::<syn::Token![..]>()?;
        let inclusive = input.parse::<syn::Token![=]>().is_ok();
        let end: syn::LitInt = input.parse()?;

        let body;
        syn::braced!(body in input);
        let body = body.parse::<proc_macro2::TokenStream>()?;

        Ok(Seq {
            ident,
            start: start.base10_parse()?,
            end: (end.base10_parse::<usize>()?) + if inclusive { 1 } else { 0 },
            body,
        })
    }
}

fn replace_ident(
    body: &proc_macro2::TokenStream,
    target_ident: &proc_macro2::Ident,
    n: usize,
) -> proc_macro2::TokenStream {
    let body = body.clone().into_iter().collect::<Vec<_>>();
    let mut i = 0;

    let mut modified_token_stream = proc_macro2::TokenStream::new();

    while let Some(token_tree) = body.get(i) {
        match (token_tree, body.get(i + 1), body.get(i + 2)) {
            // prefix~N
            (
                proc_macro2::TokenTree::Ident(prefix_ident),
                Some(proc_macro2::TokenTree::Punct(tilde)),
                Some(proc_macro2::TokenTree::Ident(ident)),
            ) => {
                if tilde.as_char() == '~' && target_ident == ident {
                    modified_token_stream.append(proc_macro2::TokenTree::Ident(
                        proc_macro2::Ident::new(
                            &format!("{}{}", prefix_ident, n),
                            prefix_ident.span(),
                        ),
                    ));

                    // Advance by 3
                    i += 3;
                    continue;
                }
            }
            // N
            (proc_macro2::TokenTree::Ident(ident), ..) => {
                if target_ident == ident {
                    // Exact replacement
                    modified_token_stream.append({
                        let mut literal = proc_macro2::TokenTree::Literal(
                            proc_macro2::Literal::usize_unsuffixed(n),
                        );

                        literal.set_span(ident.span());

                        literal
                    });

                    // Advance by 1
                    i += 1;
                    continue;
                }
            }
            // Nested content
            (proc_macro2::TokenTree::Group(group), ..) => {
                modified_token_stream.append(proc_macro2::TokenTree::Group({
                    let mut new_group = proc_macro2::Group::new(
                        group.delimiter(),
                        replace_ident(&group.stream(), target_ident, n),
                    );

                    new_group.set_span(group.span());

                    new_group
                }));

                // Advance by 1
                i += 1;
                continue;
            }
            _ => (),
        }

        // Nothing changed, add token and continue
        modified_token_stream.append(body[i].clone());
        i += 1;
    }

    modified_token_stream
}

fn find_repetitions(
    body: &proc_macro2::TokenStream,
    target_ident: &proc_macro2::Ident,
    range: std::ops::Range<usize>,
) -> Option<proc_macro2::TokenStream> {
    let body = body.clone().into_iter().collect::<Vec<_>>();
    let mut i = 0;

    let mut modified = false;
    let mut modified_token_stream = proc_macro2::TokenStream::new();

    while let Some(token_tree) = body.get(i) {
        match (token_tree, body.get(i + 1), body.get(i + 2)) {
            (
                proc_macro2::TokenTree::Punct(hash),
                Some(proc_macro2::TokenTree::Group(body)),
                Some(proc_macro2::TokenTree::Punct(asterix)),
            ) => {
                if hash.as_char() == '#' && asterix.as_char() == '*' {
                    // Repeat this section
                    modified_token_stream.extend(repeat(
                        &body.stream(),
                        target_ident,
                        range.clone(),
                    ));

                    i += 3;
                    modified = true;
                    continue;
                }
            }
            (proc_macro2::TokenTree::Group(body), ..) => {
                if let Some(token_stream) =
                    find_repetitions(&body.stream(), target_ident, range.clone())
                {
                    modified_token_stream.append(proc_macro2::TokenTree::Group(
                        proc_macro2::Group::new(body.delimiter(), token_stream),
                    ));

                    i += 1;
                    modified = true;
                    continue;
                }
            }
            _ => (),
        }

        i += 1;
        modified_token_stream.append(token_tree.clone());
    }

    if modified {
        Some(modified_token_stream)
    } else {
        None
    }
}

fn repeat(
    body: &proc_macro2::TokenStream,
    target_ident: &proc_macro2::Ident,
    range: std::ops::Range<usize>,
) -> proc_macro2::TokenStream {
    let mut modified_token_stream = proc_macro2::TokenStream::new();

    for n in range {
        modified_token_stream.extend(replace_ident(body, target_ident, n));
    }

    modified_token_stream
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);

    if let Some(modified) = find_repetitions(&seq.body, &seq.ident, seq.start..seq.end) {
        // Attempt to find repetition blocks denoted by #(..)* within the body
        modified
    } else {
        // Nothing found, repeat the whole outer block
        (seq.start..seq.end)
            .map(|n| replace_ident(&seq.body, &seq.ident, n))
            .collect::<proc_macro2::TokenStream>()
    }
    .into()
}
