#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_ident = input.ident;
    let struct_ident_str = struct_ident.to_string();

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = input.data
    {
        named.into_iter().collect::<std::vec::Vec<_>>()
    } else {
        unimplemented!("Only support named structs for now");
    };

    let mut errors = std::vec::Vec::new();

    // Determine ident for each field
    let (field_idents, field_idents_str): (Vec<_>, Vec<_>) = fields
        .iter()
        .filter_map(|f| f.ident.as_ref())
        .map(|f| (f, f.to_string()))
        .unzip();

    // Determine attributes for each field
    let formats = fields
        .iter()
        .map(|f| {
            if let Some(attr) = f.attrs.iter().find(|attr| {
                attr.path
                    .is_ident(&syn::Ident::new("debug", proc_macro2::Span::call_site()))
            }) {
                // 'debug' attribute found for field
                match attr.parse_meta() {
                    syn::Result::Ok(syn::Meta::NameValue(syn::MetaNameValue {
                        lit: syn::Lit::Str(lit),
                        ..
                    })) => {
                        return lit.value();
                    }
                    syn::Result::Ok(_) => errors.push(
                        syn::Error::new_spanned(attr, "Can only parse name value attributes")
                            .to_compile_error(),
                    ),
                    syn::Result::Err(e) => errors.push(e.to_compile_error()),
                }
            }

            "{:?}".to_string()
        })
        .collect::<std::vec::Vec<_>>();

    // Find all PhantomData fields
    let phandom_data_generics: std::collections::HashSet<_> = fields
        .iter()
        .filter_map(|fields| {
            if let syn::Type::Path(syn::TypePath { path, .. }) = &fields.ty {
                if let Some(syn::PathSegment {
                    ident,
                    arguments:
                        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                            args,
                            ..
                        }),
                }) = path.segments.last()
                {
                    if ident == &syn::Ident::new("PhantomData", proc_macro2::Span::call_site()) {
                        // Return generic parameters within PhantomData
                        return std::option::Option::Some(args.into_iter().filter_map(|arg| {
                            if let syn::GenericArgument::Type(syn::Type::Path(inner_type)) = arg {
                                // Really bad :(
                                // Reasoning: If it is generic (which is all that is being
                                // checked), it will be an ident
                                inner_type.path.get_ident()
                            } else {
                                std::option::Option::None
                            }
                        }));
                    }
                }
            }

            std::option::Option::None
        })
        .flatten()
        .collect();

    let generic_param_idents: std::collections::HashSet<_> = input
        .generics
        .type_params()
        .map(|p| p.ident.clone())
        .collect();

    let mut excluded_generic_idents = std::collections::HashSet::new();

    let attr_bounds = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if let Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) = attr.parse_meta() {
                if path
                    .get_ident()
                    .map(|ident| *ident == "debug")
                    .unwrap_or(false)
                {
                    for meta in nested {
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                            path,
                            lit: syn::Lit::Str(lit),
                            ..
                        })) = meta
                        {
                            if let Some(ident) = path.get_ident().map(|ident| ident.to_string()) {
                                if let ("bound", Ok(where_predicate)) = (
                                    ident.as_str(),
                                    syn::parse_str::<syn::WherePredicate>(&lit.value()),
                                ) {
                                    return Some(where_predicate);
                                }
                            }
                        }
                    }
                }
            }

            None
        })
        .collect::<std::vec::Vec<_>>();

    let infer_bounds = attr_bounds.is_empty();

    input
        .generics
        .make_where_clause()
        .predicates
        .extend(attr_bounds.into_iter());

    // Find associated types
    for field in &fields {
        // Check if the field type is a type path
        let ty = &field.ty;
        if let syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = ty
        {
            // Extract the arguments from the last segment of the path (eg <T> in std::vec::Vec<T>)
            if let Some(syn::PathSegment {
                arguments:
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args, ..
                    }),
                ..
            }) = segments.last()
            {
                // Iterate through each of those arguments
                for arg in args {
                    // Must be of type syn::Type::Path
                    if let syn::GenericArgument::Type(syn::Type::Path(ty)) = arg {
                        // Path length must be greater than 1 (eg T::Value)
                        if ty.path.segments.len() > 1 {
                            // Extract first part of the path, as it is the generic
                            if let Some(first_segment) = ty.path.segments.first() {
                                // Check if it matches one of the generic idents
                                if generic_param_idents.contains(&first_segment.ident) {
                                    // If it does match, it's an associated type so prevent the
                                    // generic from having the Debug trait added to it
                                    excluded_generic_idents.insert(first_segment.ident.clone());

                                    if infer_bounds {
                                        // Add a predicate to the where clause
                                        input
                                            .generics
                                            .make_where_clause()
                                            .predicates
                                            .push(syn::parse_quote!( #ty: std::fmt::Debug ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if infer_bounds {
        // Make sure that all the generics have the Debug type bound
        input
            .generics
            .type_params_mut()
            .filter(|param| {
                // Only include parameter if it exists outside of std::marker::PhantomData
                !phandom_data_generics.contains(&&param.ident)
                    && !excluded_generic_idents.contains(&param.ident)
            })
            .for_each(|param| {
                // Add the debug bound to the generic type
                param.bounds.push(syn::parse_quote!(std::fmt::Debug));
            });
    }

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    quote::quote! {
        #(#errors)*

        impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_ident_str)
                    #(.field(#field_idents_str, &std::format_args!(#formats, &self.#field_idents)))*
                    .finish()
            }
        }
    }
    .into()
}
