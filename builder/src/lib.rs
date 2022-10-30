#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    struct MacroAttrs {
        each: std::option::Option<std::string::String>,
    }

    #[derive(core::clone::Clone, core::marker::Copy)]
    enum WrappedType {
        Vec,
        Option,
    }

    impl WrappedType {
        fn parse(ident: &syn::Ident) -> std::option::Option<Self> {
            match ident.to_string().as_str() {
                "Vec" => std::option::Option::Some(Self::Vec),
                "Option" => std::option::Option::Some(Self::Option),
                _ => std::option::Option::None,
            }
        }
    }

    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let struct_name = input.ident;
    let struct_builder = syn::Ident::new(
        &format!("{}Builder", struct_name),
        proc_macro2::Span::call_site(),
    );

    let mut errors = std::vec::Vec::new();

    let fields: std::vec::Vec<(syn::Ident, syn::Type, MacroAttrs)> =
        if let syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) = input.data
        {
            fields
                .named
                .into_iter()
                .map(|field| {
                    (field.ident.clone().unwrap(), field.ty.clone(), {
                        let mut attrs = MacroAttrs {
                            each: std::option::Option::None,
                        };

                        if let std::option::Option::Some(Ok(syn::Meta::List(meta))) =
                            field.attrs.first().map(|a| a.parse_meta())
                        {
                            if meta.path.get_ident().map(|i| i.to_string())
                                == std::option::Option::Some("builder".to_string())
                            {
                                for nested in &meta.nested {
                                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(
                                        syn::MetaNameValue {
                                            path,
                                            eq_token: syn::token::Eq { .. },
                                            lit: syn::Lit::Str(rhs),
                                        },
                                    )) = nested
                                    {
                                        if let std::option::Option::Some(lhs) =
                                            path.get_ident().map(|i| i.to_string())
                                        {
                                            if let "each" = lhs.as_str() {
                                                attrs.each = std::option::Option::Some(rhs.value());
                                            } else {
                                                errors.push(
                                                    syn::Error::new_spanned(
                                                        meta.clone(),
                                                        "expected `builder(each = \"...\")`",
                                                    )
                                                    .to_compile_error(),
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        attrs
                    })
                })
                .collect()
        } else {
            panic!("Can only handle structs with named fields");
        };

    // Determine types for all the fields
    let (
        field,
        builder_type,
        field_method,
        default_value,
        build_check,
    ): (std::vec::Vec<_>, std::vec::Vec<_>, std::vec::Vec<_>, std::vec::Vec<_>, std::vec::Vec<_>) =
        itertools::multiunzip(fields.iter().map(|(field, ty, attrs)| {
            let field_name = field.to_string();

            // Type that is stored on the original struct
            let field_type = ty;

            // Type within the field type
            let mut inner_field_type = std::option::Option::None;
            let mut wrapped_type = std::option::Option::None;

            if let syn::Type::Path(syn::TypePath {
                qself: std::option::Option::None,
                path:
                    syn::Path {
                        leading_colon: std::option::Option::None,
                        segments,
                    },
            }) = ty
            {
                // Type is a type path, attempt to see if there's a generic in the end of the path
                if let std::option::Option::Some(syn::PathSegment {
                    ident,
                    arguments:
                        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                            args,
                            ..
                        }),
                }) = segments.last()
                {
                    if let std::option::Option::Some(syn::GenericArgument::Type(generic_type)) = args.first() {
                        if let std::option::Option::Some(determined_wrapped_time) = WrappedType::parse(ident) {
                            inner_field_type = std::option::Option::Some(generic_type);
                            wrapped_type = std::option::Option::Some(determined_wrapped_time);
                        }
                    }
                }
            };

            // Type that is stored on the builder
            let builder_type: syn::Type = if wrapped_type.is_some() {
                // Type is Vec or Option, so builder type doesn't need to be wrapped in Option<..>
                field_type.to_owned()
            } else {
                syn::parse_quote!( std::option::Option<#field_type> )
            };

            // Builder methods
            let multi_setter = {
                // If Vec, rhs will be the field directly, otherwise wrapped in Some
                let rhs = if let std::option::Option::Some(WrappedType::Vec) = wrapped_type {
                    quote::quote! { #field }
                } else {
                    quote::quote! { std::option::Option::Some(#field) }
                };

                // If wrapped type isn't Vec, then will be stored in an Option
                let arg_type = match (wrapped_type, inner_field_type) {
                    (std::option::Option::Some(WrappedType::Vec), _) => field_type,
                    (std::option::Option::Some(WrappedType::Option), std::option::Option::Some(inner_field_type)) => inner_field_type,
                    (std::option::Option::None, std::option::Option::None) => field_type,
                    _ => unreachable!()
                };

                quote::quote! {
                    fn #field(&mut self, #field: #arg_type) -> &mut Self {
                        self.#field = #rhs;
                        self
                    }
                }
            };

            let single_setter = if let (
                MacroAttrs { each: std::option::Option::Some(each) },
                std::option::Option::Some(WrappedType::Vec),
                std::option::Option::Some(inner_field_type),
            ) = (attrs, wrapped_type, inner_field_type)
            {
                let single_setter_name = syn::Ident::new(each, proc_macro2::Span::call_site());

                std::option::Option::Some(quote::quote! {
                    fn #single_setter_name(&mut self, #single_setter_name: #inner_field_type) -> &mut Self {
                        self.#field.push(#single_setter_name);
                        self
                    }
                })
            } else {
                std::option::Option::None
            };

            let field_method = if let (std::option::Option::Some(single_setter), std::option::Option::Some(single_setter_name)) =
                (single_setter, attrs.each.as_ref())
            {
                if &field.to_string() == single_setter_name {
                    single_setter
                } else {
                    quote::quote! {
                        #single_setter
                        #multi_setter
                    }
                }
            } else {
                multi_setter
            };

            let default_value = if let std::option::Option::Some(WrappedType::Vec) = wrapped_type {
                quote::quote! { std::vec::Vec::new() }
            } else {
                quote::quote! { std::option::Option::None }
            };

            let build_check = if wrapped_type.is_some() {
                // If the type is wrapped (Vec or Option), clone and return directly
                quote::quote! { self.#field.clone() }
            } else {
                // If the type is not, make sure that it's specified
                quote::quote! {
                    match &self.#field {
                        std::option::Option::Some(v) => v.clone(),
                        std::option::Option::None => { return std::result::Result::Err(format!("Field not present: {}", #field_name).into()); }
                    }
                }
            };

            (
                field,
                builder_type,
                field_method,
                default_value,
                build_check,
            )
        }));

    quote::quote! {
        #(#errors)*

        pub struct #struct_builder {
            #(#field: #builder_type),*
        }

        impl #struct_builder {
            #(#field_method)*

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#struct_name {
                    #(#field: #build_check),*
                })
            }
        }

        impl #struct_name {
            pub fn builder() -> #struct_builder {
                #struct_builder {
                    #(#field: #default_value),*
                }
            }
        }
    }
    .into()
}
