// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        ConnectionProviderUpdateRequest, InstanceConfigUpdateRequest,
        InstanceIntegrationsUpdateRequest,
    },
    templates::pages::instance_config::{
        CONNECTION_PROVIDER_COUNT_FIELD, connection_provider_field,
    },
    utils::forms::MultiValueForm,
};

const MAX_PROVIDER_CARDS: usize = 21;

fn build_provider(form: &MultiValueForm, index: usize) -> Option<ConnectionProviderUpdateRequest> {
    let field = |name: &str| form.clean(&connection_provider_field(index, name));
    if form.bool_value(&connection_provider_field(index, "remove")) {
        return None;
    }
    let id = field("id")?;
    let kind = field("kind").unwrap_or_else(|| "oidc".to_owned());
    let oidc = kind == "oidc";
    Some(ConnectionProviderUpdateRequest {
        name: field("name").unwrap_or_else(|| id.clone()),
        id,
        enabled: form.bool_value(&connection_provider_field(index, "enabled")),
        issuer: if oidc { field("issuer") } else { None },
        authorization_url: field("authorization_url"),
        token_url: field("token_url"),
        userinfo_url: field("userinfo_url"),
        client_id: field("client_id").unwrap_or_default(),
        client_secret: field("client_secret"),
        scope: field("scope").unwrap_or_else(|| {
            if oidc {
                "openid profile".to_owned()
            } else {
                String::new()
            }
        }),
        name_claim: if oidc { field("name_claim") } else { None },
        id_path: if oidc { None } else { field("id_path") },
        name_path: if oidc { None } else { field("name_path") },
        profile_url_template: field("profile_url_template"),
        icon_url: field("icon_url"),
        kind,
    })
}

pub(super) fn build_connection_providers_update(
    form: &MultiValueForm,
) -> InstanceConfigUpdateRequest {
    let count = form
        .parse_value::<usize>(CONNECTION_PROVIDER_COUNT_FIELD)
        .ok()
        .flatten()
        .map(|count| count.min(MAX_PROVIDER_CARDS));
    let providers = count.map(|count| {
        (0..count)
            .filter_map(|index| build_provider(form, index))
            .collect()
    });
    InstanceConfigUpdateRequest {
        integrations: Some(InstanceIntegrationsUpdateRequest {
            connection_providers: providers,
            ..Default::default()
        }),
        ..Default::default()
    }
}
