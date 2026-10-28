// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{InstanceConfigUpdateRequest, SsoConfigUpdateRequest, SsoGroupRoleMapping},
    templates::pages::instance_config::SSO_ALLOWED_DOMAINS_FIELD,
    utils::forms::MultiValueForm,
};

fn parse_sso_group_role_mappings(raw: &str) -> Result<Vec<SsoGroupRoleMapping>, String> {
    raw.lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            let invalid = || {
                format!(
                    "Write each group mapping as a group name, \"=\" and a role ID, not \"{line}\""
                )
            };
            let (group, role_id) = line.rsplit_once('=').ok_or_else(invalid)?;
            let (group, role_id) = (group.trim(), role_id.trim());
            if group.is_empty()
                || role_id.is_empty()
                || !role_id.chars().all(|c| c.is_ascii_digit())
            {
                return Err(invalid());
            }
            Ok(SsoGroupRoleMapping {
                group: group.to_string(),
                role_id: role_id.to_string(),
            })
        })
        .collect()
}

pub(super) fn build_sso_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    let flag = |key: &str| form.bool_value(key);
    let get = |key: &str| Some(form.clean(key));
    let new_secret = form.clean("sso_client_secret");
    let clear_secret = flag("sso_clear_client_secret");
    let allowed = form.list_values_any(&["sso_allowed_domains[]", SSO_ALLOWED_DOMAINS_FIELD]);
    let client_secret = if new_secret.is_some() {
        Some(new_secret)
    } else if clear_secret {
        Some(None)
    } else {
        None
    };
    Ok(InstanceConfigUpdateRequest {
        sso: Some(SsoConfigUpdateRequest {
            enabled: Some(flag("sso_enabled")),
            enforced: Some(flag("sso_enforced")),
            auto_provision: Some(flag("sso_auto_provision")),
            username_from_provider: Some(flag("sso_username_from_provider")),
            display_name: get("sso_display_name"),
            button_label: get("sso_button_label"),
            issuer: get("sso_issuer"),
            authorization_url: get("sso_authorization_url"),
            token_url: get("sso_token_url"),
            userinfo_url: get("sso_userinfo_url"),
            jwks_url: get("sso_jwks_url"),
            client_id: get("sso_client_id"),
            client_secret,
            scope: get("sso_scope"),
            allowed_domains: Some(allowed),
            redirect_uri: None,
            groups_claim: form
                .first("sso_groups_claim")
                .map(|_| form.clean("sso_groups_claim")),
            group_role_mappings: form
                .first("sso_group_role_mappings")
                .map(parse_sso_group_role_mappings)
                .transpose()?,
        }),
        ..Default::default()
    })
}
