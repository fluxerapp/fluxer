// SPDX-License-Identifier: AGPL-3.0-or-later

use super::system_actions::{ActionQuery, redirect_back_with_flash};
use crate::{
    api::{
        client::AdminApiClient,
        types::{LimitConfigUpdateRequest, LimitKeyMetadata, LimitRule, LimitRuleFilters},
    },
    middleware::{auth::AuthContext, flash::FlashData},
    state::AppState,
    utils::forms::{MultiValueForm, clean_string},
};
use axum::{
    extract::{Query, Request, State},
    response::Response,
};

fn build_limit_filters(form: &MultiValueForm) -> Option<LimitRuleFilters> {
    let traits = form.list_values_any(&["traits[]", "traits"]);
    let guild_features = form.list_values_any(&["guild_features[]", "guild_features"]);
    if traits.is_empty() && guild_features.is_empty() {
        None
    } else {
        Some(LimitRuleFilters {
            traits,
            guild_features,
        })
    }
}

fn update_limit_rule_values(
    rule: &mut LimitRule,
    form: &MultiValueForm,
    limit_keys: &[String],
    metadata: &std::collections::BTreeMap<String, LimitKeyMetadata>,
    fallback_limits: Option<&std::collections::BTreeMap<String, u64>>,
) {
    let mut limits = std::collections::BTreeMap::new();
    for key in limit_keys {
        if let Some(parsed) = form.parse_u64(key) {
            limits.insert(key.clone(), parsed);
        }
    }
    if limits.is_empty()
        && let Some(defaults) = fallback_limits
    {
        limits.extend(defaults.clone());
    }
    for key in limit_keys {
        if metadata.get(key).is_some_and(|metadata| metadata.is_toggle)
            && rule.limits.contains_key(key)
        {
            limits.entry(key.clone()).or_insert(0);
        }
    }
    rule.limits = limits;
    rule.filters = build_limit_filters(form);
}

pub(super) async fn limit_config_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return redirect_back_with_flash(
                base,
                "/limit-config",
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let secure_cookies = config.secure_cookies();
    let redirect = |flash| redirect_back_with_flash(base, "/limit-config", flash, secure_cookies);
    let current = match client.get_limit_config().await {
        Ok(current) => current,
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: fetch current limit configuration");
            return redirect(FlashData::error(
                "Failed to fetch current limit configuration",
            ));
        }
    };
    let mut limit_config = current.limit_config;
    let (success_message, error_message) = match action {
        "update" => {
            let Some(rule_id) = aq.rule.as_deref().and_then(clean_string) else {
                return redirect(FlashData::error("Rule not found"));
            };
            let Some(rule) = limit_config
                .rules
                .iter_mut()
                .find(|rule| rule.id == rule_id)
            else {
                return redirect(FlashData::error("Rule not found"));
            };
            let fallback = current
                .defaults
                .get(&rule_id)
                .or_else(|| current.defaults.get("default"));
            update_limit_rule_values(
                rule,
                &form,
                &current.limit_keys,
                &current.metadata,
                fallback,
            );
            (
                "Limit configuration updated",
                "Failed to update limit configuration",
            )
        }
        "reset" => {
            let rule = aq
                .rule
                .as_deref()
                .and_then(clean_string)
                .and_then(|rule_id| {
                    limit_config
                        .rules
                        .iter_mut()
                        .find(|rule| rule.id == rule_id)
                });
            let Some(rule) = rule else {
                return redirect(FlashData::error("Rule not found"));
            };
            let Some(defaults) = current.defaults.get(&rule.id) else {
                return redirect(FlashData::error("This rule has no defaults to reset to"));
            };
            rule.limits = defaults.clone();
            ("Limit rule reset to defaults", "Failed to reset limit rule")
        }
        "delete" => {
            let Some(rule_id) = aq.rule.as_deref().and_then(clean_string) else {
                return redirect(FlashData::error("Rule not found"));
            };
            if rule_id == "default" {
                return redirect(FlashData::error("The default rule cannot be deleted"));
            }
            let old_len = limit_config.rules.len();
            limit_config.rules.retain(|rule| rule.id != rule_id);
            if limit_config.rules.len() == old_len {
                return redirect(FlashData::error("Rule not found"));
            }
            ("Limit rule deleted", "Failed to delete limit rule")
        }
        "create" => {
            let Some(rule_id) = form.clean("rule_id") else {
                return redirect(FlashData::error("Rule ID is required"));
            };
            if rule_id == "default" {
                return redirect(FlashData::error("The default rule ID is reserved"));
            }
            if limit_config.rules.iter().any(|rule| rule.id == rule_id) {
                return redirect(FlashData::error("Rule ID already exists"));
            }
            let limits = current.defaults.get("default").cloned().unwrap_or_default();
            limit_config.rules.push(LimitRule {
                id: rule_id,
                filters: build_limit_filters(&form),
                limits,
                modified_fields: None,
            });
            ("Limit rule created", "Failed to create limit rule")
        }
        _ => return redirect(FlashData::success("Limit config updated")),
    };
    let request = LimitConfigUpdateRequest { limit_config };
    let flash = match client.update_limit_config(&request).await {
        Ok(_) => FlashData::success(success_message),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: update limit config");
            match error.validation_message() {
                Some(message) => FlashData::error(format!("{error_message}: {message}")),
                None => FlashData::error(error_message),
            }
        }
    };
    redirect(flash)
}
