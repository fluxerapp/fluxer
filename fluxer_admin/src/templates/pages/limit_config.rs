// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl::{self, INSTANCE_LIMIT_CONFIG_UPDATE},
    admin_hints,
    api::types::{LimitConfigResponse, LimitKeyMetadata, LimitRule},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{FORM_INPUT_CLASS, csrf_input, danger_button, form_actions, submit_button},
            page_container::{card_with_header, page_header},
            tooltip,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

const CATEGORY_ORDER: &[&str] = &[
    "features",
    "messages",
    "guilds",
    "channels",
    "expressions",
    "files",
    "social",
];

pub fn limit_config_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    selected_rule: Option<&str>,
    response: Option<&LimitConfigResponse>,
) -> Markup {
    let can_update = auth
        .admin_user
        .as_ref()
        .is_some_and(|user| acl::has_permission(&user.acls, INSTANCE_LIMIT_CONFIG_UPDATE));
    let content = html! {
        @if let Some(response) = response {
            (render_limit_page(config, csrf_token, selected_rule, response, can_update))
        } @else {
            (page_header("Limit Configuration", Some("Manage rate limits and resource limits")))
            div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm" {
                p class="text-sm text-red-600" { "Failed to load limit configuration." }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Limit Configuration",
        "limit-config",
        None,
        content,
    )
}

fn render_limit_page(
    config: &AdminConfig,
    csrf_token: &str,
    selected_rule: Option<&str>,
    response: &LimitConfigResponse,
    can_update: bool,
) -> Markup {
    let rules = sorted_rules(response);
    let active_rule_id = selected_rule
        .filter(|rule_id| rules.iter().any(|rule| rule.id == *rule_id))
        .or_else(|| rules.first().map(|rule| rule.id.as_str()))
        .unwrap_or("default");
    let active_rule = rules.iter().find(|rule| rule.id == active_rule_id);
    let description = if response.self_hosted.unwrap_or(false) {
        "Self-hosted instance with all premium features enabled by default. Configure limits to customize user and guild restrictions."
    } else {
        "Configure limit rules that control user and guild restrictions. Different rules apply based on user traits or guild features."
    };
    html! {
        (page_header("Limit Configuration", Some(description)))
        div class="space-y-6" {
            (rule_tabs(config, &rules, active_rule_id, can_update))
            @if let Some(rule) = active_rule {
                (rule_header(config, csrf_token, rule, can_update))
                (rule_editor(config, csrf_token, response, rule, can_update))
            } @else {
                div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm" {
                    p class="text-sm text-neutral-500" { "Rule not found." }
                }
            }
            @if can_update {
                (create_rule_section(config, csrf_token, response))
            }
        }
    }
}

fn sorted_rules(response: &LimitConfigResponse) -> Vec<&LimitRule> {
    let mut rules: Vec<&LimitRule> = response.limit_config.rules.iter().collect();
    if let Some(default_index) = rules.iter().position(|rule| rule.id == "default")
        && default_index > 0
    {
        let default_rule = rules.remove(default_index);
        rules.insert(0, default_rule);
    }
    rules
}

fn rule_tabs(
    config: &AdminConfig,
    rules: &[&LimitRule],
    active_rule_id: &str,
    can_update: bool,
) -> Markup {
    let base = &config.base_path;
    html! {
        div class="flex flex-wrap gap-2" {
            @for rule in rules {
                @let active = rule.id == active_rule_id;
                a href={(base) "/limit-config?rule=" (rule.id)}
                    class={
                        "inline-flex items-center justify-center gap-2 rounded-lg px-3 py-1.5 text-sm font-medium transition-all "
                        (if active {
                            "bg-neutral-900 text-white hover:bg-neutral-800"
                        } else {
                            "border border-neutral-300 bg-neutral-50 text-neutral-700 hover:border-neutral-400 hover:text-neutral-900"
                        })
                    } {
                    (format_rule_name(rule))
                    @if let Some(count) = rule.modified_fields.as_ref().map(Vec::len).filter(|count| *count > 0) {
                        span class={
                            "rounded-full px-1.5 py-0.5 text-xs "
                            (if active { "bg-white/20 text-white" } else { "bg-neutral-100 text-neutral-700" })
                        } {
                            (count) " modified"
                        }
                    }
                }
            }
            @if can_update {
                a href="#create-rule"
                    class="inline-flex items-center justify-center gap-2 rounded-lg border \
                           border-neutral-300 bg-neutral-50 px-3 py-1.5 text-sm font-medium \
                           text-neutral-700 transition-all hover:border-neutral-400 \
                           hover:text-neutral-900" {
                    "+ Create New Rule"
                }
            }
        }
    }
}

fn rule_header(
    config: &AdminConfig,
    csrf_token: &str,
    rule: &LimitRule,
    can_update: bool,
) -> Markup {
    card_with_header(
        "",
        html! {
            div class="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between" {
                div class="space-y-2" {
                    div class="flex flex-wrap items-center gap-2" {
                        h2 class="text-lg font-semibold text-neutral-900" { (format_rule_name(rule)) }
                        span class="text-sm text-neutral-500" { "ID: " (rule.id) }
                    }
                    @if let Some(filters) = &rule.filters {
                        div class="space-y-1 text-sm text-neutral-500" {
                            @if !filters.traits.is_empty() {
                                p { "Traits: " (filters.traits.join(", ")) }
                            }
                            @if !filters.guild_features.is_empty() {
                                p { "Guild Features: " (filters.guild_features.join(", ")) }
                            }
                        }
                    }
                }
                @if can_update && rule.id != "default" {
                    form method="post"
                        action={(config.base_path) "/limit-config?action=delete&rule=" (rule.id)} {
                        (csrf_input(csrf_token))
                        (danger_button("Delete Rule"))
                    }
                }
            }
        },
    )
}

fn rule_editor(
    config: &AdminConfig,
    csrf_token: &str,
    response: &LimitConfigResponse,
    rule: &LimitRule,
    can_update: bool,
) -> Markup {
    let action = format!(
        "{}/limit-config?action=update&rule={}",
        config.base_path, rule.id
    );
    html! {
        form method="post" action=(action) {
            (csrf_input(csrf_token))
            input type="hidden" name="rule_id" value=(rule.id);
            div class="space-y-6" {
                @if can_update {
                    (filter_inputs(rule))
                }
                @for category in CATEGORY_ORDER {
                    @let keys = keys_for_category(response, category);
                    @if !keys.is_empty() {
                        (category_section(&config.base_path, response, rule, category, &keys, can_update))
                    }
                }
                @if can_update {
                    div class="sticky bottom-0 -mx-3 -mb-3 border-t border-neutral-200 bg-neutral-50 px-3 py-4 sm:-mx-6 sm:-mb-6 sm:px-6" {
                        (form_actions(html! {
                            (submit_button("Save Changes"))
                        }))
                    }
                }
            }
        }
    }
}

fn filter_inputs(rule: &LimitRule) -> Markup {
    let traits = rule
        .filters
        .as_ref()
        .map(|filters| filters.traits.join(", "))
        .unwrap_or_default();
    let guild_features = rule
        .filters
        .as_ref()
        .map(|filters| filters.guild_features.join(", "))
        .unwrap_or_default();
    card_with_header(
        "Filters",
        html! {
            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                div class="space-y-2" {
                    label for="traits" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                        "User Traits (Optional)"
                    }
                    input type="text" id="traits" name="traits" value=(traits)
                        placeholder="e.g., premium, supporter" class=(FORM_INPUT_CLASS);
                    p class="text-xs text-neutral-500" { "Separate values with commas; leave blank to disable this filter." }
                }
                div class="space-y-2" {
                    label for="guild_features" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                        "Guild Features (Optional)"
                    }
                    input type="text" id="guild_features" name="guild_features" value=(guild_features)
                        placeholder="e.g., VIP_SERVERS, BOOSTER_LEVEL_2" class=(FORM_INPUT_CLASS);
                    p class="text-xs text-neutral-500" { "Separate values with commas; leave blank to disable this filter." }
                }
            }
        },
    )
}

fn keys_for_category(response: &LimitConfigResponse, category: &str) -> Vec<String> {
    response
        .limit_keys
        .iter()
        .filter(|key| {
            response
                .metadata
                .get(*key)
                .is_some_and(|metadata| metadata.category == category)
        })
        .cloned()
        .collect()
}

fn category_section(
    base: &str,
    response: &LimitConfigResponse,
    rule: &LimitRule,
    category: &str,
    keys: &[String],
    can_update: bool,
) -> Markup {
    let title = response
        .categories
        .as_ref()
        .and_then(|categories| categories.get(category))
        .cloned()
        .unwrap_or_else(|| format_rule_name_str(category));
    card_with_header(
        &title,
        html! {
            div class="space-y-4" {
                @for key in keys {
                    @if let Some(metadata) = response.metadata.get(key) {
                        (limit_field(base, response, rule, key, metadata, can_update))
                    }
                }
            }
        },
    )
}

fn limit_field(
    base: &str,
    response: &LimitConfigResponse,
    rule: &LimitRule,
    key: &str,
    metadata: &LimitKeyMetadata,
    can_update: bool,
) -> Markup {
    let current_value = rule.limits.get(key).copied();
    let default_value = response
        .defaults
        .get(&rule.id)
        .or_else(|| response.defaults.get("default"))
        .and_then(|defaults| defaults.get(key))
        .copied();
    let modified = rule
        .modified_fields
        .as_ref()
        .is_some_and(|fields| fields.iter().any(|field| field == key));
    if metadata.is_toggle {
        toggle_field(base, key, metadata, current_value, modified, can_update)
    } else {
        numeric_field(
            base,
            key,
            metadata,
            current_value,
            default_value,
            modified,
            can_update,
        )
    }
}

fn toggle_field(
    base: &str,
    key: &str,
    metadata: &LimitKeyMetadata,
    current_value: Option<u64>,
    modified: bool,
    can_update: bool,
) -> Markup {
    let enabled = current_value.is_some_and(|value| value > 0);
    html! {
        div class={(field_class(modified, false))} {
            div class="flex-1 space-y-1" {
                (field_label_row(base, key, metadata, modified))
                p class="text-xs text-neutral-500" { (metadata.description) }
            }
            div class="shrink-0" {
                @if can_update {
                    label class="relative inline-flex cursor-pointer items-center" {
                        input type="checkbox" name=(key) id=(key) value="1" checked[enabled] class="peer sr-only";
                        div class="peer h-6 w-11 rounded-full bg-neutral-200 after:absolute \
                                   after:top-[2px] after:left-[2px] after:h-5 after:w-5 \
                                   after:rounded-full after:border after:border-neutral-300 \
                                   after:bg-white after:transition-all after:content-[''] \
                                   peer-checked:bg-blue-600 peer-checked:after:translate-x-full \
                                   peer-checked:after:border-white peer-focus:outline-none \
                                   peer-focus:ring-2 peer-focus:ring-blue-300" {}
                    }
                } @else {
                    span class={(if enabled { "text-sm font-medium text-green-700" } else { "text-sm font-medium text-neutral-500" })} {
                        (if enabled { "Enabled" } else { "Disabled" })
                    }
                }
            }
        }
    }
}

fn numeric_field(
    base: &str,
    key: &str,
    metadata: &LimitKeyMetadata,
    current_value: Option<u64>,
    default_value: Option<u64>,
    modified: bool,
    can_update: bool,
) -> Markup {
    let value = current_value
        .map(|value| value.to_string())
        .unwrap_or_default();
    let placeholder = default_value
        .map(|value| format_value_with_unit(value, metadata.unit.as_deref()))
        .unwrap_or_default();
    html! {
        div class={(field_class(modified, true))} {
            div class="flex flex-wrap items-center justify-between gap-2" {
                (field_label_row(base, key, metadata, modified))
            }
            p class="text-xs text-neutral-500" {
                (metadata.description)
                @if let (Some(min), Some(max)) = (metadata.min, metadata.max) {
                    " (Allowed: " (min) "-" (max) ")"
                }
            }
            div class="flex flex-wrap items-center gap-3" {
                @if can_update {
                    input type="number" name=(key) id=(key) value=(value)
                        placeholder=(placeholder) min=(metadata.min.unwrap_or(0))
                        max=[metadata.max] class={(FORM_INPUT_CLASS) " max-w-xs"};
                } @else {
                    span class="text-sm text-neutral-900" {
                        @if let Some(value) = current_value {
                            (format_value_with_unit(value, metadata.unit.as_deref()))
                        } @else {
                            "-"
                        }
                    }
                }
                @if let Some(default_value) = default_value {
                    span class="text-xs text-neutral-500" {
                        "Default: " (format_value_with_unit(default_value, metadata.unit.as_deref()))
                    }
                }
            }
        }
    }
}

fn field_label_row(base: &str, key: &str, metadata: &LimitKeyMetadata, modified: bool) -> Markup {
    html! {
        div class="flex flex-wrap items-center gap-2" {
            label for=(key) class="font-medium text-neutral-900 text-sm" { (metadata.label) }
            @if let Some(hint) = admin_hints::limit_key_hint(key) {
                (tooltip::info(base, &hint))
            }
            span class=(scope_class(&metadata.scope)) { (scope_label(&metadata.scope)) }
            @if modified {
                span class="rounded bg-neutral-100 px-1.5 py-0.5 text-neutral-700 text-xs" { "Modified" }
            }
        }
    }
}

fn field_class(modified: bool, vertical: bool) -> String {
    let layout = if vertical {
        "flex flex-col gap-2"
    } else {
        "flex items-center justify-between gap-2"
    };
    let bg = if modified {
        "border-neutral-200 bg-neutral-50"
    } else {
        "border-neutral-200 bg-white"
    };
    format!("{layout} rounded-lg border p-3 {bg}")
}

fn scope_class(scope: &str) -> &'static str {
    match scope {
        "user" => "rounded bg-blue-100 px-1.5 py-0.5 text-blue-700 text-xs",
        "guild" => "rounded bg-purple-100 px-1.5 py-0.5 text-purple-700 text-xs",
        _ => "rounded bg-neutral-100 px-1.5 py-0.5 text-neutral-600 text-xs",
    }
}

fn scope_label(scope: &str) -> String {
    match scope {
        "user" => "User".to_owned(),
        "guild" => "Guild".to_owned(),
        "both" => "Both".to_owned(),
        other => format_rule_name_str(other),
    }
}

fn create_rule_section(
    config: &AdminConfig,
    csrf_token: &str,
    response: &LimitConfigResponse,
) -> Markup {
    let existing_ids = sorted_rules(response)
        .iter()
        .map(|rule| rule.id.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    card_with_header(
        "Create New Limit Rule",
        html! {
            form id="create-rule" method="post" action={(config.base_path) "/limit-config?action=create"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="space-y-2" {
                        label for="new-rule-id" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                            "Rule ID"
                        }
                        input type="text" id="new-rule-id" name="rule_id" required
                            placeholder="e.g., supporter, vip, custom" class=(FORM_INPUT_CLASS);
                        p class="text-xs text-neutral-500" { "Unique identifier for this rule." }
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        div class="space-y-2" {
                            label for="new-rule-traits" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                                "User Traits (Optional)"
                            }
                            input type="text" id="new-rule-traits" name="traits"
                                placeholder="e.g., premium, supporter" class=(FORM_INPUT_CLASS);
                        }
                        div class="space-y-2" {
                            label for="new-rule-features" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                                "Guild Features (Optional)"
                            }
                            input type="text" id="new-rule-features" name="guild_features"
                                placeholder="e.g., VIP_SERVERS, BOOSTER_LEVEL_2" class=(FORM_INPUT_CLASS);
                        }
                    }
                    div class="rounded bg-neutral-50 p-3" {
                        p class="text-xs text-neutral-500" { "Existing rule IDs: " (existing_ids) }
                    }
                    (form_actions(html! {
                        (submit_button("Create Rule"))
                    }))
                }
            }
        },
    )
}

fn format_rule_name(rule: &LimitRule) -> String {
    format_rule_name_str(&rule.id)
}

fn format_rule_name_str(value: &str) -> String {
    let mut chars = value.chars();
    match chars.next() {
        Some(first) => format!("{}{}", first.to_uppercase(), chars.as_str()),
        None => String::new(),
    }
}

fn format_value_with_unit(value: u64, unit: Option<&str>) -> String {
    match unit {
        Some("bytes") => format!("{value} bytes"),
        Some("count") | None => value.to_string(),
        Some(other) => format!("{value} {other}"),
    }
}
