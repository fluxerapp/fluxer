// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl::{self, INSTANCE_LIMIT_CONFIG_UPDATE},
    admin_hints,
    api::types::{LimitConfigResponse, LimitKeyMetadata, LimitRule},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{
                BUTTON_BASE_CLASS, FORM_INPUT_CLASS, SECONDARY_BUTTON_CLASS, csrf_input,
                danger_button, form_actions, secondary_button, submit_button,
            },
            icons,
            page_container::{card_with_header, card_with_header_desc, page_header},
            tooltip,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, PreEscaped, html};

const CATEGORY_ORDER: &[&str] = &[
    "features",
    "messages",
    "guilds",
    "channels",
    "expressions",
    "files",
    "social",
    "media",
];

const DESCRIPTION_LINK_CLASS: &str = "rounded font-medium text-brand-primary underline underline-offset-2 \
     transition-colors hover:text-neutral-900 hover:decoration-2 focus:outline-none focus-visible:ring-2 \
     focus-visible:ring-brand-primary focus-visible:ring-offset-2";

const FILTER_PREVIEW_SCRIPT: &str = r#"
(function () {
	if (window.__fluxerLimitFilterPreview) return;
	window.__fluxerLimitFilterPreview = true;

	var FIELDS = ['traits', 'guild_features', 'field_scope'];
	var EMPTY = '___';

	function parse(value) {
		return (value || '')
			.split(',')
			.map(function (item) { return item.trim(); })
			.filter(function (item) { return item.length > 0; });
	}

	function fieldValue(form, name) {
		var input = form.querySelector('[name="' + name + '"]');
		return input ? input.value : '';
	}

	function setValue(form, key, text) {
		Array.prototype.forEach.call(form.querySelectorAll('[data-filter-value="' + key + '"]'), function (node) {
			node.textContent = text;
		});
	}

	function sync(form) {
		var traits = parse(fieldValue(form, 'traits'));
		var features = parse(fieldValue(form, 'guild_features'));
		var hasTraits = traits.length > 0;
		var hasFeatures = features.length > 0;
		var activeScope = hasTraits && hasFeatures ? 'both' : (hasTraits || hasFeatures ? 'either' : 'default');

		Array.prototype.forEach.call(form.querySelectorAll('[data-filter-scope]'), function (row) {
			row.setAttribute('data-state', row.getAttribute('data-filter-scope') === activeScope ? 'active' : 'inactive');
		});

		var scopeLabel;
		var values;
		if (hasTraits && !hasFeatures) {
			scopeLabel = 'users given the trait: ';
			values = traits.join(', ');
		} else if (!hasTraits && hasFeatures) {
			scopeLabel = 'communities given the feature: ';
			values = features.join(', ');
		} else {
			scopeLabel = 'users given this rule as a trait or communities given this rule as a feature';
			values = '';
		}

		setValue(form, 'scope', scopeLabel);
		setValue(form, 'values', values);
		setValue(form, 'user_values', traits.join(',') || EMPTY);
		setValue(form, 'guild_values', features.join(',') || EMPTY);
		var warning = form.querySelector('[data-default-rule-warning]');
		if (warning) {
			warning.hidden = !(fieldValue(form, 'rule_id') === 'default' && (hasTraits || hasFeatures));
		}	}

	function syncAll(root) {
		var scope = root && root.querySelectorAll ? root : document;
		if (scope instanceof HTMLFormElement) {
			sync(scope);
			return;
		}
		Array.prototype.forEach.call(scope.querySelectorAll('form'), sync);
	}

	document.addEventListener('input', function (event) {
		var control = event.target;
		if (!(control instanceof Element)) return;
		if (FIELDS.indexOf(control.name) < 0) return;
		var form = control.closest('form');
		if (form) sync(form);
	});

	if (document.readyState === 'loading') {
		document.addEventListener('DOMContentLoaded', function () {
			syncAll(document);
		});
	} else {
		syncAll(document);
	}

	document.body.addEventListener('htmx:load', function (event) {
		syncAll(event.detail && event.detail.elt ? event.detail.elt : event.target);
	});

	document.addEventListener('htmx:afterRequest', function (event) {
		var detail = event.detail || {};
		if (!detail.successful) return;
		var triggerElement = detail.elt || (detail.requestConfig && detail.requestConfig.elt);
		var form = triggerElement && triggerElement.closest ? triggerElement.closest('form') : null;
		if (!form || form.id !== 'create-rule') return;

		var xhr = detail.xhr;
		var toastHeader = xhr && xhr.getResponseHeader ? xhr.getResponseHeader('X-Fluxer-Admin-Toast') : null;
		if (!toastHeader) return;
		var toast;
		try {
			toast = JSON.parse(toastHeader);
		} catch (error) {
			return;
		}

		if (!toast || toast.level !== 'success') return;
		if (String(toast.message || '').toLowerCase().indexOf('created') < 0) return;

		var refreshPrompt = form.querySelector('[data-refresh-after-create]');
		if (!refreshPrompt) return;
		refreshPrompt.hidden = false;
		refreshPrompt.focus();
	});
})();
"#;

pub fn limit_config_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    selected_rule: Option<&str>,
    response: Option<&LimitConfigResponse>,
) -> Markup {
    let can_update = acl::has_permission(auth.acls(), INSTANCE_LIMIT_CONFIG_UPDATE);
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
        "Configure limits by adding rules that raise limits ABOVE the default for specific users or communities. Rules apply to users or communities depending on their filters. The default rule applies to everyone and sets the baseline; filtered rules can only raise values above it, never lower them."
    } else {
        "Configure limit rules that raise limits above the default for users or communities, based on user traits or community features."
    };
    html! {
        (page_header("Limit Configuration", Some(description)))
        div class="space-y-6" {
            (rule_tabs(config, &rules, active_rule_id, can_update))
            @if let Some(rule) = active_rule {
                (rule_header(
                    &config.base_path,
                    csrf_token,
                    rule,
                    can_update,
                    response.defaults.contains_key(&rule.id),
                    unused_value_count(response, rule),
                ))
                (rule_editor(config, csrf_token, response, rule, can_update))
            } @else {
                div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm" {
                    p class="text-sm text-neutral-500" { "Rule not found." }
                }
            }
            @if can_update {
                (create_rule_section(config, csrf_token, response, active_rule_id))
            }
        }
        script { (PreEscaped(FILTER_PREVIEW_SCRIPT)) }
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
    base: &str,
    csrf_token: &str,
    rule: &LimitRule,
    can_update: bool,
    has_defaults: bool,
    unused_count: usize,
) -> Markup {
    let can_reset = can_update
        && has_defaults
        && rule
            .modified_fields
            .as_ref()
            .is_some_and(|fields| !fields.is_empty());
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
                    @if has_filters(rule) {
                        p class="max-w-2xl text-sm text-neutral-600" {
                            "This rule applies only when every listed trait and guild feature matches. "
                            "Rules with filters can only raise limits, so a value here is used only when it's higher than what the Default rule gives. "
                            "To lower a limit, change it in the Default rule."
                        }
                    }
                    @if unused_count > 0 {
                        p class="max-w-2xl rounded-lg border border-amber-200 bg-amber-50 px-3 py-2 text-sm text-amber-800" {
                            @if unused_count == 1 {
                                "1 value in this rule never applies. It's marked below."
                            } @else {
                                (unused_count) " values in this rule never apply. They're marked below."
                            }
                        }
                    }
                }
                div class="flex flex-wrap gap-2" {
                    @if can_reset {
                        form method="post"
                            action={(base) "/limit-config?action=reset&rule=" (rule.id)} {
                            (csrf_input(csrf_token))
                            (secondary_button("Reset to Defaults"))
                        }
                    }
                    @if can_update && rule.id != "default" {
                        form method="post"
                            action={(base) "/limit-config?action=delete&rule=" (rule.id)} {
                            (csrf_input(csrf_token))
                            (danger_button("Delete Rule"))
                        }
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
                    (filter_inputs(&config.base_path, rule))
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

fn filter_inputs(base: &str, rule: &LimitRule) -> Markup {
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
    let has_traits = !traits.trim().is_empty();
    let has_features = !guild_features.trim().is_empty();
    let is_default_rule = rule.id == "default";
    let show_default_warning = is_default_rule && (has_traits || has_features);
    let active_scope = match (has_traits, has_features) {
        (true, true) => "both",
        (false, false) => "default",
        _ => "either",
    };
    let state_for = |scope: &str| {
        if scope == active_scope {
            "active"
        } else {
            "inactive"
        }
    };
    let (scope_display, values_display) = match (has_traits, has_features) {
        (true, false) => ("users given the trait: ", traits.clone()),
        (false, true) => ("communities given the feature: ", guild_features.clone()),
        _ => (
            "users given this rule as a trait or communities given this rule as a feature",
            String::new(),
        ),
    };
    let traits_display = if has_traits {
        traits.clone()
    } else {
        "…".to_owned()
    };
    let guild_features_display = if has_features {
        guild_features.clone()
    } else {
        "…".to_owned()
    };
    card_with_header_desc(
        "Filters",
        Some(html! {
            "Write custom filter names to only apply this rule to specific users or communities. These filters can then be assigned as traits to a "
            a href={(base) "/users"} hx-boost="false" class=(DESCRIPTION_LINK_CLASS) { "user" }
            ", or a feature for a "
            a href={(base) "/guilds"} hx-boost="false" class=(DESCRIPTION_LINK_CLASS) { "community" }
            ". A filtered rule can only raise a value above the default, and a rule filtered by guild feature never applies user-wide limits."
        }),
        html! {
            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                div class="space-y-2" {
                    label for="traits" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                        "Custom User Traits (Optional)"
                    }
                    input type="text" id="traits" name="traits" value=(traits)
                        placeholder="e.g., premium, supporter" class=(FORM_INPUT_CLASS);
                    p class="text-xs text-neutral-500" { "Separate values with commas; leave blank to disable this filter." }
                }
                div class="space-y-2" {
                    label for="guild_features" class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                        "Custom Guild Features (Optional)"
                    }
                    input type="text" id="guild_features" name="guild_features" value=(guild_features)
                        placeholder="e.g., VIP_SERVERS, BOOSTER_LEVEL_2" class=(FORM_INPUT_CLASS);
                    p class="text-xs text-neutral-500" { "Separate values with commas; leave blank to disable this filter." }
                }
            }
            @if is_default_rule {
                div class="mt-2 rounded-lg border border-amber-200 bg-amber-50 p-3 text-xs text-amber-900"
                    data-default-rule-warning hidden[!show_default_warning] {
                    strong class="font-semibold" { "Not recommended on the default rule." }
                    " The default rule is the baseline for every user and community. Filtering it does not restrict anyone, it means anything that does not match falls back to the built-in defaults. Exceptions, for example Community Creation Access, are best used in their own rule."
                }
            }
            div class="mt-2 rounded-lg border border-neutral-200 bg-neutral-50 p-3" {
                ul class="space-y-1.5 text-xs text-neutral-500" {
                    li class="group flex items-start gap-2 opacity-50 transition-opacity data-[state=active]:opacity-100"
                        data-filter-scope="default" data-state=(state_for("default")) {
                        span aria-hidden="true"
                            class="mt-1 h-2 w-2 shrink-0 rounded-full bg-rose-400 group-data-[state=active]:bg-emerald-500" {}
                        span {
                            @if is_default_rule {
                                "This rule is applied to everyone."
                            } @else {
                                "No filters set, so this rule matches every user and community."
                            }
                        }
                    }
                    li class="group flex items-start gap-2 opacity-50 transition-opacity data-[state=active]:opacity-100"
                        data-filter-scope="either" data-state=(state_for("either")) {
                        span aria-hidden="true"
                            class="mt-1 h-2 w-2 shrink-0 rounded-full bg-rose-400 group-data-[state=active]:bg-emerald-500" {}
                        span {
                            "Applied only to "
                            span data-filter-value="scope" { (scope_display) }
                            span class="font-mono text-neutral-700" data-filter-value="values" { (values_display) }
                        }
                    }
                    li class="group flex items-start gap-2 opacity-50 transition-opacity data-[state=active]:opacity-100"
                        data-filter-scope="both" data-state=(state_for("both")) {
                        span aria-hidden="true"
                            class="mt-1 h-2 w-2 shrink-0 rounded-full bg-rose-400 group-data-[state=active]:bg-emerald-500" {}
                        span {
                            "Applied only when a user has the trait: "
                            span class="font-mono text-neutral-700" data-filter-value="user_values" { (traits_display) }
                            " and their community has the feature: "
                            span class="font-mono text-neutral-700" data-filter-value="guild_values" { (guild_features_display) }
                        }
                    }
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
    let field = if metadata.is_toggle {
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
    };
    match unused_value_reason(response, rule, key, metadata) {
        Some(reason) => html! {
            div class="space-y-2" {
                (field)
                p class="rounded-lg border border-amber-200 bg-amber-50 px-3 py-2 text-sm text-amber-800" {
                    (unused_value_message(reason, metadata))
                }
            }
        },
        None => field,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum UnusedValueReason {
    BelowDefault(u64),
    UserLimitWithGuildFeatures,
    GuildLimitWithOnlyTraits,
}

fn has_filters(rule: &LimitRule) -> bool {
    rule.filters
        .as_ref()
        .is_some_and(|filters| !filters.traits.is_empty() || !filters.guild_features.is_empty())
}

fn default_rule_value(response: &LimitConfigResponse, key: &str) -> Option<u64> {
    response
        .limit_config
        .rules
        .iter()
        .find(|rule| rule.id == "default")
        .and_then(|rule| rule.limits.get(key))
        .or_else(|| {
            response
                .defaults
                .get("default")
                .and_then(|defaults| defaults.get(key))
        })
        .copied()
}

fn unused_value_reason(
    response: &LimitConfigResponse,
    rule: &LimitRule,
    key: &str,
    metadata: &LimitKeyMetadata,
) -> Option<UnusedValueReason> {
    let filters = rule.filters.as_ref()?;
    let has_traits = !filters.traits.is_empty();
    let has_guild_features = !filters.guild_features.is_empty();
    if !has_traits && !has_guild_features {
        return None;
    }
    let value = *rule.limits.get(key)?;
    let baseline = default_rule_value(response, key)?;
    if value == baseline {
        return None;
    }
    if has_guild_features && metadata.scope == "user" {
        return Some(UnusedValueReason::UserLimitWithGuildFeatures);
    }
    if !has_guild_features && metadata.scope == "guild" {
        return Some(UnusedValueReason::GuildLimitWithOnlyTraits);
    }
    if value < baseline {
        return Some(UnusedValueReason::BelowDefault(baseline));
    }
    None
}

fn unused_value_count(response: &LimitConfigResponse, rule: &LimitRule) -> usize {
    response
        .limit_keys
        .iter()
        .filter(|key| {
            response.metadata.get(*key).is_some_and(|metadata| {
                unused_value_reason(response, rule, key, metadata).is_some()
            })
        })
        .count()
}

fn unused_value_message(reason: UnusedValueReason, metadata: &LimitKeyMetadata) -> String {
    match reason {
        UnusedValueReason::BelowDefault(_) if metadata.is_toggle => {
            "This never applies. It's on in the Default rule, and rules with filters can only raise limits, so it stays on. To turn it off, change it in the Default rule.".to_owned()
        }
        UnusedValueReason::BelowDefault(baseline) => format!(
            "This never applies. It's lower than the Default rule's {}, and rules with filters can only raise limits. To lower it, change it in the Default rule.",
            format_value_with_unit(baseline, metadata.unit.as_deref())
        ),
        UnusedValueReason::UserLimitWithGuildFeatures => {
            "This never applies. It's a user limit, which is checked without a guild, so a rule with guild features never matches it. Remove the guild features from this rule, or set this limit in a rule with only user traits.".to_owned()
        }
        UnusedValueReason::GuildLimitWithOnlyTraits => {
            "This never applies. It's a guild limit, which only uses rules with guild features or no filters. Add a guild feature to this rule, or set this limit in the Default rule.".to_owned()
        }
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
    active_rule_id: &str,
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
                        a href={(config.base_path) "/limit-config?rule=" (active_rule_id)}
                            role="button" hx-boost="false" hidden data-refresh-after-create
                            class={(BUTTON_BASE_CLASS) " " (SECONDARY_BUTTON_CLASS)} {
                            (icons::arrow_up_icon())
                            span { "Scroll up & refresh" }
                        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn response() -> LimitConfigResponse {
        let metadata = |key: &str, scope: &str| {
            serde_json::json!({
                "key": key, "label": key, "description": "", "category": "files",
                "scope": scope, "isToggle": false, "unit": null, "min": 0, "max": null
            })
        };
        serde_json::from_value(serde_json::json!({
            "limit_config": {"traitDefinitions": [], "rules": [
                {"id": "default", "limits": {"max_attachments_per_message": 10, "max_screen_share_bitrate": 9000000, "emoji_max_size": 524288}},
                {"id": "no_uploads", "filters": {"traits": ["no_uploads"], "guildFeatures": ["no_uploads"]},
                    "limits": {"max_attachments_per_message": 0, "max_screen_share_bitrate": 9000000, "emoji_max_size": 524288}},
                {"id": "streaming", "filters": {"traits": ["fast"], "guildFeatures": ["fast"]},
                    "limits": {"max_attachments_per_message": 20, "max_screen_share_bitrate": 50000000, "emoji_max_size": 524288}},
                {"id": "traits_only", "filters": {"traits": ["fast"]},
                    "limits": {"max_attachments_per_message": 10, "max_screen_share_bitrate": 50000000, "emoji_max_size": 1048576}}
            ]},
            "limit_config_json": "", "self_hosted": true, "defaults": {}, "categories": null, "bounds": null,
            "limit_keys": ["max_attachments_per_message", "max_screen_share_bitrate", "emoji_max_size"],
            "metadata": {
                "max_attachments_per_message": metadata("max_attachments_per_message", "both"),
                "max_screen_share_bitrate": metadata("max_screen_share_bitrate", "user"),
                "emoji_max_size": metadata("emoji_max_size", "guild")
            }
        }))
        .unwrap()
    }

    fn reasons(rule_id: &str) -> Vec<(String, Option<UnusedValueReason>)> {
        let response = response();
        let rule = response
            .limit_config
            .rules
            .iter()
            .find(|rule| rule.id == rule_id)
            .unwrap();
        response
            .limit_keys
            .iter()
            .map(|key| {
                let metadata = response.metadata.get(key).unwrap();
                (
                    key.clone(),
                    unused_value_reason(&response, rule, key, metadata),
                )
            })
            .collect()
    }

    #[test]
    fn flags_values_that_can_never_apply() {
        assert!(
            reasons("default")
                .iter()
                .all(|(_, reason)| reason.is_none())
        );
        assert_eq!(
            reasons("no_uploads")[0],
            (
                "max_attachments_per_message".to_owned(),
                Some(UnusedValueReason::BelowDefault(10))
            )
        );
        assert_eq!(
            reasons("streaming"),
            vec![
                ("max_attachments_per_message".to_owned(), None),
                (
                    "max_screen_share_bitrate".to_owned(),
                    Some(UnusedValueReason::UserLimitWithGuildFeatures)
                ),
                ("emoji_max_size".to_owned(), None),
            ]
        );
        assert_eq!(
            reasons("traits_only"),
            vec![
                ("max_attachments_per_message".to_owned(), None),
                ("max_screen_share_bitrate".to_owned(), None),
                (
                    "emoji_max_size".to_owned(),
                    Some(UnusedValueReason::GuildLimitWithOnlyTraits)
                ),
            ]
        );
    }
}
