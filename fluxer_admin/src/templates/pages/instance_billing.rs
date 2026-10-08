// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        AppBrandingConfigResponse, BillingCatalogMode, BillingPriceSet, InstanceBillingResponse,
        PremiumMode, TRI_STATE_DEFAULT, TRI_STATE_OFF, TRI_STATE_ON,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{
            FORM_INPUT_CLASS, FORM_SELECT_CLASS, checkbox, csrf_input, form_actions,
            form_field_group, select_chevron, submit_button, text_input, textarea_input,
        },
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

const PRICE_COLUMNS: [(&str, &str); 4] = [
    ("billing_price_monthly", "Monthly"),
    ("billing_price_yearly", "Yearly"),
    ("billing_price_gift_1_month", "Gift 1 month"),
    ("billing_price_gift_1_year", "Gift 1 year"),
];

pub fn billing_blockers(
    billing: &InstanceBillingResponse,
    premium_mode: PremiumMode,
) -> Vec<&'static str> {
    if billing.billing_active {
        return Vec::new();
    }
    let mut blockers = Vec::new();
    if matches!(premium_mode, PremiumMode::Everyone) {
        blockers.push("the premium model is Everyone, so there is no paid tier to sell");
    }
    if !billing.effective_enabled {
        blockers.push("billing is not enabled");
    }
    if !billing.stripe_secret_key_set {
        blockers.push("no Stripe secret key is set");
    }
    let has_pair = billing
        .prices
        .as_ref()
        .is_some_and(|prices| prices.values().any(BillingPriceSet::has_recurring_pair));
    if billing.catalog_mode == BillingCatalogMode::Operator && !has_pair {
        blockers.push("no currency has both a monthly and a yearly price ID");
    }
    if blockers.is_empty() {
        blockers.push(match billing.catalog_mode {
            BillingCatalogMode::Env => {
                "the environment price catalog has no currency with both a monthly and a yearly price ID"
            }
            BillingCatalogMode::Operator => "the API reports billing as inactive",
        });
    }
    blockers
}

fn billing_status(billing: &InstanceBillingResponse, premium_mode: PremiumMode) -> Markup {
    let blockers = billing_blockers(billing, premium_mode);
    html! {
        div class="space-y-2" {
            div class="flex flex-wrap items-center gap-2" {
                @if billing.billing_active {
                    (badge("Billing active", BadgeVariant::Success))
                } @else {
                    (badge("Billing inactive", BadgeVariant::Default))
                }
                @match billing.catalog_mode {
                    BillingCatalogMode::Operator => {
                        (badge("Catalog: price table", BadgeVariant::Default))
                    }
                    BillingCatalogMode::Env => {
                        (badge("Catalog: environment", BadgeVariant::Default))
                    }
                }
                (secret_badge(
                    "Stripe secret key",
                    billing.stripe_secret_key_set,
                    billing.stripe_secret_key_stored,
                    BadgeVariant::Default,
                ))
                (secret_badge(
                    "Webhook secret",
                    billing.stripe_webhook_secret_set,
                    billing.stripe_webhook_secret_stored,
                    BadgeVariant::Warning,
                ))
            }
            @if !blockers.is_empty() {
                p class="text-sm text-neutral-600" {
                    "Purchases are unavailable because " (blockers.join("; ")) "."
                }
            }
            @if billing.billing_active && !billing.stripe_webhook_secret_set {
                p class="text-sm text-amber-700" {
                    "Without a webhook secret, Stripe events are rejected, so subscriptions never reach accounts."
                }
            }
        }
    }
}

fn secret_badge(label: &str, is_set: bool, is_stored: bool, missing: BadgeVariant) -> Markup {
    match (is_set, is_stored) {
        (_, true) => badge(&format!("{label} set"), BadgeVariant::Success),
        (true, false) => badge(&format!("{label} from environment"), BadgeVariant::Success),
        (false, false) => badge(&format!("{label} missing"), missing),
    }
}

fn secret_field(
    name: &str,
    clear_name: &str,
    label: &str,
    is_set: bool,
    is_stored: bool,
) -> Markup {
    let helper = if !is_stored && is_set {
        "Set from the environment. Enter a value to override it, or leave blank to keep using it."
    } else {
        "Leave blank to keep the current value."
    };
    html! {
        div class="flex flex-col gap-2" {
            (form_field_group(
                label,
                name,
                false,
                None,
                Some(helper),
                html! {
                    input type="password" id=(name) name=(name) value="" class=(FORM_INPUT_CLASS)
                        autocomplete="new-password";
                },
            ))
            @if is_stored {
                (checkbox(clear_name, "true", "Clear the stored value", false, true))
            }
        }
    }
}

fn tri_state_value(value: Option<bool>) -> &'static str {
    match value {
        None => TRI_STATE_DEFAULT,
        Some(true) => TRI_STATE_ON,
        Some(false) => TRI_STATE_OFF,
    }
}

fn tri_state_select(
    name: &str,
    label: &str,
    default_label: &str,
    stored: Option<bool>,
    helper: &str,
) -> Markup {
    let selected = tri_state_value(stored);
    let options = [
        (TRI_STATE_DEFAULT, default_label),
        (TRI_STATE_ON, "On"),
        (TRI_STATE_OFF, "Off"),
    ];
    form_field_group(
        label,
        name,
        false,
        None,
        Some(helper),
        html! {
            div class="relative" {
                select id=(name) name=(name) class=(FORM_SELECT_CLASS) {
                    @for (value, display) in options {
                        option value=(value) selected[value == selected] { (display) }
                    }
                }
                (select_chevron())
            }
        },
    )
}

fn on_off(value: bool) -> &'static str {
    if value { "on" } else { "off" }
}

fn checkout_options(billing: &InstanceBillingResponse) -> Markup {
    let automatic_tax = format!(
        "Calculates tax at checkout. Needs Stripe Tax activated and a head office address in the Stripe dashboard. Currently {}.",
        on_off(billing.effective_automatic_tax)
    );
    let tax_id = format!(
        "Lets buyers add a VAT or other tax ID at checkout. Pair it with automatic tax. Currently {}.",
        on_off(billing.effective_tax_id_collection)
    );
    let terms = format!(
        "Buyers must accept your terms of service at checkout. Needs a terms of service URL in the Stripe dashboard public details. Currently {}.",
        on_off(billing.effective_terms_consent_required)
    );
    html! {
        div class="space-y-4" {
            h4 class="text-sm font-medium text-neutral-900" { "Checkout options" }
            div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                (tri_state_select(
                    "billing_automatic_tax",
                    "Automatic tax",
                    "Use default",
                    billing.automatic_tax,
                    &automatic_tax,
                ))
                (tri_state_select(
                    "billing_tax_id_collection",
                    "Tax ID collection",
                    "Use default",
                    billing.tax_id_collection,
                    &tax_id,
                ))
                (tri_state_select(
                    "billing_terms_consent_required",
                    "Terms consent",
                    "Use default",
                    billing.terms_consent_required,
                    &terms,
                ))
            }
            p class="text-xs text-neutral-500" {
                "Members manage and cancel subscriptions in the Stripe customer portal. It only opens after you save its \
                 settings once in the Stripe dashboard under Settings, Billing, Customer portal."
            }
        }
    }
}

fn price_cell(name: &str, label: &str, value: Option<&str>) -> Markup {
    html! {
        td class="px-2 py-2" {
            input type="text" name=(name) value=(value.unwrap_or(""))
                placeholder="price_..." aria-label=(label)
                autocomplete="off" spellcheck="false"
                class=(FORM_INPUT_CLASS);
        }
    }
}

fn price_row(currency: &str, set: &BillingPriceSet) -> Markup {
    let values = [
        set.monthly.as_deref(),
        set.yearly.as_deref(),
        set.gift_1_month.as_deref(),
        set.gift_1_year.as_deref(),
    ];
    html! {
        tr {
            td class="px-2 py-2" {
                input type="text" name="billing_price_currency" value=(currency)
                    placeholder="GBP" maxlength="3" aria-label="Currency"
                    autocomplete="off" spellcheck="false"
                    class={(FORM_INPUT_CLASS) " w-24 uppercase"};
            }
            @for ((name, label), value) in PRICE_COLUMNS.iter().zip(values) {
                (price_cell(name, label, value))
            }
        }
    }
}

fn price_table(billing: &InstanceBillingResponse) -> Markup {
    let empty = BillingPriceSet::default();
    html! {
        div class="space-y-2" {
            h4 class="text-sm font-medium text-neutral-900" { "Prices" }
            p class="text-xs text-neutral-500" {
                "One row per currency, using Stripe price IDs from your own account. Monthly and yearly are the \
                 subscription prices; the gift prices are one-time prices for buying gifts. Clear a currency to \
                 remove its row. Leave the table empty to use the prices from environment variables."
            }
            div class="overflow-x-auto" {
                table class="min-w-full text-sm" {
                    thead {
                        tr class="text-left text-xs text-neutral-500" {
                            th class="px-2 py-1 font-medium" { "Currency" }
                            @for (_, label) in PRICE_COLUMNS {
                                th class="px-2 py-1 font-medium" { (label) }
                            }
                        }
                    }
                    tbody {
                        @if let Some(prices) = &billing.prices {
                            @for (currency, set) in prices {
                                (price_row(currency, set))
                            }
                        }
                        (price_row("", &empty))
                    }
                }
            }
        }
    }
}

fn country_currencies_text(billing: &InstanceBillingResponse) -> String {
    billing
        .country_currencies
        .iter()
        .flatten()
        .map(|(country, currency)| format!("{country}={currency}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn legacy_prices_text(billing: &InstanceBillingResponse) -> String {
    billing
        .legacy_prices
        .iter()
        .flatten()
        .flat_map(|(slot, ids)| ids.iter().map(move |id| format!("{slot}={id}")))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn premium_billing_section(
    base: &str,
    csrf_token: &str,
    branding: &AppBrandingConfigResponse,
    billing: &InstanceBillingResponse,
    premium_mode: PremiumMode,
) -> Markup {
    let enabled_helper = format!(
        "Use environment setting follows FLUXER_STRIPE_ENABLED or the config file. Billing is currently {}.",
        on_off(billing.effective_enabled)
    );
    section_card_with_description(
        "Premium & Billing",
        "Name the premium tier and sell it through your own Stripe account. Subscriptions and gift purchases need \
         the Mirror premium model, a Stripe secret key and at least one currency with monthly and yearly prices.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_billing"}
                data-admin-result-form="true" {
                (csrf_input(csrf_token))
                div class="space-y-8" {
                    div class="space-y-4" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Premium tier" }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "billing_premium_product_name",
                                "Premium name",
                                &branding.premium_product_name,
                                "Premium",
                            ))
                            (text_input(
                                "billing_premium_info_url",
                                "Premium info URL",
                                branding.premium_info_url.as_deref().unwrap_or(""),
                                "https://example.com/premium",
                            ))
                        }
                        p class="text-xs text-neutral-500" {
                            "Clients show this name wherever the premium tier is mentioned. Clear it to use the default. \
                             The info URL is an optional page that describes the tier."
                        }
                        @if matches!(premium_mode, PremiumMode::Everyone) {
                            p class="text-sm text-amber-700" {
                                "The premium model is Everyone, so every member already has premium limits and clients hide \
                                 premium. Switch the premium model to Mirror to sell subscriptions or redeem gift codes."
                            }
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Stripe" }
                        (billing_status(billing, premium_mode))
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (tri_state_select(
                                "billing_enabled",
                                "Billing",
                                "Use environment setting",
                                billing.enabled,
                                &enabled_helper,
                            ))
                        }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (secret_field(
                                "billing_stripe_secret_key",
                                "billing_clear_stripe_secret_key",
                                "Stripe secret key",
                                billing.stripe_secret_key_set,
                                billing.stripe_secret_key_stored,
                            ))
                            (secret_field(
                                "billing_stripe_webhook_secret",
                                "billing_clear_stripe_webhook_secret",
                                "Stripe webhook signing secret",
                                billing.stripe_webhook_secret_set,
                                billing.stripe_webhook_secret_stored,
                            ))
                        }
                        (form_field_group(
                            "Webhook URL",
                            "billing_webhook_url",
                            false,
                            None,
                            Some("Add this endpoint in the Stripe dashboard, then paste its signing secret above."),
                            html! {
                                input type="text" id="billing_webhook_url" value=(billing.webhook_url)
                                    readonly class=(FORM_INPUT_CLASS);
                            },
                        ))
                        (checkout_options(billing))
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Catalog" }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "billing_default_currency",
                                "Default currency",
                                billing.default_currency.as_deref().unwrap_or(""),
                                "GBP",
                            ))
                        }
                        p class="text-xs text-neutral-500" {
                            "Used when a buyer's country has no mapping below. Leave blank to use the first currency in the table."
                        }
                        (price_table(billing))
                        div class="grid grid-cols-1 gap-4 lg:grid-cols-2" {
                            div class="space-y-2" {
                                (textarea_input(
                                    "billing_country_currencies",
                                    "Country currencies",
                                    "SE=SEK\nGB=GBP",
                                    &country_currencies_text(billing),
                                    6,
                                    false,
                                ))
                                p class="text-xs text-neutral-500" {
                                    "One COUNTRY=CURRENCY per line, using 2-letter country codes. Each currency needs a row in the table."
                                }
                            }
                            div class="space-y-2" {
                                (textarea_input(
                                    "billing_legacy_prices",
                                    "Legacy prices",
                                    "monthly_GBP=price_...",
                                    &legacy_prices_text(billing),
                                    6,
                                    false,
                                ))
                                p class="text-xs text-neutral-500" {
                                    "Older price IDs that existing subscribers may still be on, one SLOT_CURRENCY=price ID per line. \
                                     Repeat a slot for several IDs. Slots are monthly, yearly, gift_1_month and gift_1_year."
                                }
                            }
                        }
                    }

                    (form_actions(html! {
                        (submit_button("Save premium & billing"))
                    }))
                }
            }
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn operator_billing() -> InstanceBillingResponse {
        InstanceBillingResponse {
            enabled: Some(true),
            effective_enabled: true,
            stripe_secret_key_set: true,
            stripe_webhook_secret_set: true,
            stripe_secret_key_stored: true,
            stripe_webhook_secret_stored: true,
            default_currency: Some("GBP".to_owned()),
            prices: Some(BTreeMap::from([(
                "GBP".to_owned(),
                BillingPriceSet {
                    monthly: Some("price_1GbpM".to_owned()),
                    yearly: Some("price_1GbpY".to_owned()),
                    gift_1_month: None,
                    gift_1_year: Some("price_1GbpG".to_owned()),
                },
            )])),
            country_currencies: Some(BTreeMap::from([
                ("GB".to_owned(), "GBP".to_owned()),
                ("IE".to_owned(), "GBP".to_owned()),
            ])),
            legacy_prices: Some(BTreeMap::from([(
                "monthly_GBP".to_owned(),
                vec!["price_1OldA".to_owned(), "price_1OldB".to_owned()],
            )])),
            billing_active: true,
            stripe_serviceable: true,
            catalog_mode: BillingCatalogMode::Operator,
            webhook_url: "https://api.example.com/stripe/webhook".to_owned(),
            automatic_tax: None,
            tax_id_collection: Some(true),
            terms_consent_required: Some(false),
            effective_automatic_tax: false,
            effective_tax_id_collection: true,
            effective_terms_consent_required: false,
        }
    }

    fn branding(name: &str) -> AppBrandingConfigResponse {
        AppBrandingConfigResponse {
            premium_product_name: name.to_owned(),
            premium_info_url: Some("https://example.com/gold".to_owned()),
            ..Default::default()
        }
    }

    #[test]
    fn section_renders_every_field_and_one_empty_price_row() {
        let markup = premium_billing_section(
            "/admin",
            "csrf",
            &branding("Gold"),
            &operator_billing(),
            PremiumMode::Mirror,
        )
        .into_string();
        assert!(markup.contains("action=\"/admin/instance-config?action=update_billing\""));
        assert!(markup.contains("data-admin-result-form=\"true\""));
        assert!(markup.contains("<option value=\"on\" selected>On</option>"));
        assert!(markup.contains("name=\"billing_automatic_tax\""));
        assert!(markup.contains("name=\"billing_tax_id_collection\""));
        assert!(markup.contains("name=\"billing_terms_consent_required\""));
        assert!(markup.contains("Customer portal"));
        assert!(markup.contains("name=\"billing_premium_product_name\""));
        assert!(markup.contains("value=\"Gold\""));
        assert!(markup.contains("value=\"https://example.com/gold\""));
        assert!(markup.contains("name=\"billing_enabled\""));
        assert!(markup.contains("type=\"password\" id=\"billing_stripe_secret_key\""));
        assert!(markup.contains("name=\"billing_clear_stripe_secret_key\""));
        assert!(markup.contains("name=\"billing_clear_stripe_webhook_secret\""));
        assert!(markup.contains("value=\"https://api.example.com/stripe/webhook\""));
        assert!(markup.contains("Billing active"));
        assert!(!markup.contains("Purchases are unavailable"));
        assert_eq!(markup.matches("name=\"billing_price_currency\"").count(), 2);
        assert_eq!(
            markup.matches("name=\"billing_price_gift_1_year\"").count(),
            2
        );
        assert!(markup.contains("value=\"price_1GbpG\""));
        assert!(markup.contains("GB=GBP\nIE=GBP"));
        assert!(markup.contains("monthly_GBP=price_1OldA\nmonthly_GBP=price_1OldB"));
        assert!(!markup.contains("Plutonium"));
        assert!(!markup.contains("sk_"));
    }

    #[test]
    fn unset_secrets_have_no_clear_checkbox() {
        let billing = InstanceBillingResponse::default();
        let markup = premium_billing_section(
            "/admin",
            "csrf",
            &branding("Premium"),
            &billing,
            PremiumMode::Everyone,
        )
        .into_string();
        assert!(!markup.contains("billing_clear_stripe_secret_key"));
        assert!(!markup.contains("billing_clear_stripe_webhook_secret"));
        assert_eq!(markup.matches("name=\"billing_price_currency\"").count(), 1);
        assert!(markup.contains("Switch the premium model to Mirror"));
        assert!(markup.contains("Catalog: environment"));
    }

    #[test]
    fn env_secrets_are_labelled_and_cannot_be_cleared() {
        let billing = InstanceBillingResponse {
            stripe_secret_key_set: true,
            stripe_webhook_secret_set: true,
            ..Default::default()
        };
        let markup = premium_billing_section(
            "/admin",
            "csrf",
            &branding("Premium"),
            &billing,
            PremiumMode::Mirror,
        )
        .into_string();
        assert!(markup.contains("Stripe secret key from environment"));
        assert!(markup.contains("Webhook secret from environment"));
        assert!(markup.contains("Set from the environment"));
        assert!(!markup.contains("billing_clear_stripe_secret_key"));
        assert!(!markup.contains("billing_clear_stripe_webhook_secret"));
    }

    #[test]
    fn tri_state_selects_reflect_the_stored_value() {
        let render = |stored| {
            tri_state_select(
                "billing_enabled",
                "Billing",
                "Use environment setting",
                stored,
                "",
            )
            .into_string()
        };
        assert!(
            render(None)
                .contains("<option value=\"default\" selected>Use environment setting</option>")
        );
        assert!(render(Some(true)).contains("<option value=\"on\" selected>On</option>"));
        assert!(render(Some(false)).contains("<option value=\"off\" selected>Off</option>"));
    }

    #[test]
    fn blockers_explain_why_billing_is_inactive() {
        let mut billing = InstanceBillingResponse::default();
        assert_eq!(
            billing_blockers(&billing, PremiumMode::Everyone),
            vec![
                "the premium model is Everyone, so there is no paid tier to sell",
                "billing is not enabled",
                "no Stripe secret key is set",
            ]
        );
        billing.effective_enabled = true;
        billing.stripe_secret_key_set = true;
        assert_eq!(
            billing_blockers(&billing, PremiumMode::Mirror),
            vec![
                "the environment price catalog has no currency with both a monthly and a yearly price ID"
            ]
        );
        billing.catalog_mode = BillingCatalogMode::Operator;
        billing.prices = Some(BTreeMap::from([(
            "GBP".to_owned(),
            BillingPriceSet {
                monthly: Some("price_1A".to_owned()),
                ..Default::default()
            },
        )]));
        assert_eq!(
            billing_blockers(&billing, PremiumMode::Mirror),
            vec!["no currency has both a monthly and a yearly price ID"]
        );
        assert!(billing_blockers(&operator_billing(), PremiumMode::Mirror).is_empty());
    }
}
