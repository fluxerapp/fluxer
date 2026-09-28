// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{InstanceConfigResponse, PremiumMode};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub const BILLING_MAX_CURRENCIES: usize = 64;
pub const BILLING_MAX_COUNTRY_CURRENCIES: usize = 300;
pub const BILLING_MAX_LEGACY_SLOTS: usize = 256;
pub const BILLING_MAX_LEGACY_PRICES_PER_SLOT: usize = 32;
pub const BILLING_PRICE_SLOTS: [&str; 4] = ["monthly", "yearly", "gift_1_month", "gift_1_year"];
pub const PREMIUM_PRODUCT_NAME_MAX_CHARS: usize = 40;
pub const TRI_STATE_DEFAULT: &str = "default";
pub const TRI_STATE_ON: &str = "on";
pub const TRI_STATE_OFF: &str = "off";

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BillingCatalogMode {
    #[default]
    Env,
    Operator,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct BillingPriceSet {
    pub monthly: Option<String>,
    pub yearly: Option<String>,
    pub gift_1_month: Option<String>,
    pub gift_1_year: Option<String>,
}

impl BillingPriceSet {
    pub fn has_recurring_pair(&self) -> bool {
        self.monthly.is_some() && self.yearly.is_some()
    }

    pub fn is_empty(&self) -> bool {
        self.monthly.is_none()
            && self.yearly.is_none()
            && self.gift_1_month.is_none()
            && self.gift_1_year.is_none()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceBillingResponse {
    pub enabled: Option<bool>,
    #[serde(default)]
    pub effective_enabled: bool,
    #[serde(default)]
    pub stripe_secret_key_set: bool,
    #[serde(default)]
    pub stripe_webhook_secret_set: bool,
    #[serde(default)]
    pub stripe_secret_key_stored: bool,
    #[serde(default)]
    pub stripe_webhook_secret_stored: bool,
    pub default_currency: Option<String>,
    pub prices: Option<BTreeMap<String, BillingPriceSet>>,
    pub country_currencies: Option<BTreeMap<String, String>>,
    pub legacy_prices: Option<BTreeMap<String, Vec<String>>>,
    #[serde(default)]
    pub billing_active: bool,
    #[serde(default)]
    pub stripe_serviceable: bool,
    #[serde(default)]
    pub catalog_mode: BillingCatalogMode,
    #[serde(default)]
    pub webhook_url: String,
    pub automatic_tax: Option<bool>,
    pub tax_id_collection: Option<bool>,
    pub terms_consent_required: Option<bool>,
    #[serde(default)]
    pub effective_automatic_tax: bool,
    #[serde(default)]
    pub effective_tax_id_collection: bool,
    #[serde(default)]
    pub effective_terms_consent_required: bool,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceBillingUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stripe_secret_key: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stripe_webhook_secret: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_currency: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prices: Option<Option<BTreeMap<String, BillingPriceSet>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub country_currencies: Option<Option<BTreeMap<String, String>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub legacy_prices: Option<Option<BTreeMap<String, Vec<String>>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub automatic_tax: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tax_id_collection: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub terms_consent_required: Option<Option<bool>>,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstancePremiumDiscovery {
    #[serde(default)]
    pub app_public: InstancePremiumDiscoveryAppPublic,
    #[serde(default)]
    pub features: InstancePremiumDiscoveryFeatures,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstancePremiumDiscoveryAppPublic {
    #[serde(default)]
    pub branding: InstancePremiumDiscoveryBranding,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstancePremiumDiscoveryBranding {
    pub premium_product_name: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstancePremiumDiscoveryFeatures {
    #[serde(default)]
    pub premium_enabled: bool,
}

impl InstancePremiumDiscovery {
    pub fn premium_product_name(&self) -> Option<&str> {
        self.app_public
            .branding
            .premium_product_name
            .as_deref()
            .map(str::trim)
            .filter(|name| !name.is_empty())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PremiumBranding {
    pub name: Option<String>,
    pub premium_enabled: bool,
}

impl PremiumBranding {
    pub fn from_discovery(discovery: &InstancePremiumDiscovery) -> Self {
        Self {
            name: discovery.premium_product_name().map(str::to_owned),
            premium_enabled: discovery.features.premium_enabled,
        }
    }

    pub fn from_instance_config(config: &InstanceConfigResponse) -> Self {
        Self::from_config_parts(
            config.self_hosted,
            &config.app_public.branding.premium_product_name,
            config.policy.premium_mode,
        )
    }

    fn from_config_parts(self_hosted: bool, name: &str, premium_mode: PremiumMode) -> Self {
        let name = name.trim();
        Self {
            name: (!name.is_empty()).then(|| name.to_owned()),
            premium_enabled: !self_hosted || matches!(premium_mode, PremiumMode::Mirror),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::generated::types as generated_types;
    use serde_json::json;

    #[test]
    fn billing_response_round_trips_through_the_generated_contract() {
        let value = json!({
            "enabled": true,
            "effective_enabled": true,
            "stripe_secret_key_set": true,
            "stripe_webhook_secret_set": false,
            "stripe_secret_key_stored": true,
            "stripe_webhook_secret_stored": false,
            "default_currency": "GBP",
            "prices": {
                "GBP": {
                    "monthly": "price_1Monthly",
                    "yearly": "price_1Yearly",
                    "gift_1_month": null,
                    "gift_1_year": null
                }
            },
            "country_currencies": {"GB": "GBP"},
            "legacy_prices": {"monthly_GBP": ["price_1Old"]},
            "billing_active": false,
            "stripe_serviceable": false,
            "catalog_mode": "operator",
            "webhook_url": "https://api.example.com/stripe/webhook",
            "automatic_tax": null,
            "tax_id_collection": false,
            "terms_consent_required": true,
            "effective_automatic_tax": false,
            "effective_tax_id_collection": false,
            "effective_terms_consent_required": true
        });
        let generated: generated_types::InstanceBillingResponse =
            serde_json::from_value(value.clone()).expect("generated billing response");
        let ours: InstanceBillingResponse =
            serde_json::from_value(value.clone()).expect("hand-written billing response");
        assert_eq!(ours.catalog_mode, BillingCatalogMode::Operator);
        assert!(ours.stripe_secret_key_stored);
        assert_eq!(ours.automatic_tax, None);
        assert_eq!(ours.tax_id_collection, Some(false));
        assert!(ours.effective_terms_consent_required);
        assert!(ours.prices.as_ref().expect("prices")["GBP"].has_recurring_pair());
        assert_eq!(serde_json::to_value(&ours).expect("serializable"), value);
        assert_eq!(
            serde_json::to_value(generated).expect("serializable generated"),
            value
        );
    }

    #[test]
    fn default_billing_response_matches_the_generated_contract() {
        let value = serde_json::to_value(InstanceBillingResponse::default()).expect("serializable");
        serde_json::from_value::<generated_types::InstanceBillingResponse>(value.clone())
            .expect("generated billing response");
        assert_eq!(value["catalog_mode"], json!("env"));
        assert_eq!(value["prices"], json!(null));
    }

    #[test]
    fn billing_update_preserves_explicit_nulls_and_omits_untouched_fields() {
        let mut prices = BTreeMap::new();
        prices.insert(
            "SEK".to_owned(),
            BillingPriceSet {
                monthly: Some("price_1Monthly".to_owned()),
                yearly: Some("price_1Yearly".to_owned()),
                ..Default::default()
            },
        );
        let update = InstanceBillingUpdateRequest {
            enabled: Some(None),
            stripe_secret_key: Some(None),
            default_currency: Some(None),
            prices: Some(Some(prices)),
            country_currencies: Some(None),
            legacy_prices: Some(Some(BTreeMap::new())),
            automatic_tax: Some(None),
            tax_id_collection: Some(Some(true)),
            terms_consent_required: Some(Some(false)),
            ..Default::default()
        };
        let value = serde_json::to_value(update).expect("serializable update");
        serde_json::from_value::<generated_types::InstanceBillingUpdateRequest>(value.clone())
            .expect("generated update contract");
        assert_eq!(
            value,
            json!({
                "enabled": null,
                "stripe_secret_key": null,
                "default_currency": null,
                "prices": {
                    "SEK": {
                        "monthly": "price_1Monthly",
                        "yearly": "price_1Yearly",
                        "gift_1_month": null,
                        "gift_1_year": null
                    }
                },
                "country_currencies": null,
                "legacy_prices": {},
                "automatic_tax": null,
                "tax_id_collection": true,
                "terms_consent_required": false
            })
        );
        assert_eq!(
            serde_json::to_value(InstanceBillingUpdateRequest::default())
                .expect("serializable update"),
            json!({})
        );
    }

    #[test]
    fn premium_discovery_reads_the_name_and_feature_flag() {
        let discovery: InstancePremiumDiscovery = serde_json::from_value(json!({
            "app_public": {"branding": {"product_name": "Example", "premium_product_name": " Gold "}},
            "features": {"premium_enabled": true, "stripe_enabled": false}
        }))
        .expect("discovery");
        assert_eq!(discovery.premium_product_name(), Some("Gold"));
        assert!(discovery.features.premium_enabled);
        let empty: InstancePremiumDiscovery =
            serde_json::from_value(json!({})).expect("empty discovery");
        assert_eq!(empty.premium_product_name(), None);
        assert!(!empty.features.premium_enabled);
        assert_eq!(
            PremiumBranding::from_discovery(&discovery),
            PremiumBranding {
                name: Some("Gold".to_owned()),
                premium_enabled: true
            }
        );
    }

    #[test]
    fn premium_branding_from_instance_config_matches_discovery_rules() {
        assert_eq!(
            PremiumBranding::from_config_parts(true, " Gold ", PremiumMode::Everyone),
            PremiumBranding {
                name: Some("Gold".to_owned()),
                premium_enabled: false
            }
        );
        assert!(
            PremiumBranding::from_config_parts(true, "Gold", PremiumMode::Mirror).premium_enabled
        );
        assert_eq!(
            PremiumBranding::from_config_parts(false, "  ", PremiumMode::Everyone),
            PremiumBranding {
                name: None,
                premium_enabled: true
            }
        );
    }
}
