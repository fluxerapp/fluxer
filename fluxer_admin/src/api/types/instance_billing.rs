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
    pub billing_emails: Option<bool>,
    #[serde(default)]
    pub effective_automatic_tax: bool,
    #[serde(default)]
    pub effective_tax_id_collection: bool,
    #[serde(default)]
    pub effective_terms_consent_required: bool,
    #[serde(default)]
    pub effective_billing_emails: bool,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub billing_emails: Option<Option<bool>>,
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
    pub product_name: Option<String>,
    pub icon_url: Option<String>,
    pub symbol_url: Option<String>,
    pub favicon_url: Option<String>,
    pub favicon_dark_url: Option<String>,
    pub premium_product_name: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstancePremiumDiscoveryFeatures {
    #[serde(default)]
    pub premium_enabled: bool,
}

impl InstancePremiumDiscovery {
    pub fn premium_product_name(&self) -> Option<&str> {
        non_blank(self.app_public.branding.premium_product_name.as_deref())
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PremiumBranding {
    pub name: Option<String>,
    pub premium_enabled: bool,
    pub product_name: Option<String>,
    pub favicon_url: Option<String>,
    pub favicon_dark_url: Option<String>,
    pub neutral_default_avatars: bool,
}

impl PremiumBranding {
    pub fn from_discovery(discovery: &InstancePremiumDiscovery) -> Self {
        let branding = &discovery.app_public.branding;
        Self {
            name: discovery.premium_product_name().map(str::to_owned),
            premium_enabled: discovery.features.premium_enabled,
            product_name: non_blank(branding.product_name.as_deref()).map(str::to_owned),
            favicon_url: favicon_url(
                branding.favicon_url.as_deref(),
                branding.icon_url.as_deref(),
            ),
            favicon_dark_url: non_blank(branding.favicon_dark_url.as_deref()).map(str::to_owned),
            neutral_default_avatars: has_own_mark(
                branding.icon_url.as_deref(),
                branding.symbol_url.as_deref(),
            ),
        }
    }

    pub fn from_instance_config(config: &InstanceConfigResponse) -> Self {
        let branding = &config.app_public.branding;
        Self {
            product_name: non_blank(Some(&branding.product_name)).map(str::to_owned),
            favicon_url: favicon_url(
                branding.favicon_url.as_deref(),
                branding.icon_url.as_deref(),
            ),
            favicon_dark_url: non_blank(branding.favicon_dark_url.as_deref()).map(str::to_owned),
            neutral_default_avatars: has_own_mark(
                branding.icon_url.as_deref(),
                branding.symbol_url.as_deref(),
            ),
            ..Self::from_config_parts(
                config.self_hosted,
                &branding.premium_product_name,
                config.policy.premium_mode,
            )
        }
    }

    fn from_config_parts(self_hosted: bool, name: &str, premium_mode: PremiumMode) -> Self {
        Self {
            name: non_blank(Some(name)).map(str::to_owned),
            premium_enabled: !self_hosted || matches!(premium_mode, PremiumMode::Mirror),
            ..Self::default()
        }
    }
}

fn non_blank(value: Option<&str>) -> Option<&str> {
    value.map(str::trim).filter(|value| !value.is_empty())
}

fn has_own_mark(icon_url: Option<&str>, symbol_url: Option<&str>) -> bool {
    non_blank(icon_url).is_some() || non_blank(symbol_url).is_some()
}

fn favicon_url(favicon_url: Option<&str>, icon_url: Option<&str>) -> Option<String> {
    non_blank(favicon_url)
        .or_else(|| non_blank(icon_url))
        .map(str::to_owned)
}
