// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        client::ApiError,
        types::{
            AppBrandingConfigUpdateRequest, AppPublicConfigUpdateRequest,
            BILLING_MAX_COUNTRY_CURRENCIES, BILLING_MAX_CURRENCIES,
            BILLING_MAX_LEGACY_PRICES_PER_SLOT, BILLING_MAX_LEGACY_SLOTS, BILLING_PRICE_SLOTS,
            BillingPriceSet, InstanceBillingUpdateRequest, InstanceConfigUpdateRequest,
            PREMIUM_PRODUCT_NAME_MAX_CHARS, TRI_STATE_DEFAULT, TRI_STATE_OFF, TRI_STATE_ON,
        },
    },
    middleware::flash::FlashData,
    utils::forms::MultiValueForm,
};
use std::collections::BTreeMap;

const PRICE_ID_MAX_CHARS: usize = 255;
const INFO_URL_MAX_CHARS: usize = 2048;

pub(super) fn build_billing_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, String> {
    let premium_product_name = if form.contains_key("billing_premium_product_name") {
        Some(parse_premium_product_name(
            form.clean("billing_premium_product_name"),
        )?)
    } else {
        None
    };
    let premium_info_url = if form.contains_key("billing_premium_info_url") {
        Some(parse_info_url(form.clean("billing_premium_info_url"))?)
    } else {
        None
    };
    let branding = (premium_product_name.is_some() || premium_info_url.is_some()).then(|| {
        AppBrandingConfigUpdateRequest {
            premium_product_name,
            premium_info_url,
            ..Default::default()
        }
    });
    let prices = if form.contains_key("billing_price_currency") {
        Some(parse_price_rows(form)?)
    } else {
        None
    };
    let default_currency = if form.contains_key("billing_default_currency") {
        Some(
            form.clean("billing_default_currency")
                .map(|value| parse_currency(&value))
                .transpose()?,
        )
    } else {
        None
    };
    let country_currencies = if form.contains_key("billing_country_currencies") {
        Some(parse_country_currencies(
            form.first("billing_country_currencies").unwrap_or(""),
        )?)
    } else {
        None
    };
    let legacy_prices = if form.contains_key("billing_legacy_prices") {
        Some(parse_legacy_prices(
            form.first("billing_legacy_prices").unwrap_or(""),
        )?)
    } else {
        None
    };
    if let Some(Some(prices)) = &prices {
        if let Some(Some(currency)) = &default_currency
            && !prices.contains_key(currency)
        {
            return Err(format!(
                "Default currency {currency} has no row in the price table"
            ));
        }
        if let Some(Some(countries)) = &country_currencies
            && let Some((country, currency)) = countries
                .iter()
                .find(|(_, currency)| !prices.contains_key(*currency))
        {
            return Err(format!(
                "{country} maps to {currency}, which has no row in the price table"
            ));
        }
    }
    Ok(InstanceConfigUpdateRequest {
        app_public: branding.map(|branding| AppPublicConfigUpdateRequest {
            branding: Some(branding),
            ..Default::default()
        }),
        billing: Some(InstanceBillingUpdateRequest {
            enabled: parse_tri_state(form, "billing_enabled")?,
            stripe_secret_key: secret_update(
                form,
                "billing_stripe_secret_key",
                "billing_clear_stripe_secret_key",
            ),
            stripe_webhook_secret: secret_update(
                form,
                "billing_stripe_webhook_secret",
                "billing_clear_stripe_webhook_secret",
            ),
            default_currency,
            prices,
            country_currencies,
            legacy_prices,
            automatic_tax: parse_tri_state(form, "billing_automatic_tax")?,
            tax_id_collection: parse_tri_state(form, "billing_tax_id_collection")?,
            terms_consent_required: parse_tri_state(form, "billing_terms_consent_required")?,
            billing_emails: parse_tri_state(form, "billing_billing_emails")?,
        }),
        ..Default::default()
    })
}

fn parse_tri_state(form: &MultiValueForm, key: &str) -> Result<Option<Option<bool>>, String> {
    if !form.contains_key(key) {
        return Ok(None);
    }
    match form.first(key).map(str::trim).unwrap_or("") {
        TRI_STATE_DEFAULT => Ok(Some(None)),
        TRI_STATE_ON => Ok(Some(Some(true))),
        TRI_STATE_OFF => Ok(Some(Some(false))),
        other => Err(format!("Invalid choice \"{other}\" for {key}")),
    }
}

pub(super) fn billing_result<T>(result: Result<T, ApiError>) -> FlashData {
    match result {
        Ok(_) => FlashData::success("Premium and billing settings updated"),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: update billing config");
            match error.validation_message() {
                Some(message) => FlashData::error(format!(
                    "Failed to update premium and billing settings: {message}"
                )),
                None => FlashData::error("Failed to update premium and billing settings"),
            }
        }
    }
}

fn secret_update(form: &MultiValueForm, key: &str, clear_key: &str) -> Option<Option<String>> {
    match form.clean(key) {
        Some(secret) => Some(Some(secret)),
        None if form.bool_value(clear_key) => Some(None),
        None => None,
    }
}

fn parse_premium_product_name(value: Option<String>) -> Result<Option<String>, String> {
    match value {
        Some(name) if name.encode_utf16().count() > PREMIUM_PRODUCT_NAME_MAX_CHARS => Err(format!(
            "Premium name must be at most {PREMIUM_PRODUCT_NAME_MAX_CHARS} characters"
        )),
        other => Ok(other),
    }
}

fn parse_info_url(value: Option<String>) -> Result<Option<String>, String> {
    let Some(value) = value else {
        return Ok(None);
    };
    let valid = value.chars().count() <= INFO_URL_MAX_CHARS
        && url::Url::parse(&value).is_ok_and(|url| {
            matches!(url.scheme(), "http" | "https")
                && url.host_str().is_some_and(|h| !h.is_empty())
        });
    if valid {
        Ok(Some(value))
    } else {
        Err("Premium info URL must be an absolute http or https URL".to_owned())
    }
}

fn parse_currency(value: &str) -> Result<String, String> {
    let currency = value.trim().to_ascii_uppercase();
    if currency.len() == 3 && currency.bytes().all(|byte| byte.is_ascii_uppercase()) {
        Ok(currency)
    } else {
        Err(format!(
            "Invalid currency \"{}\": use a 3-letter ISO 4217 code such as GBP",
            value.trim()
        ))
    }
}

fn parse_country(value: &str) -> Result<String, String> {
    let country = value.trim().to_ascii_uppercase();
    if country.len() == 2 && country.bytes().all(|byte| byte.is_ascii_uppercase()) {
        Ok(country)
    } else {
        Err(format!(
            "Invalid country \"{}\": use a 2-letter ISO 3166 code such as SE",
            value.trim()
        ))
    }
}

fn parse_price_id(value: &str) -> Result<String, String> {
    let id = value.trim();
    let valid = id.len() <= PRICE_ID_MAX_CHARS
        && id.strip_prefix("price_").is_some_and(|rest| {
            !rest.is_empty() && rest.bytes().all(|b| b.is_ascii_alphanumeric())
        });
    if valid {
        Ok(id.to_owned())
    } else {
        Err(format!(
            "Invalid Stripe price ID \"{id}\": it must look like price_1AbC"
        ))
    }
}

fn parse_optional_price_id(value: Option<&String>) -> Result<Option<String>, String> {
    match value
        .map(|value| value.trim())
        .filter(|value| !value.is_empty())
    {
        Some(id) => parse_price_id(id).map(Some),
        None => Ok(None),
    }
}

fn parse_price_rows(
    form: &MultiValueForm,
) -> Result<Option<BTreeMap<String, BillingPriceSet>>, String> {
    let currencies = form.values("billing_price_currency");
    let column = |key: &str, index: usize| form.values(key).get(index);
    let mut prices = BTreeMap::new();
    for (index, currency) in currencies.iter().enumerate() {
        if currency.trim().is_empty() {
            continue;
        }
        let currency = parse_currency(currency)?;
        let set = BillingPriceSet {
            monthly: parse_optional_price_id(column("billing_price_monthly", index))?,
            yearly: parse_optional_price_id(column("billing_price_yearly", index))?,
            gift_1_month: parse_optional_price_id(column("billing_price_gift_1_month", index))?,
            gift_1_year: parse_optional_price_id(column("billing_price_gift_1_year", index))?,
        };
        if set.is_empty() {
            return Err(format!("{currency} needs at least one price ID"));
        }
        if prices.insert(currency.clone(), set).is_some() {
            return Err(format!(
                "{currency} appears more than once in the price table"
            ));
        }
    }
    if prices.len() > BILLING_MAX_CURRENCIES {
        return Err(format!(
            "The price table holds at most {BILLING_MAX_CURRENCIES} currencies"
        ));
    }
    Ok((!prices.is_empty()).then_some(prices))
}

fn key_value_lines(value: &str) -> impl Iterator<Item = Result<(&str, &str), String>> {
    value
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split_once('=')
                .map(|(key, value)| (key.trim(), value.trim()))
                .ok_or_else(|| format!("Line \"{line}\" must use the form KEY=VALUE"))
        })
}

fn parse_country_currencies(value: &str) -> Result<Option<BTreeMap<String, String>>, String> {
    let mut countries = BTreeMap::new();
    for line in key_value_lines(value) {
        let (country, currency) = line?;
        let country = parse_country(country)?;
        let currency = parse_currency(currency)?;
        if countries.insert(country.clone(), currency).is_some() {
            return Err(format!("{country} is mapped more than once"));
        }
    }
    if countries.len() > BILLING_MAX_COUNTRY_CURRENCIES {
        return Err(format!(
            "At most {BILLING_MAX_COUNTRY_CURRENCIES} country mappings are allowed"
        ));
    }
    Ok((!countries.is_empty()).then_some(countries))
}

fn parse_legacy_slot(value: &str) -> Result<String, String> {
    let invalid = || {
        format!(
            "Invalid legacy price slot \"{value}\": use monthly, yearly, gift_1_month or gift_1_year followed by _ and a currency, such as monthly_GBP"
        )
    };
    let (slot, currency) = value.rsplit_once('_').ok_or_else(invalid)?;
    let slot = slot.to_ascii_lowercase();
    if !BILLING_PRICE_SLOTS.contains(&slot.as_str()) {
        return Err(invalid());
    }
    let currency = parse_currency(currency).map_err(|_| invalid())?;
    Ok(format!("{slot}_{currency}"))
}

fn parse_legacy_prices(value: &str) -> Result<Option<BTreeMap<String, Vec<String>>>, String> {
    let mut legacy: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for line in key_value_lines(value) {
        let (slot, ids) = line?;
        let slot = parse_legacy_slot(slot)?;
        let entry = legacy.entry(slot.clone()).or_default();
        for id in ids.split(',').map(str::trim).filter(|id| !id.is_empty()) {
            let id = parse_price_id(id)?;
            if !entry.contains(&id) {
                entry.push(id);
            }
        }
        if entry.is_empty() {
            return Err(format!("{slot} needs at least one price ID"));
        }
        if entry.len() > BILLING_MAX_LEGACY_PRICES_PER_SLOT {
            return Err(format!(
                "{slot} holds at most {BILLING_MAX_LEGACY_PRICES_PER_SLOT} legacy price IDs"
            ));
        }
    }
    if legacy.len() > BILLING_MAX_LEGACY_SLOTS {
        return Err(format!(
            "At most {BILLING_MAX_LEGACY_SLOTS} legacy price slots are allowed"
        ));
    }
    Ok((!legacy.is_empty()).then_some(legacy))
}
