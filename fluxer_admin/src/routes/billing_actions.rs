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
            match validation_message(&error) {
                Some(message) => FlashData::error(format!(
                    "Failed to update premium and billing settings: {message}"
                )),
                None => FlashData::error("Failed to update premium and billing settings"),
            }
        }
    }
}

fn validation_message(error: &ApiError) -> Option<String> {
    let ApiError::Http {
        status: 400,
        message,
    } = error
    else {
        return None;
    };
    let body: serde_json::Value = serde_json::from_str(message).ok()?;
    let first = body["errors"].as_array().and_then(|errors| errors.first());
    let detail = first.and_then(|error| {
        let message = error["message"].as_str()?;
        Some(
            match error["path"].as_str().filter(|path| !path.is_empty()) {
                Some(path) => format!("{path}: {message}"),
                None => message.to_owned(),
            },
        )
    });
    detail.or_else(|| body["message"].as_str().map(str::to_owned))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::generated::types as generated_types;
    use serde_json::json;

    fn full_form(extra: &str) -> MultiValueForm {
        let base = "billing_premium_product_name=%20Gold%20\
            &billing_premium_info_url=https%3A%2F%2Fexample.com%2Fgold\
            &billing_enabled=on\
            &billing_automatic_tax=default&billing_tax_id_collection=on&billing_terms_consent_required=off\
            &billing_stripe_secret_key=\
            &billing_stripe_webhook_secret=whsec_new\
            &billing_default_currency=gbp\
            &billing_price_currency=gbp&billing_price_monthly=price_1GbpM&billing_price_yearly=price_1GbpY\
            &billing_price_gift_1_month=&billing_price_gift_1_year=price_1GbpG\
            &billing_price_currency=SEK&billing_price_monthly=price_1SekM&billing_price_yearly=price_1SekY\
            &billing_price_gift_1_month=&billing_price_gift_1_year=\
            &billing_price_currency=&billing_price_monthly=&billing_price_yearly=\
            &billing_price_gift_1_month=&billing_price_gift_1_year=\
            &billing_country_currencies=se%3Dsek%0D%0AGB%20%3D%20GBP%0D%0A\
            &billing_legacy_prices=monthly_GBP%3Dprice_1OldA%0Amonthly_gbp%3Dprice_1OldB%2Cprice_1OldA%0Ayearly_SEK%3Dprice_1OldC";
        MultiValueForm::parse(format!("{base}{extra}").as_bytes())
    }

    #[test]
    fn full_billing_form_builds_the_expected_patch() {
        let update = build_billing_update(&full_form("")).expect("valid form");
        let value = serde_json::to_value(&update).expect("serializable");
        serde_json::from_value::<generated_types::InstanceConfigUpdateRequest>(value.clone())
            .expect("generated update contract");
        assert_eq!(
            value,
            json!({
                "app_public": {
                    "branding": {
                        "premium_product_name": "Gold",
                        "premium_info_url": "https://example.com/gold"
                    }
                },
                "billing": {
                    "enabled": true,
                    "stripe_webhook_secret": "whsec_new",
                    "default_currency": "GBP",
                    "prices": {
                        "GBP": {
                            "monthly": "price_1GbpM",
                            "yearly": "price_1GbpY",
                            "gift_1_month": null,
                            "gift_1_year": "price_1GbpG"
                        },
                        "SEK": {
                            "monthly": "price_1SekM",
                            "yearly": "price_1SekY",
                            "gift_1_month": null,
                            "gift_1_year": null
                        }
                    },
                    "country_currencies": {"GB": "GBP", "SE": "SEK"},
                    "legacy_prices": {
                        "monthly_GBP": ["price_1OldA", "price_1OldB"],
                        "yearly_SEK": ["price_1OldC"]
                    },
                    "automatic_tax": null,
                    "tax_id_collection": true,
                    "terms_consent_required": false
                }
            })
        );
    }

    #[test]
    fn blank_fields_clear_and_the_default_choice_sends_null() {
        let form = MultiValueForm::parse(
            b"billing_premium_product_name=&billing_premium_info_url=&billing_enabled=default\
              &billing_stripe_secret_key=&billing_clear_stripe_secret_key=true\
              &billing_stripe_webhook_secret=\
              &billing_default_currency=\
              &billing_price_currency=&billing_price_monthly=price_1Ignored\
              &billing_country_currencies=&billing_legacy_prices=",
        );
        let value = serde_json::to_value(build_billing_update(&form).expect("valid form")).unwrap();
        assert_eq!(
            value,
            json!({
                "app_public": {
                    "branding": {"premium_product_name": null, "premium_info_url": null}
                },
                "billing": {
                    "enabled": null,
                    "stripe_secret_key": null,
                    "default_currency": null,
                    "prices": null,
                    "country_currencies": null,
                    "legacy_prices": null
                }
            })
        );
    }

    #[test]
    fn a_new_secret_wins_over_the_clear_checkbox() {
        let form = MultiValueForm::parse(
            b"billing_stripe_secret_key=%20sk_live_x%20&billing_clear_stripe_secret_key=true",
        );
        let billing = build_billing_update(&form)
            .expect("valid form")
            .billing
            .expect("billing");
        assert_eq!(
            billing.stripe_secret_key,
            Some(Some("sk_live_x".to_owned()))
        );
        assert_eq!(billing.stripe_webhook_secret, None);
    }

    #[test]
    fn absent_form_keys_leave_their_fields_untouched() {
        let update = build_billing_update(&MultiValueForm::parse(b"billing_enabled=off"))
            .expect("valid form");
        assert!(update.app_public.is_none());
        assert_eq!(
            serde_json::to_value(update.billing).unwrap(),
            json!({"enabled": false})
        );
        let untouched = build_billing_update(&MultiValueForm::parse(b"")).expect("valid form");
        assert_eq!(serde_json::to_value(untouched.billing).unwrap(), json!({}));
    }

    #[test]
    fn invalid_input_is_rejected_with_a_message() {
        let cases: &[(&str, &str)] = &[
            (
                "billing_premium_info_url=ftp%3A%2F%2Fexample.com",
                "http or https",
            ),
            ("billing_premium_info_url=example.com", "http or https"),
            ("billing_default_currency=GB", "Invalid currency"),
            ("billing_enabled=true", "Invalid choice"),
            ("billing_automatic_tax=maybe", "Invalid choice"),
            (
                "billing_price_currency=GBPX&billing_price_monthly=price_1A",
                "Invalid currency",
            ),
            (
                "billing_price_currency=GBP&billing_price_monthly=prod_1A",
                "Invalid Stripe price ID",
            ),
            (
                "billing_price_currency=GBP&billing_price_monthly=price_1-A",
                "Invalid Stripe price ID",
            ),
            ("billing_price_currency=GBP", "needs at least one price ID"),
            (
                "billing_price_currency=GBP&billing_price_monthly=price_1A&billing_price_currency=gbp&billing_price_monthly=price_1B",
                "more than once",
            ),
            ("billing_country_currencies=SWE%3DSEK", "Invalid country"),
            ("billing_country_currencies=SE", "KEY=VALUE"),
            (
                "billing_country_currencies=SE%3DSEK%0ASE%3DEUR",
                "mapped more than once",
            ),
            (
                "billing_legacy_prices=weekly_GBP%3Dprice_1A",
                "Invalid legacy price slot",
            ),
            (
                "billing_legacy_prices=monthly_GBP%3D",
                "needs at least one price ID",
            ),
            (
                "billing_default_currency=EUR&billing_price_currency=GBP&billing_price_monthly=price_1A",
                "Default currency EUR has no row",
            ),
            (
                "billing_country_currencies=SE%3DSEK&billing_price_currency=GBP&billing_price_monthly=price_1A",
                "SE maps to SEK",
            ),
        ];
        let long_name = format!("billing_premium_product_name={}", "A".repeat(41));
        let emoji_name = format!(
            "billing_premium_product_name=Gold{}",
            "%F0%9F%92%8E".repeat(20)
        );
        let long_case = [
            (long_name.as_str(), "at most 40"),
            (emoji_name.as_str(), "at most 40"),
        ];
        for (body, expected) in long_case.iter().chain(cases.iter()) {
            let error =
                build_billing_update(&MultiValueForm::parse(body.as_bytes())).expect_err(body);
            assert!(error.contains(expected), "{body}: {error}");
        }
    }

    #[test]
    fn premium_name_limit_counts_utf16_units() {
        let name = format!("{}{}", "A".repeat(39), "\u{1F48E}");
        assert_eq!(name.chars().count(), 40);
        assert!(parse_premium_product_name(Some(name)).is_err());
        let fits = format!("{}{}", "A".repeat(38), "\u{E9}\u{E9}");
        assert_eq!(
            parse_premium_product_name(Some(fits.clone())),
            Ok(Some(fits))
        );
    }

    #[test]
    fn country_currencies_are_not_cross_checked_without_a_price_table() {
        let form = MultiValueForm::parse(b"billing_country_currencies=SE%3DSEK");
        let billing = build_billing_update(&form).unwrap().billing.unwrap();
        assert_eq!(
            billing.country_currencies,
            Some(Some(BTreeMap::from([("SE".to_owned(), "SEK".to_owned())])))
        );
    }

    #[test]
    fn validation_errors_surface_the_first_api_message() {
        let error = ApiError::Http {
            status: 400,
            message: json!({
                "code": "VALIDATION_ERROR",
                "message": "Validation failed",
                "errors": [{"path": "billing.enabled", "code": "X", "message": "Switch the premium model to mirror first"}]
            })
            .to_string(),
        };
        assert_eq!(
            validation_message(&error).as_deref(),
            Some("billing.enabled: Switch the premium model to mirror first")
        );
        let server_error = ApiError::Http {
            status: 500,
            message: "{}".to_owned(),
        };
        assert_eq!(validation_message(&server_error), None);
    }
}
