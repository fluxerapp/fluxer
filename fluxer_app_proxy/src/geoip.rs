// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
use fluxer_common::geoip::{GeoipConfig, GeoipLookup, GeoipResolver};
use serde::{Deserialize, Serialize};
use std::sync::LazyLock;

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct GeoEntry {
    country_code: String,
    region_code: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct AgeGeos {
    age_restricted_geos: Vec<GeoEntry>,
    age_blocked_geos: Vec<GeoEntry>,
}

static AGE_GEOS: LazyLock<AgeGeos> = LazyLock::new(|| {
    serde_json::from_str(include_str!("../../packages/constants/src/AgeGeos.json")).expect(
        "bundled packages/constants/src/AgeGeos.json must contain a valid age geography policy",
    )
});

pub fn resolver_from_app_config(config: &AppProxyConfig) -> GeoipResolver {
    GeoipResolver::from_config(&GeoipConfig {
        geoip_source: config.geoip_source.clone(),
        geoip_s3_config: config.geoip_s3_config.clone(),
        trust_client_ip_header: config.trust_client_ip_header,
        client_ip_header_name: config.client_ip_header_name.clone(),
    })
}

pub fn build_geoip_response(lookup: GeoipLookup) -> serde_json::Value {
    serde_json::json!({
        "countryCode": lookup.country_code,
        "regionCode": lookup.region_code,
        "latitude": lookup.latitude.map(|v| v.to_string()),
        "longitude": lookup.longitude.map(|v| v.to_string()),
        "ageRestrictedGeos": &AGE_GEOS.age_restricted_geos,
        "ageBlockedGeos": &AGE_GEOS.age_blocked_geos,
    })
}
