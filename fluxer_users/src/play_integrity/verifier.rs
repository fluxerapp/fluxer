// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{DeviceIntegrity, PlayIntegrityConfig, PlayIntegrityRejection, URL_SAFE_ANY_PADDING};
use crate::app_attest::AgeFloor;
use crate::app_attest::verifier::{constant_time_eq, sha256};
use base64::Engine;
use base64::engine::general_purpose::URL_SAFE_NO_PAD;
use serde::Deserialize;
use serde_json::Value;

const PLAY_RECOGNIZED: &str = "PLAY_RECOGNIZED";
const LICENSED: &str = "LICENSED";
const MEETS_DEVICE_INTEGRITY: &str = "MEETS_DEVICE_INTEGRITY";
const MEETS_STRONG_INTEGRITY: &str = "MEETS_STRONG_INTEGRITY";
const MAX_TOKEN_AGE_MS: i64 = 5 * 60 * 1000;
const MAX_CLOCK_SKEW_MS: i64 = 60 * 1000;
const ADULT_AGE: u8 = 18;
const MINIMUM_AGE: u8 = 13;
const MIN_AGE_UPPER: u8 = 2;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TokenPayload {
    request_details: RequestDetails,
    app_integrity: AppIntegrity,
    device_integrity: DeviceIntegrityVerdict,
    account_details: AccountDetails,
    #[serde(default)]
    testing_details: Option<TestingDetails>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RequestDetails {
    request_package_name: String,
    #[serde(default)]
    request_hash: Option<String>,
    timestamp_millis: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AppIntegrity {
    app_recognition_verdict: String,
    #[serde(default)]
    package_name: Option<String>,
    #[serde(default)]
    certificate_sha256_digest: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DeviceIntegrityVerdict {
    #[serde(default)]
    device_recognition_verdict: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AccountDetails {
    app_licensing_verdict: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TestingDetails {
    #[serde(default)]
    is_testing_response: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AgeSignalsClientData {
    pub challenge: String,
    pub age_lower: u8,
    pub age_upper: Option<u8>,
    pub age_range_source: AgeRangeSource,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AgeRangeSource {
    TierA,
    TierB,
    TierC,
    TierD,
}

impl AgeRangeSource {
    pub fn is_verified(self) -> bool {
        matches!(self, Self::TierC | Self::TierD)
    }
}

impl AgeSignalsClientData {
    pub fn age_floor(&self) -> Option<AgeFloor> {
        if self.age_lower > ADULT_AGE {
            return None;
        }
        if let Some(upper) = self.age_upper
            && (!(MIN_AGE_UPPER..=ADULT_AGE).contains(&upper)
                || upper < self.age_lower
                || (self.age_lower == ADULT_AGE && upper != ADULT_AGE))
        {
            return None;
        }
        Some(if self.age_lower >= ADULT_AGE {
            AgeFloor::Adult
        } else if self.age_lower >= MINIMUM_AGE {
            AgeFloor::Minor
        } else {
            AgeFloor::UnderMinimum
        })
    }
}

pub fn request_hash(client_data: &[u8]) -> String {
    URL_SAFE_NO_PAD.encode(sha256(client_data))
}

pub fn verify_payload(
    payload: Value,
    package_name: &str,
    expected_request_hash: &str,
    config: &PlayIntegrityConfig,
    now_ms: i64,
) -> Result<(), PlayIntegrityRejection> {
    let payload: TokenPayload =
        serde_json::from_value(payload).map_err(|_| PlayIntegrityRejection::InvalidToken)?;
    if payload
        .testing_details
        .is_some_and(|details| details.is_testing_response)
    {
        return Err(PlayIntegrityRejection::InvalidToken);
    }
    let request = &payload.request_details;
    let request_hash = request.request_hash.as_deref().unwrap_or_default();
    if request.request_package_name != package_name
        || !constant_time_eq(request_hash.as_bytes(), expected_request_hash.as_bytes())
    {
        return Err(PlayIntegrityRejection::InvalidToken);
    }
    let timestamp_ms = request
        .timestamp_millis
        .parse::<i64>()
        .map_err(|_| PlayIntegrityRejection::InvalidToken)?;
    if now_ms.saturating_sub(timestamp_ms) > MAX_TOKEN_AGE_MS
        || timestamp_ms.saturating_sub(now_ms) > MAX_CLOCK_SKEW_MS
    {
        return Err(PlayIntegrityRejection::InvalidToken);
    }
    let app = &payload.app_integrity;
    if app.app_recognition_verdict != PLAY_RECOGNIZED
        || app.package_name.as_deref() != Some(package_name)
        || !app
            .certificate_sha256_digest
            .iter()
            .filter_map(|digest| URL_SAFE_ANY_PADDING.decode(digest).ok())
            .any(|digest| {
                config
                    .cert_digests
                    .iter()
                    .any(|allowed| constant_time_eq(allowed, &digest))
            })
        || payload.account_details.app_licensing_verdict != LICENSED
    {
        return Err(PlayIntegrityRejection::AppNotRecognized);
    }
    let labels = &payload.device_integrity.device_recognition_verdict;
    let has = |label: &str| labels.iter().any(|candidate| candidate == label);
    let trusted = match config.device_integrity {
        DeviceIntegrity::Device => has(MEETS_DEVICE_INTEGRITY) || has(MEETS_STRONG_INTEGRITY),
        DeviceIntegrity::Strong => has(MEETS_STRONG_INTEGRITY),
    };
    if !trusted {
        return Err(PlayIntegrityRejection::DeviceNotTrusted);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    const PACKAGE: &str = "com.fluxer";
    const NOW_MS: i64 = 1_800_000_000_000;
    const CLIENT_DATA: &[u8] =
        br#"{"challenge":"c","age_lower":18,"age_upper":null,"age_range_source":"tier_c"}"#;

    fn cert() -> [u8; 32] {
        sha256(b"fluxer signing certificate")
    }

    fn config() -> PlayIntegrityConfig {
        PlayIntegrityConfig {
            package_names: vec![PACKAGE.to_owned()],
            cert_digests: vec![cert()],
            ..PlayIntegrityConfig::default()
        }
    }

    fn passing() -> Value {
        json!({
            "requestDetails": {
                "requestPackageName": PACKAGE,
                "requestHash": request_hash(CLIENT_DATA),
                "timestampMillis": (NOW_MS - 2_000).to_string()
            },
            "appIntegrity": {
                "appRecognitionVerdict": "PLAY_RECOGNIZED",
                "packageName": PACKAGE,
                "certificateSha256Digest": [URL_SAFE_NO_PAD.encode(cert())],
                "versionCode": "42"
            },
            "deviceIntegrity": {"deviceRecognitionVerdict": ["MEETS_DEVICE_INTEGRITY"]},
            "accountDetails": {"appLicensingVerdict": "LICENSED"},
            "environmentDetails": {"playProtectVerdict": "NO_ISSUES"}
        })
    }

    fn verify(payload: Value) -> Result<(), PlayIntegrityRejection> {
        verify_payload(
            payload,
            PACKAGE,
            &request_hash(CLIENT_DATA),
            &config(),
            NOW_MS,
        )
    }

    fn with(mut payload: Value, pointer: &str, value: Value) -> Value {
        *payload.pointer_mut(pointer).unwrap() = value;
        payload
    }

    #[test]
    fn accepts_a_genuine_play_verdict() {
        assert_eq!(verify(passing()), Ok(()));
    }

    #[test]
    fn rejects_an_unknown_signing_certificate() {
        let payload = with(
            passing(),
            "/appIntegrity/certificateSha256Digest",
            json!([URL_SAFE_NO_PAD.encode(sha256(b"someone else"))]),
        );
        assert_eq!(
            verify(payload),
            Err(PlayIntegrityRejection::AppNotRecognized)
        );
    }

    #[test]
    fn rejects_a_request_hash_for_other_client_data() {
        let other = request_hash(
            br#"{"challenge":"c","age_lower":13,"age_upper":15,"age_range_source":"tier_c"}"#,
        );
        let payload = with(passing(), "/requestDetails/requestHash", json!(other));
        assert_eq!(verify(payload), Err(PlayIntegrityRejection::InvalidToken));
        let mut payload = passing();
        payload["requestDetails"]
            .as_object_mut()
            .unwrap()
            .remove("requestHash");
        assert_eq!(verify(payload), Err(PlayIntegrityRejection::InvalidToken));
    }

    #[test]
    fn maps_age_bands_to_floors() {
        let floor = |lower: u8, upper: Option<u8>| {
            AgeSignalsClientData {
                challenge: String::new(),
                age_lower: lower,
                age_upper: upper,
                age_range_source: AgeRangeSource::TierA,
            }
            .age_floor()
        };
        assert_eq!(floor(18, None), Some(AgeFloor::Adult));
        assert_eq!(floor(16, Some(17)), Some(AgeFloor::Minor));
        assert_eq!(floor(15, None), Some(AgeFloor::Minor));
        assert_eq!(floor(13, Some(15)), Some(AgeFloor::Minor));
        assert_eq!(floor(11, Some(14)), Some(AgeFloor::UnderMinimum));
        assert_eq!(floor(0, Some(12)), Some(AgeFloor::UnderMinimum));
        assert_eq!(floor(16, Some(15)), None);
        assert_eq!(floor(18, Some(17)), None);
        assert_eq!(floor(19, None), None);
    }

    #[test]
    fn only_checked_age_sources_are_verified() {
        assert!(!AgeRangeSource::TierA.is_verified());
        assert!(!AgeRangeSource::TierB.is_verified());
        assert!(AgeRangeSource::TierC.is_verified());
        assert!(AgeRangeSource::TierD.is_verified());
    }
}
