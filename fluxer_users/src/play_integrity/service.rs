// SPDX-License-Identifier: AGPL-3.0-or-later

use super::google::{DecodeError, GoogleClient};
use super::verifier::{self, AgeSignalsClientData};
use super::{PlayIntegrityConfig, PlayIntegrityRejection};
use crate::app_attest::ChallengePurpose;
use crate::app_attest::service::{
    MAX_CLIENT_DATA_BYTES, consume_challenge, decode_limited, issue_challenge, unix_ms,
};
use crate::app_attest::store::{AppAttestStore, iso_millis};
use crate::types::UserResponse;
use ring::rand::SystemRandom;
use std::time::SystemTime;

const MAX_INTEGRITY_TOKEN_LEN: usize = 16 * 1024;

pub struct PlayIntegrityService {
    config: PlayIntegrityConfig,
    google: Option<GoogleClient>,
    store: AppAttestStore,
    random: SystemRandom,
}

type Outcome = Result<UserResponse, PlayIntegrityRejection>;

impl PlayIntegrityService {
    pub fn new(config: PlayIntegrityConfig, store: AppAttestStore) -> anyhow::Result<Self> {
        let google = match (&config.service_account, config.is_enabled()) {
            (Some(account), true) => Some(GoogleClient::new(account.clone())?),
            _ => None,
        };
        Ok(Self {
            config,
            google,
            store,
            random: SystemRandom::new(),
        })
    }

    pub async fn create_challenge(
        &self,
        user_id: i64,
        now: SystemTime,
    ) -> anyhow::Result<UserResponse> {
        if self.google.is_none() {
            return Ok(rejected(PlayIntegrityRejection::NotConfigured));
        }
        issue_challenge(
            &self.store,
            &self.random,
            user_id,
            ChallengePurpose::PlayIntegrity,
            now,
        )
        .await
    }

    pub async fn submit_age(
        &self,
        user_id: i64,
        package_name: &str,
        integrity_token: &str,
        client_data_base64: &str,
        now: SystemTime,
    ) -> anyhow::Result<UserResponse> {
        Ok(self
            .try_submit_age(
                user_id,
                package_name,
                integrity_token,
                client_data_base64,
                now,
            )
            .await?
            .unwrap_or_else(rejected))
    }

    async fn try_submit_age(
        &self,
        user_id: i64,
        package_name: &str,
        integrity_token: &str,
        client_data_base64: &str,
        now: SystemTime,
    ) -> anyhow::Result<Outcome> {
        let Some(google) = &self.google else {
            return Ok(Err(PlayIntegrityRejection::NotConfigured));
        };
        if !self.config.allows_package(package_name) || !is_plausible_token(integrity_token) {
            return Ok(Err(PlayIntegrityRejection::InvalidRequest));
        }
        let Some(client_data) = decode_limited(client_data_base64, MAX_CLIENT_DATA_BYTES) else {
            return Ok(Err(PlayIntegrityRejection::InvalidRequest));
        };
        let Ok(parsed) = serde_json::from_slice::<AgeSignalsClientData>(&client_data) else {
            return Ok(Err(PlayIntegrityRejection::InvalidRequest));
        };
        let Some(age_floor) = parsed.age_floor() else {
            return Ok(Err(PlayIntegrityRejection::InvalidRequest));
        };
        if !consume_challenge(
            &self.store,
            &parsed.challenge,
            user_id,
            ChallengePurpose::PlayIntegrity,
            now,
        )
        .await?
        {
            return Ok(Err(PlayIntegrityRejection::InvalidChallenge));
        }
        let now_ms = unix_ms(now);
        let payload = match google
            .decode_integrity_token(package_name, integrity_token, now_ms / 1000)
            .await
        {
            Ok(payload) => payload,
            Err(DecodeError::TokenRejected) => {
                tracing::info!(user_id, "play integrity token rejected by google");
                return Ok(Err(PlayIntegrityRejection::InvalidToken));
            }
            Err(DecodeError::Refused(status)) => {
                tracing::error!(
                    user_id,
                    status,
                    "play integrity decode refused, check the cloud project link"
                );
                return Ok(Err(PlayIntegrityRejection::InvalidToken));
            }
            Err(DecodeError::Unavailable(reason)) => {
                tracing::warn!(user_id, reason, "play integrity decode unavailable");
                return Ok(Err(PlayIntegrityRejection::Unavailable));
            }
        };
        if let Err(rejection) = verifier::verify_payload(
            payload,
            package_name,
            &verifier::request_hash(&client_data),
            &self.config,
            now_ms,
        ) {
            tracing::info!(user_id, ?rejection, "play integrity verdict rejected");
            return Ok(Err(rejection));
        }
        tracing::info!(user_id, "play integrity age range accepted");
        Ok(Ok(UserResponse::AppAttestAgeAccepted {
            age_floor,
            attested_at: iso_millis(now_ms),
        }))
    }
}

fn is_plausible_token(token: &str) -> bool {
    !token.is_empty()
        && token.len() <= MAX_INTEGRITY_TOKEN_LEN
        && token
            .bytes()
            .all(|byte| byte.is_ascii_graphic() && byte != b'"' && byte != b'\\')
}

fn rejected(code: PlayIntegrityRejection) -> UserResponse {
    UserResponse::PlayIntegrityRejected { code }
}
