// SPDX-License-Identifier: AGPL-3.0-or-later

use super::store::{AppAttestStore, CHALLENGE_TTL_SECONDS, ChallengeRecord, StoredKey, iso_millis};
use super::verifier::{self, KEY_ID_LEN, KeyForAssertion};
use super::{AgeFloor, AppAttestConfig, AppAttestRejection, ChallengePurpose};
use crate::types::UserResponse;
use base64::Engine;
use base64::engine::general_purpose::{GeneralPurpose, GeneralPurposeConfig, URL_SAFE_NO_PAD};
use base64::engine::{DecodePaddingMode, general_purpose};
use ring::rand::{SecureRandom, SystemRandom};
use serde::Deserialize;
use std::time::{SystemTime, UNIX_EPOCH};

const MAX_KEYS_PER_USER: usize = 10;
const CHALLENGE_BYTES: usize = 32;
const CHALLENGE_TEXT_LEN: usize = 43;
const MAX_ATTESTATION_BYTES: usize = 16 * 1024;
const MAX_ASSERTION_BYTES: usize = 4 * 1024;
const MAX_CLIENT_DATA_BYTES: usize = 1024;

const STANDARD_ANY_PADDING: GeneralPurpose = GeneralPurpose::new(
    &base64::alphabet::STANDARD,
    GeneralPurposeConfig::new().with_decode_padding_mode(DecodePaddingMode::Indifferent),
);

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct AgeClientData {
    challenge: String,
    age_floor: AgeFloor,
}

pub struct AppAttestService {
    config: AppAttestConfig,
    store: AppAttestStore,
    random: SystemRandom,
}

type Outcome = Result<UserResponse, AppAttestRejection>;

impl AppAttestService {
    pub fn new(config: AppAttestConfig, store: AppAttestStore) -> Self {
        Self {
            config,
            store,
            random: SystemRandom::new(),
        }
    }

    pub async fn create_challenge(
        &self,
        user_id: i64,
        purpose: ChallengePurpose,
        now: SystemTime,
    ) -> anyhow::Result<UserResponse> {
        if !self.config.is_enabled() {
            return Ok(rejected(AppAttestRejection::NotConfigured));
        }
        let mut bytes = [0u8; CHALLENGE_BYTES];
        self.random
            .fill(&mut bytes)
            .map_err(|_| anyhow::anyhow!("system random source failed"))?;
        let challenge = URL_SAFE_NO_PAD.encode(bytes);
        let expires_at_ms = unix_ms(now) + CHALLENGE_TTL_SECONDS * 1000;
        self.store
            .insert_challenge(
                &challenge,
                &ChallengeRecord {
                    user_id,
                    purpose,
                    expires_at_ms,
                },
            )
            .await?;
        Ok(UserResponse::AppAttestChallenge {
            challenge,
            expires_at: iso_millis(expires_at_ms),
        })
    }

    pub async fn register_key(
        &self,
        user_id: i64,
        key_id: &str,
        attestation_base64: &str,
        challenge: &str,
        now: SystemTime,
    ) -> anyhow::Result<UserResponse> {
        flatten(
            self.try_register_key(user_id, key_id, attestation_base64, challenge, now)
                .await,
        )
    }

    async fn try_register_key(
        &self,
        user_id: i64,
        key_id: &str,
        attestation_base64: &str,
        challenge: &str,
        now: SystemTime,
    ) -> anyhow::Result<Outcome> {
        if !self.config.is_enabled() {
            return Ok(Err(AppAttestRejection::NotConfigured));
        }
        let Some(key_id) = decode_key_id(key_id) else {
            return Ok(Err(AppAttestRejection::InvalidRequest));
        };
        let Some(attestation) = decode_limited(attestation_base64, MAX_ATTESTATION_BYTES) else {
            return Ok(Err(AppAttestRejection::InvalidRequest));
        };
        if !self
            .consume_challenge(challenge, user_id, ChallengePurpose::Attest, now)
            .await?
        {
            return Ok(Err(AppAttestRejection::InvalidChallenge));
        }
        let client_data_hash = verifier::sha256(challenge.as_bytes());
        let attested = match verifier::verify_attestation(
            &attestation,
            &key_id,
            &client_data_hash,
            now,
            &self.config,
        ) {
            Ok(attested) => attested,
            Err(error) => {
                tracing::info!(user_id, ?error, "app attest attestation rejected");
                return Ok(Err(AppAttestRejection::InvalidAttestation));
            }
        };
        let key_id = general_purpose::STANDARD.encode(key_id);
        if self.store.claim_key_id(&key_id, user_id).await? != user_id {
            return Ok(Err(AppAttestRejection::KeyInUse));
        }
        let key = StoredKey {
            key_id,
            public_key: attested.public_key.to_vec(),
            environment: attested.environment,
            app_id: attested.app_id,
            counter: 0,
            created_at_ms: unix_ms(now),
            last_used_at_ms: None,
        };
        let environment = key.environment;
        if self.store.insert_key_if_absent(user_id, &key).await? {
            self.enforce_key_cap(user_id).await?;
        }
        tracing::info!(user_id, ?environment, "app attest attestation verified");
        Ok(Ok(UserResponse::AppAttestKeyRegistered))
    }

    async fn enforce_key_cap(&self, user_id: i64) -> anyhow::Result<()> {
        let keys = self.store.list_keys(user_id).await?;
        for evicted in keys_to_evict(&keys, MAX_KEYS_PER_USER) {
            self.store.delete_key(user_id, &evicted).await?;
        }
        Ok(())
    }

    pub async fn submit_age(
        &self,
        user_id: i64,
        key_id: &str,
        assertion_base64: &str,
        client_data_base64: &str,
        now: SystemTime,
    ) -> anyhow::Result<UserResponse> {
        flatten(
            self.try_submit_age(user_id, key_id, assertion_base64, client_data_base64, now)
                .await,
        )
    }

    async fn try_submit_age(
        &self,
        user_id: i64,
        key_id: &str,
        assertion_base64: &str,
        client_data_base64: &str,
        now: SystemTime,
    ) -> anyhow::Result<Outcome> {
        if !self.config.is_enabled() {
            return Ok(Err(AppAttestRejection::NotConfigured));
        }
        let (Some(key_id), Some(assertion), Some(client_data)) = (
            decode_key_id(key_id),
            decode_limited(assertion_base64, MAX_ASSERTION_BYTES),
            decode_limited(client_data_base64, MAX_CLIENT_DATA_BYTES),
        ) else {
            return Ok(Err(AppAttestRejection::InvalidRequest));
        };
        let Ok(parsed) = serde_json::from_slice::<AgeClientData>(&client_data) else {
            return Ok(Err(AppAttestRejection::InvalidRequest));
        };
        if !self
            .consume_challenge(&parsed.challenge, user_id, ChallengePurpose::Assert, now)
            .await?
        {
            return Ok(Err(AppAttestRejection::InvalidChallenge));
        }
        let key_id = general_purpose::STANDARD.encode(key_id);
        let Some(key) = self.store.get_key(user_id, &key_id).await? else {
            return Ok(Err(AppAttestRejection::UnknownKey));
        };
        let previous_counter = u32::try_from(key.counter).unwrap_or(u32::MAX);
        let counter = match verifier::verify_assertion(
            &assertion,
            &client_data,
            KeyForAssertion {
                public_key: &key.public_key,
                app_id: &key.app_id,
                environment: key.environment,
            },
            previous_counter,
            &self.config,
        ) {
            Ok(counter) => counter,
            Err(error) => {
                tracing::info!(user_id, ?error, "app attest assertion rejected");
                return Ok(Err(AppAttestRejection::InvalidAssertion));
            }
        };
        let now_ms = unix_ms(now);
        if !self
            .store
            .advance_counter(user_id, &key_id, i64::from(counter), now_ms)
            .await?
        {
            tracing::info!(user_id, "app attest assertion counter lost a race");
            return Ok(Err(AppAttestRejection::InvalidAssertion));
        }
        tracing::info!(user_id, counter, "app attest assertion verified");
        Ok(Ok(UserResponse::AppAttestAgeAccepted {
            age_floor: parsed.age_floor,
            attested_at: iso_millis(now_ms),
        }))
    }

    async fn consume_challenge(
        &self,
        challenge: &str,
        user_id: i64,
        purpose: ChallengePurpose,
        now: SystemTime,
    ) -> anyhow::Result<bool> {
        if challenge.len() != CHALLENGE_TEXT_LEN || URL_SAFE_NO_PAD.decode(challenge).is_err() {
            return Ok(false);
        }
        self.store
            .consume_challenge(challenge, user_id, purpose, unix_ms(now))
            .await
    }
}

fn keys_to_evict(keys: &[StoredKey], max_keys: usize) -> Vec<String> {
    let mut by_age = keys.iter().collect::<Vec<_>>();
    by_age.sort_by(|left, right| {
        let left_used = left.last_used_at_ms.unwrap_or(left.created_at_ms);
        let right_used = right.last_used_at_ms.unwrap_or(right.created_at_ms);
        left_used
            .cmp(&right_used)
            .then_with(|| left.key_id.cmp(&right.key_id))
    });
    let excess = keys.len().saturating_sub(max_keys);
    by_age
        .into_iter()
        .take(excess)
        .map(|key| key.key_id.clone())
        .collect()
}

fn decode_key_id(key_id: &str) -> Option<[u8; KEY_ID_LEN]> {
    STANDARD_ANY_PADDING.decode(key_id).ok()?.try_into().ok()
}

fn decode_limited(value: &str, max_bytes: usize) -> Option<Vec<u8>> {
    if value.len() > max_bytes.div_ceil(3) * 4 {
        return None;
    }
    STANDARD_ANY_PADDING
        .decode(value)
        .ok()
        .filter(|bytes| !bytes.is_empty())
}

fn flatten(outcome: anyhow::Result<Outcome>) -> anyhow::Result<UserResponse> {
    Ok(outcome?.unwrap_or_else(rejected))
}

fn rejected(code: AppAttestRejection) -> UserResponse {
    UserResponse::AppAttestRejected { code }
}

fn unix_ms(now: SystemTime) -> i64 {
    now.duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_millis() as i64)
        .unwrap_or_default()
}
