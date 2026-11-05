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
const MAX_ATTESTATION_BYTES: usize = 64 * 1024;
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
        if self.store.insert_key_if_absent(user_id, &key).await? {
            self.enforce_key_cap(user_id).await?;
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app_attest::Environment;
    use crate::app_attest::verifier::sha256;
    use ring::signature::{ECDSA_P256_SHA256_ASN1_SIGNING, EcdsaKeyPair, KeyPair};
    #[cfg(feature = "scylla")]
    use std::sync::Arc;
    use std::time::Duration;

    const APP_ID: &str = "3G5837T29K.com.fluxer";
    const POSTGRES_ENV: &str = "FLUXER_USERS_TEST_POSTGRES_URL";
    #[cfg(feature = "scylla")]
    const SCYLLA_ENV: &str = "FLUXER_USERS_TEST_SCYLLA_HOST";

    async fn stores() -> Vec<(&'static str, AppAttestStore)> {
        let mut stores = Vec::new();
        if let Ok(url) = std::env::var(POSTGRES_ENV) {
            let config = fluxer_svc::postgres::PostgresConfig {
                url: Some(url),
                host: String::new(),
                port: 0,
                database: String::new(),
                username: String::new(),
                password: None,
                ssl: false,
                ssl_ca: None,
                max_connections: 4,
                kv_table: "fluxer_kv".to_owned(),
                prepared_statements: true,
            };
            let pool = fluxer_svc::postgres::connect(&config)
                .await
                .expect("postgres");
            let kv = fluxer_svc::postgres::KvClient::new(pool, &config).expect("kv client");
            stores.push(("postgres", AppAttestStore::Postgres(kv)));
        }
        #[cfg(feature = "scylla")]
        if let Ok(host) = std::env::var(SCYLLA_ENV) {
            stores.push(("scylla", AppAttestStore::Scylla(scylla_store(&host).await)));
        }
        assert!(
            !stores.is_empty(),
            "set {POSTGRES_ENV} or FLUXER_USERS_TEST_SCYLLA_HOST to run the storage tests"
        );
        stores
    }

    #[cfg(feature = "scylla")]
    async fn scylla_store(host: &str) -> Arc<crate::app_attest::store::ScyllaAppAttestStore> {
        use scylla::client::session_builder::SessionBuilder;
        let schema: serde_json::Value = serde_json::from_str(include_str!(
            "../../../tools/dev/cassandra_target_schema.json"
        ))
        .expect("schema");
        let admin = SessionBuilder::new()
            .known_node(host)
            .build()
            .await
            .expect("scylla");
        admin
            .query_unpaged(
                "CREATE KEYSPACE IF NOT EXISTS app_attest_test WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 1}",
                (),
            )
            .await
            .expect("keyspace");
        for table in schema["tables"].as_array().expect("tables") {
            let name = table["name"].as_str().expect("name");
            if !name.starts_with("app_attest_") {
                continue;
            }
            let columns = table["columns"]
                .as_array()
                .expect("columns")
                .iter()
                .map(|column| {
                    format!(
                        "{} {}",
                        column["name"].as_str().unwrap(),
                        column["type"].as_str().unwrap()
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let primary_key = table["primary_key"].as_str().expect("primary key");
            admin
                .query_unpaged(
                    format!("CREATE TABLE IF NOT EXISTS app_attest_test.{name} ({columns}, PRIMARY KEY {primary_key})"),
                    (),
                )
                .await
                .expect("table");
        }
        admin
            .await_schema_agreement()
            .await
            .expect("schema agreement");
        let db = fluxer_svc::scylla::connect(&fluxer_svc::scylla::ScyllaConfig {
            hosts: vec![host.to_owned()],
            keyspace: "app_attest_test".to_owned(),
            username: None,
            password: None,
        })
        .await
        .expect("scylla keyspace");
        Arc::new(crate::app_attest::store::ScyllaAppAttestStore::new(db))
    }

    fn config() -> AppAttestConfig {
        AppAttestConfig {
            app_ids: vec![APP_ID.to_owned()],
            allow_development: false,
        }
    }

    fn user() -> i64 {
        let mut bytes = [0u8; 8];
        SystemRandom::new().fill(&mut bytes).expect("random");
        i64::from_be_bytes(bytes) & i64::MAX
    }

    struct Device {
        key_id: String,
        pair: EcdsaKeyPair,
    }

    impl Device {
        fn new() -> Self {
            let random = SystemRandom::new();
            let pkcs8 = EcdsaKeyPair::generate_pkcs8(&ECDSA_P256_SHA256_ASN1_SIGNING, &random)
                .expect("key");
            let pair =
                EcdsaKeyPair::from_pkcs8(&ECDSA_P256_SHA256_ASN1_SIGNING, pkcs8.as_ref(), &random)
                    .expect("pair");
            Self {
                key_id: general_purpose::STANDARD.encode(sha256(pair.public_key().as_ref())),
                pair,
            }
        }

        fn stored(&self, created_at_ms: i64) -> StoredKey {
            StoredKey {
                key_id: self.key_id.clone(),
                public_key: self.pair.public_key().as_ref().to_vec(),
                environment: Environment::Production,
                app_id: APP_ID.to_owned(),
                counter: 0,
                created_at_ms,
                last_used_at_ms: None,
            }
        }

        fn assert(&self, challenge: &str, age_floor: &str, counter: u32) -> (String, String) {
            let client_data = format!(r#"{{"challenge":"{challenge}","age_floor":"{age_floor}"}}"#);
            let mut auth_data = sha256(APP_ID.as_bytes()).to_vec();
            auth_data.push(0x40);
            auth_data.extend_from_slice(&counter.to_be_bytes());
            let mut message = auth_data.clone();
            message.extend_from_slice(&sha256(client_data.as_bytes()));
            let nonce = sha256(&message);
            let signature = self
                .pair
                .sign(&SystemRandom::new(), &nonce)
                .expect("signature");
            let object = ciborium::Value::Map(vec![
                (
                    ciborium::Value::Text("signature".to_owned()),
                    ciborium::Value::Bytes(signature.as_ref().to_vec()),
                ),
                (
                    ciborium::Value::Text("authenticatorData".to_owned()),
                    ciborium::Value::Bytes(auth_data),
                ),
            ]);
            let mut encoded = Vec::new();
            ciborium::ser::into_writer(&object, &mut encoded).expect("cbor");
            (
                general_purpose::STANDARD.encode(encoded),
                general_purpose::STANDARD.encode(client_data),
            )
        }
    }

    async fn challenge(
        service: &AppAttestService,
        user_id: i64,
        purpose: ChallengePurpose,
    ) -> String {
        challenge_at(service, user_id, purpose, SystemTime::now()).await
    }

    async fn challenge_at(
        service: &AppAttestService,
        user_id: i64,
        purpose: ChallengePurpose,
        now: SystemTime,
    ) -> String {
        match service
            .create_challenge(user_id, purpose, now)
            .await
            .expect("challenge")
        {
            UserResponse::AppAttestChallenge { challenge, .. } => challenge,
            other => panic!("unexpected {other:?}"),
        }
    }

    async fn submit(
        service: &AppAttestService,
        user_id: i64,
        device: &Device,
        challenge: &str,
        counter: u32,
    ) -> UserResponse {
        let (assertion, client_data) = device.assert(challenge, "minor", counter);
        service
            .submit_age(
                user_id,
                &device.key_id,
                &assertion,
                &client_data,
                SystemTime::now(),
            )
            .await
            .expect("submit")
    }

    fn is_rejected(response: &UserResponse, expected: AppAttestRejection) -> bool {
        matches!(response, UserResponse::AppAttestRejected { code } if *code == expected)
    }

    #[tokio::test]
    #[ignore = "needs FLUXER_USERS_TEST_POSTGRES_URL or FLUXER_USERS_TEST_SCYLLA_HOST"]
    async fn challenges_are_single_use_and_bound_to_user_purpose_and_time() {
        for (backend, store) in stores().await {
            let service = AppAttestService::new(config(), store.clone());
            let user_id = user();
            let device = Device::new();
            assert!(
                store
                    .insert_key_if_absent(user_id, &device.stored(1))
                    .await
                    .unwrap()
            );

            let assert = challenge(&service, user_id, ChallengePurpose::Assert).await;
            assert!(
                matches!(
                    submit(&service, user_id, &device, &assert, 1).await,
                    UserResponse::AppAttestAgeAccepted {
                        age_floor: AgeFloor::Minor,
                        ..
                    }
                ),
                "{backend}"
            );
            assert!(
                is_rejected(
                    &submit(&service, user_id, &device, &assert, 2).await,
                    AppAttestRejection::InvalidChallenge
                ),
                "{backend} reused"
            );

            let attest = challenge(&service, user_id, ChallengePurpose::Attest).await;
            assert!(
                is_rejected(
                    &submit(&service, user_id, &device, &attest, 3).await,
                    AppAttestRejection::InvalidChallenge
                ),
                "{backend} purpose"
            );

            let foreign = challenge(&service, user(), ChallengePurpose::Assert).await;
            assert!(
                is_rejected(
                    &submit(&service, user_id, &device, &foreign, 4).await,
                    AppAttestRejection::InvalidChallenge
                ),
                "{backend} foreign"
            );

            let expired = challenge_at(
                &service,
                user_id,
                ChallengePurpose::Assert,
                SystemTime::now() - Duration::from_secs(301),
            )
            .await;
            assert!(
                is_rejected(
                    &submit(&service, user_id, &device, &expired, 5).await,
                    AppAttestRejection::InvalidChallenge
                ),
                "{backend} expired"
            );

            let unknown = URL_SAFE_NO_PAD.encode([7u8; CHALLENGE_BYTES]);
            assert!(
                is_rejected(
                    &submit(&service, user_id, &device, &unknown, 6).await,
                    AppAttestRejection::InvalidChallenge
                ),
                "{backend} unknown"
            );

            let raced = challenge(&service, user_id, ChallengePurpose::Assert).await;
            let (first, second) = tokio::join!(
                store.consume_challenge(
                    &raced,
                    user_id,
                    ChallengePurpose::Assert,
                    unix_ms(SystemTime::now())
                ),
                store.consume_challenge(
                    &raced,
                    user_id,
                    ChallengePurpose::Assert,
                    unix_ms(SystemTime::now())
                ),
            );
            assert!(
                first.unwrap() ^ second.unwrap(),
                "{backend} concurrent consume"
            );
        }
    }

    #[tokio::test]
    #[ignore = "needs FLUXER_USERS_TEST_POSTGRES_URL or FLUXER_USERS_TEST_SCYLLA_HOST"]
    async fn assertion_counters_only_move_forward() {
        for (backend, store) in stores().await {
            let service = AppAttestService::new(config(), store.clone());
            let user_id = user();
            let device = Device::new();
            assert!(
                store
                    .insert_key_if_absent(user_id, &device.stored(1))
                    .await
                    .unwrap()
            );
            let next = || challenge(&service, user_id, ChallengePurpose::Assert);

            let accepted = submit(&service, user_id, &device, &next().await, 5).await;
            assert!(
                matches!(accepted, UserResponse::AppAttestAgeAccepted { .. }),
                "{backend} {accepted:?}"
            );
            for stale in [5, 3, 0] {
                assert!(
                    is_rejected(
                        &submit(&service, user_id, &device, &next().await, stale).await,
                        AppAttestRejection::InvalidAssertion
                    ),
                    "{backend} counter {stale}"
                );
            }
            assert!(matches!(
                submit(&service, user_id, &device, &next().await, 6).await,
                UserResponse::AppAttestAgeAccepted { .. }
            ));
            let stored = store
                .get_key(user_id, &device.key_id)
                .await
                .unwrap()
                .unwrap();
            assert_eq!(stored.counter, 6, "{backend}");
            assert!(stored.last_used_at_ms.is_some(), "{backend}");
            assert_eq!(
                stored.public_key,
                device.pair.public_key().as_ref(),
                "{backend}"
            );

            let now = unix_ms(SystemTime::now());
            let (first, second) = tokio::join!(
                store.advance_counter(user_id, &device.key_id, 9, now),
                store.advance_counter(user_id, &device.key_id, 9, now),
            );
            assert!(
                first.unwrap() ^ second.unwrap(),
                "{backend} concurrent counter"
            );

            let stranger = user();
            assert!(
                is_rejected(
                    &submit(
                        &service,
                        stranger,
                        &device,
                        &challenge(&service, stranger, ChallengePurpose::Assert).await,
                        10
                    )
                    .await,
                    AppAttestRejection::UnknownKey
                ),
                "{backend} key of another user"
            );
        }
    }

    #[tokio::test]
    #[ignore = "needs FLUXER_USERS_TEST_POSTGRES_URL or FLUXER_USERS_TEST_SCYLLA_HOST"]
    async fn a_key_id_belongs_to_one_user_and_each_user_keeps_the_newest_ten() {
        for (backend, store) in stores().await {
            let service = AppAttestService::new(config(), store.clone());
            let owner = user();
            let other = user();
            let device = Device::new();
            assert_eq!(
                store.claim_key_id(&device.key_id, owner).await.unwrap(),
                owner
            );
            assert_eq!(
                store.claim_key_id(&device.key_id, other).await.unwrap(),
                owner
            );
            assert_eq!(
                store.claim_key_id(&device.key_id, owner).await.unwrap(),
                owner
            );
            let contested = Device::new();
            let (first, second) = tokio::join!(
                store.claim_key_id(&contested.key_id, owner),
                store.claim_key_id(&contested.key_id, other),
            );
            assert_eq!(
                first.unwrap(),
                second.unwrap(),
                "{backend} concurrent claim"
            );

            let devices = (0..12).map(|_| Device::new()).collect::<Vec<_>>();
            for (index, device) in devices.iter().enumerate() {
                assert_eq!(
                    store.claim_key_id(&device.key_id, owner).await.unwrap(),
                    owner
                );
                assert!(
                    store
                        .insert_key_if_absent(owner, &device.stored(1_000 + index as i64))
                        .await
                        .unwrap()
                );
            }
            assert!(
                !store
                    .insert_key_if_absent(owner, &devices[0].stored(5_000))
                    .await
                    .unwrap(),
                "{backend} second insert"
            );
            store
                .advance_counter(owner, &devices[0].key_id, 1, 10_000)
                .await
                .unwrap();
            service.enforce_key_cap(owner).await.unwrap();
            let mut kept = store
                .list_keys(owner)
                .await
                .unwrap()
                .into_iter()
                .map(|key| key.key_id)
                .collect::<Vec<_>>();
            kept.sort();
            let mut expected = std::iter::once(&devices[0])
                .chain(&devices[3..])
                .map(|device| device.key_id.clone())
                .collect::<Vec<_>>();
            expected.sort();
            assert_eq!(kept, expected, "{backend}");
            for evicted in &devices[1..3] {
                assert_eq!(
                    store.claim_key_id(&evicted.key_id, other).await.unwrap(),
                    other,
                    "{backend} evicted lookup is released"
                );
            }
        }
    }
}
