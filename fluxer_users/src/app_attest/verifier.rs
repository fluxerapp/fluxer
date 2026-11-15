// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{AppAttestConfig, Environment};
use base64::Engine;
use base64::engine::general_purpose::STANDARD;
use ciborium::Value;
use ring::signature::{self, UnparsedPublicKey, VerificationAlgorithm};
use sha2::{Digest, Sha256};
use std::sync::LazyLock;
use std::time::SystemTime;
use x509_cert::Certificate;
use x509_cert::der::asn1::ObjectIdentifier;
use x509_cert::der::{Decode, Encode};
use x509_cert::ext::pkix::BasicConstraints;

pub const KEY_ID_LEN: usize = 32;
pub const PUBLIC_KEY_LEN: usize = 65;

const ROOT_CA_PEM: &str = include_str!("Apple_App_Attestation_Root_CA.pem");
const FORMAT: &str = "apple-appattest";
const MAX_CHAIN_LEN: usize = 4;
const MAX_MAP_ENTRIES: usize = 16;
const RP_ID_HASH_END: usize = 32;
const FLAGS_INDEX: usize = 32;
const COUNTER_END: usize = 37;
const AAGUID_END: usize = 53;
const CREDENTIAL_ID_LEN_END: usize = 55;
const FLAG_ATTESTED_CREDENTIAL_DATA: u8 = 0x40;
const AAGUID_DEVELOPMENT: &[u8; 16] = b"appattestdevelop";
const AAGUID_PRODUCTION: &[u8; 16] = b"appattest\0\0\0\0\0\0\0";
const NONCE_EXTENSION_PREFIX: [u8; 6] = [0x30, 0x24, 0xa1, 0x22, 0x04, 0x20];

const OID_NONCE: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.840.113635.100.8.2");
const OID_EC_PUBLIC_KEY: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.840.10045.2.1");
const OID_P256: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.840.10045.3.1.7");
const OID_P384: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.3.132.0.34");
const OID_ECDSA_SHA256: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.840.10045.4.3.2");
const OID_ECDSA_SHA384: ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.840.10045.4.3.3");
const OID_BASIC_CONSTRAINTS: ObjectIdentifier = ObjectIdentifier::new_unwrap("2.5.29.19");
const OID_KEY_USAGE: ObjectIdentifier = ObjectIdentifier::new_unwrap("2.5.29.15");

static ROOT_CA: LazyLock<Certificate> = LazyLock::new(|| {
    let body = ROOT_CA_PEM
        .lines()
        .filter(|line| !line.starts_with("-----"))
        .collect::<String>();
    let der = STANDARD
        .decode(body)
        .expect("the embedded App Attest root is base64");
    Certificate::from_der(&der).expect("the embedded App Attest root is a certificate")
});

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttestationError {
    Malformed,
    UnsupportedFormat,
    Certificate,
    UntrustedChain,
    CertificateTime,
    Nonce,
    KeyId,
    AppId,
    Flags,
    Counter,
    Environment,
    DevelopmentNotAllowed,
    CredentialId,
    CoseKey,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssertionError {
    Malformed,
    KeyNotAccepted,
    Signature,
    AppId,
    Counter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttestedKey {
    pub public_key: [u8; PUBLIC_KEY_LEN],
    pub environment: Environment,
    pub app_id: String,
}

#[derive(Clone, Copy, Debug)]
pub struct KeyForAssertion<'a> {
    pub public_key: &'a [u8],
    pub app_id: &'a str,
    pub environment: Environment,
}

pub fn verify_attestation(
    attestation: &[u8],
    key_id: &[u8],
    client_data_hash: &[u8],
    now: SystemTime,
    config: &AppAttestConfig,
) -> Result<AttestedKey, AttestationError> {
    if key_id.len() != KEY_ID_LEN {
        return Err(AttestationError::KeyId);
    }
    let object = decode_single(attestation).ok_or(AttestationError::Malformed)?;
    let object = as_map(&object).ok_or(AttestationError::Malformed)?;
    let format = map_text(object, "fmt").ok_or(AttestationError::Malformed)?;
    if format != FORMAT {
        return Err(AttestationError::UnsupportedFormat);
    }
    let statement = map_get(object, "attStmt")
        .and_then(as_map)
        .ok_or(AttestationError::Malformed)?;
    let auth_data = map_bytes(object, "authData").ok_or(AttestationError::Malformed)?;
    map_bytes(statement, "receipt").ok_or(AttestationError::Malformed)?;
    let chain = map_get(statement, "x5c")
        .and_then(Value::as_array)
        .ok_or(AttestationError::Malformed)?
        .iter()
        .map(|cert| cert.as_bytes().map(Vec::as_slice))
        .collect::<Option<Vec<_>>>()
        .ok_or(AttestationError::Malformed)?;

    let leaf = verify_chain(&chain, now)?;
    let nonce = sha256_concat(auth_data, client_data_hash);
    if !constant_time_eq(&leaf_nonce(&leaf)?, &nonce) {
        return Err(AttestationError::Nonce);
    }
    let public_key = leaf_public_key(&leaf)?;
    if !constant_time_eq(&sha256(&public_key), key_id) {
        return Err(AttestationError::KeyId);
    }
    let (environment, app_id, cose_key) = check_attested_auth_data(auth_data, key_id, config)?;
    if cose_key != public_key {
        return Err(AttestationError::CoseKey);
    }
    Ok(AttestedKey {
        public_key,
        environment,
        app_id,
    })
}

pub fn check_attested_auth_data(
    auth_data: &[u8],
    key_id: &[u8],
    config: &AppAttestConfig,
) -> Result<(Environment, String, [u8; PUBLIC_KEY_LEN]), AttestationError> {
    if auth_data.len() < CREDENTIAL_ID_LEN_END + KEY_ID_LEN {
        return Err(AttestationError::Malformed);
    }
    let app_id = configured_app_id(&auth_data[..RP_ID_HASH_END], config)
        .ok_or(AttestationError::AppId)?
        .to_owned();
    if auth_data[FLAGS_INDEX] & FLAG_ATTESTED_CREDENTIAL_DATA == 0 {
        return Err(AttestationError::Flags);
    }
    if read_counter(auth_data) != 0 {
        return Err(AttestationError::Counter);
    }
    let environment = match &auth_data[COUNTER_END..AAGUID_END] {
        aaguid if aaguid == AAGUID_PRODUCTION => Environment::Production,
        aaguid if aaguid == AAGUID_DEVELOPMENT => Environment::Development,
        _ => return Err(AttestationError::Environment),
    };
    if environment == Environment::Development && !config.allow_development {
        return Err(AttestationError::DevelopmentNotAllowed);
    }
    let credential_id_len =
        u16::from_be_bytes([auth_data[AAGUID_END], auth_data[AAGUID_END + 1]]) as usize;
    let credential_id_end = CREDENTIAL_ID_LEN_END + KEY_ID_LEN;
    if credential_id_len != KEY_ID_LEN
        || !constant_time_eq(&auth_data[CREDENTIAL_ID_LEN_END..credential_id_end], key_id)
    {
        return Err(AttestationError::CredentialId);
    }
    let mut rest = &auth_data[credential_id_end..];
    let cose_key = decode_next(&mut rest).ok_or(AttestationError::CoseKey)?;
    let cose_key = cose_public_key(&cose_key).ok_or(AttestationError::CoseKey)?;
    if !rest.is_empty() {
        let extensions = decode_single(rest).ok_or(AttestationError::Malformed)?;
        as_map(&extensions).ok_or(AttestationError::Malformed)?;
    }
    Ok((environment, app_id, cose_key))
}

pub fn verify_assertion(
    assertion: &[u8],
    client_data: &[u8],
    key: KeyForAssertion<'_>,
    previous_counter: u32,
    config: &AppAttestConfig,
) -> Result<u32, AssertionError> {
    if !config.app_ids.iter().any(|app_id| app_id == key.app_id)
        || (key.environment == Environment::Development && !config.allow_development)
    {
        return Err(AssertionError::KeyNotAccepted);
    }
    let object = decode_single(assertion).ok_or(AssertionError::Malformed)?;
    let object = as_map(&object).ok_or(AssertionError::Malformed)?;
    let signature_der = map_bytes(object, "signature").ok_or(AssertionError::Malformed)?;
    let auth_data = map_bytes(object, "authenticatorData").ok_or(AssertionError::Malformed)?;
    if auth_data.len() < COUNTER_END {
        return Err(AssertionError::Malformed);
    }
    if auth_data.len() > COUNTER_END {
        let extensions =
            decode_single(&auth_data[COUNTER_END..]).ok_or(AssertionError::Malformed)?;
        as_map(&extensions).ok_or(AssertionError::Malformed)?;
    }
    let nonce = sha256_concat(auth_data, &sha256(client_data));
    UnparsedPublicKey::new(&signature::ECDSA_P256_SHA256_ASN1, key.public_key)
        .verify(&nonce, signature_der)
        .map_err(|_| AssertionError::Signature)?;
    if !constant_time_eq(&auth_data[..RP_ID_HASH_END], &sha256(key.app_id.as_bytes())) {
        return Err(AssertionError::AppId);
    }
    let counter = read_counter(auth_data);
    if counter <= previous_counter {
        return Err(AssertionError::Counter);
    }
    Ok(counter)
}

fn verify_chain(chain: &[&[u8]], now: SystemTime) -> Result<Certificate, AttestationError> {
    if chain.len() < 2 || chain.len() > MAX_CHAIN_LEN {
        return Err(AttestationError::UntrustedChain);
    }
    let certs = chain
        .iter()
        .map(|der| Certificate::from_der(der).map_err(|_| AttestationError::Certificate))
        .collect::<Result<Vec<_>, _>>()?;
    check_validity(&ROOT_CA, now)?;
    for (index, cert) in certs.iter().enumerate() {
        let issuer = certs.get(index + 1).unwrap_or(&ROOT_CA);
        check_validity(cert, now)?;
        check_critical_extensions(cert)?;
        if !is_ca(issuer)? {
            return Err(AttestationError::UntrustedChain);
        }
        check_issued_by(cert, issuer)?;
    }
    let leaf = certs
        .into_iter()
        .next()
        .ok_or(AttestationError::Malformed)?;
    if is_ca(&leaf)? {
        return Err(AttestationError::UntrustedChain);
    }
    Ok(leaf)
}

fn check_validity(cert: &Certificate, now: SystemTime) -> Result<(), AttestationError> {
    let now = now
        .duration_since(SystemTime::UNIX_EPOCH)
        .map_err(|_| AttestationError::CertificateTime)?;
    let validity = &cert.tbs_certificate.validity;
    if now < validity.not_before.to_unix_duration() || now > validity.not_after.to_unix_duration() {
        return Err(AttestationError::CertificateTime);
    }
    Ok(())
}

fn check_critical_extensions(cert: &Certificate) -> Result<(), AttestationError> {
    let known = |oid: &ObjectIdentifier| *oid == OID_BASIC_CONSTRAINTS || *oid == OID_KEY_USAGE;
    let unknown_critical = cert
        .tbs_certificate
        .extensions
        .iter()
        .flatten()
        .any(|extension| extension.critical && !known(&extension.extn_id));
    if unknown_critical {
        return Err(AttestationError::Certificate);
    }
    Ok(())
}

fn is_ca(cert: &Certificate) -> Result<bool, AttestationError> {
    let mut found = cert
        .tbs_certificate
        .extensions
        .iter()
        .flatten()
        .filter(|extension| extension.extn_id == OID_BASIC_CONSTRAINTS);
    let Some(extension) = found.next() else {
        return Ok(false);
    };
    if found.next().is_some() {
        return Err(AttestationError::Certificate);
    }
    BasicConstraints::from_der(extension.extn_value.as_bytes())
        .map(|constraints| constraints.ca)
        .map_err(|_| AttestationError::Certificate)
}

fn check_issued_by(cert: &Certificate, issuer: &Certificate) -> Result<(), AttestationError> {
    let issuer_name = cert
        .tbs_certificate
        .issuer
        .to_der()
        .map_err(|_| AttestationError::Certificate)?;
    let subject_name = issuer
        .tbs_certificate
        .subject
        .to_der()
        .map_err(|_| AttestationError::Certificate)?;
    if issuer_name != subject_name {
        return Err(AttestationError::UntrustedChain);
    }
    if cert.signature_algorithm != cert.tbs_certificate.signature
        || cert.signature_algorithm.parameters.is_some()
    {
        return Err(AttestationError::Certificate);
    }
    let curve = ec_curve(issuer).ok_or(AttestationError::UntrustedChain)?;
    let algorithm: &dyn VerificationAlgorithm = match (curve, cert.signature_algorithm.oid) {
        (OID_P384, OID_ECDSA_SHA384) => &signature::ECDSA_P384_SHA384_ASN1,
        (OID_P384, OID_ECDSA_SHA256) => &signature::ECDSA_P384_SHA256_ASN1,
        (OID_P256, OID_ECDSA_SHA256) => &signature::ECDSA_P256_SHA256_ASN1,
        _ => return Err(AttestationError::UntrustedChain),
    };
    let issuer_key = issuer
        .tbs_certificate
        .subject_public_key_info
        .subject_public_key
        .as_bytes()
        .ok_or(AttestationError::Certificate)?;
    let signature = cert
        .signature
        .as_bytes()
        .ok_or(AttestationError::Certificate)?;
    let tbs = cert
        .tbs_certificate
        .to_der()
        .map_err(|_| AttestationError::Certificate)?;
    UnparsedPublicKey::new(algorithm, issuer_key)
        .verify(&tbs, signature)
        .map_err(|_| AttestationError::UntrustedChain)
}

fn ec_curve(cert: &Certificate) -> Option<ObjectIdentifier> {
    let algorithm = &cert.tbs_certificate.subject_public_key_info.algorithm;
    if algorithm.oid != OID_EC_PUBLIC_KEY {
        return None;
    }
    algorithm.parameters.as_ref()?.decode_as().ok()
}

fn leaf_nonce(leaf: &Certificate) -> Result<[u8; 32], AttestationError> {
    let mut found = leaf
        .tbs_certificate
        .extensions
        .iter()
        .flatten()
        .filter(|extension| extension.extn_id == OID_NONCE);
    let extension = found.next().ok_or(AttestationError::Nonce)?;
    if found.next().is_some() {
        return Err(AttestationError::Nonce);
    }
    let value = extension.extn_value.as_bytes();
    let nonce = value
        .strip_prefix(&NONCE_EXTENSION_PREFIX)
        .ok_or(AttestationError::Nonce)?;
    nonce.try_into().map_err(|_| AttestationError::Nonce)
}

fn leaf_public_key(leaf: &Certificate) -> Result<[u8; PUBLIC_KEY_LEN], AttestationError> {
    if ec_curve(leaf) != Some(OID_P256) {
        return Err(AttestationError::KeyId);
    }
    let point = leaf
        .tbs_certificate
        .subject_public_key_info
        .subject_public_key
        .as_bytes()
        .ok_or(AttestationError::KeyId)?;
    if point.first() != Some(&0x04) {
        return Err(AttestationError::KeyId);
    }
    point.try_into().map_err(|_| AttestationError::KeyId)
}

fn cose_public_key(value: &Value) -> Option<[u8; PUBLIC_KEY_LEN]> {
    let map = as_map(value)?;
    if map.len() != 5 {
        return None;
    }
    let int_entry = |label: i128| {
        map.iter()
            .find(|(key, _)| key.as_integer().map(i128::from) == Some(label))
            .map(|(_, value)| value)
    };
    let int_value = |label: i128| int_entry(label)?.as_integer().map(i128::from);
    if int_value(1)? != 2 || int_value(3)? != -7 || int_value(-1)? != 1 {
        return None;
    }
    let x = int_entry(-2)?.as_bytes()?;
    let y = int_entry(-3)?.as_bytes()?;
    if x.len() != 32 || y.len() != 32 {
        return None;
    }
    let mut point = [0u8; PUBLIC_KEY_LEN];
    point[0] = 0x04;
    point[1..33].copy_from_slice(x);
    point[33..].copy_from_slice(y);
    Some(point)
}

fn configured_app_id<'a>(rp_id_hash: &[u8], config: &'a AppAttestConfig) -> Option<&'a str> {
    config
        .app_ids
        .iter()
        .find(|app_id| constant_time_eq(&sha256(app_id.as_bytes()), rp_id_hash))
        .map(String::as_str)
}

fn read_counter(auth_data: &[u8]) -> u32 {
    u32::from_be_bytes([
        auth_data[FLAGS_INDEX + 1],
        auth_data[FLAGS_INDEX + 2],
        auth_data[FLAGS_INDEX + 3],
        auth_data[FLAGS_INDEX + 4],
    ])
}

fn decode_next(input: &mut &[u8]) -> Option<Value> {
    ciborium::de::from_reader::<Value, _>(input).ok()
}

fn decode_single(bytes: &[u8]) -> Option<Value> {
    let mut rest = bytes;
    let value = decode_next(&mut rest)?;
    rest.is_empty().then_some(value)
}

fn as_map(value: &Value) -> Option<&[(Value, Value)]> {
    let map = value.as_map()?;
    if map.len() > MAX_MAP_ENTRIES {
        return None;
    }
    let distinct = map
        .iter()
        .enumerate()
        .all(|(index, (key, _))| map[..index].iter().all(|(other, _)| other != key));
    distinct.then_some(map.as_slice())
}

fn map_get<'a>(map: &'a [(Value, Value)], key: &str) -> Option<&'a Value> {
    map.iter()
        .find(|(candidate, _)| candidate.as_text() == Some(key))
        .map(|(_, value)| value)
}

fn map_text<'a>(map: &'a [(Value, Value)], key: &str) -> Option<&'a str> {
    map_get(map, key)?.as_text()
}

fn map_bytes<'a>(map: &'a [(Value, Value)], key: &str) -> Option<&'a [u8]> {
    map_get(map, key)?.as_bytes().map(Vec::as_slice)
}

pub fn sha256(bytes: &[u8]) -> [u8; 32] {
    Sha256::digest(bytes).into()
}

fn sha256_concat(first: &[u8], second: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(first);
    hasher.update(second);
    hasher.finalize().into()
}

pub fn constant_time_eq(left: &[u8], right: &[u8]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .fold(0u8, |diff, (a, b)| diff | (a ^ b))
            == 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value as Json;
    use std::time::Duration;

    const VECTORS: &str = include_str!("testdata/vectors.json");

    struct Attestation {
        id: String,
        object: Vec<u8>,
        key_id: Vec<u8>,
        client_data_hash: Vec<u8>,
        app_id: String,
        environment: Environment,
        public_key: Vec<u8>,
        now: SystemTime,
    }

    struct Assertion {
        id: String,
        object: Vec<u8>,
        client_data: Vec<u8>,
        app_id: String,
        environment: Environment,
        public_key: Vec<u8>,
        previous_counter: u32,
        expected_counter: Option<u32>,
    }

    fn vectors() -> Vec<Json> {
        serde_json::from_str::<Json>(VECTORS)
            .expect("vectors parse")
            .as_array()
            .expect("a vector list")
            .clone()
    }

    fn testdata(name: &str) -> Vec<u8> {
        std::fs::read(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("src/app_attest/testdata")
                .join(name),
        )
        .expect("vector file")
    }

    fn text<'a>(vector: &'a Json, field: &str) -> &'a str {
        vector[field].as_str().unwrap_or_else(|| panic!("{field}"))
    }

    fn b64(vector: &Json, field: &str) -> Vec<u8> {
        STANDARD.decode(text(vector, field)).expect("base64")
    }

    fn hex(value: &str) -> Vec<u8> {
        (0..value.len())
            .step_by(2)
            .map(|index| u8::from_str_radix(&value[index..index + 2], 16).expect("hex"))
            .collect()
    }

    fn time(value: &str) -> SystemTime {
        let millis = chrono::DateTime::parse_from_rfc3339(value)
            .expect("rfc3339")
            .timestamp_millis();
        SystemTime::UNIX_EPOCH + Duration::from_millis(millis as u64)
    }

    fn environment(vector: &Json) -> Environment {
        Environment::parse(text(vector, "environment")).unwrap_or(Environment::Production)
    }

    fn attestations() -> Vec<Attestation> {
        vectors()
            .iter()
            .filter(|vector| vector["kind"] == "attestation")
            .map(|vector| Attestation {
                id: text(vector, "id").to_owned(),
                object: testdata(text(vector, "attestation_object_file")),
                key_id: b64(vector, "key_id_b64"),
                client_data_hash: if vector["client_data_hash_b64"].is_string() {
                    b64(vector, "client_data_hash_b64")
                } else {
                    sha256(&b64(vector, "client_data_b64")).to_vec()
                },
                app_id: text(vector, "app_id").to_owned(),
                environment: environment(vector),
                public_key: vector["public_key_x962_hex"]
                    .as_str()
                    .map(hex)
                    .unwrap_or_default(),
                now: time(text(vector, "now")),
            })
            .collect()
    }

    fn assertions() -> Vec<Assertion> {
        vectors()
            .iter()
            .filter(|vector| vector["kind"] == "assertion")
            .map(|vector| Assertion {
                id: text(vector, "id").to_owned(),
                object: testdata(text(vector, "assertion_file")),
                client_data: b64(vector, "client_data_b64"),
                app_id: text(vector, "app_id").to_owned(),
                environment: environment(vector),
                public_key: hex(text(vector, "public_key_x962_hex")),
                previous_counter: vector["previous_counter"].as_u64().unwrap_or(0) as u32,
                expected_counter: vector["expected_counter"]
                    .as_u64()
                    .map(|value| value as u32),
            })
            .collect()
    }

    fn config(app_id: &str, allow_development: bool) -> AppAttestConfig {
        AppAttestConfig {
            app_ids: vec!["3G5837T29K.com.fluxer".to_owned(), app_id.to_owned()],
            allow_development,
        }
    }

    fn development() -> Attestation {
        attestations()
            .into_iter()
            .find(|vector| vector.id == "veehaitch-ios-14.4-attestation")
            .expect("a development vector")
    }

    fn production() -> Attestation {
        attestations()
            .into_iter()
            .find(|vector| vector.id == "node-app-attest-production-attestation")
            .expect("a production vector")
    }

    fn verify(vector: &Attestation, object: &[u8]) -> Result<AttestedKey, AttestationError> {
        verify_attestation(
            object,
            &vector.key_id,
            &vector.client_data_hash,
            vector.now,
            &config(&vector.app_id, true),
        )
    }

    fn rewrite(object: &[u8], edit: impl FnOnce(&mut Vec<(Value, Value)>)) -> Vec<u8> {
        let mut value: Value = ciborium::de::from_reader(object).expect("cbor");
        let Value::Map(map) = &mut value else {
            panic!("not a map")
        };
        edit(map);
        let mut out = Vec::new();
        ciborium::ser::into_writer(&value, &mut out).expect("encode");
        out
    }

    fn entry<'a>(map: &'a mut [(Value, Value)], key: &str) -> &'a mut Value {
        &mut map
            .iter_mut()
            .find(|(candidate, _)| candidate.as_text() == Some(key))
            .expect("key")
            .1
    }

    fn auth_data(object: &[u8]) -> Vec<u8> {
        let value: Value = ciborium::de::from_reader(object).expect("cbor");
        map_bytes(value.as_map().expect("map"), "authData")
            .expect("authData")
            .to_vec()
    }

    fn flip(bytes: &mut [u8], index: usize) {
        bytes[index] ^= 0x01;
    }

    #[test]
    fn every_real_attestation_verifies_inside_its_validity() {
        let vectors = attestations();
        assert_eq!(vectors.len(), 2);
        for vector in &vectors {
            let attested = verify(vector, &vector.object)
                .unwrap_or_else(|error| panic!("{}: {error:?}", vector.id));
            assert_eq!(attested.environment, vector.environment, "{}", vector.id);
            assert_eq!(attested.app_id, vector.app_id, "{}", vector.id);
            if !vector.public_key.is_empty() {
                assert_eq!(
                    attested.public_key.to_vec(),
                    vector.public_key,
                    "{}",
                    vector.id
                );
            }
            assert_eq!(
                verify(vector, &rewrite(&vector.object, |_| {})),
                Ok(attested),
                "{} re-encoded",
                vector.id
            );
        }
    }

    #[test]
    fn every_real_assertion_verifies_and_returns_its_counter() {
        let vectors = assertions();
        assert_eq!(vectors.len(), 1);
        for vector in &vectors {
            let counter = verify_assertion(
                &vector.object,
                &vector.client_data,
                KeyForAssertion {
                    public_key: &vector.public_key,
                    app_id: &vector.app_id,
                    environment: vector.environment,
                },
                vector.previous_counter,
                &config(&vector.app_id, true),
            )
            .unwrap_or_else(|error| panic!("{}: {error:?}", vector.id));
            assert!(counter > vector.previous_counter, "{}", vector.id);
            if let Some(expected) = vector.expected_counter {
                assert_eq!(counter, expected, "{}", vector.id);
            }
        }
    }

    #[test]
    fn attestation_rejects_the_wrong_app_id_or_disallowed_development() {
        let vector = development();
        let mut wrong_app = config("ABCDEFGHIJ.de.vincent-haupert.apple-appattest-poc", true);
        wrong_app.app_ids.retain(|app_id| app_id != &vector.app_id);
        let check = |config: &AppAttestConfig| {
            verify_attestation(
                &vector.object,
                &vector.key_id,
                &vector.client_data_hash,
                vector.now,
                config,
            )
        };
        assert_eq!(check(&wrong_app), Err(AttestationError::AppId));
        assert_eq!(
            check(&config(&vector.app_id, false)),
            Err(AttestationError::DevelopmentNotAllowed)
        );
        let production = production();
        assert!(
            verify_attestation(
                &production.object,
                &production.key_id,
                &production.client_data_hash,
                production.now,
                &config(&production.app_id, false),
            )
            .is_ok()
        );
    }

    fn assertion_vector() -> Assertion {
        assertions()
            .into_iter()
            .find(|vector| vector.id == "veehaitch-ios-14.4-assertion")
            .expect("an assertion vector")
    }

    fn rewrite_assertion(object: &[u8], field: &str, edit: impl FnOnce(&mut Vec<u8>)) -> Vec<u8> {
        rewrite(object, |map| {
            let Value::Bytes(bytes) = entry(map, field) else {
                panic!("{field}")
            };
            edit(bytes);
        })
    }

    #[test]
    fn assertion_rejects_replays_changes_and_foreign_keys() {
        let vector = assertion_vector();
        let config = config(&vector.app_id, true);
        let key = KeyForAssertion {
            public_key: &vector.public_key,
            app_id: &vector.app_id,
            environment: Environment::Development,
        };
        let check = verify_assertion;
        assert_eq!(
            check(&vector.object, &vector.client_data, key, 0, &config),
            Ok(1)
        );
        assert_eq!(
            check(&vector.object, &vector.client_data, key, 1, &config),
            Err(AssertionError::Counter)
        );
        assert_eq!(
            check(&vector.object, &vector.client_data, key, 7, &config),
            Err(AssertionError::Counter)
        );
        assert_eq!(
            check(&vector.object, b"wurzelpfropg", key, 0, &config),
            Err(AssertionError::Signature)
        );
        for index in [0, 32, 36] {
            let object = rewrite_assertion(&vector.object, "authenticatorData", |data| {
                flip(data, index)
            });
            assert_eq!(
                check(&object, &vector.client_data, key, 0, &config),
                Err(AssertionError::Signature),
                "byte {index}"
            );
        }
        let object = rewrite_assertion(&vector.object, "signature", |signature| {
            let last = signature.len() - 1;
            flip(signature, last);
        });
        assert_eq!(
            check(&object, &vector.client_data, key, 0, &config),
            Err(AssertionError::Signature)
        );
        let object = rewrite_assertion(&vector.object, "authenticatorData", |data| data.push(0));
        assert_eq!(
            check(&object, &vector.client_data, key, 0, &config),
            Err(AssertionError::Malformed)
        );
        let other = production();
        let other_key = verify(&other, &other.object)
            .expect("another key")
            .public_key;
        assert_eq!(
            check(
                &vector.object,
                &vector.client_data,
                KeyForAssertion {
                    public_key: &other_key,
                    ..key
                },
                0,
                &config
            ),
            Err(AssertionError::Signature)
        );
        let other_app = "ABCDEFGHIJ.de.vincent-haupert.apple-appattest-poc";
        assert_eq!(
            check(
                &vector.object,
                &vector.client_data,
                KeyForAssertion {
                    app_id: other_app,
                    ..key
                },
                0,
                &AppAttestConfig {
                    app_ids: vec![other_app.to_owned()],
                    allow_development: true,
                }
            ),
            Err(AssertionError::AppId)
        );
        assert_eq!(
            check(
                &vector.object,
                &vector.client_data,
                key,
                0,
                &AppAttestConfig {
                    app_ids: vec![other_app.to_owned()],
                    allow_development: true,
                }
            ),
            Err(AssertionError::KeyNotAccepted)
        );
        let mut no_development = config.clone();
        no_development.allow_development = false;
        assert_eq!(
            check(&vector.object, &vector.client_data, key, 0, &no_development),
            Err(AssertionError::KeyNotAccepted)
        );
    }

    #[test]
    fn attestation_rejects_a_chain_that_copies_apple_names_under_another_root() {
        let object = testdata("synthetic-untrusted-root-attestation.cbor");
        let key_id = STANDARD
            .decode("G+TdKKtHTrJ9RAqGkJl/1YfvudeyG7u/uHZRWyJct8Y=")
            .expect("key id");
        let config = config("3G5837T29K.com.fluxer", true);
        assert!(check_attested_auth_data(&auth_data(&object), &key_id, &config).is_ok());
        assert_eq!(
            verify_attestation(
                &object,
                &key_id,
                &sha256(b"fake-challenge"),
                SystemTime::UNIX_EPOCH + Duration::from_secs(1_793_836_800),
                &config,
            ),
            Err(AttestationError::UntrustedChain)
        );
    }
}
