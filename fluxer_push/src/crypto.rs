// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use hmac::{Hmac, KeyInit, Mac};
use p256::PublicKey;
use p256::ecdsa::signature::Signer as _;
use p256::ecdsa::{Signature, SigningKey};
use p256::elliptic_curve::sec1::ToEncodedPoint as _;
use p256::pkcs8::DecodePrivateKey as _;
use rand::Rng as _;
use ring::aead::{AES_128_GCM, Aad, LessSafeKey, Nonce, UnboundKey};
use ring::rand::SystemRandom;
use ring::signature::{RSA_PKCS1_SHA256, RsaKeyPair};
use serde_json::Value;
use sha2::Sha256;
use thiserror::Error;

type HmacSha256 = Hmac<Sha256>;

pub const ES256: &str = "ES256";

const SALT_LEN: usize = 16;
const TAG_LEN: usize = 16;
const RECORD_SIZE_LEN: usize = 4;
const KEY_ID_LEN_LEN: usize = 1;
const IKM_LEN: usize = 32;
const CEK_LEN: usize = 16;
const NONCE_LEN: usize = 12;
const SCALAR_LEN: usize = 32;
const UNCOMPRESSED_POINT_LEN: usize = 65;
const UNCOMPRESSED_POINT_TAG: u8 = 0x04;
const PADDING_DELIMITER: u8 = 0x02;
const EPHEMERAL_KEY_TRIES: usize = 4;

#[derive(Debug, Error)]
pub enum CryptoError {
    #[error("subscription key is not base64")]
    InvalidSubscriptionKey,
    #[error("subscription public key is not an uncompressed P-256 point")]
    InvalidPeerKey,
    #[error("private key is not a P-256 key")]
    InvalidP256Key,
    #[error("private key is not an RSA key")]
    InvalidRsaKey,
    #[error("no ephemeral P-256 key could be generated")]
    EphemeralKey,
    #[error("record size {0} is too small to frame a payload")]
    RecordSizeTooSmall(usize),
    #[error("payload does not fit in a {0} byte record")]
    MaxPadExceeded(usize),
    #[error("sealing the payload failed")]
    SealFailed,
    #[error("signing failed")]
    SigningFailed,
}

pub fn decode_subscription_key(value: &str) -> Result<Vec<u8>, CryptoError> {
    let trimmed = value.trim().trim_end_matches('=');
    BASE64_URL_SAFE_NO_PAD
        .decode(trimmed)
        .or_else(|_| BASE64_STANDARD_NO_PAD.decode(trimmed))
        .map_err(|_| CryptoError::InvalidSubscriptionKey)
}

pub fn parse_p256_private_scalar(value: &str) -> Result<SigningKey, CryptoError> {
    let raw = decode_subscription_key(value)?;
    SigningKey::from_slice(&raw).map_err(|_| CryptoError::InvalidP256Key)
}

pub fn parse_p256_pkcs8_pem(pem: &str) -> Result<SigningKey, CryptoError> {
    let secret = p256::SecretKey::from_pkcs8_pem(&expand_escaped_newlines(pem))
        .map_err(|_| CryptoError::InvalidP256Key)?;
    Ok(SigningKey::from(&secret))
}

pub fn parse_rsa_pkcs8_pem(pem: &str) -> Result<RsaKeyPair, CryptoError> {
    let der = pem_to_der(pem)?;
    RsaKeyPair::from_pkcs8(&der).map_err(|_| CryptoError::InvalidRsaKey)
}

pub fn es256_jwt(header: &Value, claims: &Value, key: &SigningKey) -> Result<String, CryptoError> {
    let signing_input = signing_input(header, claims);
    let signature: Signature = key
        .try_sign(signing_input.as_bytes())
        .map_err(|_| CryptoError::SigningFailed)?;
    Ok(format!(
        "{signing_input}.{}",
        BASE64_URL_SAFE_NO_PAD.encode(signature.to_bytes())
    ))
}

pub fn rs256_jwt(header: &Value, claims: &Value, key: &RsaKeyPair) -> Result<String, CryptoError> {
    let signing_input = signing_input(header, claims);
    let mut signature = vec![0u8; key.public().modulus_len()];
    key.sign(
        &RSA_PKCS1_SHA256,
        &SystemRandom::new(),
        signing_input.as_bytes(),
        &mut signature,
    )
    .map_err(|_| CryptoError::SigningFailed)?;
    Ok(format!(
        "{signing_input}.{}",
        BASE64_URL_SAFE_NO_PAD.encode(&signature)
    ))
}

pub fn encrypt_aes128gcm(
    message: &[u8],
    p256dh: &[u8],
    auth: &[u8],
    record_size: usize,
) -> Result<Vec<u8>, CryptoError> {
    let mut rng = rand::rng();
    let mut salt = [0u8; SALT_LEN];
    rng.fill_bytes(&mut salt);
    let local_secret = ephemeral_secret(&mut rng)?;
    seal(message, p256dh, auth, &local_secret, salt, record_size)
}

fn seal(
    message: &[u8],
    p256dh: &[u8],
    auth: &[u8],
    local_secret: &p256::SecretKey,
    salt: [u8; SALT_LEN],
    record_size: usize,
) -> Result<Vec<u8>, CryptoError> {
    if p256dh.len() != UNCOMPRESSED_POINT_LEN || p256dh[0] != UNCOMPRESSED_POINT_TAG {
        return Err(CryptoError::InvalidPeerKey);
    }
    let peer_public =
        PublicKey::from_sec1_bytes(p256dh).map_err(|_| CryptoError::InvalidPeerKey)?;
    let local_point = local_secret.public_key().to_encoded_point(false);
    let local_pub = local_point.as_bytes();

    let shared =
        p256::ecdh::diffie_hellman(local_secret.to_nonzero_scalar(), peer_public.as_affine());
    let mut info = Vec::with_capacity(b"WebPush: info\0".len() + p256dh.len() + local_pub.len());
    info.extend_from_slice(b"WebPush: info\0");
    info.extend_from_slice(p256dh);
    info.extend_from_slice(local_pub);
    let ikm = hkdf_sha256(shared.raw_secret_bytes(), auth, &info, IKM_LEN);
    let cek = hkdf_sha256(&ikm, &salt, b"Content-Encoding: aes128gcm\0", CEK_LEN);
    let nonce = hkdf_sha256(&ikm, &salt, b"Content-Encoding: nonce\0", NONCE_LEN);

    let header_len = SALT_LEN + RECORD_SIZE_LEN + KEY_ID_LEN_LEN + local_pub.len();
    let required = record_size
        .checked_sub(TAG_LEN)
        .and_then(|record_len| record_len.checked_sub(header_len))
        .ok_or(CryptoError::RecordSizeTooSmall(record_size))?;
    let record_size_be = u32::try_from(record_size)
        .map_err(|_| CryptoError::RecordSizeTooSmall(record_size))?
        .to_be_bytes();

    let mut data = Vec::with_capacity(required + TAG_LEN);
    data.extend_from_slice(message);
    data.push(PADDING_DELIMITER);
    if data.len() > required {
        return Err(CryptoError::MaxPadExceeded(record_size));
    }
    data.resize(required, 0);

    let key =
        LessSafeKey::new(UnboundKey::new(&AES_128_GCM, &cek).map_err(|_| CryptoError::SealFailed)?);
    let nonce = Nonce::try_assume_unique_for_key(&nonce).map_err(|_| CryptoError::SealFailed)?;
    key.seal_in_place_append_tag(nonce, Aad::empty(), &mut data)
        .map_err(|_| CryptoError::SealFailed)?;

    let mut body = Vec::with_capacity(header_len + data.len());
    body.extend_from_slice(&salt);
    body.extend_from_slice(&record_size_be);
    body.push(u8::try_from(local_pub.len()).map_err(|_| CryptoError::InvalidP256Key)?);
    body.extend_from_slice(local_pub);
    body.append(&mut data);
    Ok(body)
}

fn ephemeral_secret(rng: &mut impl rand::Rng) -> Result<p256::SecretKey, CryptoError> {
    let mut scalar = [0u8; SCALAR_LEN];
    for _ in 0..EPHEMERAL_KEY_TRIES {
        rng.fill_bytes(&mut scalar);
        if let Ok(secret) = p256::SecretKey::from_slice(&scalar) {
            return Ok(secret);
        }
    }
    Err(CryptoError::EphemeralKey)
}

fn hkdf_sha256(ikm: &[u8], salt: &[u8], info: &[u8], len: usize) -> Vec<u8> {
    let mut extract = HmacSha256::new_from_slice(salt).expect("hmac accepts any key length");
    extract.update(ikm);
    let prk = extract.finalize().into_bytes();

    let mut out = Vec::with_capacity(len + IKM_LEN);
    let mut block = Vec::new();
    let mut counter: u8 = 1;
    while out.len() < len {
        let mut expand = HmacSha256::new_from_slice(&prk).expect("hmac accepts any key length");
        expand.update(&block);
        expand.update(info);
        expand.update(&[counter]);
        block = expand.finalize().into_bytes().to_vec();
        out.extend_from_slice(&block);
        counter = counter.wrapping_add(1);
    }
    out.truncate(len);
    out
}

fn signing_input(header: &Value, claims: &Value) -> String {
    format!("{}.{}", encode_segment(header), encode_segment(claims))
}

fn encode_segment(value: &Value) -> String {
    BASE64_URL_SAFE_NO_PAD.encode(serde_json::to_vec(value).expect("a json value serialises"))
}

fn expand_escaped_newlines(value: &str) -> String {
    value.replace("\\n", "\n")
}

fn pem_to_der(pem: &str) -> Result<Vec<u8>, CryptoError> {
    let normalized = expand_escaped_newlines(pem);
    let body: String = normalized
        .lines()
        .filter(|line| !line.trim().starts_with("-----"))
        .flat_map(|line| line.chars().filter(|c| !c.is_ascii_whitespace()))
        .collect();
    BASE64_STANDARD
        .decode(body)
        .map_err(|_| CryptoError::InvalidRsaKey)
}
