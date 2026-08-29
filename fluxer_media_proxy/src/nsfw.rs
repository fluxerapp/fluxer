// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::{Engine as _, engine::general_purpose};
use reqwest::Client;
use serde_json::{Value, json};
use std::time::Duration;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Config {
    pub endpoint: String,
    pub threshold: f32,
    pub timeout_ms: u64,
    pub connect_timeout_ms: u64,
}

impl Config {
    pub fn disabled() -> Self {
        Self {
            endpoint: String::new(),
            threshold: 0.85,
            timeout_ms: 5_000,
            connect_timeout_ms: 1_500,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Result {
    pub probability: f32,
    pub is_nsfw: bool,
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum Error {
    #[error("NSFW service is disabled")]
    NsfwDisabled,
    #[error("NSFW service unavailable")]
    NsfwUnavailable,
    #[error("invalid NSFW service response")]
    InvalidResponse,
}

pub fn is_enabled(cfg: &Config) -> bool {
    !cfg.endpoint.is_empty()
}

pub async fn check(
    client: &Client,
    cfg: &Config,
    image_bytes: &[u8],
) -> std::result::Result<Result, Error> {
    let start = crate::metrics::now_ms();
    let result = check_inner(client, cfg, image_bytes).await;
    crate::request_log::record_stage(
        crate::request_log::Stage::Nsfw,
        (crate::metrics::now_ms() - start).max(0) as u64,
    );
    result
}

async fn check_inner(
    client: &Client,
    cfg: &Config,
    image_bytes: &[u8],
) -> std::result::Result<Result, Error> {
    if !is_enabled(cfg) {
        return Err(Error::NsfwDisabled);
    }
    if image_bytes.is_empty() {
        return Err(Error::NsfwUnavailable);
    }
    let body = json!({
        "base64_data": general_purpose::STANDARD.encode(image_bytes),
    });
    let url = format!("{}/predict/image", trim_trailing_slash(&cfg.endpoint));
    let response = client
        .post(url)
        .timeout(Duration::from_millis(cfg.timeout_ms))
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .json(&body)
        .send()
        .await
        .map_err(|_| Error::NsfwUnavailable)?;
    if !response.status().is_success() {
        return Err(Error::NsfwUnavailable);
    }
    let bytes = response.bytes().await.map_err(|_| Error::NsfwUnavailable)?;
    let probability = parse_probability(&bytes)?;
    Ok(Result {
        probability,
        is_nsfw: probability >= cfg.threshold,
    })
}

pub async fn check_buffers(
    client: &Client,
    cfg: &Config,
    frames: &[Vec<u8>],
) -> std::result::Result<Result, Error> {
    let start = crate::metrics::now_ms();
    let result = check_buffers_inner(client, cfg, frames).await;
    crate::request_log::record_stage(
        crate::request_log::Stage::Nsfw,
        (crate::metrics::now_ms() - start).max(0) as u64,
    );
    result
}

async fn check_buffers_inner(
    client: &Client,
    cfg: &Config,
    frames: &[Vec<u8>],
) -> std::result::Result<Result, Error> {
    if !is_enabled(cfg) {
        return Err(Error::NsfwDisabled);
    }
    if frames.is_empty() {
        return Err(Error::NsfwUnavailable);
    }
    if frames.len() == 1 {
        return check_inner(client, cfg, &frames[0]).await;
    }
    if frames.iter().any(Vec::is_empty) {
        return Err(Error::NsfwUnavailable);
    }
    let images = frames
        .iter()
        .map(|frame| json!({ "base64_data": general_purpose::STANDARD.encode(frame) }))
        .collect::<Vec<_>>();
    let body = json!({ "images": images });
    let url = format!("{}/predict/images", trim_trailing_slash(&cfg.endpoint));
    let response = client
        .post(url)
        .timeout(Duration::from_millis(cfg.timeout_ms.saturating_mul(3)))
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .json(&body)
        .send()
        .await
        .map_err(|_| Error::NsfwUnavailable)?;
    if !response.status().is_success() {
        return Err(Error::NsfwUnavailable);
    }
    let bytes = response.bytes().await.map_err(|_| Error::NsfwUnavailable)?;
    let probabilities = parse_batch_probabilities(&bytes)?;
    Ok(verdict_from_frame_probabilities(
        &probabilities,
        cfg.threshold,
    ))
}

fn trim_trailing_slash(value: &str) -> &str {
    value.trim_end_matches('/')
}

pub fn parse_probability(body: &[u8]) -> std::result::Result<f32, Error> {
    let value: Value = serde_json::from_slice(body).map_err(|_| Error::InvalidResponse)?;
    let object = value.as_object().ok_or(Error::InvalidResponse)?;
    for key in ["nsfw_probability", "score", "probability", "nsfw"] {
        if let Some(value) = object.get(key)
            && let Some(score) = number_as_f32(value)
        {
            return Ok(score.clamp(0.0, 1.0));
        }
    }
    Err(Error::InvalidResponse)
}

pub const CORROBORATING_FRAMES_REQUIRED: usize = 2;

pub fn parse_batch_probabilities(body: &[u8]) -> std::result::Result<Vec<f32>, Error> {
    let value: Value = serde_json::from_slice(body).map_err(|_| Error::InvalidResponse)?;
    let predictions = value
        .get("predictions")
        .and_then(Value::as_array)
        .ok_or(Error::InvalidResponse)?;
    let mut out = Vec::with_capacity(predictions.len());
    for item in predictions {
        let Some(object) = item.as_object() else {
            continue;
        };
        let mut frame = None;
        for key in ["nsfw_probability", "score", "probability", "nsfw"] {
            if let Some(score) = object.get(key).and_then(number_as_f32) {
                let score = score.clamp(0.0, 1.0);
                frame = Some(frame.map_or(score, |current: f32| current.max(score)));
            }
        }
        if let Some(frame) = frame {
            out.push(frame);
        }
    }
    Ok(out)
}

pub fn verdict_from_frame_probabilities(probabilities: &[f32], threshold: f32) -> Result {
    let max = probabilities.iter().copied().fold(0.0f32, f32::max);
    let over = probabilities
        .iter()
        .filter(|probability| **probability >= threshold)
        .count();
    let required = if probabilities.len() >= CORROBORATING_FRAMES_REQUIRED {
        CORROBORATING_FRAMES_REQUIRED
    } else {
        1
    };
    Result {
        probability: max,
        is_nsfw: over >= required,
    }
}

fn number_as_f32(value: &Value) -> Option<f32> {
    value.as_f64().map(|v| v as f32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_batch_probabilities_reads_every_frame() {
        let parsed = parse_batch_probabilities(
            br#"{"predictions":[{"nsfw_probability":0.1},{"nsfw_probability":0.7},{"nsfw_probability":0.3}]}"#,
        )
        .unwrap();
        assert_eq!(3, parsed.len());
        assert!((parsed[1] - 0.7).abs() < 0.001);
        assert!(
            parse_batch_probabilities(br#"{"predictions":[]}"#)
                .unwrap()
                .is_empty()
        );
        assert_eq!(
            Err(Error::InvalidResponse),
            parse_batch_probabilities(br#"{}"#)
        );
        assert_eq!(
            Err(Error::InvalidResponse),
            parse_batch_probabilities(b"not-json")
        );
    }

    #[test]
    fn single_high_frame_does_not_flag_a_multi_frame_scan() {
        let verdict = verdict_from_frame_probabilities(&[0.02, 0.99, 0.03], 0.95);
        assert!(!verdict.is_nsfw);
        assert!((verdict.probability - 0.99).abs() < 0.001);
    }

    #[test]
    fn two_high_frames_flag_a_multi_frame_scan() {
        assert!(verdict_from_frame_probabilities(&[0.02, 0.99, 0.97], 0.95).is_nsfw);
    }

    #[test]
    fn a_lone_frame_still_flags_on_its_own() {
        assert!(verdict_from_frame_probabilities(&[0.99], 0.95).is_nsfw);
        assert!(!verdict_from_frame_probabilities(&[0.94], 0.95).is_nsfw);
    }

    #[test]
    fn an_empty_scan_never_flags() {
        let verdict = verdict_from_frame_probabilities(&[], 0.95);
        assert!(!verdict.is_nsfw);
        assert!((verdict.probability - 0.0).abs() < 0.001);
    }

    #[test]
    fn frames_at_the_threshold_count_as_over() {
        assert!(verdict_from_frame_probabilities(&[0.95, 0.95], 0.95).is_nsfw);
    }

    #[test]
    fn disabled_when_endpoint_empty() {
        let cfg = Config::disabled();
        assert!(!is_enabled(&cfg));
    }

    #[test]
    fn trim_trailing_slash_matches_service_urls() {
        assert_eq!("http://x", trim_trailing_slash("http://x"));
        assert_eq!("http://x", trim_trailing_slash("http://x/"));
        assert_eq!("http://x", trim_trailing_slash("http://x///"));
        assert_eq!("", trim_trailing_slash("/"));
    }

    #[test]
    fn parse_probability_accepts_known_shapes() {
        assert!((parse_probability(br#"{"nsfw_probability":0.42}"#).unwrap() - 0.42).abs() < 0.001);
        assert!((parse_probability(br#"{"score":0.99}"#).unwrap() - 0.99).abs() < 0.001);
        assert!((parse_probability(br#"{"probability":1}"#).unwrap() - 1.0).abs() < 0.001);
        assert!((parse_probability(br#"{"nsfw":0}"#).unwrap() - 0.0).abs() < 0.001);
        assert_eq!(Err(Error::InvalidResponse), parse_probability(br#"{}"#));
        assert_eq!(Err(Error::InvalidResponse), parse_probability(b"not-json"));
    }

    #[test]
    fn probability_clamped_to_unit_interval() {
        assert!((parse_probability(br#"{"score":2.5}"#).unwrap() - 1.0).abs() < 0.001);
        assert!((parse_probability(br#"{"score":-0.3}"#).unwrap() - 0.0).abs() < 0.001);
    }
}
