// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants,
    metrics::nsfw::NSFWMetrics,
    request_log::{Stage, timed_stage},
};
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;
use serde_json::{Value, json};
use std::{sync::Arc, time::Duration};
use thiserror::Error;

pub const NSFW_MAX_FRAME_BYTES: usize = 1024 * 1024;
pub const NSFW_MAX_FRAMES: usize = 3;

const NSFW_CONNECT_TIMEOUT_MS: u64 = 1_500;
const NSFW_POOL_IDLE_TIMEOUT_SECONDS: u64 = 30;
const NSFW_REQUEST_TIMEOUT_MS: u64 = 5_000;
const NSFW_BATCH_REQUEST_TIMEOUT_MS: u64 = NSFW_REQUEST_TIMEOUT_MS * 3;
const NSFW_PROBABILITY_KEYS: [&str; 4] = ["nsfw_probability", "score", "probability", "nsfw"];

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NSFWThreshold(f32);

impl NSFWThreshold {
    pub fn new(value: f32) -> Result<Self, InvalidNSFWThreshold> {
        if !value.is_finite() || !(0.0..=1.0).contains(&value) {
            return Err(InvalidNSFWThreshold);
        }
        Ok(Self(value))
    }

    fn classifies_as_nsfw(self, probability: f32) -> bool {
        probability >= self.0
    }
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
#[error("NSFW threshold must be finite and between zero and one")]
pub struct InvalidNSFWThreshold;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NSFWPolicy {
    Disabled,
    Enabled { threshold: NSFWThreshold },
}

impl NSFWPolicy {
    pub fn enabled(threshold: f32) -> Result<Self, InvalidNSFWThreshold> {
        Ok(Self::Enabled {
            threshold: NSFWThreshold::new(threshold)?,
        })
    }

    pub fn scan_threshold(self, scan_eligible: bool) -> Option<NSFWThreshold> {
        if !scan_eligible {
            return None;
        }
        match self {
            Self::Disabled => None,
            Self::Enabled { threshold } => Some(threshold),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct NSFWEndpoint(String);

impl NSFWEndpoint {
    fn new(raw: &str) -> Option<Self> {
        let trimmed = trim_trailing_slash(raw);
        if trimmed.is_empty() {
            return None;
        }
        Some(Self(trimmed.to_owned()))
    }

    fn image_url(&self) -> String {
        format!("{}/predict/image", self.0)
    }

    fn images_url(&self) -> String {
        format!("{}/predict/images", self.0)
    }
}

fn trim_trailing_slash(value: &str) -> &str {
    value.trim_end_matches('/')
}

#[derive(Debug)]
pub struct NSFWScanRequest {
    threshold: NSFWThreshold,
    frames: Vec<Vec<u8>>,
}

impl NSFWScanRequest {
    pub fn new(
        threshold: NSFWThreshold,
        frames: Vec<Vec<u8>>,
    ) -> Result<Self, InvalidNSFWScanRequest> {
        validate_nsfw_frames(&frames)?;
        Ok(Self { threshold, frames })
    }
}

fn validate_nsfw_frames(frames: &[Vec<u8>]) -> Result<(), InvalidNSFWScanRequest> {
    if frames.is_empty() {
        return Err(InvalidNSFWScanRequest::Empty);
    }
    if frames.len() > NSFW_MAX_FRAMES {
        return Err(InvalidNSFWScanRequest::TooManyFrames {
            count: frames.len(),
        });
    }
    for (index, frame) in frames.iter().enumerate() {
        if frame.is_empty() {
            return Err(InvalidNSFWScanRequest::EmptyFrame { index });
        }
        if frame.len() > NSFW_MAX_FRAME_BYTES {
            return Err(InvalidNSFWScanRequest::FrameTooLarge {
                index,
                bytes: frame.len(),
            });
        }
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum InvalidNSFWScanRequest {
    #[error("NSFW scan request has no images")]
    Empty,
    #[error("NSFW scan request has {count} images, exceeding the limit of {NSFW_MAX_FRAMES}")]
    TooManyFrames { count: usize },
    #[error("NSFW scan request image {index} is empty")]
    EmptyFrame { index: usize },
    #[error(
        "NSFW scan request image {index} has {bytes} bytes, exceeding the limit of {NSFW_MAX_FRAME_BYTES}"
    )]
    FrameTooLarge { index: usize, bytes: usize },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NSFWClassification {
    pub probability: f32,
    pub is_nsfw: bool,
}

impl NSFWClassification {
    pub fn not_scanned() -> Self {
        Self {
            probability: 0.0,
            is_nsfw: false,
        }
    }
}

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum NSFWError {
    #[error("NSFW service is disabled")]
    Disabled,
    #[error("NSFW service unavailable")]
    Unavailable,
    #[error("invalid NSFW service response")]
    InvalidResponse,
}

#[derive(Debug, Error)]
pub enum NSFWClientError {
    #[error(transparent)]
    InvalidThreshold(#[from] InvalidNSFWThreshold),
    #[error("NSFW HTTP client could not be built")]
    Transport(#[from] reqwest::Error),
}

#[derive(Clone)]
struct NSFWTransport {
    http: reqwest::Client,
    endpoint: NSFWEndpoint,
}

impl NSFWTransport {
    async fn check(&self, frame: &[u8]) -> Result<f32, NSFWError> {
        let body = json!({ "base64_data": general_purpose::STANDARD.encode(frame) });
        let bytes = self
            .post(
                self.endpoint.image_url(),
                Duration::from_millis(NSFW_REQUEST_TIMEOUT_MS),
                &body,
            )
            .await?;
        parse_probability(&bytes)
    }

    async fn check_buffers(&self, frames: &[Vec<u8>]) -> Result<Vec<f32>, NSFWError> {
        let images = frames
            .iter()
            .map(|frame| json!({ "base64_data": general_purpose::STANDARD.encode(frame) }))
            .collect::<Vec<_>>();
        let body = json!({ "images": images });
        let bytes = self
            .post(
                self.endpoint.images_url(),
                Duration::from_millis(NSFW_BATCH_REQUEST_TIMEOUT_MS),
                &body,
            )
            .await?;
        parse_batch_probabilities(&bytes)
    }

    async fn post(&self, url: String, timeout: Duration, body: &Value) -> Result<Bytes, NSFWError> {
        let response = self
            .http
            .post(url)
            .timeout(timeout)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json")
            .json(body)
            .send()
            .await
            .map_err(|_| NSFWError::Unavailable)?;
        if !response.status().is_success() {
            return Err(NSFWError::Unavailable);
        }
        response.bytes().await.map_err(|_| NSFWError::Unavailable)
    }
}

#[derive(Clone)]
pub struct NSFWClient {
    transport: Option<NSFWTransport>,
    policy: NSFWPolicy,
    metrics: Arc<NSFWMetrics>,
}

impl NSFWClient {
    pub fn new(
        endpoint: &str,
        threshold: f32,
        metrics: Arc<NSFWMetrics>,
    ) -> Result<Self, NSFWClientError> {
        let policy = NSFWPolicy::enabled(threshold)?;
        let Some(endpoint) = NSFWEndpoint::new(endpoint) else {
            return Ok(Self {
                transport: None,
                policy: NSFWPolicy::Disabled,
                metrics,
            });
        };
        let http = reqwest::Client::builder()
            .connect_timeout(Duration::from_millis(NSFW_CONNECT_TIMEOUT_MS))
            .pool_idle_timeout(Duration::from_secs(NSFW_POOL_IDLE_TIMEOUT_SECONDS))
            .user_agent(constants::OUTBOUND_USER_AGENT)
            .build()?;
        Ok(Self {
            transport: Some(NSFWTransport { http, endpoint }),
            policy,
            metrics,
        })
    }

    pub fn disabled() -> Self {
        Self {
            transport: None,
            policy: NSFWPolicy::Disabled,
            metrics: Arc::new(NSFWMetrics::new()),
        }
    }

    pub fn policy(&self) -> NSFWPolicy {
        self.policy
    }

    pub fn record_declined_scan(&self) {
        self.metrics.record_disabled();
    }

    pub async fn check_buffers(
        &self,
        request: NSFWScanRequest,
    ) -> Result<NSFWClassification, NSFWError> {
        let outcome = timed_stage(Stage::Nsfw, self.classify(request)).await;
        match outcome {
            Ok(_) => self.metrics.record_success(),
            Err(NSFWError::Disabled) => self.metrics.record_disabled(),
            Err(_) => self.metrics.record_failure(),
        }
        outcome
    }

    async fn classify(&self, request: NSFWScanRequest) -> Result<NSFWClassification, NSFWError> {
        let transport = self.transport.as_ref().ok_or(NSFWError::Disabled)?;
        let NSFWScanRequest { threshold, frames } = request;
        if frames.len() == 1 {
            let probability = transport.check(&frames[0]).await?;
            return Ok(NSFWClassification {
                probability,
                is_nsfw: threshold.classifies_as_nsfw(probability),
            });
        }
        let probabilities = transport.check_buffers(&frames).await?;
        Ok(verdict_from_frame_probabilities(&probabilities, threshold))
    }
}

pub fn parse_probability(body: &[u8]) -> Result<f32, NSFWError> {
    let value: Value = serde_json::from_slice(body).map_err(|_| NSFWError::InvalidResponse)?;
    let object = value.as_object().ok_or(NSFWError::InvalidResponse)?;
    for key in NSFW_PROBABILITY_KEYS {
        if let Some(value) = object.get(key)
            && let Some(score) = number_as_f32(value)
        {
            return Ok(score.clamp(0.0, 1.0));
        }
    }
    Err(NSFWError::InvalidResponse)
}

pub const CORROBORATING_FRAMES_REQUIRED: usize = 2;

pub fn parse_batch_probabilities(body: &[u8]) -> Result<Vec<f32>, NSFWError> {
    let value: Value = serde_json::from_slice(body).map_err(|_| NSFWError::InvalidResponse)?;
    let predictions = value
        .get("predictions")
        .and_then(Value::as_array)
        .ok_or(NSFWError::InvalidResponse)?;
    let mut out = Vec::with_capacity(predictions.len());
    for item in predictions {
        let Some(object) = item.as_object() else {
            continue;
        };
        let mut frame = None;
        for key in NSFW_PROBABILITY_KEYS {
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

pub fn verdict_from_frame_probabilities(
    probabilities: &[f32],
    threshold: NSFWThreshold,
) -> NSFWClassification {
    let max = probabilities.iter().copied().fold(0.0f32, f32::max);
    let over = probabilities
        .iter()
        .filter(|probability| threshold.classifies_as_nsfw(**probability))
        .count();
    let required = if probabilities.len() >= CORROBORATING_FRAMES_REQUIRED {
        CORROBORATING_FRAMES_REQUIRED
    } else {
        1
    };
    NSFWClassification {
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
    use crate::metrics::Metrics;

    fn threshold() -> NSFWThreshold {
        NSFWThreshold::new(0.85).expect("0.85 is a valid threshold")
    }

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
            Err(NSFWError::InvalidResponse),
            parse_batch_probabilities(br#"{}"#)
        );
        assert_eq!(
            Err(NSFWError::InvalidResponse),
            parse_batch_probabilities(b"not-json")
        );
    }

    #[test]
    fn single_high_frame_does_not_flag_a_multi_frame_scan() {
        let verdict = verdict_from_frame_probabilities(
            &[0.02, 0.99, 0.03],
            NSFWThreshold::new(0.95).unwrap(),
        );
        assert!(!verdict.is_nsfw);
        assert!((verdict.probability - 0.99).abs() < 0.001);
    }

    #[test]
    fn two_high_frames_flag_a_multi_frame_scan() {
        assert!(
            verdict_from_frame_probabilities(
                &[0.02, 0.99, 0.97],
                NSFWThreshold::new(0.95).unwrap()
            )
            .is_nsfw
        );
    }

    #[test]
    fn a_lone_frame_still_flags_on_its_own() {
        assert!(
            verdict_from_frame_probabilities(&[0.99], NSFWThreshold::new(0.95).unwrap()).is_nsfw
        );
        assert!(
            !verdict_from_frame_probabilities(&[0.94], NSFWThreshold::new(0.95).unwrap()).is_nsfw
        );
    }

    #[test]
    fn an_empty_scan_never_flags() {
        let verdict = verdict_from_frame_probabilities(&[], NSFWThreshold::new(0.95).unwrap());
        assert!(!verdict.is_nsfw);
        assert!((verdict.probability - 0.0).abs() < 0.001);
    }

    #[test]
    fn frames_at_the_threshold_count_as_over() {
        assert!(
            verdict_from_frame_probabilities(&[0.95, 0.95], NSFWThreshold::new(0.95).unwrap())
                .is_nsfw
        );
    }

    #[test]
    fn disabled_when_endpoint_empty() {
        let client = NSFWClient::new("", 0.85, Arc::new(NSFWMetrics::new())).unwrap();
        assert_eq!(NSFWPolicy::Disabled, client.policy());
        assert_eq!(None, client.policy().scan_threshold(true));
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
        assert_eq!(Err(NSFWError::InvalidResponse), parse_probability(br#"{}"#));
        assert_eq!(
            Err(NSFWError::InvalidResponse),
            parse_probability(b"not-json")
        );
    }

    #[test]
    fn probability_clamped_to_unit_interval() {
        assert!((parse_probability(br#"{"score":2.5}"#).unwrap() - 1.0).abs() < 0.001);
        assert!((parse_probability(br#"{"score":-0.3}"#).unwrap() - 0.0).abs() < 0.001);
    }

    #[test]
    fn a_threshold_outside_the_unit_interval_is_rejected() {
        assert_eq!(Err(InvalidNSFWThreshold), NSFWThreshold::new(1.5));
        assert_eq!(Err(InvalidNSFWThreshold), NSFWThreshold::new(-0.1));
        assert_eq!(Err(InvalidNSFWThreshold), NSFWThreshold::new(f32::NAN));
        assert!(threshold().classifies_as_nsfw(0.85));
        assert!(!threshold().classifies_as_nsfw(0.849));
    }

    #[test]
    fn a_scan_request_bounds_its_frames() {
        assert_eq!(
            InvalidNSFWScanRequest::Empty,
            NSFWScanRequest::new(threshold(), Vec::new()).unwrap_err()
        );
        assert_eq!(
            InvalidNSFWScanRequest::EmptyFrame { index: 1 },
            NSFWScanRequest::new(threshold(), vec![vec![1], Vec::new()]).unwrap_err()
        );
        assert_eq!(
            InvalidNSFWScanRequest::FrameTooLarge {
                index: 0,
                bytes: NSFW_MAX_FRAME_BYTES + 1,
            },
            NSFWScanRequest::new(threshold(), vec![vec![1; NSFW_MAX_FRAME_BYTES + 1]]).unwrap_err()
        );
        assert!(NSFWScanRequest::new(threshold(), vec![vec![1]; NSFW_MAX_FRAMES]).is_ok());
    }

    #[tokio::test]
    async fn a_four_frame_request_is_rejected_before_any_http_call() {
        let metrics = Metrics::new();
        let client = NSFWClient::new("http://127.0.0.1:9/", 0.85, metrics.nsfw()).unwrap();
        assert_eq!(
            InvalidNSFWScanRequest::TooManyFrames { count: 4 },
            NSFWScanRequest::new(threshold(), vec![vec![1]; 4]).unwrap_err()
        );
        let rendered = metrics.render();
        assert!(rendered.contains("fluxer_media_proxy_nsfw_calls_failed_total 0"));

        let reachable = NSFWScanRequest::new(threshold(), vec![vec![1]]).unwrap();
        assert_eq!(
            Err(NSFWError::Unavailable),
            client.check_buffers(reachable).await
        );
        let rendered = metrics.render();
        assert!(rendered.contains("fluxer_media_proxy_nsfw_calls_failed_total 1"));
        assert!(rendered.contains("fluxer_media_proxy_nsfw_calls_ok_total 0"));
    }

    #[tokio::test]
    async fn a_disabled_client_records_a_disabled_call() {
        let metrics = Metrics::new();
        let client = NSFWClient::new("", 0.85, metrics.nsfw()).unwrap();
        let request = NSFWScanRequest::new(threshold(), vec![vec![1]]).unwrap();
        assert_eq!(
            Err(NSFWError::Disabled),
            client.check_buffers(request).await
        );
        assert!(
            metrics
                .render()
                .contains("fluxer_media_proxy_nsfw_calls_disabled_total 1")
        );
    }
}
