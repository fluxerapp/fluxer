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
            metrics: Arc::new(NSFWMetrics::default()),
        }
    }

    pub fn policy(&self) -> NSFWPolicy {
        self.policy
    }

    pub fn record_declined_scan(&self) {
        self.metrics.nsfw_calls_disabled.inc();
    }

    pub async fn check_buffers(
        &self,
        request: NSFWScanRequest,
    ) -> Result<NSFWClassification, NSFWError> {
        let outcome = timed_stage(Stage::Nsfw, self.classify(request)).await;
        match outcome {
            Ok(_) => self.metrics.nsfw_calls_ok.inc(),
            Err(NSFWError::Disabled) => self.metrics.nsfw_calls_disabled.inc(),
            Err(_) => self.metrics.nsfw_calls_failed.inc(),
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
