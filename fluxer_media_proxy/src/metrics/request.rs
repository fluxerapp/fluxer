// SPDX-License-Identifier: AGPL-3.0-or-later

use super::histogram::Histogram;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum RequestKind {
    Health,
    Metadata,
    Thumbnail,
    Frames,
    AssetImage,
    GuildMemberImage,
    Attachment,
    External,
    Static,
    Themes,
    Upload,
    Other,
}

impl RequestKind {
    pub const ALL: [RequestKind; 12] = [
        Self::Health,
        Self::Metadata,
        Self::Thumbnail,
        Self::Frames,
        Self::AssetImage,
        Self::GuildMemberImage,
        Self::Attachment,
        Self::External,
        Self::Static,
        Self::Themes,
        Self::Upload,
        Self::Other,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Self::Health => "health",
            Self::Metadata => "metadata",
            Self::Thumbnail => "thumbnail",
            Self::Frames => "frames",
            Self::AssetImage => "asset_image",
            Self::GuildMemberImage => "guild_member_image",
            Self::Attachment => "attachment",
            Self::External => "external",
            Self::Static => "static",
            Self::Themes => "themes",
            Self::Upload => "upload",
            Self::Other => "other",
        }
    }
}

pub(super) const REQUEST_KIND_COUNT: usize = RequestKind::ALL.len();

pub struct RequestMetrics {
    pub(super) requests_1xx: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) requests_2xx: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) requests_3xx: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) requests_4xx: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) requests_5xx: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) requests_other: [AtomicU64; REQUEST_KIND_COUNT],
    pub(super) request_duration: Histogram,
    pub(super) request_duration_per_kind: [Histogram; REQUEST_KIND_COUNT],
}

impl RequestMetrics {
    pub(crate) fn new() -> Self {
        Self {
            requests_1xx: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            requests_2xx: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            requests_3xx: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            requests_4xx: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            requests_5xx: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            requests_other: [const { AtomicU64::new(0) }; REQUEST_KIND_COUNT],
            request_duration: Histogram::new(),
            request_duration_per_kind: [const { Histogram::new() }; REQUEST_KIND_COUNT],
        }
    }

    pub fn record_request(&self, kind: RequestKind, status: u16) {
        let series = match status / 100 {
            1 => &self.requests_1xx,
            2 => &self.requests_2xx,
            3 => &self.requests_3xx,
            4 => &self.requests_4xx,
            5 => &self.requests_5xx,
            _ => &self.requests_other,
        };
        series[kind as usize].fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_request_with_duration(&self, kind: RequestKind, status: u16, ms: u64) {
        self.record_request(kind, status);
        self.request_duration.observe(ms);
        self.request_duration_per_kind[kind as usize].observe(ms);
    }
}
