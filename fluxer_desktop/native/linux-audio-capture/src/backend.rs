#![allow(dead_code)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::routing::{PropMap, RoutingRule, SelfIdentity};

pub trait CaptureBridge: Send + Sync {
    fn inventory(&self) -> Vec<PropMap>;
    fn apply(&self, rule: RoutingRule) -> bool;
    fn release(&self);
    fn populate_self_identity(&self, identity: SelfIdentity);
    fn backend_name(&self) -> &'static str;
}

pub trait DirectCapture: Send + Sync {
    fn start(&self, rule: RoutingRule) -> bool;

    fn set_rule(&self, rule: RoutingRule) -> bool;
    fn read(&self) -> Option<CapturedFrame>;
    fn stop(&self);
    fn populate_self_identity(&self, identity: SelfIdentity);

    fn set_screen_audio_sink(
        &self,
        _sink: std::sync::Arc<fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef>,
    ) {
    }

    fn clear_screen_audio_sink(&self) {}
}

pub struct CapturedFrame {
    pub samples: Vec<f32>,
    pub sample_rate: u32,
    pub channels: u32,
    pub timestamp_us: i64,
}
