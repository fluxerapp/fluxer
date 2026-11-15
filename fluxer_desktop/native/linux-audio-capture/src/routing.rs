#![allow(dead_code)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{HashMap, HashSet};

pub type PropMap = HashMap<String, String>;
pub type PropPattern = PropMap;

pub const MEDIA_CLASS_PLAYBACK_STREAM: &str = "Stream/Output/Audio";
const TARGET_OBJECTS_PATTERN_KEY: &str = "fluxer.target.objects";
const DISPLAY_PATTERN_PREFIX: &str = "fluxer.display.";

#[derive(Debug, Default, Clone)]
pub struct SelfIdentity {
    pub pids: HashSet<String>,
    pub binaries: HashSet<String>,
    pub display_names: HashSet<String>,
    pub display_prefixes: Vec<String>,
}

impl SelfIdentity {
    pub fn add_pid(&mut self, pid: impl Into<String>) {
        let value = pid.into();
        if value.is_empty() {
            return;
        }
        self.pids.insert(value);
    }

    pub fn add_binary(&mut self, name: impl Into<String>) {
        let value = name.into();
        if value.is_empty() {
            return;
        }
        self.binaries.insert(value);
    }

    pub fn add_display_name(&mut self, name: impl Into<String>) {
        let value = name.into();
        if value.is_empty() {
            return;
        }
        self.display_names.insert(value);
    }

    pub fn add_display_prefix(&mut self, prefix: impl Into<String>) {
        let value = prefix.into();
        if value.is_empty() {
            return;
        }
        self.display_prefixes.push(value);
    }

    pub fn matches(&self, properties: &PropMap) -> bool {
        if let Some(raw) = properties.get("application.process.id")
            && self.pids.contains(raw)
        {
            return true;
        }
        if let Some(raw) = properties.get("pipewire.sec.pid")
            && self.pids.contains(raw)
        {
            return true;
        }
        if let Some(raw) = properties.get("application.process.binary")
            && contains_case_insensitive(&self.binaries, raw)
        {
            return true;
        }
        for key in [
            "application.name",
            "node.name",
            "node.nick",
            "node.description",
        ] {
            if let Some(raw) = properties.get(key)
                && self.matches_display_identity(raw)
            {
                return true;
            }
        }
        false
    }

    fn matches_display_identity(&self, raw: &str) -> bool {
        contains_case_insensitive(&self.binaries, raw)
            || contains_case_insensitive(&self.display_names, raw)
            || self
                .display_prefixes
                .iter()
                .any(|prefix| starts_with_case_insensitive(raw, prefix))
    }
}

fn contains_case_insensitive(values: &HashSet<String>, needle: &str) -> bool {
    values
        .iter()
        .any(|candidate| candidate.eq_ignore_ascii_case(needle))
}

fn starts_with_case_insensitive(value: &str, prefix: &str) -> bool {
    value
        .get(..prefix.len())
        .is_some_and(|head| head.eq_ignore_ascii_case(prefix))
}

#[derive(Debug, Default, Clone)]
pub struct RoutingRule {
    pub include_when: Vec<PropPattern>,
    pub never_when: Vec<PropPattern>,
    pub pin_target_for: Vec<PropPattern>,
    pub skip_hardware_devices: bool,
    pub only_audio_sinks: bool,
    pub only_default_audio_sink: bool,
}

pub fn matches_pattern(candidate: &PropMap, expected: &PropPattern) -> bool {
    for (key, value) in expected {
        if key.starts_with(DISPLAY_PATTERN_PREFIX) {
            continue;
        }
        if key == TARGET_OBJECTS_PATTERN_KEY {
            if !matches_target_object(candidate, value) {
                return false;
            }
            continue;
        }
        match candidate.get(key) {
            Some(actual) if actual == value => {}
            _ => return false,
        }
    }
    true
}

fn matches_target_object(candidate: &PropMap, expected_values: &str) -> bool {
    let Some(actual) = candidate
        .get("target.object")
        .or_else(|| candidate.get("node.target"))
    else {
        return false;
    };
    expected_values
        .split('\n')
        .filter(|value| !value.is_empty())
        .any(|expected| actual == expected)
}

pub fn matches_any(candidate: &PropMap, patterns: &[PropPattern]) -> bool {
    patterns.iter().any(|p| matches_pattern(candidate, p))
}

pub fn should_route_node(
    id: u32,
    properties: &PropMap,
    rule: &RoutingRule,
    default_sink_name: &str,
    default_sink_target_id: &str,
    sink_global_id: u32,
    self_identity: &SelfIdentity,
) -> bool {
    if id == sink_global_id {
        return false;
    }

    if self_identity.matches(properties) {
        return false;
    }
    if matches_any(properties, &rule.never_when) {
        return false;
    }
    if rule.skip_hardware_devices && properties.contains_key("device.id") {
        return false;
    }

    let Some(class) = properties.get("media.class") else {
        return false;
    };
    if class != MEDIA_CLASS_PLAYBACK_STREAM {
        return false;
    }

    if !rule.include_when.is_empty() {
        return matches_any(properties, &rule.include_when);
    }

    if rule.only_audio_sinks {
        return !rule.only_default_audio_sink
            || targets_default_sink(properties, default_sink_name, default_sink_target_id);
    }

    false
}

pub fn targets_default_sink(
    properties: &PropMap,
    default_sink_name: &str,
    default_sink_target_id: &str,
) -> bool {
    if default_sink_name.is_empty() && default_sink_target_id.is_empty() {
        return true;
    }
    let Some(target) = properties
        .get("target.object")
        .or_else(|| properties.get("node.target"))
    else {
        return true;
    };
    (!default_sink_name.is_empty() && target == default_sink_name)
        || (!default_sink_target_id.is_empty() && target == default_sink_target_id)
}

pub fn parse_default_sink_name(blob: &str) -> String {
    let trimmed = blob.trim();
    if !trimmed.starts_with('{') {
        return String::new();
    }
    let bytes = trimmed.as_bytes();
    let mut i = 1usize;
    while i < bytes.len() {
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() || bytes[i] != b'"' {
            return String::new();
        }

        i += 1;
        let key_start = i;
        while i < bytes.len() && bytes[i] != b'"' {
            if bytes[i] == b'\\' {
                i += 2;
            } else {
                i += 1;
            }
        }
        if i >= bytes.len() {
            return String::new();
        }
        let key = &trimmed[key_start..i];
        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() || bytes[i] != b':' {
            return String::new();
        }
        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        if i >= bytes.len() {
            return String::new();
        }
        if bytes[i] != b'"' {
            let mut depth = 0usize;
            while i < bytes.len() {
                match bytes[i] {
                    b'{' | b'[' => depth += 1,
                    b'}' | b']' => {
                        if depth == 0 {
                            return String::new();
                        }
                        depth -= 1;
                    }
                    b',' if depth == 0 => break,
                    _ => {}
                }
                i += 1;
            }
            if i < bytes.len() && bytes[i] == b',' {
                i += 1;
                continue;
            }
            return String::new();
        }
        i += 1;
        let val_start = i;
        let mut buf = String::new();
        while i < bytes.len() && bytes[i] != b'"' {
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                let escaped = bytes[i + 1];
                buf.push(escaped as char);
                i += 2;
            } else {
                buf.push(bytes[i] as char);
                i += 1;
            }
        }
        if i >= bytes.len() {
            return String::new();
        }
        if key == "name" {
            if buf.len() == i - val_start {
                return trimmed[val_start..i].to_string();
            }
            return buf;
        }
        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i < bytes.len() && bytes[i] == b',' {
            i += 1;
        }
    }
    String::new()
}
