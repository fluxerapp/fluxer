// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ImageQuality {
    Low,
    High,
    Lossless,
    Auto,
}

impl ImageQuality {
    pub fn parse_lenient(raw: &str) -> Self {
        match raw {
            "low" => Self::Low,
            "lossless" => Self::Lossless,
            "auto" => Self::Auto,
            _ => Self::High,
        }
    }

    pub const fn cache_serialization(self) -> &'static str {
        match self {
            Self::Low => "low",
            Self::High => "high",
            Self::Lossless => "lossless",
            Self::Auto => "auto",
        }
    }

    pub const fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }

    pub const fn resolve_static(self) -> ResolvedImageQuality {
        match self {
            Self::Low => ResolvedImageQuality::Low,
            Self::High | Self::Auto => ResolvedImageQuality::High,
            Self::Lossless => ResolvedImageQuality::Lossless,
        }
    }
}

impl fmt::Display for ImageQuality {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.cache_serialization())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResolvedImageQuality {
    Low,
    High,
    Lossless,
}

impl ResolvedImageQuality {
    pub const fn encoder_quality(self) -> u8 {
        match self {
            Self::Low => 65,
            Self::High => 85,
            Self::Lossless => 100,
        }
    }

    pub const fn is_lossless(self) -> bool {
        matches!(self, Self::Lossless)
    }

    pub const fn default_effort(self, animated: bool) -> u8 {
        if animated || matches!(self, Self::Low) {
            2
        } else {
            4
        }
    }
}

impl fmt::Display for ResolvedImageQuality {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Self::Low => "low",
            Self::High => "high",
            Self::Lossless => "lossless",
        };
        formatter.write_str(value)
    }
}

impl From<ResolvedImageQuality> for ImageQuality {
    fn from(value: ResolvedImageQuality) -> Self {
        match value {
            ResolvedImageQuality::Low => Self::Low,
            ResolvedImageQuality::High => Self::High,
            ResolvedImageQuality::Lossless => Self::Lossless,
        }
    }
}
