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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lenient_parsing_keeps_the_v1_query_contract() {
        assert_eq!(ImageQuality::Low, ImageQuality::parse_lenient("low"));
        assert_eq!(ImageQuality::High, ImageQuality::parse_lenient("high"));
        assert_eq!(
            ImageQuality::Lossless,
            ImageQuality::parse_lenient("lossless")
        );
        assert_eq!(ImageQuality::Auto, ImageQuality::parse_lenient("auto"));
        assert_eq!(ImageQuality::High, ImageQuality::parse_lenient("nonsense"));
        assert_eq!(ImageQuality::High, ImageQuality::parse_lenient(""));
        assert_eq!(ImageQuality::High, ImageQuality::parse_lenient("LOW"));
    }

    #[test]
    fn encoder_numbers_match_the_v1_quality_table() {
        assert_eq!(65, ResolvedImageQuality::Low.encoder_quality());
        assert_eq!(85, ResolvedImageQuality::High.encoder_quality());
        assert_eq!(100, ResolvedImageQuality::Lossless.encoder_quality());
        assert!(ResolvedImageQuality::Lossless.is_lossless());
        assert!(!ResolvedImageQuality::High.is_lossless());
        assert_eq!(
            ResolvedImageQuality::High,
            ImageQuality::Auto.resolve_static()
        );
        assert!(ImageQuality::Auto.is_auto());
        assert!(!ImageQuality::High.is_auto());
    }

    #[test]
    fn serialization_round_trips_through_the_cache_key_alphabet() {
        for quality in [
            ImageQuality::Low,
            ImageQuality::High,
            ImageQuality::Lossless,
            ImageQuality::Auto,
        ] {
            assert_eq!(
                quality,
                ImageQuality::parse_lenient(quality.cache_serialization())
            );
            assert_eq!(quality.cache_serialization(), quality.to_string());
        }
        for resolved in [
            ResolvedImageQuality::Low,
            ResolvedImageQuality::High,
            ResolvedImageQuality::Lossless,
        ] {
            assert_eq!(
                ImageQuality::from(resolved).cache_serialization(),
                resolved.to_string()
            );
        }
    }
}
