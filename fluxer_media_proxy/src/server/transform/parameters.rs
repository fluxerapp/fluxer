// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::AssetExtension,
    image_quality::ImageQuality,
    image_transform::EncodeEffort,
    media_limits::MediaLimits,
    server::{
        params::{
            animated_param, bool_param, explicit_output_format, parse_effort,
            parse_optional_dimension_param,
        },
        response::error::text_with_reason,
    },
};
use axum::{http::StatusCode, response::Response};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TransformRoute {
    Attachment,
    External,
    Stored,
    Asset,
}

#[derive(Clone, Copy)]
pub(in crate::server) struct ValidatedTransformParameters {
    pub(in crate::server) explicit_format: Option<AssetExtension>,
    pub(in crate::server) width: Option<u32>,
    pub(in crate::server) height: Option<u32>,
    pub(in crate::server) quality: Option<ImageQuality>,
    pub(in crate::server) animated: bool,
    pub(in crate::server) effort: Option<EncodeEffort>,
    pub(in crate::server) requested_download: bool,
    pub(in crate::server) has_transform_request: bool,
}

#[derive(Clone, Copy, Debug)]
pub(in crate::server) enum TransformParameterError {
    Format,
    Width,
    Height,
}

pub(in crate::server) fn validate_transform_parameters(
    params: &HashMap<String, String>,
    limits: &MediaLimits,
    route: TransformRoute,
) -> Result<ValidatedTransformParameters, TransformParameterError> {
    let explicit_format =
        explicit_output_format(params).map_err(|()| TransformParameterError::Format)?;
    let width = parse_optional_dimension_param(params, "width", limits)
        .map_err(|()| TransformParameterError::Width)?;
    let height = parse_optional_dimension_param(params, "height", limits)
        .map_err(|()| TransformParameterError::Height)?;
    let animated = animated_param(params, false);
    let effort = (route == TransformRoute::Attachment)
        .then(|| parse_effort(params))
        .flatten();
    Ok(ValidatedTransformParameters {
        explicit_format,
        width,
        height,
        quality: params
            .get("quality")
            .map(|raw| ImageQuality::parse_lenient(raw)),
        animated,
        effort,
        requested_download: bool_param(params, "download", false),
        has_transform_request: params.contains_key("width")
            || params.contains_key("height")
            || params.contains_key("format")
            || params.contains_key("quality")
            || animated,
    })
}

pub(in crate::server) fn transform_parameter_error_response(
    error: TransformParameterError,
) -> Response {
    let reason = match error {
        TransformParameterError::Format => "transform_format_invalid",
        TransformParameterError::Width => "transform_width_invalid",
        TransformParameterError::Height => "transform_height_invalid",
    };
    text_with_reason(StatusCode::BAD_REQUEST, "Bad Request", reason)
}

impl ValidatedTransformParameters {
    pub(in crate::server) fn wants_cover_crop(&self) -> bool {
        self.width.is_some() && self.height.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn params(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(key, value)| ((*key).to_owned(), (*value).to_owned()))
            .collect()
    }

    fn validate(
        pairs: &[(&str, &str)],
        route: TransformRoute,
    ) -> Result<ValidatedTransformParameters, TransformParameterError> {
        validate_transform_parameters(&params(pairs), &MediaLimits::default_from_config(), route)
    }

    #[test]
    fn invalid_formats_and_dimensions_are_the_only_rejections() {
        assert!(matches!(
            validate(&[("format", "bogus")], TransformRoute::Attachment),
            Err(TransformParameterError::Format)
        ));
        assert!(matches!(
            validate(&[("width", "0")], TransformRoute::Attachment),
            Err(TransformParameterError::Width)
        ));
        assert!(matches!(
            validate(&[("height", "")], TransformRoute::Attachment),
            Err(TransformParameterError::Height)
        ));
        let lenient = validate(
            &[
                ("quality", "bogus"),
                ("animated", "yes"),
                ("download", "maybe"),
                ("effort", "bogus"),
            ],
            TransformRoute::Attachment,
        )
        .expect("every remaining parameter stays lenient");
        assert_eq!(Some(ImageQuality::High), lenient.quality);
        assert!(!lenient.animated);
        assert!(!lenient.requested_download);
        assert_eq!(None, lenient.effort);
        assert!(lenient.has_transform_request);
    }

    #[test]
    fn effort_is_honoured_only_on_the_attachment_route() {
        let attachment = validate(&[("effort", "3")], TransformRoute::Attachment)
            .expect("attachment effort parses");
        assert_eq!(Some(3), attachment.effort.map(EncodeEffort::get));
        assert!(!attachment.has_transform_request);
        for route in [
            TransformRoute::External,
            TransformRoute::Stored,
            TransformRoute::Asset,
        ] {
            let other = validate(&[("effort", "3")], route).expect("effort is ignored elsewhere");
            assert_eq!(None, other.effort);
        }
    }

    #[test]
    fn the_animatd_typo_is_no_longer_an_alias_for_animated() {
        let aliased = validate(&[("animatd", "true")], TransformRoute::External)
            .expect("an unknown parameter is ignored");
        assert!(!aliased.animated);
        assert!(!aliased.has_transform_request);
        let requested = validate(&[("animated", "true")], TransformRoute::External)
            .expect("the animated parameter parses");
        assert!(requested.animated);
        assert!(requested.has_transform_request);
    }
}
