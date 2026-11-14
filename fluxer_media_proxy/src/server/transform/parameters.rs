// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::AssetExtension,
    image_quality::ImageQuality,
    image_transform::EncodeEffort,
    media_limits::MediaLimits,
    server::{
        params::{
            animated_param, bool_param, explicit_output_format, parse_effort,
            parse_optional_dimension_param, requests_transform,
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
        has_transform_request: requests_transform(params),
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
