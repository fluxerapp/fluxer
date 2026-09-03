// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::server::{
    external::{ExternalFetchError, map_internal_metadata_upstream_status},
    response::error::{canonical_reason_str, text_with_reason, text_with_source},
};
use axum::{http::StatusCode, response::Response};

#[derive(Debug)]
pub(in crate::server) enum MediaFailure {
    MediaInputUnsupportedType,
    MediaInputMissingField,
    MetadataBase64TooLarge,
    MetadataBase64Decode,
    MetadataDecodedTooLarge,
    MetadataUploadRead,
    MetadataS3Read,
    ExternalSourceBlocked,
    ExternalSourcePayloadTooLarge,
    ExternalSourceBufferBudgetExhausted,
    ExternalSourceBufferAllocationFailed,
    ExternalSourceUpstreamStatus(StatusCode),
    ExternalSourceFetchFailed,
    MetadataSvgRasterizeTimeout { detail: String },
    MetadataSvgRasterizeFailed { detail: String },
    MetadataExtractionFailed { detail: String },
}

impl MediaFailure {
    pub(in crate::server) fn code(&self) -> &'static str {
        match self {
            Self::MediaInputUnsupportedType => "media_input_unsupported_type",
            Self::MediaInputMissingField => "media_input_missing_field",
            Self::MetadataBase64TooLarge => "metadata_base64_too_large",
            Self::MetadataBase64Decode => "metadata_base64_decode",
            Self::MetadataDecodedTooLarge => "metadata_decoded_too_large",
            Self::MetadataUploadRead => "metadata_upload_read",
            Self::MetadataS3Read => "metadata_s3_read",
            Self::ExternalSourceBlocked => "metadata_external_blocked",
            Self::ExternalSourcePayloadTooLarge => "metadata_external_payload_too_large",
            Self::ExternalSourceBufferBudgetExhausted => {
                "metadata_external_buffer_budget_exhausted"
            }
            Self::ExternalSourceBufferAllocationFailed => {
                "metadata_external_buffer_allocation_failed"
            }
            Self::ExternalSourceUpstreamStatus(_) => "metadata_external_status",
            Self::ExternalSourceFetchFailed => "metadata_external_fetch",
            Self::MetadataSvgRasterizeTimeout { .. } => "metadata_svg_rasterize_timeout",
            Self::MetadataSvgRasterizeFailed { .. } => "metadata_svg_rasterize_failed",
            Self::MetadataExtractionFailed { .. } => "metadata_extraction_failed",
        }
    }

    pub(in crate::server) fn http_status(&self) -> StatusCode {
        match self {
            Self::MediaInputUnsupportedType
            | Self::MediaInputMissingField
            | Self::MetadataBase64TooLarge
            | Self::MetadataBase64Decode
            | Self::MetadataDecodedTooLarge
            | Self::MetadataUploadRead
            | Self::MetadataS3Read
            | Self::ExternalSourceBlocked
            | Self::MetadataSvgRasterizeFailed { .. }
            | Self::MetadataExtractionFailed { .. } => StatusCode::BAD_REQUEST,
            Self::ExternalSourcePayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
            Self::ExternalSourceBufferBudgetExhausted
            | Self::ExternalSourceBufferAllocationFailed => StatusCode::SERVICE_UNAVAILABLE,
            Self::ExternalSourceUpstreamStatus(status) => {
                map_internal_metadata_upstream_status(*status)
            }
            Self::ExternalSourceFetchFailed => StatusCode::BAD_GATEWAY,
            Self::MetadataSvgRasterizeTimeout { .. } => StatusCode::GATEWAY_TIMEOUT,
        }
    }

    pub(in crate::server) fn into_response(self) -> Response {
        let status = self.http_status();
        let code = self.code();
        let body = canonical_reason_str(status);
        match self.into_detail() {
            Some(detail) => text_with_source(status, body, code, detail),
            None => text_with_reason(status, body, code),
        }
    }

    fn into_detail(self) -> Option<String> {
        match self {
            Self::MetadataSvgRasterizeTimeout { detail }
            | Self::MetadataSvgRasterizeFailed { detail }
            | Self::MetadataExtractionFailed { detail } => Some(detail),
            _ => None,
        }
    }
}

impl From<ExternalFetchError> for MediaFailure {
    fn from(err: ExternalFetchError) -> Self {
        match err {
            ExternalFetchError::BlockedUrl => Self::ExternalSourceBlocked,
            ExternalFetchError::PayloadTooLarge => Self::ExternalSourcePayloadTooLarge,
            ExternalFetchError::BufferBudgetExhausted => Self::ExternalSourceBufferBudgetExhausted,
            ExternalFetchError::BufferAllocationFailed => {
                Self::ExternalSourceBufferAllocationFailed
            }
            ExternalFetchError::UpstreamFailure(status) => {
                Self::ExternalSourceUpstreamStatus(status)
            }
            ExternalFetchError::TooManyRedirects | ExternalFetchError::FetchFailed => {
                Self::ExternalSourceFetchFailed
            }
        }
    }
}
