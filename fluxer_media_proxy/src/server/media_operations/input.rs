// SPDX-License-Identifier: AGPL-3.0-or-later

use super::failure::MediaFailure;
use crate::{
    byte_budget::BudgetedBytes,
    constants,
    server::{external::fetch_external, params::url_filename, state::AppState},
};
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;
use tracing::warn;

pub(in crate::server) enum MediaInput {
    Base64 {
        data: String,
        filename: Option<String>,
    },
    Upload {
        upload_filename: String,
        filename: Option<String>,
    },
    Storage {
        bucket: String,
        key: String,
        filename: Option<String>,
    },
    External {
        url: String,
        filename: Option<String>,
    },
}

pub(in crate::server) struct LoadedMediaInput {
    pub(in crate::server) data: Bytes,
    pub(in crate::server) filename: String,
}

#[derive(Clone, Copy)]
pub(in crate::server) struct MediaInputLimit {
    encoded_bytes: usize,
    decoded_bytes: usize,
}

impl MediaInputLimit {
    pub(in crate::server) const INTERNAL_REQUEST: Self = Self {
        encoded_bytes: constants::MAX_INTERNAL_REQUEST_BODY_BYTES,
        decoded_bytes: constants::MAX_MEDIA_PROXY_BYTES,
    };
}

pub(in crate::server) async fn load_media_input(
    app: &AppState,
    input: MediaInput,
    limit: MediaInputLimit,
) -> Result<LoadedMediaInput, MediaFailure> {
    match input {
        MediaInput::Base64 { data, filename } => {
            let encoded = data
                .rsplit_once(',')
                .map(|(_, value)| value)
                .unwrap_or(data.as_str());
            if encoded.len() > limit.encoded_bytes {
                let failure = MediaFailure::MetadataBase64TooLarge;
                warn!(reason = failure.code(), len = encoded.len());
                return Err(failure);
            }
            let decoded = general_purpose::STANDARD.decode(encoded).map_err(|err| {
                let failure = MediaFailure::MetadataBase64Decode;
                warn!(reason = failure.code(), ?err);
                failure
            })?;
            if decoded.len() > limit.decoded_bytes {
                let failure = MediaFailure::MetadataDecodedTooLarge;
                warn!(reason = failure.code(), len = decoded.len());
                return Err(failure);
            }
            Ok(LoadedMediaInput {
                data: Bytes::from(decoded),
                filename: filename.unwrap_or_else(|| "inline.bin".to_owned()),
            })
        }
        MediaInput::Upload {
            upload_filename,
            filename,
        } => {
            let object = app
                .store
                .read_object(&app.cfg.storage.bucket_uploads, &upload_filename)
                .await
                .map_err(|err| {
                    let failure = MediaFailure::MetadataUploadRead;
                    warn!(reason = failure.code(), key = upload_filename.as_str(), %err);
                    failure
                })?;
            Ok(LoadedMediaInput {
                data: object.data,
                filename: filename.unwrap_or(upload_filename),
            })
        }
        MediaInput::Storage {
            bucket,
            key,
            filename,
        } => {
            let object = app.store.read_object(&bucket, &key).await.map_err(|err| {
                let failure = MediaFailure::MetadataS3Read;
                warn!(reason = failure.code(), bucket = bucket.as_str(), key = key.as_str(), %err);
                failure
            })?;
            Ok(LoadedMediaInput {
                data: object.data,
                filename: filename.unwrap_or(key),
            })
        }
        MediaInput::External { url, filename } => {
            let (fetched_url, data) = fetch_external(app, &url).await.map_err(|err| {
                let failure = MediaFailure::from(err);
                warn!(reason = failure.code(), url = url.as_str(), ?err);
                failure
            })?;
            Ok(LoadedMediaInput {
                data: retained_input_bytes(data),
                filename: filename.unwrap_or_else(|| url_filename(&fetched_url)),
            })
        }
    }
}

// The shared external buffer budget accounts for resident bytes, not for the fetch alone. The
// internal metadata and frame routes keep the whole input alive across extraction, so the
// reservation has to travel with the bytes instead of being dropped when the fetch returns.
pub(super) fn retained_input_bytes(data: BudgetedBytes) -> Bytes {
    Bytes::from_owner(data)
}
