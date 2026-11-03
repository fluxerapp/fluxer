// SPDX-License-Identifier: AGPL-3.0-or-later

use super::failure::MediaFailure;
use crate::{
    byte_budget::BudgetedBytes,
    constants::{self, AssetExtension},
    mime,
    server::{
        external::fetch_external,
        params::url_filename,
        self_origin::{self, SelfOrigin},
        state::AppState,
    },
    spool::{SpoolError, spool_to_temp},
    storage::{self, StorageError, StreamObject},
};
use axum::body::Body;
use base64::{Engine as _, engine::general_purpose};
use bytes::Bytes;
use futures_util::StreamExt as _;
use tracing::warn;

pub(in crate::server) const STORED_SNIFF_PREFIX_BYTES: usize = 8192;

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
            let data = read_stored_input(
                app,
                &app.cfg.storage.bucket_uploads,
                &upload_filename,
                limit.decoded_bytes,
            )
            .await
            .map_err(|err| {
                let failure = MediaFailure::MetadataUploadRead;
                warn!(reason = failure.code(), key = upload_filename.as_str(), %err);
                failure
            })?;
            Ok(LoadedMediaInput {
                data,
                filename: filename.unwrap_or(upload_filename),
            })
        }
        MediaInput::Storage {
            bucket,
            key,
            filename,
        } => {
            let data = read_stored_input(app, &bucket, &key, limit.decoded_bytes)
                .await
                .map_err(|err| {
                    let failure = MediaFailure::MetadataS3Read;
                    warn!(reason = failure.code(), bucket = bucket.as_str(), key = key.as_str(), %err);
                    failure
                })?;
            Ok(LoadedMediaInput {
                data,
                filename: filename.unwrap_or(key),
            })
        }
        MediaInput::External { url, filename } => match self_origin::resolve(app, &url) {
            Some(SelfOrigin::Stored {
                bucket,
                key,
                fallback_ext,
            }) => {
                let data = read_own_object(app, &bucket, &key, fallback_ext)
                    .await
                    .map_err(|err| {
                        let failure = MediaFailure::MetadataS3Read;
                        warn!(reason = failure.code(), bucket = bucket.as_str(), key = key.as_str(), %err);
                        failure
                    })?;
                Ok(LoadedMediaInput {
                    data,
                    filename: filename.unwrap_or_else(|| url_filename(&url)),
                })
            }
            Some(SelfOrigin::External { url: target }) => {
                load_external_url(app, &target, filename).await
            }
            None => load_external_url(app, &url, filename).await,
        },
    }
}

async fn load_external_url(
    app: &AppState,
    url: &str,
    filename: Option<String>,
) -> Result<LoadedMediaInput, MediaFailure> {
    let (fetched_url, data) = fetch_external(app, url).await.map_err(|err| {
        let failure = MediaFailure::from(err);
        warn!(reason = failure.code(), url, ?err);
        failure
    })?;
    Ok(LoadedMediaInput {
        data: retained_input_bytes(data),
        filename: filename.unwrap_or_else(|| url_filename(&fetched_url)),
    })
}

async fn read_stored_input(
    app: &AppState,
    bucket: &str,
    key: &str,
    buffer_limit: usize,
) -> Result<Bytes, StorageError> {
    let budget = storage::unversioned_read_budget(buffer_limit);
    match app
        .store
        .read_object_limited(bucket, key, buffer_limit, &budget)
        .await
    {
        Err(StorageError::StreamTooLong) => Ok(spool_stored_media(
            app,
            bucket,
            key,
            &[mime::Category::Video, mime::Category::Audio],
        )
        .await?
        .data),
        other => Ok(other?.data.as_bytes().clone()),
    }
}

pub(in crate::server) async fn spool_stored_media(
    app: &AppState,
    bucket: &str,
    key: &str,
    categories: &[mime::Category],
) -> Result<storage::Object, StorageError> {
    let range = format!("bytes=0-{}", STORED_SNIFF_PREFIX_BYTES - 1);
    let head = app.store.stream_object(bucket, key, Some(&range)).await?;
    let prefix = read_sniff_prefix(head.body)
        .await
        .map_err(|err| StorageError::ObjectStorage(err.into()))?;
    let sniffed = mime::sniff(&prefix).mime;
    if !mime::is_supported_media_mime(sniffed)
        || !mime::category(sniffed).is_some_and(|category| categories.contains(&category))
    {
        return Err(StorageError::StreamTooLong);
    }
    let object = app.store.stream_object(bucket, key, None).await?;
    Ok(storage::Object {
        data: spool_stream(app, object).await?,
        content_type: sniffed.to_owned(),
    })
}

async fn spool_stream(app: &AppState, object: StreamObject) -> Result<Bytes, StorageError> {
    let Some(content_length) = object.content_length else {
        return Err(StorageError::StreamTooLong);
    };
    let relay = &app.cfg.upload_relay;
    let spooled = spool_to_temp(
        object.body,
        Some(content_length),
        relay.spool_max_total_bytes,
        &relay.spool_dir,
        relay.spool_chunk_bytes,
        relay.spool_max_total_bytes,
    )
    .await
    .map_err(|err| match err {
        SpoolError::PayloadTooLarge => StorageError::StreamTooLong,
        other => StorageError::ObjectStorage(other.into()),
    })?;
    Ok(Bytes::from_owner(spooled.into_mapped()?))
}

pub(in crate::server) async fn read_sniff_prefix(body: Body) -> Result<Vec<u8>, axum::Error> {
    let mut prefix = Vec::with_capacity(STORED_SNIFF_PREFIX_BYTES);
    let mut chunks = body.into_data_stream();
    while prefix.len() < STORED_SNIFF_PREFIX_BYTES {
        let Some(chunk) = chunks.next().await else {
            break;
        };
        let chunk = chunk?;
        let wanted = chunk.len().min(STORED_SNIFF_PREFIX_BYTES - prefix.len());
        prefix.extend_from_slice(&chunk[..wanted]);
    }
    Ok(prefix)
}

async fn read_own_object(
    app: &AppState,
    bucket: &str,
    key: &str,
    fallback_ext: Option<AssetExtension>,
) -> Result<Bytes, StorageError> {
    match app.store.read_object(bucket, key).await {
        Err(StorageError::NotFound) => {
            let fallback_ext = fallback_ext.ok_or(StorageError::NotFound)?;
            let fallback_key = format!("{key}.{}", fallback_ext.name());
            Ok(app.store.read_object(bucket, &fallback_key).await?.data)
        }
        other => Ok(other?.data),
    }
}

// The shared external buffer budget accounts for resident bytes, not for the fetch alone. The
// internal metadata and frame routes keep the whole input alive across extraction, so the
// reservation has to travel with the bytes instead of being dropped when the fetch returns.
pub(super) fn retained_input_bytes(data: BudgetedBytes) -> Bytes {
    Bytes::from_owner(data)
}
