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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        config::Config,
        storage::tests::{FakeObject, FakeS3, fake_s3},
    };
    use axum::http::{Method, StatusCode, header};

    const BUFFER_LIMIT: MediaInputLimit = MediaInputLimit {
        encoded_bytes: 1024,
        decoded_bytes: 1024,
    };

    fn local_storage_app(root: &std::path::Path) -> AppState {
        let spool_dir = root.join("spool");
        std::fs::create_dir(&spool_dir).unwrap();
        let cfg = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret".to_owned()),
            ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "local".to_owned()),
            (
                "FLUXER_MEDIA_PROXY_STORAGE_ROOT",
                root.display().to_string(),
            ),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR",
                spool_dir.display().to_string(),
            ),
        ])
        .expect("test config");
        AppState::for_tests(cfg)
    }

    #[tokio::test]
    async fn a_stored_input_larger_than_the_buffer_limit_is_read_from_a_spooled_copy() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().canonicalize().unwrap();
        let app = local_storage_app(&root);
        let uploads = app.cfg.storage.bucket_uploads.clone();
        let cdn = app.cfg.storage.bucket_cdn.clone();
        let mp4 = crate::test_fixtures::fixture_h264_mp4();
        assert!(mp4.len() > BUFFER_LIMIT.decoded_bytes);
        for bucket in [&uploads, &cdn] {
            app.store
                .write_object(bucket, "big.mp4", &mp4, "video/mp4")
                .await
                .unwrap();
        }

        let upload = load_media_input(
            &app,
            MediaInput::Upload {
                upload_filename: "big.mp4".to_owned(),
                filename: None,
            },
            BUFFER_LIMIT,
        )
        .await
        .unwrap_or_else(|failure| panic!("upload input failed: {}", failure.code()));
        let stored = load_media_input(
            &app,
            MediaInput::Storage {
                bucket: cdn,
                key: "big.mp4".to_owned(),
                filename: Some("named.mp4".to_owned()),
            },
            BUFFER_LIMIT,
        )
        .await
        .unwrap_or_else(|failure| panic!("stored input failed: {}", failure.code()));

        assert_eq!(&mp4[..], &upload.data[..]);
        assert_eq!("big.mp4", upload.filename);
        assert_eq!(&mp4[..], &stored.data[..]);
        assert_eq!("named.mp4", stored.filename);
        let metadata =
            crate::media_process::probe_av_metadata(&upload.data, 0, &app.media.limits(), None)
                .unwrap();
        assert!(metadata.probe.has_video);
        assert!(
            std::fs::read_dir(root.join("spool"))
                .unwrap()
                .next()
                .is_none(),
            "a spooled copy is never visible in the spool directory"
        );
    }

    fn upload_input(key: &str) -> MediaInput {
        MediaInput::Upload {
            upload_filename: key.to_owned(),
            filename: None,
        }
    }

    fn body_reads(fake: &FakeS3) -> Vec<Option<String>> {
        fake.requests()
            .into_iter()
            .filter(|(method, _, _, _)| *method == Method::GET)
            .map(|(_, _, headers, _)| {
                headers
                    .get(header::RANGE)
                    .and_then(|value| value.to_str().ok())
                    .map(str::to_owned)
            })
            .collect()
    }

    #[tokio::test]
    async fn object_storage_spools_a_video_larger_than_the_buffer_limit() {
        let fake = fake_s3().await;
        let mp4 = crate::test_fixtures::fixture_h264_mp4();
        fake.put_object(
            "uploads/big.mp4",
            FakeObject {
                body: mp4.clone(),
                content_type: Some("application/octet-stream".to_owned()),
                ..FakeObject::default()
            },
        );
        let tmp = tempfile::tempdir().unwrap();
        let app = AppState::for_tests(fake.config(tmp.path()));

        let loaded = load_media_input(&app, upload_input("big.mp4"), BUFFER_LIMIT)
            .await
            .unwrap_or_else(|failure| panic!("upload input failed: {}", failure.code()));

        assert_eq!(&mp4[..], &loaded.data[..]);
        assert_eq!(
            vec![None, Some("bytes=0-8191".to_owned()), None],
            body_reads(&fake)
        );
    }

    #[tokio::test]
    async fn an_oversized_input_that_is_not_audio_or_video_is_refused_from_its_prefix() {
        let fake = fake_s3().await;
        let mut png = crate::test_fixtures::synthetic_png(8, 8);
        png.resize(BUFFER_LIMIT.decoded_bytes + 1, 0);
        for (key, body) in [
            ("uploads/huge.png", png),
            (
                "uploads/notes.mp4",
                vec![b'a'; BUFFER_LIMIT.decoded_bytes + 1],
            ),
        ] {
            fake.put_object(
                key,
                FakeObject {
                    body,
                    ..FakeObject::default()
                },
            );
        }
        let tmp = tempfile::tempdir().unwrap();
        let app = AppState::for_tests(fake.config(tmp.path()));

        for key in ["huge.png", "notes.mp4"] {
            let failure = load_media_input(&app, upload_input(key), BUFFER_LIMIT)
                .await
                .err()
                .expect("only audio and video are read past the buffer limit");
            assert_eq!("metadata_upload_read", failure.code(), "{key}");
        }

        assert_eq!(
            vec![
                None,
                Some("bytes=0-8191".to_owned()),
                None,
                Some("bytes=0-8191".to_owned())
            ],
            body_reads(&fake)
        );
    }

    #[tokio::test]
    async fn a_stream_without_a_declared_length_is_not_spooled() {
        let tmp = tempfile::tempdir().unwrap();
        let app = local_storage_app(&tmp.path().canonicalize().unwrap());
        let object = StreamObject {
            body: Body::from_stream(futures_util::stream::iter([Ok::<_, std::io::Error>(
                Bytes::from_static(b"payload"),
            )])),
            status: StatusCode::OK,
            content_length: None,
            content_type: "video/mp4".to_owned(),
            byte_range: None,
            total_length: None,
        };

        let refused = spool_stream(&app, object).await;

        assert!(matches!(refused, Err(StorageError::StreamTooLong)));
    }

    #[tokio::test]
    async fn a_missing_stored_input_is_reported_as_a_read_failure() {
        let tmp = tempfile::tempdir().unwrap();
        let app = local_storage_app(&tmp.path().canonicalize().unwrap());

        let failure = load_media_input(
            &app,
            MediaInput::Upload {
                upload_filename: "missing.mp4".to_owned(),
                filename: None,
            },
            BUFFER_LIMIT,
        )
        .await
        .err()
        .expect("a missing upload cannot load");

        assert_eq!("metadata_upload_read", failure.code());
    }
}
