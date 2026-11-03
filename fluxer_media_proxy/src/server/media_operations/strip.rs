// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{MediaFailure, MediaInput, MediaInputLimit, load_media_input};
use crate::{
    media_process::{self, MediaError},
    metrics, mime,
    server::{state::AppState, transform::execution::deadline_instant},
    upload_relay::{release_spool_budget, try_reserve_spool_budget},
};
use bytes::Bytes;
use memmap2::Mmap;

const AV_STRIP_TIMEOUT_MS: i64 = 10 * 60 * 1000;
const AUDIO_DURATION_TIMEOUT_MS: i64 = 5_000;

pub(in crate::server) struct StrippedMedia {
    pub(in crate::server) data: Bytes,
    pub(in crate::server) content_type: String,
    pub(in crate::server) dimensions: Option<(u32, u32)>,
}

struct SpoolReservation(u64);

impl SpoolReservation {
    fn try_new(bytes: u64, ceiling: u64) -> Option<Self> {
        try_reserve_spool_budget(bytes, ceiling).then_some(Self(bytes))
    }
}

impl Drop for SpoolReservation {
    fn drop(&mut self) {
        release_spool_budget(self.0);
    }
}

struct StrippedFile {
    map: Mmap,
    _reservation: SpoolReservation,
}

impl AsRef<[u8]> for StrippedFile {
    fn as_ref(&self) -> &[u8] {
        &self.map
    }
}

fn normalize_content_type(content_type: &str) -> String {
    content_type
        .split(';')
        .next()
        .unwrap_or_default()
        .trim()
        .to_ascii_lowercase()
}

fn strip_failure(error: &anyhow::Error) -> MediaFailure {
    match error.downcast_ref::<MediaError>() {
        Some(MediaError::UnsupportedMediaType) => MediaFailure::StripUnsupportedType,
        Some(MediaError::RequestTimeout) => MediaFailure::StripTimeout,
        _ => MediaFailure::StripFailed {
            detail: format!("{error:?}"),
        },
    }
}

pub(in crate::server) async fn strip_media(
    app: &AppState,
    input: MediaInput,
    content_type: Option<String>,
) -> Result<StrippedMedia, MediaFailure> {
    let input = load_media_input(app, input, MediaInputLimit::INTERNAL_REQUEST).await?;
    let content_type = match content_type {
        Some(content_type) => normalize_content_type(&content_type),
        None => mime::sniff(&input.data).mime.to_owned(),
    };
    match mime::category(&content_type) {
        Some(mime::Category::Image) => strip_image(app, input.data).await,
        Some(mime::Category::Video | mime::Category::Audio) => {
            strip_av(app, input.data, content_type).await
        }
        None => Err(MediaFailure::StripUnsupportedType),
    }
}

async fn strip_image(app: &AppState, data: Bytes) -> Result<StrippedMedia, MediaFailure> {
    if !media_process::strips_image(mime::sniff(&data).mime) {
        return Err(MediaFailure::StripUnsupportedType);
    }
    let runtime = app.media.transforms();
    let deadline_ms = runtime.transform_deadline_ms();
    let media_limits = runtime.limits();
    let cost_data = data.clone();
    let stripped = runtime
        .tasks()
        .run_native(
            deadline_instant(deadline_ms),
            move || Ok(media_process::metadata_cost(&cost_data, &media_limits)?),
            move || {
                Ok(media_process::strip_image_metadata(
                    &data,
                    &media_limits,
                    deadline_ms,
                )?)
            },
        )
        .await
        .map_err(|error| strip_failure(&error))?;
    Ok(StrippedMedia {
        data: Bytes::from(stripped.bytes),
        content_type: stripped.content_type.to_owned(),
        dimensions: Some((stripped.width, stripped.height)),
    })
}

async fn strip_av(
    app: &AppState,
    data: Bytes,
    content_type: String,
) -> Result<StrippedMedia, MediaFailure> {
    let container =
        media_process::av_container(&content_type).ok_or(MediaFailure::StripUnsupportedType)?;
    let relay = &app.cfg.upload_relay;
    let reservation = SpoolReservation::try_new(data.len() as u64, relay.spool_max_total_bytes)
        .ok_or(MediaFailure::StripSpoolExhausted)?;
    let output = tempfile::NamedTempFile::new_in(&relay.spool_dir).map_err(|error| {
        MediaFailure::StripFailed {
            detail: format!("spool file: {error}"),
        }
    })?;
    let path = output.path().to_owned();
    let deadline_ms = metrics::now_ms() + AV_STRIP_TIMEOUT_MS;
    let cost = media_process::av_native_cost(data.len());
    app.media
        .transforms()
        .tasks()
        .run_native(
            deadline_instant(Some(deadline_ms)),
            move || Ok(cost),
            move || {
                media_process::strip_av_metadata(&data, container, &path, Some(deadline_ms))?;
                Ok(())
            },
        )
        .await
        .map_err(|error| strip_failure(&error))?;
    let map =
        unsafe { Mmap::map(output.as_file()) }.map_err(|error| MediaFailure::StripFailed {
            detail: format!("map stripped output: {error}"),
        })?;
    drop(output);
    Ok(StrippedMedia {
        data: Bytes::from_owner(StrippedFile {
            map,
            _reservation: reservation,
        }),
        content_type,
        dimensions: None,
    })
}

pub(in crate::server) async fn probe_audio_duration(
    app: &AppState,
    input: MediaInput,
) -> Result<Option<f64>, MediaFailure> {
    let input = load_media_input(app, input, MediaInputLimit::INTERNAL_REQUEST).await?;
    let deadline_ms = metrics::now_ms() + AUDIO_DURATION_TIMEOUT_MS;
    let cost = media_process::av_native_cost(input.data.len());
    let data = input.data;
    app.media
        .transforms()
        .tasks()
        .run_native(
            deadline_instant(Some(deadline_ms)),
            move || Ok(cost),
            move || {
                Ok(media_process::audio_duration_seconds(
                    &data,
                    Some(deadline_ms),
                )?)
            },
        )
        .await
        .map_err(|error| strip_failure(&error))
}
