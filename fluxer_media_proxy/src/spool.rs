// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::body::Body;
use http_body_util::BodyExt;
use memmap2::Mmap;
use std::{io::SeekFrom, path::Path};
use thiserror::Error;
use tokio::{
    fs::File,
    io::{AsyncSeekExt, AsyncWriteExt, BufWriter},
};

use crate::upload_relay::SpoolReservation;

#[derive(Debug, Error)]
pub enum SpoolError {
    #[error("payload exceeded declared length")]
    PayloadTooLarge,
    #[error("payload shorter than declared length")]
    PayloadShortRead,
    #[error("disk spool budget exhausted")]
    BudgetExhausted,
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("body read error: {0}")]
    Body(axum::Error),
}

#[derive(Debug)]
pub struct SpooledBody {
    file: File,
    length: u64,
    reservation: SpoolReservation,
}

impl SpooledBody {
    pub fn into_parts(self) -> (File, u64, SpoolReservation) {
        (self.file, self.length, self.reservation)
    }

    pub fn into_mapped(self) -> std::io::Result<MappedSpool> {
        let map = unsafe { Mmap::map(&self.file)? };
        Ok(MappedSpool { map, _spool: self })
    }
}

pub struct MappedSpool {
    map: Mmap,
    _spool: SpooledBody,
}

impl AsRef<[u8]> for MappedSpool {
    fn as_ref(&self) -> &[u8] {
        &self.map
    }
}

pub async fn spool_to_temp(
    body: Body,
    declared_length: Option<u64>,
    max_body_bytes: u64,
    dir: &Path,
    write_buffer_bytes: usize,
    spool_total_ceiling: u64,
) -> Result<SpooledBody, SpoolError> {
    if let Some(declared_length) = declared_length
        && declared_length > max_body_bytes
    {
        return Err(SpoolError::PayloadTooLarge);
    }
    let mut reservation =
        SpoolReservation::try_new(declared_length.unwrap_or(0), spool_total_ceiling)
            .ok_or(SpoolError::BudgetExhausted)?;
    let (file, length) = spool_body_inner(
        body,
        declared_length,
        max_body_bytes,
        spool_total_ceiling,
        &mut reservation,
        dir,
        write_buffer_bytes,
    )
    .await?;
    Ok(SpooledBody {
        file,
        length,
        reservation,
    })
}

async fn spool_body_inner(
    mut body: Body,
    declared_length: Option<u64>,
    max_body_bytes: u64,
    spool_total_ceiling: u64,
    reservation: &mut SpoolReservation,
    dir: &Path,
    write_buffer_bytes: usize,
) -> Result<(File, u64), SpoolError> {
    let std_file = tempfile::tempfile_in(dir)?;
    let mut writer = BufWriter::with_capacity(write_buffer_bytes, File::from_std(std_file));
    let mut written: u64 = 0;
    while let Some(frame_result) = body.frame().await {
        let frame = frame_result.map_err(SpoolError::Body)?;
        let Ok(chunk) = frame.into_data() else {
            continue;
        };
        if chunk.is_empty() {
            continue;
        }
        let next = written.saturating_add(chunk.len() as u64);
        if next > max_body_bytes || declared_length.is_some_and(|declared| next > declared) {
            return Err(SpoolError::PayloadTooLarge);
        }
        if declared_length.is_none()
            && !reservation.try_grow(chunk.len() as u64, spool_total_ceiling)
        {
            return Err(SpoolError::BudgetExhausted);
        }
        writer.write_all(&chunk).await?;
        written = next;
    }
    if declared_length.is_some_and(|declared| written != declared) {
        return Err(SpoolError::PayloadShortRead);
    }
    writer.flush().await?;
    let mut file = writer.into_inner();
    file.seek(SeekFrom::Start(0)).await?;
    Ok((file, written))
}
