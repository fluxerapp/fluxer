// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{FluxerNSFWFrameOut, fluxer_nsfw_frames_free};
use crate::nsfw::NSFW_MAX_FRAME_BYTES;
use std::{ptr::NonNull, slice};

const MAX_NSFW_FRAME_OUTPUTS: usize = 3;

pub struct NSFWFrameOutput {
    slots: [FluxerNSFWFrameOut; MAX_NSFW_FRAME_OUTPUTS],
    expected: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NSFWFrameCopyError {
    AllocationFailed,
    InvalidOutput,
}

impl NSFWFrameOutput {
    pub fn new(expected: usize) -> Self {
        assert!(expected > 0, "native NSFW frame count must be positive");
        assert!(
            expected <= MAX_NSFW_FRAME_OUTPUTS,
            "native NSFW frame count exceeds slots"
        );
        Self {
            slots: std::array::from_fn(|_| FluxerNSFWFrameOut::empty()),
            expected,
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut FluxerNSFWFrameOut {
        self.slots.as_mut_ptr()
    }

    pub fn copy_frames(&self) -> Result<Vec<Vec<u8>>, NSFWFrameCopyError> {
        let mut frames = Vec::new();
        frames
            .try_reserve_exact(self.expected)
            .map_err(|_| NSFWFrameCopyError::AllocationFailed)?;
        for slot in self.slots.iter().take(self.expected) {
            // a sample the native decoder could not reach leaves its slot empty
            let Some(data) = NonNull::new(slot.data.cast::<u8>()) else {
                if slot.len != 0 {
                    return Err(NSFWFrameCopyError::InvalidOutput);
                }
                continue;
            };
            if !(1..=NSFW_MAX_FRAME_BYTES).contains(&slot.len) || slot.len > isize::MAX as usize {
                return Err(NSFWFrameCopyError::InvalidOutput);
            }
            let bytes = unsafe { slice::from_raw_parts(data.as_ptr(), slot.len) };
            let mut frame = Vec::new();
            frame
                .try_reserve_exact(slot.len)
                .map_err(|_| NSFWFrameCopyError::AllocationFailed)?;
            frame.extend_from_slice(bytes);
            frames.push(frame);
        }
        if frames.is_empty() {
            return Err(NSFWFrameCopyError::InvalidOutput);
        }
        Ok(frames)
    }
}

impl Drop for NSFWFrameOutput {
    fn drop(&mut self) {
        unsafe { fluxer_nsfw_frames_free(self.slots.as_mut_ptr(), self.expected) };
    }
}
