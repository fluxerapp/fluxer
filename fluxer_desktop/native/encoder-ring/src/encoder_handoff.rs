// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ring::RingError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EncoderDims {
    pub width: u32,
    pub height: u32,
}

impl EncoderDims {
    pub fn new(width: u32, height: u32) -> Self {
        assert!(width > 0, "width must be positive");
        assert!(height > 0, "height must be positive");
        Self { width, height }
    }
}

pub const ENCODER_FRAME_RATE_MIN: u32 = 1;
pub const ENCODER_FRAME_RATE_MAX: u32 = 240;
pub const ENCODER_FRAME_RATE_DEFAULT: u32 = 60;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EncoderFrameRate {
    pub numerator: u32,
    pub denominator: u32,
}

impl EncoderFrameRate {
    pub fn new(numerator: u32, denominator: u32) -> Self {
        assert!(
            numerator >= ENCODER_FRAME_RATE_MIN,
            "fps numerator positive"
        );
        assert!(denominator > 0, "fps denominator positive");
        let frame_rate = Self {
            numerator: numerator.min(ENCODER_FRAME_RATE_MAX),
            denominator,
        };
        assert!(frame_rate.numerator >= ENCODER_FRAME_RATE_MIN);
        assert!(frame_rate.denominator > 0);
        frame_rate
    }

    pub fn from_fps(fps: u32) -> Self {
        let numerator = fps.clamp(ENCODER_FRAME_RATE_MIN, ENCODER_FRAME_RATE_MAX);
        Self::new(numerator, 1)
    }

    pub fn frame_interval_us(self) -> u64 {
        let numerator = u64::from(self.numerator);
        let denominator = u64::from(self.denominator);
        assert!(numerator > 0, "fps numerator positive");
        assert!(denominator > 0, "fps denominator positive");
        (1_000_000u64 * denominator).div_ceil(numerator)
    }

    pub fn gop_pic_size(self) -> u16 {
        let rounded = u64::from(self.numerator).div_ceil(u64::from(self.denominator));
        let bounded = rounded.clamp(1, u64::from(ENCODER_FRAME_RATE_MAX));
        bounded as u16
    }
}

impl Default for EncoderFrameRate {
    fn default() -> Self {
        Self::from_fps(ENCODER_FRAME_RATE_DEFAULT)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EncoderSubmission {
    pub shared_handle: u64,
    pub keyed_mutex_key: u64,
    pub dims: EncoderDims,
    pub sequence: u64,
    pub capture_pts_us: Option<u64>,
}

impl EncoderSubmission {
    pub fn new(shared_handle: u64, keyed_mutex_key: u64, dims: EncoderDims, sequence: u64) -> Self {
        assert!(shared_handle != 0, "shared handle must be non-zero");
        assert!(dims.width > 0, "dims width positive");
        let s = Self {
            shared_handle,
            keyed_mutex_key,
            dims,
            sequence,
            capture_pts_us: None,
        };
        assert!(s.shared_handle == shared_handle, "post construct intact");
        assert!(s.capture_pts_us.is_none(), "capture pts defaults absent");
        s
    }

    pub fn with_capture_pts_us(mut self, capture_pts_us: u64) -> Self {
        assert!(self.shared_handle != 0, "shared handle must be non-zero");
        self.capture_pts_us = Some(capture_pts_us);
        assert_eq!(
            self.capture_pts_us,
            Some(capture_pts_us),
            "capture pts recorded"
        );
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PicParams {
    pub pts_us: u64,
    pub force_keyframe: bool,
}

impl PicParams {
    pub fn new(pts_us: u64, force_keyframe: bool) -> Self {
        let p = Self {
            pts_us,
            force_keyframe,
        };
        assert!(p.pts_us == pts_us, "pts_us intact");
        assert!(p.force_keyframe == force_keyframe, "force_keyframe intact");
        p
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HandoffSlot {
    pub slot_index: u32,
    pub shared_handle: u64,
}

impl HandoffSlot {
    pub fn new(slot_index: u32, shared_handle: u64) -> Self {
        assert!(slot_index < 64, "slot_index within plausible bound");
        assert!(shared_handle != 0, "shared_handle non-zero");
        Self {
            slot_index,
            shared_handle,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EncodedBitstream {
    pub data: Vec<u8>,
    pub pts_us: u64,
    pub dts_us: u64,
    pub is_keyframe: bool,
}

impl EncodedBitstream {
    pub fn new(data: Vec<u8>, pts_us: u64, dts_us: u64, is_keyframe: bool) -> Self {
        assert!(!data.is_empty(), "encoded bitstream must be non-empty");
        assert!(data.len() <= MAX_BITSTREAM_BYTES, "bitstream within cap");
        Self {
            data,
            pts_us,
            dts_us,
            is_keyframe,
        }
    }
}

pub const MAX_BITSTREAM_BYTES: usize = 16 * 1024 * 1024;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EncoderError {
    SdkNotFound {
        vendor: &'static str,
        dll: &'static str,
    },
    SymbolMissing {
        vendor: &'static str,
        symbol: &'static str,
    },
    SessionInitFailed {
        vendor: &'static str,
        status: i64,
    },
    RegisterFailed {
        vendor: &'static str,
        status: i64,
    },
    EncodeFailed {
        vendor: &'static str,
        status: i64,
    },
    BitstreamReadFailed {
        vendor: &'static str,
        status: i64,
    },
    SlotUnknown {
        slot_index: u32,
    },
    KeyMismatch {
        expected: u64,
        observed: u64,
    },
    DimensionsOutOfRange {
        width: u32,
        height: u32,
    },
    PlatformUnsupported {
        reason: &'static str,
    },
    BitstreamTooLarge {
        byte_size: usize,
    },
    NotImplemented {
        what: &'static str,
    },
}

impl std::fmt::Display for EncoderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SdkNotFound { vendor, dll } => {
                write!(f, "{vendor} SDK runtime '{dll}' not found")
            }
            Self::SymbolMissing { vendor, symbol } => {
                write!(f, "{vendor} symbol '{symbol}' missing from runtime")
            }
            Self::SessionInitFailed { vendor, status } => {
                write!(f, "{vendor} session init failed: status={status}")
            }
            Self::RegisterFailed { vendor, status } => {
                write!(f, "{vendor} register failed: status={status}")
            }
            Self::EncodeFailed { vendor, status } => {
                write!(f, "{vendor} encode failed: status={status}")
            }
            Self::BitstreamReadFailed { vendor, status } => {
                write!(f, "{vendor} bitstream read failed: status={status}")
            }
            Self::SlotUnknown { slot_index } => write!(f, "slot {slot_index} unknown"),
            Self::KeyMismatch { expected, observed } => {
                write!(
                    f,
                    "keyed-mutex key mismatch: expected={expected} observed={observed}"
                )
            }
            Self::DimensionsOutOfRange { width, height } => {
                write!(f, "encoder dims out of range: {width}x{height}")
            }
            Self::PlatformUnsupported { reason } => write!(f, "platform unsupported: {reason}"),
            Self::BitstreamTooLarge { byte_size } => {
                write!(f, "bitstream {byte_size} bytes exceeds cap")
            }
            Self::NotImplemented { what } => write!(f, "not implemented: {what}"),
        }
    }
}

impl std::error::Error for EncoderError {}

pub trait EncoderCompletionCallback: Send {
    fn on_complete(&mut self, sequence: u64, encoded_bytes: u32);
}

pub trait NvencHandoff: Send {
    fn register_slot(
        &mut self,
        shared_handle: u64,
        key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError>;

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError>;

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream>;

    fn unregister_slot(&mut self, slot: HandoffSlot);

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError>;
}

pub trait AmfHandoff: Send {
    fn register_slot(
        &mut self,
        shared_handle: u64,
        key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError>;

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError>;

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream>;

    fn unregister_slot(&mut self, slot: HandoffSlot);

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError>;
}

pub trait QsvHandoff: Send {
    fn register_slot(
        &mut self,
        shared_handle: u64,
        key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError>;

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError>;

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream>;

    fn unregister_slot(&mut self, slot: HandoffSlot);

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError>;
}

pub trait VideoToolboxHandoff: Send {
    fn register_slot(
        &mut self,
        iosurface_handle: u64,
        key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError>;

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError>;

    fn poll_completed(&mut self, slot: HandoffSlot) -> Option<EncodedBitstream>;

    fn unregister_slot(&mut self, slot: HandoffSlot);

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError>;
}

pub struct VtNoOpHandoff {
    accepted: u64,
    next_slot_index: u32,
    pending: std::collections::VecDeque<(u64, u64)>,
}

impl VtNoOpHandoff {
    pub fn new() -> Self {
        let h = Self {
            accepted: 0,
            next_slot_index: 0,
            pending: std::collections::VecDeque::with_capacity(16),
        };
        assert_eq!(h.accepted, 0, "fresh handoff has no accepted frames");
        assert_eq!(h.next_slot_index, 0, "fresh handoff slot index zero");
        h
    }

    pub fn accepted_count(&self) -> u64 {
        let n = self.accepted;
        assert!(
            self.pending.len() <= u32::MAX as usize,
            "pending queue plausible"
        );
        assert!(n >= self.pending.len() as u64, "accepted >= pending");
        n
    }

    pub fn pending_len(&self) -> usize {
        let len = self.pending.len();
        assert!(
            len <= self.pending.capacity().max(1),
            "pending within capacity bound"
        );
        assert!(len as u64 <= self.accepted, "pending <= accepted");
        len
    }
}

impl Default for VtNoOpHandoff {
    fn default() -> Self {
        Self::new()
    }
}

impl VideoToolboxHandoff for VtNoOpHandoff {
    fn register_slot(
        &mut self,
        iosurface_handle: u64,
        _key: u64,
        dims: EncoderDims,
    ) -> Result<HandoffSlot, EncoderError> {
        if iosurface_handle == 0 {
            return Err(EncoderError::SlotUnknown {
                slot_index: u32::MAX,
            });
        }
        if dims.width == 0 || dims.height == 0 {
            return Err(EncoderError::DimensionsOutOfRange {
                width: dims.width,
                height: dims.height,
            });
        }
        let slot = HandoffSlot::new(self.next_slot_index, iosurface_handle);
        self.next_slot_index = self.next_slot_index.saturating_add(1);
        assert!(
            slot.shared_handle == iosurface_handle,
            "slot handle round-trip"
        );
        assert!(self.next_slot_index > 0, "slot counter advanced");
        Ok(slot)
    }

    fn encode_shared_async(
        &mut self,
        slot: HandoffSlot,
        _key: u64,
        dims: EncoderDims,
        pic_params: PicParams,
    ) -> Result<(), EncoderError> {
        if dims.width == 0 || dims.height == 0 {
            return Err(EncoderError::DimensionsOutOfRange {
                width: dims.width,
                height: dims.height,
            });
        }
        self.pending.push_back((self.accepted, pic_params.pts_us));
        self.accepted = self.accepted.saturating_add(1);
        assert!(slot.shared_handle != 0, "async slot handle non-zero");
        assert!(self.accepted > 0, "encode_shared_async advanced accepted");
        Ok(())
    }

    fn poll_completed(&mut self, _slot: HandoffSlot) -> Option<EncodedBitstream> {
        None
    }

    fn unregister_slot(&mut self, slot: HandoffSlot) {
        assert!(slot.shared_handle != 0, "unregister slot handle non-zero");
    }

    fn encode_shared(
        &mut self,
        submission: EncoderSubmission,
        callback: &mut dyn EncoderCompletionCallback,
    ) -> Result<(), RingError> {
        if submission.shared_handle == 0 {
            return Err(RingError::UnknownSlot);
        }
        if submission.dims.width == 0 || submission.dims.height == 0 {
            return Err(RingError::BackendFailed {
                source: crate::backend::BackendError::DimensionsOutOfRange {
                    width: submission.dims.width,
                    height: submission.dims.height,
                },
            });
        }
        let pre_accepted = self.accepted;
        self.pending.push_back((submission.sequence, 0));
        self.accepted = self.accepted.saturating_add(1);
        callback.on_complete(submission.sequence, 0);
        assert!(
            self.accepted == pre_accepted + 1,
            "VtNoOpHandoff accepted advanced"
        );
        assert!(submission.dims.width > 0, "submission dims preserved");
        Ok(())
    }
}

pub struct NotImplementedHandoff {
    pub vendor: &'static str,
}

impl NotImplementedHandoff {
    pub fn nvenc() -> Self {
        Self { vendor: "nvenc" }
    }

    pub fn amf() -> Self {
        Self { vendor: "amf" }
    }

    pub fn qsv() -> Self {
        Self { vendor: "qsv" }
    }
}

pub fn compute_dts_offset_us(first_pts_us: u64, num_b_frames: u32, frame_interval_us: u64) -> i64 {
    assert!(frame_interval_us > 0, "frame interval positive");
    assert!(num_b_frames <= 8, "B-frame count plausible");
    let offset = (num_b_frames as u64).saturating_mul(frame_interval_us);
    let result = -(offset as i64);
    let _ = first_pts_us;
    assert!(result <= 0, "DTS offset is non-positive for B-frames");
    result
}

pub fn apply_dts_offset(pts_us: u64, offset_us: i64) -> u64 {
    let signed_pts = pts_us as i64;
    let dts = signed_pts.saturating_add(offset_us);
    let clamped = if dts < 0 { 0 } else { dts as u64 };
    assert!(
        clamped <= pts_us || offset_us > 0,
        "DTS <= PTS without future B-frames"
    );
    clamped
}

macro_rules! impl_not_implemented_for_trait {
    ($trait_name:ident, $vendor_tag:expr) => {
        impl $trait_name for NotImplementedHandoff {
            fn register_slot(
                &mut self,
                shared_handle: u64,
                _key: u64,
                dims: EncoderDims,
            ) -> Result<HandoffSlot, EncoderError> {
                assert!(shared_handle != 0, "shared_handle non-zero");
                assert!(dims.width > 0, "dims width positive");
                Err(EncoderError::NotImplemented {
                    what: concat!($vendor_tag, "::register_slot"),
                })
            }

            fn encode_shared_async(
                &mut self,
                slot: HandoffSlot,
                _key: u64,
                dims: EncoderDims,
                _pic_params: PicParams,
            ) -> Result<(), EncoderError> {
                assert!(slot.shared_handle != 0, "slot shared_handle non-zero");
                assert!(dims.width > 0, "dims width positive");
                Err(EncoderError::NotImplemented {
                    what: concat!($vendor_tag, "::encode_shared_async"),
                })
            }

            fn poll_completed(&mut self, _slot: HandoffSlot) -> Option<EncodedBitstream> {
                None
            }

            fn unregister_slot(&mut self, slot: HandoffSlot) {
                assert!(slot.shared_handle != 0, "slot shared_handle non-zero");
            }

            fn encode_shared(
                &mut self,
                submission: EncoderSubmission,
                _callback: &mut dyn EncoderCompletionCallback,
            ) -> Result<(), RingError> {
                assert!(submission.shared_handle != 0, "submission handle non-zero");
                assert!(submission.dims.width > 0, "submission width positive");
                Err(RingError::NotImplemented {
                    what: concat!($vendor_tag, "::encode_shared"),
                })
            }
        }
    };
}

impl_not_implemented_for_trait!(NvencHandoff, "NvencHandoff");
impl_not_implemented_for_trait!(AmfHandoff, "AmfHandoff");
impl_not_implemented_for_trait!(QsvHandoff, "QsvHandoff");

#[cfg(test)]
mod tests {
    use super::*;

    struct NoopCallback;
    impl EncoderCompletionCallback for NoopCallback {
        fn on_complete(&mut self, _sequence: u64, _encoded_bytes: u32) {}
    }

    fn submission() -> EncoderSubmission {
        EncoderSubmission::new(0xfeed_face, 7, EncoderDims::new(1920, 1080), 42)
    }

    #[test]
    fn nvenc_stub_returns_not_implemented() {
        let mut h = NotImplementedHandoff::nvenc();
        let mut cb = NoopCallback;
        let err = NvencHandoff::encode_shared(&mut h, submission(), &mut cb).err();
        assert!(matches!(err, Some(RingError::NotImplemented { what })
            if what.contains("Nvenc")));
    }

    #[test]
    fn amf_stub_returns_not_implemented() {
        let mut h = NotImplementedHandoff::amf();
        let mut cb = NoopCallback;
        let err = AmfHandoff::encode_shared(&mut h, submission(), &mut cb).err();
        assert!(matches!(err, Some(RingError::NotImplemented { what })
            if what.contains("Amf")));
    }

    #[test]
    fn qsv_stub_returns_not_implemented() {
        let mut h = NotImplementedHandoff::qsv();
        let mut cb = NoopCallback;
        let err = QsvHandoff::encode_shared(&mut h, submission(), &mut cb).err();
        assert!(matches!(err, Some(RingError::NotImplemented { what })
            if what.contains("Qsv")));
    }

    #[test]
    fn encoder_dims_rejects_zero_width_via_assert() {
        let result = std::panic::catch_unwind(|| EncoderDims::new(0, 1080));
        assert!(result.is_err());
    }

    #[test]
    fn nvenc_stub_register_returns_not_implemented() {
        let mut h = NotImplementedHandoff::nvenc();
        let dims = EncoderDims::new(1920, 1080);
        let err = NvencHandoff::register_slot(&mut h, 0xabc, 0, dims).err();
        assert!(matches!(err, Some(EncoderError::NotImplemented { what })
            if what.contains("Nvenc")));
    }

    #[test]
    fn amf_stub_register_returns_not_implemented() {
        let mut h = NotImplementedHandoff::amf();
        let dims = EncoderDims::new(1920, 1080);
        let err = AmfHandoff::register_slot(&mut h, 0xabc, 0, dims).err();
        assert!(matches!(err, Some(EncoderError::NotImplemented { what })
            if what.contains("Amf")));
    }

    #[test]
    fn qsv_stub_register_returns_not_implemented() {
        let mut h = NotImplementedHandoff::qsv();
        let dims = EncoderDims::new(1920, 1080);
        let err = QsvHandoff::register_slot(&mut h, 0xabc, 0, dims).err();
        assert!(matches!(err, Some(EncoderError::NotImplemented { what })
            if what.contains("Qsv")));
    }

    #[test]
    fn stub_poll_completed_returns_none() {
        let mut h_nv = NotImplementedHandoff::nvenc();
        let mut h_amf = NotImplementedHandoff::amf();
        let mut h_qsv = NotImplementedHandoff::qsv();
        let slot = HandoffSlot::new(0, 0xdead);
        assert!(NvencHandoff::poll_completed(&mut h_nv, slot).is_none());
        assert!(AmfHandoff::poll_completed(&mut h_amf, slot).is_none());
        assert!(QsvHandoff::poll_completed(&mut h_qsv, slot).is_none());
    }

    #[test]
    fn stub_unregister_does_not_panic() {
        let mut h = NotImplementedHandoff::nvenc();
        let slot = HandoffSlot::new(0, 0xdead);
        NvencHandoff::unregister_slot(&mut h, slot);
    }

    #[test]
    fn encoded_bitstream_rejects_empty() {
        let result = std::panic::catch_unwind(|| EncodedBitstream::new(vec![], 0, 0, true));
        assert!(result.is_err());
    }

    #[test]
    fn dts_offset_zero_for_no_b_frames() {
        let offset = compute_dts_offset_us(1000, 0, 16_666);
        assert_eq!(offset, 0);
    }

    #[test]
    fn dts_offset_negative_for_b_frames() {
        let offset = compute_dts_offset_us(1000, 2, 16_666);
        assert_eq!(offset, -(2 * 16_666_i64));
    }

    #[test]
    fn dts_offset_application_clamps_to_zero() {
        let dts = apply_dts_offset(100, -1000);
        assert_eq!(dts, 0);
    }

    #[test]
    fn dts_offset_application_below_pts_for_b_frames() {
        let pts = 100_000_u64;
        let dts = apply_dts_offset(pts, -33_333);
        assert!(dts < pts);
        assert_eq!(dts, 66_667);
    }

    #[test]
    fn encoder_frame_rate_derives_interval_and_gop() {
        let sixty = EncoderFrameRate::from_fps(60);
        assert_eq!(sixty.frame_interval_us(), 16_667);
        assert_eq!(sixty.gop_pic_size(), 60);
        let capped = EncoderFrameRate::from_fps(999);
        assert_eq!(capped.numerator, ENCODER_FRAME_RATE_MAX);
        assert_eq!(capped.gop_pic_size(), ENCODER_FRAME_RATE_MAX as u16);
    }

    #[test]
    fn handoff_slot_rejects_zero_handle() {
        let result = std::panic::catch_unwind(|| HandoffSlot::new(0, 0));
        assert!(result.is_err());
    }

    #[test]
    fn submission_capture_pts_defaults_absent_and_round_trips() {
        let s = submission();
        assert_eq!(s.capture_pts_us, None);
        let with_pts = s.with_capture_pts_us(123_456);
        assert_eq!(with_pts.capture_pts_us, Some(123_456));
        assert_eq!(with_pts.sequence, s.sequence);
        assert_eq!(with_pts.shared_handle, s.shared_handle);
    }

    struct CountingCallback {
        seen: Vec<(u64, u32)>,
    }

    impl EncoderCompletionCallback for CountingCallback {
        fn on_complete(&mut self, sequence: u64, encoded_bytes: u32) {
            self.seen.push((sequence, encoded_bytes));
        }
    }

    #[test]
    fn vt_noop_accepts_frames_in_fifo_order() {
        let mut h = VtNoOpHandoff::new();
        let mut cb = CountingCallback { seen: Vec::new() };
        let dims = EncoderDims::new(1920, 1080);
        for seq in 1..=5u64 {
            let s = EncoderSubmission::new(0xfeed_face_u64, 0, dims, seq);
            VideoToolboxHandoff::encode_shared(&mut h, s, &mut cb).expect("vt encode_shared");
        }
        assert_eq!(cb.seen.len(), 5);
        for (idx, &(seq, _)) in cb.seen.iter().enumerate() {
            assert_eq!(seq, (idx as u64) + 1, "fifo sequence");
        }
        assert_eq!(h.accepted_count(), 5);
    }

    #[test]
    fn vt_noop_register_returns_slot() {
        let mut h = VtNoOpHandoff::new();
        let dims = EncoderDims::new(1920, 1080);
        let slot_a =
            VideoToolboxHandoff::register_slot(&mut h, 0xabc, 0, dims).expect("register a");
        let slot_b =
            VideoToolboxHandoff::register_slot(&mut h, 0xdef, 0, dims).expect("register b");
        assert_eq!(slot_a.shared_handle, 0xabc);
        assert_eq!(slot_b.shared_handle, 0xdef);
        assert_ne!(slot_a.slot_index, slot_b.slot_index);
    }

    #[test]
    fn vt_noop_register_rejects_zero_handle() {
        let mut h = VtNoOpHandoff::new();
        let dims = EncoderDims::new(1920, 1080);
        let err = VideoToolboxHandoff::register_slot(&mut h, 0, 0, dims).err();
        assert!(matches!(err, Some(EncoderError::SlotUnknown { .. })));
    }

    #[test]
    fn vt_noop_encode_shared_rejects_zero_handle() {
        let mut h = VtNoOpHandoff::new();
        let mut cb = CountingCallback { seen: Vec::new() };
        let dims = EncoderDims::new(1920, 1080);
        let s = EncoderSubmission {
            shared_handle: 0,
            keyed_mutex_key: 0,
            dims,
            sequence: 1,
            capture_pts_us: None,
        };
        let err = VideoToolboxHandoff::encode_shared(&mut h, s, &mut cb).err();
        assert!(matches!(err, Some(RingError::UnknownSlot)));
        assert!(cb.seen.is_empty(), "no callback on rejection");
    }

    #[test]
    fn vt_noop_encode_shared_async_advances_accepted() {
        let mut h = VtNoOpHandoff::new();
        let dims = EncoderDims::new(1280, 720);
        let slot = HandoffSlot::new(0, 0xfeed);
        let params = PicParams::new(16_666, false);
        VideoToolboxHandoff::encode_shared_async(&mut h, slot, 0, dims, params)
            .expect("async encode ok");
        assert_eq!(h.accepted_count(), 1);
        assert_eq!(h.pending_len(), 1);
    }
}
