#![allow(non_snake_case, non_camel_case_types)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use core::ffi::c_void;
use core::ptr;
use std::sync::{
    Mutex, MutexGuard,
    atomic::{AtomicPtr, AtomicU64, Ordering},
};

use objc2::rc::Retained;
use objc2::runtime::{NSObject, NSObjectProtocol, ProtocolObject};
use objc2::{AllocAnyThread, DefinedClass, define_class, msg_send};
use objc2_core_foundation::CFAllocator;
use objc2_core_media::CMSampleBuffer;
use objc2_foundation::NSError;
use objc2_screen_capture_kit::{SCStream, SCStreamDelegate, SCStreamOutput, SCStreamOutputType};

use crate::audio_converter::{self as ac, AudioBufferList, AudioStreamBasicDescription};
use crate::pcm_pool::{PcmFramePool, PooledPcmFrame};
use crate::source_state::Machine;

const MAX_CALLBACK_INPUT_FRAMES: u32 = 48_000;
const MIN_INPUT_SAMPLE_RATE: f64 = 8_000.0;
const MAX_ABL_BUFFERS: usize = 32;

pub type PcmCallback =
    unsafe extern "C" fn(ctx: *mut c_void, slot: *mut Option<PooledPcmFrame>, frames: u32);
pub type StopCallback = unsafe extern "C" fn(ctx: *mut c_void, err: *mut NSError);

pub struct SourceOptions {
    pub target_sample_rate: f64,
    pub target_channels: u32,
}

impl Default for SourceOptions {
    fn default() -> Self {
        Self {
            target_sample_rate: 48_000.0,
            target_channels: 2,
        }
    }
}

const EMPTY_AUDIO_BUFFER: ac::AudioBuffer = ac::AudioBuffer {
    m_number_channels: 0,
    m_data_byte_size: 0,
    m_data: ptr::null_mut(),
};

#[repr(C, align(16))]
struct AblScratch {
    n_buffers: u32,
    _pad: u32,
    buffers: [ac::AudioBuffer; MAX_ABL_BUFFERS],
}

impl AblScratch {
    const fn empty() -> Self {
        Self {
            n_buffers: 0,
            _pad: 0,
            buffers: [EMPTY_AUDIO_BUFFER; MAX_ABL_BUFFERS],
        }
    }
}

const _: () = {
    assert!(core::mem::align_of::<AblScratch>() == 16);
    assert!(
        core::mem::size_of::<AblScratch>()
            >= core::mem::size_of::<u32>()
                + MAX_ABL_BUFFERS * core::mem::size_of::<ac::AudioBuffer>()
    );
    assert!(core::mem::offset_of!(AblScratch, buffers) == 8);
    assert!(core::mem::size_of::<AblScratch>() >= core::mem::size_of::<AudioBufferList>());
};

pub struct Source {
    pub delegate: Retained<FluxerSCKAudioSource>,
    pub state: Machine,
    pub target_sample_rate: f64,
    pub target_channels: u32,
    scratch: Mutex<SourceScratch>,
    callbacks: Mutex<Callbacks>,
    pub dropped_buffers: AtomicU64,
    pub output_queue: dispatch2::DispatchRetained<dispatch2::DispatchQueue>,
}

struct SourceScratch {
    abl: AblScratch,
}

struct Callbacks {
    pcm_callback: Option<PcmCallback>,
    pcm_callback_ctx: *mut c_void,
    stop_callback: Option<StopCallback>,
    stop_callback_ctx: *mut c_void,
    pcm_pool: Option<PcmFramePool>,
}

unsafe impl Send for Source {}
unsafe impl Sync for Source {}

#[derive(Debug)]
pub enum SourceCreateError {
    DispatchQueue,
}

#[derive(Debug)]
pub struct ClassRegistrationError;

pub struct DelegateIvars {
    pub source: AtomicPtr<Source>,
}

define_class!(
    #[unsafe(super(NSObject))]
    #[name = "FluxerSCKAudioSource"]
    #[ivars = DelegateIvars]
    pub struct FluxerSCKAudioSource;

    unsafe impl NSObjectProtocol for FluxerSCKAudioSource {}

    unsafe impl SCStreamDelegate for FluxerSCKAudioSource {
        #[unsafe(method(stream:didStopWithError:))]
        unsafe fn did_stop_with_error(&self, _stream: &SCStream, err: &NSError) {
            let ptr = self.ivars().source.load(Ordering::Acquire);
            if ptr.is_null() {
                return;
            }
            let src = unsafe { &*ptr };
            let _ = src.state.request_stop();
            let _ = src.state.mark_stopped();
            let (cb, ctx) = {
                let callbacks = src.lock_callbacks();
                (callbacks.stop_callback, callbacks.stop_callback_ctx)
            };
            if let Some(cb) = cb {
                unsafe {
                    cb(ctx, err as *const NSError as *mut NSError);
                }
            }
        }
    }

    unsafe impl SCStreamOutput for FluxerSCKAudioSource {
        #[unsafe(method(stream:didOutputSampleBuffer:ofType:))]
        unsafe fn did_output_sample_buffer(
            &self,
            _stream: &SCStream,
            sample_buffer: &CMSampleBuffer,
            output_type: SCStreamOutputType,
        ) {
            if output_type != SCStreamOutputType::Audio {
                return;
            }
            let ptr = self.ivars().source.load(Ordering::Acquire);
            if ptr.is_null() {
                return;
            }
            unsafe {
                handle_audio_sample(&*ptr, sample_buffer);
            }
        }
    }
);

impl FluxerSCKAudioSource {
    pub fn new() -> Retained<Self> {
        let this = Self::alloc().set_ivars(DelegateIvars {
            source: AtomicPtr::new(core::ptr::null_mut()),
        });
        unsafe { msg_send![super(this), init] }
    }
}

struct RetainedBlockGuard(*mut objc2_core_media::CMBlockBuffer);

impl Drop for RetainedBlockGuard {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe {
                let _ = objc2_core_foundation::CFRetained::from_raw(
                    core::ptr::NonNull::new_unchecked(self.0),
                );
            }
            self.0 = ptr::null_mut();
        }
    }
}

enum AblFillOutcome {
    Filled(RetainedBlockGuard),
    TooLarge,
    Unavailable,
}

fn read_stream_asbd(sample_buffer: &CMSampleBuffer) -> Option<AudioStreamBasicDescription> {
    let desc = unsafe { sample_buffer.format_description() }?;
    unsafe extern "C-unwind" {
        fn CMAudioFormatDescriptionGetStreamBasicDescription(
            desc: &objc2_core_media::CMFormatDescription,
        ) -> *const AudioStreamBasicDescription;
    }
    let asbd_ptr = unsafe { CMAudioFormatDescriptionGetStreamBasicDescription(&desc) };
    if asbd_ptr.is_null() {
        return None;
    }
    Some(unsafe { *asbd_ptr })
}

unsafe fn admit_sample(
    src: &Source,
    sample_buffer: &CMSampleBuffer,
) -> Option<(u32, AudioStreamBasicDescription)> {
    if !unsafe { sample_buffer.data_is_ready() } {
        return None;
    }
    let num_samples = unsafe { sample_buffer.num_samples() };
    if num_samples <= 0 || (num_samples as u32) > MAX_CALLBACK_INPUT_FRAMES {
        return None;
    }
    let asbd = read_stream_asbd(sample_buffer)?;
    if !asbd.m_sample_rate.is_finite() {
        src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
        return None;
    }
    if asbd.m_sample_rate < MIN_INPUT_SAMPLE_RATE {
        src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
        return None;
    }
    Some((num_samples as u32, asbd))
}

unsafe fn fill_abl_scratch(sample_buffer: &CMSampleBuffer, abl: &mut AblScratch) -> AblFillOutcome {
    assert!(core::mem::size_of::<AblScratch>() >= core::mem::size_of::<AudioBufferList>());
    let mut abl_size: usize = 0;
    let size_status = unsafe {
        sample_buffer.audio_buffer_list_with_retained_block_buffer(
            &mut abl_size,
            ptr::null_mut(),
            0,
            None::<&CFAllocator>,
            None::<&CFAllocator>,
            0,
            ptr::null_mut(),
        )
    };
    if size_status != 0 || abl_size < core::mem::size_of::<AudioBufferList>() {
        return AblFillOutcome::Unavailable;
    }
    if abl_size > core::mem::size_of::<AblScratch>() {
        return AblFillOutcome::TooLarge;
    }
    let abl_ptr = (abl as *mut AblScratch) as *mut objc2_core_audio_types::AudioBufferList;
    let mut retained_block: *mut objc2_core_media::CMBlockBuffer = ptr::null_mut();
    let list_status = unsafe {
        sample_buffer.audio_buffer_list_with_retained_block_buffer(
            ptr::null_mut(),
            abl_ptr,
            core::mem::size_of::<AblScratch>(),
            None::<&CFAllocator>,
            None::<&CFAllocator>,
            0,
            &mut retained_block,
        )
    };
    let guard = RetainedBlockGuard(retained_block);
    if list_status != 0 {
        return AblFillOutcome::Unavailable;
    }
    if abl.n_buffers as usize > MAX_ABL_BUFFERS {
        return AblFillOutcome::Unavailable;
    }
    AblFillOutcome::Filled(guard)
}

unsafe fn handle_audio_sample(src: &Source, sample_buffer: &CMSampleBuffer) {
    let Some((frames, asbd)) = (unsafe { admit_sample(src, sample_buffer) }) else {
        return;
    };
    let (cb, ctx, pool) = {
        let callbacks = src.lock_callbacks();
        (
            callbacks.pcm_callback,
            callbacks.pcm_callback_ctx,
            callbacks.pcm_pool.clone(),
        )
    };
    let Some(cb) = cb else { return };
    let Some(pool) = pool else { return };
    let out_capacity =
        ac::output_frame_capacity(frames, asbd.m_sample_rate, src.target_sample_rate);
    let needed = (out_capacity as usize) * (src.target_channels as usize);
    if needed > pool.samples_per_slot() as usize {
        src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
        return;
    }
    let mut scratch = match src.scratch.try_lock() {
        Ok(guard) => guard,
        Err(_) => {
            src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
            return;
        }
    };
    let block_guard = match unsafe { fill_abl_scratch(sample_buffer, &mut scratch.abl) } {
        AblFillOutcome::Filled(guard) => guard,
        AblFillOutcome::TooLarge => {
            src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
            return;
        }
        AblFillOutcome::Unavailable => return,
    };
    let Some(mut slot) = pool.try_acquire() else {
        src.dropped_buffers.fetch_add(1, Ordering::Relaxed);
        return;
    };
    let out_frames = unsafe {
        ac::convert_buffer_list_to_interleaved_f32(
            &asbd,
            (&raw const scratch.abl) as *const AudioBufferList,
            frames,
            src.target_sample_rate,
            src.target_channels,
            slot.unfilled_mut(),
        )
    };
    drop(block_guard);
    drop(scratch);
    let frames_emitted = match out_frames {
        Ok(0) | Err(_) => return,
        Ok(n) => n,
    };
    let filled = (frames_emitted as usize) * (src.target_channels as usize);
    assert!(filled <= slot.capacity());
    slot.set_filled_len(filled);
    let mut handoff = Some(slot);
    unsafe {
        cb(
            ctx,
            &mut handoff as *mut Option<PooledPcmFrame>,
            frames_emitted,
        );
    }
}

impl Source {
    pub fn create(opts: SourceOptions) -> Result<Box<Source>, SourceCreateError> {
        assert!(opts.target_sample_rate > 0.0);
        assert!(opts.target_channels > 0);
        let delegate = FluxerSCKAudioSource::new();

        let queue_attr = dispatch2::DispatchQueueAttr::with_qos_class(
            dispatch2::DispatchQueueAttr::SERIAL,
            dispatch2::DispatchQoS::UserInteractive,
            0,
        );
        let queue =
            dispatch2::DispatchQueue::new("app.fluxer.mac-app-audio.sck", Some(&queue_attr));

        let mut src = Box::new(Source {
            delegate,
            state: Machine::new(),
            target_sample_rate: opts.target_sample_rate,
            target_channels: opts.target_channels,
            scratch: Mutex::new(SourceScratch {
                abl: AblScratch::empty(),
            }),
            callbacks: Mutex::new(Callbacks {
                pcm_callback: None,
                pcm_callback_ctx: ptr::null_mut(),
                stop_callback: None,
                stop_callback_ctx: ptr::null_mut(),
                pcm_pool: None,
            }),
            dropped_buffers: AtomicU64::new(0),
            output_queue: queue,
        });

        let raw: *mut Source = &mut *src;
        src.delegate.ivars().source.store(raw, Ordering::Release);
        Ok(src)
    }

    fn lock_callbacks(&self) -> MutexGuard<'_, Callbacks> {
        self.callbacks
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    pub fn set_pcm_callback(&mut self, cb: PcmCallback, ctx: *mut c_void) {
        let mut callbacks = self.lock_callbacks();
        callbacks.pcm_callback = Some(cb);
        callbacks.pcm_callback_ctx = ctx;
    }

    pub fn set_pcm_pool(&mut self, pool: PcmFramePool) {
        assert!(pool.capacity() > 0);
        assert!(pool.samples_per_slot() > 0);
        let mut callbacks = self.lock_callbacks();
        callbacks.pcm_pool = Some(pool);
    }

    pub fn set_stop_callback(&mut self, cb: StopCallback, ctx: *mut c_void) {
        let mut callbacks = self.lock_callbacks();
        callbacks.stop_callback = Some(cb);
        callbacks.stop_callback_ctx = ctx;
    }

    pub fn clear_stop_callback(&mut self) {
        let mut callbacks = self.lock_callbacks();
        callbacks.stop_callback = None;
        callbacks.stop_callback_ctx = ptr::null_mut();
    }

    pub fn delegate_as_output(&self) -> &ProtocolObject<dyn SCStreamOutput> {
        ProtocolObject::from_ref(&*self.delegate)
    }

    pub fn delegate_as_delegate(&self) -> &ProtocolObject<dyn SCStreamDelegate> {
        ProtocolObject::from_ref(&*self.delegate)
    }

    pub fn output_queue(&self) -> &dispatch2::DispatchQueue {
        &self.output_queue
    }
}

impl Drop for Source {
    fn drop(&mut self) {
        self.delegate
            .ivars()
            .source
            .store(ptr::null_mut(), Ordering::Release);
    }
}
