// SPDX-License-Identifier: AGPL-3.0-or-later
//
// X11 screen/window capture backend.
//
// The portal (xdg-desktop-portal ScreenCast) backend only works on Wayland;
// xdg-desktop-portal-kde refuses ScreenCast on X11 outright. This backend
// captures directly from the X server via GetImage and feeds NV12 frames into
// the same pipeline the PipeWire path uses, so native screen share works on X11.
//
// v1 uses core-protocol GetImage (no MIT-SHM); this is simple and correct.
// A shared-memory fast path can be layered on later if throughput needs it.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::sync_channel;
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use x11rb::connection::Connection;
use x11rb::protocol::randr::ConnectionExt as _;
use x11rb::protocol::xproto::{ConnectionExt as _, Drawable, ImageFormat, Window};

use crate::frame_buffer_pool::LINUX_FRAME_DIM_MAX;
use crate::nv12_packing::{bgra_to_nv12, Nv12Layout};
use crate::pipewire_stream::{FrameCallback, LifecycleCallback, VideoFrame, VideoFrameData};

const DIM_MAX: u32 = LINUX_FRAME_DIM_MAX as u32;

/// What to capture.
pub enum X11Target {
    /// A single toplevel window, by X11 window id (XID).
    Window(u32),
    /// An explicit rectangle on the root window, in root pixel coordinates.
    /// Used to capture a single monitor of a multi-monitor desktop.
    Region {
        x: i16,
        y: i16,
        width: u32,
        height: u32,
    },
    /// A display. `Some(index)` selects a RandR monitor by index; `None` (or an
    /// out-of-range index) captures the whole root window (all monitors).
    Screen(Option<usize>),
}

pub struct X11VideoStream {
    running: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,
}

impl X11VideoStream {
    /// Opens the capture and starts delivering frames on a background thread.
    /// Returns the stream plus the initial capture dimensions.
    pub fn open(
        target: X11Target,
        frame_rate: u32,
        on_frame: FrameCallback,
        on_lifecycle: LifecycleCallback,
    ) -> Result<(Self, u32, u32), String> {
        let running = Arc::new(AtomicBool::new(true));
        let running_thread = Arc::clone(&running);
        let fps = frame_rate.clamp(1, 240);
        let (ready_tx, ready_rx) = sync_channel::<Result<(u32, u32), String>>(1);
        let handle = std::thread::Builder::new()
            .name("fluxer-x11-capture".into())
            .spawn(move || {
                run_capture(target, fps, on_frame, on_lifecycle, running_thread, ready_tx);
            })
            .map_err(|e| format!("failed to spawn X11 capture thread: {e}"))?;
        match ready_rx.recv() {
            Ok(Ok((w, h))) => Ok((
                Self {
                    running,
                    handle: Some(handle),
                },
                w,
                h,
            )),
            Ok(Err(e)) => {
                let _ = handle.join();
                Err(e)
            }
            Err(_) => {
                let _ = handle.join();
                Err("X11 capture thread exited before signalling readiness".into())
            }
        }
    }
}

impl Drop for X11VideoStream {
    fn drop(&mut self) {
        self.running.store(false, Ordering::Release);
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

struct CaptureRect {
    drawable: Drawable,
    x: i16,
    y: i16,
    width: u32,
    height: u32,
}

/// Resolves the current capture rectangle. Returns `Err` when the target is
/// permanently gone (e.g. window destroyed), `Ok(None)` when it is momentarily
/// uncapturable (e.g. zero-sized), so the loop can keep polling.
fn resolve_rect<C: Connection>(
    conn: &C,
    target: &X11Target,
    root: Window,
) -> Result<Option<CaptureRect>, ()> {
    let (drawable, x, y, mut width, mut height) = match target {
        X11Target::Window(xid) => {
            let geo = conn
                .get_geometry(*xid)
                .map_err(|_| ())?
                .reply()
                .map_err(|_| ())?;
            (*xid as Drawable, 0i16, 0i16, geo.width as u32, geo.height as u32)
        }
        X11Target::Region {
            x,
            y,
            width,
            height,
        } => (root as Drawable, *x, *y, *width, *height),
        X11Target::Screen(index) => {
            if let Some(rect) = index.and_then(|i| monitor_rect(conn, root, i)) {
                rect
            } else {
                let geo = conn
                    .get_geometry(root)
                    .map_err(|_| ())?
                    .reply()
                    .map_err(|_| ())?;
                (root as Drawable, 0i16, 0i16, geo.width as u32, geo.height as u32)
            }
        }
    };
    // NV12 requires even dimensions; clamp to the pipeline's max frame size.
    width = (width & !1).min(DIM_MAX);
    height = (height & !1).min(DIM_MAX);
    if width < 2 || height < 2 {
        return Ok(None);
    }
    Ok(Some(CaptureRect {
        drawable,
        x,
        y,
        width,
        height,
    }))
}

fn monitor_rect<C: Connection>(
    conn: &C,
    root: Window,
    index: usize,
) -> Option<(Drawable, i16, i16, u32, u32)> {
    let monitors = conn.randr_get_monitors(root, true).ok()?.reply().ok()?;
    let monitor = monitors.monitors.get(index)?;
    Some((
        root as Drawable,
        monitor.x,
        monitor.y,
        monitor.width as u32,
        monitor.height as u32,
    ))
}

fn run_capture(
    target: X11Target,
    fps: u32,
    on_frame: FrameCallback,
    on_lifecycle: LifecycleCallback,
    running: Arc<AtomicBool>,
    ready_tx: std::sync::mpsc::SyncSender<Result<(u32, u32), String>>,
) {
    let (conn, screen_num) = match x11rb::connect(None) {
        Ok(pair) => pair,
        Err(err) => {
            let _ = ready_tx.send(Err(format!("X11 connect failed: {err}")));
            return;
        }
    };
    let root = conn.setup().roots[screen_num].root;

    let initial = match resolve_rect(&conn, &target, root) {
        Ok(Some(rect)) => rect,
        Ok(None) => {
            let _ = ready_tx.send(Err("X11 capture target has no capturable area".into()));
            return;
        }
        Err(()) => {
            let _ = ready_tx.send(Err("X11 capture target is not available".into()));
            return;
        }
    };
    let _ = ready_tx.send(Ok((initial.width, initial.height)));

    let base = Instant::now();
    let frame_interval = Duration::from_secs_f64(1.0 / fps as f64);
    let mut scratch: Vec<u8> = Vec::new();

    while running.load(Ordering::Acquire) {
        let loop_start = Instant::now();

        let rect = match resolve_rect(&conn, &target, root) {
            Ok(Some(rect)) => rect,
            Ok(None) => {
                std::thread::sleep(frame_interval);
                continue;
            }
            Err(()) => break, // target destroyed
        };

        let image = match conn.get_image(
            ImageFormat::Z_PIXMAP,
            rect.drawable,
            rect.x,
            rect.y,
            rect.width as u16,
            rect.height as u16,
            !0u32,
        ) {
            Ok(cookie) => match cookie.reply() {
                Ok(reply) => reply,
                Err(_) => {
                    // Momentary failure (e.g. window unmapped); keep polling.
                    std::thread::sleep(frame_interval);
                    continue;
                }
            },
            Err(_) => {
                std::thread::sleep(frame_interval);
                continue;
            }
        };

        let bgra = &image.data;
        let pixels = (rect.width as usize) * (rect.height as usize);
        if pixels == 0 || bgra.len() < pixels * 4 {
            // Only 32-bpp Z_PIXMAP data is supported (depth 24 is padded to 32).
            std::thread::sleep(frame_interval);
            continue;
        }
        let bgra_stride = rect.width * 4;

        let layout = Nv12Layout {
            width: rect.width,
            height: rect.height,
            stride_y: rect.width,
            stride_uv: rect.width,
        };
        let Some(total) = layout.packed_size() else {
            std::thread::sleep(frame_interval);
            continue;
        };
        if scratch.len() != total {
            scratch.resize(total, 0);
        }
        if !bgra_to_nv12(layout, bgra, bgra_stride, &mut scratch, false) {
            std::thread::sleep(frame_interval);
            continue;
        }

        let frame = VideoFrame {
            width: rect.width,
            height: rect.height,
            stride_y: layout.packed_stride_y(),
            stride_uv: layout.packed_stride_uv(),
            timestamp_us: base.elapsed().as_micros().min(i64::MAX as u128) as i64,
            data: VideoFrameData::from_vec(std::mem::take(&mut scratch)),
            dmabuf: None,
        };
        on_frame(frame);

        let elapsed = loop_start.elapsed();
        if elapsed < frame_interval {
            std::thread::sleep(frame_interval - elapsed);
        }
    }

    on_lifecycle("closed-clean", "X11 capture stopped");
}
