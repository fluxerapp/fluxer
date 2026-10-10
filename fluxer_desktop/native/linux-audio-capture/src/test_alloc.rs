// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::Ordering;

pub(crate) static ALLOC_PROBE: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

thread_local! {
    pub(crate) static THREAD_ALLOC_TRACK: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    pub(crate) static THREAD_ALLOC_COUNT: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

struct CountingAllocator;

unsafe impl std::alloc::GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        ALLOC_PROBE.fetch_add(1, Ordering::Relaxed);
        let _ = THREAD_ALLOC_TRACK.try_with(|tracked| {
            if tracked.get() {
                let _ = THREAD_ALLOC_COUNT.try_with(|c| c.set(c.get().saturating_add(1)));
            }
        });
        unsafe { std::alloc::System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        unsafe { std::alloc::System.dealloc(ptr, layout) }
    }
}

#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

pub(crate) fn begin_thread_alloc_probe() {
    THREAD_ALLOC_COUNT.with(|c| c.set(0));
    THREAD_ALLOC_TRACK.with(|t| t.set(true));
}

pub(crate) fn end_thread_alloc_probe() -> usize {
    THREAD_ALLOC_TRACK.with(|t| t.set(false));
    THREAD_ALLOC_COUNT.with(|c| c.get())
}
