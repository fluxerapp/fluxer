// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::StagingBackend;
use fluxer_gpu_rebuild::{GpuLossCallback, GpuRebuildError};

pub const MIN_STAGING_BYTES: u64 = 1;
pub const MAX_STAGING_BYTES: u64 = 1 << 30;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WgpuStagingConfig {
    pub byte_len: u64,
}

impl WgpuStagingConfig {
    pub fn new(byte_len: u64) -> Self {
        assert!(byte_len >= MIN_STAGING_BYTES, "byte_len must be positive");
        assert!(byte_len <= MAX_STAGING_BYTES, "byte_len exceeds sanity cap");
        Self { byte_len }
    }
}

struct WgpuStagingResources {
    buffer: wgpu::Buffer,
    cpu_mirror: Vec<u8>,
    ready: bool,
}

pub struct WgpuStagingBackend {
    config: WgpuStagingConfig,
    resources: Option<WgpuStagingResources>,
}

impl WgpuStagingBackend {
    pub fn new(device: &wgpu::Device, config: WgpuStagingConfig) -> Self {
        assert!(config.byte_len >= MIN_STAGING_BYTES, "config min invariant");
        assert!(config.byte_len <= MAX_STAGING_BYTES, "config max invariant");
        let resources = build_resources(device, config);
        Self {
            config,
            resources: Some(resources),
        }
    }

    pub fn new_unbuilt(config: WgpuStagingConfig) -> Self {
        assert!(config.byte_len >= MIN_STAGING_BYTES, "config min invariant");
        assert!(config.byte_len <= MAX_STAGING_BYTES, "config max invariant");
        Self {
            config,
            resources: None,
        }
    }

    pub fn config(&self) -> WgpuStagingConfig {
        assert!(
            self.config.byte_len >= MIN_STAGING_BYTES,
            "config min invariant"
        );
        assert!(
            self.config.byte_len <= MAX_STAGING_BYTES,
            "config max invariant"
        );
        self.config
    }

    pub fn is_built(&self) -> bool {
        let built = self.resources.is_some();
        assert!(
            self.config.byte_len >= MIN_STAGING_BYTES,
            "config min while introspecting"
        );
        assert!(
            self.config.byte_len <= MAX_STAGING_BYTES,
            "config max while introspecting"
        );
        built
    }

    pub fn buffer(&self) -> Option<&wgpu::Buffer> {
        let buf = self.resources.as_ref().map(|r| &r.buffer);
        assert_eq!(
            buf.is_some(),
            self.is_built(),
            "buffer presence must align with built state",
        );
        buf
    }
}

impl StagingBackend for WgpuStagingBackend {
    fn write<F: FnOnce(&mut [u8])>(&mut self, fill: F) {
        let Some(resources) = self.resources.as_mut() else {
            return;
        };
        fill(&mut resources.cpu_mirror);
        resources.ready = true;
    }

    fn read<R, F: FnOnce(&[u8]) -> R>(&self, read: F) -> R {
        let empty: &[u8] = &[];
        match self.resources.as_ref() {
            Some(r) => read(&r.cpu_mirror),
            None => read(empty),
        }
    }

    fn is_ready(&self) -> bool {
        match self.resources.as_ref() {
            Some(r) => r.ready,
            None => false,
        }
    }

    fn is_idle(&self) -> bool {
        match self.resources.as_ref() {
            Some(r) => !r.ready,
            None => true,
        }
    }
}

impl GpuLossCallback for WgpuStagingBackend {
    fn release(&mut self) {
        assert!(
            self.config.byte_len >= MIN_STAGING_BYTES,
            "release config min invariant"
        );
        assert!(
            self.config.byte_len <= MAX_STAGING_BYTES,
            "release config max invariant"
        );
        self.resources = None;
        assert!(!self.is_built(), "release postcondition: must be unbuilt");
    }

    fn rebuild(
        &mut self,
        device: &wgpu::Device,
        _queue: &wgpu::Queue,
    ) -> Result<(), GpuRebuildError> {
        assert!(
            self.config.byte_len >= MIN_STAGING_BYTES,
            "rebuild config min invariant"
        );
        assert!(
            self.config.byte_len <= MAX_STAGING_BYTES,
            "rebuild config max invariant"
        );
        if self.resources.is_some() {
            return Err(GpuRebuildError::OwnerInvariantBroken {
                reason: "rebuild without prior release",
            });
        }
        let resources = build_resources(device, self.config);
        self.resources = Some(resources);
        assert!(self.is_built(), "rebuild postcondition: must be built");
        Ok(())
    }

    fn is_ready(&self) -> bool {
        self.is_built()
    }

    fn debug_label(&self) -> &'static str {
        "screen_frame_bus.wgpu_staging_backend"
    }
}

fn build_resources(device: &wgpu::Device, config: WgpuStagingConfig) -> WgpuStagingResources {
    assert!(
        config.byte_len >= MIN_STAGING_BYTES,
        "build_resources min invariant"
    );
    assert!(
        config.byte_len <= MAX_STAGING_BYTES,
        "build_resources max invariant"
    );
    let buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("screen_frame_bus.wgpu_staging_buffer"),
        size: config.byte_len,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });
    let cpu_mirror = vec![0u8; config.byte_len as usize];
    assert_eq!(
        cpu_mirror.len() as u64,
        config.byte_len,
        "cpu mirror must match configured byte len",
    );
    WgpuStagingResources {
        buffer,
        cpu_mirror,
        ready: false,
    }
}

#[cfg(test)]
mod tests {
    use super::StagingBackend;
    use crate::CpuStagingBackend;

    #[test]
    fn cpu_backend_still_works_alongside_wgpu_backend() {
        let cpu = CpuStagingBackend::new(64);
        assert!(<CpuStagingBackend as StagingBackend>::is_idle(&cpu));
        assert!(<CpuStagingBackend as StagingBackend>::is_ready(&cpu));
    }
}
