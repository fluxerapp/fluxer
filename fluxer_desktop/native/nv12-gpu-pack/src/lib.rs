// SPDX-License-Identifier: AGPL-3.0-or-later

use bytemuck::{Pod, Zeroable};
use fluxer_gpu_rebuild::{GpuLossCallback, GpuRebuildError};
use parking_lot::Mutex;
use std::num::NonZeroU64;
use wgpu::util::DeviceExt;

const WORKGROUP_DIM: u32 = 8;
const Y_BYTES_PER_PIXEL: u64 = 1;
const UV_BYTES_PER_PIXEL_PAIR: u64 = 2;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nv12PackError {
    DimensionsExceedMax {
        requested: (u32, u32),
        max: (u32, u32),
    },
    DimensionsNotEven {
        requested: (u32, u32),
    },
    DimensionsZero,
    YBufferTooSmall {
        required: u64,
        actual: u64,
    },
    UvBufferTooSmall {
        required: u64,
        actual: u64,
    },
    StrideNotAligned {
        stride: u32,
    },
    NotReady,
}

impl std::fmt::Display for Nv12PackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DimensionsExceedMax { requested, max } => {
                write!(f, "dimensions {requested:?} exceed packer maximum {max:?}")
            }
            Self::DimensionsNotEven { requested } => {
                write!(f, "dimensions {requested:?} must be even for NV12")
            }
            Self::DimensionsZero => write!(f, "dimensions must be non-zero"),
            Self::YBufferTooSmall { required, actual } => write!(
                f,
                "Y buffer too small: required {required} bytes, actual {actual}"
            ),
            Self::UvBufferTooSmall { required, actual } => write!(
                f,
                "UV buffer too small: required {required} bytes, actual {actual}"
            ),
            Self::StrideNotAligned { stride } => {
                write!(
                    f,
                    "stride {stride} must be a multiple of 4 for word-packed writes"
                )
            }
            Self::NotReady => write!(f, "packer pipeline is released; rebuild required"),
        }
    }
}

impl std::error::Error for Nv12PackError {}

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct PackUniforms {
    width: u32,
    height: u32,
    stride_y: u32,
    stride_uv: u32,
}

struct PackResources {
    pipeline: wgpu::ComputePipeline,
    bind_group_layout: wgpu::BindGroupLayout,
    uniform_buffer: wgpu::Buffer,
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct CachedBindGroupKey {
    source_view: wgpu::TextureView,
    y_buf: wgpu::Buffer,
    uv_buf: wgpu::Buffer,
}

struct CachedBindGroup {
    key: CachedBindGroupKey,
    bind_group: wgpu::BindGroup,
}

pub struct Nv12Packer {
    resources: Option<PackResources>,
    bind_group_cache: Mutex<Option<CachedBindGroup>>,
    max_width: u32,
    max_height: u32,
}

pub struct PackJob<'a> {
    pub device: &'a wgpu::Device,
    pub queue: &'a wgpu::Queue,
    pub encoder: &'a mut wgpu::CommandEncoder,
    pub source: &'a wgpu::TextureView,
    pub y_out: &'a wgpu::Buffer,
    pub uv_out: &'a wgpu::Buffer,
    pub dims: (u32, u32),
}

impl Nv12Packer {
    pub fn new(device: &wgpu::Device, max_width: u32, max_height: u32) -> Self {
        assert!(max_width > 0, "max_width must be positive");
        assert!(max_height > 0, "max_height must be positive");
        assert!(
            max_width.is_multiple_of(2),
            "max_width must be even for NV12"
        );
        assert!(
            max_height.is_multiple_of(2),
            "max_height must be even for NV12"
        );
        let resources = build_resources(device);
        Self {
            resources: Some(resources),
            bind_group_cache: Mutex::new(None),
            max_width,
            max_height,
        }
    }

    pub fn new_unbuilt(max_width: u32, max_height: u32) -> Self {
        assert!(max_width > 0, "max_width must be positive");
        assert!(max_height > 0, "max_height must be positive");
        assert!(
            max_width.is_multiple_of(2),
            "max_width must be even for NV12"
        );
        assert!(
            max_height.is_multiple_of(2),
            "max_height must be even for NV12"
        );
        Self {
            resources: None,
            bind_group_cache: Mutex::new(None),
            max_width,
            max_height,
        }
    }

    pub fn max_width(&self) -> u32 {
        assert!(self.max_width > 0, "max_width invariant");
        assert!(self.max_width.is_multiple_of(2), "max_width even invariant");
        self.max_width
    }

    pub fn max_height(&self) -> u32 {
        assert!(self.max_height > 0, "max_height invariant");
        assert!(
            self.max_height.is_multiple_of(2),
            "max_height even invariant"
        );
        self.max_height
    }

    pub fn is_built(&self) -> bool {
        let built = self.resources.is_some();
        assert!(
            self.max_width > 0,
            "max_width must be positive while introspecting state"
        );
        assert!(
            self.max_height > 0,
            "max_height must be positive while introspecting state"
        );
        built
    }

    pub fn y_plane_size(width: u32, height: u32) -> u64 {
        assert!(width > 0, "y_plane_size width must be positive");
        assert!(height > 0, "y_plane_size height must be positive");
        u64::from(width) * u64::from(height) * Y_BYTES_PER_PIXEL
    }

    pub fn uv_plane_size(width: u32, height: u32) -> u64 {
        assert!(width > 0, "uv_plane_size width must be positive");
        assert!(height > 0, "uv_plane_size height must be positive");
        assert!(width.is_multiple_of(2), "uv plane requires even width");
        assert!(height.is_multiple_of(2), "uv plane requires even height");
        u64::from(width) * u64::from(height) * UV_BYTES_PER_PIXEL_PAIR / 4
    }

    pub fn pack(&self, mut job: PackJob<'_>) -> Result<(), Nv12PackError> {
        let (width, height) = job.dims;
        let resources = match self.resources.as_ref() {
            Some(r) => r,
            None => return Err(Nv12PackError::NotReady),
        };
        self.validate(width, height, job.y_out, job.uv_out)?;
        assert!(width <= self.max_width, "validated width must respect max");
        assert!(
            height <= self.max_height,
            "validated height must respect max"
        );
        let bind_group = self.acquire_bind_group(resources, &job);
        record_pack_pass(resources, &bind_group, &mut job);
        Ok(())
    }

    fn acquire_bind_group(&self, resources: &PackResources, job: &PackJob<'_>) -> wgpu::BindGroup {
        let key = CachedBindGroupKey {
            source_view: job.source.clone(),
            y_buf: job.y_out.clone(),
            uv_buf: job.uv_out.clone(),
        };
        let mut cache = self.bind_group_cache.lock();
        if let Some(cached) = cache.as_ref()
            && cached.key == key
        {
            return cached.bind_group.clone();
        }
        let bind_group = build_bind_group(resources, job);
        *cache = Some(CachedBindGroup {
            key,
            bind_group: bind_group.clone(),
        });
        assert!(
            cache.is_some(),
            "bind group cache must be populated after rebuild"
        );
        bind_group
    }

    pub fn cached_bind_group_count(&self) -> usize {
        let cache = self.bind_group_cache.lock();
        match cache.as_ref() {
            Some(_) => 1,
            None => 0,
        }
    }

    fn validate(
        &self,
        width: u32,
        height: u32,
        y_out: &wgpu::Buffer,
        uv_out: &wgpu::Buffer,
    ) -> Result<(), Nv12PackError> {
        assert!(self.max_width > 0, "validate requires positive max_width");
        assert!(self.max_height > 0, "validate requires positive max_height");
        if width == 0 {
            return Err(Nv12PackError::DimensionsZero);
        }
        if height == 0 {
            return Err(Nv12PackError::DimensionsZero);
        }
        if width > self.max_width {
            return Err(Nv12PackError::DimensionsExceedMax {
                requested: (width, height),
                max: (self.max_width, self.max_height),
            });
        }
        if height > self.max_height {
            return Err(Nv12PackError::DimensionsExceedMax {
                requested: (width, height),
                max: (self.max_width, self.max_height),
            });
        }
        if !width.is_multiple_of(2) {
            return Err(Nv12PackError::DimensionsNotEven {
                requested: (width, height),
            });
        }
        if !height.is_multiple_of(2) {
            return Err(Nv12PackError::DimensionsNotEven {
                requested: (width, height),
            });
        }
        if !width.is_multiple_of(4) {
            return Err(Nv12PackError::StrideNotAligned { stride: width });
        }
        let required_y = Self::y_plane_size(width, height);
        let actual_y = y_out.size();
        if actual_y < required_y {
            return Err(Nv12PackError::YBufferTooSmall {
                required: required_y,
                actual: actual_y,
            });
        }
        let required_uv = Self::uv_plane_size(width, height);
        let actual_uv = uv_out.size();
        if actual_uv < required_uv {
            return Err(Nv12PackError::UvBufferTooSmall {
                required: required_uv,
                actual: actual_uv,
            });
        }
        Ok(())
    }
}

impl GpuLossCallback for Nv12Packer {
    fn release(&mut self) {
        assert!(self.max_width > 0, "release precondition: max_width valid");
        assert!(
            self.max_height > 0,
            "release precondition: max_height valid"
        );
        self.resources = None;
        {
            let mut cache = self.bind_group_cache.lock();
            *cache = None;
            assert!(
                cache.is_none(),
                "release postcondition: cache must be cleared"
            );
        }
        assert!(!self.is_built(), "release postcondition: must be unbuilt");
    }

    fn rebuild(
        &mut self,
        device: &wgpu::Device,
        _queue: &wgpu::Queue,
    ) -> Result<(), GpuRebuildError> {
        assert!(self.max_width > 0, "rebuild precondition: max_width valid");
        assert!(
            self.max_height > 0,
            "rebuild precondition: max_height valid"
        );
        if self.resources.is_some() {
            return Err(GpuRebuildError::OwnerInvariantBroken {
                reason: "rebuild without prior release",
            });
        }
        let resources = build_resources(device);
        self.resources = Some(resources);
        assert!(self.is_built(), "rebuild postcondition: must be built");
        Ok(())
    }

    fn is_ready(&self) -> bool {
        self.is_built()
    }

    fn debug_label(&self) -> &'static str {
        "nv12_gpu_pack.packer"
    }
}

fn record_pack_pass(
    resources: &PackResources,
    bind_group: &wgpu::BindGroup,
    job: &mut PackJob<'_>,
) {
    let (width, height) = job.dims;
    assert!(
        width.is_multiple_of(2),
        "record_pack_pass width even precondition"
    );
    assert!(
        height.is_multiple_of(2),
        "record_pack_pass height even precondition"
    );
    let uniforms = PackUniforms {
        width,
        height,
        stride_y: width,
        stride_uv: width,
    };
    job.queue
        .write_buffer(&resources.uniform_buffer, 0, bytemuck::bytes_of(&uniforms));
    job.encoder
        .clear_buffer(job.y_out, 0, Some(Nv12Packer::y_plane_size(width, height)));
    job.encoder.clear_buffer(
        job.uv_out,
        0,
        Some(Nv12Packer::uv_plane_size(width, height)),
    );
    let mut pass = job
        .encoder
        .begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: Some("nv12_gpu_pack.pass"),
            timestamp_writes: None,
        });
    pass.set_pipeline(&resources.pipeline);
    pass.set_bind_group(0, bind_group, &[]);
    let groups_x = (width / 2).div_ceil(WORKGROUP_DIM);
    let groups_y = (height / 2).div_ceil(WORKGROUP_DIM);
    assert!(groups_x > 0, "record_pack_pass groups_x positive");
    assert!(groups_y > 0, "record_pack_pass groups_y positive");
    pass.dispatch_workgroups(groups_x, groups_y, 1);
}

fn build_bind_group(resources: &PackResources, job: &PackJob<'_>) -> wgpu::BindGroup {
    job.device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("nv12_gpu_pack.bind_group"),
        layout: &resources.bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(job.source),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: job.y_out.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: job.uv_out.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 3,
                resource: resources.uniform_buffer.as_entire_binding(),
            },
        ],
    })
}

fn build_resources(device: &wgpu::Device) -> PackResources {
    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some("nv12_gpu_pack.shader"),
        source: wgpu::ShaderSource::Wgsl(include_str!("shader.wgsl").into()),
    });
    let bind_group_layout = create_bind_group_layout(device);
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("nv12_gpu_pack.pipeline_layout"),
        bind_group_layouts: &[Some(&bind_group_layout)],
        immediate_size: 0,
    });
    let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: Some("nv12_gpu_pack.pipeline"),
        layout: Some(&pipeline_layout),
        module: &shader,
        entry_point: Some("pack_nv12"),
        compilation_options: wgpu::PipelineCompilationOptions::default(),
        cache: None,
    });
    let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("nv12_gpu_pack.uniforms"),
        contents: bytemuck::bytes_of(&PackUniforms {
            width: 0,
            height: 0,
            stride_y: 0,
            stride_uv: 0,
        }),
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
    });
    PackResources {
        pipeline,
        bind_group_layout,
        uniform_buffer,
    }
}

fn create_bind_group_layout(device: &wgpu::Device) -> wgpu::BindGroupLayout {
    device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("nv12_gpu_pack.bind_group_layout"),
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::COMPUTE,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: false },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 1,
                visibility: wgpu::ShaderStages::COMPUTE,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: false },
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 2,
                visibility: wgpu::ShaderStages::COMPUTE,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: false },
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 3,
                visibility: wgpu::ShaderStages::COMPUTE,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: NonZeroU64::new(std::mem::size_of::<PackUniforms>() as u64),
                },
                count: None,
            },
        ],
    })
}

pub fn try_acquire_device() -> Option<(wgpu::Device, wgpu::Queue, wgpu::Instance)> {
    let mut descriptor = wgpu::InstanceDescriptor::new_without_display_handle();
    descriptor.backends = wgpu::Backends::all() | wgpu::Backends::SECONDARY;
    let instance = wgpu::Instance::new(descriptor);
    let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::default(),
        force_fallback_adapter: false,
        compatible_surface: None,
    }))
    .ok()?;
    let device_result = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
        label: Some("nv12_gpu_pack.test_device"),
        required_features: wgpu::Features::empty(),
        required_limits: wgpu::Limits::default(),
        memory_hints: wgpu::MemoryHints::default(),
        trace: wgpu::Trace::Off,
        experimental_features: wgpu::ExperimentalFeatures::default(),
    }));
    match device_result {
        Ok((device, queue)) => Some((device, queue, instance)),
        Err(_) => None,
    }
}
