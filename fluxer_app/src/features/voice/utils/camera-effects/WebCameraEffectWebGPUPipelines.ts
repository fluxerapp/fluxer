// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	WEB_GPU_SHADER_STAGE_COMPUTE,
	WEB_GPU_SHADER_STAGE_FRAGMENT,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGPUConstants';
import {
	CAMERA_BLUR_SHADER,
	CAMERA_COMPOSITE_SHADER,
	CAMERA_COPY_SHADER,
	CAMERA_COVER_SHADER,
	CAMERA_MASK_SHADER,
	CAMERA_PREPROCESS_SHADER,
	CAMERA_SOURCE_SHADER,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGPUShaders';

export interface WebCameraEffectWebGPUPipelines {
	readonly sourceLayout: GPUBindGroupLayout;
	readonly preprocessLayout: GPUBindGroupLayout;
	readonly blurLayout: GPUBindGroupLayout;
	readonly copyLayout: GPUBindGroupLayout;
	readonly coverLayout: GPUBindGroupLayout;
	readonly compositeLayout: GPUBindGroupLayout;
	readonly maskLayout: GPUBindGroupLayout;
	readonly source: GPURenderPipeline;
	readonly preprocess: GPURenderPipeline;
	readonly blur: GPURenderPipeline;
	readonly copy: GPURenderPipeline;
	readonly cover: GPURenderPipeline;
	readonly composite: GPURenderPipeline;
	readonly mask: GPUComputePipeline;
}

interface WebCameraEffectRenderPipelineCreation {
	readonly device: GPUDevice;
	readonly label: string;
	readonly code: string;
	readonly layout: GPUBindGroupLayout;
	readonly format: GPUTextureFormat;
}

function renderPipeline({
	device,
	label,
	code,
	layout,
	format,
}: WebCameraEffectRenderPipelineCreation): GPURenderPipeline {
	const module = device.createShaderModule({label, code});
	return device.createRenderPipeline({
		label,
		layout: device.createPipelineLayout({bindGroupLayouts: [layout]}),
		vertex: {module, entryPoint: 'vertexMain'},
		fragment: {module, entryPoint: 'fragmentMain', targets: [{format}]},
		primitive: {topology: 'triangle-list'},
	});
}

export function createWebCameraEffectWebGPUPipelines(
	device: GPUDevice,
	canvasFormat: GPUTextureFormat,
): WebCameraEffectWebGPUPipelines {
	const sourceLayout = device.createBindGroupLayout({
		label: 'camera-source-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, externalTexture: {}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
		],
	});
	const preprocessLayout = device.createBindGroupLayout({
		label: 'camera-preprocess-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, externalTexture: {}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
			{binding: 2, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, buffer: {type: 'storage'}},
		],
	});
	const blurLayout = device.createBindGroupLayout({
		label: 'camera-blur-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
			{binding: 2, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, buffer: {type: 'uniform'}},
		],
	});
	const copyLayout = device.createBindGroupLayout({
		label: 'camera-copy-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
		],
	});
	const coverLayout = device.createBindGroupLayout({
		label: 'camera-cover-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
			{binding: 2, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, buffer: {type: 'uniform'}},
		],
	});
	const compositeLayout = device.createBindGroupLayout({
		label: 'camera-composite-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 2, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, texture: {sampleType: 'float'}},
			{binding: 3, visibility: WEB_GPU_SHADER_STAGE_FRAGMENT, sampler: {type: 'filtering'}},
		],
	});
	const maskLayout = device.createBindGroupLayout({
		label: 'camera-mask-layout',
		entries: [
			{binding: 0, visibility: WEB_GPU_SHADER_STAGE_COMPUTE, buffer: {type: 'read-only-storage'}},
			{binding: 1, visibility: WEB_GPU_SHADER_STAGE_COMPUTE, buffer: {type: 'storage'}},
			{
				binding: 2,
				visibility: WEB_GPU_SHADER_STAGE_COMPUTE,
				storageTexture: {access: 'write-only', format: 'rgba8unorm'},
			},
			{binding: 3, visibility: WEB_GPU_SHADER_STAGE_COMPUTE, buffer: {type: 'uniform'}},
		],
	});
	const maskModule = device.createShaderModule({label: 'camera-mask', code: CAMERA_MASK_SHADER});
	return {
		sourceLayout,
		preprocessLayout,
		blurLayout,
		copyLayout,
		coverLayout,
		compositeLayout,
		maskLayout,
		source: renderPipeline({
			device,
			label: 'camera-source',
			code: CAMERA_SOURCE_SHADER,
			layout: sourceLayout,
			format: 'rgba8unorm',
		}),
		preprocess: renderPipeline({
			device,
			label: 'camera-preprocess',
			code: CAMERA_PREPROCESS_SHADER,
			layout: preprocessLayout,
			format: 'rgba8unorm',
		}),
		blur: renderPipeline({
			device,
			label: 'camera-blur',
			code: CAMERA_BLUR_SHADER,
			layout: blurLayout,
			format: 'rgba8unorm',
		}),
		copy: renderPipeline({
			device,
			label: 'camera-copy',
			code: CAMERA_COPY_SHADER,
			layout: copyLayout,
			format: canvasFormat,
		}),
		cover: renderPipeline({
			device,
			label: 'camera-cover',
			code: CAMERA_COVER_SHADER,
			layout: coverLayout,
			format: 'rgba8unorm',
		}),
		composite: renderPipeline({
			device,
			label: 'camera-composite',
			code: CAMERA_COMPOSITE_SHADER,
			layout: compositeLayout,
			format: canvasFormat,
		}),
		mask: device.createComputePipeline({
			label: 'camera-mask',
			layout: device.createPipelineLayout({bindGroupLayouts: [maskLayout]}),
			compute: {module: maskModule, entryPoint: 'computeMain'},
		}),
	};
}
