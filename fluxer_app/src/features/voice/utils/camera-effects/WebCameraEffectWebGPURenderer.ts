// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	collectSettledFailures,
	throwCollectedFailures,
} from '@app/features/voice/utils/camera-effects/AggregateOperations';
import {CameraBackgroundMode} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import type {
	WebCameraEffectCustomFrame,
	WebCameraEffectCustomFrameSource,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomImage';
import {
	cameraEffectBlurPixels,
	requireCameraEffectBlurStrength,
	validateCameraEffectFrameDimensions,
	WEB_CAMERA_EFFECT_SEGMENTATION_MIN_INTERVAL_MS,
	WebCameraEffectBackend,
	type WebCameraPipelineConfig,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import type {WebCameraEffectRenderer} from '@app/features/voice/utils/camera-effects/WebCameraEffectRenderer';
import {WebCameraEffectSegmentationOwner} from '@app/features/voice/utils/camera-effects/WebCameraEffectSegmentationOwner';
import {
	WEB_GPU_BUFFER_USAGE_COPY_DST,
	WEB_GPU_BUFFER_USAGE_STORAGE,
	WEB_GPU_BUFFER_USAGE_UNIFORM,
	WEB_GPU_TEXTURE_USAGE_COPY_DST,
	WEB_GPU_TEXTURE_USAGE_RENDER_ATTACHMENT,
	WEB_GPU_TEXTURE_USAGE_STORAGE_BINDING,
	WEB_GPU_TEXTURE_USAGE_TEXTURE_BINDING,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGPUConstants';
import {
	createWebCameraEffectWebGPUPipelines,
	type WebCameraEffectWebGPUPipelines,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGPUPipelines';
import {
	loadWebSelfieRuntime,
	MissingSegmentationAlphasOutputError,
	SEG_INPUT_EDGE,
	SEG_INPUT_NAME,
	SEG_INPUT_PIXELS,
	SEG_OUTPUT_NAME,
	type WebSelfieOrtModule,
} from '@app/features/voice/utils/camera-effects/WebSelfieSegmenter';
import type * as OrtNamespace from 'onnxruntime-web/webgpu';
import invariant from 'tiny-invariant';

class WebGPUCameraFrameResourcesUnavailableError extends Error {
	constructor() {
		super('WebGPU camera frame resources are unavailable');
		this.name = 'WebGPUCameraFrameResourcesUnavailableError';
	}
}

class MissingSegmentationGPUFloatMaskError extends Error {
	constructor() {
		super('Segmentation model did not produce the required GPU-resident float mask');
		this.name = 'MissingSegmentationGPUFloatMaskError';
	}
}

class MissingWebGPUCameraFrameTexturesError extends Error {
	constructor() {
		super('Cannot build WebGPU camera bind groups without frame textures');
		this.name = 'MissingWebGPUCameraFrameTexturesError';
	}
}

const INPUT_BUFFER_BYTES = 3 * SEG_INPUT_PIXELS * Float32Array.BYTES_PER_ELEMENT;
const MASK_BUFFER_BYTES = SEG_INPUT_PIXELS * Float32Array.BYTES_PER_ELEMENT;

interface CustomTexture {
	readonly texture: GPUTexture;
	readonly width: number;
	readonly height: number;
	frameIndex: number;
}

interface FrameBindGroups {
	readonly sourceCopy: GPUBindGroup;
	readonly backgroundCopy: GPUBindGroup;
	readonly horizontalBlur: GPUBindGroup;
	readonly verticalBlur: GPUBindGroup;
	readonly composite: GPUBindGroup;
}

interface FrameConfigurationResources {
	readonly horizontalBlurParamsBuffer: GPUBuffer;
	readonly verticalBlurParamsBuffer: GPUBuffer;
	readonly coverParamsBuffer: GPUBuffer;
	readonly frameBindGroups: FrameBindGroups;
	readonly customCoverBindGroup: GPUBindGroup | null;
}

interface CustomBackgroundCoverScale {
	readonly scaleX: number;
	readonly scaleY: number;
}

interface WebCameraEffectWebGPUInitialization {
	readonly canvas: OffscreenCanvas;
	readonly context: GPUCanvasContext;
	readonly canvasFormat: GPUTextureFormat;
	readonly device: GPUDevice;
	readonly ort: WebSelfieOrtModule;
	readonly session: OrtNamespace.InferenceSession;
}

function errorMessage(error: unknown): string {
	if (error instanceof Error) {
		return error.message;
	}
	return String(error);
}

function resolveValidationDetail(validationError: GPUError | null): string {
	if (validationError == null) {
		return '';
	}
	return `; validation failed: ${validationError.message}`;
}

async function popValidationErrorScope(
	device: GPUDevice,
	validationScopeActive: boolean,
	validationFailures: Array<unknown>,
): Promise<GPUError | null> {
	if (!validationScopeActive) {
		return null;
	}
	try {
		return await device.popErrorScope();
	} catch (validationFailure) {
		validationFailures.push(validationFailure);
		return null;
	}
}

function resolveCustomBackgroundCoverScale(imageAspect: number, canvasAspect: number): CustomBackgroundCoverScale {
	if (imageAspect > canvasAspect) {
		return {
			scaleX: canvasAspect / imageAspect,
			scaleY: 1,
		};
	}
	return {
		scaleX: 1,
		scaleY: imageAspect / canvasAspect,
	};
}

function GPUBooleanFlag(value: boolean): number {
	if (value) {
		return 1;
	}
	return 0;
}

function destroyGPUTexture(texture: GPUTexture | null): void {
	if (texture == null) {
		return;
	}
	texture.destroy();
}

function destroyGPUBuffer(buffer: GPUBuffer | null): void {
	if (buffer == null) {
		return;
	}
	buffer.destroy();
}

function destroyCustomTexture(customTexture: CustomTexture | null): void {
	if (customTexture == null) {
		return;
	}
	customTexture.texture.destroy();
}

function collectInferenceOutputDisposalFailures(
	outputs: Readonly<Record<string, OrtNamespace.Tensor>>,
): ReadonlyArray<unknown> {
	const failures: Array<unknown> = [];
	for (const output of Object.values(outputs)) {
		try {
			output.dispose();
		} catch (error) {
			failures.push(error);
		}
	}
	return failures;
}

async function collectWebGPUInitializationCleanupFailures(
	renderer: WebCameraEffectWebGPURenderer | null,
	session: OrtNamespace.InferenceSession | null,
	device: GPUDevice,
): Promise<Array<unknown>> {
	if (renderer != null) {
		return collectSettledFailures([renderer.dispose()]);
	}
	return collectSettledFailures([
		Promise.resolve().then(() => {
			if (session != null) {
				session.release();
			}
		}),
		Promise.resolve().then(() => device.destroy()),
	]);
}

function requireGPUMask(output: OrtNamespace.Tensor): OrtNamespace.Tensor {
	let elements = 1;
	for (const dimension of output.dims) {
		elements *= dimension;
	}
	if (output.type !== 'float32') {
		throw new MissingSegmentationGPUFloatMaskError();
	}
	if (output.location !== 'gpu-buffer') {
		throw new MissingSegmentationGPUFloatMaskError();
	}
	if (elements !== SEG_INPUT_PIXELS) {
		throw new MissingSegmentationGPUFloatMaskError();
	}
	return output;
}

function beginRenderPass(encoder: GPUCommandEncoder, target: GPUTextureView): GPURenderPassEncoder {
	return encoder.beginRenderPass({
		colorAttachments: [
			{
				view: target,
				clearValue: {r: 0, g: 0, b: 0, a: 1},
				loadOp: 'clear',
				storeOp: 'store',
			},
		],
	});
}

function drawRenderPass(pass: GPURenderPassEncoder, pipeline: GPURenderPipeline, bindGroup: GPUBindGroup): void {
	pass.setPipeline(pipeline);
	pass.setBindGroup(0, bindGroup);
	pass.draw(3);
	pass.end();
}

export class WebCameraEffectWebGPURenderer implements WebCameraEffectRenderer {
	readonly backend = WebCameraEffectBackend.WEB_GPU;
	private readonly canvas: OffscreenCanvas;
	private readonly canvasContext: GPUCanvasContext;
	private readonly canvasFormat: GPUTextureFormat;
	private readonly device: GPUDevice;
	private readonly session: OrtNamespace.InferenceSession;
	private readonly pipelines: WebCameraEffectWebGPUPipelines;
	private readonly sampler: GPUSampler;
	private readonly inputBuffer: GPUBuffer;
	private readonly inputTensor: OrtNamespace.Tensor;
	private readonly smoothedMaskBuffer: GPUBuffer;
	private horizontalBlurParamsBuffer: GPUBuffer;
	private verticalBlurParamsBuffer: GPUBuffer;
	private coverParamsBuffer: GPUBuffer;
	private readonly maskParamsBuffer: GPUBuffer;
	private readonly horizontalBlurParams = new Float32Array(4);
	private readonly verticalBlurParams = new Float32Array(4);
	private readonly coverParams = new Float32Array(4);
	private readonly maskParams = new ArrayBuffer(16);
	private readonly maskParamsView = new DataView(this.maskParams);
	private readonly segmentationOwner = new WebCameraEffectSegmentationOwner();
	private readonly preprocessTarget: GPUTexture;
	private readonly preprocessTargetView: GPUTextureView;
	private readonly maskTexture: GPUTexture;
	private readonly maskTextureView: GPUTextureView;
	private config: WebCameraPipelineConfig = {background: null};
	private customFrameSource: WebCameraEffectCustomFrameSource | null = null;
	private customTexture: CustomTexture | null = null;
	private sourceTexture: GPUTexture | null = null;
	private sourceTextureView: GPUTextureView | null = null;
	private blurTexture: GPUTexture | null = null;
	private blurTextureView: GPUTextureView | null = null;
	private backgroundTexture: GPUTexture | null = null;
	private backgroundTextureView: GPUTextureView | null = null;
	private frameBindGroups: FrameBindGroups | null = null;
	private customCoverBindGroup: GPUBindGroup | null = null;
	private width = 0;
	private height = 0;
	private customBackgroundDirty = false;
	private lastSegmentationAt = Number.NEGATIVE_INFINITY;
	private maskPrimed = false;
	private maskReady = false;
	private disposed = false;
	private disposePromise: Promise<void> | null = null;
	private deviceLostReason: string | null = null;

	private constructor({canvas, context, canvasFormat, device, ort, session}: WebCameraEffectWebGPUInitialization) {
		this.canvas = canvas;
		this.canvasContext = context;
		this.canvasFormat = canvasFormat;
		this.device = device;
		this.session = session;
		this.pipelines = createWebCameraEffectWebGPUPipelines(device, canvasFormat);
		this.sampler = device.createSampler({
			label: 'camera-linear-sampler',
			addressModeU: 'clamp-to-edge',
			addressModeV: 'clamp-to-edge',
			magFilter: 'linear',
			minFilter: 'linear',
		});
		this.inputBuffer = device.createBuffer({
			label: 'camera-segmentation-input',
			size: INPUT_BUFFER_BYTES,
			usage: WEB_GPU_BUFFER_USAGE_STORAGE | WEB_GPU_BUFFER_USAGE_COPY_DST,
		});
		this.inputTensor = ort.Tensor.fromGpuBuffer(this.inputBuffer, {
			dataType: 'float32',
			dims: [1, 3, SEG_INPUT_EDGE, SEG_INPUT_EDGE],
		});
		this.smoothedMaskBuffer = device.createBuffer({
			label: 'camera-smoothed-mask',
			size: MASK_BUFFER_BYTES,
			usage: WEB_GPU_BUFFER_USAGE_STORAGE | WEB_GPU_BUFFER_USAGE_COPY_DST,
		});
		this.horizontalBlurParamsBuffer = this.uniformBuffer('camera-horizontal-blur-params');
		this.verticalBlurParamsBuffer = this.uniformBuffer('camera-vertical-blur-params');
		this.coverParamsBuffer = this.uniformBuffer('camera-cover-params');
		this.maskParamsBuffer = this.uniformBuffer('camera-mask-params');
		this.preprocessTarget = device.createTexture({
			label: 'camera-preprocess-target',
			size: [SEG_INPUT_EDGE, SEG_INPUT_EDGE],
			format: 'rgba8unorm',
			usage: WEB_GPU_TEXTURE_USAGE_RENDER_ATTACHMENT,
		});
		this.maskTexture = device.createTexture({
			label: 'camera-mask',
			size: [SEG_INPUT_EDGE, SEG_INPUT_EDGE],
			format: 'rgba8unorm',
			usage: WEB_GPU_TEXTURE_USAGE_STORAGE_BINDING | WEB_GPU_TEXTURE_USAGE_TEXTURE_BINDING,
		});
		this.preprocessTargetView = this.preprocessTarget.createView();
		this.maskTextureView = this.maskTexture.createView();
		void device.lost
			.then((info) => {
				if (!this.disposed) {
					this.deviceLostReason = `${info.reason}: ${info.message}`;
				}
			})
			.catch(() => {});
	}

	static async create(
		canvas: OffscreenCanvas,
		config: WebCameraPipelineConfig,
		customFrameSource: WebCameraEffectCustomFrameSource | null,
	): Promise<WebCameraEffectWebGPURenderer> {
		const GPU = navigator.gpu;
		if (GPU == null) {
			throw new Error('WebGPU is unavailable');
		}
		const adapter = await GPU.requestAdapter({powerPreference: 'high-performance'});
		if (adapter == null) {
			throw new Error('WebGPU did not provide an adapter');
		}
		const device = await adapter.requestDevice();
		const context = canvas.getContext('webgpu') as GPUCanvasContext | null;
		if (context == null) {
			device.destroy();
			throw new Error('OffscreenCanvas WebGPU context is unavailable');
		}
		const format = GPU.getPreferredCanvasFormat();
		context.configure({device, format, alphaMode: 'opaque'});
		let session: OrtNamespace.InferenceSession | null = null;
		let renderer: WebCameraEffectWebGPURenderer | null = null;
		let validationScopeActive = true;
		device.pushErrorScope('validation');
		try {
			const {ort, modelBytes} = await loadWebSelfieRuntime();
			session = await ort.InferenceSession.create(modelBytes, {
				executionProviders: [{name: 'webgpu', device, preferredLayout: 'NCHW'}],
				graphOptimizationLevel: 'all',
				logSeverityLevel: 3,
				preferredOutputLocation: {[SEG_OUTPUT_NAME]: 'gpu-buffer'},
			});
			renderer = new WebCameraEffectWebGPURenderer({
				canvas,
				context,
				canvasFormat: format,
				device,
				ort,
				session,
			});
			await renderer.configure(config, customFrameSource);
			await renderer.warmup();
			const validationError = await device.popErrorScope();
			validationScopeActive = false;
			if (validationError != null) {
				throw new Error(`WebGPU validation failed: ${validationError.message}`);
			}
			return renderer;
		} catch (error) {
			const validationFailures: Array<unknown> = [];
			const validationError = await popValidationErrorScope(device, validationScopeActive, validationFailures);
			const validationDetail = resolveValidationDetail(validationError);
			const initializationError = new Error(
				`WebGPU camera effect initialization failed: ${errorMessage(error)}${validationDetail}`,
				{
					cause: error,
				},
			);
			const cleanupFailures = await collectWebGPUInitializationCleanupFailures(renderer, session, device);
			throwCollectedFailures({
				failures: [initializationError, ...validationFailures, ...cleanupFailures],
				message: 'WebGPU camera effect initialization and cleanup failed',
			});
		}
	}

	async configure(
		config: WebCameraPipelineConfig,
		customFrameSource: WebCameraEffectCustomFrameSource | null,
	): Promise<void> {
		this.requireActive();
		if (config.background != null) {
			requireCameraEffectBlurStrength(config.background.blurStrength);
		}
		const customBackground = config.background?.mode === CameraBackgroundMode.CUSTOM;
		if (customBackground !== (customFrameSource != null)) {
			throw new Error('Camera effect custom frame source does not match its configuration');
		}
		let nextCustomTexture = this.customTexture;
		if (this.customFrameSource !== customFrameSource) {
			nextCustomTexture = await this.createCustomTexture(customFrameSource);
		}
		let nextFrameConfiguration: FrameConfigurationResources | null = null;
		try {
			if (this.width > 0 && this.height > 0) {
				nextFrameConfiguration = await this.createFrameConfigurationResources(config, nextCustomTexture);
			}
			const backgroundLifecycleChanged = (this.config.background == null) !== (config.background == null);
			if (backgroundLifecycleChanged) {
				this.segmentationOwner.advanceLifecycle();
			}
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([
				Promise.resolve().then(() => {
					if (nextCustomTexture !== this.customTexture) {
						destroyCustomTexture(nextCustomTexture);
					}
				}),
			]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'WebGPU camera effect configuration failed during cleanup',
			});
		}
		const backgroundLifecycleChanged = (this.config.background == null) !== (config.background == null);
		const previousCustomTexture = this.customTexture;
		const previousHorizontalBlurParamsBuffer = this.horizontalBlurParamsBuffer;
		const previousVerticalBlurParamsBuffer = this.verticalBlurParamsBuffer;
		const previousCoverParamsBuffer = this.coverParamsBuffer;
		this.customTexture = nextCustomTexture;
		this.customFrameSource = customFrameSource;
		this.config = config;
		if (nextFrameConfiguration != null) {
			this.horizontalBlurParamsBuffer = nextFrameConfiguration.horizontalBlurParamsBuffer;
			this.verticalBlurParamsBuffer = nextFrameConfiguration.verticalBlurParamsBuffer;
			this.coverParamsBuffer = nextFrameConfiguration.coverParamsBuffer;
			this.frameBindGroups = nextFrameConfiguration.frameBindGroups;
			this.customCoverBindGroup = nextFrameConfiguration.customCoverBindGroup;
		}
		if (backgroundLifecycleChanged) {
			this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
			this.maskPrimed = false;
			this.maskReady = false;
		}
		this.customBackgroundDirty = nextCustomTexture != null;
		if (previousCustomTexture !== nextCustomTexture) {
			destroyCustomTexture(previousCustomTexture);
		}
		if (nextFrameConfiguration != null) {
			destroyGPUBuffer(previousHorizontalBlurParamsBuffer);
			destroyGPUBuffer(previousVerticalBlurParamsBuffer);
			destroyGPUBuffer(previousCoverParamsBuffer);
		}
	}

	private async createFrameConfigurationResources(
		config: WebCameraPipelineConfig,
		customTexture: CustomTexture | null,
	): Promise<FrameConfigurationResources> {
		let horizontalBlurParamsBuffer: GPUBuffer | null = null;
		let verticalBlurParamsBuffer: GPUBuffer | null = null;
		let coverParamsBuffer: GPUBuffer | null = null;
		try {
			horizontalBlurParamsBuffer = this.uniformBuffer('camera-horizontal-blur-params');
			verticalBlurParamsBuffer = this.uniformBuffer('camera-vertical-blur-params');
			coverParamsBuffer = this.uniformBuffer('camera-cover-params');
			this.updateFrameParams(
				config,
				customTexture,
				horizontalBlurParamsBuffer,
				verticalBlurParamsBuffer,
				coverParamsBuffer,
			);
			return {
				horizontalBlurParamsBuffer,
				verticalBlurParamsBuffer,
				coverParamsBuffer,
				frameBindGroups: this.createFrameBindGroups(horizontalBlurParamsBuffer, verticalBlurParamsBuffer),
				customCoverBindGroup: this.createCustomCoverBindGroup(customTexture, coverParamsBuffer),
			};
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([
				Promise.resolve().then(() => destroyGPUBuffer(horizontalBlurParamsBuffer)),
				Promise.resolve().then(() => destroyGPUBuffer(verticalBlurParamsBuffer)),
				Promise.resolve().then(() => destroyGPUBuffer(coverParamsBuffer)),
			]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'WebGPU camera effect frame configuration failed during cleanup',
			});
		}
	}

	private async createCustomTexture(
		customFrameSource: WebCameraEffectCustomFrameSource | null,
	): Promise<CustomTexture | null> {
		if (customFrameSource == null) {
			return null;
		}
		const lease = customFrameSource.acquireFrame(0);
		try {
			return await this.loadCustomTexture(lease.frame);
		} finally {
			lease.release();
		}
	}

	async render(frame: VideoFrame, now: number): Promise<void> {
		this.requireActive();
		const outputFrameAdmission = this.segmentationOwner.admitOutputFrame();
		if (outputFrameAdmission != null) {
			await outputFrameAdmission;
			this.requireActive();
		}
		this.ensureSize(frame.displayWidth, frame.displayHeight);
		this.refreshCustomTexture(now);
		const externalTexture = this.device.importExternalTexture({source: frame});
		const sourceBindGroup = this.device.createBindGroup({
			label: 'camera-source-frame',
			layout: this.pipelines.sourceLayout,
			entries: [
				{binding: 0, resource: externalTexture},
				{binding: 1, resource: this.sampler},
			],
		});
		const sourceView = this.sourceTextureView;
		const backgroundView = this.backgroundTextureView;
		const frameBindGroups = this.frameBindGroups;
		if (sourceView == null) {
			throw new WebGPUCameraFrameResourcesUnavailableError();
		}
		if (backgroundView == null) {
			throw new WebGPUCameraFrameResourcesUnavailableError();
		}
		if (frameBindGroups == null) {
			throw new WebGPUCameraFrameResourcesUnavailableError();
		}
		const encoder = this.device.createCommandEncoder({label: 'camera-frame'});
		drawRenderPass(beginRenderPass(encoder, sourceView), this.pipelines.source, sourceBindGroup);
		const background = this.config.background;
		if (background != null && background.mode === CameraBackgroundMode.BLUR) {
			const blurView = this.blurTextureView;
			if (blurView == null) {
				throw new Error('WebGPU camera blur texture is unavailable');
			}
			drawRenderPass(beginRenderPass(encoder, blurView), this.pipelines.blur, frameBindGroups.horizontalBlur);
			drawRenderPass(beginRenderPass(encoder, backgroundView), this.pipelines.blur, frameBindGroups.verticalBlur);
		} else if (background != null && background.mode === CameraBackgroundMode.CUSTOM && this.customBackgroundDirty) {
			const coverBindGroup = this.customCoverBindGroup;
			if (coverBindGroup == null) {
				throw new Error('WebGPU custom camera background resources are unavailable');
			}
			drawRenderPass(beginRenderPass(encoder, backgroundView), this.pipelines.cover, coverBindGroup);
			this.customBackgroundDirty = false;
		}
		const outputView = this.canvasContext.getCurrentTexture().createView();
		if (background == null) {
			drawRenderPass(beginRenderPass(encoder, outputView), this.pipelines.copy, frameBindGroups.sourceCopy);
		} else if (!this.maskReady) {
			drawRenderPass(beginRenderPass(encoder, outputView), this.pipelines.copy, frameBindGroups.backgroundCopy);
		} else {
			drawRenderPass(beginRenderPass(encoder, outputView), this.pipelines.composite, frameBindGroups.composite);
		}
		this.device.queue.submit([encoder.finish()]);
		if (this.config.background != null) {
			this.maybeStartSegmentation(frame, now);
		}
	}

	dispose(): Promise<void> {
		if (this.disposePromise == null) {
			this.disposePromise = this.disposeOwned();
		}
		return this.disposePromise;
	}

	private async disposeOwned(): Promise<void> {
		invariant(!this.disposed, 'WebGPU camera effect disposal must have one owner');
		this.disposed = true;
		this.segmentationOwner.advanceLifecycle();
		let segmentationFailures: ReadonlyArray<unknown>;
		try {
			segmentationFailures = await this.segmentationOwner.settleForDisposal();
		} catch (error) {
			segmentationFailures = [error];
		}
		const inputFailures = await collectSettledFailures([Promise.resolve().then(() => this.inputTensor.dispose())]);
		const sessionFailures = await collectSettledFailures([Promise.resolve().then(() => this.session.release())]);
		const resourceFailures = await collectSettledFailures([
			Promise.resolve().then(() => destroyGPUTexture(this.sourceTexture)),
			Promise.resolve().then(() => destroyGPUTexture(this.blurTexture)),
			Promise.resolve().then(() => destroyGPUTexture(this.backgroundTexture)),
			Promise.resolve().then(() => destroyCustomTexture(this.customTexture)),
			Promise.resolve().then(() => this.preprocessTarget.destroy()),
			Promise.resolve().then(() => this.maskTexture.destroy()),
			Promise.resolve().then(() => this.inputBuffer.destroy()),
			Promise.resolve().then(() => this.smoothedMaskBuffer.destroy()),
			Promise.resolve().then(() => this.horizontalBlurParamsBuffer.destroy()),
			Promise.resolve().then(() => this.verticalBlurParamsBuffer.destroy()),
			Promise.resolve().then(() => this.coverParamsBuffer.destroy()),
			Promise.resolve().then(() => this.maskParamsBuffer.destroy()),
		]);
		const deviceFailures = await collectSettledFailures([Promise.resolve().then(() => this.device.destroy())]);
		throwCollectedFailures({
			failures: [...segmentationFailures, ...inputFailures, ...sessionFailures, ...resourceFailures, ...deviceFailures],
			message: 'WebGPU camera effect teardown failed',
		});
	}

	private uniformBuffer(label: string): GPUBuffer {
		return this.device.createBuffer({
			label,
			size: 16,
			usage: WEB_GPU_BUFFER_USAGE_UNIFORM | WEB_GPU_BUFFER_USAGE_COPY_DST,
		});
	}

	private async warmup(): Promise<void> {
		const width = this.canvas.width;
		const height = this.canvas.height;
		validateCameraEffectFrameDimensions(width, height);
		const probeCanvas = new OffscreenCanvas(width, height);
		const probeContext = probeCanvas.getContext('2d');
		if (probeContext == null) {
			throw new Error('WebGPU camera effect warm-up requires OffscreenCanvas 2D');
		}
		probeContext.fillStyle = '#000';
		probeContext.fillRect(0, 0, width, height);
		const frame = new VideoFrame(probeCanvas, {timestamp: 0});
		try {
			await this.render(frame, 0);
			await this.segmentationOwner.settlePhysicalOperation();
		} finally {
			frame.close();
		}
		this.device.queue.writeBuffer(this.smoothedMaskBuffer, 0, new Uint8Array(MASK_BUFFER_BYTES));
		this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
		this.maskPrimed = false;
		this.maskReady = false;
	}

	private requireActive(): void {
		if (this.disposed) {
			throw new Error('Cannot use a disposed WebGPU camera effect renderer');
		}
		this.segmentationOwner.requireNoDeferredFailure();
		if (this.deviceLostReason != null) {
			throw new Error(`WebGPU camera effect device was lost: ${this.deviceLostReason}`);
		}
	}

	private ensureSize(width: number, height: number): void {
		validateCameraEffectFrameDimensions(width, height);
		if (this.width === width && this.height === height) {
			return;
		}
		this.width = width;
		this.height = height;
		this.canvas.width = width;
		this.canvas.height = height;
		this.canvasContext.configure({device: this.device, format: this.canvasFormat, alphaMode: 'opaque'});
		destroyGPUTexture(this.sourceTexture);
		destroyGPUTexture(this.blurTexture);
		destroyGPUTexture(this.backgroundTexture);
		this.sourceTexture = this.frameTexture('camera-source');
		this.blurTexture = this.frameTexture('camera-blur');
		this.backgroundTexture = this.frameTexture('camera-background');
		this.sourceTextureView = this.sourceTexture.createView();
		this.blurTextureView = this.blurTexture.createView();
		this.backgroundTextureView = this.backgroundTexture.createView();
		this.updateFrameParams(
			this.config,
			this.customTexture,
			this.horizontalBlurParamsBuffer,
			this.verticalBlurParamsBuffer,
			this.coverParamsBuffer,
		);
		this.frameBindGroups = this.createFrameBindGroups(this.horizontalBlurParamsBuffer, this.verticalBlurParamsBuffer);
		this.customCoverBindGroup = this.createCustomCoverBindGroup(this.customTexture, this.coverParamsBuffer);
		this.customBackgroundDirty = this.customTexture != null;
		this.device.queue.writeBuffer(this.smoothedMaskBuffer, 0, new Uint8Array(MASK_BUFFER_BYTES));
		this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
		this.maskPrimed = false;
		this.maskReady = false;
	}

	private frameTexture(label: string): GPUTexture {
		return this.device.createTexture({
			label,
			size: [this.width, this.height],
			format: 'rgba8unorm',
			usage: WEB_GPU_TEXTURE_USAGE_RENDER_ATTACHMENT | WEB_GPU_TEXTURE_USAGE_TEXTURE_BINDING,
		});
	}

	private updateFrameParams(
		config: WebCameraPipelineConfig,
		customTexture: CustomTexture | null,
		horizontalBlurParamsBuffer: GPUBuffer,
		verticalBlurParamsBuffer: GPUBuffer,
		coverParamsBuffer: GPUBuffer,
	): void {
		let radius = 0;
		const background = config.background;
		if (background != null) {
			radius = cameraEffectBlurPixels(background.blurStrength);
		}
		this.horizontalBlurParams[0] = 1 / this.width;
		this.horizontalBlurParams[1] = 0;
		this.horizontalBlurParams[2] = radius;
		this.horizontalBlurParams[3] = 0;
		this.device.queue.writeBuffer(horizontalBlurParamsBuffer, 0, this.horizontalBlurParams);
		this.verticalBlurParams[0] = 0;
		this.verticalBlurParams[1] = 1 / this.height;
		this.verticalBlurParams[2] = radius;
		this.verticalBlurParams[3] = 0;
		this.device.queue.writeBuffer(verticalBlurParamsBuffer, 0, this.verticalBlurParams);
		if (customTexture == null) {
			return;
		}
		const imageAspect = customTexture.width / customTexture.height;
		const canvasAspect = this.width / this.height;
		const {scaleX, scaleY} = resolveCustomBackgroundCoverScale(imageAspect, canvasAspect);
		this.coverParams[0] = scaleX;
		this.coverParams[1] = scaleY;
		this.coverParams[2] = (1 - scaleX) / 2;
		this.coverParams[3] = (1 - scaleY) / 2;
		this.device.queue.writeBuffer(coverParamsBuffer, 0, this.coverParams);
	}

	private createFrameBindGroups(
		horizontalBlurParamsBuffer: GPUBuffer,
		verticalBlurParamsBuffer: GPUBuffer,
	): FrameBindGroups {
		const sourceView = this.sourceTextureView;
		const blurView = this.blurTextureView;
		const backgroundView = this.backgroundTextureView;
		if (sourceView == null) {
			throw new MissingWebGPUCameraFrameTexturesError();
		}
		if (blurView == null) {
			throw new MissingWebGPUCameraFrameTexturesError();
		}
		if (backgroundView == null) {
			throw new MissingWebGPUCameraFrameTexturesError();
		}
		return {
			sourceCopy: this.device.createBindGroup({
				label: 'camera-source-copy',
				layout: this.pipelines.copyLayout,
				entries: [
					{binding: 0, resource: sourceView},
					{binding: 1, resource: this.sampler},
				],
			}),
			backgroundCopy: this.device.createBindGroup({
				label: 'camera-background-copy',
				layout: this.pipelines.copyLayout,
				entries: [
					{binding: 0, resource: backgroundView},
					{binding: 1, resource: this.sampler},
				],
			}),
			horizontalBlur: this.device.createBindGroup({
				label: 'camera-horizontal-blur',
				layout: this.pipelines.blurLayout,
				entries: [
					{binding: 0, resource: sourceView},
					{binding: 1, resource: this.sampler},
					{binding: 2, resource: {buffer: horizontalBlurParamsBuffer}},
				],
			}),
			verticalBlur: this.device.createBindGroup({
				label: 'camera-vertical-blur',
				layout: this.pipelines.blurLayout,
				entries: [
					{binding: 0, resource: blurView},
					{binding: 1, resource: this.sampler},
					{binding: 2, resource: {buffer: verticalBlurParamsBuffer}},
				],
			}),
			composite: this.device.createBindGroup({
				label: 'camera-composite',
				layout: this.pipelines.compositeLayout,
				entries: [
					{binding: 0, resource: sourceView},
					{binding: 1, resource: backgroundView},
					{binding: 2, resource: this.maskTextureView},
					{binding: 3, resource: this.sampler},
				],
			}),
		};
	}

	private createCustomCoverBindGroup(
		customTexture: CustomTexture | null,
		coverParamsBuffer: GPUBuffer,
	): GPUBindGroup | null {
		if (customTexture == null) {
			return null;
		}
		return this.device.createBindGroup({
			label: 'camera-custom-cover',
			layout: this.pipelines.coverLayout,
			entries: [
				{binding: 0, resource: customTexture.texture.createView()},
				{binding: 1, resource: this.sampler},
				{binding: 2, resource: {buffer: coverParamsBuffer}},
			],
		});
	}

	private async loadCustomTexture(frame: WebCameraEffectCustomFrame): Promise<CustomTexture> {
		let texture: GPUTexture | null = null;
		try {
			texture = this.device.createTexture({
				label: 'camera-custom-background',
				size: [frame.width, frame.height],
				format: 'rgba8unorm',
				usage: WEB_GPU_TEXTURE_USAGE_COPY_DST | WEB_GPU_TEXTURE_USAGE_TEXTURE_BINDING,
			});
			this.device.queue.copyExternalImageToTexture(
				{source: frame.image},
				{texture},
				{width: frame.width, height: frame.height},
			);
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([Promise.resolve().then(() => destroyGPUTexture(texture))]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'WebGPU custom camera background upload failed during cleanup',
			});
		}
		if (texture == null) {
			throw new Error('WebGPU custom camera background upload produced no texture');
		}
		return {texture, width: frame.width, height: frame.height, frameIndex: frame.index};
	}

	private refreshCustomTexture(now: number): void {
		const customFrameSource = this.customFrameSource;
		const customTexture = this.customTexture;
		if (customFrameSource == null || customTexture == null) {
			return;
		}
		const lease = customFrameSource.acquireFrame(now);
		try {
			const frame = lease.frame;
			if (customTexture.frameIndex === frame.index) {
				return;
			}
			if (customTexture.width !== frame.width || customTexture.height !== frame.height) {
				throw new Error('Custom camera background frame dimensions changed after initialization');
			}
			this.device.queue.copyExternalImageToTexture(
				{source: frame.image},
				{texture: customTexture.texture},
				{width: frame.width, height: frame.height},
			);
			customTexture.frameIndex = frame.index;
			this.customBackgroundDirty = true;
		} finally {
			lease.release();
		}
	}

	private maybeStartSegmentation(frame: VideoFrame, now: number): void {
		if (!this.segmentationOwner.canStartPhysicalOperation()) {
			return;
		}
		if (now - this.lastSegmentationAt < WEB_CAMERA_EFFECT_SEGMENTATION_MIN_INTERVAL_MS) {
			return;
		}
		this.lastSegmentationAt = now;
		let operation: Promise<void>;
		try {
			this.submitSegmentationPreprocess(frame);
			operation = this.executeSegmentation();
		} catch (error) {
			operation = Promise.reject(error);
		}
		this.segmentationOwner.startPhysicalOperation(operation, () => {
			this.maskPrimed = true;
			this.maskReady = true;
		});
	}

	private submitSegmentationPreprocess(frame: VideoFrame): void {
		const externalTexture = this.device.importExternalTexture({source: frame});
		const preprocessBindGroup = this.device.createBindGroup({
			label: 'camera-preprocess-frame',
			layout: this.pipelines.preprocessLayout,
			entries: [
				{binding: 0, resource: externalTexture},
				{binding: 1, resource: this.sampler},
				{binding: 2, resource: {buffer: this.inputBuffer}},
			],
		});
		const preprocessEncoder = this.device.createCommandEncoder({label: 'camera-preprocess'});
		drawRenderPass(
			beginRenderPass(preprocessEncoder, this.preprocessTargetView),
			this.pipelines.preprocess,
			preprocessBindGroup,
		);
		this.device.queue.submit([preprocessEncoder.finish()]);
	}

	private async executeSegmentation(): Promise<void> {
		const outputs = await this.session.run({[SEG_INPUT_NAME]: this.inputTensor});
		const failures: Array<unknown> = [];
		try {
			const inferenceOutput = outputs[SEG_OUTPUT_NAME];
			if (inferenceOutput == null) {
				throw new MissingSegmentationAlphasOutputError();
			}
			const output = requireGPUMask(inferenceOutput);
			this.maskParamsView.setUint32(0, GPUBooleanFlag(this.maskPrimed), true);
			this.device.queue.writeBuffer(this.maskParamsBuffer, 0, this.maskParams);
			const maskBindGroup = this.device.createBindGroup({
				label: 'camera-mask-frame',
				layout: this.pipelines.maskLayout,
				entries: [
					{binding: 0, resource: {buffer: output.gpuBuffer}},
					{binding: 1, resource: {buffer: this.smoothedMaskBuffer}},
					{binding: 2, resource: this.maskTextureView},
					{binding: 3, resource: {buffer: this.maskParamsBuffer}},
				],
			});
			const maskEncoder = this.device.createCommandEncoder({label: 'camera-mask'});
			const pass = maskEncoder.beginComputePass();
			pass.setPipeline(this.pipelines.mask);
			pass.setBindGroup(0, maskBindGroup);
			pass.dispatchWorkgroups(SEG_INPUT_EDGE / 8, SEG_INPUT_EDGE / 8);
			pass.end();
			this.device.queue.submit([maskEncoder.finish()]);
			await this.device.queue.onSubmittedWorkDone();
		} catch (error) {
			failures.push(error);
		}
		failures.push(...collectInferenceOutputDisposalFailures(outputs));
		throwCollectedFailures({failures, message: 'WebGPU camera segmentation failed'});
	}
}
