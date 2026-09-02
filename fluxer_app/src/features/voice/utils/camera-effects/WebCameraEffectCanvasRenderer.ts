// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	collectSettledFailures,
	throwCollectedFailures,
} from '@app/features/voice/utils/camera-effects/AggregateOperations';
import {CameraBackgroundMode} from '@app/features/voice/utils/camera-effects/CameraCaptureContract';
import type {WebCameraEffectCustomFrameSource} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomImage';
import {
	cameraEffectBlurPixels,
	validateCameraEffectFrameDimensions,
	WEB_CAMERA_EFFECT_SEGMENTATION_MIN_INTERVAL_MS,
	WebCameraEffectBackend,
	type WebCameraPipelineConfig,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import type {WebCameraEffectRenderer} from '@app/features/voice/utils/camera-effects/WebCameraEffectRenderer';
import {WebCameraEffectWebGLMaskRefiner} from '@app/features/voice/utils/camera-effects/WebCameraEffectWebGLMaskRefiner';
import {SEG_INPUT_EDGE, WebSelfieSegmenter} from '@app/features/voice/utils/camera-effects/WebSelfieSegmenter';

function twoDContext(
	canvas: OffscreenCanvas,
	options: CanvasRenderingContext2DSettings,
): OffscreenCanvasRenderingContext2D {
	const context = canvas.getContext('2d', options);
	if (context == null) {
		throw new Error('OffscreenCanvas 2D context is unavailable for camera effects');
	}
	return context;
}

function resetContext(context: OffscreenCanvasRenderingContext2D): void {
	context.setTransform(1, 0, 0, 1, 0, 0);
	context.globalCompositeOperation = 'source-over';
	context.filter = 'none';
	context.globalAlpha = 1;
}

async function disposeWebSelfieSegmenter(segmenter: WebSelfieSegmenter | null): Promise<void> {
	if (segmenter == null) {
		return;
	}
	await segmenter.dispose();
}

export class WebCameraEffectCanvasRenderer implements WebCameraEffectRenderer {
	private readonly outputCanvas: OffscreenCanvas;
	private readonly outputContext: OffscreenCanvasRenderingContext2D;
	private readonly segmentationCanvas = new OffscreenCanvas(SEG_INPUT_EDGE, SEG_INPUT_EDGE);
	private readonly segmentationContext = twoDContext(this.segmentationCanvas, {
		alpha: false,
		willReadFrequently: true,
	});
	private readonly maskCanvas = new OffscreenCanvas(SEG_INPUT_EDGE, SEG_INPUT_EDGE);
	private readonly maskContext = twoDContext(this.maskCanvas, {alpha: true});
	private readonly maskImage = this.maskContext.createImageData(SEG_INPUT_EDGE, SEG_INPUT_EDGE);
	private readonly foregroundCanvas = new OffscreenCanvas(1, 1);
	private readonly foregroundContext = twoDContext(this.foregroundCanvas, {alpha: true});
	private readonly maskRefiner = WebCameraEffectWebGLMaskRefiner.create();
	private config: WebCameraPipelineConfig = {background: null};
	private segmenter: WebSelfieSegmenter | null = null;
	private customFrameSource: WebCameraEffectCustomFrameSource | null = null;
	private width = 0;
	private height = 0;
	private lastSegmentationAt = Number.NEGATIVE_INFINITY;
	private maskRevision = 0;
	private maskReady = false;
	private disposed = false;

	get backend(): WebCameraEffectBackend {
		if (this.segmenter == null) {
			return WebCameraEffectBackend.CANVAS_WORKER;
		}
		return WebCameraEffectBackend.WASM_WORKER;
	}

	private constructor(canvas: OffscreenCanvas) {
		this.outputCanvas = canvas;
		this.outputContext = twoDContext(canvas, {alpha: false, desynchronized: true});
		this.outputContext.imageSmoothingEnabled = true;
		this.outputContext.imageSmoothingQuality = 'high';
		this.segmentationContext.imageSmoothingEnabled = true;
		this.segmentationContext.imageSmoothingQuality = 'medium';
		this.foregroundContext.imageSmoothingEnabled = true;
		this.foregroundContext.imageSmoothingQuality = 'high';
	}

	static async create(
		canvas: OffscreenCanvas,
		config: WebCameraPipelineConfig,
		customFrameSource: WebCameraEffectCustomFrameSource | null,
	): Promise<WebCameraEffectCanvasRenderer> {
		const renderer = new WebCameraEffectCanvasRenderer(canvas);
		try {
			await renderer.configure(config, customFrameSource);
			await renderer.warmup();
			return renderer;
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([renderer.dispose()]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'Canvas camera effect initialization failed',
			});
		}
	}

	async configure(
		config: WebCameraPipelineConfig,
		customFrameSource: WebCameraEffectCustomFrameSource | null,
	): Promise<void> {
		if (this.disposed) {
			throw new Error('Cannot configure a disposed camera effect renderer');
		}
		const customBackground = config.background?.mode === CameraBackgroundMode.CUSTOM;
		if (customBackground !== (customFrameSource != null)) {
			throw new Error('Camera effect custom frame source does not match its configuration');
		}
		if (
			config.background != null &&
			config.background.mode === CameraBackgroundMode.BLUR &&
			typeof this.outputContext.filter !== 'string'
		) {
			throw new Error('OffscreenCanvas blur filters are unavailable');
		}
		let createdSegmenter: WebSelfieSegmenter | null = null;
		try {
			if (config.background != null && this.segmenter == null) {
				createdSegmenter = await WebSelfieSegmenter.create();
			}
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([disposeWebSelfieSegmenter(createdSegmenter)]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'Canvas camera effect configuration failed during cleanup',
			});
		}
		if (this.segmenter == null) {
			this.segmenter = createdSegmenter;
		}
		const backgroundLifecycleChanged = (this.config.background == null) !== (config.background == null);
		if (backgroundLifecycleChanged) {
			this.segmenter?.reset();
			this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
			this.maskReady = false;
		}
		this.customFrameSource = customFrameSource;
		this.config = config;
	}

	async render(frame: VideoFrame, now: number): Promise<void> {
		if (this.disposed) {
			throw new Error('Cannot render with a disposed camera effect renderer');
		}
		const width = frame.displayWidth;
		const height = frame.displayHeight;
		this.ensureSize(width, height);
		const source = frame;
		const background = this.config.background;
		resetContext(this.outputContext);
		if (background == null) {
			this.outputContext.globalCompositeOperation = 'copy';
			this.outputContext.drawImage(source, 0, 0, width, height);
			this.outputContext.globalCompositeOperation = 'source-over';
			return;
		}
		await this.maybeSegment(source, now);
		this.drawBackground(source, now);
		if (!this.maskReady) {
			return;
		}
		resetContext(this.foregroundContext);
		this.foregroundContext.globalCompositeOperation = 'copy';
		this.foregroundContext.drawImage(source, 0, 0, width, height);
		this.foregroundContext.globalCompositeOperation = 'destination-in';
		const refinedMask = this.maskRefiner?.refine(frame, this.maskCanvas, this.maskRevision, width, height) ?? null;
		if (refinedMask == null) {
			this.foregroundContext.drawImage(this.maskCanvas, 0, 0, SEG_INPUT_EDGE, SEG_INPUT_EDGE, 0, 0, width, height);
		} else {
			this.foregroundContext.drawImage(refinedMask, 0, 0, width, height);
		}
		this.foregroundContext.globalCompositeOperation = 'source-over';
		this.outputContext.drawImage(this.foregroundCanvas, 0, 0);
	}

	async dispose(): Promise<void> {
		if (this.disposed) {
			return;
		}
		this.disposed = true;
		const segmenter = this.segmenter;
		this.customFrameSource = null;
		this.segmenter = null;
		const failures = await collectSettledFailures([
			disposeWebSelfieSegmenter(segmenter),
			Promise.resolve().then(() => this.maskRefiner?.dispose()),
		]);
		throwCollectedFailures({failures, message: 'Canvas camera effect teardown failed'});
	}

	private ensureSize(width: number, height: number): void {
		validateCameraEffectFrameDimensions(width, height);
		if (this.width === width && this.height === height) {
			return;
		}
		this.width = width;
		this.height = height;
		this.foregroundCanvas.width = width;
		this.foregroundCanvas.height = height;
		this.outputCanvas.width = width;
		this.outputCanvas.height = height;
		this.foregroundContext.imageSmoothingEnabled = true;
		this.foregroundContext.imageSmoothingQuality = 'high';
		this.outputContext.imageSmoothingEnabled = true;
		this.outputContext.imageSmoothingQuality = 'high';
		this.segmenter?.reset();
		this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
		this.maskReady = false;
		this.maskRevision += 1;
	}

	private async warmup(): Promise<void> {
		const width = this.outputCanvas.width;
		const height = this.outputCanvas.height;
		validateCameraEffectFrameDimensions(width, height);
		const probeCanvas = new OffscreenCanvas(width, height);
		const probeContext = probeCanvas.getContext('2d');
		if (probeContext == null) {
			throw new Error('Canvas camera effect warm-up requires OffscreenCanvas 2D');
		}
		probeContext.fillStyle = '#000';
		probeContext.fillRect(0, 0, width, height);
		const frame = new VideoFrame(probeCanvas, {timestamp: 0});
		try {
			await this.render(frame, 0);
		} finally {
			frame.close();
		}
		if (this.segmenter != null) {
			this.segmenter.reset();
		}
		this.lastSegmentationAt = Number.NEGATIVE_INFINITY;
		this.maskReady = false;
	}

	private async maybeSegment(source: CanvasImageSource, now: number): Promise<void> {
		if (now - this.lastSegmentationAt < WEB_CAMERA_EFFECT_SEGMENTATION_MIN_INTERVAL_MS) {
			return;
		}
		const segmenter = this.segmenter;
		if (segmenter == null) {
			throw new Error('Camera background rendering requires an initialized segmenter');
		}
		this.lastSegmentationAt = now;
		resetContext(this.segmentationContext);
		this.segmentationContext.drawImage(source, 0, 0, SEG_INPUT_EDGE, SEG_INPUT_EDGE);
		const input = this.segmentationContext.getImageData(0, 0, SEG_INPUT_EDGE, SEG_INPUT_EDGE);
		await segmenter.segmentIntoMask(input.data, this.maskImage.data);
		this.maskContext.putImageData(this.maskImage, 0, 0);
		this.maskRevision += 1;
		this.maskReady = true;
	}

	private drawBackground(source: CanvasImageSource, now: number): void {
		const background = this.config.background;
		if (background == null) {
			throw new Error('Camera background renderer has no configured background');
		}
		this.outputContext.globalCompositeOperation = 'copy';
		if (background.mode === CameraBackgroundMode.CUSTOM) {
			const customFrameSource = this.customFrameSource;
			if (customFrameSource == null) {
				throw new Error('Custom camera background frame source is unavailable');
			}
			const lease = customFrameSource.acquireFrame(now);
			try {
				const frame = lease.frame;
				const scale = Math.max(this.width / frame.width, this.height / frame.height);
				const drawWidth = frame.width * scale;
				const drawHeight = frame.height * scale;
				this.outputContext.drawImage(
					frame.image,
					(this.width - drawWidth) / 2,
					(this.height - drawHeight) / 2,
					drawWidth,
					drawHeight,
				);
			} finally {
				lease.release();
			}
			this.outputContext.globalCompositeOperation = 'source-over';
			return;
		}
		this.outputContext.filter = `blur(${cameraEffectBlurPixels(background.blurStrength)}px)`;
		this.outputContext.drawImage(source, 0, 0, this.width, this.height);
		this.outputContext.filter = 'none';
		this.outputContext.globalCompositeOperation = 'source-over';
	}
}
