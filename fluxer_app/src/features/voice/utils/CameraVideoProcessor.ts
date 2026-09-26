// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	WebCameraEffectPipeline,
	type WebCameraPipeline,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectPipeline';
import type {WebCameraEffectConfig} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';
import type {Track, TrackProcessor, VideoProcessorOptions} from 'livekit-client';

const logger = new Logger('CameraVideoProcessor');
const DEFAULT_FALLBACK_WIDTH = 640;
const DEFAULT_FALLBACK_HEIGHT = 360;
const DEFAULT_FALLBACK_FRAME_RATE = 30;
const CONTEXT_LOST_RESTARTS_MAX = 3;

type VideoTrackProcessor = TrackProcessor<Track.Kind.Video, VideoProcessorOptions>;
type MediaStreamTrackProcessorInstance = {
	readable: ReadableStream<VideoFrame>;
};
type MediaStreamTrackProcessorConstructor = new (options: {
	track: MediaStreamTrack;
}) => MediaStreamTrackProcessorInstance;
type MediaStreamTrackGeneratorInstance = MediaStreamTrack & {
	writable: WritableStream<VideoFrame>;
};
type MediaStreamTrackGeneratorConstructor = new (options: {
	kind: 'video';
	signalTarget?: MediaStreamTrack;
}) => MediaStreamTrackGeneratorInstance;
type Canvas2DContext = OffscreenCanvasRenderingContext2D | CanvasRenderingContext2D;

interface MediaStreamTrackProcessorGlobals {
	MediaStreamTrackProcessor?: MediaStreamTrackProcessorConstructor;
	MediaStreamTrackGenerator?: MediaStreamTrackGeneratorConstructor;
}

export interface CameraVideoProcessorOptions {
	mirror?: boolean;
	background?: WebCameraEffectConfig | null;
}

export interface CameraVideoProcessorHandle extends VideoTrackProcessor {
	canUpdate(options: CameraVideoProcessorOptions): boolean;
	update(options: CameraVideoProcessorOptions): Promise<void>;
	setOperationalFailureHandler(handler: (error: Error) => void): void;
}

function isTrackProcessorConstructor(value: unknown): value is MediaStreamTrackProcessorConstructor {
	return typeof value === 'function';
}

function isTrackGeneratorConstructor(value: unknown): value is MediaStreamTrackGeneratorConstructor {
	return typeof value === 'function';
}

function getTrackProcessorGlobals(): MediaStreamTrackProcessorGlobals {
	const processor: unknown = Reflect.get(globalThis, 'MediaStreamTrackProcessor');
	const generator: unknown = Reflect.get(globalThis, 'MediaStreamTrackGenerator');
	return {
		MediaStreamTrackProcessor: isTrackProcessorConstructor(processor) ? processor : undefined,
		MediaStreamTrackGenerator: isTrackGeneratorConstructor(generator) ? generator : undefined,
	};
}

function createCanvas(width: number, height: number): OffscreenCanvas | HTMLCanvasElement {
	if (typeof OffscreenCanvas !== 'undefined') {
		return new OffscreenCanvas(width, height);
	}
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	return canvas;
}

function getCanvasContext(canvas: OffscreenCanvas | HTMLCanvasElement): Canvas2DContext | null {
	return canvas.getContext('2d', {alpha: false}) as Canvas2DContext | null;
}

function resizeCanvas(canvas: OffscreenCanvas | HTMLCanvasElement, width: number, height: number): void {
	if (canvas.width === width && canvas.height === height) {
		return;
	}
	canvas.width = width;
	canvas.height = height;
}

class MirrorVideoProcessor implements VideoTrackProcessor {
	name = 'camera-mirror-processor';
	processedTrack?: MediaStreamTrack;
	private canvas: OffscreenCanvas | HTMLCanvasElement | null = null;
	private canvasContext: OffscreenCanvasRenderingContext2D | CanvasRenderingContext2D | null = null;
	private abortController: AbortController | null = null;
	private pipePromise: Promise<void> | null = null;
	private fallbackVideoElement: HTMLVideoElement | null = null;
	private fallbackStream: MediaStream | null = null;
	private fallbackSourceTrack: MediaStreamTrack | null = null;
	private fallbackAnimationFrameId: number | null = null;
	private fallbackLastFrameAt = 0;
	private fallbackMinFrameIntervalMs = 0;
	private lastProcessorOptions: VideoProcessorOptions | null = null;
	private contextLostCanvas: OffscreenCanvas | HTMLCanvasElement | null = null;
	private contextLostRestartCount = 0;

	async init(opts: VideoProcessorOptions): Promise<void> {
		this.lastProcessorOptions = opts;
		await this.setup(opts.track);
	}

	async restart(opts: VideoProcessorOptions): Promise<void> {
		await this.destroy();
		this.lastProcessorOptions = opts;
		await this.setup(opts.track);
	}

	private readonly handleContextLost = (): void => {
		const processorOptions = this.lastProcessorOptions;
		this.contextLostRestartCount += 1;
		if (!processorOptions) {
			logger.warn('Camera mirror canvas context lost without restartable processor options');
			return;
		}
		if (this.contextLostRestartCount > CONTEXT_LOST_RESTARTS_MAX) {
			logger.error('Camera mirror canvas context lost beyond the restart budget; dropping the processor restart', {
				restartCount: this.contextLostRestartCount,
				restartsMax: CONTEXT_LOST_RESTARTS_MAX,
			});
			return;
		}
		logger.warn('Camera mirror canvas context lost; restarting processor', {
			restartCount: this.contextLostRestartCount,
		});
		void this.restart(processorOptions).catch((error) => {
			logger.warn('Failed to restart camera mirror processor after canvas context loss', {error});
		});
	};

	private registerContextLostListener(canvas: OffscreenCanvas | HTMLCanvasElement): void {
		this.contextLostCanvas = canvas;
		canvas.addEventListener('contextlost', this.handleContextLost);
	}

	private removeContextLostListener(): void {
		if (!this.contextLostCanvas) return;
		this.contextLostCanvas.removeEventListener('contextlost', this.handleContextLost);
		this.contextLostCanvas = null;
	}

	private readonly handleFallbackVisibilityChange = (): void => {
		this.updateFallbackFrameLoop();
	};

	private readonly handleFallbackSourceEnded = (): void => {
		this.stopFallbackFrameLoop();
	};

	async destroy(): Promise<void> {
		this.lastProcessorOptions = null;
		this.removeContextLostListener();
		this.removeFallbackLifecycleListeners();
		this.abortController?.abort();
		this.abortController = null;
		this.stopFallbackFrameLoop();
		if (this.fallbackVideoElement) {
			this.fallbackVideoElement.pause();
			this.fallbackVideoElement.srcObject = null;
			this.fallbackVideoElement.remove();
			this.fallbackVideoElement = null;
		}
		this.fallbackStream?.getTracks().forEach((track) => track.stop());
		this.fallbackStream = null;
		this.fallbackSourceTrack = null;
		this.processedTrack?.stop();
		this.processedTrack = undefined;
		this.canvas = null;
		this.canvasContext = null;
		await this.pipePromise?.catch(() => {});
		this.pipePromise = null;
	}

	private async setup(sourceTrack: MediaStreamTrack): Promise<void> {
		const globals = getTrackProcessorGlobals();
		if (globals.MediaStreamTrackProcessor && globals.MediaStreamTrackGenerator) {
			this.setupStreamProcessor(sourceTrack, globals.MediaStreamTrackProcessor, globals.MediaStreamTrackGenerator);
			return;
		}
		this.setupCanvasFallback(sourceTrack);
	}

	private setupStreamProcessor(
		sourceTrack: MediaStreamTrack,
		Processor: MediaStreamTrackProcessorConstructor,
		Generator: MediaStreamTrackGeneratorConstructor,
	): void {
		const sourceSettings = sourceTrack.getSettings();
		const initialWidth = sourceSettings.width ?? DEFAULT_FALLBACK_WIDTH;
		const initialHeight = sourceSettings.height ?? DEFAULT_FALLBACK_HEIGHT;
		const canvas = createCanvas(initialWidth, initialHeight);
		const canvasContext = getCanvasContext(canvas);
		if (!canvasContext) {
			throw new Error('Unable to create camera mirror canvas context');
		}
		this.canvas = canvas;
		this.canvasContext = canvasContext;
		this.registerContextLostListener(canvas);
		const processor = new Processor({track: sourceTrack});
		const generator = new Generator({kind: 'video', signalTarget: sourceTrack});
		this.abortController = new AbortController();
		this.pipePromise = this.pumpFrames(processor.readable, generator.writable, this.abortController.signal);
		this.processedTrack = generator;
	}

	private async pumpFrames(
		readable: ReadableStream<VideoFrame>,
		writable: WritableStream<VideoFrame>,
		signal: AbortSignal,
	): Promise<void> {
		const reader = readable.getReader();
		const writer = writable.getWriter();
		const cancelReader = () => {
			reader.cancel().catch(() => {});
		};
		signal.addEventListener('abort', cancelReader, {once: true});
		try {
			while (!signal.aborted) {
				const {done, value: frame} = await reader.read();
				if (done || !frame) {
					return;
				}
				if (signal.aborted) {
					frame.close();
					return;
				}
				await this.writeMirroredFrame(frame, writer);
			}
		} catch (error) {
			if (!signal.aborted) {
				logger.warn('Camera mirror stream processor failed', {error});
			}
		} finally {
			signal.removeEventListener('abort', cancelReader);
			cancelReader();
			writer.close().catch(() => {});
		}
	}

	private async writeMirroredFrame(frame: VideoFrame, writer: WritableStreamDefaultWriter<VideoFrame>): Promise<void> {
		const mirroredFrame = this.createMirroredFrame(frame);
		const outputFrame = mirroredFrame ?? frame;
		try {
			await writer.write(outputFrame);
		} catch (error) {
			outputFrame.close();
			throw error;
		} finally {
			if (mirroredFrame) {
				frame.close();
			}
		}
	}

	private createMirroredFrame(frame: VideoFrame): VideoFrame | null {
		const width = frame.displayWidth || frame.codedWidth;
		const height = frame.displayHeight || frame.codedHeight;
		if (!this.canvas || !this.canvasContext || width <= 0 || height <= 0) {
			return null;
		}
		try {
			resizeCanvas(this.canvas, width, height);
			this.drawMirrored(frame, width, height);
			return new VideoFrame(this.canvas, {
				timestamp: frame.timestamp ?? Math.round(performance.now() * 1000),
			});
		} catch (error) {
			logger.warn('Failed to mirror camera frame', {error});
			return null;
		}
	}

	private drawMirrored(source: CanvasImageSource, width: number, height: number): void {
		const ctx = this.canvasContext;
		if (!ctx) {
			return;
		}
		ctx.save();
		ctx.clearRect(0, 0, width, height);
		ctx.translate(width, 0);
		ctx.scale(-1, 1);
		ctx.drawImage(source, 0, 0, width, height);
		ctx.restore();
	}

	private setupCanvasFallback(sourceTrack: MediaStreamTrack): void {
		if (typeof HTMLCanvasElement === 'undefined' || !('captureStream' in HTMLCanvasElement.prototype)) {
			throw new Error('Camera mirror processing is not supported in this browser');
		}
		const sourceSettings = sourceTrack.getSettings();
		const width = sourceSettings.width ?? DEFAULT_FALLBACK_WIDTH;
		const height = sourceSettings.height ?? DEFAULT_FALLBACK_HEIGHT;
		const frameRate = sourceSettings.frameRate ?? DEFAULT_FALLBACK_FRAME_RATE;
		const videoElement = document.createElement('video');
		videoElement.muted = true;
		videoElement.autoplay = true;
		videoElement.playsInline = true;
		videoElement.srcObject = new MediaStream([sourceTrack]);
		const canvas = document.createElement('canvas');
		canvas.width = width;
		canvas.height = height;
		const canvasContext = getCanvasContext(canvas);
		if (!canvasContext) {
			throw new Error('Unable to create camera mirror fallback canvas context');
		}
		this.fallbackVideoElement = videoElement;
		this.fallbackSourceTrack = sourceTrack;
		this.canvas = canvas;
		this.canvasContext = canvasContext;
		this.registerContextLostListener(canvas);
		this.fallbackStream = canvas.captureStream(frameRate);
		this.processedTrack = this.fallbackStream.getVideoTracks()[0];
		videoElement.play().catch((error) => {
			logger.warn('Failed to start camera mirror fallback video element', {error});
		});
		this.fallbackMinFrameIntervalMs = 1000 / Math.max(1, frameRate);
		this.addFallbackLifecycleListeners(sourceTrack);
		this.updateFallbackFrameLoop();
	}

	private addFallbackLifecycleListeners(sourceTrack: MediaStreamTrack): void {
		document.addEventListener('visibilitychange', this.handleFallbackVisibilityChange);
		sourceTrack.addEventListener('ended', this.handleFallbackSourceEnded, {once: true});
	}

	private removeFallbackLifecycleListeners(): void {
		document.removeEventListener('visibilitychange', this.handleFallbackVisibilityChange);
		this.fallbackSourceTrack?.removeEventListener('ended', this.handleFallbackSourceEnded);
	}

	private shouldRunFallbackFrameLoop(): boolean {
		if (!this.fallbackVideoElement) return false;
		if (!this.canvas) return false;
		if (!this.canvasContext) return false;
		if (this.fallbackSourceTrack?.readyState !== 'live') return false;
		return document.visibilityState !== 'hidden';
	}

	private updateFallbackFrameLoop(): void {
		if (this.shouldRunFallbackFrameLoop()) {
			this.startFallbackFrameLoop();
			return;
		}
		this.stopFallbackFrameLoop();
	}

	private startFallbackFrameLoop(): void {
		if (this.fallbackAnimationFrameId !== null) return;
		this.fallbackAnimationFrameId = requestAnimationFrame(this.drawFallbackFrame);
	}

	private stopFallbackFrameLoop(): void {
		if (this.fallbackAnimationFrameId === null) return;
		cancelAnimationFrame(this.fallbackAnimationFrameId);
		this.fallbackAnimationFrameId = null;
	}

	private readonly drawFallbackFrame = (now: number): void => {
		this.fallbackAnimationFrameId = null;
		if (!this.shouldRunFallbackFrameLoop()) return;
		const videoElement = this.fallbackVideoElement;
		const canvas = this.canvas;
		if (!videoElement) return;
		if (!canvas) return;
		if (now - this.fallbackLastFrameAt >= this.fallbackMinFrameIntervalMs) {
			this.fallbackLastFrameAt = now;
			const nextWidth = videoElement.videoWidth || canvas.width;
			const nextHeight = videoElement.videoHeight || canvas.height;
			resizeCanvas(canvas, nextWidth, nextHeight);
			if (videoElement.readyState >= HTMLMediaElement.HAVE_CURRENT_DATA) {
				this.drawMirrored(videoElement, nextWidth, nextHeight);
			}
		}
		this.startFallbackFrameLoop();
	};
}

class CameraVideoProcessor implements CameraVideoProcessorHandle {
	name = 'camera-video-processor';
	processedTrack?: MediaStreamTrack;
	private mirrorProcessor: MirrorVideoProcessor | null = null;
	private backgroundPipeline: WebCameraPipeline | null = null;
	private currentOptions: CameraVideoProcessorOptions;
	private operationTail: Promise<void> = Promise.resolve();
	private destroyPromise: Promise<void> | null = null;
	private operationalFailureHandler: ((error: Error) => void) | null = null;
	private initialized = false;
	private destroyed = false;

	constructor(options: CameraVideoProcessorOptions) {
		this.currentOptions = options;
	}

	async init(opts: VideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (this.initialized || this.destroyed) {
				throw new Error('Camera video processor cannot be initialized in its current lifecycle state');
			}
			await this.setup(opts);
			this.initialized = true;
		});
	}

	async restart(opts: VideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (this.destroyed) {
				throw new Error('Cannot restart a destroyed camera video processor');
			}
			await this.teardown();
			this.initialized = false;
			await this.setup(opts);
			this.initialized = true;
		});
	}

	destroy(): Promise<void> {
		if (this.destroyPromise == null) {
			this.destroyPromise = this.runExclusive(async () => {
				if (this.destroyed) {
					return;
				}
				await this.teardown();
				this.initialized = false;
				this.destroyed = true;
			});
		}
		return this.destroyPromise;
	}

	canUpdate(options: CameraVideoProcessorOptions): boolean {
		const currentMirror = this.currentOptions.mirror === true;
		const nextMirror = options.mirror === true;
		if (currentMirror !== nextMirror) {
			return false;
		}
		const currentHasBackground = this.currentOptions.background != null;
		const nextHasBackground = options.background != null;
		return currentHasBackground === nextHasBackground;
	}

	setOperationalFailureHandler(handler: (error: Error) => void): void {
		if (this.operationalFailureHandler != null) {
			throw new Error('Camera video processor operational failure handler was already assigned');
		}
		this.operationalFailureHandler = handler;
	}

	async update(options: CameraVideoProcessorOptions): Promise<void> {
		await this.runExclusive(async () => {
			if (!this.initialized || this.destroyed) {
				throw new Error('Cannot update an inactive camera video processor');
			}
			if (!this.canUpdate(options)) {
				throw new Error('Camera video processor update would change its output topology');
			}
			const background = options.background ?? null;
			if (background != null) {
				const backgroundPipeline = this.backgroundPipeline;
				if (backgroundPipeline == null) {
					throw new Error('Camera video processor has no active background pipeline to update');
				}
				await backgroundPipeline.updateConfig({background});
			}
			this.currentOptions = options;
		});
	}

	private runExclusive<T>(operation: () => Promise<T>): Promise<T> {
		const result = this.operationTail.then(operation);
		this.operationTail = result.then(
			() => undefined,
			() => undefined,
		);
		return result;
	}

	private async teardown(): Promise<void> {
		const backgroundPipeline = this.backgroundPipeline;
		if (backgroundPipeline) {
			try {
				backgroundPipeline.beginStop();
			} catch (error) {
				logger.warn('Failed to signal camera background pipeline stop intent', {error});
			}
		}
		const mirrorProcessor = this.mirrorProcessor;
		this.backgroundPipeline = null;
		this.mirrorProcessor = null;
		this.processedTrack = undefined;
		if (backgroundPipeline) {
			try {
				backgroundPipeline.stop();
			} catch (error) {
				logger.warn('Failed to stop camera background pipeline', {error});
			}
		}
		await mirrorProcessor?.destroy();
	}

	private async setup(opts: VideoProcessorOptions): Promise<void> {
		const background = this.currentOptions.background ?? null;
		try {
			let inputTrack = opts.track;
			if (this.currentOptions.mirror) {
				const mirrorProcessor = new MirrorVideoProcessor();
				this.mirrorProcessor = mirrorProcessor;
				await mirrorProcessor.init({...opts, track: inputTrack});
				inputTrack = mirrorProcessor.processedTrack ?? inputTrack;
			}
			if (background) {
				const backgroundPipeline = await WebCameraEffectPipeline.create({
					source: inputTrack,
					config: {background},
					onFailure: (_pipeline, error) => {
						this.handleBackgroundPipelineFailure(error);
					},
				});
				this.backgroundPipeline = backgroundPipeline;
				this.processedTrack = backgroundPipeline.outputTrack;
				return;
			}
			this.processedTrack = inputTrack === opts.track ? undefined : inputTrack;
		} catch (error) {
			await this.teardown();
			throw error;
		}
	}

	private handleBackgroundPipelineFailure(error: unknown): void {
		const normalizedError =
			error instanceof Error ? error : new Error('Camera background pipeline failed', {cause: error});
		const handler = this.operationalFailureHandler;
		if (handler == null) {
			logger.error('Camera background pipeline failed without a recovery owner', {error: normalizedError});
			return;
		}
		handler(normalizedError);
	}
}

export function createCameraVideoProcessor(options: CameraVideoProcessorOptions): CameraVideoProcessorHandle {
	return new CameraVideoProcessor(options);
}

export function isCameraVideoProcessor(processor: unknown): processor is CameraVideoProcessorHandle {
	return processor instanceof CameraVideoProcessor;
}
