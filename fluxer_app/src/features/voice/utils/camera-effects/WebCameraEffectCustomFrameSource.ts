// SPDX-License-Identifier: AGPL-3.0-or-later

import {runWithResponseDeadline} from '@app/features/voice/utils/camera-effects/BoundedResponse';
import {validateCameraEffectFrameDimensions} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';

const MAX_ANIMATED_IMAGE_FRAMES = 300;
const MAX_ANIMATED_IMAGE_AGGREGATE_PIXELS = 32_000_000;
const MAX_ANIMATED_IMAGE_DURATION_MICROSECONDS = 60_000_000;
const MIN_ANIMATED_FRAME_DURATION_MICROSECONDS = 20_000;
const MAX_ANIMATED_FRAME_DURATION_MICROSECONDS = 10_000_000;
const MAX_ANIMATED_FRAME_SEARCH_STEPS = 10;
const VIDEO_FIRST_FRAME_TIMEOUT_MS = 8_000;
const VIDEO_LEASE_RELEASE_TIMEOUT_MS = 1_000;

export const WebCameraEffectCustomFrameSourceKind = Object.freeze({
	STATIC: 'static',
	ANIMATED: 'animated',
	VIDEO: 'video',
} as const);

export type WebCameraEffectCustomFrameSourceKind =
	(typeof WebCameraEffectCustomFrameSourceKind)[keyof typeof WebCameraEffectCustomFrameSourceKind];

export type WebCameraEffectCustomFrameImage = ImageBitmap | VideoFrame;

export interface WebCameraEffectCustomFrame {
	readonly image: WebCameraEffectCustomFrameImage;
	readonly width: number;
	readonly height: number;
	readonly index: number;
}

export interface WebCameraEffectCustomFrameLease {
	readonly frame: WebCameraEffectCustomFrame;
	release(): void;
}

export interface WebCameraEffectCustomFrameSource {
	readonly kind: WebCameraEffectCustomFrameSourceKind;
	readonly width: number;
	readonly height: number;
	readonly frameCount: number | null;
	acquireFrame(nowMilliseconds: number): WebCameraEffectCustomFrameLease;
	dispose(): Promise<void>;
}

interface MutableWebCameraEffectCustomFrame {
	image: WebCameraEffectCustomFrameImage;
	width: number;
	height: number;
	index: number;
}

class ReusableWebCameraEffectCustomFrameLease implements WebCameraEffectCustomFrameLease {
	private value: WebCameraEffectCustomFrame | null = null;

	constructor(private readonly onRelease: () => void) {}

	get frame(): WebCameraEffectCustomFrame {
		if (this.value == null) {
			throw new Error('Custom camera background frame lease is not active');
		}
		return this.value;
	}

	get active(): boolean {
		return this.value != null;
	}

	acquire(frame: WebCameraEffectCustomFrame): WebCameraEffectCustomFrameLease {
		if (this.value != null) {
			throw new Error('Custom camera background frame source already has an active lease');
		}
		this.value = frame;
		return this;
	}

	release(): void {
		if (this.value == null) {
			throw new Error('Custom camera background frame lease was released more than once');
		}
		this.value = null;
		this.onRelease();
	}
}

class StaticWebCameraEffectCustomFrameSource implements WebCameraEffectCustomFrameSource {
	readonly kind = WebCameraEffectCustomFrameSourceKind.STATIC;
	readonly width: number;
	readonly height: number;
	readonly frameCount = 1;
	private readonly frame: WebCameraEffectCustomFrame;
	private readonly lease = new ReusableWebCameraEffectCustomFrameLease(() => {});
	private disposed = false;

	constructor(image: WebCameraEffectCustomFrameImage) {
		const dimensions = customFrameImageDimensions(image);
		this.width = dimensions.width;
		this.height = dimensions.height;
		this.frame = {image, width: this.width, height: this.height, index: 0};
	}

	acquireFrame(nowMilliseconds: number): WebCameraEffectCustomFrameLease {
		requireFrameSourceTime(nowMilliseconds);
		this.requireActive();
		return this.lease.acquire(this.frame);
	}

	async dispose(): Promise<void> {
		if (this.disposed) return;
		if (this.lease.active) {
			throw new Error('Cannot dispose a custom camera background while its frame is leased');
		}
		this.disposed = true;
		this.frame.image.close();
	}

	private requireActive(): void {
		if (this.disposed) {
			throw new Error('Cannot read a disposed custom camera background frame source');
		}
	}
}

class AnimatedWebCameraEffectCustomFrameSource implements WebCameraEffectCustomFrameSource {
	readonly kind = WebCameraEffectCustomFrameSourceKind.ANIMATED;
	readonly width: number;
	readonly height: number;
	readonly frameCount: number;
	private readonly frames: ReadonlyArray<WebCameraEffectCustomFrame>;
	private readonly frameEndMicroseconds: ReadonlyArray<number>;
	private readonly totalDurationMicroseconds: number;
	private readonly lease = new ReusableWebCameraEffectCustomFrameLease(() => {});
	private epochMilliseconds: number | null = null;
	private disposed = false;

	constructor(frames: ReadonlyArray<VideoFrame>, frameEndMicroseconds: ReadonlyArray<number>) {
		const firstFrame = frames[0];
		if (firstFrame == null) {
			throw new Error('Animated custom camera background requires at least one decoded frame');
		}
		this.width = firstFrame.displayWidth;
		this.height = firstFrame.displayHeight;
		this.frameCount = frames.length;
		this.frames = frames.map((image, index) => ({image, width: this.width, height: this.height, index}));
		this.frameEndMicroseconds = frameEndMicroseconds;
		this.totalDurationMicroseconds = frameEndMicroseconds[frameEndMicroseconds.length - 1] ?? 0;
		if (this.totalDurationMicroseconds <= 0) {
			throw new Error('Animated custom camera background has no positive duration');
		}
	}

	acquireFrame(nowMilliseconds: number): WebCameraEffectCustomFrameLease {
		requireFrameSourceTime(nowMilliseconds);
		this.requireActive();
		return this.lease.acquire(this.requireFrame(this.frameIndexAt(nowMilliseconds)));
	}

	async dispose(): Promise<void> {
		if (this.disposed) return;
		if (this.lease.active) {
			throw new Error('Cannot dispose a custom camera background while its frame is leased');
		}
		this.disposed = true;
		const failures = closeFrameImages(this.frames.map((frame) => frame.image));
		throwCleanupFailures(failures, 'Animated custom camera background teardown failed');
	}

	private frameIndexAt(nowMilliseconds: number): number {
		if (nowMilliseconds === 0) return 0;
		if (this.epochMilliseconds == null) {
			this.epochMilliseconds = nowMilliseconds;
			return 0;
		}
		if (nowMilliseconds < this.epochMilliseconds) {
			throw new Error('Custom camera background frame time moved backwards');
		}
		const elapsedMicroseconds = Math.floor((nowMilliseconds - this.epochMilliseconds) * 1000);
		return this.findFrameIndex(elapsedMicroseconds % this.totalDurationMicroseconds);
	}

	private findFrameIndex(positionMicroseconds: number): number {
		let low = 0;
		let high = this.frameEndMicroseconds.length - 1;
		for (let step = 0; step < MAX_ANIMATED_FRAME_SEARCH_STEPS; step += 1) {
			if (low >= high) return low;
			const middle = low + Math.floor((high - low) / 2);
			if ((this.frameEndMicroseconds[middle] ?? 0) > positionMicroseconds) high = middle;
			else low = middle + 1;
		}
		throw new Error('Animated custom camera background frame search exceeded its bound');
	}

	private requireFrame(index: number): WebCameraEffectCustomFrame {
		const frame = this.frames[index];
		if (frame == null) {
			throw new Error('Animated custom camera background selected an unavailable frame');
		}
		return frame;
	}

	private requireActive(): void {
		if (this.disposed) {
			throw new Error('Cannot read a disposed custom camera background frame source');
		}
	}
}

class VideoWebCameraEffectCustomFrameSource implements WebCameraEffectCustomFrameSource {
	readonly kind = WebCameraEffectCustomFrameSourceKind.VIDEO;
	readonly frameCount = null;
	private readonly reader: ReadableStreamDefaultReader<VideoFrame>;
	private readonly lease = new ReusableWebCameraEffectCustomFrameLease(() => this.releaseFrameLease());
	private readonly frame: MutableWebCameraEffectCustomFrame = {
		image: null as unknown as VideoFrame,
		width: 0,
		height: 0,
		index: 0,
	};
	private readonly firstFramePromise: Promise<void>;
	private resolveFirstFrame: (() => void) | null = null;
	private rejectFirstFrame: ((error: unknown) => void) | null = null;
	private currentFrame: VideoFrame | null = null;
	private pendingFrame: VideoFrame | null = null;
	private currentFrameIndex = 0;
	private pendingFrameIndex = 0;
	private failure: unknown = null;
	private failed = false;
	private nextFrameIndex = 0;
	private stopping = false;
	private disposed = false;
	private leaseReleaseResolve: (() => void) | null = null;
	private leaseReleaseTimedOut = false;
	private readonly pumpPromise: Promise<void>;
	private disposePromise: Promise<void> | null = null;

	constructor(readable: ReadableStream<VideoFrame>) {
		this.reader = readable.getReader();
		this.firstFramePromise = new Promise<void>((resolve, reject) => {
			this.resolveFirstFrame = resolve;
			this.rejectFirstFrame = reject;
		});
		this.pumpPromise = this.pump();
	}

	get width(): number {
		return this.currentFrame?.displayWidth ?? 0;
	}

	get height(): number {
		return this.currentFrame?.displayHeight ?? 0;
	}

	async waitForFirstFrame(signal: AbortSignal): Promise<void> {
		await waitForAbortablePromise(this.firstFramePromise, signal);
	}

	acquireFrame(nowMilliseconds: number): WebCameraEffectCustomFrameLease {
		requireFrameSourceTime(nowMilliseconds);
		this.requireActive();
		this.promotePendingFrame();
		const currentFrame = this.currentFrame;
		if (currentFrame == null) {
			throw new Error('Custom camera background video has no current frame');
		}
		this.frame.image = currentFrame;
		this.frame.width = currentFrame.displayWidth;
		this.frame.height = currentFrame.displayHeight;
		this.frame.index = this.currentFrameIndex;
		return this.lease.acquire(this.frame);
	}

	dispose(): Promise<void> {
		if (this.disposePromise == null) {
			this.disposePromise = this.disposeOwned();
		}
		return this.disposePromise;
	}

	private async pump(): Promise<void> {
		try {
			while (!this.stopping) {
				const result = await this.reader.read();
				if (result.done) {
					if (!this.stopping) throw new Error('Custom camera background video frame stream ended');
					break;
				}
				if (this.stopping) {
					result.value.close();
					break;
				}
				this.acceptFrame(result.value);
			}
		} catch (error) {
			if (!this.stopping) this.recordFailure(error);
		}
	}

	private acceptFrame(frame: VideoFrame): void {
		try {
			validateCameraEffectFrameDimensions(frame.displayWidth, frame.displayHeight);
			if (this.currentFrame != null) this.requireStableDimensions(frame);
		} catch (error) {
			frame.close();
			throw error;
		}
		const frameIndex = this.takeFrameIndex();
		const currentFrame = this.currentFrame;
		if (currentFrame == null) {
			this.currentFrame = frame;
			this.currentFrameIndex = frameIndex;
			this.resolveFirstFrame?.();
			this.clearFirstFrameSettlers();
			return;
		}
		if (this.lease.active) {
			const replaced = this.pendingFrame;
			this.pendingFrame = frame;
			this.pendingFrameIndex = frameIndex;
			replaced?.close();
			return;
		}
		this.currentFrame = frame;
		this.currentFrameIndex = frameIndex;
		currentFrame.close();
	}

	private requireStableDimensions(frame: VideoFrame): void {
		const currentFrame = this.currentFrame;
		if (currentFrame == null) {
			throw new Error('Custom camera background video has no current frame for dimension validation');
		}
		if (frame.displayWidth !== currentFrame.displayWidth) {
			throw new Error('Custom camera background video width changed');
		}
		if (frame.displayHeight !== currentFrame.displayHeight) {
			throw new Error('Custom camera background video height changed');
		}
	}

	private takeFrameIndex(): number {
		const frameIndex = this.nextFrameIndex;
		this.nextFrameIndex = (this.nextFrameIndex + 1) % Number.MAX_SAFE_INTEGER;
		return frameIndex;
	}

	private promotePendingFrame(): void {
		const pendingFrame = this.pendingFrame;
		if (this.lease.active || pendingFrame == null) return;
		const replaced = this.currentFrame;
		this.currentFrame = pendingFrame;
		this.currentFrameIndex = this.pendingFrameIndex;
		this.pendingFrame = null;
		replaced?.close();
	}

	private releaseFrameLease(): void {
		let failure: unknown;
		try {
			this.promotePendingFrame();
		} catch (error) {
			failure = error;
		}
		this.leaseReleaseResolve?.();
		this.leaseReleaseResolve = null;
		if (this.stopping && this.leaseReleaseTimedOut) {
			const currentFrame = this.currentFrame;
			this.currentFrame = null;
			try {
				currentFrame?.close();
			} catch (error) {
				if (failure !== undefined) {
					throw new AggregateError([failure, error], 'Custom camera background video frame release failed');
				}
				throw error;
			}
		}
		if (failure !== undefined) throw failure;
	}

	private recordFailure(error: unknown): void {
		if (this.failed) return;
		this.failed = true;
		this.failure = error ?? new Error('Custom camera background video frame stream failed without a reason');
		this.rejectFirstFrame?.(this.failure);
		this.clearFirstFrameSettlers();
	}

	private clearFirstFrameSettlers(): void {
		this.resolveFirstFrame = null;
		this.rejectFirstFrame = null;
	}

	private requireActive(): void {
		if (this.disposed || this.stopping) {
			throw new Error('Cannot read a disposed custom camera background video source');
		}
		if (this.failed) {
			throw this.failure;
		}
	}

	private async disposeOwned(): Promise<void> {
		if (this.disposed) return;
		this.stopping = true;
		this.rejectFirstFrame?.(new Error('Custom camera background video source was disposed before its first frame'));
		this.clearFirstFrameSettlers();
		const cancellation = Promise.resolve().then(() => this.reader.cancel());
		const failures = await settledFailures([cancellation, this.pumpPromise]);
		try {
			this.reader.releaseLock();
		} catch (error) {
			failures.push(error);
		}
		this.closeOwnedFrame('pendingFrame', failures);
		if (this.lease.active) {
			try {
				await this.waitForLeaseRelease();
			} catch (error) {
				this.leaseReleaseTimedOut = true;
				failures.push(error);
			}
		}
		if (!this.lease.active) this.closeOwnedFrame('currentFrame', failures);
		this.disposed = true;
		throwCleanupFailures(failures, 'Custom camera background video teardown failed');
	}

	private closeOwnedFrame(key: 'currentFrame' | 'pendingFrame', failures: Array<unknown>): void {
		const frame = this[key];
		this[key] = null;
		if (frame == null) return;
		try {
			frame.close();
		} catch (error) {
			failures.push(error);
		}
	}

	private waitForLeaseRelease(): Promise<void> {
		return new Promise<void>((resolve, reject) => {
			const timeout = setTimeout(() => {
				this.leaseReleaseResolve = null;
				reject(new Error('Custom camera background video frame lease exceeded its release deadline'));
			}, VIDEO_LEASE_RELEASE_TIMEOUT_MS);
			this.leaseReleaseResolve = () => {
				clearTimeout(timeout);
				resolve();
			};
		});
	}
}

function customFrameImageDimensions(image: WebCameraEffectCustomFrameImage): {width: number; height: number} {
	if ('displayWidth' in image) {
		return {width: image.displayWidth, height: image.displayHeight};
	}
	return {width: image.width, height: image.height};
}

function requireFrameSourceTime(nowMilliseconds: number): void {
	if (!Number.isFinite(nowMilliseconds) || nowMilliseconds < 0) {
		throw new Error('Custom camera background frame time must be finite and non-negative');
	}
}

function closeFrameImages(images: ReadonlyArray<WebCameraEffectCustomFrameImage>): Array<unknown> {
	const failures: Array<unknown> = [];
	for (const image of images) {
		try {
			image.close();
		} catch (error) {
			failures.push(error);
		}
	}
	return failures;
}

function throwCleanupFailures(failures: ReadonlyArray<unknown>, message: string): void {
	if (failures.length === 0) return;
	if (failures.length === 1) throw failures[0];
	throw new AggregateError(failures, message);
}

function throwWithCleanupFailures(error: unknown, failures: ReadonlyArray<unknown>, message: string): never {
	if (failures.length === 0) throw error;
	throw new AggregateError([error, ...failures], message);
}

async function settledFailures(promises: ReadonlyArray<Promise<unknown>>): Promise<Array<unknown>> {
	const outcomes = await Promise.allSettled(promises);
	const failures: Array<unknown> = [];
	for (const outcome of outcomes) {
		if (outcome.status === 'rejected') failures.push(outcome.reason);
	}
	return failures;
}

function waitForAbortablePromise<T>(promise: Promise<T>, signal: AbortSignal): Promise<T> {
	if (signal.aborted) return Promise.reject(signal.reason);
	return new Promise<T>((resolve, reject) => {
		const handleAbort = (): void => reject(signal.reason);
		signal.addEventListener('abort', handleAbort, {once: true});
		promise.then(
			(value) => {
				signal.removeEventListener('abort', handleAbort);
				resolve(value);
			},
			(error: unknown) => {
				signal.removeEventListener('abort', handleAbort);
				reject(error);
			},
		);
	});
}

function throwIfAborted(signal: AbortSignal): void {
	if (signal.aborted) throw signal.reason ?? new Error('Custom camera background operation was aborted');
}

function requireAnimatedFrameDuration(duration: number | null): number {
	if (duration == null || !Number.isSafeInteger(duration) || duration < 0) {
		throw new Error('Animated custom camera background has an invalid frame duration');
	}
	const normalizedDuration = Math.max(duration, MIN_ANIMATED_FRAME_DURATION_MICROSECONDS);
	if (normalizedDuration > MAX_ANIMATED_FRAME_DURATION_MICROSECONDS) {
		throw new Error('Animated custom camera background frame duration exceeds the supported maximum');
	}
	return normalizedDuration;
}

function validateAnimatedImageBudget(width: number, height: number, frameCount: number): void {
	validateCameraEffectFrameDimensions(width, height);
	const aggregatePixels = width * height * frameCount;
	if (!Number.isSafeInteger(aggregatePixels)) {
		throw new Error('Animated custom camera background decoded pixel budget is invalid');
	}
	if (aggregatePixels > MAX_ANIMATED_IMAGE_AGGREGATE_PIXELS) {
		throw new Error('Animated custom camera background exceeds the aggregate decoded pixel budget');
	}
}

function validateDecodedFrameDimensions(frame: VideoFrame, width: number, height: number): void {
	validateCameraEffectFrameDimensions(frame.displayWidth, frame.displayHeight);
	if (frame.displayWidth !== width || frame.displayHeight !== height) {
		throw new Error('Animated custom camera background frame dimensions changed during decoding');
	}
}

async function decodeFrames(
	decoder: ImageDecoder,
	frameCount: number,
	animated: boolean,
	signal: AbortSignal,
): Promise<WebCameraEffectCustomFrameSource> {
	if (!animated && frameCount !== 1) {
		throw new Error('Static custom camera background unexpectedly contains multiple frames');
	}
	const frames: Array<VideoFrame> = [];
	const frameEndMicroseconds: Array<number> = [];
	let width = 0;
	let height = 0;
	let totalDurationMicroseconds = 0;
	let decodedFrame: VideoFrame | null = null;
	try {
		for (let frameIndex = 0; frameIndex < frameCount; frameIndex += 1) {
			throwIfAborted(signal);
			const result = await decoder.decode({frameIndex, completeFramesOnly: true});
			decodedFrame = result.image;
			throwIfAborted(signal);
			if (!result.complete) {
				throw new Error('Animated custom camera background produced an incomplete decoded frame');
			}
			if (frameIndex === 0) {
				width = decodedFrame.displayWidth;
				height = decodedFrame.displayHeight;
				validateAnimatedImageBudget(width, height, frameCount);
			}
			validateDecodedFrameDimensions(decodedFrame, width, height);
			if (animated) {
				totalDurationMicroseconds += requireAnimatedFrameDuration(decodedFrame.duration);
				if (totalDurationMicroseconds > MAX_ANIMATED_IMAGE_DURATION_MICROSECONDS) {
					throw new Error('Animated custom camera background exceeds the supported duration');
				}
				frameEndMicroseconds.push(totalDurationMicroseconds);
			}
			frames.push(decodedFrame);
			decodedFrame = null;
		}
	} catch (error) {
		const failures = closeFrameImages(decodedFrame == null ? frames : [decodedFrame, ...frames]);
		throwWithCleanupFailures(error, failures, 'Animated custom camera background decoding failed during cleanup');
	}
	const firstFrame = frames[0];
	if (firstFrame == null) {
		throw new Error('Animated custom camera background decoding produced no frames');
	}
	if (!animated) return new StaticWebCameraEffectCustomFrameSource(firstFrame);
	try {
		return new AnimatedWebCameraEffectCustomFrameSource(frames, frameEndMicroseconds);
	} catch (error) {
		const failures = closeFrameImages(frames);
		throwWithCleanupFailures(error, failures, 'Animated custom camera background setup failed during cleanup');
	}
}

type CameraImageDecoderConstructor = {
	new (init: ImageDecoderInit): ImageDecoder;
	isTypeSupported(type: string): Promise<boolean>;
};

function getCameraImageDecoderConstructor(): CameraImageDecoderConstructor | null {
	const candidate: unknown = Reflect.get(globalThis, 'ImageDecoder');
	if (typeof candidate !== 'function') return null;
	if (typeof Reflect.get(candidate, 'isTypeSupported') !== 'function') return null;
	return candidate as CameraImageDecoderConstructor;
}

async function decodeImageDecoderSource(
	blob: Blob,
	mediaType: string,
	signal: AbortSignal,
): Promise<WebCameraEffectCustomFrameSource> {
	const ImageDecoderConstructor = getCameraImageDecoderConstructor();
	if (ImageDecoderConstructor == null) {
		throw new Error('Animated custom camera backgrounds require ImageDecoder');
	}
	if (!(await ImageDecoderConstructor.isTypeSupported(mediaType))) {
		throw new Error(`Animated custom camera background type is unsupported: ${mediaType}`);
	}
	throwIfAborted(signal);
	const decoder = new ImageDecoderConstructor({data: blob.stream(), type: mediaType, preferAnimation: true});
	let decoderClosed = false;
	let source: WebCameraEffectCustomFrameSource | null = null;
	const closeDecoder = (): void => {
		if (decoderClosed) return;
		decoderClosed = true;
		decoder.close();
	};
	const handleAbort = (): void => closeDecoder();
	signal.addEventListener('abort', handleAbort, {once: true});
	try {
		await Promise.all([decoder.tracks.ready, decoder.completed]);
		throwIfAborted(signal);
		const selectedTrack = decoder.tracks.selectedTrack;
		if (selectedTrack == null) {
			throw new Error('Animated custom camera background has no selected image track');
		}
		const frameCount = selectedTrack.frameCount;
		if (!Number.isSafeInteger(frameCount) || frameCount <= 0) {
			throw new Error('Animated custom camera background has an invalid frame count');
		}
		if (frameCount > MAX_ANIMATED_IMAGE_FRAMES) {
			throw new Error('Animated custom camera background exceeds the supported frame count');
		}
		const animated = mediaType === 'image/gif' || selectedTrack.animated;
		source = await decodeFrames(decoder, frameCount, animated, signal);
		closeDecoder();
		return source;
	} catch (error) {
		const failures: Array<unknown> = [];
		try {
			closeDecoder();
		} catch (cleanupError) {
			failures.push(cleanupError);
		}
		if (source != null) {
			try {
				await source.dispose();
			} catch (cleanupError) {
				failures.push(cleanupError);
			}
		}
		throwWithCleanupFailures(error, failures, 'Animated custom camera background initialization failed during cleanup');
	} finally {
		signal.removeEventListener('abort', handleAbort);
	}
}

async function decodeStaticSource(blob: Blob, signal: AbortSignal): Promise<WebCameraEffectCustomFrameSource> {
	const image = await createImageBitmap(blob);
	try {
		throwIfAborted(signal);
		validateCameraEffectFrameDimensions(image.width, image.height);
		return new StaticWebCameraEffectCustomFrameSource(image);
	} catch (error) {
		const failures = closeFrameImages([image]);
		throwWithCleanupFailures(error, failures, 'Custom camera background validation failed during cleanup');
	}
}

export function createWebCameraEffectImageFrameSource(
	blob: Blob,
	mediaType: string,
	signal: AbortSignal,
): Promise<WebCameraEffectCustomFrameSource> {
	if (mediaType === 'image/gif' || mediaType === 'image/webp') {
		return decodeImageDecoderSource(blob, mediaType, signal);
	}
	return decodeStaticSource(blob, signal);
}

export async function createWebCameraEffectVideoFrameSource(
	readable: ReadableStream<VideoFrame>,
): Promise<WebCameraEffectCustomFrameSource> {
	const source = new VideoWebCameraEffectCustomFrameSource(readable);
	try {
		await runWithResponseDeadline({
			timeoutMilliseconds: VIDEO_FIRST_FRAME_TIMEOUT_MS,
			description: 'Custom camera background video first frame',
			signal: null,
			operation: (signal) => source.waitForFirstFrame(signal),
		});
		return source;
	} catch (error) {
		try {
			await source.dispose();
		} catch (cleanupError) {
			throw new AggregateError(
				[error, cleanupError],
				'Custom camera background video initialization failed during cleanup',
			);
		}
		throw error;
	}
}
