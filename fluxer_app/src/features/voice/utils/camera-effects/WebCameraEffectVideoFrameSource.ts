// SPDX-License-Identifier: AGPL-3.0-or-later

import {runWithResponseDeadline} from '@app/features/voice/utils/camera-effects/BoundedResponse';
import {
	readWebCameraEffectCustomMediaBlob,
	requireWebCameraEffectCustomMediaURL,
	WEB_CAMERA_EFFECT_CUSTOM_MEDIA_OPERATION_TIMEOUT_MS,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomImage';
import {validateCameraEffectFrameDimensions} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';

const MAX_CUSTOM_VIDEO_DURATION_SECONDS = 60;
const SUPPORTED_VIDEO_MEDIA_TYPES = new Set(['video/mp4', 'video/webm']);
const VIDEO_HEADER_SNIFF_BYTES = 128;
const MP4_FILE_TYPE_BOX_OFFSET = 4;
const MP4_FILE_TYPE_BOX: ReadonlyArray<number> = [0x66, 0x74, 0x79, 0x70];
const EBML_MAGIC: ReadonlyArray<number> = [0x1a, 0x45, 0xdf, 0xa3];
const EBML_DOC_TYPE_ELEMENT_ID: ReadonlyArray<number> = [0x42, 0x82];
const EBML_VINT_SINGLE_BYTE_MARKER = 0x80;
const EBML_VINT_SINGLE_BYTE_VALUE_MASK = 0x7f;
const MAX_EBML_DOC_TYPE_LENGTH = 16;
const WEBM_DOC_TYPE = 'webm';

export interface WebCameraEffectVideoFrameProducer {
	readonly readable: ReadableStream<VideoFrame>;
	readonly width: number;
	readonly height: number;
	stop(): void;
}

class WebCameraEffectVideoFrameProducerOwner implements WebCameraEffectVideoFrameProducer {
	readonly readable: ReadableStream<VideoFrame>;
	private readonly video: HTMLVideoElement;
	private readonly objectURL: string;
	private controller: ReadableStreamDefaultController<VideoFrame> | null = null;
	private pendingFrame: VideoFrame | null = null;
	private pullResolve: (() => void) | null = null;
	private callbackHandle: number | null = null;
	private started = false;
	private stopped = false;
	private videoWidth = 0;
	private videoHeight = 0;

	constructor(blob: Blob) {
		this.video = document.createElement('video');
		this.video.autoplay = true;
		this.video.crossOrigin = 'anonymous';
		this.video.loop = true;
		this.video.muted = true;
		this.video.playsInline = true;
		this.video.preload = 'auto';
		this.readable = new ReadableStream<VideoFrame>(
			{
				start: (controller): void => {
					this.controller = controller;
				},
				pull: (controller) => this.pullFrame(controller),
				cancel: () => this.cancelFromStream(),
			},
			{highWaterMark: 0},
		);
		this.objectURL = URL.createObjectURL(blob);
	}

	get width(): number {
		return this.videoWidth;
	}

	get height(): number {
		return this.videoHeight;
	}

	async start(signal: AbortSignal): Promise<void> {
		if (this.started) {
			throw new Error('Custom camera background video producer was started more than once');
		}
		this.started = true;
		if (typeof this.video.requestVideoFrameCallback !== 'function') {
			throw new Error('Custom camera background video requires requestVideoFrameCallback');
		}
		if (!('VideoFrame' in globalThis)) {
			throw new Error('Custom camera background video requires VideoFrame');
		}
		this.video.src = this.objectURL;
		this.video.load();
		await waitForVideoMetadata(this.video, signal);
		this.validateMetadata();
		this.video.addEventListener('error', this.handlePlaybackError);
		this.video.addEventListener('ended', this.handleUnexpectedEnd);
		await waitForAbortablePromise(this.video.play(), signal);
		throwIfAborted(signal);
		this.scheduleNextFrame();
	}

	stop(): void {
		const failures = this.releaseOwnedResources(true);
		throwCleanupFailures(failures, 'Custom camera background video producer teardown failed');
	}

	private pullFrame(controller: ReadableStreamDefaultController<VideoFrame>): void | Promise<void> {
		if (this.stopped) return;
		if (this.pendingFrame != null) {
			const frame = this.pendingFrame;
			this.pendingFrame = null;
			controller.enqueue(frame);
			return;
		}
		if (this.pullResolve != null) {
			throw new Error('Custom camera background video stream has more than one pending pull');
		}
		return new Promise<void>((resolve) => {
			this.pullResolve = resolve;
		});
	}

	private cancelFromStream(): void {
		const failures = this.releaseOwnedResources(false);
		throwCleanupFailures(failures, 'Custom camera background video stream cancellation failed');
	}

	private validateMetadata(): void {
		validateCameraEffectFrameDimensions(this.video.videoWidth, this.video.videoHeight);
		if (!Number.isFinite(this.video.duration) || this.video.duration <= 0) {
			throw new Error('Custom camera background video has an invalid duration');
		}
		if (this.video.duration > MAX_CUSTOM_VIDEO_DURATION_SECONDS) {
			throw new Error('Custom camera background video exceeds the supported duration');
		}
		this.videoWidth = this.video.videoWidth;
		this.videoHeight = this.video.videoHeight;
	}

	private scheduleNextFrame(): void {
		if (this.stopped) return;
		try {
			this.callbackHandle = this.video.requestVideoFrameCallback(this.handleVideoFrame);
		} catch (error) {
			this.fail(error);
		}
	}

	private readonly handleVideoFrame: VideoFrameRequestCallback = (_now, metadata): void => {
		this.callbackHandle = null;
		if (this.stopped) return;
		let frame: VideoFrame | null = null;
		try {
			if (!Number.isFinite(metadata.mediaTime) || metadata.mediaTime < 0) {
				throw new Error('Custom camera background video produced an invalid frame timestamp');
			}
			const timestamp = Math.round(metadata.mediaTime * 1_000_000);
			if (!Number.isSafeInteger(timestamp)) {
				throw new Error('Custom camera background video frame timestamp exceeds the safe range');
			}
			frame = new VideoFrame(this.video, {timestamp});
			validateCameraEffectFrameDimensions(frame.displayWidth, frame.displayHeight);
			this.publishFrame(frame);
			frame = null;
		} catch (error) {
			frame?.close();
			this.fail(error);
			return;
		}
		this.scheduleNextFrame();
	};

	private publishFrame(frame: VideoFrame): void {
		const controller = this.controller;
		if (controller == null) {
			throw new Error('Custom camera background video stream has no controller');
		}
		if (this.pullResolve != null) {
			const resolve = this.pullResolve;
			this.pullResolve = null;
			controller.enqueue(frame);
			resolve();
			return;
		}
		const replaced = this.pendingFrame;
		this.pendingFrame = frame;
		replaced?.close();
	}

	private readonly handlePlaybackError = (): void => {
		this.fail(videoElementError(this.video));
	};

	private readonly handleUnexpectedEnd = (): void => {
		this.fail(new Error('Custom camera background video playback ended unexpectedly'));
	};

	private fail(error: unknown): void {
		if (this.stopped) return;
		const failures = this.releaseOwnedResources(false);
		const reportedError =
			failures.length === 0
				? error
				: new AggregateError([error, ...failures], 'Custom camera background video failed during cleanup');
		this.controller?.error(reportedError);
	}

	private releaseOwnedResources(closeStream: boolean): Array<unknown> {
		if (this.stopped) return [];
		this.stopped = true;
		const failures: Array<unknown> = [];
		this.cancelFrameCallback(failures);
		this.closePendingFrame(failures);
		this.resolvePendingPull();
		this.releaseVideoElement(failures);
		if (closeStream) {
			try {
				this.controller?.close();
			} catch (error) {
				failures.push(error);
			}
		}
		return failures;
	}

	private cancelFrameCallback(failures: Array<unknown>): void {
		if (this.callbackHandle == null) return;
		try {
			this.video.cancelVideoFrameCallback(this.callbackHandle);
		} catch (error) {
			failures.push(error);
		}
		this.callbackHandle = null;
	}

	private closePendingFrame(failures: Array<unknown>): void {
		const pendingFrame = this.pendingFrame;
		this.pendingFrame = null;
		if (pendingFrame == null) return;
		try {
			pendingFrame.close();
		} catch (error) {
			failures.push(error);
		}
	}

	private resolvePendingPull(): void {
		const resolve = this.pullResolve;
		this.pullResolve = null;
		resolve?.();
	}

	private releaseVideoElement(failures: Array<unknown>): void {
		this.video.removeEventListener('error', this.handlePlaybackError);
		this.video.removeEventListener('ended', this.handleUnexpectedEnd);
		try {
			this.video.pause();
			this.video.removeAttribute('src');
			this.video.load();
		} catch (error) {
			failures.push(error);
		}
		try {
			URL.revokeObjectURL(this.objectURL);
		} catch (error) {
			failures.push(error);
		}
	}
}

function normalizedMediaType(value: string): string {
	return value.split(';', 1)[0]?.trim().toLowerCase() ?? '';
}

function bytesEqualAt(bytes: Uint8Array, offset: number, expected: ReadonlyArray<number>): boolean {
	if (bytes.byteLength < offset + expected.length) return false;
	for (let index = 0; index < expected.length; index += 1) {
		if (bytes[offset + index] !== expected[index]) return false;
	}
	return true;
}

function isEBMLDocTypeByte(byte: number): boolean {
	if (byte >= 0x30 && byte <= 0x39) return true;
	if (byte >= 0x41 && byte <= 0x5a) return true;
	if (byte >= 0x61 && byte <= 0x7a) return true;
	return byte === 0x2d || byte === 0x5f;
}

function readEBMLDocTypeValue(bytes: Uint8Array, start: number, length: number): string | null {
	let docType = '';
	for (let index = 0; index < length; index += 1) {
		const byte = bytes[start + index] ?? 0;
		if (byte === 0) break;
		if (!isEBMLDocTypeByte(byte)) return null;
		docType += String.fromCharCode(byte);
	}
	if (docType.length === 0) return null;
	return docType;
}

function readEBMLDocType(bytes: Uint8Array): string | null {
	const searchLimit = bytes.byteLength - EBML_DOC_TYPE_ELEMENT_ID.length;
	for (let offset = EBML_MAGIC.length; offset < searchLimit; offset += 1) {
		if (!bytesEqualAt(bytes, offset, EBML_DOC_TYPE_ELEMENT_ID)) continue;
		const sizeOffset = offset + EBML_DOC_TYPE_ELEMENT_ID.length;
		const sizeByte = bytes[sizeOffset] ?? 0;
		if ((sizeByte & EBML_VINT_SINGLE_BYTE_MARKER) === 0) continue;
		const length = sizeByte & EBML_VINT_SINGLE_BYTE_VALUE_MASK;
		if (length === 0 || length > MAX_EBML_DOC_TYPE_LENGTH) continue;
		const valueOffset = sizeOffset + 1;
		if (valueOffset + length > bytes.byteLength) return null;
		return readEBMLDocTypeValue(bytes, valueOffset, length);
	}
	return null;
}

function sniffVideoMediaType(bytes: Uint8Array): string | null {
	if (bytesEqualAt(bytes, MP4_FILE_TYPE_BOX_OFFSET, MP4_FILE_TYPE_BOX)) return 'video/mp4';
	if (!bytesEqualAt(bytes, 0, EBML_MAGIC)) return null;
	const docType = readEBMLDocType(bytes);
	if (docType == null) {
		throw new Error('Custom camera background video has an unreadable EBML document type');
	}
	if (docType !== WEBM_DOC_TYPE) {
		throw new Error(`Custom camera background video document type is unsupported: ${docType}`);
	}
	return 'video/webm';
}

async function normalizeCustomVideoBlob(blob: Blob, signal: AbortSignal): Promise<Blob> {
	const header = new Uint8Array(await blob.slice(0, VIDEO_HEADER_SNIFF_BYTES).arrayBuffer());
	throwIfAborted(signal);
	const detectedMediaType = sniffVideoMediaType(header);
	if (detectedMediaType == null) {
		throw new Error('Custom camera background is not a supported video format');
	}
	const declaredMediaType = normalizedMediaType(blob.type);
	if (SUPPORTED_VIDEO_MEDIA_TYPES.has(declaredMediaType) && declaredMediaType !== detectedMediaType) {
		throw new Error('Custom camera background video type does not match its encoded data');
	}
	if (declaredMediaType === detectedMediaType) return blob;
	return new Blob([blob], {type: detectedMediaType});
}

function videoElementError(video: HTMLVideoElement): Error {
	const code = video.error?.code ?? 0;
	const message = video.error?.message.trim() ?? '';
	if (message.length > 0) return new Error(`Custom camera background video failed (${code}): ${message}`);
	return new Error(`Custom camera background video failed with media error ${code}`);
}

function waitForVideoMetadata(video: HTMLVideoElement, signal: AbortSignal): Promise<void> {
	if (signal.aborted) return Promise.reject(signal.reason);
	if (video.readyState >= 1 && video.videoWidth > 0 && video.videoHeight > 0) return Promise.resolve();
	return new Promise<void>((resolve, reject) => {
		const cleanup = (): void => {
			video.removeEventListener('loadedmetadata', handleMetadata);
			video.removeEventListener('error', handleError);
			signal.removeEventListener('abort', handleAbort);
		};
		const handleMetadata = (): void => {
			cleanup();
			resolve();
		};
		const handleError = (): void => {
			cleanup();
			reject(videoElementError(video));
		};
		const handleAbort = (): void => {
			cleanup();
			reject(signal.reason);
		};
		video.addEventListener('loadedmetadata', handleMetadata, {once: true});
		video.addEventListener('error', handleError, {once: true});
		signal.addEventListener('abort', handleAbort, {once: true});
	});
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
	if (signal.aborted) throw signal.reason ?? new Error('Custom camera background video operation was aborted');
}

function throwCleanupFailures(failures: ReadonlyArray<unknown>, message: string): void {
	if (failures.length === 0) return;
	if (failures.length === 1) throw failures[0];
	throw new AggregateError(failures, message);
}

export async function createWebCameraEffectVideoFrameProducer(
	mediaURL: string,
): Promise<WebCameraEffectVideoFrameProducer> {
	requireWebCameraEffectCustomMediaURL(mediaURL);
	return runWithResponseDeadline({
		timeoutMilliseconds: WEB_CAMERA_EFFECT_CUSTOM_MEDIA_OPERATION_TIMEOUT_MS,
		description: 'Custom camera background video initialization',
		signal: null,
		operation: async (signal) => {
			const blob = await readWebCameraEffectCustomMediaBlob(mediaURL, signal);
			const normalizedBlob = await normalizeCustomVideoBlob(blob, signal);
			const producer = new WebCameraEffectVideoFrameProducerOwner(normalizedBlob);
			try {
				await producer.start(signal);
				return producer;
			} catch (error) {
				try {
					producer.stop();
				} catch (cleanupError) {
					throw new AggregateError(
						[error, cleanupError],
						'Custom camera background video initialization failed during cleanup',
					);
				}
				throw error;
			}
		},
	});
}
