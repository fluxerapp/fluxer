// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	drawVideoFrameToCanvas,
	type FluxerImageDecoderConstructor,
	type FluxerImageDecoderInstance,
	getImageDecoderConstructor,
} from '@app/features/platform/utils/ImageDecoderInterop';
import {useEffect, useRef, useState} from 'react';

interface UseAnimatedImageDecoderOptions {
	src: string | null;
	playing: boolean;
	canvas: HTMLCanvasElement | null;
	maxCachedFrames?: number;
}

export interface AnimatedImageDecoderState {
	supported: boolean;
	loaded: boolean;
	error: boolean;
	naturalWidth: number;
	naturalHeight: number;
}

interface DecoderSourceIdentity {
	src: string | null;
	canvas: HTMLCanvasElement | null;
	maxCachedFrames: number | undefined;
}

export interface AnimatedImageFrameAdvanceState {
	frameIndex: number;
	frameCount: number;
	repetitionCount: number;
	completedRepetitions: number;
}

export interface AnimatedImageFrameAdvanceResult {
	frameIndex: number;
	completedRepetitions: number;
}

const DEFAULT_FRAME_DURATION_MS = 100;
const DEFAULT_MAX_CACHED_FRAMES = 24;
const MAX_CACHED_FRAME_BYTES = 64 * 1024 * 1024;
const MAXIMUM_FRAME_TIMER_DELAY_MS = 2_147_000_000;
const MAX_ANIMATED_IMAGE_ENCODED_BYTES = 16 * 1024 * 1024;
const MAX_ANIMATED_IMAGE_RESPONSE_CHUNKS = 4096;
const ANIMATED_IMAGE_REQUEST_TIMEOUT_MS = 30_000;

function createIdleDecoderState(): AnimatedImageDecoderState {
	return {
		supported: getImageDecoderConstructor() !== null,
		loaded: false,
		error: false,
		naturalWidth: 0,
		naturalHeight: 0,
	};
}

function sameDecoderSourceIdentity(current: DecoderSourceIdentity, next: DecoderSourceIdentity): boolean {
	return current.src === next.src && current.canvas === next.canvas && current.maxCachedFrames === next.maxCachedFrames;
}

interface CachedAnimatedImageFrame {
	image: CanvasImageSource;
	width: number;
	height: number;
	byteSize: number;
	durationMs: number;
	close: () => void;
}

interface AnimatedImageResponseData {
	type: string;
	encodedBytes: Uint8Array;
}

const guessMimeFromUrl = (url: string): string => {
	const path = url.split('?')[0];
	const lower = path === undefined ? '' : path.toLowerCase();
	if (lower.endsWith('.webp')) return 'image/webp';
	if (lower.endsWith('.gif')) return 'image/gif';
	if (lower.endsWith('.apng') || lower.endsWith('.png')) return 'image/png';
	if (lower.endsWith('.avif')) return 'image/avif';
	return 'image/webp';
};

function resolveMimeFromResponse(contentType: string | null, source: string): string {
	if (contentType === null) return guessMimeFromUrl(source);
	const separator = contentType.indexOf(';');
	const mediaType = separator < 0 ? contentType : contentType.slice(0, separator);
	const normalized = mediaType.trim().toLowerCase();
	return normalized.length === 0 ? guessMimeFromUrl(source) : normalized;
}

function decodedFrameByteSize(width: number, height: number): number | null {
	if (!Number.isSafeInteger(width) || !Number.isSafeInteger(height)) return null;
	if (width <= 0 || height <= 0) return null;
	const byteSize = width * height * 4;
	if (!Number.isSafeInteger(byteSize)) return null;
	return byteSize;
}

function frameDurationMs(durationMicroseconds: number | null | undefined): number {
	if (durationMicroseconds === null || durationMicroseconds === undefined) return DEFAULT_FRAME_DURATION_MS;
	if (!Number.isFinite(durationMicroseconds) || durationMicroseconds <= 0) return DEFAULT_FRAME_DURATION_MS;
	return Math.min(MAXIMUM_FRAME_TIMER_DELAY_MS, Math.max(16, durationMicroseconds / 1000));
}

function cancelResponseBody(response: Response): void {
	const body = response.body;
	if (body == null) return;
	try {
		void body.cancel().catch(() => {});
	} catch {}
}

function closeVideoFrame(image: VideoFrame): void {
	try {
		image.close();
	} catch {}
}

async function readBoundedResponseBytes(response: Response): Promise<Uint8Array> {
	const body = response.body;
	if (body == null) throw new Error('Animated image response did not provide a body');
	const contentLength = response.headers.get('content-length');
	if (contentLength !== null) {
		const maximumContentLengthDigits = Number.MAX_SAFE_INTEGER.toString().length;
		if (
			contentLength.length === 0 ||
			contentLength.length > maximumContentLengthDigits ||
			!/^[0-9]+$/.test(contentLength)
		) {
			cancelResponseBody(response);
			throw new Error('Animated image response has an invalid Content-Length header');
		}
		const declaredBytes = Number(contentLength);
		if (!Number.isSafeInteger(declaredBytes) || declaredBytes > MAX_ANIMATED_IMAGE_ENCODED_BYTES) {
			cancelResponseBody(response);
			throw new Error('Animated image response exceeded its byte limit');
		}
	}
	const reader = body.getReader();
	const chunks: Array<Uint8Array> = [];
	let totalBytes = 0;
	try {
		while (true) {
			const result = await reader.read();
			if (result.done) break;
			const chunk = result.value;
			if (chunk == null) continue;
			if (chunks.length >= MAX_ANIMATED_IMAGE_RESPONSE_CHUNKS) {
				throw new Error('Animated image response exceeded its chunk limit');
			}
			totalBytes += chunk.byteLength;
			if (totalBytes > MAX_ANIMATED_IMAGE_ENCODED_BYTES) {
				throw new Error('Animated image response exceeded its byte limit');
			}
			chunks.push(chunk);
		}
	} catch (error) {
		try {
			await reader.cancel();
		} catch {}
		throw error;
	} finally {
		reader.releaseLock();
	}
	const bytes = new Uint8Array(totalBytes);
	let offset = 0;
	for (const chunk of chunks) {
		bytes.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return bytes;
}

function normalizeRepetitionCount(repetitionCount: number): number {
	if (repetitionCount === Infinity) return Infinity;
	if (!Number.isFinite(repetitionCount)) return 0;
	return Math.max(0, Math.floor(repetitionCount));
}

async function createCachedAnimatedImageFrame(
	image: VideoFrame,
	byteSize: number,
	durationMs: number,
	ownerWindow: Window,
): Promise<CachedAnimatedImageFrame> {
	const width = image.displayWidth;
	const height = image.displayHeight;
	if (typeof ownerWindow.createImageBitmap === 'function') {
		try {
			const bitmap = await ownerWindow.createImageBitmap(image as unknown as ImageBitmapSource);
			closeVideoFrame(image);
			return {
				image: bitmap,
				width,
				height,
				byteSize,
				durationMs,
				close: () => bitmap.close(),
			};
		} catch {}
	}
	return {
		image: image as unknown as CanvasImageSource,
		width,
		height,
		byteSize,
		durationMs,
		close: () => image.close(),
	};
}

export function getNextAnimatedImageFrame({
	frameIndex,
	frameCount,
	repetitionCount,
	completedRepetitions,
}: AnimatedImageFrameAdvanceState): AnimatedImageFrameAdvanceResult | null {
	const normalizedFrameCount = Math.max(0, Math.floor(frameCount));
	if (normalizedFrameCount <= 1) return null;
	const currentFrameIndex = Math.max(0, Math.min(normalizedFrameCount - 1, Math.floor(frameIndex)));
	const normalizedCompletedRepetitions = Math.max(0, Math.floor(completedRepetitions));
	if (currentFrameIndex < normalizedFrameCount - 1) {
		return {frameIndex: currentFrameIndex + 1, completedRepetitions: normalizedCompletedRepetitions};
	}
	const normalizedRepetitionCount = normalizeRepetitionCount(repetitionCount);
	if (normalizedCompletedRepetitions >= normalizedRepetitionCount) return null;
	return {frameIndex: 0, completedRepetitions: normalizedCompletedRepetitions + 1};
}

export function useAnimatedImageDecoder({
	src,
	playing,
	canvas,
	maxCachedFrames = DEFAULT_MAX_CACHED_FRAMES,
}: UseAnimatedImageDecoderOptions): AnimatedImageDecoderState {
	const sourceIdentity: DecoderSourceIdentity = {src, canvas, maxCachedFrames};
	const [state, setState] = useState<AnimatedImageDecoderState>(createIdleDecoderState);
	const [stateIdentity, setStateIdentity] = useState<DecoderSourceIdentity>(sourceIdentity);
	const runnerRef = useRef<{
		kick: () => void;
		pause: () => void;
		cancelled: boolean;
	} | null>(null);
	const playingRef = useRef(playing);
	playingRef.current = playing;
	const stateIsCurrent = sameDecoderSourceIdentity(stateIdentity, sourceIdentity);
	const visibleState = stateIsCurrent ? state : createIdleDecoderState();
	useEffect(() => {
		setStateIdentity(sourceIdentity);
		const Ctor = getImageDecoderConstructor();
		setState(createIdleDecoderState);
		if (!Ctor || !src || !canvas) {
			if (!Ctor) setState((prev) => ({...prev, supported: false}));
			return;
		}
		const ownerWindow = canvas.ownerDocument.defaultView;
		if (ownerWindow == null) {
			setState((prev) => ({...prev, error: true, supported: false}));
			return;
		}
		const ctx = canvas.getContext('2d', {alpha: true});
		if (!ctx) {
			setState((prev) => ({...prev, error: true}));
			return;
		}
		const normalizedMaxCachedFrames = Number.isFinite(maxCachedFrames)
			? Math.max(2, Math.floor(maxCachedFrames))
			: DEFAULT_MAX_CACHED_FRAMES;
		const runner = {cancelled: false, kick: () => {}, pause: () => {}};
		runnerRef.current = runner;
		const fetchController = new ownerWindow.AbortController();
		let decoder: FluxerImageDecoderInstance | null = null;
		let frameCount = Number.POSITIVE_INFINITY;
		let repetitionCount = 0;
		let completedRepetitions = 0;
		let frameIndex = 0;
		let timer: number | null = null;
		let resolveTimer: (() => void) | null = null;
		let advancing = false;
		let cachedFrameBytes = 0;
		const frameCache = new Map<number, CachedAnimatedImageFrame>();
		const isCurrentRunner = () => !runner.cancelled && runnerRef.current === runner;
		const publishState = (
			nextState: AnimatedImageDecoderState | ((previous: AnimatedImageDecoderState) => AnimatedImageDecoderState),
		) => {
			if (!isCurrentRunner()) return;
			setState(nextState);
		};
		const closeFrameCache = () => {
			for (const frame of frameCache.values()) {
				try {
					frame.close();
				} catch {}
			}
			frameCache.clear();
			cachedFrameBytes = 0;
		};
		const closeDecoder = () => {
			if (decoder == null) return;
			try {
				decoder.close();
			} catch {}
			decoder = null;
		};
		const failRunner = () => {
			if (runner.cancelled) return;
			publishState((prev) => ({...prev, error: true}));
			runner.cancelled = true;
			try {
				fetchController.abort();
			} catch {}
			clearTimer();
			closeFrameCache();
			closeDecoder();
		};
		publishState((prev) => ({...prev, loaded: false, error: false, supported: true}));
		const clearTimer = () => {
			if (timer != null) {
				ownerWindow.clearTimeout(timer);
				timer = null;
			}
			const resolve = resolveTimer;
			resolveTimer = null;
			if (resolve != null) resolve();
		};
		const draw = (frame: CachedAnimatedImageFrame): boolean => {
			if (!isCurrentRunner()) return false;
			const w = frame.width;
			const h = frame.height;
			if (canvas.width !== w) canvas.width = w;
			if (canvas.height !== h) canvas.height = h;
			try {
				ctx.clearRect(0, 0, w, h);
				ctx.drawImage(frame.image, 0, 0, w, h);
				return true;
			} catch {
				return false;
			}
		};
		const getFrame = async (index: number): Promise<CachedAnimatedImageFrame | null> => {
			const cached = frameCache.get(index);
			if (cached) {
				frameCache.delete(index);
				frameCache.set(index, cached);
				return cached;
			}
			const activeDecoder = decoder;
			if (activeDecoder == null) return null;
			let decodedImage: VideoFrame | null = null;
			try {
				const result = await activeDecoder.decode({frameIndex: index, completeFramesOnly: true});
				decodedImage = result.image;
				if (!isCurrentRunner()) {
					closeVideoFrame(decodedImage);
					decodedImage = null;
					return null;
				}
				const byteSize = decodedFrameByteSize(decodedImage.displayWidth, decodedImage.displayHeight);
				if (byteSize == null || byteSize > MAX_CACHED_FRAME_BYTES) {
					closeVideoFrame(decodedImage);
					decodedImage = null;
					failRunner();
					return null;
				}
				const durationMs = frameDurationMs(decodedImage.duration);
				const entry = await createCachedAnimatedImageFrame(decodedImage, byteSize, durationMs, ownerWindow);
				decodedImage = null;
				if (!isCurrentRunner()) {
					entry.close();
					return null;
				}
				frameCache.set(index, entry);
				cachedFrameBytes += entry.byteSize;
				while (frameCache.size > normalizedMaxCachedFrames || cachedFrameBytes > MAX_CACHED_FRAME_BYTES) {
					const oldestIndex = frameCache.keys().next().value;
					if (oldestIndex === undefined) break;
					const oldest = frameCache.get(oldestIndex);
					frameCache.delete(oldestIndex);
					if (oldest != null) {
						cachedFrameBytes -= oldest.byteSize;
						try {
							oldest.close();
						} catch {}
					}
				}
				return entry;
			} catch {
				if (decodedImage != null) {
					closeVideoFrame(decodedImage);
				}
				failRunner();
				return null;
			}
		};
		const advance = async () => {
			if (advancing || runner.cancelled) return;
			advancing = true;
			try {
				while (!runner.cancelled && playingRef.current) {
					const next = getNextAnimatedImageFrame({
						frameIndex,
						frameCount,
						repetitionCount,
						completedRepetitions,
					});
					if (!next) return;
					const frame = await getFrame(next.frameIndex);
					if (!frame || runner.cancelled || !playingRef.current) return;
					frameIndex = next.frameIndex;
					completedRepetitions = next.completedRepetitions;
					if (!draw(frame)) {
						failRunner();
						return;
					}
					await new Promise<void>((resolve) => {
						clearTimer();
						resolveTimer = resolve;
						timer = ownerWindow.setTimeout(
							() => {
								timer = null;
								resolveTimer = null;
								resolve();
							},
							Math.max(16, frame.durationMs),
						);
					});
					if (runner.cancelled || !playingRef.current) return;
				}
			} finally {
				advancing = false;
			}
		};
		runner.kick = () => {
			if (runner.cancelled) return;
			void advance();
		};
		runner.pause = () => {
			clearTimer();
		};
		const loadAnimatedImage = async (): Promise<AnimatedImageResponseData | null> => {
			let response: Response | null = null;
			let timeout: number | null = null;
			const operation = (async (): Promise<AnimatedImageResponseData | null> => {
				const fetchedResponse = await ownerWindow.fetch(src, {
					cache: 'force-cache',
					credentials: 'omit',
					redirect: 'error',
					referrerPolicy: 'no-referrer',
					signal: fetchController.signal,
				});
				response = fetchedResponse;
				if (!fetchedResponse.ok || fetchedResponse.body == null) {
					cancelResponseBody(fetchedResponse);
					throw new Error('Animated image request failed');
				}
				const type = resolveMimeFromResponse(fetchedResponse.headers.get('content-type'), src);
				const isSupported = await Ctor.isTypeSupported(type).catch(() => false);
				if (!isCurrentRunner()) return null;
				if (!isSupported) {
					cancelResponseBody(fetchedResponse);
					return null;
				}
				const encodedBytes = await readBoundedResponseBytes(fetchedResponse);
				return {type, encodedBytes};
			})();
			const timeoutPromise = new Promise<never>((_resolve, reject) => {
				timeout = ownerWindow.setTimeout(() => {
					try {
						fetchController.abort();
					} catch {}
					if (response != null) cancelResponseBody(response);
					reject(new Error('Animated image request timed out'));
				}, ANIMATED_IMAGE_REQUEST_TIMEOUT_MS);
			});
			try {
				return await Promise.race([operation, timeoutPromise]);
			} finally {
				if (timeout != null) ownerWindow.clearTimeout(timeout);
			}
		};
		const start = async () => {
			try {
				const animatedImage = await loadAnimatedImage();
				if (!isCurrentRunner()) return;
				if (animatedImage == null) {
					publishState((prev) => ({...prev, supported: false}));
					runner.cancelled = true;
					return;
				}
				const {type, encodedBytes} = animatedImage;
				const decoderBuffer = new ArrayBuffer(encodedBytes.byteLength);
				new Uint8Array(decoderBuffer).set(encodedBytes);
				const decoderBody = new ownerWindow.Response(decoderBuffer).body;
				if (decoderBody == null) throw new Error('Animated image decoder body was unavailable');
				const createdDecoder = new Ctor({data: decoderBody, type, preferAnimation: true});
				decoder = createdDecoder;
				await createdDecoder.completed;
				if (!isCurrentRunner()) return;
				const activeDecoder = decoder;
				if (activeDecoder == null) throw new Error('Animated image decoder was closed before completion');
				const track = activeDecoder.tracks.selectedTrack;
				if (track == null || !Number.isSafeInteger(track.frameCount) || track.frameCount < 1) {
					throw new Error('Animated image decoder returned an invalid frame count');
				}
				frameCount = track.frameCount;
				if (track.repetitionCount === undefined) {
					repetitionCount = 0;
				} else if (
					track.repetitionCount !== Infinity &&
					(!Number.isSafeInteger(track.repetitionCount) || track.repetitionCount < 0)
				) {
					throw new Error('Animated image decoder returned an invalid repetition count');
				} else {
					repetitionCount = track.repetitionCount;
				}
				const first = await getFrame(0);
				if (!first || !isCurrentRunner()) return;
				if (!draw(first)) {
					failRunner();
					return;
				}
				publishState({
					supported: true,
					loaded: true,
					error: false,
					naturalWidth: first.width,
					naturalHeight: first.height,
				});
				if (playingRef.current && frameCount > 1) {
					void advance();
				}
			} catch {
				if (!isCurrentRunner()) return;
				failRunner();
			}
		};
		void start();
		return () => {
			runner.cancelled = true;
			try {
				fetchController.abort();
			} catch {}
			clearTimer();
			closeFrameCache();
			closeDecoder();
			if (runnerRef.current === runner) runnerRef.current = null;
		};
	}, [canvas, maxCachedFrames, src]);
	useEffect(() => {
		const runner = runnerRef.current;
		if (runner == null) return;
		if (playing) {
			runner.kick();
			return;
		}
		runner.pause();
	}, [playing]);
	return visibleState;
}

export interface DecodedImageFrames {
	frames: Array<ImageData>;
	delays: Array<number>;
	width: number;
	height: number;
	ready: boolean;
	error: Error | null;
}

interface UseDecodedImageFramesOptions {
	bytes: Uint8Array | null;
	mime: string | null;
}

const HEIC_MIMES = new Set(['image/heic', 'image/heif']);

export function useDecodedImageFrames({bytes, mime}: UseDecodedImageFramesOptions): DecodedImageFrames {
	const [state, setState] = useState<DecodedImageFrames>(() => ({
		frames: [],
		delays: [],
		width: 0,
		height: 0,
		ready: false,
		error: null,
	}));
	useEffect(() => {
		if (!bytes || !mime) return;
		let cancelled = false;
		(async () => {
			try {
				const result = await decodeAllFrames(bytes, mime);
				if (cancelled) return;
				setState({...result, ready: true, error: null});
			} catch (err) {
				if (cancelled) return;
				setState({
					frames: [],
					delays: [],
					width: 0,
					height: 0,
					ready: false,
					error: err instanceof Error ? err : new Error(String(err)),
				});
			}
		})();
		return () => {
			cancelled = true;
		};
	}, [bytes, mime]);
	return state;
}

async function decodeAllFrames(
	bytes: Uint8Array,
	mime: string,
): Promise<{
	frames: Array<ImageData>;
	delays: Array<number>;
	width: number;
	height: number;
}> {
	const Cls = getImageDecoderConstructor();
	const lower = mime.toLowerCase();
	if (HEIC_MIMES.has(lower)) {
		const frame = await decodeStaticViaImage(bytes, lower);
		return {frames: [frame.image], delays: [0], width: frame.width, height: frame.height};
	}
	if (Cls && (await Cls.isTypeSupported(lower).catch(() => false))) {
		return decodeAllFramesViaImageDecoder(bytes, lower, Cls);
	}
	const frame = await decodeStaticViaImage(bytes, lower);
	return {frames: [frame.image], delays: [0], width: frame.width, height: frame.height};
}

async function decodeAllFramesViaImageDecoder(
	bytes: Uint8Array,
	type: string,
	Cls: FluxerImageDecoderConstructor,
): Promise<{
	frames: Array<ImageData>;
	delays: Array<number>;
	width: number;
	height: number;
}> {
	const decoder = new Cls({data: bytes, type, preferAnimation: true});
	try {
		await decoder.completed;
		const track = decoder.tracks.selectedTrack;
		const trackFrameCount =
			track == null || !Number.isSafeInteger(track.frameCount) || track.frameCount < 1 ? 1 : track.frameCount;
		const count = Math.max(1, trackFrameCount);
		const frames: Array<ImageData> = [];
		const delays: Array<number> = [];
		let width = 0;
		let height = 0;
		for (let i = 0; i < count; i++) {
			const {image} = await decoder.decode({frameIndex: i, completeFramesOnly: true});
			try {
				const w = image.displayWidth;
				const h = image.displayHeight;
				width = w;
				height = h;
				const canvas = typeof OffscreenCanvas !== 'undefined' ? new OffscreenCanvas(w, h) : null;
				if (!canvas) throw new Error('OffscreenCanvas unavailable for ImageDecoder fallback');
				const ctx = canvas.getContext('2d');
				if (!ctx) throw new Error('failed to acquire 2d context');
				drawVideoFrameToCanvas(ctx, image);
				const data = ctx.getImageData(0, 0, w, h);
				frames.push(new ImageData(new Uint8ClampedArray(data.data), w, h));
				const duration = image.duration;
				delays.push(duration === null || duration === undefined ? 0 : duration / 1000);
			} finally {
				image.close();
			}
		}
		return {frames, delays, width, height};
	} finally {
		decoder.close();
	}
}

async function decodeStaticViaImage(
	bytes: Uint8Array,
	mime: string,
): Promise<{
	image: ImageData;
	width: number;
	height: number;
}> {
	if (typeof Image === 'undefined' || typeof URL === 'undefined') {
		throw new Error('static decode unavailable: no Image/URL globals');
	}
	const blob = new Blob([new Uint8Array(bytes)], {type: mime});
	const url = URL.createObjectURL(blob);
	try {
		const img = new Image();
		img.src = url;
		if (typeof img.decode === 'function') {
			await img.decode();
		} else {
			await new Promise<void>((resolve, reject) => {
				img.onload = () => resolve();
				img.onerror = () => reject(new Error(`failed to decode ${mime}`));
			});
		}
		const w = img.naturalWidth;
		const h = img.naturalHeight;
		if (w === 0 || h === 0) throw new Error(`failed to decode ${mime}: zero dimensions`);
		const canvas =
			typeof OffscreenCanvas !== 'undefined' ? new OffscreenCanvas(w, h) : document.createElement('canvas');
		if (canvas instanceof HTMLCanvasElement) {
			canvas.width = w;
			canvas.height = h;
		}
		const ctx = (canvas as OffscreenCanvas | HTMLCanvasElement).getContext('2d');
		if (!ctx) throw new Error('failed to acquire 2d context for static decode');
		(ctx as CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D).drawImage(img, 0, 0);
		const data = (ctx as CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D).getImageData(0, 0, w, h);
		return {image: new ImageData(new Uint8ClampedArray(data.data), w, h), width: w, height: h};
	} finally {
		URL.revokeObjectURL(url);
	}
}
