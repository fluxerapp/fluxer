// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AnimatedImageFormat,
	CropAnimatedImageMessageType,
	type CropAnimatedImageStartMessage,
	type CropOutputFormat,
	type CropParams,
	type CropWorkerErrorCode,
	type CropWorkerImageFormat,
	type DecodedFrameInput,
	type ProcessBatchCompleteMessage,
	type ProcessBatchMessage,
} from '@app/features/expressions/workers/AnimatedImageCropMessages';
import {getNativeBridge, type NativeFrame, pickDecoderRouteFor} from '@app/features/messaging/utils/MediaNativeBridge';
import {
	drawVideoFrameToCanvas,
	type FluxerImageDecoderInstance,
	getImageDecoderConstructor,
} from '@app/features/platform/utils/ImageDecoderInterop';
import {
	cropRotateRgba,
	ensureLibfluxcoreReady,
	releaseLibfluxcoreMemoryIfIdle,
} from '@app/features/platform/utils/LibFluxcore';

interface WorkerRequest {
	resolve: (result: Uint8Array) => void;
	reject: (error: Error) => void;
	timeout: NodeJS.Timeout;
}

interface WorkerState {
	worker: Worker;
	busy: boolean;
	currentRequest: WorkerRequest | null;
}

interface BatchRequest {
	reject: (error: Error) => void;
	cleanup: () => void;
}

const PARALLEL_FRAME_THRESHOLD = 8;
const PARALLEL_PIXEL_THRESHOLD = 1_000_000;
const WORKER_IDLE_TIMEOUT_MS = 30_000;

export class CropPipelineError extends Error {
	readonly code: CropWorkerErrorCode;

	constructor(code: CropWorkerErrorCode, message: string) {
		super(message);
		this.code = code;
		this.name = 'CropPipelineError';
	}
}

export interface CropSource {
	bytes: Uint8Array;
	mime: string;
	format?: CropWorkerImageFormat;
}

export interface CropOptionsEx extends CropParams {
	outputFormat?: CropOutputFormat;
}

export class AnimatedImageCropWorkerManager {
	private static instance: AnimatedImageCropWorkerManager | null = null;
	private workers: Array<WorkerState> = [];
	private readonly pendingBatchRequests = new Set<BatchRequest>();
	private readonly maxWorkers: number;
	private terminated = false;
	private activeJobCount = 0;
	private idleTerminationTimeout: NodeJS.Timeout | null = null;
	private readonly workerTimeout: number = 30000;

	private constructor() {
		this.maxWorkers = Math.max(1, Math.min(4, (navigator.hardwareConcurrency || 4) - 1));
	}

	static getInstance(): AnimatedImageCropWorkerManager {
		if (!AnimatedImageCropWorkerManager.instance) {
			AnimatedImageCropWorkerManager.instance = new AnimatedImageCropWorkerManager();
		}
		return AnimatedImageCropWorkerManager.instance;
	}

	private ensureWorkersInitialized(): void {
		if (this.workers.length < this.maxWorkers && !this.terminated) {
			const workersToCreate = this.maxWorkers - this.workers.length;
			for (let i = 0; i < workersToCreate; i++) {
				const worker = new Worker(
					new URL(/* webpackChunkName: "animated-image-crop.worker" */ './AnimatedImageCropWorker.ts', import.meta.url),
					{
						type: 'module',
					},
				);
				const workerState: WorkerState = {
					worker,
					busy: false,
					currentRequest: null,
				};
				worker.addEventListener('message', (event: MessageEvent) => {
					this.handleWorkerMessage(workerState, event);
				});
				worker.addEventListener('error', (error) => {
					this.handleWorkerError(workerState, error);
				});
				this.workers.push(workerState);
			}
		}
	}

	private handleWorkerMessage(workerState: WorkerState, event: MessageEvent): void {
		const msg = event.data;
		const request = workerState.currentRequest;
		if (!request) {
			return;
		}
		if (msg.type === CropAnimatedImageMessageType.CROP_ANIMATED_IMAGE_COMPLETE) {
			clearTimeout(request.timeout);
			workerState.busy = false;
			workerState.currentRequest = null;
			if (msg.result) {
				request.resolve(msg.result);
			} else {
				request.reject(new CropPipelineError('internal', 'Empty result from worker'));
			}
		} else if (msg.type === CropAnimatedImageMessageType.CROP_ANIMATED_IMAGE_ERROR) {
			clearTimeout(request.timeout);
			workerState.busy = false;
			workerState.currentRequest = null;
			const code: CropWorkerErrorCode = msg.code ?? 'internal';
			request.reject(new CropPipelineError(code, msg.error || 'Unknown error from worker'));
		}
	}

	private handleWorkerError(workerState: WorkerState, error: ErrorEvent): void {
		const request = workerState.currentRequest;
		if (request) {
			clearTimeout(request.timeout);
			workerState.currentRequest = null;
			request.reject(
				new CropPipelineError(
					'internal',
					`Worker error: ${error.message || 'unknown error (worker script may have failed to load)'}`,
				),
			);
		}
		workerState.busy = false;
		try {
			workerState.worker.terminate();
		} catch {}
		const index = this.workers.indexOf(workerState);
		if (index !== -1) {
			this.workers.splice(index, 1);
		}
	}

	async cropImage(imageBytes: Uint8Array, format: AnimatedImageFormat, options: CropParams): Promise<Uint8Array> {
		return this.cropImageEx({bytes: imageBytes, mime: mimeForAnimatedFormat(format), format}, options);
	}

	async cropImageEx(source: CropSource, options: CropOptionsEx): Promise<Uint8Array> {
		if (this.terminated) {
			throw new CropPipelineError('internal', 'Worker manager has been terminated');
		}
		const mime = source.mime.toLowerCase();
		if (mime === 'application/json' || mime === 'application/lottie+json') {
			throw new CropPipelineError('lottie_unsupported', "Lottie can't be cropped. Upload a square JSON instead.");
		}
		const route = pickDecoderRouteFor(mime, getNativeBridge() !== null);
		const wasmFormat = inferWasmFormat(mime, source.format);
		if (route === 'unsupported') {
			if (mime === 'image/heic' || mime === 'image/heif') {
				throw new CropPipelineError(
					'heic_unsupported_in_browser',
					'HEIC decode requires the desktop app or a Safari browser',
				);
			}
			throw new CropPipelineError('unsupported_mime', `unsupported MIME type: ${mime}`);
		}
		if (route === 'libfluxcore' && wasmFormat) {
			return this.runLibfluxcoreWorker(source.bytes, wasmFormat, options, options.outputFormat);
		}
		if (route !== 'native' && route !== 'browser') {
			throw new CropPipelineError('unsupported_mime', `unhandled decoder route: ${route}`);
		}
		const frames = await this.decodeToFrames(source, route);
		return this.encodeCroppedFrames(frames, options);
	}

	private runLibfluxcoreWorker(
		imageBytes: Uint8Array,
		format: CropWorkerImageFormat,
		options: CropParams,
		outputFormat: CropOutputFormat | undefined,
	): Promise<Uint8Array> {
		if (this.terminated) {
			throw new CropPipelineError('internal', 'Worker manager has been terminated');
		}
		this.beginJob();
		return this.dispatchLibfluxcoreWorker(imageBytes, format, options, outputFormat).finally(() => {
			this.endJob();
		});
	}

	private dispatchLibfluxcoreWorker(
		imageBytes: Uint8Array,
		format: CropWorkerImageFormat,
		options: CropParams,
		outputFormat: CropOutputFormat | undefined,
	): Promise<Uint8Array> {
		if (this.terminated) {
			return Promise.reject(new CropPipelineError('internal', 'Worker manager has been terminated'));
		}
		this.ensureWorkersInitialized();
		const workerState = this.findAvailableWorker();
		if (!workerState) {
			return new Promise((resolve, reject) => {
				setTimeout(() => {
					this.dispatchLibfluxcoreWorker(imageBytes, format, options, outputFormat).then(resolve, reject);
				}, 50);
			});
		}
		return new Promise<Uint8Array>((resolve, reject) => {
			const timeout = setTimeout(() => {
				workerState.busy = false;
				workerState.currentRequest = null;
				reject(new CropPipelineError('internal', `Crop operation timed out after ${this.workerTimeout}ms`));
			}, this.workerTimeout);
			workerState.busy = true;
			workerState.currentRequest = {resolve, reject, timeout};
			const message: CropAnimatedImageStartMessage = {
				type: CropAnimatedImageMessageType.CROP_ANIMATED_IMAGE_START,
				imageBytes,
				format,
				...options,
				outputFormat,
			};
			const transferables: Array<Transferable> = [];
			if (imageBytes.buffer) {
				transferables.push(imageBytes.buffer);
			}
			workerState.worker.postMessage(message, transferables);
		});
	}

	private async decodeToFrames(source: CropSource, route: 'native' | 'browser'): Promise<Array<NativeFrame>> {
		if (route === 'native') {
			const bridge = getNativeBridge();
			if (!bridge) {
				throw new CropPipelineError('decode_failed', 'native bridge unavailable');
			}
			try {
				if (source.mime === 'image/heic' || source.mime === 'image/heif') {
					const f = bridge.decodeHeic(source.bytes);
					return [{rgba: f.rgba, width: f.width, height: f.height, delayMs: 0}];
				}
				if (source.mime === 'image/jxl') {
					const f = bridge.decodeJxl(source.bytes);
					return [{rgba: f.rgba, width: f.width, height: f.height, delayMs: 0}];
				}
				const decoded = bridge.decodeImage(source.bytes);
				return decoded.frames;
			} catch (err) {
				throw new CropPipelineError(
					'decode_failed',
					err instanceof Error ? err.message : `failed to decode ${source.mime}`,
				);
			}
		}
		return decodeViaImageDecoder(source);
	}

	private async encodeCroppedFrames(frames: Array<NativeFrame>, options: CropOptionsEx): Promise<Uint8Array> {
		if (frames.length === 0) {
			throw new CropPipelineError('decode_failed', 'no frames decoded');
		}
		let cropped: Array<NativeFrame>;
		try {
			await ensureLibfluxcoreReady();
			cropped = await this.cropDecodedFrames(frames, options);
		} catch (err) {
			throw new CropPipelineError('internal', err instanceof Error ? err.message : 'wasm RGBA crop failed');
		} finally {
			releaseLibfluxcoreMemoryIfIdle();
		}
		const isAnimated = frames.length > 1;
		const outputFormat: CropOutputFormat = options.outputFormat ?? (isAnimated ? 'animated_webp' : 'webp');
		const bridge = getNativeBridge();
		if (outputFormat === 'animated_webp' || outputFormat === 'apng') {
			if (!bridge) {
				throw new CropPipelineError('encode_failed', 'animated output requires the native bridge (desktop app)');
			}
			try {
				if (outputFormat === 'animated_webp') return bridge.encodeAnimatedWebp(cropped);
				return bridge.encodeAnimatedApng(cropped);
			} catch (err) {
				throw new CropPipelineError('encode_failed', err instanceof Error ? err.message : 'animated encode failed');
			}
		}
		const first = cropped[0];
		if (first === undefined) {
			throw new CropPipelineError('encode_failed', 'no cropped frame available');
		}
		if (outputFormat === 'avif') {
			if (!bridge) throw new CropPipelineError('encode_failed', 'AVIF encode requires the native bridge');
			try {
				return bridge.encodeAvif(first.rgba, first.width, first.height, true);
			} catch (err) {
				throw new CropPipelineError('encode_failed', err instanceof Error ? err.message : 'AVIF encode failed');
			}
		}
		return encodeViaCanvas(first, outputFormat);
	}

	private async cropDecodedFrames(frames: Array<NativeFrame>, options: CropOptionsEx): Promise<Array<NativeFrame>> {
		if (this.shouldCropFramesInWorkers(frames)) {
			return this.cropFramesInWorkerBatches(frames, options);
		}
		return frames.map((frame) => cropDecodedFrame(frame, options));
	}

	private shouldCropFramesInWorkers(frames: Array<NativeFrame>): boolean {
		if (frames.length < PARALLEL_FRAME_THRESHOLD || this.maxWorkers <= 1) return false;
		let pixels = 0;
		for (const frame of frames) pixels += frame.width * frame.height;
		return pixels >= PARALLEL_PIXEL_THRESHOLD;
	}

	private async cropFramesInWorkerBatches(
		frames: Array<NativeFrame>,
		options: CropOptionsEx,
	): Promise<Array<NativeFrame>> {
		this.beginJob();
		try {
			this.ensureWorkersInitialized();
			const availableWorkers = this.workers.filter((worker) => !worker.busy);
			if (availableWorkers.length <= 1) {
				return frames.map((frame) => cropDecodedFrame(frame, options));
			}
			const batches = createFrameBatches(frames, availableWorkers.length);
			const results = await Promise.all(
				batches.map((batch, index) =>
					this.processFrameBatchWithWorker(availableWorkers[index], index + 1, batch, options),
				),
			);
			const ordered: Array<NativeFrame | undefined> = new Array(frames.length);
			for (const batch of results) {
				for (const item of batch) ordered[item.index] = item.frame;
			}
			return ordered.map((frame, index) => {
				if (!frame) throw new CropPipelineError('internal', `Missing transformed frame ${index}`);
				return frame;
			});
		} finally {
			this.endJob();
		}
	}

	private processFrameBatchWithWorker(
		workerState: WorkerState,
		jobId: number,
		batch: Array<{frame: NativeFrame; index: number}>,
		options: CropOptionsEx,
	): Promise<Array<{frame: NativeFrame; index: number}>> {
		return new Promise((resolve, reject) => {
			let pendingBatchRequest: BatchRequest | null = null;
			const timeout = setTimeout(() => {
				cleanup();
				workerState.busy = false;
				reject(new CropPipelineError('internal', `Frame batch timed out after ${this.workerTimeout}ms`));
			}, this.workerTimeout);
			const cleanup = () => {
				clearTimeout(timeout);
				workerState.worker.removeEventListener('message', handleMessage);
				workerState.worker.removeEventListener('error', handleError);
				if (pendingBatchRequest) {
					this.pendingBatchRequests.delete(pendingBatchRequest);
					pendingBatchRequest = null;
				}
			};
			const handleMessage = (event: MessageEvent<ProcessBatchCompleteMessage>) => {
				const msg = event.data;
				if (msg.type !== CropAnimatedImageMessageType.BATCH_COMPLETE || msg.jobId !== jobId) return;
				cleanup();
				workerState.busy = false;
				const processed = msg.processedDecodedFrames;
				if (!processed) {
					reject(new CropPipelineError('internal', 'Frame batch returned no decoded frames'));
					return;
				}
				resolve(processed.map((frame, offset) => ({frame, index: batch[offset]?.index ?? offset})));
			};
			const handleError = (error: ErrorEvent) => {
				cleanup();
				workerState.busy = false;
				reject(
					new CropPipelineError(
						'internal',
						`Worker error: ${error.message || 'unknown error (frame batch worker failed)'}`,
					),
				);
			};
			workerState.busy = true;
			pendingBatchRequest = {reject, cleanup};
			this.pendingBatchRequests.add(pendingBatchRequest);
			workerState.worker.addEventListener('message', handleMessage);
			workerState.worker.addEventListener('error', handleError);
			const message: ProcessBatchMessage = {
				type: CropAnimatedImageMessageType.PROCESS_BATCH,
				jobId,
				cropParams: options,
				batch: {
					startIndex: 0,
					operation: 'transform_rgba',
					frames: batch.map(({frame}) => ({
						data: frame.rgba,
						width: frame.width,
						height: frame.height,
						delay: frame.delayMs,
						delayMs: frame.delayMs,
						disposeOp: 0,
					})),
				},
			};
			const transferables = batch.map(({frame}) => frame.rgba.buffer);
			workerState.worker.postMessage(message, transferables);
		});
	}

	private findAvailableWorker(): WorkerState | null {
		for (const workerState of this.workers) {
			if (!workerState.busy) {
				return workerState;
			}
		}
		return null;
	}

	private beginJob(): void {
		this.clearIdleTermination();
		this.activeJobCount += 1;
	}

	private endJob(): void {
		this.activeJobCount = Math.max(0, this.activeJobCount - 1);
		this.scheduleIdleTermination();
	}

	private isPoolIdle(): boolean {
		return (
			this.activeJobCount === 0 &&
			this.pendingBatchRequests.size === 0 &&
			this.workers.every((workerState) => !workerState.busy && workerState.currentRequest === null)
		);
	}

	private clearIdleTermination(): void {
		if (this.idleTerminationTimeout === null) {
			return;
		}
		clearTimeout(this.idleTerminationTimeout);
		this.idleTerminationTimeout = null;
	}

	private scheduleIdleTermination(): void {
		this.clearIdleTermination();
		if (this.terminated || this.workers.length === 0 || !this.isPoolIdle()) {
			return;
		}
		this.idleTerminationTimeout = setTimeout(() => {
			this.idleTerminationTimeout = null;
			if (this.isPoolIdle()) {
				this.terminateIdleWorkers();
			}
		}, WORKER_IDLE_TIMEOUT_MS);
	}

	private terminateIdleWorkers(): void {
		for (const workerState of this.workers) {
			try {
				workerState.worker.terminate();
			} catch {}
		}
		this.workers = [];
	}

	getActiveWorkerCount(): number {
		return this.workers.filter((w) => w.busy).length;
	}

	getTotalWorkerCount(): number {
		return this.workers.length;
	}

	terminate(): void {
		if (this.terminated) {
			return;
		}
		this.terminated = true;
		this.clearIdleTermination();
		for (const workerState of this.workers) {
			if (workerState.currentRequest) {
				clearTimeout(workerState.currentRequest.timeout);
				workerState.currentRequest.reject(new CropPipelineError('internal', 'Worker manager terminated'));
			}
			workerState.worker.terminate();
		}
		const error = new CropPipelineError('internal', 'Worker manager terminated');
		for (const request of Array.from(this.pendingBatchRequests)) {
			request.cleanup();
			request.reject(error);
		}
		this.pendingBatchRequests.clear();
		this.workers = [];
		AnimatedImageCropWorkerManager.instance = null;
	}

	restart(): void {
		if (!this.terminated) {
			return;
		}
		this.terminated = false;
		this.ensureWorkersInitialized();
		this.scheduleIdleTermination();
	}
}

function inferWasmFormat(mime: string, hint: CropWorkerImageFormat | undefined): CropWorkerImageFormat | null {
	if (hint) return hint;
	switch (mime) {
		case 'image/png':
			return 'png';
		case 'image/apng':
			return 'apng';
		case 'image/jpeg':
			return 'jpeg';
		case 'image/gif':
			return 'gif';
		case 'image/webp':
			return 'webp';
		case 'image/avif':
			return 'avif';
		default:
			return null;
	}
}

function mimeForAnimatedFormat(format: AnimatedImageFormat): string {
	switch (format) {
		case 'apng':
			return 'image/apng';
		case 'gif':
			return 'image/gif';
		case 'webp':
			return 'image/webp';
		case 'avif':
			return 'image/avif';
	}
}

function cropDecodedFrame(frame: NativeFrame, options: CropOptionsEx): NativeFrame {
	const transformed = cropRotateRgba(
		frame.rgba,
		frame.width,
		frame.height,
		options.x,
		options.y,
		options.width,
		options.height,
		options.imageRotation ?? 0,
		options.resizeWidth ?? null,
		options.resizeHeight ?? null,
	);
	return {
		rgba: transformed.rgba,
		width: transformed.width,
		height: transformed.height,
		delayMs: frame.delayMs,
	};
}

function createFrameBatches(
	frames: Array<NativeFrame>,
	workerCount: number,
): Array<Array<{frame: DecodedFrameInput; index: number}>> {
	const batches: Array<Array<{frame: DecodedFrameInput; index: number}>> = Array.from({length: workerCount}, () => []);
	for (let index = 0; index < frames.length; index += 1) {
		batches[index % workerCount]?.push({frame: frames[index], index});
	}
	return batches.filter((batch) => batch.length > 0);
}

async function encodeViaCanvas(frame: NativeFrame, format: 'webp' | 'png' | 'jpeg' | 'gif'): Promise<Uint8Array> {
	if (typeof OffscreenCanvas === 'undefined') {
		throw new CropPipelineError('encode_failed', 'OffscreenCanvas required for static encode fallback');
	}
	const canvas = new OffscreenCanvas(frame.width, frame.height);
	const ctx = canvas.getContext('2d');
	if (!ctx) throw new CropPipelineError('encode_failed', 'no 2d context');
	const imageData = new ImageData(new Uint8ClampedArray(frame.rgba), frame.width, frame.height);
	ctx.putImageData(imageData, 0, 0);
	const mime = format === 'jpeg' ? 'image/jpeg' : format === 'gif' ? 'image/gif' : `image/${format}`;
	let blob: Blob;
	try {
		blob = await canvas.convertToBlob({type: mime, quality: 0.92});
	} catch (err) {
		throw new CropPipelineError('encode_failed', err instanceof Error ? err.message : 'canvas encode failed');
	}
	const buffer = await blob.arrayBuffer();
	return new Uint8Array(buffer);
}

async function decodeViaImageDecoder(source: CropSource): Promise<Array<NativeFrame>> {
	const Cls = getImageDecoderConstructor();
	if (!Cls) {
		throw new CropPipelineError('decode_failed', 'ImageDecoder unavailable in this browser');
	}
	let decoder: FluxerImageDecoderInstance | null = null;
	try {
		decoder = new Cls({data: source.bytes, type: source.mime, preferAnimation: true});
		await decoder.completed;
		const track = decoder.tracks.selectedTrack;
		const frameCount = Math.max(1, track?.frameCount ?? 1);
		const frames: Array<NativeFrame> = [];
		for (let i = 0; i < frameCount; i++) {
			const {image} = await decoder.decode({frameIndex: i, completeFramesOnly: true});
			try {
				const w = image.displayWidth;
				const h = image.displayHeight;
				if (typeof OffscreenCanvas === 'undefined') {
					throw new CropPipelineError('decode_failed', 'OffscreenCanvas required for ImageDecoder fallback');
				}
				const canvas = new OffscreenCanvas(w, h);
				const ctx = canvas.getContext('2d');
				if (!ctx) throw new CropPipelineError('decode_failed', 'no 2d context');
				drawVideoFrameToCanvas(ctx, image);
				const data = ctx.getImageData(0, 0, w, h);
				const delayMs = (image.duration ?? 0) / 1000;
				frames.push({rgba: new Uint8Array(data.data.buffer.slice(0)), width: w, height: h, delayMs});
			} finally {
				image.close();
			}
		}
		return frames;
	} catch (err) {
		throw new CropPipelineError(
			'decode_failed',
			err instanceof Error ? err.message : `failed to decode ${source.mime}`,
		);
	} finally {
		decoder?.close();
	}
}

export async function cropAnimatedImageWithWorkerPool(
	imageBytes: Uint8Array,
	format: AnimatedImageFormat,
	options: CropParams,
): Promise<Uint8Array> {
	const manager = AnimatedImageCropWorkerManager.getInstance();
	return manager.cropImage(imageBytes, format, options);
}

export async function cropImageWithSource(source: CropSource, options: CropOptionsEx): Promise<Uint8Array> {
	const manager = AnimatedImageCropWorkerManager.getInstance();
	return manager.cropImageEx(source, options);
}

export function getWorkerManager(): AnimatedImageCropWorkerManager {
	return AnimatedImageCropWorkerManager.getInstance();
}
