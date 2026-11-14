// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assertOutputBudget,
	assertSourceBudget,
	chooseMethod,
	type EncodeRung,
	isNoopCrop,
	LOSSLESS_RUNG,
	lossyRung,
	nextRung,
	usesLosslessFirstRung,
} from '@app/features/expressions/workers/AnimatedCropPlan';
import {
	type FrameSource,
	openApngFrameSource,
	openAvifFrameSource,
	openGifFrameSink,
	openGifFrameSource,
} from '@app/features/expressions/workers/AnimatedFrameCodecs';
import {
	type AnimatedCropOutputMime,
	type CropErrorCode,
	CropPipelineError,
	type CropProgressPhase,
	type CropStats,
	type CropWorkerRequest,
	type CropWorkerResponse,
} from '@app/features/expressions/workers/AnimatedImageCropMessages';
import {cropRotateRgbaInto, ensureLibfluxcoreReady} from '@app/features/platform/workers/LibFluxcoreWorker';
import {
	ensureLibWebpReady,
	openWebpFrameSink,
	openWebpFrameSource,
	probeWebp,
} from '@app/features/platform/workers/LibWebpWorker';

declare const self: DedicatedWorkerGlobalScope;

const PROGRESS_INTERVAL_MS = 50;

interface OpenedSource {
	source: FrameSource;
	lossless: boolean;
}

interface FrameSinkLike {
	add(rgba: Uint8Array, delayMs: number): unknown;
	finish(): Uint8Array;
	close(): void;
}

interface CropResult {
	bytes: Uint8Array;
	mime: AnimatedCropOutputMime;
}

class CropJob {
	readonly stats: CropStats = {
		variant: 'none',
		passes: 0,
		method: 0,
		quality: 0,
		lossless: true,
		framesDecoded: 0,
		decodeMs: 0,
		encodeMs: 0,
	};
	private readonly bytes: Uint8Array;
	private lastProgressAt = 0;
	private cropStart = 0;
	private cropReached = 0;
	private reusableSource: OpenedSource | null = null;

	constructor(private readonly request: CropWorkerRequest) {
		this.bytes = new Uint8Array(request.bytes);
	}

	async run(): Promise<CropResult> {
		this.postProgress('init', 0, 1, true);
		await this.loadLibfluxcore();
		const {request} = this;
		const {crop} = request;
		const opened = await this.openSource();
		this.reusableSource = opened;
		try {
			const {source} = opened;
			assertOutputBudget(source.frameCount, crop.outputWidth, crop.outputHeight);
			if (isNoopCrop(crop, source.width, source.height, request.format, this.bytes.length, request.sizeLimitBytes)) {
				return {bytes: this.bytes, mime: request.format === 'gif' ? 'image/gif' : 'image/webp'};
			}
			const cap = request.sizeLimitBytes;
			const frameCount = source.frameCount;
			const plays = source.plays;
			let phase: CropProgressPhase = 'crop';
			if (request.format === 'gif') {
				this.setRung({quality: 0, method: 0, lossless: true});
				const gif = await this.runPass(
					this.openSink(() => openGifFrameSink(crop.outputWidth, crop.outputHeight, plays, cap)),
					'crop',
				);
				if (gif && gif.length <= cap) return {bytes: gif, mime: 'image/gif'};
				if (gif) phase = 'compress';
				this.cropStart = this.cropReached;
			}
			await this.loadLibWebp();
			const workPixels = frameCount * crop.outputWidth * crop.outputHeight;
			const method = chooseMethod(workPixels);
			let rungIndex = usesLosslessFirstRung(opened.lossless, workPixels) ? -1 : 0;
			for (;;) {
				const rung = rungIndex < 0 ? LOSSLESS_RUNG : lossyRung(rungIndex, method);
				this.setRung(rung);
				const webp = await this.runPass(
					this.openSink(() => openWebpFrameSink(crop.outputWidth, crop.outputHeight, plays, rung)),
					phase,
				);
				phase = 'compress';
				if (!webp) throw new CropPipelineError('encode_failed', 'WebP encoder rejected a frame');
				if (webp.length <= cap) return {bytes: webp, mime: 'image/webp'};
				if (rungIndex < 0) {
					rungIndex = 0;
					continue;
				}
				const next = nextRung(rungIndex, webp.length, cap);
				if (next === 'too_large') {
					throw new CropPipelineError('too_large', `Output is ${webp.length} bytes, over the ${cap} byte limit`);
				}
				rungIndex = next;
			}
		} finally {
			this.reusableSource?.source.close();
			this.reusableSource = null;
		}
	}

	private async loadLibfluxcore(): Promise<void> {
		if (typeof WebAssembly === 'undefined') {
			throw new CropPipelineError('wasm_unavailable', 'WebAssembly is not available');
		}
		try {
			await ensureLibfluxcoreReady();
		} catch (error) {
			throw new CropPipelineError('wasm_unavailable', errorMessage(error));
		}
	}

	private async loadLibWebp(): Promise<void> {
		if (this.stats.variant !== 'none') return;
		try {
			this.stats.variant = await ensureLibWebpReady();
		} catch (error) {
			throw new CropPipelineError('wasm_unavailable', errorMessage(error));
		}
	}

	private async openSource(): Promise<OpenedSource> {
		const {format} = this.request;
		if (format === 'webp') await this.loadLibWebp();
		return this.decodeStage(async () => {
			switch (format) {
				case 'gif':
					return {source: openGifFrameSource(this.bytes), lossless: true};
				case 'png':
					return {source: openApngFrameSource(this.bytes), lossless: true};
				case 'avif':
					return {source: await openAvifFrameSource(this.bytes), lossless: false};
				case 'webp': {
					const probe = probeWebp(this.bytes);
					assertSourceBudget(probe.width, probe.height, probe.frameCount);
					return {source: openWebpFrameSource(this.bytes), lossless: probe.lossless};
				}
			}
		}).then((opened) => {
			assertSourceBudget(opened.source.width, opened.source.height, opened.source.frameCount);
			return opened;
		});
	}

	private async takeSource(): Promise<FrameSource> {
		const reusable = this.reusableSource;
		this.reusableSource = null;
		if (reusable) return reusable.source;
		return (await this.openSource()).source;
	}

	private openSink(open: () => FrameSinkLike): FrameSinkLike {
		try {
			return open();
		} catch (error) {
			throw asStageError('encode_failed', error);
		}
	}

	private async runPass(sink: FrameSinkLike, phase: CropProgressPhase): Promise<Uint8Array | null> {
		const {crop} = this.request;
		this.stats.passes++;
		let source: FrameSource | null = null;
		try {
			source = await this.takeSource();
			const total = source.frameCount;
			const cropped = new Uint8Array(crop.outputWidth * crop.outputHeight * 4);
			let done = 0;
			this.postProgress(phase, 0, total, true);
			for (;;) {
				const decodeStart = performance.now();
				const frameSource: FrameSource = source;
				const frame = await this.decodeStage(() => frameSource.next());
				if (!frame) break;
				try {
					cropRotateRgbaInto(
						frame.rgba,
						frameSource.width,
						frameSource.height,
						crop.x,
						crop.y,
						crop.width,
						crop.height,
						crop.rotation,
						cropped,
						crop.outputWidth,
						crop.outputHeight,
					);
				} catch (error) {
					throw asStageError('decode_failed', error);
				}
				this.stats.framesDecoded++;
				const encodeStart = performance.now();
				this.stats.decodeMs += encodeStart - decodeStart;
				let accepted: unknown;
				try {
					accepted = sink.add(cropped, frame.delayMs);
				} catch (error) {
					throw asStageError('encode_failed', error);
				}
				this.stats.encodeMs += performance.now() - encodeStart;
				if (accepted === false) return null;
				done++;
				if (this.postProgress(phase, done, total, false)) await yieldToEventLoop();
			}
			if (done === 0) throw new CropPipelineError('decode_failed', 'Animation has no frames');
			this.postProgress('compress', done, total, true);
			const finishStart = performance.now();
			try {
				return sink.finish();
			} catch (error) {
				throw asStageError('encode_failed', error);
			} finally {
				this.stats.encodeMs += performance.now() - finishStart;
			}
		} finally {
			sink.close();
			source?.close();
		}
	}

	private async decodeStage<T>(read: () => T | Promise<T>): Promise<T> {
		try {
			return await read();
		} catch (error) {
			throw asStageError('decode_failed', error);
		}
	}

	private setRung(rung: EncodeRung): void {
		this.stats.quality = rung.quality;
		this.stats.method = rung.method;
		this.stats.lossless = rung.lossless;
	}

	private postProgress(phase: CropProgressPhase, done: number, total: number, force: boolean): boolean {
		const now = performance.now();
		if (!force && done < total && now - this.lastProgressAt < PROGRESS_INTERVAL_MS) return false;
		this.lastProgressAt = now;
		if (phase === 'crop') {
			this.cropReached = this.cropStart + (1 - this.cropStart) * (total > 0 ? done / total : 0);
			post({type: 'progress', jobId: this.request.jobId, phase, done: this.cropReached * total, total});
			return true;
		}
		post({type: 'progress', jobId: this.request.jobId, phase, done, total});
		return true;
	}
}

function yieldToEventLoop(): Promise<void> {
	return new Promise((resolve) => {
		const channel = new MessageChannel();
		channel.port1.onmessage = () => {
			channel.port1.close();
			resolve();
		};
		channel.port2.postMessage(null);
	});
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function asStageError(code: CropErrorCode, error: unknown): CropPipelineError {
	return error instanceof CropPipelineError ? error : new CropPipelineError(code, errorMessage(error));
}

function post(message: CropWorkerResponse, transfer: Array<Transferable> = []): void {
	self.postMessage(message, transfer);
}

function toTransferableBuffer(bytes: Uint8Array): ArrayBuffer {
	if (bytes.buffer instanceof ArrayBuffer && bytes.byteOffset === 0 && bytes.byteLength === bytes.buffer.byteLength) {
		return bytes.buffer;
	}
	return bytes.slice().buffer;
}

async function handleCrop(request: CropWorkerRequest): Promise<void> {
	const job = new CropJob(request);
	try {
		const result = await job.run();
		const buffer = toTransferableBuffer(result.bytes);
		post({type: 'done', jobId: request.jobId, bytes: buffer, mime: result.mime, stats: job.stats}, [buffer]);
	} catch (error) {
		const failure = asStageError('internal', error);
		post({type: 'error', jobId: request.jobId, code: failure.code, message: failure.message});
	}
}

self.addEventListener('message', (event: MessageEvent<CropWorkerRequest>) => {
	if (event.data?.type !== 'crop') return;
	void handleCrop(event.data);
});
