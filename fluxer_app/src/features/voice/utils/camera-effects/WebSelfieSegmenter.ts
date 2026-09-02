// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveWorkerAssetUrl} from '@app/features/platform/utils/WorkerAssetUrl';
import {
	collectSettledFailures,
	throwCollectedFailures,
} from '@app/features/voice/utils/camera-effects/AggregateOperations';
import {
	cancelResponseBodyAndThrow,
	readBoundedResponseArrayBuffer,
	runWithResponseDeadline,
} from '@app/features/voice/utils/camera-effects/BoundedResponse';
import {
	shapeWebCameraEffectMaskAlpha,
	WEB_CAMERA_EFFECT_MASK_CORE_GROW_MIN,
	WEB_CAMERA_EFFECT_MASK_CORE_MIN,
	WEB_CAMERA_EFFECT_MASK_HOLE_NEIGHBOUR_MIN,
	WEB_CAMERA_EFFECT_MASK_SPECKLE_NEIGHBOUR_MAX,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_KEEP_STILL,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_HIGH,
	WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW,
	WEB_CAMERA_EFFECT_MASK_VOID_MAX,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectMask';
import ortWasmUrl from 'onnxruntime-web/ort-wasm-simd-threaded.asyncify.wasm';
import type * as OrtNamespace from 'onnxruntime-web/webgpu';
import modelAssetUrl from './models/selfie_segmentation_256x256.onnx';

export class MissingSegmentationAlphasOutputError extends Error {
	constructor() {
		super('Segmentation model produced no alphas output');
		this.name = 'MissingSegmentationAlphasOutputError';
	}
}

export const SEG_INPUT_EDGE = 256;
export const SEG_INPUT_NAME = 'pixel_values';
export const SEG_OUTPUT_NAME = 'alphas';
export const SEG_INPUT_PIXELS = SEG_INPUT_EDGE * SEG_INPUT_EDGE;

const MODEL_RESPONSE_MAX_BYTES = 4 * 1024 * 1024;
const MODEL_RESPONSE_MAX_CHUNKS = 2048;
const MODEL_REQUEST_TIMEOUT_MS = 30_000;

export type WebSelfieOrtModule = typeof OrtNamespace;

export interface WebSelfieRuntime {
	readonly ort: WebSelfieOrtModule;
	readonly modelBytes: Uint8Array;
}

class WebSelfieRuntimeAssetOwner {
	private ortModule: Promise<WebSelfieOrtModule> | null = null;
	private modelBytes: Promise<Uint8Array> | null = null;

	loadOrt(): Promise<WebSelfieOrtModule> {
		if (this.ortModule == null) {
			const loading = import('onnxruntime-web/webgpu').then((ort) => this.configureOrt(ort));
			const cached = loading.catch((error) => {
				this.clearFailedOrtLoad(cached);
				throw error;
			});
			this.ortModule = cached;
		}
		return this.ortModule;
	}

	private configureOrt(ort: WebSelfieOrtModule): WebSelfieOrtModule {
		ort.env.wasm.wasmPaths = {wasm: resolveWorkerAssetUrl(ortWasmUrl)};
		ort.env.wasm.proxy = false;
		ort.env.wasm.numThreads = resolveOrtThreadCount();
		ort.env.logLevel = 'error';
		return ort;
	}

	private clearFailedOrtLoad(cached: Promise<WebSelfieOrtModule>): void {
		if (this.ortModule === cached) {
			this.ortModule = null;
		}
	}

	loadModel(): Promise<Uint8Array> {
		if (this.modelBytes == null) {
			const loading = runWithResponseDeadline({
				timeoutMilliseconds: MODEL_REQUEST_TIMEOUT_MS,
				description: 'Web selfie segmentation model request',
				signal: null,
				operation: async (signal) => {
					let response: Response;
					try {
						response = await fetch(resolveWorkerAssetUrl(modelAssetUrl), {
							credentials: 'omit',
							redirect: 'error',
							referrerPolicy: 'no-referrer',
							signal,
						});
					} catch {
						if (signal.aborted) throw signal.reason;
						throw new Error('Web selfie segmentation model request failed');
					}
					if (!response.ok) {
						await cancelResponseBodyAndThrow({
							response,
							error: new Error(`Segmentation model request failed with status ${response.status}`),
							description: 'Web selfie segmentation model response',
						});
					}
					return readBoundedResponseArrayBuffer({
						response,
						maximumBytes: MODEL_RESPONSE_MAX_BYTES,
						maximumChunks: MODEL_RESPONSE_MAX_CHUNKS,
						description: 'Web selfie segmentation model response',
					});
				},
			}).then((bytes) => new Uint8Array(bytes));
			const cached = loading.catch((error) => {
				this.clearFailedModelLoad(cached);
				throw error;
			});
			this.modelBytes = cached;
		}
		return this.modelBytes;
	}

	private clearFailedModelLoad(cached: Promise<Uint8Array>): void {
		if (this.modelBytes === cached) {
			this.modelBytes = null;
		}
	}
}

const webSelfieRuntimeAssetOwner = new WebSelfieRuntimeAssetOwner();

async function loadWebSelfieOrt(): Promise<WebSelfieOrtModule> {
	return webSelfieRuntimeAssetOwner.loadOrt();
}

async function loadWebSelfieModel(): Promise<Uint8Array> {
	return webSelfieRuntimeAssetOwner.loadModel();
}

function resolveOrtThreadCount(): number {
	if (!globalThis.crossOriginIsolated) {
		return 1;
	}
	if (!('navigator' in globalThis)) {
		return 1;
	}
	const hardwareConcurrency = navigator.hardwareConcurrency;
	if (!Number.isSafeInteger(hardwareConcurrency)) {
		return 1;
	}
	if (hardwareConcurrency < 1) {
		return 1;
	}
	return Math.min(4, hardwareConcurrency);
}

export async function loadWebSelfieRuntime(): Promise<WebSelfieRuntime> {
	const [ortOutcome, modelBytesOutcome] = await Promise.allSettled([loadWebSelfieOrt(), loadWebSelfieModel()]);
	const failures: Array<unknown> = [];
	if (ortOutcome.status === 'rejected') failures.push(ortOutcome.reason);
	if (modelBytesOutcome.status === 'rejected') failures.push(modelBytesOutcome.reason);
	throwCollectedFailures({failures, message: 'Web selfie runtime loading failed'});
	if (ortOutcome.status !== 'fulfilled' || modelBytesOutcome.status !== 'fulfilled') {
		throw new Error('Web selfie runtime loading produced no result');
	}
	return {ort: ortOutcome.value, modelBytes: modelBytesOutcome.value};
}

function requireFloatMask(output: OrtNamespace.Tensor): Float32Array {
	const data = output.data;
	if (!(data instanceof Float32Array) || data.length !== SEG_INPUT_PIXELS) {
		throw new Error('Segmentation model produced an unexpected CPU output');
	}
	return data;
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

export class WebSelfieSegmenter {
	private readonly session: OrtNamespace.InferenceSession;
	private readonly inputChw = new Float32Array(3 * SEG_INPUT_PIXELS);
	private readonly inputTensor: OrtNamespace.Tensor;
	private readonly previous = new Float32Array(SEG_INPUT_PIXELS);
	private primed = false;
	private disposed = false;

	private constructor(ort: WebSelfieOrtModule, session: OrtNamespace.InferenceSession) {
		this.session = session;
		this.inputTensor = new ort.Tensor('float32', this.inputChw, [1, 3, SEG_INPUT_EDGE, SEG_INPUT_EDGE]);
	}

	static async create(): Promise<WebSelfieSegmenter> {
		const {ort, modelBytes} = await loadWebSelfieRuntime();
		const session = await ort.InferenceSession.create(modelBytes, {
			executionProviders: ['wasm'],
			graphOptimizationLevel: 'all',
			logSeverityLevel: 3,
		});
		const segmenter = new WebSelfieSegmenter(ort, session);
		try {
			await segmenter.warmup();
			return segmenter;
		} catch (error) {
			const cleanupFailures = await collectSettledFailures([segmenter.dispose()]);
			throwCollectedFailures({
				failures: [error, ...cleanupFailures],
				message: 'Web selfie segmenter initialization failed',
			});
		}
	}

	private async warmup(): Promise<void> {
		const outputs = await this.session.run({[SEG_INPUT_NAME]: this.inputTensor});
		const failures: Array<unknown> = [];
		try {
			const output = outputs[SEG_OUTPUT_NAME];
			if (output == null) {
				throw new MissingSegmentationAlphasOutputError();
			}
			requireFloatMask(output);
		} catch (error) {
			failures.push(error);
		}
		failures.push(...collectInferenceOutputDisposalFailures(outputs));
		throwCollectedFailures({failures, message: 'Web selfie segmenter warm-up failed'});
	}

	private writeShapedMask(alphas: Float32Array, maskRGBA: Uint8ClampedArray): void {
		const lastTexel = SEG_INPUT_EDGE - 1;
		for (let y = 0; y < SEG_INPUT_EDGE; y += 1) {
			for (let x = 0; x < SEG_INPUT_EDGE; x += 1) {
				const index = y * SEG_INPUT_EDGE + x;
				const centre = Math.max(0, Math.min(1, alphas[index]));
				let neighbourhoodSum = 0;
				let maxNeighbour = 0;
				for (let offsetY = -1; offsetY <= 1; offsetY += 1) {
					const sampleY = Math.max(0, Math.min(lastTexel, y + offsetY));
					for (let offsetX = -1; offsetX <= 1; offsetX += 1) {
						const sampleX = Math.max(0, Math.min(lastTexel, x + offsetX));
						const sample = Math.max(0, Math.min(1, alphas[sampleY * SEG_INPUT_EDGE + sampleX]));
						neighbourhoodSum += sample;
						if (offsetX !== 0 || offsetY !== 0) {
							maxNeighbour = Math.max(maxNeighbour, sample);
						}
					}
				}
				const neighbourhoodMean = neighbourhoodSum / 9;
				let clean = centre;
				if (
					centre >= WEB_CAMERA_EFFECT_MASK_CORE_MIN &&
					neighbourhoodMean < WEB_CAMERA_EFFECT_MASK_SPECKLE_NEIGHBOUR_MAX
				) {
					clean = 0;
				}
				if (
					centre <= WEB_CAMERA_EFFECT_MASK_VOID_MAX &&
					neighbourhoodMean > WEB_CAMERA_EFFECT_MASK_HOLE_NEIGHBOUR_MIN
				) {
					clean = 1;
				}
				if (centre >= WEB_CAMERA_EFFECT_MASK_CORE_GROW_MIN && maxNeighbour >= WEB_CAMERA_EFFECT_MASK_CORE_MIN) {
					clean = 1;
				}
				const shaped = shapeWebCameraEffectMaskAlpha(clean);
				const previous = this.previous[index];
				const delta = Math.abs(shaped - previous);
				const rawMotion = Math.max(
					0,
					Math.min(
						1,
						(delta - WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW) /
							(WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_HIGH - WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW),
					),
				);
				const motion = rawMotion * rawMotion * (3 - 2 * rawMotion);
				const keep = this.primed ? WEB_CAMERA_EFFECT_MASK_TEMPORAL_KEEP_STILL * (1 - motion) : 0;
				let next = keep * previous + (1 - keep) * shaped;
				if (clean >= WEB_CAMERA_EFFECT_MASK_CORE_MIN) {
					next = 1;
				}
				if (clean <= WEB_CAMERA_EFFECT_MASK_VOID_MAX) {
					next = 0;
				}
				this.previous[index] = next;
				const RGBAIndex = index * 4;
				maskRGBA[RGBAIndex] = 0;
				maskRGBA[RGBAIndex + 1] = 0;
				maskRGBA[RGBAIndex + 2] = 0;
				maskRGBA[RGBAIndex + 3] = Math.round(next * 255);
			}
		}
	}

	async segmentIntoMask(RGBA: Uint8ClampedArray, maskRGBA: Uint8ClampedArray): Promise<void> {
		if (this.disposed) {
			throw new Error('Cannot segment with a disposed web selfie segmenter');
		}
		if (RGBA.length !== SEG_INPUT_PIXELS * 4) {
			throw new Error('Segmentation input must be one 256 by 256 RGBA frame');
		}
		if (maskRGBA.length !== SEG_INPUT_PIXELS * 4) {
			throw new Error('Segmentation mask must hold RGBA for every model output sample');
		}
		const green = SEG_INPUT_PIXELS;
		const blue = SEG_INPUT_PIXELS * 2;
		for (let pixel = 0; pixel < SEG_INPUT_PIXELS; pixel += 1) {
			const source = pixel * 4;
			this.inputChw[pixel] = RGBA[source] / 255;
			this.inputChw[green + pixel] = RGBA[source + 1] / 255;
			this.inputChw[blue + pixel] = RGBA[source + 2] / 255;
		}
		const outputs = await this.session.run({[SEG_INPUT_NAME]: this.inputTensor});
		const failures: Array<unknown> = [];
		try {
			const output = outputs[SEG_OUTPUT_NAME];
			if (output == null) {
				throw new MissingSegmentationAlphasOutputError();
			}
			const alphas = requireFloatMask(output);
			this.writeShapedMask(alphas, maskRGBA);
			this.primed = true;
		} catch (error) {
			failures.push(error);
		}
		failures.push(...collectInferenceOutputDisposalFailures(outputs));
		throwCollectedFailures({failures, message: 'Web selfie segmentation failed'});
	}

	reset(): void {
		this.previous.fill(0);
		this.primed = false;
	}

	async dispose(): Promise<void> {
		if (this.disposed) {
			return;
		}
		this.disposed = true;
		const inputFailures = await collectSettledFailures([Promise.resolve().then(() => this.inputTensor.dispose())]);
		const sessionFailures = await collectSettledFailures([Promise.resolve().then(() => this.session.release())]);
		throwCollectedFailures({
			failures: [...inputFailures, ...sessionFailures],
			message: 'Web selfie segmenter teardown failed',
		});
	}
}
