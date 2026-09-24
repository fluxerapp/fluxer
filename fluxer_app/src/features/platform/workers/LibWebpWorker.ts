// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveWorkerAssetUrl} from '@app/features/platform/utils/WorkerAssetUrl';
import initLibWebp, {AnimDecoder, AnimEncoder, webp_probe} from '@pkgs/libfluxwebp/libfluxwebp';
import libWebpScalarWasmUrl from '@pkgs/libfluxwebp/libfluxwebp_bg.wasm';
import libWebpSimdWasmUrl from '@pkgs/libfluxwebp/libfluxwebp_simd_bg.wasm';

export type LibWebpVariant = 'simd' | 'scalar';

export interface WebpProbe {
	width: number;
	height: number;
	frameCount: number;
	plays: number;
	hasAlpha: boolean;
	lossless: boolean;
}

interface WebpFrame {
	rgba: Uint8Array;
	delayMs: number;
}

interface WebpFrameSource {
	width: number;
	height: number;
	frameCount: number;
	plays: number;
	next(): WebpFrame | null;
	close(): void;
}

interface WebpFrameSink {
	add(rgba: Uint8Array, delayMs: number): void;
	finish(): Uint8Array;
	close(): void;
}

interface WebpRung {
	quality: number;
	method: number;
	lossless: boolean;
}

const WEBP_MAX_FRAME_DURATION_MS = 0xffffff;
const SIMD_PROBE_MODULE = new Uint8Array([
	0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 96, 0, 1, 123, 3, 2, 1, 0, 10, 10, 1, 8, 0, 65, 0, 253, 15, 253, 98, 11,
]);

let readyPromise: Promise<LibWebpVariant> | null = null;
let wasmMemory: WebAssembly.Memory | null = null;

async function loadLibWebp(): Promise<LibWebpVariant> {
	const variant: LibWebpVariant = WebAssembly.validate(SIMD_PROBE_MODULE) ? 'simd' : 'scalar';
	const wasmUrl = resolveWorkerAssetUrl(variant === 'simd' ? libWebpSimdWasmUrl : libWebpScalarWasmUrl);
	const exports = await initLibWebp({module_or_path: wasmUrl});
	wasmMemory = exports.memory;
	return variant;
}

export function ensureLibWebpReady(): Promise<LibWebpVariant> {
	if (!readyPromise) {
		readyPromise = loadLibWebp().catch((error: unknown) => {
			readyPromise = null;
			wasmMemory = null;
			throw error;
		});
	}
	return readyPromise;
}

function requireMemory(): WebAssembly.Memory {
	if (!wasmMemory) {
		throw new Error('libwebp is not loaded. Call ensureLibWebpReady() first.');
	}
	return wasmMemory;
}

export function probeWebp(bytes: Uint8Array): WebpProbe {
	requireMemory();
	const [width, height, frameCount, plays, hasAlpha, lossless] = webp_probe(bytes);
	return {width, height, frameCount, plays, hasAlpha: hasAlpha === 1, lossless: lossless === 1};
}

export function openWebpFrameSource(bytes: Uint8Array): WebpFrameSource {
	const {width, height, frameCount, plays} = probeWebp(bytes);
	const decoder = new AnimDecoder(bytes);
	const frameBytes = width * height * 4;
	let previousEnd = 0;
	let closed = false;
	return {
		width,
		height,
		frameCount,
		plays,
		next() {
			if (!decoder.next()) {
				return null;
			}
			const end = decoder.end_timestamp;
			const delayMs = end - previousEnd;
			previousEnd = end;
			return {rgba: new Uint8Array(requireMemory().buffer, decoder.frame_ptr, frameBytes), delayMs};
		},
		close() {
			if (closed) return;
			closed = true;
			decoder.free();
		},
	};
}

export function openWebpFrameSink(width: number, height: number, plays: number, rung: WebpRung): WebpFrameSink {
	requireMemory();
	const encoder = new AnimEncoder(width, height, plays, rung.lossless);
	let elapsed = 0;
	let closed = false;
	return {
		add(rgba, delayMs) {
			encoder.add(rgba, Math.round(elapsed), rung.quality, rung.method);
			elapsed += Math.min(delayMs, WEBP_MAX_FRAME_DURATION_MS);
		},
		finish() {
			return encoder.finish(Math.round(elapsed));
		},
		close() {
			if (closed) return;
			closed = true;
			encoder.free();
		},
	};
}
