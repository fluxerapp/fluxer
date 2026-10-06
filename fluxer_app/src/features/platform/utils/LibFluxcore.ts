// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {InitOutput} from '@pkgs/libfluxcore/libfluxcore';
import initLibfluxcore, * as wasm from '@pkgs/libfluxcore/libfluxcore';

const logger = new Logger('libfluxcore');
const MAX_RETAINED_LIBFLUXCORE_WASM_MEMORY_BYTES = 64 * 1024 * 1024;

let modulePromise: Promise<void> | null = null;
let moduleReady = false;
let moduleExports: InitOutput | null = null;
let pendingModuleOperations = 0;
const liveZstdStreamDecoders = new Set<number>();
const liveZstdStreamEncoders = new Set<number>();

async function loadModule(): Promise<void> {
	if (!modulePromise) {
		modulePromise = (async () => {
			try {
				if (typeof initLibfluxcore === 'function') {
					moduleExports = await initLibfluxcore();
				}
				moduleReady = true;
			} catch (err) {
				moduleExports = null;
				moduleReady = false;
				modulePromise = null;
				logger.warn('Failed to load wasm module', err);
				throw err;
			}
		})();
	}
	await modulePromise;
}

function isLibfluxcoreModulePinned(): boolean {
	return pendingModuleOperations > 0 || liveZstdStreamDecoders.size > 0 || liveZstdStreamEncoders.size > 0;
}

async function withPinnedLibfluxcoreModule<T>(run: () => T): Promise<T> {
	pendingModuleOperations++;
	try {
		await loadModule();
		return run();
	} finally {
		pendingModuleOperations--;
		releaseLibfluxcoreMemoryIfIdle();
	}
}

function requireLiveZstdStreamHandle(handle: number, live: Set<number>): number {
	if (!live.has(handle)) {
		throw new Error('libfluxcore zstd stream handle is no longer live');
	}
	return handle;
}

export function isLibfluxcoreReady(): boolean {
	return moduleReady;
}

export async function ensureLibfluxcoreReady(): Promise<void> {
	await loadModule();
}

export function releaseLibfluxcoreMemoryIfIdle(): void {
	if (isLibfluxcoreModulePinned()) return;
	if ((moduleExports?.memory.buffer.byteLength ?? 0) <= MAX_RETAINED_LIBFLUXCORE_WASM_MEMORY_BYTES) return;
	wasm.__resetLibfluxcoreWasmForMemoryPressure();
	modulePromise = null;
	moduleReady = false;
	moduleExports = null;
}

export async function detectAnimatedImage(data: Uint8Array): Promise<boolean> {
	return withPinnedLibfluxcoreModule(() => Boolean(wasm.is_animated_image(data)));
}

export async function sniffImageFormat(data: Uint8Array): Promise<number> {
	return withPinnedLibfluxcoreModule(() => wasm.sniff_image_format(data));
}

export async function decompressZstdFrame(input: Uint8Array): Promise<Uint8Array | null> {
	return withPinnedLibfluxcoreModule(() => {
		const result = wasm.decompress_zstd_frame(input);
		return result instanceof Uint8Array ? result : new Uint8Array(result);
	});
}

export async function createZstdStreamDecoder(): Promise<number> {
	return withPinnedLibfluxcoreModule(() => {
		const decoder = wasm.create_zstd_stream_decoder();
		liveZstdStreamDecoders.add(decoder);
		return decoder;
	});
}

export async function decompressZstdStreamChunk(decoder: number, input: Uint8Array): Promise<Uint8Array> {
	return withPinnedLibfluxcoreModule(() => {
		const result = wasm.decompress_zstd_stream_chunk(
			requireLiveZstdStreamHandle(decoder, liveZstdStreamDecoders),
			input,
		);
		return result instanceof Uint8Array ? result : new Uint8Array(result);
	});
}

export function freeZstdStreamDecoder(decoder: number): void {
	if (!liveZstdStreamDecoders.delete(decoder)) return;
	try {
		wasm.free_zstd_stream_decoder(decoder);
	} finally {
		releaseLibfluxcoreMemoryIfIdle();
	}
}

export async function createZstdStreamEncoder(level: number): Promise<number> {
	return withPinnedLibfluxcoreModule(() => {
		const encoder = wasm.create_zstd_stream_encoder(level);
		liveZstdStreamEncoders.add(encoder);
		return encoder;
	});
}

export function createZstdStreamEncoderSync(level: number): number {
	const encoder = wasm.create_zstd_stream_encoder(level);
	liveZstdStreamEncoders.add(encoder);
	return encoder;
}

export function compressZstdStreamChunk(encoder: number, input: Uint8Array): Uint8Array {
	const result = wasm.compress_zstd_stream_chunk(requireLiveZstdStreamHandle(encoder, liveZstdStreamEncoders), input);
	return result instanceof Uint8Array ? result : new Uint8Array(result);
}

export function freeZstdStreamEncoder(encoder: number): void {
	if (!liveZstdStreamEncoders.delete(encoder)) return;
	try {
		wasm.free_zstd_stream_encoder(encoder);
	} finally {
		releaseLibfluxcoreMemoryIfIdle();
	}
}
