// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveWorkerAssetUrl} from '@app/features/platform/utils/WorkerAssetUrl';
import initLibfluxcore, * as wasm from '@pkgs/libfluxcore/libfluxcore';
import libfluxcoreWasmUrl from '@pkgs/libfluxcore/libfluxcore_bg.wasm';

let modulePromise: Promise<void> | null = null;
let moduleReady = false;

export async function ensureLibfluxcoreReady(): Promise<void> {
	if (!modulePromise) {
		modulePromise = initLibfluxcore({module_or_path: resolveWorkerAssetUrl(libfluxcoreWasmUrl)})
			.then(() => {
				moduleReady = true;
			})
			.catch((error) => {
				modulePromise = null;
				moduleReady = false;
				throw error;
			});
	}
	await modulePromise;
}

function toU32(value: number): number {
	return Number.isFinite(value) && value > 0 ? Math.min(0xffffffff, Math.floor(value)) : 0;
}

export function cropRotateRgbaInto(
	rgba: Uint8Array,
	width: number,
	height: number,
	x: number,
	y: number,
	cropWidth: number,
	cropHeight: number,
	rotation: number,
	output: Uint8Array,
	outputWidth: number,
	outputHeight: number,
): void {
	if (!moduleReady) {
		throw new Error('WASM module not loaded. Call ensureLibfluxcoreReady() first.');
	}
	wasm.crop_rotate_rgba_into_buffer(
		rgba,
		output,
		toU32(width),
		toU32(height),
		toU32(x),
		toU32(y),
		toU32(cropWidth),
		toU32(cropHeight),
		toU32(rotation),
		toU32(outputWidth),
		toU32(outputHeight),
	);
}
