/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

let cachedWasm: WebAssembly.Instance | null = null;
let wasmMemory: WebAssembly.Memory | null = null;
let wasmExternrefTable: WebAssembly.Table | null = null;
let wasmModule: WebAssembly.Module | null = null;

declare const self: DedicatedWorkerGlobalScope;

function getArrayU8FromWasm0(ptr: number, len: number): Uint8Array {
	ptr = ptr >>> 0;
	return getUint8ArrayMemory0().subarray(ptr / 1, ptr / 1 + len);
}

let cachedUint8ArrayMemory0: Uint8Array | null = null;
function getUint8ArrayMemory0(): Uint8Array {
	if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
		cachedUint8ArrayMemory0 = new Uint8Array(wasmMemory!.buffer);
	}
	return cachedUint8ArrayMemory0;
}

function isLikeNone(x: unknown): x is null | undefined {
	return x === null || x === undefined;
}

let WASM_VECTOR_LEN = 0;

function passArray8ToWasm0(arg: Uint8Array, malloc: (n: number, align: number) => number): number {
	const ptr = malloc(arg.length * 1, 1) >>> 0;
	getUint8ArrayMemory0().set(arg, ptr / 1);
	WASM_VECTOR_LEN = arg.length;
	return ptr;
}

let cachedTextDecoder = new TextDecoder('utf-8', {ignoreBOM: true, fatal: true});
cachedTextDecoder.decode();

const MAX_SAFARI_DECODE_BYTES = 2146435072;
let numBytesDecoded = 0;

function decodeText(ptr: number, len: number): string {
	numBytesDecoded += len;
	if (numBytesDecoded >= MAX_SAFARI_DECODE_BYTES) {
		cachedTextDecoder = new TextDecoder('utf-8', {ignoreBOM: true, fatal: true});
		cachedTextDecoder.decode();
		numBytesDecoded = len;
	}
	return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

function takeObject(idx: number): any {
	const ret = wasmExternrefTable!.get(idx);
	wasmExternrefTable!.set(idx, undefined);
	return ret;
}

async function loadWasmModule(wasmUrl: string): Promise<void> {
	if (cachedWasm) {
		return;
	}

	const response = await fetch(wasmUrl);
	if (!response.ok) {
		throw new Error(`Failed to fetch WASM: ${response.statusText}`);
	}

	const buffer = await response.arrayBuffer();
	wasmModule = await WebAssembly.compile(buffer);

	const imports = getImports();
	const instance = await WebAssembly.instantiate(wasmModule, imports);
	cachedWasm = instance;

	wasmMemory = instance.exports.memory as WebAssembly.Memory;
	wasmExternrefTable = instance.exports.__wbindgen_externref_table as WebAssembly.Table;

	cachedUint8ArrayMemory0 = null;

	const exports = instance.exports as any;
	exports.__wbindgen_start();
}

function getImports(): Record<string, any> {
	const imports: Record<string, any> = {
		wbg: {},
	};

	imports.wbg.__wbindgen_string_get = (arg0: number, arg1: number) => {
		const ret = decodeText(arg0, arg1);
		return ret;
	};

	imports.wbg.__wbindgen_is_null = (arg0: number) => (arg0 === 0 ? 1 : 0);

	imports.wbg.__wbindgen_is_undefined = (arg0: number) => (arg0 === 0 ? 1 : 0);

	imports.wbg.__wbindgen_number_get = (_arg0: number, arg1: number) => {
		const obj = takeObject(arg1);
		const ret = typeof obj === 'number' ? obj : undefined;
		return ret;
	};

	imports.wbg.__wbindgen_object_clone_ref = (arg0: number) => {
		const obj = takeObject(arg0);
		const ret = obj;
		return ret;
	};

	imports.wbg.__wbindgen_object_drop_ref = (_arg0: number) => {};

	imports.wbg.__wbindgen_cb_drop = (arg0: number) => {
		const obj = takeObject(arg0).original;
		if (obj.cnt-- === 1) {
			obj.a = 0;
			return 1;
		}
		return 0;
	};

	imports.wbg.__wbindgen_init_externref_table = () => {
		const table = wasmExternrefTable!;
		const offset = table.grow(4);
		table.set(0, undefined);
		table.set(offset + 0, undefined);
		table.set(offset + 1, null);
		table.set(offset + 2, true);
		table.set(offset + 3, false);
	};

	imports.wbg.__wbindgen_throw = (arg0: number, arg1: number) => {
		throw new Error(decodeText(arg0, arg1));
	};

	return imports;
}

export async function ensureLibfluxcoreReady(): Promise<void> {
	const wasmUrl = new URL('@pkgs/libfluxcore/libfluxcore_bg.wasm', import.meta.url);
	await loadWasmModule(wasmUrl.href);
}

export function cropAndRotateGif(
	gif: Uint8Array,
	x: number,
	y: number,
	width: number,
	height: number,
	rotation: number,
	resizeWidth: number | null,
	resizeHeight: number | null,
): Uint8Array {
	if (!cachedWasm) {
		throw new Error('WASM module not loaded. Call ensureLibfluxcoreReady() first.');
	}

	const exports = cachedWasm.exports as any;

	const ptr0 = passArray8ToWasm0(gif, exports.__wbindgen_malloc);
	const len0 = WASM_VECTOR_LEN;

	const ret = exports.crop_and_rotate_gif(
		ptr0,
		len0,
		x,
		y,
		width,
		height,
		rotation,
		isLikeNone(resizeWidth) ? 0x100000001 : (resizeWidth ?? 0) >>> 0,
		isLikeNone(resizeHeight) ? 0x100000001 : (resizeHeight ?? 0) >>> 0,
	);

	if (ret[3]) {
		throw takeObject(ret[2]);
	}

	const v2 = getArrayU8FromWasm0(ret[0], ret[1]).slice();
	exports.__wbindgen_free(ret[0], ret[1] * 1, 1);

	return v2;
}
