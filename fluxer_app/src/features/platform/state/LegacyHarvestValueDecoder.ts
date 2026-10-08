// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_LEGACY_HARVEST_BLOB_BYTES_KEY,
	DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY,
	DESKTOP_LEGACY_HARVEST_BLOB_TRANSPORT_TYPE,
	DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY,
	DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_VERSION,
	DesktopLegacyHarvestEncodedNumber,
	DesktopLegacyHarvestEncodedValueType,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const MAX_VALUE_DEPTH = 64;
const MAX_VALUE_NODES = 1_000_000;
const MAX_BINARY_BYTES = 128 * 1024 * 1024;
const MAX_BLOB_BYTES = 32 * 1024 * 1024;
const MAX_TOTAL_BLOB_BYTES = 256 * 1024 * 1024;
const MAX_BIGINT_DIGITS = 4096;
const MAX_REGEXP_SOURCE_LENGTH = 64 * 1024;
const MAX_REGEXP_FLAGS_LENGTH = 32;
const BLOB_ID_PATTERN = /^[0-9a-f]{32}$/u;
const BLOB_HASH_PATTERN = /^[0-9a-f]{64}$/u;
const BASE64_PATTERN = /^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/u;
const BIGINT_PATTERN = /^-?(?:0|[1-9][0-9]*)$/u;

export type LegacyHarvestValueDecoder = (value: unknown) => unknown;

interface DecodedBlob {
	readonly blob: Blob;
	readonly sha256: string;
	readonly size: number;
	readonly type: string;
}

interface LegacyHarvestValueContext {
	readonly activeObjects: WeakSet<object>;
	readonly blobs: Map<string, DecodedBlob>;
	totalBinaryBytes: number;
	totalBlobBytes: number;
	totalNodes: number;
}

class LegacyHarvestBlobError extends Error {
	public constructor() {
		super('The legacy harvest contains an incomplete blob value');
		this.name = 'LegacyHarvestBlobError';
	}
}

class LegacyHarvestValueError extends Error {
	public constructor() {
		super('The legacy harvest contains an invalid encoded value');
		this.name = 'LegacyHarvestValueError';
	}
}

function createContext(): LegacyHarvestValueContext {
	return {
		activeObjects: new WeakSet(),
		blobs: new Map(),
		totalBinaryBytes: 0,
		totalBlobBytes: 0,
		totalNodes: 0,
	};
}

function isPlainRecord(value: unknown): value is Record<string, unknown> {
	if (typeof value !== 'object' || value === null || Array.isArray(value)) return false;
	const prototype = Object.getPrototypeOf(value);
	return prototype === Object.prototype || prototype === null;
}

function readEncodedPayload(value: Record<string, unknown>): ReadonlyArray<unknown> | null {
	if (!Object.hasOwn(value, DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY)) return null;
	const payload = value[DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY];
	if (
		Object.keys(value).length !== 1 ||
		!Array.isArray(payload) ||
		payload.length < 2 ||
		payload[0] !== DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_VERSION ||
		typeof payload[1] !== 'string'
	) {
		throw new LegacyHarvestValueError();
	}
	return payload;
}

function decodeNumber(value: unknown): number {
	if (value === DesktopLegacyHarvestEncodedNumber.NOT_A_NUMBER) return Number.NaN;
	if (value === DesktopLegacyHarvestEncodedNumber.POSITIVE_INFINITY) return Number.POSITIVE_INFINITY;
	if (value === DesktopLegacyHarvestEncodedNumber.NEGATIVE_INFINITY) return Number.NEGATIVE_INFINITY;
	if (value === DesktopLegacyHarvestEncodedNumber.NEGATIVE_ZERO) return -0;
	throw new LegacyHarvestValueError();
}

function decodeDate(value: unknown): number {
	if (value === DesktopLegacyHarvestEncodedNumber.NOT_A_NUMBER) return Number.NaN;
	if (typeof value === 'number' && Number.isFinite(value)) return value;
	throw new LegacyHarvestValueError();
}

function decodeBase64(value: unknown, context: LegacyHarvestValueContext): Uint8Array {
	if (typeof value !== 'string' || value.length % 4 !== 0 || !BASE64_PATTERN.test(value)) {
		throw new LegacyHarvestValueError();
	}
	const padding = value.endsWith('==') ? 2 : value.endsWith('=') ? 1 : 0;
	if ((value.length / 4) * 3 - padding > MAX_BINARY_BYTES) {
		throw new LegacyHarvestValueError();
	}
	let binary: string;
	try {
		binary = globalThis.atob(value);
	} catch {
		throw new LegacyHarvestValueError();
	}
	const bytes = new Uint8Array(binary.length);
	for (let index = 0; index < binary.length; index += 1) {
		bytes[index] = binary.charCodeAt(index);
	}
	context.totalBinaryBytes += bytes.byteLength;
	if (context.totalBinaryBytes > MAX_BINARY_BYTES) {
		throw new LegacyHarvestValueError();
	}
	return bytes;
}

function arrayBufferFromBytes(bytes: Uint8Array): ArrayBuffer {
	const buffer = new ArrayBuffer(bytes.byteLength);
	new Uint8Array(buffer).set(bytes);
	return buffer;
}

function typedArrayFromBytes(name: unknown, bytes: Uint8Array): ArrayBufferView {
	const buffer = arrayBufferFromBytes(bytes);
	if (name === 'DataView') return new DataView(buffer);
	if (name === 'Int8Array' && bytes.byteLength % Int8Array.BYTES_PER_ELEMENT === 0) return new Int8Array(buffer);
	if (name === 'Uint8Array') return new Uint8Array(buffer);
	if (name === 'Uint8ClampedArray') return new Uint8ClampedArray(buffer);
	if (name === 'Int16Array' && bytes.byteLength % Int16Array.BYTES_PER_ELEMENT === 0) return new Int16Array(buffer);
	if (name === 'Uint16Array' && bytes.byteLength % Uint16Array.BYTES_PER_ELEMENT === 0) return new Uint16Array(buffer);
	if (name === 'Int32Array' && bytes.byteLength % Int32Array.BYTES_PER_ELEMENT === 0) return new Int32Array(buffer);
	if (name === 'Uint32Array' && bytes.byteLength % Uint32Array.BYTES_PER_ELEMENT === 0) return new Uint32Array(buffer);
	if (name === 'Float32Array' && bytes.byteLength % Float32Array.BYTES_PER_ELEMENT === 0) {
		return new Float32Array(buffer);
	}
	if (name === 'Float64Array' && bytes.byteLength % Float64Array.BYTES_PER_ELEMENT === 0) {
		return new Float64Array(buffer);
	}
	if (
		name === 'BigInt64Array' &&
		typeof BigInt64Array !== 'undefined' &&
		bytes.byteLength % BigInt64Array.BYTES_PER_ELEMENT === 0
	) {
		return new BigInt64Array(buffer);
	}
	if (
		name === 'BigUint64Array' &&
		typeof BigUint64Array !== 'undefined' &&
		bytes.byteLength % BigUint64Array.BYTES_PER_ELEMENT === 0
	) {
		return new BigUint64Array(buffer);
	}
	throw new LegacyHarvestValueError();
}

function accountBlobBytes(context: LegacyHarvestValueContext, bytes: Uint8Array): void {
	context.totalBlobBytes += bytes.byteLength;
	if (context.totalBlobBytes > MAX_TOTAL_BLOB_BYTES) {
		throw new LegacyHarvestValueError();
	}
}

function readLegacyBlob(value: Record<string, unknown>, context: LegacyHarvestValueContext): Blob | null {
	const hasReference = Object.hasOwn(value, DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY);
	const hasBytes = Object.hasOwn(value, DESKTOP_LEGACY_HARVEST_BLOB_BYTES_KEY);
	if (hasReference && hasBytes) {
		if (Object.keys(value).length !== 2) throw new LegacyHarvestBlobError();
		const reference = value[DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY];
		const bytes = value[DESKTOP_LEGACY_HARVEST_BLOB_BYTES_KEY];
		if (
			!isPlainRecord(reference) ||
			Object.keys(reference).length !== 3 ||
			typeof reference.blobId !== 'string' ||
			!BLOB_ID_PATTERN.test(reference.blobId) ||
			typeof reference.type !== 'string' ||
			typeof reference.size !== 'number' ||
			!Number.isSafeInteger(reference.size) ||
			reference.size < 0 ||
			reference.size > MAX_BLOB_BYTES ||
			!(bytes instanceof Uint8Array) ||
			reference.size !== bytes.byteLength
		) {
			throw new LegacyHarvestBlobError();
		}
		accountBlobBytes(context, bytes);
		return new Blob([new Uint8Array(bytes)], {type: reference.type});
	}
	if (hasReference || hasBytes) return null;
	const payload = readEncodedPayload(value);
	if (payload === null || payload[1] !== DESKTOP_LEGACY_HARVEST_BLOB_TRANSPORT_TYPE) return null;
	if (payload.length !== 4) throw new LegacyHarvestBlobError();
	const reference = payload[2];
	const bytes = payload[3];
	if (!isPlainRecord(reference) || Object.keys(reference).length !== 4 || !(bytes instanceof Uint8Array)) {
		throw new LegacyHarvestBlobError();
	}
	const {blobId, sha256, size, type} = reference;
	if (
		typeof blobId !== 'string' ||
		!BLOB_ID_PATTERN.test(blobId) ||
		typeof sha256 !== 'string' ||
		!BLOB_HASH_PATTERN.test(sha256) ||
		typeof size !== 'number' ||
		!Number.isSafeInteger(size) ||
		size < 0 ||
		size > MAX_BLOB_BYTES ||
		size !== bytes.byteLength ||
		typeof type !== 'string'
	) {
		throw new LegacyHarvestBlobError();
	}
	const cached = context.blobs.get(blobId);
	if (cached != null) {
		if (cached.sha256 !== sha256 || cached.size !== size || cached.type !== type) {
			throw new LegacyHarvestBlobError();
		}
		return cached.blob;
	}
	accountBlobBytes(context, bytes);
	const blob = new Blob([new Uint8Array(bytes)], {type});
	context.blobs.set(blobId, {blob, sha256, size, type});
	return blob;
}

function defineProperty(target: Record<string, unknown>, key: string, value: unknown): void {
	Object.defineProperty(target, key, {configurable: true, enumerable: true, value, writable: true});
}

function decodeEncodedValue(
	payload: ReadonlyArray<unknown>,
	context: LegacyHarvestValueContext,
	depth: number,
): unknown {
	switch (payload[1]) {
		case DesktopLegacyHarvestEncodedValueType.UNDEFINED:
			if (payload.length !== 2) throw new LegacyHarvestValueError();
			return undefined;
		case DesktopLegacyHarvestEncodedValueType.NUMBER:
			if (payload.length !== 3) throw new LegacyHarvestValueError();
			return decodeNumber(payload[2]);
		case DesktopLegacyHarvestEncodedValueType.BIGINT: {
			const value = payload[2];
			if (
				payload.length !== 3 ||
				typeof value !== 'string' ||
				(value.startsWith('-') ? value.length - 1 : value.length) > MAX_BIGINT_DIGITS ||
				!BIGINT_PATTERN.test(value)
			) {
				throw new LegacyHarvestValueError();
			}
			try {
				return BigInt(value);
			} catch {
				throw new LegacyHarvestValueError();
			}
		}
		case DesktopLegacyHarvestEncodedValueType.DATE:
			if (payload.length !== 3) throw new LegacyHarvestValueError();
			return new Date(decodeDate(payload[2]));
		case DesktopLegacyHarvestEncodedValueType.ARRAY_BUFFER:
			if (payload.length !== 3) throw new LegacyHarvestValueError();
			return arrayBufferFromBytes(decodeBase64(payload[2], context));
		case DesktopLegacyHarvestEncodedValueType.TYPED_ARRAY:
			if (payload.length !== 4) throw new LegacyHarvestValueError();
			return typedArrayFromBytes(payload[2], decodeBase64(payload[3], context));
		case DesktopLegacyHarvestEncodedValueType.REGEXP:
			if (
				payload.length !== 4 ||
				typeof payload[2] !== 'string' ||
				typeof payload[3] !== 'string' ||
				payload[2].length > MAX_REGEXP_SOURCE_LENGTH ||
				payload[3].length > MAX_REGEXP_FLAGS_LENGTH
			) {
				throw new LegacyHarvestValueError();
			}
			try {
				return new RegExp(payload[2], payload[3]);
			} catch {
				throw new LegacyHarvestValueError();
			}
		case DesktopLegacyHarvestEncodedValueType.MAP: {
			if (payload.length !== 3 || !Array.isArray(payload[2])) throw new LegacyHarvestValueError();
			const map = new Map<unknown, unknown>();
			for (const entry of payload[2]) {
				if (!Array.isArray(entry) || entry.length !== 2) throw new LegacyHarvestValueError();
				map.set(decodeValue(entry[0], context, depth + 1), decodeValue(entry[1], context, depth + 1));
			}
			return map;
		}
		case DesktopLegacyHarvestEncodedValueType.SET: {
			if (payload.length !== 3 || !Array.isArray(payload[2])) throw new LegacyHarvestValueError();
			const set = new Set<unknown>();
			for (const item of payload[2]) {
				set.add(decodeValue(item, context, depth + 1));
			}
			return set;
		}
		case DesktopLegacyHarvestEncodedValueType.OBJECT: {
			if (payload.length !== 3 || !Array.isArray(payload[2])) throw new LegacyHarvestValueError();
			const decoded = Object.create(null) as Record<string, unknown>;
			for (const entry of payload[2]) {
				if (
					!Array.isArray(entry) ||
					entry.length !== 2 ||
					typeof entry[0] !== 'string' ||
					Object.hasOwn(decoded, entry[0])
				) {
					throw new LegacyHarvestValueError();
				}
				defineProperty(decoded, entry[0], decodeValue(entry[1], context, depth + 1));
			}
			return decoded;
		}
		case DESKTOP_LEGACY_HARVEST_BLOB_TRANSPORT_TYPE:
			throw new LegacyHarvestBlobError();
		default:
			throw new LegacyHarvestValueError();
	}
}

function decodeValue(value: unknown, context: LegacyHarvestValueContext, depth: number = 0): unknown {
	context.totalNodes += 1;
	if (context.totalNodes > MAX_VALUE_NODES || depth > MAX_VALUE_DEPTH) {
		throw new LegacyHarvestValueError();
	}
	if (value === null || typeof value !== 'object') return value;
	if (
		value instanceof Date ||
		value instanceof RegExp ||
		value instanceof ArrayBuffer ||
		ArrayBuffer.isView(value) ||
		value instanceof Blob
	) {
		return value;
	}
	if (context.activeObjects.has(value)) throw new LegacyHarvestValueError();
	context.activeObjects.add(value);
	try {
		if (Array.isArray(value)) {
			return value.map((item) => decodeValue(item, context, depth + 1));
		}
		if (value instanceof Map) {
			const decoded = new Map<unknown, unknown>();
			for (const [key, item] of value) {
				decoded.set(decodeValue(key, context, depth + 1), decodeValue(item, context, depth + 1));
			}
			return decoded;
		}
		if (value instanceof Set) {
			const decoded = new Set<unknown>();
			for (const item of value) {
				decoded.add(decodeValue(item, context, depth + 1));
			}
			return decoded;
		}
		if (!isPlainRecord(value)) throw new LegacyHarvestValueError();
		const blob = readLegacyBlob(value, context);
		if (blob !== null) return blob;
		const payload = readEncodedPayload(value);
		if (payload !== null) return decodeEncodedValue(payload, context, depth);
		const decoded = Object.create(null) as Record<string, unknown>;
		for (const [key, item] of Object.entries(value)) {
			defineProperty(decoded, key, decodeValue(item, context, depth + 1));
		}
		return decoded;
	} finally {
		context.activeObjects.delete(value);
	}
}

export function createLegacyHarvestValueDecoder(): LegacyHarvestValueDecoder {
	const context = createContext();
	return (value) => decodeValue(value, context);
}
