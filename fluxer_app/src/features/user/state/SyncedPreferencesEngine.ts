// SPDX-License-Identifier: AGPL-3.0-or-later

import {fromBinary, toBinary} from '@bufbuild/protobuf';
import {type SyncedPreferences, SyncedPreferencesSchema} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import {base64ToUint8Array, uint8ArrayToBase64} from 'uint8array-extras';

const EMPTY_BYTES = new Uint8Array();

export type SyncedPreferencesField = Exclude<keyof SyncedPreferences, '$typeName' | '$unknown'>;

export type {SyncedPreferences};

interface TopLevelFieldChunk {
	field: number;
	bytes: Uint8Array;
}

export const SYNCED_PREFERENCES_FIELDS = SyncedPreferencesSchema.fields.map(
	(field) => field.localName as SyncedPreferencesField,
);
export const SYNCED_PREFERENCES_FIELD_NAMES = new Set<string>(SYNCED_PREFERENCES_FIELDS.map(String));
const FIELD_TO_NUMBER = new Map<SyncedPreferencesField, number>(
	SyncedPreferencesSchema.fields.map((field) => [field.localName as SyncedPreferencesField, field.number]),
);
const NUMBER_TO_FIELD = new Map<number, SyncedPreferencesField>(
	SyncedPreferencesSchema.fields.map((field) => [field.number, field.localName as SyncedPreferencesField]),
);

export function isSyncedPreferencesField(value: unknown): value is SyncedPreferencesField {
	return typeof value === 'string' && SYNCED_PREFERENCES_FIELD_NAMES.has(value);
}

function fieldNumber(field: SyncedPreferencesField): number {
	const number = FIELD_TO_NUMBER.get(field);
	if (number == null) {
		throw new Error(`Unknown synced preferences field: ${String(field)}`);
	}
	return number;
}

function fieldName(number: number): SyncedPreferencesField | null {
	return NUMBER_TO_FIELD.get(number) ?? null;
}

function toFieldNames(fieldNumbers: ArrayLike<number>): Array<SyncedPreferencesField> {
	const fields: Array<SyncedPreferencesField> = [];
	for (let index = 0; index < fieldNumbers.length; index += 1) {
		const number = fieldNumbers[index];
		const field = fieldName(number);
		if (field != null) fields.push(field);
	}
	return fields;
}

function parseTopLevelFieldChunks(bytes: Uint8Array): Array<TopLevelFieldChunk> {
	const chunks: Array<TopLevelFieldChunk> = [];
	let offset = 0;
	while (offset < bytes.byteLength) {
		const start = offset;
		const key = readVarint(bytes, offset);
		if (key == null) break;
		offset = key.nextOffset;
		const field = key.value >> 3;
		const nextOffset = skipWireValue(bytes, offset, key.value & 7);
		if (nextOffset == null) break;
		offset = nextOffset;
		chunks.push({field, bytes: bytes.slice(start, offset)});
	}
	return chunks;
}

function indexFieldChunks(chunks: ReadonlyArray<TopLevelFieldChunk>): Map<number, Array<Uint8Array>> {
	const fields = new Map<number, Array<Uint8Array>>();
	for (const chunk of chunks) {
		const entries = fields.get(chunk.field) ?? [];
		entries.push(chunk.bytes);
		fields.set(chunk.field, entries);
	}
	return fields;
}

function readVarint(bytes: Uint8Array, offset: number): {value: number; nextOffset: number} | null {
	let value = 0;
	let shift = 0;
	while (offset < bytes.byteLength) {
		const byte = bytes[offset++];
		value |= (byte & 0x7f) << shift;
		if ((byte & 0x80) === 0) {
			return {value, nextOffset: offset};
		}
		shift += 7;
	}
	return null;
}

function skipWireValue(bytes: Uint8Array, offset: number, wireType: number): number | null {
	if (wireType === 0) {
		return readVarint(bytes, offset)?.nextOffset ?? null;
	}
	if (wireType === 1) {
		const nextOffset = offset + 8;
		return nextOffset <= bytes.byteLength ? nextOffset : null;
	}
	if (wireType === 2) {
		const length = readVarint(bytes, offset);
		if (length == null) return null;
		const nextOffset = length.nextOffset + length.value;
		return nextOffset <= bytes.byteLength ? nextOffset : null;
	}
	if (wireType === 5) {
		const nextOffset = offset + 4;
		return nextOffset <= bytes.byteLength ? nextOffset : null;
	}
	return null;
}

function concatChunks(chunks: ReadonlyArray<Uint8Array>): Uint8Array {
	let total = 0;
	for (const chunk of chunks) total += chunk.byteLength;
	const out = new Uint8Array(total);
	let offset = 0;
	for (const chunk of chunks) {
		out.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return out;
}

function replaceField(target: Uint8Array, source: Uint8Array, field: number): Uint8Array {
	const targetFields = parseTopLevelFieldChunks(target);
	const sourceChunks = indexFieldChunks(parseTopLevelFieldChunks(source)).get(field) ?? [];
	const pieces: Array<Uint8Array> = [];
	let inserted = false;
	for (const chunk of targetFields) {
		if (chunk.field === field) {
			if (!inserted) {
				for (const sourceChunk of sourceChunks) pieces.push(sourceChunk);
				inserted = true;
			}
			continue;
		}
		pieces.push(chunk.bytes);
	}
	if (!inserted) {
		for (const sourceChunk of sourceChunks) pieces.push(sourceChunk);
	}
	return concatChunks(pieces);
}

function chunksEqual(left: Array<Uint8Array> | undefined, right: Array<Uint8Array> | undefined): boolean {
	if (left == null || right == null) return left == null && right == null;
	if (left.length !== right.length) return false;
	return left.every((chunk, index) => {
		const other = right[index];
		if (chunk.byteLength !== other.byteLength) return false;
		for (let i = 0; i < chunk.byteLength; i += 1) {
			if (chunk[i] !== other[i]) return false;
		}
		return true;
	});
}

function changedFieldNumbers(left: Uint8Array, right: Uint8Array): Array<number> {
	const leftFields = indexFieldChunks(parseTopLevelFieldChunks(left));
	const rightFields = indexFieldChunks(parseTopLevelFieldChunks(right));
	const fieldNumbers = new Set([...leftFields.keys(), ...rightFields.keys()]);
	return Array.from(fieldNumbers)
		.sort((a, b) => a - b)
		.filter((field) => !chunksEqual(leftFields.get(field), rightFields.get(field)));
}

export function preferencesToBytes(preferences: SyncedPreferences): Uint8Array {
	return toBinary(SyncedPreferencesSchema, preferences);
}

export function preferencesFromBytes(bytes: Uint8Array): SyncedPreferences {
	if (bytes.byteLength === 0) return createEmptySyncedPreferences();
	return fromBinary(SyncedPreferencesSchema, bytes);
}

export function createEmptySyncedPreferences(): SyncedPreferences {
	return fromBinary(SyncedPreferencesSchema, EMPTY_BYTES);
}

export function encodeSyncedPreferences(preferences: SyncedPreferences): string {
	const bytes = preferencesToBytes(preferences);
	if (bytes.byteLength === 0) return '';
	return uint8ArrayToBase64(bytes);
}

export function decodeSyncedPreferencesBytes(encoded: string | null | undefined): Uint8Array {
	if (!encoded) return EMPTY_BYTES;
	try {
		return base64ToUint8Array(encoded);
	} catch (error) {
		throw new SyncedPreferencesDecodeError(
			error instanceof Error ? `invalid base64: ${error.message}` : 'invalid base64',
		);
	}
}

export function decodeSyncedPreferences(encoded: string | null | undefined): SyncedPreferences {
	try {
		return preferencesFromBytes(decodeSyncedPreferencesBytes(encoded));
	} catch (error) {
		if (error instanceof SyncedPreferencesDecodeError) throw error;
		throw new SyncedPreferencesDecodeError(error instanceof Error ? error.message : 'invalid synced preferences');
	}
}

export function decodeSyncedPreferencesLenient(encoded: string | null | undefined): SyncedPreferences {
	try {
		return decodeSyncedPreferences(encoded);
	} catch {
		return createEmptySyncedPreferences();
	}
}

export function changedSyncedPreferenceFields(
	left: SyncedPreferences,
	right: SyncedPreferences,
): Array<SyncedPreferencesField> {
	return toFieldNames(changedFieldNumbers(preferencesToBytes(left), preferencesToBytes(right)));
}

export function syncedPreferencesEqual(left: SyncedPreferences, right: SyncedPreferences): boolean {
	return changedFieldNumbers(preferencesToBytes(left), preferencesToBytes(right)).length === 0;
}

export function copySyncedPreferenceField(
	target: SyncedPreferences,
	source: SyncedPreferences,
	field: SyncedPreferencesField,
): SyncedPreferences {
	const bytes = replaceField(preferencesToBytes(target), preferencesToBytes(source), fieldNumber(field));
	return preferencesFromBytes(bytes);
}

export function mergeIncomingSyncedPreferences(args: {
	local: SyncedPreferences;
	wire: SyncedPreferences;
	incoming: SyncedPreferences;
	protectedFields: Iterable<SyncedPreferencesField>;
	recentlyAckedFields: Iterable<SyncedPreferencesField>;
	inFlight: SyncedPreferences | null;
	syncInFlight: boolean;
}): {
	merged: SyncedPreferences;
	wire: SyncedPreferences;
	dirtyFields: Array<SyncedPreferencesField>;
} {
	const localBytes = preferencesToBytes(args.local);
	const incomingBytes = preferencesToBytes(args.incoming);
	const inFlightBytes = args.inFlight == null ? null : preferencesToBytes(args.inFlight);
	const protectedSet = new Set<number>();
	for (const field of args.protectedFields) protectedSet.add(fieldNumber(field));
	const ackedSet = new Set<number>();
	for (const field of args.recentlyAckedFields) ackedSet.add(fieldNumber(field));
	const localFields = indexFieldChunks(parseTopLevelFieldChunks(localBytes));
	const incomingFields = indexFieldChunks(parseTopLevelFieldChunks(incomingBytes));
	const inFlightFields =
		inFlightBytes == null
			? new Map<number, Array<Uint8Array>>()
			: indexFieldChunks(parseTopLevelFieldChunks(inFlightBytes));
	let merged = incomingBytes;
	let wire = incomingBytes;
	const dirtyFieldNumbers: Array<number> = [];
	const fieldNumbers = new Set([...localFields.keys(), ...incomingFields.keys(), ...inFlightFields.keys()]);
	for (const field of Array.from(fieldNumbers).sort((a, b) => a - b)) {
		if (protectedSet.has(field)) {
			merged = replaceField(merged, localBytes, field);
			continue;
		}
		if (ackedSet.has(field) && !chunksEqual(incomingFields.get(field), localFields.get(field))) {
			merged = replaceField(merged, localBytes, field);
			wire = replaceField(wire, localBytes, field);
			continue;
		}
		if (
			args.syncInFlight &&
			inFlightBytes != null &&
			!chunksEqual(incomingFields.get(field), inFlightFields.get(field))
		) {
			dirtyFieldNumbers.push(field);
		}
	}
	return {
		merged: preferencesFromBytes(merged),
		wire: preferencesFromBytes(wire),
		dirtyFields: toFieldNames(dirtyFieldNumbers),
	};
}

export function isEmptySyncedPreferencesEncoded(encoded: string | null | undefined): boolean {
	return !encoded;
}

export class SyncedPreferencesDecodeError extends Error {
	constructor(message: string) {
		super(`failed to decode synced_preferences: ${message}`);
		this.name = 'SyncedPreferencesDecodeError';
	}
}
