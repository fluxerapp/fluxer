// SPDX-License-Identifier: AGPL-3.0-or-later

import {create, fromBinary, toBinary} from '@bufbuild/protobuf';
import {timestampDate, timestampFromDate} from '@bufbuild/protobuf/wkt';
import {type ReadStateBundle, ReadStateBundleSchema} from '@fluxer/schema/src/gen/fluxer/read_state/v1/read_state_pb';
import {base64ToUint8Array, uint8ArrayToBase64} from 'uint8array-extras';

interface ReadStateProtoEntryInput {
	id: string;
	mention_count?: number | null;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
	version?: string | null;
}

interface ReadStateProtoEntry {
	id: string;
	mention_count: number;
	last_message_id: string | null;
	last_pin_timestamp: string | null;
	version?: string;
}

export const EMPTY_READ_STATE_PROTO = '';
const MAX_UINT32 = 0xffffffff;
const MAX_UINT64 = 0xffffffffffffffffn;

export function encodeReadStateProto(readStates: ReadonlyArray<ReadStateProtoEntryInput>): string {
	const bundle = create(ReadStateBundleSchema, {
		readStates: readStates.map((readState) => ({
			channelId: parseUint64(readState.id, 'id'),
			lastMessageId: optionalUint64(readState.last_message_id, 'last_message_id'),
			mentionCount: normalizeMentionCount(readState.mention_count),
			lastPinTimestamp: optionalTimestamp(readState.last_pin_timestamp),
			version: optionalUint64(readState.version, 'version'),
		})),
	});
	const bytes = toBinary(ReadStateBundleSchema, bundle);
	if (bytes.byteLength === 0) return EMPTY_READ_STATE_PROTO;
	return uint8ArrayToBase64(bytes);
}

export function decodeReadStateProto(encoded: string | null | undefined): Array<ReadStateProtoEntry> {
	const bundle = decodeReadStateProtoBundle(encoded);
	return bundle.readStates.map((readState) => ({
		id: readState.channelId.toString(),
		mention_count: normalizeMentionCount(readState.mentionCount),
		last_message_id: readState.lastMessageId?.toString() ?? null,
		last_pin_timestamp: readState.lastPinTimestamp ? timestampDate(readState.lastPinTimestamp).toISOString() : null,
		version: readState.version?.toString(),
	}));
}

export function decodeReadStateProtoBundle(encoded: string | null | undefined): ReadStateBundle {
	if (!encoded) return create(ReadStateBundleSchema);
	let bytes: Uint8Array;
	try {
		bytes = base64ToUint8Array(encoded);
	} catch (error) {
		throw new ReadStateProtoDecodeError(error instanceof Error ? `invalid base64: ${error.message}` : 'invalid base64');
	}
	if (bytes.byteLength === 0) return create(ReadStateBundleSchema);
	try {
		return fromBinary(ReadStateBundleSchema, bytes);
	} catch (error) {
		throw new ReadStateProtoDecodeError(error instanceof Error ? error.message : 'invalid read-state protobuf');
	}
}

export class ReadStateProtoDecodeError extends Error {
	constructor(message: string) {
		super(`failed to decode read_state_proto: ${message}`);
		this.name = 'ReadStateProtoDecodeError';
	}
}

class ReadStateProtoEncodeError extends Error {
	constructor(message: string) {
		super(`failed to encode read_state_proto: ${message}`);
		this.name = 'ReadStateProtoEncodeError';
	}
}

function optionalUint64(value: string | null | undefined, field: string): bigint | undefined {
	return value == null ? undefined : parseUint64(value, field);
}

function parseUint64(value: string, field: string): bigint {
	try {
		const parsed = BigInt(value);
		if (parsed < 0n || parsed > MAX_UINT64) {
			throw new Error('out of uint64 range');
		}
		return parsed;
	} catch (error) {
		throw new ReadStateProtoEncodeError(
			error instanceof Error ? `${field}: ${error.message}` : `${field}: invalid uint64`,
		);
	}
}

function optionalTimestamp(value: string | null | undefined) {
	if (value == null) return undefined;
	const date = new Date(value);
	if (Number.isNaN(date.getTime())) {
		throw new ReadStateProtoEncodeError('last_pin_timestamp: invalid timestamp');
	}
	return timestampFromDate(date);
}

function normalizeMentionCount(value: number | null | undefined): number {
	if (value == null || !Number.isFinite(value)) return 0;
	return Math.min(MAX_UINT32, Math.max(0, Math.floor(value)));
}
