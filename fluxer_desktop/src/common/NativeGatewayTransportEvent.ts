// SPDX-License-Identifier: AGPL-3.0-or-later

import {readExactPlainRecord} from '@electron/common/PlainRecord';
import {
	isNativeGatewayTransportConnectionId,
	NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_ERROR_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES,
	type NativeGatewayTransportEvent,
	NativeGatewayTransportEventKind,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';

const EVENT_KEYS = Object.freeze(['connectionId', 'kind', 'data', 'binary', 'code', 'reason', 'wasClean', 'message']);
const CLOSE_CODE_MAX = 65_535;
const MAX_UTF8_BYTES_PER_UTF16_UNIT = 3;
const TEXT_ENCODER = new TextEncoder();

class InvalidNativeGatewayTransportEventError extends TypeError {
	public constructor(reason: string) {
		super(`Native gateway transport event is invalid: ${reason}`);
		this.name = 'InvalidNativeGatewayTransportEventError';
	}
}

function requireString(value: unknown, maxBytes: number, name: string): string {
	if (typeof value !== 'string') {
		throw new InvalidNativeGatewayTransportEventError(`${name} must be a string`);
	}
	if (value.length > maxBytes) {
		throw new InvalidNativeGatewayTransportEventError(`${name} is too large`);
	}
	if (value.length * MAX_UTF8_BYTES_PER_UTF16_UNIT > maxBytes && TEXT_ENCODER.encode(value).length > maxBytes) {
		throw new InvalidNativeGatewayTransportEventError(`${name} is too large`);
	}
	return value;
}

function requireNull(record: Readonly<Record<string, unknown>>, fields: ReadonlyArray<string>): void {
	for (const field of fields) {
		if (record[field] !== null) {
			throw new InvalidNativeGatewayTransportEventError(`${field} must be null for this event kind`);
		}
	}
}

function requireArrayBuffer(value: unknown): ArrayBuffer {
	if (value instanceof ArrayBuffer) {
		if (value.byteLength > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
			throw new InvalidNativeGatewayTransportEventError('binary frame data is too large');
		}
		return value;
	}
	if (ArrayBuffer.isView(value)) {
		if (value.byteLength > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
			throw new InvalidNativeGatewayTransportEventError('binary frame data is too large');
		}
		const copy = new Uint8Array(value.byteLength);
		copy.set(new Uint8Array(value.buffer, value.byteOffset, value.byteLength));
		return copy.buffer;
	}
	throw new InvalidNativeGatewayTransportEventError('binary frame data must be a byte buffer');
}

export function parseNativeGatewayTransportEvent(value: unknown, connectionId?: string): NativeGatewayTransportEvent {
	const record = readExactPlainRecord({
		value,
		expectedKeys: connectionId === undefined ? EVENT_KEYS : EVENT_KEYS.slice(1),
	});
	if (record == null) {
		throw new InvalidNativeGatewayTransportEventError('payload must be a plain object with exactly the declared keys');
	}
	if (connectionId === undefined) {
		connectionId = requireString(
			record.connectionId,
			NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_MAX_BYTES,
			'connection id',
		);
		if (!isNativeGatewayTransportConnectionId(connectionId)) {
			throw new InvalidNativeGatewayTransportEventError('connection id has an invalid format');
		}
	}
	const base = {connectionId, data: null, binary: null, code: null, reason: null, wasClean: null, message: null};
	switch (record.kind) {
		case NativeGatewayTransportEventKind.OPEN:
			requireNull(record, ['data', 'binary', 'code', 'reason', 'wasClean', 'message']);
			return {...base, kind: NativeGatewayTransportEventKind.OPEN};
		case NativeGatewayTransportEventKind.MESSAGE: {
			const data = requireString(record.data, NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES, 'message data');
			requireNull(record, ['binary', 'code', 'reason', 'wasClean', 'message']);
			return {...base, kind: NativeGatewayTransportEventKind.MESSAGE, data};
		}
		case NativeGatewayTransportEventKind.BINARY: {
			const binary = requireArrayBuffer(record.binary);
			requireNull(record, ['data', 'code', 'reason', 'wasClean', 'message']);
			return {...base, kind: NativeGatewayTransportEventKind.BINARY, binary};
		}
		case NativeGatewayTransportEventKind.CLOSE: {
			const {code} = record;
			if (typeof code !== 'number' || !Number.isInteger(code) || code < 0 || code > CLOSE_CODE_MAX) {
				throw new InvalidNativeGatewayTransportEventError('close code must be an unsigned 16-bit integer');
			}
			const reason = requireString(record.reason, NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES, 'close reason');
			if (typeof record.wasClean !== 'boolean') {
				throw new InvalidNativeGatewayTransportEventError('close disposition must be a boolean');
			}
			requireNull(record, ['data', 'binary', 'message']);
			return {...base, kind: NativeGatewayTransportEventKind.CLOSE, code, reason, wasClean: record.wasClean};
		}
		case NativeGatewayTransportEventKind.ERROR: {
			const message = requireString(record.message, NATIVE_GATEWAY_TRANSPORT_ERROR_MAX_BYTES, 'error message');
			requireNull(record, ['data', 'binary', 'code', 'reason', 'wasClean']);
			return {...base, kind: NativeGatewayTransportEventKind.ERROR, message};
		}
		default:
			throw new InvalidNativeGatewayTransportEventError('event kind is unknown');
	}
}
