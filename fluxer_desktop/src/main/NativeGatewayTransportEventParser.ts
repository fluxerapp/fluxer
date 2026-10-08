// SPDX-License-Identifier: AGPL-3.0-or-later

import {readExactPlainRecord} from '@electron/common/PlainRecord';
import {
	NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_ERROR_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES,
	type NativeGatewayTransportEvent,
	NativeGatewayTransportEventKind,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';

const NATIVE_GATEWAY_EVENT_KEYS = Object.freeze(['kind', 'data', 'binary', 'code', 'reason', 'wasClean', 'message']);
const OPEN_NULL_FIELDS = Object.freeze(['data', 'binary', 'code', 'reason', 'wasClean', 'message']);
const MESSAGE_NULL_FIELDS = Object.freeze(['binary', 'code', 'reason', 'wasClean', 'message']);
const BINARY_NULL_FIELDS = Object.freeze(['data', 'code', 'reason', 'wasClean', 'message']);
const CLOSE_NULL_FIELDS = Object.freeze(['data', 'binary', 'message']);
const ERROR_NULL_FIELDS = Object.freeze(['data', 'binary', 'code', 'reason', 'wasClean']);
const CLOSE_CODE_MAX = 65_535;

class InvalidNativeGatewayTransportEventError extends TypeError {
	public constructor(reason: string) {
		super(`Native gateway transport event is invalid: ${reason}`);
		this.name = 'InvalidNativeGatewayTransportEventError';
	}
}

function requireNullFields(record: Readonly<Record<string, unknown>>, fields: ReadonlyArray<string>): void {
	for (const field of fields) {
		if (record[field] !== null) {
			throw new InvalidNativeGatewayTransportEventError(`${field} must be null for this event kind`);
		}
	}
}

function requireBoundedString(value: unknown, maxBytes: number, name: string): string {
	if (typeof value !== 'string') {
		throw new InvalidNativeGatewayTransportEventError(`${name} must be a string`);
	}
	if (Buffer.byteLength(value) > maxBytes) {
		throw new InvalidNativeGatewayTransportEventError(`${name} is too large`);
	}
	return value;
}

function requireCloseCode(value: unknown): number {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < 0 || value > CLOSE_CODE_MAX) {
		throw new InvalidNativeGatewayTransportEventError('close code must be an unsigned 16-bit integer');
	}
	return value;
}

function toArrayBuffer(value: Buffer): ArrayBuffer {
	const copy = new Uint8Array(value.byteLength);
	copy.set(value);
	return copy.buffer;
}

export function parseNativeGatewayTransportEvent(connectionId: string, event: unknown): NativeGatewayTransportEvent {
	const record = readExactPlainRecord({value: event, expectedKeys: NATIVE_GATEWAY_EVENT_KEYS});
	if (record == null) {
		throw new InvalidNativeGatewayTransportEventError('payload must be a plain object with exactly the declared keys');
	}
	const base = {connectionId, data: null, binary: null, code: null, reason: null, wasClean: null, message: null};
	switch (record.kind) {
		case NativeGatewayTransportEventKind.OPEN:
			requireNullFields(record, OPEN_NULL_FIELDS);
			return {...base, kind: NativeGatewayTransportEventKind.OPEN};
		case NativeGatewayTransportEventKind.MESSAGE: {
			const data = requireBoundedString(record.data, NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES, 'message data');
			requireNullFields(record, MESSAGE_NULL_FIELDS);
			return {...base, kind: NativeGatewayTransportEventKind.MESSAGE, data};
		}
		case NativeGatewayTransportEventKind.BINARY: {
			if (!Buffer.isBuffer(record.binary)) {
				throw new InvalidNativeGatewayTransportEventError('binary frame data must be a Buffer');
			}
			if (record.binary.byteLength > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
				throw new InvalidNativeGatewayTransportEventError('binary frame data is too large');
			}
			requireNullFields(record, BINARY_NULL_FIELDS);
			return {...base, kind: NativeGatewayTransportEventKind.BINARY, binary: toArrayBuffer(record.binary)};
		}
		case NativeGatewayTransportEventKind.CLOSE: {
			const code = requireCloseCode(record.code);
			const reason = requireBoundedString(
				record.reason,
				NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES,
				'close reason',
			);
			if (typeof record.wasClean !== 'boolean') {
				throw new InvalidNativeGatewayTransportEventError('close disposition must be a boolean');
			}
			requireNullFields(record, CLOSE_NULL_FIELDS);
			return {...base, kind: NativeGatewayTransportEventKind.CLOSE, code, reason, wasClean: record.wasClean};
		}
		case NativeGatewayTransportEventKind.ERROR: {
			const message = requireBoundedString(record.message, NATIVE_GATEWAY_TRANSPORT_ERROR_MAX_BYTES, 'error message');
			requireNullFields(record, ERROR_NULL_FIELDS);
			return {...base, kind: NativeGatewayTransportEventKind.ERROR, message};
		}
		default:
			throw new InvalidNativeGatewayTransportEventError('event kind is unknown');
	}
}
