// SPDX-License-Identifier: AGPL-3.0-or-later

import {readExactPlainRecord} from '@electron/common/PlainRecord';
import {
	isNativeGatewayTransportConnectionId,
	NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES,
	NATIVE_GATEWAY_TRANSPORT_URL_MAX_BYTES,
	type NativeGatewayTransportCloseRequest,
	type NativeGatewayTransportCreateRequest,
	NativeGatewayTransportMode,
	type NativeGatewayTransportSendBinaryRequest,
	type NativeGatewayTransportSendTextRequest,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';

const CREATE_REQUEST_KEYS = Object.freeze(['connectionId', 'url', 'mode']);
const SEND_REQUEST_KEYS = Object.freeze(['connectionId', 'payload']);
const CLOSE_REQUEST_KEYS = Object.freeze(['connectionId', 'code', 'reason']);
const NORMAL_CLOSE_CODE = 1000;
const APPLICATION_CLOSE_CODE_MIN = 3000;
const APPLICATION_CLOSE_CODE_MAX = 4999;

class InvalidNativeGatewayTransportRequestError extends TypeError {
	public constructor(reason: string) {
		super(`Native gateway transport request is invalid: ${reason}`);
		this.name = 'InvalidNativeGatewayTransportRequestError';
	}
}

function requireRecord(value: unknown, expectedKeys: ReadonlyArray<string>, subject: string) {
	const record = readExactPlainRecord({value, expectedKeys});
	if (record == null) {
		throw new InvalidNativeGatewayTransportRequestError(`${subject} must be a plain object with exactly its own keys`);
	}
	return record;
}

export function requireNativeGatewayConnectionId(value: unknown): string {
	if (typeof value !== 'string') {
		throw new InvalidNativeGatewayTransportRequestError('connection id must be a string');
	}
	if (Buffer.byteLength(value) > NATIVE_GATEWAY_TRANSPORT_CONNECTION_ID_MAX_BYTES) {
		throw new InvalidNativeGatewayTransportRequestError('connection id has an invalid format');
	}
	if (!isNativeGatewayTransportConnectionId(value)) {
		throw new InvalidNativeGatewayTransportRequestError('connection id has an invalid format');
	}
	return value;
}

function requireNativeGatewayTransportURL(value: unknown): string {
	if (typeof value !== 'string' || value.length === 0) {
		throw new InvalidNativeGatewayTransportRequestError('URL must be a non-empty string');
	}
	if (Buffer.byteLength(value) > NATIVE_GATEWAY_TRANSPORT_URL_MAX_BYTES) {
		throw new InvalidNativeGatewayTransportRequestError('URL is too long');
	}
	let parsed: URL;
	try {
		parsed = new URL(value);
	} catch {
		throw new InvalidNativeGatewayTransportRequestError('URL must be absolute');
	}
	if (parsed.protocol !== 'ws:' && parsed.protocol !== 'wss:') {
		throw new InvalidNativeGatewayTransportRequestError('URL protocol must be ws: or wss:');
	}
	if (parsed.username !== '' || parsed.password !== '') {
		throw new InvalidNativeGatewayTransportRequestError('URL credentials are prohibited');
	}
	if (parsed.hash !== '') {
		throw new InvalidNativeGatewayTransportRequestError('URL fragment is prohibited');
	}
	return parsed.toString();
}

export function requireNativeGatewayCreateRequest(value: unknown): NativeGatewayTransportCreateRequest {
	const record = requireRecord(value, CREATE_REQUEST_KEYS, 'create request');
	if (record.mode !== NativeGatewayTransportMode.GATEWAY) {
		throw new InvalidNativeGatewayTransportRequestError('transport mode must be gateway');
	}
	return {
		connectionId: requireNativeGatewayConnectionId(record.connectionId),
		url: requireNativeGatewayTransportURL(record.url),
		mode: NativeGatewayTransportMode.GATEWAY,
	};
}

export function requireNativeGatewaySendTextRequest(value: unknown): NativeGatewayTransportSendTextRequest {
	const record = requireRecord(value, SEND_REQUEST_KEYS, 'text send request');
	if (typeof record.payload !== 'string') {
		throw new InvalidNativeGatewayTransportRequestError('text payload must be a string');
	}
	if (Buffer.byteLength(record.payload) > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
		throw new InvalidNativeGatewayTransportRequestError('text payload is too large');
	}
	return {connectionId: requireNativeGatewayConnectionId(record.connectionId), payload: record.payload};
}

function requireNativeGatewaySendBinaryPayload(value: unknown): Buffer {
	if (value instanceof ArrayBuffer) {
		if (value.byteLength > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
			throw new InvalidNativeGatewayTransportRequestError('binary payload is too large');
		}
		return Buffer.from(new Uint8Array(value));
	}
	if (ArrayBuffer.isView(value)) {
		if (value.byteLength > NATIVE_GATEWAY_TRANSPORT_FRAME_MAX_BYTES) {
			throw new InvalidNativeGatewayTransportRequestError('binary payload is too large');
		}
		return Buffer.from(new Uint8Array(value.buffer, value.byteOffset, value.byteLength));
	}
	throw new InvalidNativeGatewayTransportRequestError('binary payload must be a byte buffer');
}

export function requireNativeGatewaySendBinaryRequest(
	value: unknown,
): Omit<NativeGatewayTransportSendBinaryRequest, 'payload'> & {payload: Buffer} {
	const record = requireRecord(value, SEND_REQUEST_KEYS, 'binary send request');
	return {
		connectionId: requireNativeGatewayConnectionId(record.connectionId),
		payload: requireNativeGatewaySendBinaryPayload(record.payload),
	};
}

export function requireNativeGatewayCloseRequest(value: unknown): NativeGatewayTransportCloseRequest {
	const record = requireRecord(value, CLOSE_REQUEST_KEYS, 'close request');
	const {code} = record;
	if (typeof code !== 'number' || !Number.isInteger(code)) {
		throw new InvalidNativeGatewayTransportRequestError('close code must be an integer');
	}
	if (code !== NORMAL_CLOSE_CODE && (code < APPLICATION_CLOSE_CODE_MIN || code > APPLICATION_CLOSE_CODE_MAX)) {
		throw new InvalidNativeGatewayTransportRequestError('close code is not permitted');
	}
	if (typeof record.reason !== 'string') {
		throw new InvalidNativeGatewayTransportRequestError('close reason must be a string');
	}
	if (Buffer.byteLength(record.reason) > NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES) {
		throw new InvalidNativeGatewayTransportRequestError('close reason is too long');
	}
	return {connectionId: requireNativeGatewayConnectionId(record.connectionId), code, reason: record.reason};
}
