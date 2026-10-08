// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import '../main/LocalAppTestSupport.test.mjs';

const {NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES, NativeGatewayTransportEventKind} = await import(
	'../../../packages/desktop_ipc/src/GatewayTransportContract.ts'
);
const {reconstructNativeGatewayTransportEvent} = await import('./PreloadNativeGatewayEvent.ts');

const CONNECTION_ID = 'gateway-renderer-3f2b1c4d-5e6f-4a7b-8c9d-0e1f2a3b4c5d:9a8b7c6d-5e4f-4a3b-9c2d-1e0f9a8b7c6d';

function frame(overrides) {
	return {
		connectionId: CONNECTION_ID,
		kind: NativeGatewayTransportEventKind.OPEN,
		data: null,
		binary: null,
		code: null,
		reason: null,
		wasClean: null,
		message: null,
		...overrides,
	};
}

function closeFrame(reason) {
	return frame({kind: NativeGatewayTransportEventKind.CLOSE, code: 1000, reason, wasClean: true});
}

function rejects(value, fragment) {
	assert.throws(
		() => reconstructNativeGatewayTransportEvent(value),
		(error) => error.name === 'InvalidPreloadNativeGatewayEventError' && error.message.includes(fragment),
		`the preload must refuse this event with ${fragment}`,
	);
}

describe('reconstructing a native gateway transport event in the preload', () => {
	test('each event kind keeps its own payload and nulls the rest', () => {
		const opened = reconstructNativeGatewayTransportEvent(frame({}));
		assert.deepEqual(opened, {
			connectionId: CONNECTION_ID,
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});

		const message = reconstructNativeGatewayTransportEvent(
			frame({kind: NativeGatewayTransportEventKind.MESSAGE, data: '{"op":11}'}),
		);
		assert.equal(message.data, '{"op":11}');
		assert.equal(message.binary, null);

		const closed = reconstructNativeGatewayTransportEvent(closeFrame('going away'));
		assert.equal(closed.code, 1000);
		assert.equal(closed.reason, 'going away');
		assert.equal(closed.wasClean, true);

		const errored = reconstructNativeGatewayTransportEvent(
			frame({kind: NativeGatewayTransportEventKind.ERROR, message: 'socket died'}),
		);
		assert.equal(errored.message, 'socket died');
	});

	test('a binary frame is copied out of a view so the sender cannot mutate it afterwards', () => {
		const view = new Uint8Array([1, 2, 3, 4]);
		const event = reconstructNativeGatewayTransportEvent(
			frame({kind: NativeGatewayTransportEventKind.BINARY, binary: view}),
		);
		view[0] = 99;
		assert.deepEqual(Array.from(new Uint8Array(event.binary)), [1, 2, 3, 4]);
	});

	test('a payload that is not exactly the declared shape is refused', () => {
		rejects(null, 'plain object');
		rejects([], 'plain object');
		rejects({...frame({}), extra: 1}, 'plain object');
		const missing = frame({});
		delete missing.message;
		rejects(missing, 'plain object');
		rejects(frame({kind: 'sneak'}), 'event kind is unknown');
		rejects(frame({connectionId: 'gateway-renderer-nope'}), 'connection id has an invalid format');
		rejects(frame({connectionId: 42}), 'connection id must be a string');
	});

	test('a field that belongs to another kind must still be null', () => {
		rejects(frame({data: 'stowaway'}), 'data must be null');
		rejects(
			frame({kind: NativeGatewayTransportEventKind.MESSAGE, data: 'hi', message: 'stowaway'}),
			'message must be null',
		);
		rejects({...closeFrame('bye'), binary: new ArrayBuffer(1)}, 'binary must be null');
	});

	test('a close frame validates its code and disposition', () => {
		rejects(
			frame({kind: NativeGatewayTransportEventKind.CLOSE, code: 1000.5, reason: 'x', wasClean: true}),
			'close code',
		);
		rejects(frame({kind: NativeGatewayTransportEventKind.CLOSE, code: -1, reason: 'x', wasClean: true}), 'close code');
		rejects(
			frame({kind: NativeGatewayTransportEventKind.CLOSE, code: 65_536, reason: 'x', wasClean: true}),
			'close code',
		);
		rejects(
			frame({kind: NativeGatewayTransportEventKind.CLOSE, code: 1000, reason: 'x', wasClean: 'yes'}),
			'disposition',
		);
	});

	test('the size cap counts utf-8 bytes, not utf-16 units', () => {
		const cap = NATIVE_GATEWAY_TRANSPORT_CLOSE_REASON_MAX_BYTES;
		assert.equal(cap, 123);

		const asciiAtCap = 'a'.repeat(cap);
		assert.equal(reconstructNativeGatewayTransportEvent(closeFrame(asciiAtCap)).reason, asciiAtCap);
		rejects(closeFrame('a'.repeat(cap + 1)), 'close reason is too large');

		const threeByteAtCap = '€'.repeat(cap / 3);
		assert.equal(reconstructNativeGatewayTransportEvent(closeFrame(threeByteAtCap)).reason, threeByteAtCap);
		rejects(closeFrame('€'.repeat(cap / 3 + 1)), 'close reason is too large');

		const astral = '\u{1f600}'.repeat(30);
		assert.equal(new TextEncoder().encode(astral).length, 120);
		assert.equal(astral.length, 60);
		assert.equal(reconstructNativeGatewayTransportEvent(closeFrame(astral)).reason, astral);
		rejects(closeFrame('\u{1f600}'.repeat(31)), 'close reason is too large');
	});
});
