// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {dirname, join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {GatewayCompression} from '@app/features/gateway/transport/GatewayCompression';
import {GatewayConnectionRole} from '@app/features/gateway/transport/GatewayConnectionRole';
import {
	type GatewayDispatchDelivery,
	type GatewayDispatchReceipt,
	GatewaySocket,
	type GatewaySocketOptions,
	type GatewaySocketProperties,
	GatewayState,
	MAX_DEFERRED_GATEWAY_EMIT_BYTES,
	MAX_DEFERRED_GATEWAY_EMITS,
} from '@app/features/gateway/transport/GatewaySocket';
import {GatewayCloseCodes} from '@fluxer/constants/src/GatewayConstants';
import {initSync} from '@pkgs/libfluxcore/libfluxcore';
import {afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const PROPERTIES: GatewaySocketProperties = {
	os: 'macos',
	browser: 'Fluxer Client',
	device: 'desktop',
	locale: 'en-US',
	user_agent: 'test',
	browser_version: '1',
	os_version: '1',
	build_version: '1',
};

interface SocketInternals {
	socket: {
		readyState: number;
		send: (data: string | Uint8Array) => void;
		close?: (code: number, reason: string) => void;
	} | null;
	payloadDecompressor: GatewayCompression | null;
	deferredEmitQueue: Array<() => void>;
	deferredEmitBytes: number;
	emitDispatchDeferred: (delivery: GatewayDispatchDelivery) => void;
	buildGatewayUrl: () => Promise<string>;
	openSocket: () => void;
	handleSocketMessage: (event: {data: string | ArrayBuffer}) => Promise<void>;
	handleSocketClose: (event: {code: number; reason: string; wasClean: boolean}) => void;
}

function internals(socket: GatewaySocket): SocketInternals {
	return socket as unknown as SocketInternals;
}

function createSocket(overrides: Partial<GatewaySocketOptions> = {}): GatewaySocket {
	return new GatewaySocket('wss://gateway.example/', {
		token: 'token-100',
		apiVersion: 9,
		properties: PROPERTIES,
		isMobileLayout: () => false,
		geo: () => ({latitude: null, longitude: null}),
		role: GatewayConnectionRole.FOREGROUND,
		...overrides,
	});
}

function attachOpenTransport(socket: GatewaySocket): Array<string> {
	const sent: Array<string> = [];
	internals(socket).socket = {
		readyState: WebSocket.OPEN,
		send: (data) => {
			sent.push(typeof data === 'string' ? data : new TextDecoder().decode(data));
		},
	};
	return sent;
}

function sequenceOf(data: unknown): number {
	return (data as {sequence: number}).sequence;
}

function enqueueDispatch(socket: GatewaySocket, sequence: number, retainedByteSize: number): void {
	internals(socket).emitDispatchDeferred({
		type: 'TYPING_START',
		data: {sequence},
		retainedByteSize,
		receipt: {sequence, generation: 0},
	});
}

function dispatchFrame(eventType: string, sequence: number, data: unknown): string {
	return JSON.stringify({op: 0, t: eventType, s: sequence, d: data});
}

beforeEach(() => {
	vi.useFakeTimers();
});

afterEach(() => {
	vi.useRealTimers();
});

describe('gateway url', () => {
	test('pins the deployed query string including stream=1', async () => {
		const url = await internals(createSocket()).buildGatewayUrl();

		expect(url).toBe('wss://gateway.example/?v=9&encoding=json&compress=zstd-stream&stream=1');
	});
});

describe('voice state payloads', () => {
	test('two sockets each stamp only their own connection id', () => {
		const first = createSocket();
		const second = createSocket();
		const firstSent = attachOpenTransport(first);
		const secondSent = attachOpenTransport(second);
		const params = {
			guild_id: 'guild-1',
			channel_id: 'channel-1',
			self_mute: false,
			self_deaf: false,
			self_video: false,
			self_stream: false,
		};

		first.updateVoiceStateExplicit({...params, connection_id: 'connection-first'});
		second.updateVoiceStateExplicit({...params, connection_id: null});

		expect(JSON.parse(firstSent[0]).d.connection_id).toBe('connection-first');
		expect(JSON.parse(secondSent[0]).d.connection_id).toBeNull();
	});

	test('stamps the injected mobile layout and geo instead of reading a global', () => {
		const socket = createSocket({
			isMobileLayout: () => true,
			geo: () => ({latitude: '1.5', longitude: '-2.5'}),
		});
		const sent = attachOpenTransport(socket);

		socket.updateVoiceStateExplicit({
			guild_id: null,
			channel_id: null,
			self_mute: true,
			self_deaf: true,
			self_video: false,
			self_stream: false,
			connection_id: null,
		});

		expect(JSON.parse(sent[0]).d).toMatchObject({is_mobile: true, latitude: '1.5', longitude: '-2.5'});
	});
});

describe('dispatch delivery', () => {
	test('carries the event, the retained byte size and an active receipt', async () => {
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));
		const frame = dispatchFrame('MESSAGE_CREATE', 7, {id: 'message-1'});

		await internals(socket).handleSocketMessage({data: frame});
		vi.advanceTimersByTime(5000);

		expect(deliveries).toHaveLength(1);
		expect(deliveries[0].type).toBe('MESSAGE_CREATE');
		expect(deliveries[0].data).toEqual({id: 'message-1'});
		expect(deliveries[0].retainedByteSize).toBe(frame.length * 3);
		expect(deliveries[0].receipt.sequence).toBe(7);
		expect(socket.isDispatchActive(deliveries[0].receipt)).toBe(true);
	});

	test('exposes the receipt of the dispatch being emitted and nothing outside it', async () => {
		const socket = createSocket();
		let observed: GatewayDispatchReceipt | null = null;
		socket.on('dispatch', (delivery) => {
			observed = socket.currentDispatchReceipt();
			expect(observed).toBe(delivery.receipt);
		});

		await internals(socket).handleSocketMessage({data: dispatchFrame('TYPING_START', 3, {})});
		vi.advanceTimersByTime(5000);

		expect(observed).not.toBeNull();
		expect(() => socket.currentDispatchReceipt()).toThrow();
	});

	test('completing a dispatch retires its receipt', async () => {
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		await internals(socket).handleSocketMessage({data: dispatchFrame('TYPING_START', 1, {})});
		vi.advanceTimersByTime(5000);
		socket.completeDispatchProcessing(deliveries[0].receipt);

		expect(socket.isDispatchActive(deliveries[0].receipt)).toBe(false);
	});

	test('failing a dispatch clears the session and reconnects with a fresh identify', async () => {
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		await internals(socket).handleSocketMessage({
			data: JSON.stringify({op: 0, t: 'READY', s: 1, d: {session_id: 'session-1'}}),
		});
		vi.advanceTimersByTime(0);
		expect(socket.getSessionId()).toBe('session-1');

		socket.failDispatchProcessing(deliveries[0].receipt, new Error('handler blew up'));

		expect(socket.getSessionId()).toBeNull();
		expect(socket.getState()).toBe(GatewayState.Reconnecting);
		expect(socket.isDispatchActive(deliveries[0].receipt)).toBe(false);
	});

	test('a receipt from a cleared session is no longer active', async () => {
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		await internals(socket).handleSocketMessage({data: dispatchFrame('TYPING_START', 4, {})});
		vi.advanceTimersByTime(5000);
		socket.reset(false);

		expect(socket.isDispatchActive(deliveries[0].receipt)).toBe(false);
	});
});

describe('dispatch failure reconnects', () => {
	beforeEach(() => {
		vi.spyOn(Math, 'random').mockReturnValue(0.5);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	function recordSocketOpens(socket: GatewaySocket): Array<number> {
		const opens: Array<number> = [];
		internals(socket).openSocket = () => {
			opens.push(Date.now());
		};
		return opens;
	}

	function advanceToNextSocketOpen(opens: ReadonlyArray<number>): void {
		const seen = opens.length;
		for (let step = 0; step < 10 && opens.length === seen; step += 1) {
			vi.advanceTimersToNextTimer();
		}
	}

	function readyFrame(sequence: number): string {
		return JSON.stringify({op: 0, t: 'READY', s: sequence, d: {session_id: `session-${sequence}`}});
	}

	test('a failed dispatch waits out the minimum reconnect delay instead of reopening at 0ms', async () => {
		const socket = createSocket();
		const opens = recordSocketOpens(socket);
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		await internals(socket).handleSocketMessage({data: readyFrame(1)});
		vi.advanceTimersByTime(0);
		socket.failDispatchProcessing(deliveries[0].receipt, new Error('handler blew up'));

		vi.advanceTimersByTime(999);
		expect(opens).toHaveLength(0);

		vi.advanceTimersByTime(1000);
		expect(opens).toHaveLength(1);
	});

	test('a dispatch failure that repeats on every READY escalates the reconnect delay', async () => {
		const socket = createSocket();
		const opens = recordSocketOpens(socket);
		socket.on('dispatch', (delivery) => {
			socket.failDispatchProcessing(delivery.receipt, new Error('handler blew up'));
		});
		const delays: Array<number> = [];

		for (let sequence = 1; sequence <= 4; sequence += 1) {
			const scheduledAt = Date.now();
			await internals(socket).handleSocketMessage({data: readyFrame(sequence)});
			advanceToNextSocketOpen(opens);
			delays.push(Date.now() - scheduledAt);
		}

		expect(opens).toHaveLength(4);
		expect(delays[0]).toBeGreaterThanOrEqual(1000);
		expect(delays[1]).toBeGreaterThan(delays[0]);
		expect(delays[2]).toBeGreaterThan(delays[1]);
		expect(delays[3]).toBeGreaterThan(delays[2]);
	});

	test('a dispatch that completes lets the next READY clear the backoff again', async () => {
		const socket = createSocket();
		const opens = recordSocketOpens(socket);
		let shouldFail = true;
		socket.on('dispatch', (delivery) => {
			if (shouldFail) {
				socket.failDispatchProcessing(delivery.receipt, new Error('handler blew up'));
			} else {
				socket.completeDispatchProcessing(delivery.receipt);
			}
		});

		await internals(socket).handleSocketMessage({data: readyFrame(1)});
		advanceToNextSocketOpen(opens);
		shouldFail = false;
		await internals(socket).handleSocketMessage({data: readyFrame(2)});
		vi.advanceTimersToNextTimer();
		shouldFail = true;
		const scheduledAt = Date.now();
		await internals(socket).handleSocketMessage({data: readyFrame(3)});
		advanceToNextSocketOpen(opens);

		expect(opens).toHaveLength(2);
		expect(Date.now() - scheduledAt).toBeLessThan(1500);
	});
});

describe('compression decode failure recovery', () => {
	beforeAll(() => {
		const wasmPath = join(dirname(fileURLToPath(import.meta.url)), '../../../../pkgs/libfluxcore/libfluxcore_bg.wasm');
		initSync({module: readFileSync(wasmPath)});
	});

	function attachRecordingTransport(socket: GatewaySocket): Array<{code: number; reason: string}> {
		const closes: Array<{code: number; reason: string}> = [];
		internals(socket).socket = {
			readyState: WebSocket.OPEN,
			send: () => {},
			close: (code, reason) => {
				closes.push({code, reason});
			},
		};
		internals(socket).payloadDecompressor = new GatewayCompression('zstd-stream', true);
		return closes;
	}

	function corruptZstdFrame(): ArrayBuffer {
		return new Uint8Array([0x6e, 0x6f, 0x70, 0x65]).buffer;
	}

	test('an undecodable frame closes with DECODE_ERROR and reopens without compression', async () => {
		const socket = createSocket();
		const closes = attachRecordingTransport(socket);

		await internals(socket).handleSocketMessage({data: corruptZstdFrame()});

		expect(closes).toEqual([{code: GatewayCloseCodes.DECODE_ERROR, reason: 'Retrying without compression'}]);
		expect(socket.getState()).toBe(GatewayState.Reconnecting);
		expect(await internals(socket).buildGatewayUrl()).toBe('wss://gateway.example/?v=9&encoding=json&compress=none');
	});

	test('a second undecodable frame before the reconnect neither closes again nor reports a fatal error', async () => {
		const socket = createSocket();
		const closes = attachRecordingTransport(socket);
		const fatalErrors: Array<Error> = [];
		socket.on('fatalError', (error) => fatalErrors.push(error));

		await internals(socket).handleSocketMessage({data: corruptZstdFrame()});
		await internals(socket).handleSocketMessage({data: corruptZstdFrame()});
		vi.advanceTimersByTime(0);

		expect(closes).toHaveLength(1);
		expect(fatalErrors).toEqual([]);
	});
});

describe('deferred emit queue bounds', () => {
	test('2000 undrained dispatches never grow the queue past the entry cap and never lose an event', () => {
		const socket = createSocket();
		const received: Array<number> = [];
		socket.on('dispatch', (delivery) => received.push(sequenceOf(delivery.data)));
		let observedMaxLength = 0;

		for (let sequence = 1; sequence <= 2000; sequence += 1) {
			enqueueDispatch(socket, sequence, 0);
			observedMaxLength = Math.max(observedMaxLength, internals(socket).deferredEmitQueue.length);
		}
		vi.advanceTimersByTime(5000);

		expect(observedMaxLength).toBe(MAX_DEFERRED_GATEWAY_EMITS);
		expect(received).toHaveLength(2000);
		expect(received[0]).toBe(1);
		expect(received[1999]).toBe(2000);
	});

	test('a burst of large payloads never grows the queue past the byte cap', () => {
		const socket = createSocket();
		const received: Array<number> = [];
		socket.on('dispatch', (delivery) => received.push(sequenceOf(delivery.data)));
		const byteSize = 4_500_000;
		let observedMaxBytes = 0;

		for (let sequence = 1; sequence <= 40; sequence += 1) {
			enqueueDispatch(socket, sequence, byteSize);
			observedMaxBytes = Math.max(observedMaxBytes, internals(socket).deferredEmitBytes);
		}
		vi.advanceTimersByTime(5000);

		expect(observedMaxBytes).toBeGreaterThan(0);
		expect(observedMaxBytes).toBeLessThanOrEqual(MAX_DEFERRED_GATEWAY_EMIT_BYTES);
		expect(received).toHaveLength(40);
	});

	test('a throwing listener does not drop the rest of the flush', () => {
		const socket = createSocket();
		const received: Array<number> = [];
		socket.on('dispatch', (delivery) => {
			const sequence = sequenceOf(delivery.data);
			if (sequence === 1) throw new Error('listener blew up');
			received.push(sequence);
		});

		for (let sequence = 1; sequence <= 3; sequence += 1) {
			enqueueDispatch(socket, sequence, 0);
		}
		vi.advanceTimersByTime(5000);

		expect(received).toEqual([2, 3]);
	});

	test('reset discards queued emits', () => {
		const socket = createSocket();
		const received: Array<number> = [];
		socket.on('dispatch', (delivery) => received.push(sequenceOf(delivery.data)));

		enqueueDispatch(socket, 1, 128);
		socket.reset(false);
		vi.advanceTimersByTime(5000);

		expect(received).toEqual([]);
		expect(internals(socket).deferredEmitQueue).toHaveLength(0);
		expect(internals(socket).deferredEmitBytes).toBe(0);
	});
});

describe('authentication failure', () => {
	test('a 4004 close only disconnects and reports it to the owner', () => {
		const socket = createSocket();
		const disconnects: Array<{code: number}> = [];
		socket.on('disconnect', (event) => disconnects.push(event));

		internals(socket).handleSocketClose({code: 4004, reason: 'Authentication failed', wasClean: false});
		vi.advanceTimersByTime(0);

		expect(disconnects).toEqual([{code: 4004, reason: 'Authentication failed', wasClean: false}]);
		expect(socket.getState()).toBe(GatewayState.Disconnected);
	});

	test('a non-4004 close still schedules a reconnect', () => {
		const socket = createSocket();

		internals(socket).handleSocketClose({code: 1006, reason: 'Abnormal closure', wasClean: false});

		expect(socket.getState()).toBe(GatewayState.Reconnecting);
	});
});
