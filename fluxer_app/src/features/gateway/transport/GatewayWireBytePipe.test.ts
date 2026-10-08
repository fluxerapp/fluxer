// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {resolve} from 'node:path';
import {constants, createZstdCompress, type ZstdCompress} from 'node:zlib';
import {GatewayConnectionRole} from '@app/features/gateway/transport/GatewayConnectionRole';
import {
	type GatewayDispatchDelivery,
	GatewaySocket,
	type GatewaySocketProperties,
} from '@app/features/gateway/transport/GatewaySocket';
import type {
	NativeGatewayTransportAPI,
	NativeGatewayTransportEvent,
	NativeGatewayTransportEventKind,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';
import {initSync} from '@pkgs/libfluxcore/libfluxcore';
import {afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/platform/DesktopLocalAppRuntime')>()),
	isDesktopLocalAppDocument: vi.fn(() => true),
}));

const wasmBytes = readFileSync(resolve(process.cwd(), 'pkgs/libfluxcore/libfluxcore_bg.wasm'));

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

type SocketEvent = {data?: unknown; code?: number; reason?: string; wasClean?: boolean};

class FakeWebSocket {
	static readonly CONNECTING = 0;
	static readonly OPEN = 1;
	static readonly CLOSING = 2;
	static readonly CLOSED = 3;
	static instances: Array<FakeWebSocket> = [];

	readyState: number = FakeWebSocket.CONNECTING;
	binaryType = 'blob';
	readonly closeCalls: Array<{code: number; reason: string}> = [];
	private readonly listeners = new Map<string, Set<(event: SocketEvent) => void>>();

	constructor(readonly url: string) {
		FakeWebSocket.instances.push(this);
	}

	addEventListener(type: string, callback: (event: SocketEvent) => void): void {
		const existing = this.listeners.get(type) ?? new Set();
		existing.add(callback);
		this.listeners.set(type, existing);
	}

	removeEventListener(type: string, callback: (event: SocketEvent) => void): void {
		this.listeners.get(type)?.delete(callback);
	}

	send(): void {}

	close(code: number, reason: string): void {
		this.closeCalls.push({code, reason});
		this.readyState = FakeWebSocket.CLOSED;
	}

	dispatch(type: string, event: SocketEvent = {}): void {
		for (const callback of [...(this.listeners.get(type) ?? [])]) {
			callback(event);
		}
	}

	openSocket(): void {
		this.readyState = FakeWebSocket.OPEN;
		this.dispatch('open');
	}
}

function gatewayJsonFrame(sequence: number): string {
	return JSON.stringify({
		op: 0,
		t: 'MESSAGE_CREATE',
		s: sequence,
		d: {id: String(sequence), content: `byte pipe payload ${sequence}`},
	});
}

function compressAndFlushChunk(encoder: ZstdCompress, payload: string): Promise<Uint8Array> {
	return new Promise((resolve, reject) => {
		const chunks: Array<Buffer> = [];
		const onData = (chunk: Buffer): void => {
			chunks.push(Buffer.from(chunk));
		};
		const cleanup = (): void => {
			encoder.off('data', onData);
			encoder.off('error', onError);
		};
		function onError(error: Error): void {
			cleanup();
			reject(error);
		}
		encoder.on('data', onData);
		encoder.once('error', onError);
		encoder.write(Buffer.from(payload), (error?: Error | null) => {
			if (error) {
				onError(error);
				return;
			}
			encoder.flush(constants.ZSTD_e_flush, () => {
				cleanup();
				resolve(new Uint8Array(Buffer.concat(chunks)));
			});
		});
	});
}

function toArrayBuffer(bytes: Uint8Array): ArrayBuffer {
	const copy = new Uint8Array(bytes.byteLength);
	copy.set(bytes);
	return copy.buffer;
}

function createDeployedGatewayEncoder(): ZstdCompress {
	return createZstdCompress({params: {[constants.ZSTD_c_compressionLevel]: 3}});
}

function createSocket(): GatewaySocket {
	return new GatewaySocket('wss://gateway.example/', {
		token: 'token-100',
		apiVersion: 9,
		properties: PROPERTIES,
		isMobileLayout: () => false,
		geo: () => ({latitude: null, longitude: null}),
		role: GatewayConnectionRole.FOREGROUND,
	});
}

async function waitFor(predicate: () => boolean): Promise<void> {
	for (let attempt = 0; attempt < 400; attempt += 1) {
		if (predicate()) return;
		await new Promise((resolve) => setTimeout(resolve, 5));
	}
	throw new Error('timed out waiting for the gateway socket');
}

function nativeEvent(
	connectionId: string,
	kind: NativeGatewayTransportEventKind,
	overrides: Partial<NativeGatewayTransportEvent> = {},
): NativeGatewayTransportEvent {
	return {
		connectionId,
		kind,
		data: null,
		binary: null,
		code: null,
		reason: null,
		wasClean: null,
		message: null,
		...overrides,
	};
}

interface NativeStub {
	api: NativeGatewayTransportAPI;
	connectionIds: Array<string>;
	emit(event: NativeGatewayTransportEvent): void;
}

function createNativeStub(): NativeStub {
	const listeners = new Set<(event: NativeGatewayTransportEvent) => void>();
	const connectionIds: Array<string> = [];
	return {
		api: {
			isAvailable: true,
			create: async ({connectionId}) => {
				connectionIds.push(connectionId);
				return {connectionId};
			},
			sendText: async () => undefined,
			sendBinary: async () => undefined,
			close: async () => undefined,
			dispose: async () => undefined,
			onEvent: (callback) => {
				listeners.add(callback);
				return () => listeners.delete(callback);
			},
		},
		connectionIds,
		emit: (event) => {
			for (const callback of [...listeners]) {
				callback(event);
			}
		},
	};
}

let originalWebSocket: unknown;

beforeAll(() => {
	initSync({module: wasmBytes});
});

beforeEach(() => {
	originalWebSocket = Reflect.get(globalThis, 'WebSocket');
	Reflect.set(globalThis, 'WebSocket', FakeWebSocket);
	FakeWebSocket.instances = [];
});

afterEach(() => {
	Reflect.set(globalThis, 'WebSocket', originalWebSocket);
	Reflect.deleteProperty(window, 'electron');
	Reflect.deleteProperty(globalThis, '__fluxerDesktopGatewayTransport');
});

describe('deployed zstd-stream frames decode over both transports', () => {
	test('the browser transport delivers a level-3 unconstrained-window frame to the renderer codec', async () => {
		const encoder = createDeployedGatewayEncoder();
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		try {
			socket.connect();
			await waitFor(() => FakeWebSocket.instances.length === 1);
			const wire = FakeWebSocket.instances[0];
			wire.openSocket();

			for (let sequence = 1; sequence <= 3; sequence += 1) {
				const compressed = await compressAndFlushChunk(encoder, gatewayJsonFrame(sequence));
				wire.dispatch('message', {data: toArrayBuffer(compressed)});
			}
			await waitFor(() => deliveries.length === 3);

			expect(wire.url).toBe('wss://gateway.example/?v=9&encoding=json&compress=zstd-stream&stream=1');
			expect(wire.binaryType).toBe('arraybuffer');
			expect(deliveries.map((delivery) => delivery.receipt.sequence)).toEqual([1, 2, 3]);
			expect(deliveries.map((delivery) => (delivery.data as {content: string}).content)).toEqual([
				'byte pipe payload 1',
				'byte pipe payload 2',
				'byte pipe payload 3',
			]);
		} finally {
			socket.reset(false);
			encoder.destroy();
		}
	});

	test('the desktop native transport forwards the same frame verbatim and the renderer still decodes it', async () => {
		const encoder = createDeployedGatewayEncoder();
		const stub = createNativeStub();
		Reflect.set(window, 'electron', {nativeGatewayTransport: stub.api});
		const socket = createSocket();
		const deliveries: Array<GatewayDispatchDelivery> = [];
		socket.on('dispatch', (delivery) => deliveries.push(delivery));

		try {
			socket.connect();
			await waitFor(() => stub.connectionIds.length === 1);
			const connectionId = stub.connectionIds[0];
			stub.emit(nativeEvent(connectionId, 'open'));

			for (let sequence = 1; sequence <= 3; sequence += 1) {
				const compressed = await compressAndFlushChunk(encoder, gatewayJsonFrame(sequence));
				stub.emit(nativeEvent(connectionId, 'binary', {binary: toArrayBuffer(compressed)}));
			}
			await waitFor(() => deliveries.length === 3);

			expect(FakeWebSocket.instances).toHaveLength(0);
			expect(deliveries.map((delivery) => delivery.receipt.sequence)).toEqual([1, 2, 3]);
			expect(deliveries.map((delivery) => (delivery.data as {content: string}).content)).toEqual([
				'byte pipe payload 1',
				'byte pipe payload 2',
				'byte pipe payload 3',
			]);
		} finally {
			socket.reset(false);
			encoder.destroy();
		}
	});
});

describe('web inertness', () => {
	test('with no electron API a full account switch keeps exactly one live WebSocket', async () => {
		const firstAccount = createSocket();
		const secondAccount = createSocket();

		try {
			firstAccount.connect();
			await waitFor(() => FakeWebSocket.instances.length === 1);
			FakeWebSocket.instances[0].openSocket();

			firstAccount.reset(false);
			secondAccount.connect();
			await waitFor(() => FakeWebSocket.instances.length === 2);
			FakeWebSocket.instances[1].openSocket();

			const live = FakeWebSocket.instances.filter((wire) => wire.readyState !== FakeWebSocket.CLOSED);
			expect(live).toEqual([FakeWebSocket.instances[1]]);
			expect(FakeWebSocket.instances[0].closeCalls).toEqual([{code: 1000, reason: 'Disposing stale socket'}]);
			expect(Reflect.get(globalThis, '__fluxerDesktopGatewayTransport')).toBeUndefined();
		} finally {
			secondAccount.reset(false);
		}
	});
});
