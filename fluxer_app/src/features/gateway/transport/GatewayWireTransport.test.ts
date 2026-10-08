// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGatewayWireTransport,
	GatewayWireReadyState,
	GatewayWireTransportKind,
	InvalidDesktopGatewayTransportRouterStateError,
	isDesktopNativeGatewayTransportAvailable,
} from '@app/features/gateway/transport/GatewayWireTransport';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {
	NATIVE_GATEWAY_TRANSPORT_PROXIED_MESSAGE,
	type NativeGatewayTransportAPI,
	type NativeGatewayTransportEvent,
} from '@fluxer/desktop_ipc/src/GatewayTransportContract';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/platform/DesktopLocalAppRuntime', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/platform/DesktopLocalAppRuntime')>()),
	isDesktopLocalAppDocument: vi.fn(() => true),
}));

const ROUTER_GLOBAL_KEY = '__fluxerDesktopGatewayTransport';

type SocketEvent = {data?: unknown; code?: number; reason?: string; wasClean?: boolean};

class FakeWebSocket {
	static readonly CONNECTING = 0;
	static readonly OPEN = 1;
	static readonly CLOSING = 2;
	static readonly CLOSED = 3;
	static instances: Array<FakeWebSocket> = [];

	readyState: number = FakeWebSocket.CONNECTING;
	binaryType = 'blob';
	readonly sent: Array<string | ArrayBuffer> = [];
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

	send(data: string | ArrayBuffer): void {
		this.sent.push(data);
	}

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

interface NativeGatewayTransportStub {
	api: NativeGatewayTransportAPI;
	createdConnectionIds: Array<string>;
	disposedConnectionIds: Array<string>;
	sentBinary: Array<{connectionId: string; byteLength: number}>;
	sentText: Array<{connectionId: string; payload: string}>;
	emit(event: NativeGatewayTransportEvent): void;
	listenerCount(): number;
}

function createNativeGatewayTransportStub(
	overrides: Partial<NativeGatewayTransportAPI> = {},
): NativeGatewayTransportStub {
	const listeners = new Set<(event: NativeGatewayTransportEvent) => void>();
	const createdConnectionIds: Array<string> = [];
	const disposedConnectionIds: Array<string> = [];
	const sentBinary: Array<{connectionId: string; byteLength: number}> = [];
	const sentText: Array<{connectionId: string; payload: string}> = [];
	const api: NativeGatewayTransportAPI = {
		isAvailable: true,
		create: async ({connectionId}) => {
			createdConnectionIds.push(connectionId);
			return {connectionId};
		},
		sendText: async ({connectionId, payload}) => {
			sentText.push({connectionId, payload});
		},
		sendBinary: async ({connectionId, payload}) => {
			sentBinary.push({connectionId, byteLength: payload.byteLength});
		},
		close: async () => undefined,
		dispose: async (connectionId) => {
			disposedConnectionIds.push(connectionId);
		},
		onEvent: (callback) => {
			listeners.add(callback);
			return () => listeners.delete(callback);
		},
		...overrides,
	};
	return {
		api,
		createdConnectionIds,
		disposedConnectionIds,
		sentBinary,
		sentText,
		emit: (event) => {
			for (const callback of [...listeners]) {
				callback(event);
			}
		},
		listenerCount: () => listeners.size,
	};
}

function installElectronAPI(nativeGatewayTransport: unknown): void {
	Reflect.set(window, 'electron', {nativeGatewayTransport});
}

function flush(): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, 0));
}

let originalWebSocket: unknown;

beforeEach(() => {
	originalWebSocket = Reflect.get(globalThis, 'WebSocket');
	Reflect.set(globalThis, 'WebSocket', FakeWebSocket);
	FakeWebSocket.instances = [];
});

afterEach(() => {
	Reflect.set(globalThis, 'WebSocket', originalWebSocket);
	Reflect.deleteProperty(window, 'electron');
	Reflect.deleteProperty(globalThis, ROUTER_GLOBAL_KEY);
});

describe('transport selection', () => {
	test('a web document with no electron API gets the browser transport', () => {
		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
		expect(Reflect.get(globalThis, ROUTER_GLOBAL_KEY)).toBeUndefined();
	});

	test('a desktop shell whose addon failed to load degrades to the browser transport', () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI({...stub.api, isAvailable: false});

		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
	});

	test('a desktop shell that predates the transport namespace degrades to the browser transport', () => {
		Reflect.set(window, 'electron', {});

		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
	});

	test('a shell missing any one transport method degrades to the browser transport', () => {
		const stub = createNativeGatewayTransportStub();
		const {sendBinary: _dropped, ...partial} = stub.api;
		installElectronAPI({...partial, isAvailable: true});

		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
	});

	test('an available addon gets the desktop native transport', () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);

		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.DESKTOP_NATIVE);
	});

	test('a non-local-app document (e.g. a bounced web login) never uses the native transport', () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		vi.mocked(isDesktopLocalAppDocument).mockReturnValueOnce(false);

		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});

		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
		expect(stub.createdConnectionIds).toEqual([]);
	});

	test('a corrupted router global throws a typed error', () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		Reflect.set(globalThis, ROUTER_GLOBAL_KEY, {rendererRouterId: 'not-a-uuid', api: null, router: null});

		expect(() => createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'})).toThrow(
			InvalidDesktopGatewayTransportRouterStateError,
		);
	});
});

describe('browser transport', () => {
	test('dispose closes a still-connecting socket and detaches every listener', () => {
		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		const closes: Array<number> = [];
		transport.on('close', (event) => closes.push(event.code));
		transport.start();
		const socket = FakeWebSocket.instances[0];

		transport.dispose();
		socket.dispatch('close', {code: 1000, reason: '', wasClean: true});

		expect(socket.closeCalls).toEqual([{code: 1000, reason: 'Disposing stale socket'}]);
		expect(closes).toEqual([]);
		expect(transport.readyState).toBe(GatewayWireReadyState.CLOSED);
	});
});

describe('desktop connection routing', () => {
	test('an event for a retired connection is dropped, not misrouted', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		const retired = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		const survivor = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		const survivorMessages: Array<unknown> = [];
		survivor.on('message', (event) => survivorMessages.push(event.data));
		retired.start();
		survivor.start();
		await flush();
		const [retiredConnectionId, survivorConnectionId] = stub.createdConnectionIds;
		retired.dispose();

		stub.emit({
			connectionId: survivorConnectionId,
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});
		stub.emit({
			connectionId: retiredConnectionId,
			kind: 'message',
			data: '{"op":11}',
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});

		expect(retiredConnectionId).not.toBe(survivorConnectionId);
		expect(survivorMessages).toEqual([]);
		expect(stub.disposedConnectionIds).toEqual([retiredConnectionId]);
	});

	test('connection ids are scoped to this document', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		const messages: Array<unknown> = [];
		transport.on('message', (event) => messages.push(event.data));
		transport.start();
		await flush();
		stub.emit({
			connectionId: stub.createdConnectionIds[0],
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});

		stub.emit({
			connectionId: 'gateway-renderer-00000000-0000-4000-8000-000000000000:other',
			kind: 'message',
			data: '{"op":11}',
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});

		expect(stub.createdConnectionIds[0]).toMatch(
			/^gateway-renderer-[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}:/,
		);
		expect(messages).toEqual([]);
	});

	test('one router serves every connection in the document', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);

		const first = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		const second = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		first.start();
		second.start();
		await flush();

		expect(stub.listenerCount()).toBe(1);
		expect(stub.createdConnectionIds).toHaveLength(2);
	});

	test('the renderer sends compressed frames as binary and never as text', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		const transport = createGatewayWireTransport('wss://gateway.example/', {
			binaryType: 'arraybuffer',
		});
		transport.start();
		await flush();
		stub.emit({
			connectionId: stub.createdConnectionIds[0],
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});

		transport.send(new Uint8Array([1, 2, 3, 4]));
		await flush();

		expect(stub.sentBinary).toEqual([{connectionId: stub.createdConnectionIds[0], byteLength: 4}]);
		expect(stub.sentText).toEqual([]);
	});
});

describe('native transport fallback', () => {
	function nativeEvent(
		connectionId: string,
		overrides: Partial<NativeGatewayTransportEvent>,
	): NativeGatewayTransportEvent {
		return {
			connectionId,
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
			...overrides,
		};
	}

	async function failBeforeOpen(stub: NativeGatewayTransportStub, message: string): Promise<void> {
		const transport = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
		expect(transport.kind).toBe(GatewayWireTransportKind.DESKTOP_NATIVE);
		const closes: Array<number> = [];
		transport.on('close', (event) => closes.push(event.code));
		transport.on('error', () => undefined);
		transport.start();
		await flush();
		const connectionId = stub.createdConnectionIds.at(-1) ?? '';
		stub.emit(nativeEvent(connectionId, {kind: 'error', message}));
		stub.emit(
			nativeEvent(connectionId, {
				kind: 'close',
				code: 1006,
				reason: 'Gateway websocket connect failed',
				wasClean: false,
			}),
		);
		expect(closes).toEqual([1006]);
		transport.dispose();
	}

	test('three native connects that close before opening switch new transports to the browser', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);

		for (let attempt = 0; attempt < 3; attempt += 1) {
			await failBeforeOpen(stub, 'gateway websocket connect failed: IO error: invalid peer certificate: UnknownIssuer');
		}

		expect(isDesktopNativeGatewayTransportAvailable()).toBe(false);
		const transport = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
		expect(transport.kind).toBe(GatewayWireTransportKind.BROWSER);
		transport.start();
		expect(FakeWebSocket.instances.map((socket) => socket.url)).toEqual(['wss://gateway.example/']);
	});

	test('native creates the shell refuses count toward the fallback', async () => {
		const stub = createNativeGatewayTransportStub({
			create: async () => {
				throw new Error('The native gateway transport addon is unavailable');
			},
		});
		installElectronAPI(stub.api);

		for (let attempt = 0; attempt < 3; attempt += 1) {
			const transport = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
			expect(transport.kind).toBe(GatewayWireTransportKind.DESKTOP_NATIVE);
			const closes: Array<number> = [];
			transport.on('close', (event) => closes.push(event.code));
			transport.on('error', () => undefined);
			transport.start();
			await flush();
			expect(closes).toEqual([1006]);
		}

		expect(createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'}).kind).toBe(
			GatewayWireTransportKind.BROWSER,
		);
	});

	test('a gateway the shell routes through a proxy switches new transports to the browser at once', async () => {
		const stub = createNativeGatewayTransportStub({
			create: async () => {
				throw new Error(
					`Error invoking remote method 'native-gateway-transport:create': Error: ${NATIVE_GATEWAY_TRANSPORT_PROXIED_MESSAGE}`,
				);
			},
		});
		installElectronAPI(stub.api);

		const transport = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
		expect(transport.kind).toBe(GatewayWireTransportKind.DESKTOP_NATIVE);
		transport.on('error', () => undefined);
		transport.start();
		await flush();

		const next = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
		expect(next.kind).toBe(GatewayWireTransportKind.BROWSER);
		next.start();
		expect(FakeWebSocket.instances.map((socket) => socket.url)).toEqual(['wss://gateway.example/']);
	});

	test('an open between failures resets the count', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);

		await failBeforeOpen(stub, 'gateway websocket connect timed out');
		await failBeforeOpen(stub, 'gateway websocket connect timed out');
		const healthy = createGatewayWireTransport('wss://gateway.example/', {binaryType: 'arraybuffer'});
		healthy.start();
		await flush();
		stub.emit(nativeEvent(stub.createdConnectionIds.at(-1) ?? '', {kind: 'open'}));
		healthy.dispose();
		await failBeforeOpen(stub, 'gateway websocket connect timed out');
		await failBeforeOpen(stub, 'gateway websocket connect timed out');

		expect(isDesktopNativeGatewayTransportAvailable()).toBe(true);
	});

	test('a gateway that answers the upgrade with an HTTP error does not trip the fallback', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);

		for (let attempt = 0; attempt < 4; attempt += 1) {
			await failBeforeOpen(stub, 'gateway websocket connect failed: HTTP error: 502 Bad Gateway');
		}

		expect(isDesktopNativeGatewayTransportAvailable()).toBe(true);
	});

	test('failures while the browser reports offline do not trip the fallback', async () => {
		const stub = createNativeGatewayTransportStub();
		installElectronAPI(stub.api);
		const onLine = vi.spyOn(window.navigator, 'onLine', 'get').mockReturnValue(false);

		try {
			for (let attempt = 0; attempt < 4; attempt += 1) {
				await failBeforeOpen(stub, 'gateway websocket connect failed: IO error: Network is unreachable');
			}
		} finally {
			onLine.mockRestore();
		}

		expect(isDesktopNativeGatewayTransportAvailable()).toBe(true);
	});
});
