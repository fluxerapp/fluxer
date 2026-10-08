// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopNativeGatewayTransport} = await import('./NativeGatewayTransport.ts');
const {GatewayOriginRegistry} = await import('./GatewayOriginRegistry.ts');
const {DesktopOutboundHTTP} = await import('./DesktopOutboundHTTP.ts');
const {RendererDocumentOwnerFactory} = await import('./RendererDocumentOwner.ts');
const {parseNativeGatewayTransportEvent} = await import('./NativeGatewayTransportEventParser.ts');
const {websocketHTTPOrigin, websocketOrigin} = await import('./WebSocketOrigin.ts');
const {getNativeGatewayDisableReason} = await import('./LaunchOptions.ts');
const {reconstructNativeGatewayTransportEvent} = await import('../preload/PreloadNativeGatewayEvent.ts');
const {createNativeGatewayPreloadAPI} = await import('../preload/NativeGatewayPreload.ts');
const {
	NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG,
	NATIVE_GATEWAY_TRANSPORT_CHANNELS,
	NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL,
} = await import('../../../packages/desktop_ipc/src/GatewayTransportContract.ts');

const APP_ORIGIN = 'https://web.fluxer.app';
const GATEWAY_URL = 'wss://gateway.fluxer.app/?v=9&encoding=json&compress=zstd-stream&stream=1';
const GATEWAY_WS_ORIGIN = 'wss://gateway.fluxer.app';

let connectionCounter = 0;

function nextConnectionId() {
	connectionCounter += 1;
	const suffix = String(connectionCounter).padStart(12, '0');
	return `gateway-renderer-11111111-1111-4111-8111-111111111111:22222222-2222-4222-8222-${suffix}`;
}

function createDocument({url = `${APP_ORIGIN}/channels/@me`, privileged = true} = {}) {
	const listeners = new Map();
	const sent = [];
	const frame = {
		url,
		detached: false,
		parent: null,
		send: (channel, payload) => sent.push({channel, payload}),
	};
	const sender = {
		privileged,
		destroyed: false,
		isDestroyed: () => sender.destroyed,
		isLoadingMainFrame: () => false,
		mainFrame: frame,
		on: (event, listener) => {
			const list = listeners.get(event) ?? [];
			list.push(listener);
			listeners.set(event, list);
		},
		once: (event, listener) => sender.on(event, listener),
		removeListener: (event, listener) => {
			const list = listeners.get(event) ?? [];
			const index = list.indexOf(listener);
			if (index >= 0) list.splice(index, 1);
			listeners.set(event, list);
		},
		emit: (event, ...args) => {
			for (const listener of [...(listeners.get(event) ?? [])]) listener(...args);
		},
		listenerCount: (event) => (listeners.get(event) ?? []).length,
	};
	frame.top = frame;
	frame.owner = sender;
	return {frame, sender, sent, event: {sender, senderFrame: frame}};
}

function createSubframeEvent(document) {
	const subframe = {
		url: document.frame.url,
		detached: false,
		parent: document.frame,
		send: () => {},
		owner: document.sender,
	};
	subframe.top = document.frame;
	return {sender: document.sender, senderFrame: subframe};
}

function createBoundary() {
	const connections = [];
	return {
		connections,
		boundary: {
			connect: ({url, address, onEvent, onTerminalEvent}) => {
				const connection = {
					url,
					address,
					onEvent,
					onTerminalEvent,
					disposed: false,
					text: [],
					binary: [],
					closes: [],
					sendText: (value) => connection.text.push(value),
					sendBinary: (value) => connection.binary.push(value),
					close: (code, reason) => connection.closes.push({code, reason}),
					dispose: () => {
						connection.disposed = true;
					},
				};
				connections.push(connection);
				return connection;
			},
		},
	};
}

function createRegistry({gatewayEndpoint = 'wss://gateway.fluxer.app/', anchors = []} = {}) {
	const resolved = [];
	const registry = new GatewayOriginRegistry({
		additionalAnchorOrigins: async () => anchors,
		resolveAnchorGatewayOrigin: async (anchorOrigin) => {
			resolved.push(anchorOrigin);
			return anchorOrigin === APP_ORIGIN ? gatewayEndpoint : null;
		},
	});
	registry.seedAnchorOrigin(APP_ORIGIN);
	return {registry, resolved};
}

function refuseCleartextGatewayAddress(httpOrigin) {
	return Promise.reject(new Error(`unexpected cleartext lookup for ${httpOrigin}`));
}

function createTransport({
	privileged = true,
	registry,
	boundary,
	requireCleartextGatewayAddress = refuseCleartextGatewayAddress,
	resolveProxy = async () => 'DIRECT',
} = {}) {
	const origins = registry ?? createRegistry();
	const native = boundary ?? createBoundary();
	const mainDocument = createDocument({privileged});
	const transport = new DesktopNativeGatewayTransport({
		boundary: native.boundary,
		logger: {info: () => {}, warn: () => {}, error: () => {}},
		originRegistry: origins.registry,
		requireCleartextGatewayAddress,
		resolveProxy,
		rendererDocumentOwners: new RendererDocumentOwnerFactory({
			policy: {
				isPrivilegedRendererDocument: ({sender, url}) => sender.privileged === true && url.startsWith(APP_ORIGIN),
			},
			onWatcherFailure: () => {},
		}),
	});
	return {transport, routes: transport.ipcRoutes(), native, origins, mainDocument};
}

function nativeEvent(overrides) {
	return {
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

async function rejects(promise, matcher) {
	await assert.rejects(promise, matcher);
}

describe('NativeGatewayTransportRequestBoundary', () => {
	test('refuses an http, file, credentialed or fragmented target', async () => {
		const {routes, mainDocument} = createTransport();
		for (const url of [
			'http://gateway.fluxer.app/',
			'file:///etc/passwd',
			'wss://user:secret@gateway.fluxer.app/',
			'wss://gateway.fluxer.app/#fragment',
			'not-a-url',
		]) {
			await rejects(
				routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
					connectionId: nextConnectionId(),
					url,
					mode: 'gateway',
				}),
				/Native gateway transport request is invalid/u,
			);
		}
	});

	test('refuses a malformed connection id and an unexpected request key', async () => {
		const {routes, mainDocument} = createTransport();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: 'voice-renderer-1',
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/connection id has an invalid format/u,
		);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
				compression: 'none',
			}),
			/plain object with exactly its own keys/u,
		);
	});

	test('refuses the voice transport mode at the TS boundary', async () => {
		const {routes, mainDocument} = createTransport();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'voice',
			}),
			/transport mode must be gateway/u,
		);
	});

	test('refuses an origin that no reachable instance advertises as its gateway', async () => {
		const {routes, mainDocument} = createTransport();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: 'wss://attacker.example/',
				mode: 'gateway',
			}),
			/no reachable instance advertises it as its gateway/u,
		);
	});

	test('refuses a close code outside 1000 and the application range', async () => {
		const {routes, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.close](mainDocument.event, {connectionId, code: 1006, reason: ''}),
			/close code is not permitted/u,
		);
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.close](mainDocument.event, {
			connectionId,
			code: 4000,
			reason: 'Client disconnecting',
		});
	});
});

describe('RendererDocumentOwner', () => {
	test('rejects a capture from a subframe', async () => {
		const {routes, mainDocument} = createTransport();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](createSubframeEvent(mainDocument), {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/requires the current privileged main-frame renderer document/u,
		);
	});

	test('rejects a capture from a data: URL popout document sharing the preload', async () => {
		const {routes} = createTransport();
		const popout = createDocument({url: 'data:text/html;charset=utf-8,%3Chtml%3E'});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](popout.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/requires the current privileged main-frame renderer document/u,
		);
	});

	test('rejects a capture from a second privileged-origin window that is not the app window', async () => {
		const {routes} = createTransport();
		const popout = createDocument({url: `${APP_ORIGIN}/popout`, privileged: false});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](popout.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/requires the current privileged main-frame renderer document/u,
		);
	});

	test('rejects a capture with no sender frame', async () => {
		const {routes, mainDocument} = createTransport();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](
				{sender: mainDocument.sender, senderFrame: null},
				{connectionId: nextConnectionId(), url: GATEWAY_URL, mode: 'gateway'},
			),
			/requires the current privileged main-frame renderer document/u,
		);
	});
});

describe('DesktopNativeGatewayTransport', () => {
	test('forwards compressed frames byte-for-byte in both directions without inspecting them', async () => {
		const {routes, native, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		const connection = native.connections[0];
		const compressed = new Uint8Array([0x28, 0xb5, 0x2f, 0xfd, 0x00, 0x58, 0x21, 0x00, 0x00]);
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendBinary](mainDocument.event, {
			connectionId,
			payload: compressed.buffer,
		});
		assert.deepEqual([...connection.binary[0]], [...compressed]);
		connection.onEvent(nativeEvent({kind: 'binary', binary: Buffer.from(compressed)}));
		const delivered = mainDocument.sent.at(-1);
		assert.equal(delivered.channel, NATIVE_GATEWAY_TRANSPORT_EVENT_CHANNEL);
		assert.deepEqual([...new Uint8Array(delivered.payload.binary)], [...compressed]);
		assert.equal(delivered.payload.data, null);
	});

	test('passes the admitted URL through verbatim, including stream=1', async () => {
		const {routes, native, mainDocument} = createTransport();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		assert.equal(native.connections[0].url, GATEWAY_URL);
		assert.ok(native.connections[0].url.includes('stream=1'));
		assert.ok(native.connections[0].url.includes('compress=zstd-stream'));
	});

	test('connects wss without a pinned address', async () => {
		const {routes, native, mainDocument} = createTransport();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		assert.equal(native.connections[0].address, null);
	});

	test('pins a cleartext ws gateway to the non-public address it resolved', async () => {
		const looked = [];
		const {routes, native, mainDocument} = createTransport({
			registry: createRegistry({gatewayEndpoint: 'ws://localhost:8088/gateway'}),
			requireCleartextGatewayAddress: async (httpOrigin) => {
				looked.push(httpOrigin);
				return '127.0.0.1';
			},
		});
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: 'ws://localhost:8088/gateway?v=9',
			mode: 'gateway',
		});
		assert.deepEqual(looked, ['http://localhost:8088']);
		assert.equal(native.connections[0].address, '127.0.0.1');
	});

	test('refuses a cleartext ws gateway on a publicly routable host', async () => {
		const outboundHTTP = new DesktopOutboundHTTP({
			resolveHostAddresses: async () => ['93.184.216.34'],
		});
		const {routes, native, mainDocument} = createTransport({
			registry: createRegistry({gatewayEndpoint: 'ws://gateway.example.com/'}),
			requireCleartextGatewayAddress: (httpOrigin) => outboundHTTP.requireCleartextTransportAddress(httpOrigin),
		});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: 'ws://gateway.example.com/?v=9',
				mode: 'gateway',
			}),
			/requires https for the publicly routable origin http:\/\/gateway\.example\.com/u,
		);
		assert.equal(native.connections.length, 0);
		outboundHTTP.cleanup();
	});

	test('refuses a second document acting on another document that owns the connection', async () => {
		const {routes, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		const intruder = createDocument();
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText](intruder.event, {connectionId, payload: '{"op":1}'}),
			/owned by another renderer document/u,
		);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.dispose](intruder.event, connectionId),
			/owned by another renderer document/u,
		);
	});

	test('a did-start-navigation disposes exactly that document connections', async () => {
		const shared = createTransport();
		const second = createDocument();
		const firstId = nextConnectionId();
		const secondId = nextConnectionId();
		await shared.routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](shared.mainDocument.event, {
			connectionId: firstId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		await shared.routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](second.event, {
			connectionId: secondId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		assert.equal(shared.native.connections.length, 2);
		shared.mainDocument.sender.emit('did-start-navigation', {isMainFrame: true, isSameDocument: false});
		assert.equal(shared.native.connections[0].disposed, true);
		assert.equal(shared.native.connections[1].disposed, false);
		await rejects(
			shared.routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText](shared.mainDocument.event, {
				connectionId: firstId,
				payload: '{}',
			}),
			/does not exist/u,
		);
		await shared.routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText](second.event, {
			connectionId: secondId,
			payload: '{}',
		});
	});

	test('ignores a same-document navigation and a subframe navigation', async () => {
		const {routes, native, mainDocument} = createTransport();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		mainDocument.sender.emit('did-start-navigation', {isMainFrame: true, isSameDocument: true});
		mainDocument.sender.emit('did-start-navigation', {isMainFrame: false, isSameDocument: false});
		assert.equal(native.connections[0].disposed, false);
	});

	test('a render-process-gone disposes the document connections', async () => {
		const {routes, native, mainDocument} = createTransport();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		mainDocument.sender.emit('render-process-gone');
		assert.equal(native.connections[0].disposed, true);
	});

	test('removes every renderer listener once the last connection is gone', async () => {
		const {routes, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		assert.equal(mainDocument.sender.listenerCount('did-start-navigation'), 1);
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.dispose](mainDocument.event, connectionId);
		assert.equal(mainDocument.sender.listenerCount('did-start-navigation'), 0);
		assert.equal(mainDocument.sender.listenerCount('render-process-gone'), 0);
		assert.equal(mainDocument.sender.listenerCount('destroyed'), 0);
	});

	test('refuses the ninth live connection for one document', async () => {
		const {routes, native, mainDocument} = createTransport();
		for (let index = 0; index < 8; index += 1) {
			await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			});
		}
		assert.equal(native.connections.length, 8);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/connection limit reached/u,
		);
	});

	test('refuses a duplicate connection id', async () => {
		const {routes, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId,
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/already exists/u,
		);
	});

	test('a malformed native event produces a synthetic error, close 1006 and destroys the connection', async () => {
		for (const malformed of [
			nativeEvent({kind: 'message', data: 'payload', extra: 1}),
			{kind: 'open', data: null, binary: null, code: null, reason: null, wasClean: null},
			nativeEvent({kind: 'message', data: 'payload', code: 4000}),
			nativeEvent({kind: 'unknown'}),
		]) {
			const {routes, native, mainDocument} = createTransport();
			const connectionId = nextConnectionId();
			await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId,
				url: GATEWAY_URL,
				mode: 'gateway',
			});
			mainDocument.sent.length = 0;
			native.connections[0].onEvent(malformed);
			assert.equal(mainDocument.sent.length, 2);
			assert.equal(mainDocument.sent[0].payload.kind, 'error');
			assert.equal(mainDocument.sent[1].payload.kind, 'close');
			assert.equal(mainDocument.sent[1].payload.code, 1006);
			assert.equal(mainDocument.sent[1].payload.wasClean, false);
			assert.equal(native.connections[0].disposed, true);
		}
	});

	test('a native close event retires the connection', async () => {
		const {routes, native, mainDocument} = createTransport();
		const connectionId = nextConnectionId();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId,
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		native.connections[0].onTerminalEvent(
			nativeEvent({kind: 'close', code: 1006, reason: 'delivery failed', wasClean: false}),
		);
		assert.equal(native.connections[0].disposed, true);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.sendText](mainDocument.event, {connectionId, payload: '{}'}),
			/does not exist/u,
		);
	});

	test('refuses every verb once the transport is shut down and disposes live connections', async () => {
		const {transport, routes, native, mainDocument} = createTransport();
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		transport.cleanup();
		assert.equal(native.connections[0].disposed, true);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/shutting down/u,
		);
	});

	test('refuses a gateway the session routes through a proxy and opens no native socket', async () => {
		const lookups = [];
		const {routes, mainDocument, native} = createTransport({
			resolveProxy: async (event, url) => {
				lookups.push({sender: event.sender, url});
				return 'PROXY proxy.corp.example:8080; DIRECT';
			},
		});
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/resolves through a proxy/u,
		);
		assert.deepEqual(lookups, [{sender: mainDocument.sender, url: GATEWAY_URL.replace(/^wss/u, 'https')}]);
		assert.equal(native.connections.length, 0);
	});

	test('opens a native socket when the session resolves the gateway as DIRECT', async () => {
		const {routes, mainDocument, native} = createTransport({resolveProxy: async () => ' DIRECT '});
		await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		assert.equal(native.connections.length, 1);
	});

	test('resolves the proxy route while the origin admission is still in flight', async () => {
		let releaseAdmission;
		const admissionGate = new Promise((resolve) => {
			releaseAdmission = resolve;
		});
		const registry = new GatewayOriginRegistry({
			resolveAnchorGatewayOrigin: async (anchor) => {
				await admissionGate;
				return anchor === APP_ORIGIN ? 'wss://gateway.fluxer.app/' : null;
			},
		});
		registry.seedAnchorOrigin(APP_ORIGIN);
		let proxyLookups = 0;
		const {routes, mainDocument, native} = createTransport({
			registry: {registry},
			resolveProxy: async () => {
				proxyLookups += 1;
				return 'DIRECT';
			},
		});
		const created = routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](mainDocument.event, {
			connectionId: nextConnectionId(),
			url: GATEWAY_URL,
			mode: 'gateway',
		});
		await new Promise((resolve) => setImmediate(resolve));
		assert.equal(proxyLookups, 1);
		assert.equal(native.connections.length, 0);
		releaseAdmission();
		await created;
		assert.equal(native.connections.length, 1);
	});

	test('reports availability from the boundary the shell actually loaded', async () => {
		const withAddon = createTransport();
		assert.equal(
			await withAddon.routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable](withAddon.mainDocument.event),
			true,
		);
		const origins = createRegistry();
		const withoutAddon = new DesktopNativeGatewayTransport({
			boundary: null,
			logger: {info: () => {}, warn: () => {}, error: () => {}},
			originRegistry: origins.registry,
			requireCleartextGatewayAddress: refuseCleartextGatewayAddress,
			resolveProxy: async () => 'DIRECT',
			rendererDocumentOwners: new RendererDocumentOwnerFactory({
				policy: {isPrivilegedRendererDocument: () => true},
				onWatcherFailure: () => {},
			}),
		});
		const routes = withoutAddon.ipcRoutes();
		const document = createDocument();
		assert.equal(await routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable](document.event), false);
		await rejects(
			routes[NATIVE_GATEWAY_TRANSPORT_CHANNELS.create](document.event, {
				connectionId: nextConnectionId(),
				url: GATEWAY_URL,
				mode: 'gateway',
			}),
			/addon is unavailable/u,
		);
	});
});

describe('GatewayOriginRegistry', () => {
	test('admits an origin an anchor instance advertises and caches it for the document', async () => {
		const {registry, resolved} = createRegistry();
		const document = {};
		assert.equal(await registry.admit({documentKey: document, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
		assert.deepEqual(resolved, [APP_ORIGIN]);
		assert.equal(await registry.admit({documentKey: document, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
		assert.deepEqual(resolved, [APP_ORIGIN]);
	});

	test('keys admission per renderer document', async () => {
		const {registry} = createRegistry();
		await registry.admit({documentKey: {}, url: GATEWAY_URL});
		const second = {};
		assert.equal(await registry.admit({documentKey: second, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
	});

	test('admits the gateway a runtime plan advertises without fetching discovery again', async () => {
		const resolved = [];
		let anchorReads = 0;
		const registry = new GatewayOriginRegistry({
			additionalAnchorOrigins: async () => {
				anchorReads += 1;
				return ['other.example'];
			},
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return null;
			},
		});
		registry.recordAnchorGateway(`${APP_ORIGIN}/api`, 'wss://gateway.fluxer.app');
		assert.equal(await registry.admit({documentKey: {}, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
		assert.deepEqual(resolved, []);
		assert.equal(anchorReads, 0);
	});

	test('still fetches discovery for a gateway no recorded runtime plan advertises', async () => {
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === 'https://gateway.fluxer.app' ? 'wss://gateway.fluxer.app' : null;
			},
		});
		registry.recordAnchorGateway(`${APP_ORIGIN}/api`, 'wss://gateway.other.example');
		assert.equal(await registry.admit({documentKey: {}, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
		assert.deepEqual(resolved, ['https://gateway.fluxer.app']);
	});

	test('fetches discovery for a recorded runtime plan that advertises no gateway', async () => {
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === APP_ORIGIN ? 'wss://gateway.fluxer.app' : null;
			},
		});
		registry.recordAnchorGateway(`${APP_ORIGIN}/api`, null);
		assert.equal(await registry.admit({documentKey: {}, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
		assert.deepEqual(resolved, [APP_ORIGIN]);
	});

	test('fetches discovery again once a recorded runtime plan gateway expires', async () => {
		let now = 1_000_000;
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			now: () => now,
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === APP_ORIGIN ? 'wss://gateway.fluxer.app' : null;
			},
		});
		registry.recordAnchorGateway(`${APP_ORIGIN}/api`, 'wss://gateway.fluxer.app');
		await registry.admit({documentKey: {}, url: GATEWAY_URL});
		assert.deepEqual(resolved, []);
		now += 6 * 60 * 60 * 1000 + 1;
		await registry.admit({documentKey: {}, url: GATEWAY_URL});
		assert.deepEqual(resolved, [APP_ORIGIN]);
	});

	test('tries the target own HTTP origin as a last anchor for single-host self-hosters', async () => {
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === 'https://self.example' ? 'wss://self.example/gateway' : null;
			},
		});
		assert.equal(await registry.admit({documentKey: {}, url: 'wss://self.example/gateway'}), 'wss://self.example');
		assert.deepEqual(resolved, ['https://self.example']);
	});

	test('tries a known instance origin so a split-host self-hoster still connects', async () => {
		const registry = new GatewayOriginRegistry({
			additionalAnchorOrigins: async () => ['self.example'],
			resolveAnchorGatewayOrigin: async (anchor) =>
				anchor === 'https://self.example' ? 'wss://gateway.self.example' : null,
		});
		assert.equal(
			await registry.admit({documentKey: {}, url: 'wss://gateway.self.example/'}),
			'wss://gateway.self.example',
		);
	});

	test('shares one in-flight admission so concurrent sockets on the same origin all connect', async () => {
		const now = 1_000_000;
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			now: () => now,
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === APP_ORIGIN ? 'wss://gateway.fluxer.app/' : null;
			},
		});
		registry.seedAnchorOrigin(APP_ORIGIN);
		const document = {};
		const admitted = await Promise.all([
			registry.admit({documentKey: document, url: GATEWAY_URL}),
			registry.admit({documentKey: document, url: GATEWAY_URL}),
			registry.admit({documentKey: document, url: GATEWAY_URL}),
		]);
		assert.deepEqual(admitted, [GATEWAY_WS_ORIGIN, GATEWAY_WS_ORIGIN, GATEWAY_WS_ORIGIN]);
		assert.deepEqual(resolved, [APP_ORIGIN]);
	});

	test('refuses a second attempt on the same origin within a second', async () => {
		let now = 1_000_000;
		const registry = new GatewayOriginRegistry({now: () => now, resolveAnchorGatewayOrigin: async () => null});
		const document = {};
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /no reachable instance/u);
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /attempted too recently/u);
		now += 1000;
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /no reachable instance/u);
	});

	test('refuses more than eight distinct origins in a minute and sixty-four per document', async () => {
		let now = 1_000_000;
		const registry = new GatewayOriginRegistry({now: () => now, resolveAnchorGatewayOrigin: async () => null});
		const document = {};
		for (let index = 0; index < 8; index += 1) {
			now += 1;
			await rejects(registry.admit({documentKey: document, url: `wss://host-${index}.example/`}), /no reachable/u);
		}
		now += 1;
		await rejects(registry.admit({documentKey: document, url: 'wss://host-8.example/'}), /too many origins/u);
		now += 60_000;
		let attempts = 8;
		while (attempts < 64) {
			now += 60_000;
			await rejects(registry.admit({documentKey: document, url: `wss://host-${attempts}.example/`}), /no reachable/u);
			attempts += 1;
		}
		now += 60_000;
		await rejects(
			registry.admit({documentKey: document, url: 'wss://host-100.example/'}),
			/exhausted its origin registration budget/u,
		);
	});

	test('re-verifies an admitted origin after the six hour TTL', async () => {
		let now = 1_000_000;
		const resolved = [];
		const registry = new GatewayOriginRegistry({
			now: () => now,
			resolveAnchorGatewayOrigin: async (anchor) => {
				resolved.push(anchor);
				return anchor === APP_ORIGIN ? 'wss://gateway.fluxer.app/' : null;
			},
		});
		registry.seedAnchorOrigin(APP_ORIGIN);
		const document = {};
		await registry.admit({documentKey: document, url: GATEWAY_URL});
		now += 6 * 60 * 60 * 1000 + 1;
		await registry.admit({documentKey: document, url: GATEWAY_URL});
		assert.equal(resolved.length, 2);
	});

	test('does not cache a discovery failure for six hours', async () => {
		let now = 1_000_000;
		let advertised = null;
		const registry = new GatewayOriginRegistry({now: () => now, resolveAnchorGatewayOrigin: async () => advertised});
		registry.seedAnchorOrigin(APP_ORIGIN);
		const document = {};
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /no reachable instance/u);
		advertised = 'wss://gateway.fluxer.app/';
		now += 60 * 1000 + 1;
		assert.equal(await registry.admit({documentKey: document, url: GATEWAY_URL}), GATEWAY_WS_ORIGIN);
	});

	test('releases a document state so a reload starts from a clean budget', async () => {
		const now = 1_000_000;
		const registry = new GatewayOriginRegistry({now: () => now, resolveAnchorGatewayOrigin: async () => null});
		const document = {};
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /no reachable/u);
		registry.releaseDocument(document);
		await rejects(registry.admit({documentKey: document, url: GATEWAY_URL}), /no reachable/u);
	});
});

describe('WebSocketOrigin', () => {
	test('derives both the ws and the http form with default ports collapsed', () => {
		assert.equal(websocketOrigin('wss://gateway.fluxer.app/socket?v=9'), 'wss://gateway.fluxer.app');
		assert.equal(websocketOrigin('https://gateway.fluxer.app'), 'wss://gateway.fluxer.app');
		assert.equal(websocketOrigin('ws://127.0.0.1:8080/'), 'ws://127.0.0.1:8080');
		assert.equal(websocketOrigin('file:///etc/passwd'), null);
		assert.equal(websocketHTTPOrigin('wss://gateway.fluxer.app/socket'), 'https://gateway.fluxer.app');
		assert.equal(websocketHTTPOrigin('ws://192.168.1.5:8080/'), 'http://192.168.1.5:8080');
		assert.equal(websocketHTTPOrigin('nope'), null);
	});
});

describe('NativeGatewayTransportEventParser', () => {
	test('accepts the addon exact seven field shape and normalises binary to ArrayBuffer', () => {
		const parsed = parseNativeGatewayTransportEvent(
			'id',
			nativeEvent({kind: 'binary', binary: Buffer.from([1, 2, 3])}),
		);
		assert.equal(parsed.kind, 'binary');
		assert.deepEqual([...new Uint8Array(parsed.binary)], [1, 2, 3]);
		assert.equal(parsed.data, null);
	});

	test('rejects an inapplicable field that is not null', () => {
		assert.throws(
			() => parseNativeGatewayTransportEvent('id', nativeEvent({kind: 'open', message: ''})),
			/message must be null/u,
		);
		assert.throws(
			() => parseNativeGatewayTransportEvent('id', nativeEvent({kind: 'open', data: undefined})),
			/must be a plain object with exactly the declared keys|data must be null/u,
		);
	});

	test('rejects a close event with an out of range code', () => {
		assert.throws(
			() =>
				parseNativeGatewayTransportEvent('id', nativeEvent({kind: 'close', code: 70_000, reason: '', wasClean: true})),
			/unsigned 16-bit integer/u,
		);
	});

	test('rejects a prototype-polluted record', () => {
		const hostile = Object.create({kind: 'open'});
		assert.throws(() => parseNativeGatewayTransportEvent('id', hostile), /plain object/u);
	});
});

describe('PreloadNativeGatewayEvent', () => {
	test('independently revalidates a well formed main event', () => {
		const event = {
			connectionId: nextConnectionId(),
			kind: 'message',
			data: '{"op":11}',
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		};
		assert.equal(reconstructNativeGatewayTransportEvent(event).data, '{"op":11}');
	});

	test('rejects a forged connection id, a missing key and an unknown kind', () => {
		const base = {
			connectionId: nextConnectionId(),
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		};
		assert.throws(
			() => reconstructNativeGatewayTransportEvent({...base, connectionId: 'gateway-renderer-x:y'}),
			/connection id has an invalid format/u,
		);
		const {message: _message, ...missing} = base;
		assert.throws(() => reconstructNativeGatewayTransportEvent(missing), /exactly the declared keys/u);
		assert.throws(() => reconstructNativeGatewayTransportEvent({...base, kind: 'voice'}), /event kind is unknown/u);
	});

	test('normalises a byte view to a standalone ArrayBuffer', () => {
		const source = new Uint8Array([9, 8, 7, 6]);
		const event = {
			connectionId: nextConnectionId(),
			kind: 'binary',
			data: null,
			binary: source.subarray(1, 3),
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		};
		const reconstructed = reconstructNativeGatewayTransportEvent(event);
		assert.ok(reconstructed.binary instanceof ArrayBuffer);
		assert.deepEqual([...new Uint8Array(reconstructed.binary)], [8, 7]);
	});
});

describe('native gateway kill switches', () => {
	test('names the launch flag, the environment variable and safe mode as disable reasons', () => {
		assert.equal(getNativeGatewayDisableReason([]), null);
		assert.equal(getNativeGatewayDisableReason(['--fluxer-disable-native-gateway']), '--fluxer-disable-native-gateway');
		assert.equal(getNativeGatewayDisableReason(['--fluxer-safe-mode']), '--fluxer-safe-mode');
		process.env.FLUXER_DISABLE_NATIVE_GATEWAY = '1';
		try {
			assert.equal(getNativeGatewayDisableReason([]), 'FLUXER_DISABLE_NATIVE_GATEWAY');
		} finally {
			delete process.env.FLUXER_DISABLE_NATIVE_GATEWAY;
		}
	});

	test('the renderer reports isAvailable false when the shell passes no availability argument', async () => {
		const invoked = [];
		const renderer = {
			invoke: async (channel, ...args) => {
				invoked.push({channel, args});
				return channel === NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable ? false : undefined;
			},
			on: () => {},
			removeListener: () => {},
		};
		const disabled = createNativeGatewayPreloadAPI(renderer, ['electron']).nativeGatewayTransport;
		assert.equal(disabled.isAvailable, false);
		await rejects(
			disabled.create({connectionId: nextConnectionId(), url: GATEWAY_URL, mode: 'gateway'}),
			/unavailable in this desktop shell/u,
		);
		const enabled = createNativeGatewayPreloadAPI(renderer, [
			'electron',
			NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG,
		]).nativeGatewayTransport;
		assert.equal(enabled.isAvailable, true);
	});

	test('the preload create awaits the main availability gate exactly once', async () => {
		let probes = 0;
		const renderer = {
			invoke: async (channel) => {
				if (channel === NATIVE_GATEWAY_TRANSPORT_CHANNELS.isAvailable) {
					probes += 1;
					return true;
				}
				return {connectionId: 'ok'};
			},
			on: () => {},
			removeListener: () => {},
		};
		const api = createNativeGatewayPreloadAPI(renderer, [
			'electron',
			NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG,
		]).nativeGatewayTransport;
		await api.create({connectionId: nextConnectionId(), url: GATEWAY_URL, mode: 'gateway'});
		await api.create({connectionId: nextConnectionId(), url: GATEWAY_URL, mode: 'gateway'});
		assert.equal(probes, 1);
	});

	test('the preload drops an event that fails its own validation and detaches on unsubscribe', () => {
		const listeners = [];
		const renderer = {
			invoke: async () => undefined,
			on: (_channel, listener) => listeners.push(listener),
			removeListener: (_channel, listener) => {
				const index = listeners.indexOf(listener);
				if (index >= 0) listeners.splice(index, 1);
			},
		};
		const api = createNativeGatewayPreloadAPI(renderer, ['electron']).nativeGatewayTransport;
		const received = [];
		const unsubscribe = api.onEvent((event) => received.push(event));
		listeners[0](null, {kind: 'open'});
		assert.equal(received.length, 0);
		listeners[0](null, {
			connectionId: nextConnectionId(),
			kind: 'open',
			data: null,
			binary: null,
			code: null,
			reason: null,
			wasClean: null,
			message: null,
		});
		assert.equal(received.length, 1);
		unsubscribe();
		assert.equal(listeners.length, 0);
	});
});
