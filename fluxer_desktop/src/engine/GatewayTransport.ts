// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LookupAddress} from 'node:dns';
import {createRequire} from 'node:module';
import {isIP, type LookupFunction} from 'node:net';
import {errorMessage} from '@electron/common/ErrorMessage';
import {Agent, type Dispatcher, ProxyAgent, WebSocket} from 'undici';

export type GatewayTransportEvent =
	| {readonly kind: 'open'}
	| {readonly kind: 'text'; readonly data: string}
	| {readonly kind: 'binary'; readonly data: Buffer}
	| {readonly kind: 'close'; readonly code: number; readonly reason: string}
	| {readonly kind: 'error'; readonly message: string};

export interface GatewayTransportConnection {
	sendText(text: string): void;
	sendBinary(payload: Buffer): void;
	close(code: number, reason: string): void;
}

interface GatewayTransportConnectRequest {
	readonly url: string;
	readonly address: string | null;
	readonly proxy: string | null;
	readonly onEvent: (event: GatewayTransportEvent) => void;
}

export type GatewayTransportFactory = (request: GatewayTransportConnectRequest) => GatewayTransportConnection;

interface NativeGatewayEvent {
	kind: 'open' | 'message' | 'binary' | 'error' | 'close';
	data: string | null;
	binary: Buffer | null;
	code: number | null;
	reason: string | null;
	message: string | null;
}

interface NativeGatewayConnection {
	sendText(text: string): void;
	sendBinary(payload: Buffer): void;
	close(code: number, reason: string): void;
	dispose(): void;
}

type NativeConnect = (
	options: {url: string; address: string | null; mode: 'gateway'},
	onEvent: (event: NativeGatewayEvent) => void,
	onTerminalEvent: (event: NativeGatewayEvent) => void,
) => NativeGatewayConnection;

function translateNativeEvent(event: NativeGatewayEvent): GatewayTransportEvent | null {
	switch (event.kind) {
		case 'open':
			return {kind: 'open'};
		case 'message':
			return {kind: 'text', data: event.data ?? ''};
		case 'binary':
			return event.binary === null ? null : {kind: 'binary', data: event.binary};
		case 'close':
			return {kind: 'close', code: event.code ?? 1006, reason: event.reason ?? ''};
		case 'error':
			return {kind: 'error', message: event.message ?? 'Gateway transport error'};
	}
}

function createNativeGatewayTransport(connect: NativeConnect): GatewayTransportFactory {
	return ({url, address, onEvent}) => {
		let terminated = false;
		const deliver = (event: NativeGatewayEvent): void => {
			if (terminated) {
				return;
			}
			const translated = translateNativeEvent(event);
			if (translated === null) {
				return;
			}
			if (translated.kind === 'close' || translated.kind === 'error') {
				terminated = true;
				queueMicrotask(() => native.dispose());
			}
			onEvent(translated);
		};
		const native = connect({url, address, mode: 'gateway'}, deliver, deliver);
		return {
			sendText: (text) => native.sendText(text),
			sendBinary: (payload) => native.sendBinary(payload),
			close: (code, reason) => native.close(code, reason),
		};
	};
}

function pinnedLookup(address: string): LookupFunction {
	const family = isIP(address);
	return (_hostname, options, callback) => {
		if (family === 0) {
			callback(new Error('The pinned gateway address is not an IP address'), '', 0);
			return;
		}
		if (options.all === true) {
			(callback as unknown as (error: null, addresses: Array<LookupAddress>) => void)(null, [{address, family}]);
			return;
		}
		callback(null, address, family);
	};
}

interface WebSocketGatewayTransportOptions {
	readonly trustedCertificates?: ReadonlyArray<string>;
}

function createWebSocketGatewayTransport(options: WebSocketGatewayTransportOptions = {}): GatewayTransportFactory {
	const trustedCertificates = options.trustedCertificates ?? [];
	const trust = trustedCertificates.length > 0 ? {ca: [...trustedCertificates]} : {};
	return ({url, address, proxy, onEvent}) => {
		const dispatcher: Dispatcher =
			proxy === null
				? new Agent({
						connect: {
							...(address === null ? {} : {lookup: pinnedLookup(address)}),
							...trust,
						},
					})
				: new ProxyAgent({uri: proxy, proxyTunnel: true, requestTls: trust, proxyTls: trust});
		let terminated = false;
		const finish = (event: GatewayTransportEvent): void => {
			if (terminated) {
				return;
			}
			terminated = true;
			void dispatcher.close().catch(() => undefined);
			onEvent(event);
		};
		const socket = new WebSocket(url, {dispatcher});
		socket.binaryType = 'arraybuffer';
		socket.addEventListener('open', () => {
			if (!terminated) onEvent({kind: 'open'});
		});
		socket.addEventListener('message', (event) => {
			if (terminated) return;
			const data: unknown = event.data;
			if (typeof data === 'string') {
				onEvent({kind: 'text', data});
			} else if (data instanceof ArrayBuffer) {
				onEvent({kind: 'binary', data: Buffer.from(data)});
			}
		});
		socket.addEventListener('close', (event) => finish({kind: 'close', code: event.code, reason: event.reason}));
		socket.addEventListener('error', () => {
			if (socket.readyState === WebSocket.CLOSED) {
				finish({kind: 'close', code: 1006, reason: 'Gateway socket error'});
			}
		});
		return {
			sendText: (text) => socket.send(text),
			sendBinary: (payload) => socket.send(new Uint8Array(payload)),
			close: (code, reason) => {
				try {
					socket.close(code, reason);
				} catch {
					socket.close();
				}
			},
		};
	};
}

export interface ResolvedGatewayTransport {
	readonly factory: GatewayTransportFactory;
	readonly kind: 'native' | 'websocket';
	readonly loadError: string | null;
}

export interface GatewayTransportPreferences {
	readonly preferWebSocket: boolean;
	readonly trustedCertificates: ReadonlyArray<string>;
}

export function resolveGatewayTransport({
	preferWebSocket,
	trustedCertificates,
}: GatewayTransportPreferences): ResolvedGatewayTransport {
	const websocket = createWebSocketGatewayTransport({trustedCertificates});
	if (preferWebSocket) {
		return {factory: websocket, kind: 'websocket', loadError: null};
	}
	try {
		const requireModule = typeof require === 'function' ? require : createRequire(import.meta.url);
		const addon = requireModule('@fluxer/gateway-socket') as {connect: NativeConnect | null; loadError: Error | null};
		if (addon.connect !== null) {
			const native = createNativeGatewayTransport(addon.connect);
			return {
				factory: (request) => (request.proxy === null ? native(request) : websocket(request)),
				kind: 'native',
				loadError: null,
			};
		}
		return {
			factory: websocket,
			kind: 'websocket',
			loadError: addon.loadError?.message ?? 'The gateway socket addon has no connect binding',
		};
	} catch (error) {
		return {
			factory: websocket,
			kind: 'websocket',
			loadError: errorMessage(error),
		};
	}
}
