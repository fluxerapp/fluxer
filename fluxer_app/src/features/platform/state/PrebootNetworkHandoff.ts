// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {PREBOOT_NETWORK_HINT_STORAGE_KEY, writeRawStorageItem} from '@app/features/platform/state/PrebootMirror';

export const PREBOOT_NETWORK_HINT_VERSION = 1;
const PREBOOT_HANDOFF_MAX_AGE_MS = 60_000;
const MAX_HINT_FIELD_LENGTH = 2048;

interface PrebootDiscoveryHandoff {
	readonly url: string;
	readonly response: Promise<Response>;
}

interface PrebootGatewayHandoff {
	readonly url: string;
	readonly socket: WebSocket;
	readonly messages: Array<MessageEvent>;
	open: boolean;
}

interface PrebootNetworkHandoff {
	readonly startedAt: number;
	discovery: PrebootDiscoveryHandoff | null;
	gateway: PrebootGatewayHandoff | null;
}

export interface PrebootGatewaySocket {
	readonly socket: WebSocket;
	readonly messages: ReadonlyArray<MessageEvent>;
}

export interface PrebootNetworkHint {
	readonly accountKey: string;
	readonly gatewayEndpoint: string;
	readonly discoveryUrl: string | null;
}

declare global {
	interface Window {
		__FLUXER_PREBOOT_NETWORK__?: PrebootNetworkHandoff | null;
	}
}

function readHandoff(): PrebootNetworkHandoff | null {
	if (typeof window === 'undefined') return null;
	const handoff = window.__FLUXER_PREBOOT_NETWORK__;
	if (handoff == null || typeof handoff.startedAt !== 'number') return null;
	if (Date.now() - handoff.startedAt > PREBOOT_HANDOFF_MAX_AGE_MS) {
		discardGateway(handoff);
		window.__FLUXER_PREBOOT_NETWORK__ = null;
		return null;
	}
	return handoff;
}

function detachGateway(gateway: PrebootGatewayHandoff): void {
	gateway.socket.onopen = null;
	gateway.socket.onmessage = null;
	gateway.socket.onclose = null;
	gateway.socket.onerror = null;
}

function discardGateway(handoff: PrebootNetworkHandoff): void {
	const gateway = handoff.gateway;
	handoff.gateway = null;
	if (gateway == null) return;
	detachGateway(gateway);
	try {
		gateway.socket.close(1000, 'Unused preboot gateway socket');
	} catch {}
}

function abortable<T>(promise: Promise<T>, signal: AbortSignal | null | undefined): Promise<T> {
	if (signal == null) return promise;
	if (signal.aborted) return Promise.reject(signal.reason);
	return new Promise<T>((resolve, reject) => {
		const onAbort = (): void => reject(signal.reason);
		signal.addEventListener('abort', onAbort, {once: true});
		promise.then(
			(value) => {
				signal.removeEventListener('abort', onAbort);
				resolve(value);
			},
			(error: unknown) => {
				signal.removeEventListener('abort', onAbort);
				reject(error);
			},
		);
	});
}

export function takePrebootDiscoveryResponse(url: string, signal?: AbortSignal | null): Promise<Response> | null {
	const handoff = readHandoff();
	const discovery = handoff?.discovery;
	if (handoff == null || discovery == null || discovery.url !== url) return null;
	handoff.discovery = null;
	return abortable(discovery.response, signal);
}

export function takePrebootGatewaySocket(url: string): PrebootGatewaySocket | null {
	const handoff = readHandoff();
	const gateway = handoff?.gateway;
	if (handoff == null || gateway == null) return null;
	if (gateway.url !== url) {
		discardGateway(handoff);
		return null;
	}
	handoff.gateway = null;
	const readyState = gateway.socket.readyState;
	if (readyState !== WebSocket.CONNECTING && readyState !== WebSocket.OPEN) {
		detachGateway(gateway);
		return null;
	}
	detachGateway(gateway);
	return {socket: gateway.socket, messages: gateway.messages};
}

function isPrebootNetworkSupported(): boolean {
	return typeof window !== 'undefined' && window.electron == null && !isDesktopLocalAppDocument();
}

function boundedUrl(value: string | null, protocols: ReadonlyArray<string>): string | null {
	if (value == null || value.length > MAX_HINT_FIELD_LENGTH) return null;
	try {
		return protocols.includes(new URL(value).protocol) ? value : null;
	} catch {
		return null;
	}
}

export function writePrebootNetworkHint(hint: PrebootNetworkHint): void {
	if (!isPrebootNetworkSupported()) return;
	const gateway = boundedUrl(hint.gatewayEndpoint, ['ws:', 'wss:']);
	if (gateway == null || hint.accountKey.length === 0 || hint.accountKey.length > MAX_HINT_FIELD_LENGTH) {
		writeRawStorageItem(PREBOOT_NETWORK_HINT_STORAGE_KEY, null);
		return;
	}
	writeRawStorageItem(
		PREBOOT_NETWORK_HINT_STORAGE_KEY,
		JSON.stringify({
			v: PREBOOT_NETWORK_HINT_VERSION,
			a: hint.accountKey,
			g: gateway,
			d: boundedUrl(hint.discoveryUrl, ['http:', 'https:']),
		}),
	);
}
