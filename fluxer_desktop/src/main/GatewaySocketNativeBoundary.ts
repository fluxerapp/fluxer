// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {getNativeGatewayDisableReason} from '@electron/main/LaunchOptions';
import type {NativeGatewayConnection, NativeGatewayConnectOptions, NativeGatewayEvent} from '@fluxer/gateway-socket';
import log from 'electron-log';

export const GATEWAY_SOCKET_ADDON_PACKAGE = '@fluxer/gateway-socket';

type GatewaySocketConnectBinding = (
	options: NativeGatewayConnectOptions,
	onEvent: (event: NativeGatewayEvent) => void,
	onTerminalEvent: (event: NativeGatewayEvent) => void,
) => NativeGatewayConnection;

interface GatewaySocketConnectRequest {
	url: string;
	address: string | null;
	onEvent: (event: NativeGatewayEvent) => void;
	onTerminalEvent: (event: NativeGatewayEvent) => void;
}

export interface GatewaySocketBoundary {
	connect: (request: GatewaySocketConnectRequest) => NativeGatewayConnection;
}

type GatewaySocketNativeModule = {
	connect: GatewaySocketConnectBinding | null;
	warmup?: (() => void) | null;
	loadError: Error | null;
};

const requireModule = createRequire(import.meta.url);

let bindingCache: GatewaySocketConnectBinding | null | undefined;
let warmupCache: (() => void) | null = null;

export function loadGatewaySocketBinding(): GatewaySocketConnectBinding | null {
	if (bindingCache !== undefined) return bindingCache;
	const disableReason = getNativeGatewayDisableReason();
	if (disableReason !== null) {
		log.info('[NativeGateway] The native gateway transport is disabled, using the browser transport', disableReason);
		bindingCache = null;
		return bindingCache;
	}
	try {
		const nativeModule = requireModule(GATEWAY_SOCKET_ADDON_PACKAGE) as GatewaySocketNativeModule;
		bindingCache = nativeModule.connect ?? null;
		warmupCache = bindingCache === null ? null : (nativeModule.warmup ?? null);
	} catch (error) {
		log.warn('[NativeGateway] The native gateway addon is unavailable, using the browser transport', error);
		bindingCache = null;
	}
	return bindingCache;
}

export function warmGatewaySocketTransport(): void {
	if (loadGatewaySocketBinding() === null || warmupCache === null) return;
	try {
		warmupCache();
	} catch (error) {
		log.warn('[NativeGateway] Failed to warm the native gateway transport', error);
	}
}

export function isNativeGatewayAvailable(): boolean {
	return loadGatewaySocketBinding() !== null;
}

export function createGatewaySocketBoundary(
	binding: GatewaySocketConnectBinding | null = loadGatewaySocketBinding(),
): GatewaySocketBoundary | null {
	if (!binding) return null;
	return {
		connect: ({url, address, onEvent, onTerminalEvent}) =>
			binding({url, address, mode: 'gateway'}, onEvent, onTerminalEvent),
	};
}
