// SPDX-License-Identifier: AGPL-3.0-or-later

import {CanonicalNetworkProtocol} from './HTTPConstants';

const ViteDevWebSocketProtocol = Object.freeze({
	WS: 'ws',
	WSS: 'wss',
} as const);

type ViteDevWebSocketProtocol = (typeof ViteDevWebSocketProtocol)[keyof typeof ViteDevWebSocketProtocol];

interface ViteDevWebSocketOptions {
	readonly endpoint: URL;
	readonly path: string;
}

interface ViteDevWebSocketConfig {
	readonly protocol: ViteDevWebSocketProtocol;
	readonly host: string;
	readonly clientPort: number;
	readonly path: string;
}

function resolveProtocol(endpoint: URL): ViteDevWebSocketProtocol {
	if (endpoint.protocol === CanonicalNetworkProtocol.HTTPS) {
		return ViteDevWebSocketProtocol.WSS;
	}
	return ViteDevWebSocketProtocol.WS;
}

function resolveClientPort(endpoint: URL): number {
	if (endpoint.port.length > 0) {
		return Number.parseInt(endpoint.port, 10);
	}
	if (endpoint.protocol === CanonicalNetworkProtocol.HTTPS) {
		return 443;
	}
	return 80;
}

export function resolveViteDevWebSocket({endpoint, path}: ViteDevWebSocketOptions): ViteDevWebSocketConfig {
	return {
		protocol: resolveProtocol(endpoint),
		host: endpoint.hostname,
		clientPort: resolveClientPort(endpoint),
		path,
	};
}
