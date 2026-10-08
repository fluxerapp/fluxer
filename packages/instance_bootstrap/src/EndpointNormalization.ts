// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CanonicalNetworkEndpointRule,
	HTTP_NETWORK_PROTOCOLS,
	normalizeCanonicalNetworkEndpoint,
	WEBSOCKET_NETWORK_PROTOCOLS,
} from '@fluxer/instance_bootstrap/src/NetworkOrigin';

export const InstanceEndpointKind = Object.freeze({
	API: 'api',
	WEBAPP: 'webapp',
	GATEWAY: 'gateway',
	SERVICE: 'service',
} as const);

export type InstanceEndpointKind = (typeof InstanceEndpointKind)[keyof typeof InstanceEndpointKind];

const INSTANCE_ENDPOINT_RULES: Readonly<Record<InstanceEndpointKind, CanonicalNetworkEndpointRule>> = Object.freeze({
	[InstanceEndpointKind.API]: Object.freeze({
		protocols: HTTP_NETWORK_PROTOCOLS,
		allowPath: true,
		allowRelative: true,
	}),
	[InstanceEndpointKind.WEBAPP]: Object.freeze({
		protocols: HTTP_NETWORK_PROTOCOLS,
		allowPath: false,
		allowRelative: false,
	}),
	[InstanceEndpointKind.GATEWAY]: Object.freeze({
		protocols: WEBSOCKET_NETWORK_PROTOCOLS,
		allowPath: true,
		allowRelative: false,
	}),
	[InstanceEndpointKind.SERVICE]: Object.freeze({
		protocols: HTTP_NETWORK_PROTOCOLS,
		allowPath: true,
		allowRelative: false,
	}),
});

export function normalizeInstanceEndpoint(value: unknown, kind: InstanceEndpointKind): string | null {
	return normalizeCanonicalNetworkEndpoint(value, INSTANCE_ENDPOINT_RULES[kind]);
}
