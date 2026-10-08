// SPDX-License-Identifier: AGPL-3.0-or-later

export const GatewayConnectionRole = Object.freeze({
	FOREGROUND: 'foreground',
	BACKGROUND: 'background',
} as const);

export type GatewayConnectionRole = (typeof GatewayConnectionRole)[keyof typeof GatewayConnectionRole];
