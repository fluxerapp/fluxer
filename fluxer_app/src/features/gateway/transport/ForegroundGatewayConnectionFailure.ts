// SPDX-License-Identifier: AGPL-3.0-or-later

export const ForegroundGatewayRecoveryCause = Object.freeze({
	READINESS_TIMEOUT: 'readiness-timeout',
	TRANSPORT_DISCONNECTED: 'transport-disconnected',
	TRANSPORT_START_FAILED: 'transport-start-failed',
} as const);

export type ForegroundGatewayRecoveryCause =
	(typeof ForegroundGatewayRecoveryCause)[keyof typeof ForegroundGatewayRecoveryCause];

export class ForegroundGatewayConnectionRecoverableError extends Error {
	constructor(
		readonly accountKey: string | null,
		readonly recoveryCause: ForegroundGatewayRecoveryCause,
		cause: Error,
	) {
		super(`Foreground gateway for ${accountKey ?? 'no account'} is unavailable: ${recoveryCause}`, {cause});
		this.name = 'ForegroundGatewayConnectionRecoverableError';
	}
}

export class ForegroundGatewayRecoveryExhaustedError extends Error {
	constructor(
		readonly accountKey: string,
		readonly attempts: number,
		cause: Error,
	) {
		super(`Foreground gateway recovery for ${accountKey} failed after ${attempts} attempts`, {cause});
		this.name = 'ForegroundGatewayRecoveryExhaustedError';
	}
}

export class GatewayAuthenticationFailedError extends Error {
	constructor(readonly accountKey: string | null) {
		super(`Gateway authentication failed for ${accountKey ?? 'no account'}`);
		this.name = 'GatewayAuthenticationFailedError';
	}
}
