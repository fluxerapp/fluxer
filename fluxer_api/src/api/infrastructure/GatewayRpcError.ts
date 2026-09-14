// SPDX-License-Identifier: AGPL-3.0-or-later

export const GatewayRpcMethodErrorCodes = {
	OVERLOADED: 'overloaded',
	INTERNAL_ERROR: 'internal_error',
	TIMEOUT: 'timeout',
	NO_RESPONDERS: 'no_responders',
	GUILD_NOT_FOUND: 'guild_not_found',
	FORBIDDEN: 'forbidden',
	CHANNEL_NOT_FOUND: 'channel_not_found',
	CHANNEL_NOT_VOICE: 'channel_not_voice',
	CALL_ALREADY_EXISTS: 'call_already_exists',
	CALL_NOT_FOUND: 'call_not_found',
	USER_NOT_IN_VOICE: 'user_not_in_voice',
	CONNECTION_NOT_FOUND: 'connection_not_found',
	MODERATOR_MISSING_CONNECT: 'moderator_missing_connect',
	TARGET_MISSING_CONNECT: 'target_missing_connect',
	INVALID_PARAMS: 'invalid_params',
	BATCH_TOO_LARGE: 'batch_too_large',
} as const;

export class GatewayRpcMethodError extends Error {
	readonly code: string;

	constructor(code: string) {
		super(code);
		this.name = 'GatewayRpcMethodError';
		this.code = code;
	}
}
