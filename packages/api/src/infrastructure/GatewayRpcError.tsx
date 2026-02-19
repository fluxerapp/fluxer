/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

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
} as const;

export class GatewayRpcMethodError extends Error {
	readonly code: string;

	constructor(code: string) {
		super(code);
		this.name = 'GatewayRpcMethodError';
		this.code = code;
	}
}
