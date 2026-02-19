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

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GatewayOpcodes = {
	DISPATCH: 0,
	HEARTBEAT: 1,
	IDENTIFY: 2,
	PRESENCE_UPDATE: 3,
	VOICE_STATE_UPDATE: 4,
	VOICE_SERVER_PING: 5,
	RESUME: 6,
	RECONNECT: 7,
	REQUEST_GUILD_MEMBERS: 8,
	INVALID_SESSION: 9,
	HELLO: 10,
	HEARTBEAT_ACK: 11,
	GATEWAY_ERROR: 12,
	LAZY_REQUEST: 14,
} as const;

export const LARGE_GUILD_THRESHOLD = 250;
export const MEMBER_CHUNK_SIZE = 1000;

export const GatewayIdentifyFlags = {
	DEBOUNCE_MESSAGE_REACTIONS: 1 << 1,
} as const;

export const GatewayCloseCodes = {
	UNKNOWN_ERROR: 4000,
	UNKNOWN_OPCODE: 4001,
	DECODE_ERROR: 4002,
	NOT_AUTHENTICATED: 4003,
	AUTHENTICATION_FAILED: 4004,
	ALREADY_AUTHENTICATED: 4005,
	INVALID_SEQ: 4007,
	RATE_LIMITED: 4008,
	SESSION_TIMEOUT: 4009,
	INVALID_SHARD: 4010,
	SHARDING_REQUIRED: 4011,
	INVALID_API_VERSION: 4012,
} as const;

export const GatewayErrorCodes = {
	DM_INVALID_CHANNEL_TYPE: 'DM_INVALID_CHANNEL_TYPE',
	DM_NOT_RECIPIENT: 'DM_NOT_RECIPIENT',
	UNKNOWN_ERROR: 'UNKNOWN_ERROR',
	VOICE_CHANNEL_FULL: 'VOICE_CHANNEL_FULL',
	VOICE_CHANNEL_NOT_FOUND: 'VOICE_CHANNEL_NOT_FOUND',
	VOICE_CONNECTION_NOT_FOUND: 'VOICE_CONNECTION_NOT_FOUND',
	VOICE_GUILD_ID_MISSING: 'VOICE_GUILD_ID_MISSING',
	VOICE_GUILD_NOT_FOUND: 'VOICE_GUILD_NOT_FOUND',
	VOICE_INVALID_CHANNEL_ID: 'VOICE_INVALID_CHANNEL_ID',
	VOICE_INVALID_CHANNEL_TYPE: 'VOICE_INVALID_CHANNEL_TYPE',
	VOICE_INVALID_GUILD_ID: 'VOICE_INVALID_GUILD_ID',
	VOICE_INVALID_STATE: 'VOICE_INVALID_STATE',
	VOICE_INVALID_USER_ID: 'VOICE_INVALID_USER_ID',
	VOICE_MEMBER_NOT_FOUND: 'VOICE_MEMBER_NOT_FOUND',
	VOICE_MEMBER_TIMED_OUT: 'VOICE_MEMBER_TIMED_OUT',
	VOICE_MISSING_CONNECTION_ID: 'VOICE_MISSING_CONNECTION_ID',
	VOICE_PERMISSION_DENIED: 'VOICE_PERMISSION_DENIED',
	VOICE_TOKEN_FAILED: 'VOICE_TOKEN_FAILED',
	VOICE_UNCLAIMED_ACCOUNT: 'VOICE_UNCLAIMED_ACCOUNT',
	VOICE_USER_MISMATCH: 'VOICE_USER_MISMATCH',
	VOICE_USER_NOT_IN_VOICE: 'VOICE_USER_NOT_IN_VOICE',
} as const;
export type GatewayErrorCode = ValueOf<typeof GatewayErrorCodes>;
