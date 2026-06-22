// SPDX-License-Identifier: AGPL-3.0-or-later

export const IPC_MAX_RETRIES = 9;
export const IPC_SOCKET_NAME = 'discord-ipc';

export const RPC_PROTOCOL_VERSION = 1;
export const ACTIVITY_FLAG_INSTANCE = 1 << 0;

export const DISCORD_CDN_HOST = 'cdn.discordapp.com';

export enum IPCMessageType {
	HANDSHAKE = 0,
	FRAME = 1,
	CLOSE = 2,
	PING = 3,
	PONG = 4,
}

export enum IPCCloseCode {
	CLOSE_NORMAL = 1000,
	CLOSE_UNSUPPORTED = 1003,
	CLOSE_ABNORMAL = 1006,
}

export enum IPCErrorCode {
	INVALID_CLIENTID = 4000,
	INVALID_ORIGIN = 4001,
	RATELIMITED = 4002,
	TOKEN_REVOKED = 4003,
	INVALID_VERSION = 4004,
	INVALID_ENCODING = 4005,
}

export enum RPCCommand {
	DISPATCH = 'DISPATCH',
	SET_ACTIVITY = 'SET_ACTIVITY',
	SUBSCRIBE = 'SUBSCRIBE',
	UNSUBSCRIBE = 'UNSUBSCRIBE',
}

export enum RPCEvent {
	READY = 'READY',
	ERROR = 'ERROR',
}

export enum ActivityType {
	PLAYING = 0,
	LISTENING = 2,
	WATCHING = 3,
	COMPETING = 5,
}
