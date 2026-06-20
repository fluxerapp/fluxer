// SPDX-License-Identifier: AGPL-3.0-or-later

export const IPC_MAX_RETRIES = 9;
export const IPC_HEADER_SIZE = 8;
export const IPC_MESSAGE_TYPE_MAX = 5;
export const MAX_IPC_PAYLOAD = 1024 * 1024;
export const IPC_SOCKET_NAME = 'discord-ipc';
export const UNIX_TEMP_DIR_FALLBACK = '/tmp';
export const SOCKET_AVAILABILITY_TIMEOUT = 1000;

export const RPC_PROTOCOL_VERSION = 1;
export const ACTIVITY_FLAG_INSTANCE = 1 << 0;

export const PROCESS_SCAN_INTERVAL = 15000;
export const LOST_GAME_MISS_THRESHOLD = 2;
export const EXECUTABLE_ARCH_SUFFIXES = ['64', '.x64', 'x64', '_64'] as const;
export const EXECUTABLE_EXACT_MATCH_PREFIX = '>';
export const LINUX_PROC_DIR = '/proc';
export const CMDLINE_NULL_SEPARATOR = '\0';

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

export const ANTI_CHEAT_EXECUTABLES = [
	'easyanticheat',
	'eac_launcher',
	'easyanticheat_eos',
	'battleye',
	'beclient',
	'nprotect',
	'xigncode',
	'gameguard',
	'vanguard',
	'anticheattoolkit',
];

export const STEAM_RUNTIME_PATHS = [
	'SteamLinuxRuntime',
	'Proton',
	'pressure-vessel',
	'steam-runtime',
	'compatibilitytools.d',
];
