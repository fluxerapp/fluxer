// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Socket} from 'node:net';

export interface RpcActivityTimestamps {
	start?: number;
	end?: number;
}

export interface RpcActivityAssets {
	large_image?: string;
	large_text?: string;
	small_image?: string;
	small_text?: string;
}

export interface RpcActivityPayload {
	type: number;
	application_id: string;
	name: string;
	details?: string;
	state?: string;
	timestamps?: RpcActivityTimestamps;
	assets?: RpcActivityAssets;
	flags?: number;
	pid?: number;
}

export interface RpcActivityUpdatePayload {
	activity: RpcActivityPayload | null;
	pid?: number;
	receivedAt?: number;
	source: 'ipc' | 'process-scan';
}

export interface SetActivityArgs {
	pid?: number;
	activity: Record<string, unknown> | null;
}

export interface RPCMessage {
	cmd?: string;
	args?: unknown;
	evt?: string | null;
	nonce?: string | null;
	data?: unknown;
}

export interface ExtendedSocket extends Socket {
	_handshook?: boolean;
	_readBuffer?: Buffer;
	clientId?: string;
	clientName?: string;
	lastPid?: number;
	socketId?: number;
	send?: (msg: RPCMessage) => void;
	close?: (code?: number, message?: string) => void;
}
