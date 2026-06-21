// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Socket} from 'node:net';

export interface DetectableExecutable {
	name: string;
	is_launcher?: boolean;
	arguments?: string;
	os?: string;
}

export interface DetectableApp {
	id: string;
	name: string;
	url?: string;
	icon?: string;
	icon_hash?: string;
	cover_image_hash?: string;
	executables?: Array<DetectableExecutable>;
	aliases?: Array<string>;
	presence_assets?: Record<string, string>;
	hook?: boolean;
	overlay?: boolean;
	client_id?: string;
}

export interface RpcActivityTimestamps {
	start?: number;
	end?: number;
}

export interface RpcActivityAssets {
	large_image?: string;
	large_text?: string;
	large_url?: string;
	small_image?: string;
	small_text?: string;
	small_url?: string;
}

export interface RpcActivityButton {
	label: string;
	url: string;
}

export interface RpcActivityParty {
	id?: string;
	size?: [number, number];
}

export interface RpcActivitySecrets {
	join?: string;
	spectate?: string;
	match?: string;
}

export interface RpcActivityMetadata {
	button_urls?: Array<string>;
}

export interface RpcActivityPayload {
	type: number;
	status_display_type?: number;
	application_id: string;
	name: string;
	details?: string;
	details_url?: string;
	state?: string;
	state_url?: string;
	timestamps?: RpcActivityTimestamps;
	assets?: RpcActivityAssets;
	buttons?: Array<RpcActivityButton>;
	party?: RpcActivityParty;
	secrets?: RpcActivitySecrets;
	metadata?: RpcActivityMetadata;
	flags?: number;
	pid?: number;
}

export interface RpcActivityUpdatePayload {
	activity: RpcActivityPayload | null;
	pid?: number;
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
