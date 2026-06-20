// SPDX-License-Identifier: AGPL-3.0-or-later

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
	gatewayActivity?: RpcActivityPayload | null;
	pid?: number;
	source: 'ipc' | 'process-scan';
}
