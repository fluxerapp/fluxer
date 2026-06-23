// SPDX-License-Identifier: AGPL-3.0-or-later

interface RpcActivityTimestamps {
	start?: number;
	end?: number;
}

interface RpcActivityAssets {
	large_image?: string;
	large_text?: string;
	large_url?: string;
	small_image?: string;
	small_text?: string;
	small_url?: string;
}

interface RpcActivityButton {
	label: string;
	url: string;
}

interface RpcActivityParty {
	id?: string;
	size?: [number, number];
}

interface RpcActivitySecrets {
	join?: string;
	spectate?: string;
	match?: string;
}

interface RpcActivityMetadata {
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
	gatewayActivity?: RpcActivityPayload | null;
	pid?: number;
	source: 'ipc' | 'process-scan';
}
