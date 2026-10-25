// SPDX-License-Identifier: AGPL-3.0-or-later

export interface VoiceStateWire {
	guild_id: string | null;
	channel_id: string | null;
	user_id: string;
	connection_id: string | null;
}

export interface MessageWire {
	id: string;
	channel_id: string;
	guild_id?: string;
	content: string;
	author: {id: string; bot?: boolean};
}

export interface ReactionWire {
	user_id: string;
	channel_id: string;
	message_id: string;
	guild_id?: string;
	emoji: {name: string; id: string | null};
}

export interface Embed {
	title?: string;
	url?: string;
	description?: string;
	thumbnail?: {url: string};
	footer?: {text: string};
}
