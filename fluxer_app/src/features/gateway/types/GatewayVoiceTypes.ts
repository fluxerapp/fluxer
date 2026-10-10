// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

export interface VoiceState {
	guild_id: string;
	channel_id: string | null;
	user_id: string;
	connection_id: string;
	session_id?: string;
	is_mobile?: boolean;
	mute: boolean;
	deaf: boolean;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	viewer_stream_keys?: ReadonlyArray<string> | null;
	suppress?: boolean;
	member?: GuildMemberData;
	e2ee_capable?: boolean;
	p2p?: boolean;
}

export interface CallVoiceState {
	user_id: string;
	channel_id?: string | null;
	session_id?: string;
	self_mute?: boolean;
	self_deaf?: boolean;
	self_video?: boolean;
	self_stream?: boolean;
}
