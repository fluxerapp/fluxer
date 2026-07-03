// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PresenceRecord} from '@app/features/gateway/types/GatewayPresenceTypes';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import type {Channel, ThreadResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildEmoji, GuildSticker} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';

export type GuildReadyData = Readonly<{
	id: string;
	properties: Omit<Guild, 'roles'>;
	channels: ReadonlyArray<Channel>;
	threads?: ReadonlyArray<ThreadResponse>;
	emojis: ReadonlyArray<GuildEmoji>;
	stickers?: ReadonlyArray<GuildSticker>;
	members: ReadonlyArray<GuildMemberData>;
	member_count: number;
	online_count?: number;
	presences?: ReadonlyArray<PresenceRecord>;
	voice_states?: ReadonlyArray<VoiceState>;
	roles: ReadonlyArray<GuildRole>;
	joined_at: string;
	unavailable?: boolean;
	unavailable_hidden?: boolean;
}>;
