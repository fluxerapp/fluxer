// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, GuildScheduledEventID, UserID} from '../BrandedTypes';
import type {
	GuildScheduledEventEntityTypeValue,
	GuildScheduledEventPrivacyLevelValue,
	GuildScheduledEventStatusValue,
} from '@fluxer/constants/src/GuildScheduledEventConstants';
import type {GuildScheduledEventRow} from '../database/types/GuildScheduledEventTypes';

export class GuildScheduledEvent {
	readonly id: GuildScheduledEventID;
	readonly guildId: GuildID;
	readonly channelId: ChannelID | null;
	readonly creatorId: UserID;
	readonly name: string;
	readonly description: string | null;
	readonly scheduledStartTime: Date;
	readonly scheduledEndTime: Date | null;
	readonly privacyLevel: GuildScheduledEventPrivacyLevelValue;
	readonly status: GuildScheduledEventStatusValue;
	readonly entityType: GuildScheduledEventEntityTypeValue;
	readonly entityId: ChannelID | null;
	readonly location: string | null;
	readonly image: string | null;
	readonly createdAt: Date;
	readonly updatedAt: Date;
	readonly version: number;

	constructor(row: GuildScheduledEventRow) {
		this.id = row.guild_scheduled_event_id;
		this.guildId = row.guild_id;
		this.channelId = row.channel_id ?? null;
		this.creatorId = row.creator_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.scheduledStartTime = row.scheduled_start_time;
		this.scheduledEndTime = row.scheduled_end_time ?? null;
		this.privacyLevel = row.privacy_level as GuildScheduledEventPrivacyLevelValue;
		this.status = row.status as GuildScheduledEventStatusValue;
		this.entityType = row.entity_type as GuildScheduledEventEntityTypeValue;
		this.entityId = row.entity_id ?? null;
		this.location = row.location ?? null;
		this.image = row.image ?? null;
		this.createdAt = row.created_at;
		this.updatedAt = row.updated_at;
		this.version = row.version;
	}

	toRow(): GuildScheduledEventRow {
		return {
			guild_scheduled_event_id: this.id,
			guild_id: this.guildId,
			channel_id: this.channelId,
			creator_id: this.creatorId,
			name: this.name,
			description: this.description,
			scheduled_start_time: this.scheduledStartTime,
			scheduled_end_time: this.scheduledEndTime,
			privacy_level: this.privacyLevel,
			status: this.status,
			entity_type: this.entityType,
			entity_id: this.entityId,
			location: this.location,
			image: this.image,
			created_at: this.createdAt,
			updated_at: this.updatedAt,
			version: this.version,
		};
	}

	toResponse(subscriberCount?: number, guild?: {id: string; name: string; icon?: string | null}) {
		return {
			id: this.id.toString(),
			guild_id: this.guildId.toString(),
			channel_id: this.channelId?.toString() ?? null,
			creator_id: this.creatorId.toString(),
			name: this.name,
			description: this.description,
			scheduled_start_time: this.scheduledStartTime.toISOString(),
			scheduled_end_time: this.scheduledEndTime?.toISOString() ?? null,
			privacy_level: this.privacyLevel,
			status: this.status,
			entity_type: this.entityType,
			entity_id: this.entityId?.toString() ?? null,
			location: this.location,
			image: this.image,
			created_at: this.createdAt.toISOString(),
			updated_at: this.updatedAt.toISOString(),
			version: this.version,
			subscriber_count: subscriberCount ?? 0,
			...(guild && {
				guild: {
					id: guild.id,
					name: guild.name,
					icon: guild.icon ?? null,
				},
			}),
		};
	}
}
