// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, ScheduledEventID, UserID} from '../BrandedTypes';
import type {GuildScheduledEventRow} from '../database/types/GuildTypes';

type ScheduledEventStatus = 'SCHEDULED' | 'ACTIVE' | 'COMPLETED' | 'CANCELLED';
type ScheduledEventEntityType = 'STAGE_INSTANCE' | 'VOICE' | 'EXTERNAL';

export interface ScheduledEventEntityMetadata {
	location?: string | null;
}

export class GuildScheduledEvent {
	readonly id: ScheduledEventID;
	readonly guildId: GuildID;
	readonly channelId: ChannelID | null;
	readonly creatorId: UserID;
	readonly name: string;
	readonly description: string | null;
	readonly imageHash: string | null;
	readonly scheduledStartTime: Date;
	readonly scheduledEndTime: Date | null;
	readonly privacyLevel: 'GUILD_ONLY';
	readonly status: ScheduledEventStatus;
	readonly entityType: ScheduledEventEntityType;
	readonly entityId: bigint | null;
	readonly entityMetadata: ScheduledEventEntityMetadata | null;

	constructor(row: GuildScheduledEventRow) {
		this.id = row.event_id;
		this.guildId = row.guild_id;
		this.channelId = row.channel_id ?? null;
		this.creatorId = row.creator_id;
		this.name = row.name;
		this.description = row.description ?? null;
		this.imageHash = row.image_hash ?? null;
		this.scheduledStartTime = row.scheduled_start_time;
		this.scheduledEndTime = row.scheduled_end_time ?? null;
		this.privacyLevel = 'GUILD_ONLY';
		this.status = toStatus(row.status);
		this.entityType = toEntityType(row.entity_type);
		this.entityId = row.entity_id ?? null;
		this.entityMetadata = row.entity_location ? {location: row.entity_location} : null;
	}
}

function toStatus(value: string): ScheduledEventStatus {
	switch (value) {
		case 'ACTIVE':
			return 'ACTIVE';
		case 'COMPLETED':
			return 'COMPLETED';
		case 'CANCELLED':
			return 'CANCELLED';
		default:
			return 'SCHEDULED';
	}
}

function toEntityType(value: string): ScheduledEventEntityType {
	switch (value) {
		case 'VOICE':
			return 'VOICE';
		case 'EXTERNAL':
			return 'EXTERNAL';
		default:
			return 'STAGE_INSTANCE';
	}
}
