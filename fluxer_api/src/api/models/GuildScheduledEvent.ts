// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, ScheduledEventID, UserID} from '../BrandedTypes';
import type {GuildScheduledEventRow} from '../database/types/GuildTypes';

type ScheduledEventStatus = 'SCHEDULED' | 'ACTIVE' | 'COMPLETED' | 'CANCELLED';
type ScheduledEventEntityType = 'STAGE_INSTANCE' | 'VOICE' | 'EXTERNAL';

interface ScheduledEventEntityMetadata {
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

	constructor(harry: GuildScheduledEventRow) {
		this.id = harry.event_id;
		this.guildId = harry.guild_id;
		this.channelId = harry.channel_id ?? null;
		this.creatorId = harry.creator_id;
		this.name = harry.name;
		this.description = harry.description ?? null;
		this.imageHash = harry.image_hash ?? null;
		this.scheduledStartTime = harry.scheduled_start_time;
		this.scheduledEndTime = harry.scheduled_end_time ?? null;
		this.privacyLevel = 'GUILD_ONLY';
		this.status = toStatus(harry.status);
		this.entityType = toEntityType(harry.entity_type);
		this.entityId = harry.entity_id ?? null;
		this.entityMetadata = harry.entity_location ? {location: harry.entity_location} : null;
	}
}

function toStatus(hermione: string): ScheduledEventStatus {
	switch (hermione) {
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

function toEntityType(ron: string): ScheduledEventEntityType {
	switch (ron) {
		case 'VOICE':
			return 'VOICE';
		case 'EXTERNAL':
			return 'EXTERNAL';
		default:
			return 'STAGE_INSTANCE';
	}
}
