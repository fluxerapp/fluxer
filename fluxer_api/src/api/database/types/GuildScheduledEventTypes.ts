// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, GuildScheduledEventID, UserID} from '../../BrandedTypes';

type Nullish<T> = T | null;

export interface GuildScheduledEventRow {
	guild_scheduled_event_id: GuildScheduledEventID;
	guild_id: GuildID;
	channel_id: Nullish<ChannelID>;
	creator_id: UserID;
	name: string;
	description: Nullish<string>;
	scheduled_start_time: Date;
	scheduled_end_time: Nullish<Date>;
	privacy_level: number;
	status: number;
	entity_type: number;
	entity_id: Nullish<ChannelID>;
	location: Nullish<string>;
	image: Nullish<string>;
	created_at: Date;
	updated_at: Date;
	version: number;
}

export const GUILD_SCHEDULED_EVENT_COLUMNS = [
	'guild_scheduled_event_id',
	'guild_id',
	'channel_id',
	'creator_id',
	'name',
	'description',
	'scheduled_start_time',
	'scheduled_end_time',
	'privacy_level',
	'status',
	'entity_type',
	'entity_id',
	'location',
	'image',
	'created_at',
	'updated_at',
	'version',
] as const satisfies ReadonlyArray<keyof GuildScheduledEventRow>;

export interface GuildScheduledEventByGuildRow {
	guild_id: GuildID;
	scheduled_start_time: Date;
	guild_scheduled_event_id: GuildScheduledEventID;
}

export const GUILD_SCHEDULED_EVENT_BY_GUILD_COLUMNS = [
	'guild_id',
	'scheduled_start_time',
	'guild_scheduled_event_id',
] as const satisfies ReadonlyArray<keyof GuildScheduledEventByGuildRow>;

export interface GuildScheduledEventUserRow {
	user_id: UserID;
	guild_scheduled_event_id: GuildScheduledEventID;
	guild_id: GuildID;
	subscribed_at: Date;
}

export const GUILD_SCHEDULED_EVENT_USER_COLUMNS = [
	'user_id',
	'guild_scheduled_event_id',
	'guild_id',
	'subscribed_at',
] as const satisfies ReadonlyArray<keyof GuildScheduledEventUserRow>;

export interface GuildScheduledEventUserByEventRow {
	guild_scheduled_event_id: GuildScheduledEventID;
	user_id: UserID;
	guild_id: GuildID;
	subscribed_at: Date;
}

export const GUILD_SCHEDULED_EVENT_USER_BY_EVENT_COLUMNS = [
	'guild_scheduled_event_id',
	'user_id',
	'guild_id',
	'subscribed_at',
] as const satisfies ReadonlyArray<keyof GuildScheduledEventUserByEventRow>;
