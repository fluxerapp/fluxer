// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import type {GuildScheduledEventRow} from '../../database/types/GuildTypes';
import type {GuildScheduledEvent} from '../../models/GuildScheduledEvent';

export abstract class IGuildScheduledEventRepository {
	abstract getEvent(guildId: GuildID, eventId: ScheduledEventID): Promise<GuildScheduledEvent | null>;

	abstract listEvents(guildId: GuildID): Promise<Array<GuildScheduledEvent>>;

	abstract countUserEvents(guildId: GuildID, eventId: ScheduledEventID): Promise<number>;

	abstract createEvent(data: GuildScheduledEventRow): Promise<GuildScheduledEvent>;

	abstract updateEvent(
		guildId: GuildID,
		eventId: ScheduledEventID,
		data: Partial<Omit<GuildScheduledEventRow, 'event_id' | 'guild_id' | 'creator_id'>>,
	): Promise<GuildScheduledEvent | null>;

	abstract deleteEvent(guildId: GuildID, eventId: ScheduledEventID): Promise<void>;

	abstract rsvpEvent(guildId: GuildID, eventId: ScheduledEventID, userId: UserID): Promise<void>;

	abstract unrsvpEvent(guildId: GuildID, eventId: ScheduledEventID, userId: UserID): Promise<void>;
}
