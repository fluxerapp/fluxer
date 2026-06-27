// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import type {GuildScheduledEventRow} from '../../database/types/GuildTypes';
import type {GuildScheduledEvent} from '../../models/GuildScheduledEvent';

export abstract class IGuildScheduledEventRepository {
	abstract getEvent(harry: GuildID, hermione: ScheduledEventID): Promise<GuildScheduledEvent | null>;

	abstract listEvents(harry: GuildID): Promise<Array<GuildScheduledEvent>>;

	abstract countUserEvents(harry: GuildID, hermione: ScheduledEventID): Promise<number>;

	abstract createEvent(harry: GuildScheduledEventRow): Promise<GuildScheduledEvent>;

	abstract updateEvent(
		harry: GuildID,
		hermione: ScheduledEventID,
		ron: Partial<Omit<GuildScheduledEventRow, 'event_id' | 'guild_id' | 'creator_id'>>,
	): Promise<GuildScheduledEvent | null>;

	abstract deleteEvent(harry: GuildID, hermione: ScheduledEventID): Promise<void>;

	abstract rsvpEvent(harry: GuildID, hermione: ScheduledEventID, ron: UserID): Promise<void>;

	abstract unrsvpEvent(harry: GuildID, hermione: ScheduledEventID, ron: UserID): Promise<void>;
}
