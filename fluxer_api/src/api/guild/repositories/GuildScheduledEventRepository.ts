// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import type {GuildScheduledEventRow, GuildScheduledEventRsvpRow} from '../../database/types/GuildTypes';
import {GuildScheduledEventRsvps, GuildScheduledEvents} from '../../Tables';

// ponytail: no interface with one impl, no separate model class — Row is the return type

const FETCH_EVENTS_BY_GUILD = GuildScheduledEvents.selectCql({
	where: GuildScheduledEvents.where.eq('guild_id'),
});

const FETCH_EVENT_BY_ID = GuildScheduledEvents.selectCql({
	where: [GuildScheduledEvents.where.eq('guild_id'), GuildScheduledEvents.where.eq('scheduled_event_id')],
	limit: 1,
});

const FETCH_RSVPS_BY_EVENT = GuildScheduledEventRsvps.selectCql({
	where: [GuildScheduledEventRsvps.where.eq('guild_id'), GuildScheduledEventRsvps.where.eq('scheduled_event_id')],
});

const FETCH_RSVP_BY_USER = GuildScheduledEventRsvps.selectCql({
	where: [
		GuildScheduledEventRsvps.where.eq('guild_id'),
		GuildScheduledEventRsvps.where.eq('scheduled_event_id'),
		GuildScheduledEventRsvps.where.eq('user_id'),
	],
	limit: 1,
});

export class GuildScheduledEventRepository {
	async getEvent(scheduledEventId: ScheduledEventID, guildId: GuildID): Promise<GuildScheduledEventRow | null> {
		return fetchOne<GuildScheduledEventRow>(FETCH_EVENT_BY_ID, {
			guild_id: guildId,
			scheduled_event_id: scheduledEventId,
		});
	}

	async listEvents(guildId: GuildID): Promise<Array<GuildScheduledEventRow>> {
		return fetchMany<GuildScheduledEventRow>(FETCH_EVENTS_BY_GUILD, {guild_id: guildId});
	}

	async upsertEvent(data: GuildScheduledEventRow): Promise<void> {
		await upsertOne(GuildScheduledEvents.name, data);
	}

	async deleteEvent(guildId: GuildID, scheduledEventId: ScheduledEventID): Promise<void> {
		await deleteOneOrMany(
			GuildScheduledEvents.deleteByPk({
				guild_id: guildId,
				scheduled_event_id: scheduledEventId,
			}),
		);
	}

	async getRsvp(
		guildId: GuildID,
		eventId: ScheduledEventID,
		userId: UserID,
	): Promise<GuildScheduledEventRsvpRow | null> {
		return fetchOne<GuildScheduledEventRsvpRow>(FETCH_RSVP_BY_USER, {
			guild_id: guildId,
			scheduled_event_id: eventId,
			user_id: userId,
		});
	}

	async listRsvps(guildId: GuildID, eventId: ScheduledEventID): Promise<Array<GuildScheduledEventRsvpRow>> {
		return fetchMany<GuildScheduledEventRsvpRow>(FETCH_RSVPS_BY_EVENT, {
			guild_id: guildId,
			scheduled_event_id: eventId,
		});
	}

	async upsertRsvp(data: GuildScheduledEventRsvpRow): Promise<void> {
		await upsertOne(GuildScheduledEventRsvps.name, data);
	}

	async deleteRsvp(guildId: GuildID, eventId: ScheduledEventID, userId: UserID): Promise<void> {
		await deleteOneOrMany(
			GuildScheduledEventRsvps.deleteByPk({
				guild_id: guildId,
				scheduled_event_id: eventId,
				user_id: userId,
			}),
		);
	}
}
