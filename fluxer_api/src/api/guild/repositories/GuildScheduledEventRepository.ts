// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import {
	GUILD_SCHEDULED_EVENT_COLUMNS,
	type GuildScheduledEventRow,
	type GuildScheduledEventUserRow,
} from '../../database/types/GuildTypes';
import {GuildScheduledEvent} from '../../models/GuildScheduledEvent';
import {GuildScheduledEvents, GuildScheduledEventUsers} from '../../Tables';
import {IGuildScheduledEventRepository} from './IGuildScheduledEventRepository';
import {Db} from '../../database/CassandraTypes';

const FETCH_EVENT_QUERY = GuildScheduledEvents.selectCql({
	where: [GuildScheduledEvents.where.eq('guild_id'), GuildScheduledEvents.where.eq('event_id')],
	limit: 1,
});

const FETCH_EVENTS_BY_GUILD_QUERY = GuildScheduledEvents.selectCql({
	where: GuildScheduledEvents.where.eq('guild_id'),
});

const FETCH_EVENT_USERS_QUERY = GuildScheduledEventUsers.selectCql({
	where: [
		GuildScheduledEventUsers.where.eq('guild_id'),
		GuildScheduledEventUsers.where.eq('event_id'),
	],
});

export class GuildScheduledEventRepository extends IGuildScheduledEventRepository {
	async getEvent(guildId: GuildID, eventId: ScheduledEventID): Promise<GuildScheduledEvent | null> {
		const row = await fetchOne<GuildScheduledEventRow>(FETCH_EVENT_QUERY, {
			guild_id: guildId,
			event_id: eventId,
		});
		return row ? new GuildScheduledEvent(row) : null;
	}

	async listEvents(guildId: GuildID): Promise<Array<GuildScheduledEvent>> {
		const rows = await fetchMany<GuildScheduledEventRow>(FETCH_EVENTS_BY_GUILD_QUERY, {
			guild_id: guildId,
		});
		return rows.map((row) => new GuildScheduledEvent(row));
	}

	async countUserEvents(guildId: GuildID, eventId: ScheduledEventID): Promise<number> {
		const rows = await fetchMany<GuildScheduledEventUserRow>(FETCH_EVENT_USERS_QUERY, {
			guild_id: guildId,
			event_id: eventId,
		});
		return rows.length;
	}

	async createEvent(data: GuildScheduledEventRow): Promise<GuildScheduledEvent> {
		await upsertOne(GuildScheduledEvents.insert(data));
		return new GuildScheduledEvent(data);
	}

	async updateEvent(
		guildId: GuildID,
		eventId: ScheduledEventID,
		data: Partial<Omit<GuildScheduledEventRow, 'event_id' | 'guild_id' | 'creator_id'>>,
	): Promise<GuildScheduledEvent | null> {
		const result = await executeVersionedUpdate<GuildScheduledEventRow, 'guild_id' | 'event_id'>(
			() => fetchOne<GuildScheduledEventRow>(FETCH_EVENT_QUERY, {guild_id: guildId, event_id: eventId}),
			(current) => ({
				pk: {guild_id: guildId, event_id: eventId},
				patch: buildPatchFromData(
					{...data} as Partial<GuildScheduledEventRow>,
					current,
					GUILD_SCHEDULED_EVENT_COLUMNS,
					['event_id', 'guild_id', 'creator_id'],
				),
			}),
			GuildScheduledEvents,
		);

		const updated = await fetchOne<GuildScheduledEventRow>(FETCH_EVENT_QUERY, {
			guild_id: guildId,
			event_id: eventId,
		});
		return updated ? new GuildScheduledEvent(updated) : null;
	}

	async deleteEvent(guildId: GuildID, eventId: ScheduledEventID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(GuildScheduledEvents.deleteByPk({guild_id: guildId, event_id: eventId}));
		// Also purge all RSVPs for this event
		const userRows = await fetchMany<GuildScheduledEventUserRow>(FETCH_EVENT_USERS_QUERY, {
			guild_id: guildId,
			event_id: eventId,
		});
		for (const userRow of userRows) {
			batch.addPrepared(
				GuildScheduledEventUsers.deleteByPk({
					guild_id: guildId,
					event_id: eventId,
					user_id: userRow.user_id,
				}),
			);
		}
		await batch.execute();
	}

	async rsvpEvent(guildId: GuildID, eventId: ScheduledEventID, userId: UserID): Promise<void> {
		const row: GuildScheduledEventUserRow = {
			guild_id: guildId,
			event_id: eventId,
			user_id: userId,
			created_at: new Date(),
		};
		await upsertOne(GuildScheduledEventUsers.insertIfNotExists(row));
	}

	async unrsvpEvent(guildId: GuildID, eventId: ScheduledEventID, userId: UserID): Promise<void> {
		await upsertOne(
			GuildScheduledEventUsers.deleteByPk({
				guild_id: guildId,
				event_id: eventId,
				user_id: userId,
			}),
		);
	}
}
