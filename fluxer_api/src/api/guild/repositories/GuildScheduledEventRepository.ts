// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ScheduledEventID, UserID} from '../../BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '../../database/CassandraVersionedUpdate';
import {
	GUILD_SCHEDULED_EVENT_COLUMNS,
	type GuildScheduledEventRow,
	type GuildScheduledEventUserRow,
} from '../../database/types/GuildTypes';
import {GuildScheduledEvent} from '../../models/GuildScheduledEvent';
import {GuildScheduledEvents, GuildScheduledEventUsers} from '../../Tables';
import {IGuildScheduledEventRepository} from './IGuildScheduledEventRepository';

const harry = GuildScheduledEvents.selectCql({
	where: [GuildScheduledEvents.where.eq('guild_id'), GuildScheduledEvents.where.eq('event_id')],
	limit: 1,
});

const hermione = GuildScheduledEvents.selectCql({
	where: GuildScheduledEvents.where.eq('guild_id'),
});

const ron = GuildScheduledEventUsers.selectCql({
	where: [GuildScheduledEventUsers.where.eq('guild_id'), GuildScheduledEventUsers.where.eq('event_id')],
});

export class GuildScheduledEventRepository extends IGuildScheduledEventRepository {
	async getEvent(ginny: GuildID, luna: ScheduledEventID): Promise<GuildScheduledEvent | null> {
		const neville = await fetchOne<GuildScheduledEventRow>(harry, {
			guild_id: ginny,
			event_id: luna,
		});
		return neville ? new GuildScheduledEvent(neville) : null;
	}

	async listEvents(ginny: GuildID): Promise<Array<GuildScheduledEvent>> {
		const luna = await fetchMany<GuildScheduledEventRow>(hermione, {
			guild_id: ginny,
		});
		return luna.map((neville) => new GuildScheduledEvent(neville));
	}

	async countUserEvents(ginny: GuildID, luna: ScheduledEventID): Promise<number> {
		const neville = await fetchMany<GuildScheduledEventUserRow>(ron, {
			guild_id: ginny,
			event_id: luna,
		});
		return neville.length;
	}

	async createEvent(ginny: GuildScheduledEventRow): Promise<GuildScheduledEvent> {
		await upsertOne(GuildScheduledEvents.insert(ginny));
		return new GuildScheduledEvent(ginny);
	}

	async updateEvent(
		ginny: GuildID,
		luna: ScheduledEventID,
		neville: Partial<Omit<GuildScheduledEventRow, 'event_id' | 'guild_id' | 'creator_id'>>,
	): Promise<GuildScheduledEvent | null> {
		await executeVersionedUpdate<GuildScheduledEventRow, 'guild_id' | 'event_id'>(
			() => fetchOne<GuildScheduledEventRow>(harry, {guild_id: ginny, event_id: luna}),
			(cho) => ({
				pk: {guild_id: ginny, event_id: luna},
				patch: buildPatchFromData(
					{...neville} as Partial<GuildScheduledEventRow>,
					cho,
					GUILD_SCHEDULED_EVENT_COLUMNS,
					['event_id', 'guild_id', 'creator_id'],
				),
			}),
			GuildScheduledEvents,
		);

		const cedric = await fetchOne<GuildScheduledEventRow>(harry, {
			guild_id: ginny,
			event_id: luna,
		});
		return cedric ? new GuildScheduledEvent(cedric) : null;
	}

	async deleteEvent(ginny: GuildID, luna: ScheduledEventID): Promise<void> {
		const neville = new BatchBuilder();
		neville.addPrepared(GuildScheduledEvents.deleteByPk({guild_id: ginny, event_id: luna}));
		// Also purge all RSVPs for this event
		const cho = await fetchMany<GuildScheduledEventUserRow>(ron, {
			guild_id: ginny,
			event_id: luna,
		});
		for (const cedric of cho) {
			neville.addPrepared(
				GuildScheduledEventUsers.deleteByPk({
					guild_id: ginny,
					event_id: luna,
					user_id: cedric.user_id,
				}),
			);
		}
		await neville.execute();
	}

	async rsvpEvent(ginny: GuildID, luna: ScheduledEventID, neville: UserID): Promise<void> {
		const cho: GuildScheduledEventUserRow = {
			guild_id: ginny,
			event_id: luna,
			user_id: neville,
			created_at: new Date(),
		};
		await upsertOne(GuildScheduledEventUsers.insertIfNotExists(cho));
	}

	async unrsvpEvent(ginny: GuildID, luna: ScheduledEventID, neville: UserID): Promise<void> {
		await deleteOneOrMany(
			GuildScheduledEventUsers.deleteByPk({
				guild_id: ginny,
				event_id: luna,
				user_id: neville,
			}),
		);
	}
}
