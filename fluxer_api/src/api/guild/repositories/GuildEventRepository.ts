// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildEventID, GuildID} from '@app/api/BrandedTypes';
import {fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {buildPatchFromData, executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import {GUILD_EVENT_COLUMNS, type GuildEventRow} from '@app/api/database/types/GuildTypes';
import {GuildEvent} from '@app/api/models/GuildEvent';
import {GuildEvents} from '@app/api/Tables';

const FETCH_GUILD_EVENTS_QUERY = GuildEvents.selectCql({
	where: GuildEvents.where.eq('guild_id'),
});
const FETCH_GUILD_EVENT_QUERY = GuildEvents.selectCql({
	where: [GuildEvents.where.eq('guild_id'), GuildEvents.where.eq('event_id')],
	limit: 1,
});

export class GuildEventRepository {
	async get(guildId: GuildID, eventId: GuildEventID): Promise<GuildEvent | null> {
		const row = await fetchOne<GuildEventRow>(FETCH_GUILD_EVENT_QUERY, {
			guild_id: guildId,
			event_id: eventId,
		});
		return row ? new GuildEvent(row) : null;
	}

	async list(guildId: GuildID): Promise<Array<GuildEvent>> {
		const rows = await fetchMany<GuildEventRow>(FETCH_GUILD_EVENTS_QUERY, {guild_id: guildId});
		return rows.map((row) => new GuildEvent(row)).sort((a, b) => a.startsAt.getTime() - b.startsAt.getTime());
	}

	async upsert(data: GuildEventRow, oldData?: GuildEventRow | null): Promise<GuildEvent> {
		const result = await executeVersionedUpdate<GuildEventRow, 'guild_id' | 'event_id'>(
			async () =>
				fetchOne<GuildEventRow>(FETCH_GUILD_EVENT_QUERY, {
					guild_id: data.guild_id,
					event_id: data.event_id,
				}),
			(current) => ({
				pk: {guild_id: data.guild_id, event_id: data.event_id},
				patch: buildPatchFromData(data, current, GUILD_EVENT_COLUMNS, ['guild_id', 'event_id']),
			}),
			GuildEvents,
			{initialData: oldData},
		);
		return new GuildEvent({...data, version: result.finalVersion ?? 1});
	}

	async delete(guildId: GuildID, eventId: GuildEventID): Promise<void> {
		await GuildEvents.deleteByPk({guild_id: guildId, event_id: eventId}).execute();
	}
}
