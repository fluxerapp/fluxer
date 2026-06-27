// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, GuildScheduledEventID, UserID} from '../../BrandedTypes';
import {fetchMany, fetchOne, upsertOne, deleteOneOrMany} from '../../database/CassandraQueryExecution';
import type {
	GuildScheduledEventRow,
	GuildScheduledEventByGuildRow,
	GuildScheduledEventUserRow,
	GuildScheduledEventUserByEventRow,
} from '../../database/types/GuildScheduledEventTypes';
import {GuildScheduledEvent} from '../../models/GuildScheduledEvent';
import {
	GuildScheduledEvents,
	GuildScheduledEventsByGuild,
	GuildScheduledEventUsers,
	GuildScheduledEventUsersByEvent,
} from '../../Tables';

export class GuildScheduledEventRepository {
	async getEvent(eventId: GuildScheduledEventID): Promise<GuildScheduledEvent | null> {
		const row = await fetchOne<GuildScheduledEventRow>(
			GuildScheduledEvents.selectCql({
				where: [GuildScheduledEvents.where.eq('guild_scheduled_event_id')],
			}),
			{guild_scheduled_event_id: eventId},
		);
		return row ? new GuildScheduledEvent(row) : null;
	}

	async listEventsByGuild(
		guildId: GuildID,
		limit: number = 100,
	): Promise<Array<GuildScheduledEvent>> {
		const rows = await fetchMany<GuildScheduledEventByGuildRow>(
			GuildScheduledEventsByGuild.selectCql({
				where: [GuildScheduledEventsByGuild.where.eq('guild_id')],
			}),
			{guild_id: guildId},
		);
		const events: GuildScheduledEvent[] = [];
		for (const row of rows) {
			const event = await this.getEvent(row.guild_scheduled_event_id);
			if (event) {
				events.push(event);
			}
		}
		return events.slice(0, limit);
	}

	async listEventsByUser(
		userId: UserID,
		limit: number = 100,
	): Promise<Array<GuildScheduledEvent>> {
		const rows = await fetchMany<GuildScheduledEventUserRow>(
			GuildScheduledEventUsers.selectCql({
				where: [GuildScheduledEventUsers.where.eq('user_id')],
			}),
			{user_id: userId},
		);
		const events: GuildScheduledEvent[] = [];
		for (const row of rows) {
			const event = await this.getEvent(row.guild_scheduled_event_id);
			if (event) {
				events.push(event);
			}
		}
		return events.slice(0, limit);
	}

	async upsertEvent(event: GuildScheduledEvent): Promise<void> {
		const row = event.toRow();
		await upsertOne(GuildScheduledEvents.upsertAll(row));
		await upsertOne(
			GuildScheduledEventsByGuild.upsertAll({
				guild_id: event.guildId,
				scheduled_start_time: event.scheduledStartTime,
				guild_scheduled_event_id: event.id,
			}),
		);
	}

	async deleteEvent(event: GuildScheduledEvent): Promise<void> {
		await deleteOneOrMany(
			GuildScheduledEvents.deleteByPk({
				guild_scheduled_event_id: event.id,
			}),
		);
		await deleteOneOrMany(
			GuildScheduledEventsByGuild.deleteByPk({
				guild_id: event.guildId,
				scheduled_start_time: event.scheduledStartTime,
				guild_scheduled_event_id: event.id,
			}),
		);
	}

	async subscribeUser(
		userId: UserID,
		eventId: GuildScheduledEventID,
		guildId: GuildID,
	): Promise<void> {
		const now = new Date();
		await upsertOne(
			GuildScheduledEventUsers.upsertAll({
				user_id: userId,
				guild_scheduled_event_id: eventId,
				guild_id: guildId,
				subscribed_at: now,
			}),
		);
		await upsertOne(
			GuildScheduledEventUsersByEvent.upsertAll({
				guild_scheduled_event_id: eventId,
				user_id: userId,
				guild_id: guildId,
				subscribed_at: now,
			}),
		);
	}

	async unsubscribeUser(
		userId: UserID,
		eventId: GuildScheduledEventID,
		guildId: GuildID,
	): Promise<void> {
		await deleteOneOrMany(
			GuildScheduledEventUsers.deleteByPk({
				user_id: userId,
				guild_scheduled_event_id: eventId,
			}),
		);
		await deleteOneOrMany(
			GuildScheduledEventUsersByEvent.deleteByPk({
				guild_scheduled_event_id: eventId,
				user_id: userId,
			}),
		);
	}

	async isUserSubscribed(
		userId: UserID,
		eventId: GuildScheduledEventID,
	): Promise<boolean> {
		const row = await fetchOne<GuildScheduledEventUserRow>(
			GuildScheduledEventUsers.selectCql({
				where: [GuildScheduledEventUsers.where.eq('user_id'), GuildScheduledEventUsers.where.eq('guild_scheduled_event_id')],
			}),
			{user_id: userId, guild_scheduled_event_id: eventId},
		);
		return row !== null;
	}

	async getSubscriberCount(eventId: GuildScheduledEventID): Promise<number> {
		const rows = await fetchMany<GuildScheduledEventUserByEventRow>(
			GuildScheduledEventUsersByEvent.selectCql({
				where: [GuildScheduledEventUsersByEvent.where.eq('guild_scheduled_event_id')],
			}),
			{guild_scheduled_event_id: eventId},
		);
		return rows.length;
	}

	async listSubscribers(
		eventId: GuildScheduledEventID,
		limit: number = 100,
	): Promise<Array<UserID>> {
		const rows = await fetchMany<GuildScheduledEventUserByEventRow>(
			GuildScheduledEventUsersByEvent.selectCql({
				where: [GuildScheduledEventUsersByEvent.where.eq('guild_scheduled_event_id')],
			}),
			{guild_scheduled_event_id: eventId},
		);
		return rows.map((row) => row.user_id).slice(0, limit);
	}
}
