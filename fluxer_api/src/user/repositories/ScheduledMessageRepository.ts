/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {MessageID, UserID} from '~/BrandedTypes';
import {Db, deleteOneOrMany, fetchMany, fetchOne} from '~/database/Cassandra';
import type {ScheduledMessageRow} from '~/database/CassandraTypes';
import {ScheduledMessage} from '~/models/ScheduledMessage';
import {ScheduledMessages} from '~/Tables';

export class ScheduledMessageRepository {
	private readonly fetchCql = ScheduledMessages.selectCql({
		where: [ScheduledMessages.where.eq('user_id')],
	});

	async listScheduledMessages(userId: UserID, limit: number = 25): Promise<Array<ScheduledMessage>> {
		const rows = await fetchMany<ScheduledMessageRow>(this.fetchCql, {
			user_id: userId,
		});

		const messages = rows.map((row) => ScheduledMessage.fromRow(row));
		return messages.sort((a, b) => (b.id > a.id ? 1 : a.id > b.id ? -1 : 0)).slice(0, limit);
	}

	async getScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<ScheduledMessage | null> {
		const row = await fetchOne<ScheduledMessageRow>(
			ScheduledMessages.selectCql({
				where: [ScheduledMessages.where.eq('user_id'), ScheduledMessages.where.eq('scheduled_message_id')],
			}),
			{
				user_id: userId,
				scheduled_message_id: scheduledMessageId,
			},
		);
		return row ? ScheduledMessage.fromRow(row) : null;
	}

	async upsertScheduledMessage(message: ScheduledMessage, ttlSeconds: number): Promise<void> {
		await ScheduledMessages.upsertAllWithTtl(message.toRow(), ttlSeconds);
	}

	async deleteScheduledMessage(userId: UserID, scheduledMessageId: MessageID): Promise<void> {
		await deleteOneOrMany(
			ScheduledMessages.deleteByPk({
				user_id: userId,
				scheduled_message_id: scheduledMessageId,
			}),
		);
	}

	async markInvalid(userId: UserID, scheduledMessageId: MessageID, reason: string, ttlSeconds: number): Promise<void> {
		await ScheduledMessages.patchByPkWithTtl(
			{
				user_id: userId,
				scheduled_message_id: scheduledMessageId,
			},
			{
				status: Db.set('invalid'),
				status_reason: Db.set(reason),
				invalidated_at: Db.set(new Date()),
			},
			ttlSeconds,
		);
	}
}
