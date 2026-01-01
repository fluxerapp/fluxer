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

import {createUserID, type UserID} from '~/BrandedTypes';
import {fetchMany, upsertOne} from '~/database/Cassandra';
import type {UserContactChangeLogRow} from '~/database/CassandraTypes';
import {UserContactChangeLogs} from '~/Tables';

const createListLogsCql = (limit: number, includeCursor: boolean) =>
	UserContactChangeLogs.selectCql({
		where: includeCursor
			? [UserContactChangeLogs.where.eq('user_id'), UserContactChangeLogs.where.lt('event_id', 'before_event_id')]
			: UserContactChangeLogs.where.eq('user_id'),
		orderBy: {col: 'event_id', direction: 'DESC'},
		limit,
	});

export interface ContactChangeLogListParams {
	userId: UserID;
	limit: number;
	beforeEventId?: string;
}

export interface ContactChangeLogInsertParams {
	userId: UserID;
	field: string;
	oldValue: string | null;
	newValue: string | null;
	reason: string;
	actorUserId: UserID | null;
	eventAt?: Date;
}

export class UserContactChangeLogRepository {
	async insertLog(params: ContactChangeLogInsertParams): Promise<void> {
		const eventAt = params.eventAt ?? new Date();
		await upsertOne(
			UserContactChangeLogs.insertWithNow(
				{
					user_id: params.userId,
					field: params.field,
					old_value: params.oldValue,
					new_value: params.newValue,
					reason: params.reason,
					actor_user_id: params.actorUserId,
					event_at: eventAt,
				},
				'event_id',
			),
		);
	}

	async listLogs(params: ContactChangeLogListParams): Promise<Array<UserContactChangeLogRow>> {
		const {userId, limit, beforeEventId} = params;
		const query = createListLogsCql(limit, !!beforeEventId);
		const queryParams: {user_id: UserID; before_event_id?: string} = {
			user_id: userId,
		};
		if (beforeEventId) {
			queryParams.before_event_id = beforeEventId;
		}
		const rows = await fetchMany<
			Omit<UserContactChangeLogRow, 'user_id' | 'actor_user_id'> & {
				user_id: bigint;
				actor_user_id: bigint | null;
			}
		>(query, queryParams);
		return rows.map((row) => ({
			...row,
			user_id: createUserID(row.user_id),
			actor_user_id: row.actor_user_id != null ? createUserID(row.actor_user_id) : null,
		}));
	}
}
