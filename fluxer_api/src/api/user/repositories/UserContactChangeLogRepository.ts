// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {UserContactChangeLogRow} from '@app/api/database/types/UserTypes';
import {UserContactChangeLogs} from '@app/api/Tables';

const createListLogsQuery = (limit: number, includeCursor: boolean) =>
	UserContactChangeLogs.select({
		where: includeCursor
			? [UserContactChangeLogs.where.eq('user_id'), UserContactChangeLogs.where.lt('event_id', 'before_event_id')]
			: UserContactChangeLogs.where.eq('user_id'),
		orderBy: {col: 'event_id', direction: 'DESC'},
		limit,
	});

interface ContactChangeLogListParams {
	userId: UserID;
	limit: number;
	beforeEventId?: string;
}

interface ContactChangeLogInsertParams {
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
		const query = createListLogsQuery(limit, !!beforeEventId);
		const queryParams: {
			user_id: UserID;
			before_event_id?: string;
		} = {
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
		>(query.bind(queryParams));
		return rows.map((row) => ({
			...row,
			user_id: createUserID(row.user_id),
			actor_user_id: row.actor_user_id != null ? createUserID(row.actor_user_id) : null,
		}));
	}
}
