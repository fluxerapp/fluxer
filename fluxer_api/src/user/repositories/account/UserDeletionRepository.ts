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
import {deleteOneOrMany, fetchMany, upsertOne} from '~/database/Cassandra';
import type {User} from '~/Models';
import {UsersPendingDeletion} from '~/Tables';

const FETCH_USERS_PENDING_DELETION_BY_DATE_CQL = UsersPendingDeletion.selectCql({
	columns: ['user_id', 'pending_deletion_at'],
	where: [UsersPendingDeletion.where.eq('deletion_date'), UsersPendingDeletion.where.lte('pending_deletion_at', 'now')],
});

const FETCH_USERS_PENDING_DELETION_BY_DATE_ALL_CQL = UsersPendingDeletion.selectCql({
	columns: ['user_id', 'deletion_reason_code'],
	where: UsersPendingDeletion.where.eq('deletion_date'),
});

const FETCH_USER_PENDING_DELETION_CHECK_CQL = UsersPendingDeletion.selectCql({
	columns: ['user_id'],
	where: [UsersPendingDeletion.where.eq('deletion_date'), UsersPendingDeletion.where.eq('user_id')],
});

export class UserDeletionRepository {
	constructor(private findUniqueUser: (userId: UserID) => Promise<User | null>) {}

	async addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		const deletionDate = pendingDeletionAt.toISOString().split('T')[0];

		await upsertOne(
			UsersPendingDeletion.upsertAll({
				deletion_date: deletionDate,
				pending_deletion_at: pendingDeletionAt,
				user_id: userId,
				deletion_reason_code: deletionReasonCode,
			}),
		);
	}

	async findUsersPendingDeletion(now: Date): Promise<Array<User>> {
		const userIds = new Set<bigint>();

		const startDate = new Date(now);
		startDate.setDate(startDate.getDate() - 30);

		const currentDate = new Date(startDate);
		while (currentDate <= now) {
			const deletionDate = currentDate.toISOString().split('T')[0];

			const rows = await fetchMany<{user_id: bigint; pending_deletion_at: Date}>(
				FETCH_USERS_PENDING_DELETION_BY_DATE_CQL,
				{
					deletion_date: deletionDate,
					now,
				},
			);

			for (const row of rows) {
				userIds.add(row.user_id);
			}

			currentDate.setDate(currentDate.getDate() + 1);
		}

		const users: Array<User> = [];
		for (const userId of userIds) {
			const user = await this.findUniqueUser(createUserID(userId));
			if (user?.pendingDeletionAt && user.pendingDeletionAt <= now) {
				users.push(user);
			}
		}

		return users;
	}

	async findUsersPendingDeletionByDate(
		deletionDate: string,
	): Promise<Array<{user_id: bigint; deletion_reason_code: number}>> {
		const rows = await fetchMany<{user_id: bigint; deletion_reason_code: number}>(
			FETCH_USERS_PENDING_DELETION_BY_DATE_ALL_CQL,
			{deletion_date: deletionDate},
		);
		return rows;
	}

	async isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean> {
		const rows = await fetchMany<{user_id: bigint}>(FETCH_USER_PENDING_DELETION_CHECK_CQL, {
			deletion_date: deletionDate,
			user_id: userId,
		});
		return rows.length > 0;
	}

	async removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void> {
		const deletionDate = pendingDeletionAt.toISOString().split('T')[0];

		await deleteOneOrMany(
			UsersPendingDeletion.deleteByPk({
				deletion_date: deletionDate,
				pending_deletion_at: pendingDeletionAt,
				user_id: userId,
			}),
		);
	}

	async scheduleDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void> {
		return this.addPendingDeletion(userId, pendingDeletionAt, deletionReasonCode);
	}
}
