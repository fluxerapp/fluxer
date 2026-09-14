// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {User} from '@app/api/models/User';
import {UsersPendingDeletion} from '@app/api/Tables';

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
			const rows = await fetchMany<{
				user_id: bigint;
				pending_deletion_at: Date;
			}>(FETCH_USERS_PENDING_DELETION_BY_DATE_CQL, {
				deletion_date: deletionDate,
				now,
			});
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

	async findUsersPendingDeletionByDate(deletionDate: string): Promise<
		Array<{
			user_id: bigint;
			deletion_reason_code: number;
		}>
	> {
		const rows = await fetchMany<{
			user_id: bigint;
			deletion_reason_code: number;
		}>(FETCH_USERS_PENDING_DELETION_BY_DATE_ALL_CQL, {deletion_date: deletionDate});
		return rows;
	}

	async isUserPendingDeletion(userId: UserID, deletionDate: string): Promise<boolean> {
		const rows = await fetchMany<{
			user_id: bigint;
		}>(FETCH_USER_PENDING_DELETION_CHECK_CQL, {
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
