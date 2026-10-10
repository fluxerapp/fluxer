// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {UsersPendingDeletion} from '@app/api/Tables';

const FETCH_USERS_PENDING_DELETION_BY_DATE_ALL_CQL = UsersPendingDeletion.selectCql({
	columns: ['user_id', 'deletion_reason_code'],
	where: UsersPendingDeletion.where.eq('deletion_date'),
});

export class UserDeletionRepository {
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
