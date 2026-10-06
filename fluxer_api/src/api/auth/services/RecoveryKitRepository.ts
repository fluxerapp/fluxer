// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {deleteOneOrMany, executeConditional, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {UserRecoveryKitRow} from '@app/api/database/types/AuthTypes';
import {UserRecoveryKits} from '@app/api/Tables';

const FIND_RECOVERY_KIT_QUERY = UserRecoveryKits.select({
	where: UserRecoveryKits.where.eq('user_id'),
	limit: 1,
});

export class RecoveryKitRepository {
	async find(userId: UserID): Promise<UserRecoveryKitRow | null> {
		return await fetchOne<UserRecoveryKitRow>(FIND_RECOVERY_KIT_QUERY.bind({user_id: userId}));
	}

	async upsert(row: UserRecoveryKitRow): Promise<void> {
		await upsertOne(UserRecoveryKits.upsertAll(row));
	}

	async replaceIfUnchanged(params: {
		userId: UserID;
		expectedSecretHash: string;
		secretHash: string;
		createdAt: Date;
	}): Promise<boolean> {
		return await executeConditional(
			UserRecoveryKits.conditionalPatchByPk(
				{user_id: params.userId},
				{secret_hash: Db.set(params.secretHash), created_at: Db.set(params.createdAt)},
				{secret_hash: params.expectedSecretHash},
			),
		);
	}

	async delete(userId: UserID): Promise<void> {
		await deleteOneOrMany(UserRecoveryKits.deleteByPk({user_id: userId}));
	}
}
