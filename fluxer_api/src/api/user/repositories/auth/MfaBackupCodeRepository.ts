// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMfaBackupCode, type UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import type {MfaBackupCodeRow} from '@app/api/database/types/AuthTypes';
import {MfaBackupCode} from '@app/api/models/MfaBackupCode';
import {MfaBackupCodes} from '@app/api/Tables';

const FETCH_MFA_BACKUP_CODES_CQL = MfaBackupCodes.selectCql({
	where: MfaBackupCodes.where.eq('user_id'),
});

export class MfaBackupCodeRepository {
	async listMfaBackupCodes(userId: UserID): Promise<Array<MfaBackupCode>> {
		const codes = await fetchMany<MfaBackupCodeRow>(FETCH_MFA_BACKUP_CODES_CQL, {user_id: userId});
		return codes.map((code) => new MfaBackupCode(code));
	}

	async createMfaBackupCodes(userId: UserID, codes: Array<string>): Promise<Array<MfaBackupCode>> {
		const batch = new BatchBuilder();
		const backupCodes: Array<MfaBackupCode> = [];
		for (const code of codes) {
			const codeRow: MfaBackupCodeRow = {user_id: userId, code: createMfaBackupCode(code), consumed: false};
			batch.addPrepared(MfaBackupCodes.insert(codeRow));
			backupCodes.push(new MfaBackupCode(codeRow));
		}
		await batch.execute();
		return backupCodes;
	}

	async clearMfaBackupCodes(userId: UserID): Promise<void> {
		const codes = await this.listMfaBackupCodes(userId);
		if (codes.length === 0) return;
		const batch = new BatchBuilder();
		for (const code of codes) {
			batch.addPrepared(MfaBackupCodes.deleteByPk({user_id: userId, code: createMfaBackupCode(code.code)}));
		}
		await batch.execute();
	}

	async consumeMfaBackupCode(userId: UserID, code: string): Promise<void> {
		await upsertOne(
			MfaBackupCodes.patchByPk(
				{user_id: userId, code: createMfaBackupCode(code)},
				{
					consumed: Db.set(true),
				},
			),
		);
	}

	async deleteAllMfaBackupCodes(userId: UserID): Promise<void> {
		await deleteOneOrMany(MfaBackupCodes.deleteCql({where: MfaBackupCodes.where.eq('user_id')}), {user_id: userId});
	}
}
