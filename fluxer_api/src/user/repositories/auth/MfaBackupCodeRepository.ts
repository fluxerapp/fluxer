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

import {createMfaBackupCode, type UserID} from '~/BrandedTypes';
import {BatchBuilder, Db, deleteOneOrMany, fetchMany, upsertOne} from '~/database/Cassandra';
import type {MfaBackupCodeRow} from '~/database/CassandraTypes';
import {MfaBackupCode} from '~/Models';
import {MfaBackupCodes} from '~/Tables';

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
