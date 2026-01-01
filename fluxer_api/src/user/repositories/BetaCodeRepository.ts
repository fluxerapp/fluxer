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

import {createBetaCode, type UserID} from '~/BrandedTypes';
import {BatchBuilder, Db, fetchMany, fetchOne} from '~/database/Cassandra';
import type {BetaCodeByCodeRow, BetaCodeRow} from '~/database/CassandraTypes';
import {BetaCode} from '~/Models';
import {BetaCodes, BetaCodesByCode} from '~/Tables';

const FETCH_BETA_CODES_BY_CREATOR_QUERY = BetaCodes.selectCql({
	where: BetaCodes.where.eq('creator_id'),
});

const FETCH_BETA_CODE_BY_CREATOR_AND_CODE_QUERY = BetaCodes.selectCql({
	where: [BetaCodes.where.eq('creator_id'), BetaCodes.where.eq('code')],
	limit: 1,
});

const FETCH_BETA_CODE_BY_CODE_QUERY = BetaCodesByCode.selectCql({
	where: BetaCodesByCode.where.eq('code'),
});

const FETCH_BETA_CODES_BY_CREATOR_FOR_DELETE_QUERY = BetaCodes.selectCql({
	columns: ['code'],
	where: BetaCodes.where.eq('creator_id', 'user_id'),
});

export class BetaCodeRepository {
	async listBetaCodes(creatorId: UserID): Promise<Array<BetaCode>> {
		const betaCodes = await fetchMany<BetaCodeRow>(FETCH_BETA_CODES_BY_CREATOR_QUERY, {
			creator_id: creatorId,
		});
		return betaCodes.map((betaCode) => new BetaCode(betaCode));
	}

	async getBetaCode(code: string): Promise<BetaCode | null> {
		const betaCodeByCode = await fetchOne<BetaCodeByCodeRow>(FETCH_BETA_CODE_BY_CODE_QUERY, {code});
		if (!betaCodeByCode) {
			return null;
		}

		const betaCode = await fetchOne<BetaCodeRow>(FETCH_BETA_CODE_BY_CREATOR_AND_CODE_QUERY, {
			creator_id: betaCodeByCode.creator_id,
			code: betaCodeByCode.code,
		});

		return betaCode ? new BetaCode(betaCode) : null;
	}

	async upsertBetaCode(betaCode: BetaCodeRow): Promise<BetaCode> {
		const batch = new BatchBuilder();
		batch.addPrepared(BetaCodes.upsertAll(betaCode));
		batch.addPrepared(
			BetaCodesByCode.upsertAll({
				code: betaCode.code,
				creator_id: betaCode.creator_id,
			}),
		);
		await batch.execute();
		return new BetaCode(betaCode);
	}

	async updateBetaCodeRedeemed(code: string, redeemerId: UserID, redeemedAt: Date): Promise<void> {
		const betaCodeByCode = await fetchOne<BetaCodeByCodeRow>(FETCH_BETA_CODE_BY_CODE_QUERY, {code});
		if (!betaCodeByCode) {
			return;
		}

		const batch = new BatchBuilder();
		batch.addPrepared(
			BetaCodes.patchByPk(
				{
					creator_id: betaCodeByCode.creator_id,
					code: betaCodeByCode.code,
				},
				{
					redeemer_id: Db.set(redeemerId),
					redeemed_at: Db.set(redeemedAt),
				},
			),
		);
		await batch.execute();
	}

	async deleteBetaCode(code: string, creatorId: UserID): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(BetaCodes.deleteByPk({creator_id: creatorId, code: createBetaCode(code)}));
		batch.addPrepared(BetaCodesByCode.deleteByPk({code: createBetaCode(code), creator_id: creatorId}));
		await batch.execute();
	}

	async deleteAllBetaCodes(userId: UserID): Promise<void> {
		const codes = await fetchMany<{code: string}>(FETCH_BETA_CODES_BY_CREATOR_FOR_DELETE_QUERY, {
			user_id: userId,
		});

		const batch = new BatchBuilder();
		for (const betaCode of codes) {
			batch.addPrepared(BetaCodes.deleteByPk({creator_id: userId, code: createBetaCode(betaCode.code)}));
			batch.addPrepared(BetaCodesByCode.deleteByPk({code: createBetaCode(betaCode.code), creator_id: userId}));
		}

		await batch.execute();
	}
}
