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

import type {GuildID, UserID} from '~/BrandedTypes';
import {BatchBuilder, buildPatchFromData, executeVersionedUpdate, fetchMany, fetchOne} from '~/database/Cassandra';
import {EXPRESSION_PACK_COLUMNS, type ExpressionPackRow, type PackInstallationRow} from '~/database/CassandraTypes';
import {ExpressionPack} from '~/models/ExpressionPack';
import {ExpressionPacks, ExpressionPacksByCreator, PackInstallations} from '~/Tables';

export type PackType = ExpressionPack['type'];

const FETCH_EXPRESSION_PACK_BY_ID_QUERY = ExpressionPacks.selectCql({
	where: ExpressionPacks.where.eq('pack_id'),
	limit: 1,
});

const FETCH_EXPRESSION_PACKS_BY_CREATOR_QUERY = ExpressionPacksByCreator.selectCql({
	where: ExpressionPacksByCreator.where.eq('creator_id'),
});

const FETCH_PACK_INSTALLATIONS_BY_USER_QUERY = PackInstallations.selectCql({
	where: PackInstallations.where.eq('user_id'),
});

const FETCH_PACK_INSTALLATION_QUERY = PackInstallations.selectCql({
	where: [PackInstallations.where.eq('user_id'), PackInstallations.where.eq('pack_id')],
	limit: 1,
});

export class PackRepository {
	async getPack(packId: GuildID): Promise<ExpressionPack | null> {
		const row = await fetchOne<ExpressionPackRow>(FETCH_EXPRESSION_PACK_BY_ID_QUERY, {pack_id: packId});
		return row ? new ExpressionPack(row) : null;
	}

	async listPacksByCreator(creatorId: UserID, packType?: PackType): Promise<Array<ExpressionPack>> {
		const rows = await fetchMany<ExpressionPackRow>(FETCH_EXPRESSION_PACKS_BY_CREATOR_QUERY, {creator_id: creatorId});
		return rows.filter((row) => (packType ? row.pack_type === packType : true)).map((row) => new ExpressionPack(row));
	}

	async countPacksByCreator(creatorId: UserID, packType: PackType): Promise<number> {
		const rows = await fetchMany<ExpressionPackRow>(FETCH_EXPRESSION_PACKS_BY_CREATOR_QUERY, {creator_id: creatorId});
		return rows.filter((row) => row.pack_type === packType).length;
	}

	async upsertPack(data: ExpressionPackRow): Promise<ExpressionPack> {
		const packId = data.pack_id;
		const previousPack = await this.getPack(packId);
		const result = await executeVersionedUpdate<ExpressionPackRow, 'pack_id'>(
			async () => {
				const existing = await fetchOne<ExpressionPackRow>(FETCH_EXPRESSION_PACK_BY_ID_QUERY, {
					pack_id: packId,
				});
				return existing ?? null;
			},
			(current) => ({
				pk: {pack_id: packId},
				patch: buildPatchFromData(data, current, EXPRESSION_PACK_COLUMNS, ['pack_id']),
			}),
			ExpressionPacks,
			{onFailure: 'log'},
		);

		const batch = new BatchBuilder();
		if (previousPack && previousPack.creatorId !== data.creator_id) {
			batch.addPrepared(
				ExpressionPacksByCreator.deleteByPk({
					creator_id: previousPack.creatorId,
					pack_id: packId,
				}),
			);
		}

		const finalPack = await this.getPack(packId);
		if (finalPack) {
			const finalRow = finalPack.toRow();
			finalRow.version = result.finalVersion ?? finalRow.version;
			batch.addPrepared(ExpressionPacksByCreator.insert(finalRow));
		}

		await batch.execute();

		return finalPack ?? new ExpressionPack({...data, version: result.finalVersion ?? 1});
	}

	async deletePack(packId: GuildID): Promise<void> {
		const pack = await this.getPack(packId);
		const batch = new BatchBuilder().addPrepared(ExpressionPacks.deleteByPk({pack_id: packId}));
		if (pack) {
			batch.addPrepared(
				ExpressionPacksByCreator.deleteByPk({
					creator_id: pack.creatorId,
					pack_id: packId,
				}),
			);
		}
		await batch.execute();
	}

	async listInstallations(userId: UserID): Promise<Array<PackInstallationRow>> {
		return await fetchMany<PackInstallationRow>(FETCH_PACK_INSTALLATIONS_BY_USER_QUERY, {user_id: userId});
	}

	async addInstallation(data: PackInstallationRow): Promise<void> {
		await fetchOne(PackInstallations.insert(data));
	}

	async removeInstallation(userId: UserID, packId: GuildID): Promise<void> {
		await PackInstallations.deleteByPk({
			user_id: userId,
			pack_id: packId,
		});
	}

	async hasInstallation(userId: UserID, packId: GuildID): Promise<boolean> {
		const row = await fetchOne<PackInstallationRow>(FETCH_PACK_INSTALLATION_QUERY, {
			user_id: userId,
			pack_id: packId,
		});
		return row !== null;
	}
}
