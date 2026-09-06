// SPDX-License-Identifier: AGPL-3.0-or-later

import {DISCOVERY_DEFAULT_LANGUAGE, DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import type {GuildID} from '../../BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne} from '../../database/CassandraQueryExecution';
import type {GuildDiscoveryByStatusRow, GuildDiscoveryRow} from '../../database/types/GuildDiscoveryTypes';
import {GuildDiscovery, GuildDiscoveryByStatus} from '../../Tables';

const FETCH_DISCOVERY_BY_GUILD_ID = GuildDiscovery.selectCql({
	where: GuildDiscovery.where.eq('guild_id'),
	limit: 1,
});
const FETCH_DISCOVERY_BY_STATUS = GuildDiscoveryByStatus.selectCql({
	where: GuildDiscoveryByStatus.where.eq('status'),
});
const FETCH_ALL_DISCOVERY_FIRST_PAGE = (limit: number) => GuildDiscovery.select({limit});
const FETCH_ALL_DISCOVERY_PAGINATED = (limit: number) =>
	GuildDiscovery.select({
		where: GuildDiscovery.where.tokenGt('guild_id', 'last_guild_id'),
		limit,
	});

export abstract class IGuildDiscoveryRepository {
	abstract findByGuildId(guildId: GuildID): Promise<GuildDiscoveryRow | null>;

	abstract listByStatus(status: string): Promise<Array<GuildDiscoveryByStatusRow>>;

	abstract listFullByStatus(status: string): Promise<Array<GuildDiscoveryRow>>;

	abstract listAllPaginated(limit: number, lastGuildId?: GuildID): Promise<Array<GuildDiscoveryRow>>;

	abstract upsert(row: GuildDiscoveryRow): Promise<void>;

	abstract deleteByGuildId(guildId: GuildID, status: string, appliedAt: Date): Promise<void>;

	abstract updateStatus(
		guildId: GuildID,
		oldStatus: string,
		oldAppliedAt: Date,
		updatedRow: GuildDiscoveryRow,
	): Promise<void>;
}

export class GuildDiscoveryRepository extends IGuildDiscoveryRepository {
	async findByGuildId(guildId: GuildID): Promise<GuildDiscoveryRow | null> {
		const row = await fetchOne<GuildDiscoveryRow>(FETCH_DISCOVERY_BY_GUILD_ID, {
			guild_id: guildId,
		});
		if (row) {
			row.category_type ??= DiscoveryCategories.GAMING;
			row.primary_language ??= DISCOVERY_DEFAULT_LANGUAGE;
			row.custom_tags ??= [];
		}
		return row;
	}

	async listByStatus(status: string): Promise<Array<GuildDiscoveryByStatusRow>> {
		return fetchMany<GuildDiscoveryByStatusRow>(FETCH_DISCOVERY_BY_STATUS, {
			status,
		});
	}

	async listFullByStatus(status: string): Promise<Array<GuildDiscoveryRow>> {
		const indexRows = await this.listByStatus(status);
		const fullRows = await Promise.all(indexRows.map((indexRow) => this.findByGuildId(indexRow.guild_id)));
		return fullRows.filter((row): row is GuildDiscoveryRow => row !== null);
	}

	async listAllPaginated(limit: number, lastGuildId?: GuildID): Promise<Array<GuildDiscoveryRow>> {
		if (lastGuildId) {
			return fetchMany<GuildDiscoveryRow>(
				FETCH_ALL_DISCOVERY_PAGINATED(limit).bind({
					last_guild_id: lastGuildId,
				}),
			);
		}
		return fetchMany<GuildDiscoveryRow>(FETCH_ALL_DISCOVERY_FIRST_PAGE(limit).bind({}));
	}

	async upsert(row: GuildDiscoveryRow): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildDiscovery.insert({
				guild_id: row.guild_id,
				status: row.status,
				category_type: row.category_type,
				description: row.description,
				primary_language: row.primary_language,
				custom_tags: row.custom_tags,
				applied_at: row.applied_at,
				reviewed_at: row.reviewed_at,
				reviewed_by: row.reviewed_by,
				review_reason: row.review_reason,
				removed_at: row.removed_at,
				removed_by: row.removed_by,
				removal_reason: row.removal_reason,
			}),
		);
		batch.addPrepared(
			GuildDiscoveryByStatus.insert({
				status: row.status,
				applied_at: row.applied_at,
				guild_id: row.guild_id,
			}),
		);
		await batch.execute();
	}

	async deleteByGuildId(guildId: GuildID, status: string, appliedAt: Date): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(GuildDiscovery.deleteByPk({guild_id: guildId}));
		batch.addPrepared(
			GuildDiscoveryByStatus.deleteByPk({
				status,
				applied_at: appliedAt,
				guild_id: guildId,
			}),
		);
		await batch.execute();
	}

	async updateStatus(
		guildId: GuildID,
		oldStatus: string,
		oldAppliedAt: Date,
		updatedRow: GuildDiscoveryRow,
	): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(
			GuildDiscoveryByStatus.deleteByPk({
				status: oldStatus,
				applied_at: oldAppliedAt,
				guild_id: guildId,
			}),
		);
		batch.addPrepared(
			GuildDiscovery.insert({
				guild_id: updatedRow.guild_id,
				status: updatedRow.status,
				category_type: updatedRow.category_type,
				description: updatedRow.description,
				primary_language: updatedRow.primary_language,
				custom_tags: updatedRow.custom_tags,
				applied_at: updatedRow.applied_at,
				reviewed_at: updatedRow.reviewed_at,
				reviewed_by: updatedRow.reviewed_by,
				review_reason: updatedRow.review_reason,
				removed_at: updatedRow.removed_at,
				removed_by: updatedRow.removed_by,
				removal_reason: updatedRow.removal_reason,
			}),
		);
		batch.addPrepared(
			GuildDiscoveryByStatus.insert({
				status: updatedRow.status,
				applied_at: updatedRow.applied_at,
				guild_id: updatedRow.guild_id,
			}),
		);
		await batch.execute();
	}
}
