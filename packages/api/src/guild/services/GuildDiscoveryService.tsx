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

import type {GuildID, UserID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {GuildDiscoveryRow} from '@fluxer/api/src/database/types/GuildDiscoveryTypes';
import {mapGuildToGuildResponse} from '@fluxer/api/src/guild/GuildModel';
import type {IGuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import {Logger} from '@fluxer/api/src/Logger';
import type {IGuildSearchService} from '@fluxer/api/src/search/IGuildSearchService';
import {
	DiscoveryApplicationStatus,
	DiscoveryCategories,
	type DiscoveryCategory,
} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {DiscoveryAlreadyAppliedError} from '@fluxer/errors/src/domains/discovery/DiscoveryAlreadyAppliedError';
import {DiscoveryApplicationAlreadyReviewedError} from '@fluxer/errors/src/domains/discovery/DiscoveryApplicationAlreadyReviewedError';
import {DiscoveryApplicationNotFoundError} from '@fluxer/errors/src/domains/discovery/DiscoveryApplicationNotFoundError';
import {DiscoveryInsufficientMembersError} from '@fluxer/errors/src/domains/discovery/DiscoveryInsufficientMembersError';
import {DiscoveryInvalidCategoryError} from '@fluxer/errors/src/domains/discovery/DiscoveryInvalidCategoryError';
import {DiscoveryNotDiscoverableError} from '@fluxer/errors/src/domains/discovery/DiscoveryNotDiscoverableError';
import type {GuildSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes.jsx';
import type {DiscoveryApplicationPatchRequest} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';

const VALID_CATEGORY_TYPES = new Set<number>(Object.values(DiscoveryCategories));

export abstract class IGuildDiscoveryService {
	abstract apply(params: {
		guildId: GuildID;
		userId: UserID;
		description: string;
		categoryId: number;
	}): Promise<GuildDiscoveryRow>;

	abstract editApplication(params: {
		guildId: GuildID;
		userId: UserID;
		data: DiscoveryApplicationPatchRequest;
	}): Promise<GuildDiscoveryRow>;

	abstract withdraw(params: {guildId: GuildID; userId: UserID}): Promise<void>;

	abstract getStatus(guildId: GuildID): Promise<GuildDiscoveryRow | null>;

	abstract approve(params: {guildId: GuildID; adminUserId: UserID; reason?: string}): Promise<GuildDiscoveryRow>;

	abstract reject(params: {guildId: GuildID; adminUserId: UserID; reason: string}): Promise<GuildDiscoveryRow>;

	abstract remove(params: {guildId: GuildID; adminUserId: UserID; reason: string}): Promise<GuildDiscoveryRow>;

	abstract getEligibility(guildId: GuildID): Promise<{eligible: boolean; min_member_count: number}>;

	abstract listByStatus(params: {status: string; limit: number}): Promise<Array<GuildDiscoveryRow>>;

	abstract searchDiscoverable(params: {
		query?: string;
		categoryId?: number;
		sortBy?: string;
		limit: number;
		offset: number;
	}): Promise<{guilds: Array<DiscoveryGuildResult>; total: number}>;
}

export interface DiscoveryGuildResult {
	id: string;
	name: string;
	icon: string | null;
	description: string | null;
	category_type: number;
	member_count: number;
	online_count: number;
	features: Array<string>;
	verification_level: number;
}

export class GuildDiscoveryService extends IGuildDiscoveryService {
	constructor(
		private readonly discoveryRepository: IGuildDiscoveryRepository,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly gatewayService: IGatewayService,
		private readonly guildSearchService: IGuildSearchService | null,
	) {
		super();
	}

	async apply(params: {
		guildId: GuildID;
		userId: UserID;
		description: string;
		categoryId: number;
	}): Promise<GuildDiscoveryRow> {
		const {guildId, description, categoryId} = params;

		if (!VALID_CATEGORY_TYPES.has(categoryId)) {
			throw new DiscoveryInvalidCategoryError();
		}

		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new DiscoveryApplicationNotFoundError();
		}

		const {eligible} = await this.getEligibility(guildId);
		if (!eligible) {
			throw new DiscoveryInsufficientMembersError();
		}

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (existing) {
			if (
				existing.status === DiscoveryApplicationStatus.PENDING ||
				existing.status === DiscoveryApplicationStatus.APPROVED
			) {
				throw new DiscoveryAlreadyAppliedError();
			}
		}

		const now = new Date();
		const row: GuildDiscoveryRow = {
			guild_id: guildId,
			status: DiscoveryApplicationStatus.PENDING,
			category_type: categoryId as DiscoveryCategory,
			description,
			applied_at: now,
			reviewed_at: null,
			reviewed_by: null,
			review_reason: null,
			removed_at: null,
			removed_by: null,
			removal_reason: null,
		};

		await this.discoveryRepository.upsert(row);

		return row;
	}

	async editApplication(params: {
		guildId: GuildID;
		userId: UserID;
		data: DiscoveryApplicationPatchRequest;
	}): Promise<GuildDiscoveryRow> {
		const {guildId, data} = params;

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (!existing) {
			throw new DiscoveryApplicationNotFoundError();
		}

		if (
			existing.status !== DiscoveryApplicationStatus.PENDING &&
			existing.status !== DiscoveryApplicationStatus.APPROVED
		) {
			throw new DiscoveryApplicationAlreadyReviewedError();
		}

		if (data.category_type !== undefined && !VALID_CATEGORY_TYPES.has(data.category_type)) {
			throw new DiscoveryInvalidCategoryError();
		}

		const updatedRow: GuildDiscoveryRow = {
			...existing,
			description: data.description ?? existing.description,
			category_type:
				data.category_type !== undefined ? (data.category_type as DiscoveryCategory) : existing.category_type,
		};

		await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, updatedRow);

		if (existing.status === DiscoveryApplicationStatus.APPROVED && this.guildSearchService) {
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				await this.guildSearchService.updateGuild(guild, {
					description: updatedRow.description,
					categoryId: updatedRow.category_type,
				});
			}
		}

		return updatedRow;
	}

	async withdraw(params: {guildId: GuildID; userId: UserID}): Promise<void> {
		const {guildId} = params;

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (!existing) {
			throw new DiscoveryApplicationNotFoundError();
		}

		await this.discoveryRepository.deleteByGuildId(guildId, existing.status, existing.applied_at);

		if (existing.status === DiscoveryApplicationStatus.APPROVED) {
			await this.removeDiscoverableFeature(guildId);
			if (this.guildSearchService) {
				await this.guildSearchService.deleteGuild(guildId);
			}
		}
	}

	async getStatus(guildId: GuildID): Promise<GuildDiscoveryRow | null> {
		return this.discoveryRepository.findByGuildId(guildId);
	}

	async getEligibility(guildId: GuildID): Promise<{eligible: boolean; min_member_count: number}> {
		const minMemberCount = Config.discovery.minMemberCount;
		const guild = await this.guildRepository.findUnique(guildId);
		const memberCount = guild?.memberCount ?? 0;
		return {eligible: memberCount >= minMemberCount, min_member_count: minMemberCount};
	}

	async approve(params: {guildId: GuildID; adminUserId: UserID; reason?: string}): Promise<GuildDiscoveryRow> {
		const {guildId, adminUserId, reason} = params;

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (!existing) {
			throw new DiscoveryApplicationNotFoundError();
		}

		if (existing.status !== DiscoveryApplicationStatus.PENDING) {
			throw new DiscoveryApplicationAlreadyReviewedError();
		}

		const now = new Date();
		const updatedRow: GuildDiscoveryRow = {
			...existing,
			status: DiscoveryApplicationStatus.APPROVED,
			reviewed_at: now,
			reviewed_by: adminUserId,
			review_reason: reason ?? null,
		};

		await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, updatedRow);
		await this.addDiscoverableFeature(guildId);

		if (this.guildSearchService) {
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				await this.guildSearchService.updateGuild(guild, {
					description: updatedRow.description,
					categoryId: updatedRow.category_type,
				});
			}
		}

		return updatedRow;
	}

	async reject(params: {guildId: GuildID; adminUserId: UserID; reason: string}): Promise<GuildDiscoveryRow> {
		const {guildId, adminUserId, reason} = params;

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (!existing) {
			throw new DiscoveryApplicationNotFoundError();
		}

		if (existing.status !== DiscoveryApplicationStatus.PENDING) {
			throw new DiscoveryApplicationAlreadyReviewedError();
		}

		const now = new Date();
		const updatedRow: GuildDiscoveryRow = {
			...existing,
			status: DiscoveryApplicationStatus.REJECTED,
			reviewed_at: now,
			reviewed_by: adminUserId,
			review_reason: reason,
		};

		await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, updatedRow);

		return updatedRow;
	}

	async remove(params: {guildId: GuildID; adminUserId: UserID; reason: string}): Promise<GuildDiscoveryRow> {
		const {guildId, adminUserId, reason} = params;

		const existing = await this.discoveryRepository.findByGuildId(guildId);
		if (!existing) {
			throw new DiscoveryApplicationNotFoundError();
		}

		if (existing.status !== DiscoveryApplicationStatus.APPROVED) {
			throw new DiscoveryNotDiscoverableError();
		}

		const now = new Date();
		const updatedRow: GuildDiscoveryRow = {
			...existing,
			status: DiscoveryApplicationStatus.REMOVED,
			removed_at: now,
			removed_by: adminUserId,
			removal_reason: reason,
		};

		await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, updatedRow);
		await this.removeDiscoverableFeature(guildId);

		if (this.guildSearchService) {
			await this.guildSearchService.deleteGuild(guildId);
		}

		return updatedRow;
	}

	async listByStatus(params: {status: string; limit: number}): Promise<Array<GuildDiscoveryRow>> {
		const statusRows = await this.discoveryRepository.listByStatus(params.status, params.limit);
		const fullRows = await Promise.all(statusRows.map((row) => this.discoveryRepository.findByGuildId(row.guild_id)));
		return fullRows.filter((row): row is GuildDiscoveryRow => row !== null);
	}

	async searchDiscoverable(params: {
		query?: string;
		categoryId?: number;
		sortBy?: string;
		limit: number;
		offset: number;
	}): Promise<{guilds: Array<DiscoveryGuildResult>; total: number}> {
		let guilds: Array<DiscoveryGuildResult>;
		let total: number;

		if (this.guildSearchService) {
			const filters: GuildSearchFilters = {
				isDiscoverable: true,
				discoveryCategory: params.categoryId,
				sortBy: 'relevance',
				sortOrder: 'desc',
			};

			const results = await this.guildSearchService.searchGuilds(params.query ?? '', filters, {
				limit: params.limit,
				offset: params.offset,
			});

			guilds = results.hits.map((hit) => ({
				id: hit.id,
				name: hit.name,
				icon: hit.iconHash,
				description: hit.discoveryDescription,
				category_type: hit.discoveryCategory ?? 0,
				member_count: 0,
				online_count: 0,
				features: hit.features,
				verification_level: hit.verificationLevel,
			}));

			total = results.total;
		} else {
			const statusRows = await this.discoveryRepository.listByStatus(
				DiscoveryApplicationStatus.APPROVED,
				params.limit + params.offset,
			);

			const paginatedRows = statusRows.slice(params.offset, params.offset + params.limit);
			guilds = [];

			for (const statusRow of paginatedRows) {
				const discoveryRow = await this.discoveryRepository.findByGuildId(statusRow.guild_id);
				if (!discoveryRow) continue;

				if (params.categoryId !== undefined && discoveryRow.category_type !== params.categoryId) {
					continue;
				}

				const guild = await this.guildRepository.findUnique(statusRow.guild_id);
				if (!guild) continue;

				guilds.push({
					id: statusRow.guild_id.toString(),
					name: guild.name,
					icon: guild.iconHash,
					description: discoveryRow.description,
					category_type: discoveryRow.category_type,
					member_count: guild.memberCount,
					online_count: 0,
					features: Array.from(guild.features),
					verification_level: guild.verificationLevel,
				});
			}

			total = statusRows.length;
		}

		if (guilds.length > 0) {
			try {
				const guildIds = guilds.map((g) => BigInt(g.id) as GuildID);
				const freshCounts = await this.gatewayService.getDiscoveryGuildCounts(guildIds);
				for (const guild of guilds) {
					const counts = freshCounts.get(BigInt(guild.id) as GuildID);
					if (counts) {
						guild.member_count = counts.memberCount;
						guild.online_count = counts.onlineCount;
					}
				}
			} catch (error) {
				Logger.warn(
					{error: error instanceof Error ? error.message : String(error)},
					'[discovery] Failed to fetch fresh guild counts from gateway, using stale values',
				);
			}
		}

		return {guilds, total};
	}

	private async addDiscoverableFeature(guildId: GuildID): Promise<void> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) return;

		const newFeatures = new Set(guild.features);
		newFeatures.add(GuildFeatures.DISCOVERABLE);

		const guildRow = guild.toRow();
		const updatedGuild = await this.guildRepository.upsert({
			...guildRow,
			features: newFeatures,
		});

		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(updatedGuild),
		});
	}

	private async removeDiscoverableFeature(guildId: GuildID): Promise<void> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) return;

		const newFeatures = new Set(guild.features);
		newFeatures.delete(GuildFeatures.DISCOVERABLE);

		const guildRow = guild.toRow();
		const updatedGuild = await this.guildRepository.upsert({
			...guildRow,
			features: newFeatures,
		});

		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(updatedGuild),
		});
	}
}
