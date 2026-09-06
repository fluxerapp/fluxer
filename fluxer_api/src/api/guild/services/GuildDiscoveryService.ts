// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DISCOVERY_DEFAULT_LANGUAGE,
	DISCOVERY_MAX_TAGS,
	DiscoveryApplicationStatus,
	type DiscoveryCategory,
	isValidDiscoveryLanguage,
	isValidDiscoveryTag,
	normalizeDiscoveryTag,
} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures, getEffectiveGuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {DiscoveryAlreadyAppliedError} from '@fluxer/errors/src/domains/discovery/DiscoveryAlreadyAppliedError';
import {DiscoveryApplicationAlreadyReviewedError} from '@fluxer/errors/src/domains/discovery/DiscoveryApplicationAlreadyReviewedError';
import {DiscoveryApplicationNotFoundError} from '@fluxer/errors/src/domains/discovery/DiscoveryApplicationNotFoundError';
import {DiscoveryInsufficientMembersError} from '@fluxer/errors/src/domains/discovery/DiscoveryInsufficientMembersError';
import {DiscoveryNotDiscoverableError} from '@fluxer/errors/src/domains/discovery/DiscoveryNotDiscoverableError';
import type {GuildSearchFilters} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes.jsx';
import type {DiscoveryApplicationPatchRequest} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import type {GuildID, UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {GuildDiscoveryRow} from '../../database/types/GuildDiscoveryTypes';
import {contentModerationService} from '../../infrastructure/ContentModerationService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Logger} from '../../Logger';
import type {IGuildSearchService} from '../../search/IGuildSearchService';
import {mapGuildToGuildResponse} from '../GuildModel';
import type {IGuildDiscoveryRepository} from '../repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '../repositories/IGuildRepositoryAggregate';

function sanitizeTags(tags: ReadonlyArray<string> | null | undefined): Array<string> {
	if (!tags || tags.length === 0) return [];
	const seen = new Set<string>();
	const result: Array<string> = [];
	for (const tag of tags) {
		if (!isValidDiscoveryTag(tag)) continue;
		const normalized = normalizeDiscoveryTag(tag);
		if (seen.has(normalized)) continue;
		seen.add(normalized);
		result.push(normalized);
		if (result.length >= DISCOVERY_MAX_TAGS) break;
	}
	return result;
}

function resolveLanguage(language: string | null | undefined, fallback: string | null = null): string {
	if (language && isValidDiscoveryLanguage(language)) return language;
	if (fallback && isValidDiscoveryLanguage(fallback)) return fallback;
	return DISCOVERY_DEFAULT_LANGUAGE;
}

export abstract class IGuildDiscoveryService {
	abstract apply(params: {
		guildId: GuildID;
		userId: UserID;
		description: string;
		categoryId: number;
		primaryLanguage?: string;
		customTags?: ReadonlyArray<string>;
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

	abstract getEligibility(guildId: GuildID): Promise<{
		eligible: boolean;
		min_member_count: number;
	}>;

	abstract listByStatus(params: {status: string; limit: number}): Promise<Array<GuildDiscoveryRow>>;

	abstract searchDiscoverable(params: {
		query?: string;
		categoryId?: number;
		primaryLanguage?: string;
		tag?: string;
		sortBy?: string;
		limit: number;
		offset: number;
	}): Promise<{
		guilds: Array<DiscoveryGuildResult>;
		total: number;
		category_counts: Array<DiscoveryCategoryCount>;
	}>;
}

interface DiscoveryCategoryCount {
	category_type: number;
	count: number;
}

interface DiscoveryGuildResult {
	id: string;
	name: string;
	icon: string | null;
	banner: string | null;
	description: string | null;
	category_type: number;
	primary_language: string | null;
	custom_tags: Array<string>;
	member_count: number;
	online_count: number;
	features: Array<string>;
	verification_level: number;
}

const DISCOVERY_CATEGORY_FACET = 'discoveryCategory';

function toDiscoveryCategoryCounts(
	counts: Readonly<Record<string, number>> | undefined,
): Array<DiscoveryCategoryCount> {
	if (!counts) {
		return [];
	}
	const entries: Array<DiscoveryCategoryCount> = [];
	for (const [key, count] of Object.entries(counts)) {
		const categoryType = Number.parseInt(key, 10);
		if (!Number.isInteger(categoryType) || count <= 0) {
			continue;
		}
		entries.push({category_type: categoryType, count});
	}
	return entries.sort((left, right) => left.category_type - right.category_type);
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
		primaryLanguage?: string;
		customTags?: ReadonlyArray<string>;
	}): Promise<GuildDiscoveryRow> {
		const {guildId, description, categoryId} = params;
		const primaryLanguage = resolveLanguage(params.primaryLanguage);
		const customTags = sanitizeTags(params.customTags);
		contentModerationService.scanText(description, {
			userId: params.userId,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
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
		const shouldAutoApprove = guild.features.has(GuildFeatures.VERIFIED) || guild.features.has(GuildFeatures.PARTNERED);
		const row: GuildDiscoveryRow = {
			guild_id: guildId,
			status: shouldAutoApprove ? DiscoveryApplicationStatus.APPROVED : DiscoveryApplicationStatus.PENDING,
			category_type: categoryId as DiscoveryCategory,
			description,
			primary_language: primaryLanguage,
			custom_tags: customTags,
			applied_at: now,
			reviewed_at: shouldAutoApprove ? now : null,
			reviewed_by: null,
			review_reason: null,
			removed_at: null,
			removed_by: null,
			removal_reason: null,
		};
		if (existing) {
			await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, row);
		} else {
			await this.discoveryRepository.upsert(row);
		}
		if (shouldAutoApprove) {
			await this.addDiscoverableFeature(guildId);
			if (this.guildSearchService) {
				const updatedGuild = await this.guildRepository.findUnique(guildId);
				if (updatedGuild) {
					await this.guildSearchService.updateGuild(updatedGuild, {
						description: row.description,
						categoryId: row.category_type,
						primaryLanguage: row.primary_language,
						tags: row.custom_tags,
					});
				}
			}
		}
		return row;
	}

	async editApplication(params: {
		guildId: GuildID;
		userId: UserID;
		data: DiscoveryApplicationPatchRequest;
	}): Promise<GuildDiscoveryRow> {
		const {guildId, data} = params;
		contentModerationService.scanText(data.description ?? null, {
			userId: params.userId,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
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
		const updatedRow: GuildDiscoveryRow = {
			...existing,
			description: data.description ?? existing.description,
			category_type:
				data.category_type !== undefined ? (data.category_type as DiscoveryCategory) : existing.category_type,
			primary_language:
				data.primary_language !== undefined
					? resolveLanguage(data.primary_language, existing.primary_language ?? null)
					: existing.primary_language,
			custom_tags: data.custom_tags !== undefined ? sanitizeTags(data.custom_tags) : existing.custom_tags,
		};
		await this.discoveryRepository.updateStatus(guildId, existing.status, existing.applied_at, updatedRow);
		if (existing.status === DiscoveryApplicationStatus.APPROVED && this.guildSearchService) {
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				await this.guildSearchService.updateGuild(guild, {
					description: updatedRow.description,
					categoryId: updatedRow.category_type,
					primaryLanguage: updatedRow.primary_language,
					tags: updatedRow.custom_tags,
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

	async getEligibility(guildId: GuildID): Promise<{
		eligible: boolean;
		min_member_count: number;
	}> {
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
					primaryLanguage: updatedRow.primary_language,
					tags: updatedRow.custom_tags,
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
		return this.discoveryRepository.listFullByStatus(params.status, params.limit);
	}

	async searchDiscoverable(params: {
		query?: string;
		categoryId?: number;
		primaryLanguage?: string;
		tag?: string;
		sortBy?: string;
		limit: number;
		offset: number;
	}): Promise<{
		guilds: Array<DiscoveryGuildResult>;
		total: number;
		category_counts: Array<DiscoveryCategoryCount>;
	}> {
		if (!this.guildSearchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const language =
			params.primaryLanguage && isValidDiscoveryLanguage(params.primaryLanguage) ? params.primaryLanguage : undefined;
		const tag = params.tag && params.tag.trim().length > 0 ? normalizeDiscoveryTag(params.tag) : undefined;
		const sortBy = params.sortBy === 'member_count' ? 'memberCount' : 'relevance';
		const filters: GuildSearchFilters = {
			isDiscoverable: true,
			discoveryCategory: params.categoryId,
			discoveryPrimaryLanguage: language,
			discoveryTag: tag,
			sortBy,
			sortOrder: 'desc',
		};
		const categoryFacetFilters: GuildSearchFilters = {...filters, discoveryCategory: undefined};
		const [results, categoryFacets] = await Promise.all([
			this.guildSearchService.searchGuilds(params.query ?? '', filters, {
				limit: params.limit,
				offset: params.offset,
			}),
			this.guildSearchService.searchGuilds(params.query ?? '', categoryFacetFilters, {
				limit: 0,
				facets: [DISCOVERY_CATEGORY_FACET],
			}),
		]);
		const categoryCounts = toDiscoveryCategoryCounts(categoryFacets.facetCounts?.[DISCOVERY_CATEGORY_FACET]);
		const guilds: Array<DiscoveryGuildResult> = results.hits.map((hit) => ({
			id: hit.id,
			name: hit.name,
			icon: hit.iconHash,
			banner: hit.bannerHash,
			description: hit.discoveryDescription,
			category_type: hit.discoveryCategory ?? 0,
			primary_language: hit.discoveryPrimaryLanguage ?? null,
			custom_tags: hit.discoveryTags ?? [],
			member_count: hit.memberCount,
			online_count: 0,
			features: hit.features,
			verification_level: getEffectiveGuildVerificationLevel(hit.verificationLevel, hit.isDiscoverable),
		}));
		const total = results.total;
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
		return {guilds, total, category_counts: categoryCounts};
	}

	private async addDiscoverableFeature(guildId: GuildID): Promise<void> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) return;
		const newFeatures = new Set(guild.features);
		newFeatures.add(GuildFeatures.DISCOVERABLE);
		const updatedGuild = await this.guildRepository.upsertPartial(guildId, {features: newFeatures}, guild.toRow());
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
		const updatedGuild = await this.guildRepository.upsertPartial(guildId, {features: newFeatures}, guild.toRow());
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_UPDATE',
			data: mapGuildToGuildResponse(updatedGuild),
		});
	}
}
