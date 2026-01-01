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

import type {Index, MeiliSearch} from 'meilisearch';
import type {GuildID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import type {Guild} from '~/Models';
import {SEARCH_MAX_TOTAL_HITS} from '~/search/constants';
import {extractTimestamp} from '~/utils/SnowflakeUtils';

const GUILD_INDEX_NAME = 'guilds';

interface SearchableGuild {
	id: string;
	ownerId: string;
	name: string;
	vanityUrlCode: string | null;
	iconHash: string | null;
	bannerHash: string | null;
	splashHash: string | null;
	features: Array<string>;
	verificationLevel: number;
	mfaLevel: number;
	nsfwLevel: number;
	memberCount: number;
	createdAt: number;
}

interface GuildSearchFilters {
	ownerId?: string;
	minMembers?: number;
	maxMembers?: number;
	verificationLevel?: number;
	mfaLevel?: number;
	nsfwLevel?: number;
	hasFeature?: Array<string>;
	sortBy?: 'createdAt' | 'memberCount' | 'relevance';
	sortOrder?: 'asc' | 'desc';
}

export class GuildSearchService {
	private meilisearch: MeiliSearch;
	private index: Index<SearchableGuild> | null = null;

	constructor(meilisearch: MeiliSearch) {
		this.meilisearch = meilisearch;
	}

	async initialize(): Promise<void> {
		try {
			this.index = this.meilisearch.index<SearchableGuild>(GUILD_INDEX_NAME);

			await this.index.updateSettings({
				searchableAttributes: ['name', 'id', 'vanityUrlCode', 'ownerId'],
				filterableAttributes: [
					'ownerId',
					'verificationLevel',
					'mfaLevel',
					'nsfwLevel',
					'memberCount',
					'features',
					'createdAt',
				],
				sortableAttributes: ['createdAt', 'memberCount'],
				rankingRules: ['words', 'typo', 'proximity', 'attribute', 'sort', 'exactness'],
				pagination: {
					maxTotalHits: SEARCH_MAX_TOTAL_HITS,
				},
			});

			Logger.debug('Guild search index initialized successfully');
		} catch (error) {
			Logger.error({error}, 'Failed to initialize guild search index');
			throw error;
		}
	}

	async indexGuild(guild: Guild): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		const searchableGuild = this.convertToSearchableGuild(guild);

		try {
			await this.index.addDocuments([searchableGuild], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({guildId: guild.id, error}, 'Failed to index guild');
			throw error;
		}
	}

	async indexGuilds(guilds: Array<Guild>): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		if (guilds.length === 0) return;

		const searchableGuilds = guilds.map((guild) => this.convertToSearchableGuild(guild));

		try {
			await this.index.addDocuments(searchableGuilds, {primaryKey: 'id'});
		} catch (error) {
			Logger.error({count: guilds.length, error}, 'Failed to index guilds');
			throw error;
		}
	}

	async updateGuild(guild: Guild): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		const searchableGuild = this.convertToSearchableGuild(guild);

		try {
			await this.index.updateDocuments([searchableGuild], {primaryKey: 'id'});
		} catch (error) {
			Logger.error({guildId: guild.id, error}, 'Failed to update guild in search index');
			throw error;
		}
	}

	async deleteGuild(guildId: GuildID): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		try {
			await this.index.deleteDocument(guildId.toString());
		} catch (error) {
			Logger.error({guildId, error}, 'Failed to delete guild from search index');
			throw error;
		}
	}

	async deleteGuilds(guildIds: Array<GuildID>): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		if (guildIds.length === 0) return;

		try {
			await this.index.deleteDocuments(guildIds.map((id) => id.toString()));
		} catch (error) {
			Logger.error({count: guildIds.length, error}, 'Failed to delete guilds from search index');
			throw error;
		}
	}

	async searchGuilds(
		query: string,
		filters: GuildSearchFilters,
		options?: {
			limit?: number;
			offset?: number;
		},
	): Promise<{hits: Array<SearchableGuild>; total: number}> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		const filterStrings = this.buildFilterStrings(filters);
		const sortField = this.buildSortField(filters);

		try {
			const result = await this.index.search(query, {
				filter: filterStrings.length > 0 ? filterStrings : undefined,
				limit: options?.limit ?? 50,
				offset: options?.offset ?? 0,
				sort: sortField,
			});

			return {
				hits: result.hits,
				total: result.estimatedTotalHits ?? 0,
			};
		} catch (error) {
			Logger.error({query, filters, error}, 'Failed to search guilds');
			throw error;
		}
	}

	async deleteAllDocuments(): Promise<void> {
		if (!this.index) {
			throw new Error('Guild search index not initialized');
		}

		try {
			await this.index.deleteAllDocuments();
			Logger.debug('All guild documents deleted from search index');
		} catch (error) {
			Logger.error({error}, 'Failed to delete all guild documents');
			throw error;
		}
	}

	private buildFilterStrings(filters: GuildSearchFilters): Array<string> {
		const filterStrings: Array<string> = [];

		if (filters.ownerId) {
			filterStrings.push(`ownerId = "${filters.ownerId}"`);
		}

		if (filters.minMembers !== undefined) {
			filterStrings.push(`memberCount >= ${filters.minMembers}`);
		}

		if (filters.maxMembers !== undefined) {
			filterStrings.push(`memberCount <= ${filters.maxMembers}`);
		}

		if (filters.verificationLevel !== undefined) {
			filterStrings.push(`verificationLevel = ${filters.verificationLevel}`);
		}

		if (filters.mfaLevel !== undefined) {
			filterStrings.push(`mfaLevel = ${filters.mfaLevel}`);
		}

		if (filters.nsfwLevel !== undefined) {
			filterStrings.push(`nsfwLevel = ${filters.nsfwLevel}`);
		}

		if (filters.hasFeature && filters.hasFeature.length > 0) {
			const featureFilters = filters.hasFeature.map((feature) => `features = "${feature}"`).join(' OR ');
			filterStrings.push(`(${featureFilters})`);
		}

		return filterStrings;
	}

	private buildSortField(filters: GuildSearchFilters): Array<string> {
		const sortBy = filters.sortBy ?? 'createdAt';
		const sortOrder = filters.sortOrder ?? 'desc';

		if (sortBy === 'relevance') {
			return [];
		}

		return [`${sortBy}:${sortOrder}`];
	}

	private convertToSearchableGuild(guild: Guild): SearchableGuild {
		const createdAt = Math.floor(extractTimestamp(BigInt(guild.id)) / 1000);

		return {
			id: guild.id.toString(),
			ownerId: guild.ownerId.toString(),
			name: guild.name,
			vanityUrlCode: guild.vanityUrlCode,
			iconHash: guild.iconHash,
			bannerHash: guild.bannerHash,
			splashHash: guild.splashHash,
			features: Array.from(guild.features),
			verificationLevel: guild.verificationLevel,
			mfaLevel: guild.mfaLevel,
			nsfwLevel: guild.nsfwLevel,
			memberCount: guild.memberCount,
			createdAt,
		};
	}
}
