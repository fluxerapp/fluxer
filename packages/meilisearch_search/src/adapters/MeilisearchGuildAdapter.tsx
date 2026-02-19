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

import {MeilisearchIndexAdapter} from '@fluxer/meilisearch_search/src/adapters/MeilisearchIndexAdapter';
import {
	compactFilters,
	type MeilisearchFilter,
	meiliAndEquals,
	meiliEquals,
} from '@fluxer/meilisearch_search/src/MeilisearchFilterUtils';
import {MEILISEARCH_INDEX_DEFINITIONS} from '@fluxer/meilisearch_search/src/MeilisearchIndexDefinitions';
import type {GuildSearchFilters, SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import type {MeiliSearch} from 'meilisearch';

function buildGuildFilters(filters: GuildSearchFilters): Array<MeilisearchFilter | undefined> {
	const clauses: Array<MeilisearchFilter | undefined> = [];

	if (filters.ownerId) clauses.push(meiliEquals('ownerId', filters.ownerId));
	if (filters.verificationLevel !== undefined)
		clauses.push(meiliEquals('verificationLevel', filters.verificationLevel));
	if (filters.mfaLevel !== undefined) clauses.push(meiliEquals('mfaLevel', filters.mfaLevel));
	if (filters.nsfwLevel !== undefined) clauses.push(meiliEquals('nsfwLevel', filters.nsfwLevel));

	if (filters.hasFeature && filters.hasFeature.length > 0) {
		clauses.push(...meiliAndEquals('features', filters.hasFeature));
	}

	if (filters.isDiscoverable !== undefined) clauses.push(meiliEquals('isDiscoverable', filters.isDiscoverable));
	if (filters.discoveryCategory !== undefined)
		clauses.push(meiliEquals('discoveryCategory', filters.discoveryCategory));

	return compactFilters(clauses);
}

function buildGuildSort(filters: GuildSearchFilters): Array<string> | undefined {
	const sortBy = filters.sortBy ?? 'createdAt';
	if (sortBy === 'relevance') return undefined;
	const sortOrder = filters.sortOrder ?? 'desc';
	return [`${sortBy}:${sortOrder}`];
}

export interface MeilisearchGuildAdapterOptions {
	client: MeiliSearch;
	waitForTasks: {
		enabled: boolean;
		timeoutMs: number;
		intervalMs: number;
	};
}

export class MeilisearchGuildAdapter extends MeilisearchIndexAdapter<GuildSearchFilters, SearchableGuild> {
	constructor(options: MeilisearchGuildAdapterOptions) {
		super({
			client: options.client,
			index: MEILISEARCH_INDEX_DEFINITIONS.guilds,
			buildFilters: buildGuildFilters,
			buildSort: buildGuildSort,
			waitForTasks: options.waitForTasks,
		});
	}
}
