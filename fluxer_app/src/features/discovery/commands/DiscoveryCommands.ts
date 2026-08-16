// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('Discovery');

export interface DiscoveryGuild {
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

type DiscoveryGuildPayload = Omit<DiscoveryGuild, 'banner'> & {banner?: string | null};

interface DiscoveryCategoryCountPayload {
	category_type: number;
	count: number;
}

interface DiscoverySearchPayload {
	guilds: Array<DiscoveryGuildPayload>;
	total: number;
	category_counts?: Array<DiscoveryCategoryCountPayload>;
}

export interface DiscoverySearchResponse {
	guilds: Array<DiscoveryGuild>;
	total: number;
	categoryCounts: ReadonlyMap<number, number> | null;
}

interface DiscoveryCategory {
	id: number;
	name: string;
	listed_in_all?: boolean;
}

interface DiscoverySearchParams {
	query?: string;
	category?: number;
	language?: string;
	tag?: string;
	sort_by?: string;
	limit: number;
	offset: number;
}

function discoverySearchQuery(params: DiscoverySearchParams): Record<string, string> {
	const query: Record<string, string> = {
		limit: String(params.limit),
		offset: String(params.offset),
	};
	if (params.query) {
		query.query = params.query;
	}
	if (params.category !== undefined) {
		query.category = String(params.category);
	}
	if (params.language) {
		query.language = params.language;
	}
	if (params.tag) {
		query.tag = params.tag;
	}
	if (params.sort_by) {
		query.sort_by = params.sort_by;
	}
	return query;
}

function readGuild(payload: DiscoveryGuildPayload): DiscoveryGuild {
	return {...payload, banner: payload.banner ?? null};
}

function readCategoryCounts(payload: DiscoverySearchPayload): ReadonlyMap<number, number> | null {
	const counts = payload.category_counts;
	if (counts == null) {
		return null;
	}
	return new Map(counts.map((entry) => [entry.category_type, entry.count]));
}

async function requestDiscoverySearch(params: DiscoverySearchParams): Promise<DiscoverySearchResponse> {
	const response = await http.get<DiscoverySearchPayload>(Endpoints.DISCOVERY_GUILDS, {
		query: discoverySearchQuery(params),
	});
	return {
		guilds: response.body.guilds.map(readGuild),
		total: response.body.total,
		categoryCounts: readCategoryCounts(response.body),
	};
}

async function requestDiscoveryCategories(): Promise<Array<DiscoveryCategory>> {
	const response = await http.get<Array<DiscoveryCategory>>(Endpoints.DISCOVERY_CATEGORIES);
	return response.body;
}

export async function searchGuilds(params: DiscoverySearchParams): Promise<DiscoverySearchResponse> {
	return requestDiscoverySearch(params);
}

export async function getCategories(): Promise<Array<DiscoveryCategory>> {
	return requestDiscoveryCategories();
}

export async function joinGuild(guildId: string): Promise<void> {
	await http.post(Endpoints.DISCOVERY_JOIN(guildId));
	logger.info('Joined guild via discovery', {guildId});
}
