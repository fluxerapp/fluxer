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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import * as LocaleUtils from '~/utils/LocaleUtils';

const logger = new Logger('Tenor');

const getLocale = (): string => LocaleUtils.getCurrentLocale();

export interface TenorGif {
	id: string;
	title: string;
	url: string;
	src: string;
	proxy_src: string;
	width: number;
	height: number;
}

interface TenorCategory {
	name: string;
	src: string;
	proxy_src: string;
}

export interface TenorFeatured {
	categories: Array<TenorCategory>;
	gifs: Array<TenorGif>;
}

let tenorFeaturedCache: TenorFeatured | null = null;

export const search = async (q: string): Promise<Array<TenorGif>> => {
	try {
		logger.debug(`Searching for GIFs with query: "${q}"`);
		const response = await http.get<Array<TenorGif>>({
			url: Endpoints.TENOR_SEARCH,
			query: {q, locale: getLocale()},
		});
		const gifs = response.body;
		logger.debug(`Found ${gifs.length} GIFs for query "${q}"`);
		return gifs;
	} catch (error) {
		logger.error(`Failed to search for GIFs with query "${q}":`, error);
		throw error;
	}
};

export const getFeatured = async (): Promise<TenorFeatured> => {
	if (tenorFeaturedCache) {
		logger.debug('Returning cached featured Tenor content');
		return tenorFeaturedCache;
	}

	try {
		logger.debug('Fetching featured Tenor content');
		const response = await http.get<TenorFeatured>({
			url: Endpoints.TENOR_FEATURED,
			query: {locale: getLocale()},
		});
		const featured = response.body;
		tenorFeaturedCache = featured;
		logger.debug(
			`Fetched featured Tenor content: ${featured.categories.length} categories and ${featured.gifs.length} GIFs`,
		);
		return featured;
	} catch (error) {
		logger.error('Failed to fetch featured Tenor content:', error);
		throw error;
	}
};

export const getTrending = async (): Promise<Array<TenorGif>> => {
	try {
		logger.debug('Fetching trending Tenor GIFs');
		const response = await http.get<Array<TenorGif>>({
			url: Endpoints.TENOR_TRENDING_GIFS,
			query: {locale: getLocale()},
		});
		const gifs = response.body;
		logger.debug(`Fetched ${gifs.length} trending Tenor GIFs`);
		return gifs;
	} catch (error) {
		logger.error('Failed to fetch trending Tenor GIFs:', error);
		throw error;
	}
};

export const registerShare = async (id: string, q: string): Promise<void> => {
	try {
		logger.debug(`Registering GIF share: id=${id}, query="${q}"`);
		await http.post({url: Endpoints.TENOR_REGISTER_SHARE, body: {id, q, locale: getLocale()}});
		logger.debug(`Successfully registered GIF share for id=${id}`);
	} catch (error) {
		logger.error(`Failed to register GIF share for id=${id}:`, error);
	}
};

export const suggest = async (q: string): Promise<Array<string>> => {
	try {
		logger.debug(`Getting Tenor search suggestions for: "${q}"`);
		const response = await http.get<Array<string>>({
			url: Endpoints.TENOR_SUGGEST,
			query: {q, locale: getLocale()},
		});
		const suggestions = response.body;
		logger.debug(`Received ${suggestions.length} suggestions for query "${q}"`);
		return suggestions;
	} catch (error) {
		logger.error(`Failed to get suggestions for query "${q}":`, error);
		throw error;
	}
};
