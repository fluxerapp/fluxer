// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {http} from '@app/features/platform/transport/RestTransport';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {GIF_PROVIDER_ATTRIBUTION_HEADER, GIF_PROVIDER_HEADER} from '@fluxer/schema/src/domains/gif/GifSchemas';

const logger = new Logger('GIF');
const getLocale = (): string => LocaleUtils.getCurrentLocale();

interface GifMediaFormat {
	src: string;
	proxy_src: string;
	width: number;
	height: number;
}

export interface Gif {
	id: string;
	slug: string;
	provider: string;
	title: string;
	url: string;
	src: string;
	proxy_src: string;
	width: number;
	height: number;
	media: Record<string, GifMediaFormat>;
	placeholder?: string | null;
}

interface GifCategory {
	name: string;
	src: string;
	proxy_src: string;
	gif: Gif | null;
}

export interface GifFeatured {
	categories: Array<GifCategory>;
	gifs: Array<Gif>;
}

const featuredCache: Record<string, GifFeatured> = {};

function localizedQuery(extra: Record<string, string> = {}): Record<string, string> {
	return {...extra, locale: getLocale()};
}

function gifShareBody(id: string, q: string): {id: string; q: string; locale: string} {
	return {id, q, locale: getLocale()};
}

function readHeader(headers: Record<string, string>, key: string): string | undefined {
	return headers[key] ?? headers[key.toLowerCase()];
}

function applyProviderHeaders(headers: Record<string, string>): void {
	const name = readHeader(headers, GIF_PROVIDER_HEADER);
	if (!name) return;
	const attributionRaw = readHeader(headers, GIF_PROVIDER_ATTRIBUTION_HEADER);
	const attributionRequired = attributionRaw === undefined ? undefined : attributionRaw === 'true';
	RuntimeConfig.applyGifProviderHeaders({name, attributionRequired});
}

function withProviderHeaderSync<T>(response: RestResponse<T>): T {
	applyProviderHeaders(response.headers);
	return response.body;
}

export async function search(q: string): Promise<Array<Gif>> {
	try {
		logger.debug({q}, 'Searching for GIFs');
		const response = await http.get<Array<Gif>>(Endpoints.GIFS_SEARCH, {
			query: localizedQuery({q}),
		});
		return withProviderHeaderSync(response);
	} catch (error) {
		logger.error({q, error}, 'Failed to search for GIFs');
		throw error;
	}
}

export async function getFeatured(): Promise<GifFeatured> {
	const provider = RuntimeConfig.gifProvider;
	const cached = featuredCache[provider];
	if (cached) {
		logger.debug({provider}, 'Returning cached featured GIF content');
		return cached;
	}
	try {
		logger.debug('Fetching featured GIF content');
		const response = await http.get<GifFeatured>(Endpoints.GIFS_FEATURED, {
			query: localizedQuery(),
		});
		const featured = withProviderHeaderSync(response);
		featuredCache[RuntimeConfig.gifProvider] = featured;
		return featured;
	} catch (error) {
		logger.error({error}, 'Failed to fetch featured GIF content');
		throw error;
	}
}

export async function getTrending(): Promise<Array<Gif>> {
	try {
		logger.debug('Fetching trending GIFs');
		const response = await http.get<Array<Gif>>(Endpoints.GIFS_TRENDING, {
			query: localizedQuery(),
		});
		return withProviderHeaderSync(response);
	} catch (error) {
		logger.error({error}, 'Failed to fetch trending GIFs');
		throw error;
	}
}

export async function registerShare(id: string, q: string): Promise<void> {
	try {
		logger.debug({id, q}, 'Registering GIF share');
		const response = await http.post(Endpoints.GIFS_REGISTER_SHARE, {body: gifShareBody(id, q)});
		applyProviderHeaders(response.headers);
	} catch (error) {
		logger.error({id, error}, 'Failed to register GIF share');
	}
}

export async function suggest(q: string): Promise<Array<string>> {
	try {
		logger.debug({q}, 'Getting GIF search suggestions');
		const response = await http.get<Array<string>>(Endpoints.GIFS_SUGGEST, {
			query: localizedQuery({q}),
		});
		return withProviderHeaderSync(response);
	} catch (error) {
		logger.error({q, error}, 'Failed to get GIF search suggestions');
		throw error;
	}
}
