// SPDX-License-Identifier: AGPL-3.0-or-later

import {FLUXER_USER_AGENT} from '@fluxer/constants/src/Core';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {GifCategoryTagResponse, GifMediaFormat, GifResponse} from '@fluxer/schema/src/domains/gif/GifSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms} from 'itty-time';
import {Config} from '../Config';
import type {IMediaService} from '../infrastructure/IMediaService';
import {Logger} from '../Logger';
import {getWorkerService} from '../middleware/ServiceRegistry';
import {EXTERNAL_RESPONSE_LIMITS} from '../utils/ExternalResponseLimits';
import * as FetchUtils from '../utils/FetchUtils';
import {isJsonRecord, parseJsonUnknown} from '../utils/JsonBoundaryUtils';
import {
	REFRESH_LOCK_TTL_SECONDS,
	readEnrichedCategoriesCache,
	refreshLockKey,
	trackSeenLocale,
	writeEnrichedCategoriesCache,
} from './GifFeaturedCategoriesCache';
import type {GifProviderMeta, IGifProvider} from './IGifProvider';

const TENOR_BASE_URL = 'https://tenor.googleapis.com/v2';
const DEFAULT_MEDIA_FILTER = 'webm,mp4,webp,gif,tinywebm,tinymp4,tinygif,nanogif';
const PUBLIC_MEDIA_FORMATS = ['webm', 'mp4', 'webp', 'gif', 'tinywebm', 'tinymp4', 'tinygif', 'nanogif'] as const;
const DEFAULT_CONTENT_FILTER = 'low';
const CLIENT_KEY = 'fluxer';
const MAX_RETRIES = 3;
const BACKOFF_BASE_DELAY = ms('1 second');
const CACHE_EXPIRATION_TIME = ms('5 minutes');
const TENOR_PROVIDER_META: GifProviderMeta = {
	name: 'tenor',
	displayName: 'Tenor',
	attributionRequired: false,
};
type GifApiKeyResolver = () => Promise<string | null>;

interface TenorMediaFormat {
	url: string;
	dims: [number, number];
}

interface TenorGif {
	id: string;
	title?: string;
	content_description?: string;
	media_formats?: Record<string, TenorMediaFormat>;
	itemurl?: string;
	url?: string;
}

interface TenorCategoryTag {
	searchterm: string;
	image: string;
}

function isTenorMediaFormat(value: unknown): value is TenorMediaFormat {
	if (!isJsonRecord(value) || typeof value.url !== 'string' || !Array.isArray(value.dims)) return false;
	return value.dims.length === 2 && value.dims.every((dimension) => typeof dimension === 'number');
}

function isTenorGif(value: unknown): value is TenorGif {
	if (!isJsonRecord(value) || typeof value.id !== 'string') return false;
	const mediaFormats = value.media_formats;
	return (
		(value.title === undefined || typeof value.title === 'string') &&
		(value.content_description === undefined || typeof value.content_description === 'string') &&
		(mediaFormats === undefined ||
			(isJsonRecord(mediaFormats) && Object.values(mediaFormats).every(isTenorMediaFormat))) &&
		(value.itemurl === undefined || typeof value.itemurl === 'string') &&
		(value.url === undefined || typeof value.url === 'string')
	);
}

function isTenorCategoryTag(value: unknown): value is TenorCategoryTag {
	return isJsonRecord(value) && typeof value.searchterm === 'string' && typeof value.image === 'string';
}

function readResultsArray(value: unknown): Array<unknown> {
	if (!isJsonRecord(value) || !Array.isArray(value.results)) {
		throw new Error('Tenor API response did not include a results array');
	}
	return value.results;
}

function readTagsArray(value: unknown): Array<unknown> {
	if (!isJsonRecord(value) || !Array.isArray(value.tags)) {
		throw new Error('Tenor API response did not include a tags array');
	}
	return value.tags;
}

type CacheEntry<T> = {
	data: T;
	timestamp: number;
};

export class TenorGifProvider implements IGifProvider {
	readonly meta = TENOR_PROVIDER_META;
	private refreshingKeys: Map<string, boolean> = new Map();

	private featuredCacheKey(locale: string, country: string): string {
		return `tenor:featured:${locale}:${country}`;
	}

	private trendingCacheKey(locale: string, country: string): string {
		return `tenor:trending:${locale}:${country}`;
	}

	constructor(
		private cacheService: ICacheService,
		private mediaService: IMediaService,
		private apiKeyResolver: GifApiKeyResolver = async () => Config.tenor.apiKey || null,
	) {}

	async isAvailable(): Promise<boolean> {
		return Boolean(await this.apiKeyResolver());
	}

	private async getApiKey(): Promise<string> {
		const apiKey = await this.apiKeyResolver();
		if (!apiKey) {
			throw new Error('Tenor API key is not configured');
		}
		return apiKey;
	}

	private createURL({endpoint, params}: {endpoint: string; params: Record<string, string | number | undefined>}): URL {
		const url = new URL(`${TENOR_BASE_URL}/${endpoint}`);
		for (const [key, value] of Object.entries(params)) {
			if (value !== undefined && value !== '') {
				url.searchParams.append(key, value.toString());
			}
		}
		return url;
	}

	private async fetchTenorData(url: URL): Promise<unknown> {
		let lastError: unknown = new Error('Tenor API request failed');
		for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
			try {
				const response = await fetch(url.toString(), {
					headers: {'User-Agent': FLUXER_USER_AGENT},
					signal: AbortSignal.timeout(ms('30 seconds')),
				});
				if (response.ok) {
					const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
						maxBytes: EXTERNAL_RESPONSE_LIMITS.tenorApiBytes,
						headers: response.headers,
						url: response.url,
						description: 'Tenor API response',
					});
					return parseJsonUnknown(responseText);
				}
				if (response.status === 429 || (response.status >= 400 && response.status < 500)) {
					throw new ServiceUnavailableError({
						message: `Tenor API request failed with status ${response.status}`,
					});
				}
				lastError = new Error(`Tenor API request failed with status ${response.status}`);
			} catch (error) {
				if (error instanceof ServiceUnavailableError) {
					throw error;
				}
				lastError = error;
			}
			if (attempt < MAX_RETRIES - 1) {
				await new Promise((resolve) => setTimeout(resolve, BACKOFF_BASE_DELAY * 2 ** attempt));
			}
		}
		throw new ServiceUnavailableError({
			message: lastError instanceof Error ? lastError.message : 'Tenor API request failed',
		});
	}

	private async fetchAndTransformGifs(url: URL): Promise<Array<GifResponse>> {
		const results = readResultsArray(await this.fetchTenorData(url)).filter(isTenorGif);
		return results.map((gif) => this.transformTenorGif(gif)).filter((gif): gif is GifResponse => gif !== null);
	}

	private async getCache<T>(key: string): Promise<{
		data: T;
		isStale: boolean;
	} | null> {
		const cached = await this.cacheService.get<CacheEntry<T>>(key);
		if (!cached) return null;
		const age = Date.now() - cached.timestamp;
		const isStale = age > CACHE_EXPIRATION_TIME;
		return {data: cached.data, isStale};
	}

	private async setCache<T>(key: string, data: T): Promise<void> {
		const cacheEntry: CacheEntry<T> = {
			data,
			timestamp: Date.now(),
		};
		await this.cacheService.set(key, cacheEntry);
	}

	private triggerBackgroundRefresh<T>(key: string, refreshFn: () => Promise<T>): void {
		if (this.refreshingKeys.get(key)) {
			return;
		}
		this.refreshingKeys.set(key, true);
		setImmediate(async () => {
			try {
				const freshData = await refreshFn();
				await this.setCache(key, freshData);
			} catch (error) {
				Logger.debug({key, error}, `Background refresh failed for key ${key}`);
			} finally {
				this.refreshingKeys.delete(key);
			}
		});
	}

	async search(params: {q: string; locale: string; country: string}): Promise<Array<GifResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'search',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				q: params.q,
				country: params.country,
				locale: params.locale,
				contentfilter: DEFAULT_CONTENT_FILTER,
				media_filter: DEFAULT_MEDIA_FILTER,
				limit: 50,
			},
		});
		return this.fetchAndTransformGifs(url);
	}

	async registerShare(params: {id: string; q: string; locale: string; country: string}): Promise<void> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'registershare',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				id: params.id,
				country: params.country,
				locale: params.locale,
				q: params.q,
			},
		});
		await fetch(url.toString(), {
			headers: {'User-Agent': FLUXER_USER_AGENT},
			signal: AbortSignal.timeout(ms('30 seconds')),
		});
	}

	async getFeatured(params: {locale: string; country: string}): Promise<{
		gifs: Array<GifResponse>;
		categories: Array<GifCategoryTagResponse>;
	}> {
		const cacheKey = this.featuredCacheKey(params.locale, params.country);
		const cached = await this.getCache<{
			gifs: Array<GifResponse>;
			categories: Array<GifCategoryTagResponse>;
		}>(cacheKey);
		if (cached) {
			if (cached.isStale) {
				this.triggerBackgroundRefresh(cacheKey, () => this.fetchFeaturedData(params));
			}
			return cached.data;
		}
		const data = await this.fetchFeaturedData(params);
		await this.setCache(cacheKey, data);
		return data;
	}

	private async fetchFeaturedData(params: {locale: string; country: string}): Promise<{
		gifs: Array<GifResponse>;
		categories: Array<GifCategoryTagResponse>;
	}> {
		const [gifs, categories] = await Promise.all([this.getFeaturedGifs(params), this.getFeaturedCategories(params)]);
		return {gifs, categories};
	}

	async getTrendingGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const cacheKey = this.trendingCacheKey(params.locale, params.country);
		const cached = await this.getCache<Array<GifResponse>>(cacheKey);
		if (cached) {
			if (cached.isStale) {
				this.triggerBackgroundRefresh(cacheKey, () => this.fetchTrendingGifs(params));
			}
			return cached.data;
		}
		const gifs = await this.fetchTrendingGifs(params);
		await this.setCache(cacheKey, gifs);
		return gifs;
	}

	private async fetchTrendingGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'featured',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				country: params.country,
				locale: params.locale,
				contentfilter: DEFAULT_CONTENT_FILTER,
				media_filter: DEFAULT_MEDIA_FILTER,
				limit: 50,
			},
		});
		return this.fetchAndTransformGifs(url);
	}

	async suggest(params: {q: string; locale: string}): Promise<Array<string>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'search_suggestions',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				q: params.q,
				locale: params.locale,
				limit: 20,
			},
		});
		return readResultsArray(await this.fetchTenorData(url)).filter(
			(result): result is string => typeof result === 'string',
		);
	}

	async resolveByUrl(params: {url: string; locale: string; country: string}): Promise<GifResponse | null> {
		const slug = this.extractSlugFromUrl(params.url);
		if (!slug) return null;
		const id = this.extractIdFromSlug(slug);
		if (!id) return null;
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'posts',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				ids: id,
				country: params.country,
				locale: params.locale,
				media_filter: DEFAULT_MEDIA_FILTER,
			},
		});
		const [gif] = await this.fetchAndTransformGifs(url);
		return gif ?? null;
	}

	private async getFeaturedGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'featured',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				country: params.country,
				locale: params.locale,
				contentfilter: DEFAULT_CONTENT_FILTER,
				media_filter: DEFAULT_MEDIA_FILTER,
				limit: 1,
			},
		});
		return this.fetchAndTransformGifs(url);
	}

	private async getFeaturedCategories(params: {
		locale: string;
		country: string;
	}): Promise<Array<GifCategoryTagResponse>> {
		trackSeenLocale(this.cacheService, this.meta.name, params.locale, params.country).catch((error) => {
			Logger.debug({err: error, ...params}, 'Failed to track seen GIF locale');
		});
		const rawTags = await this.fetchRawCategoryTags(params);
		const cached = await readEnrichedCategoriesCache(this.cacheService, this.meta.name, params.locale, params.country);
		if (cached) {
			if (cached.isStale) {
				this.scheduleEnrichmentRefresh(params);
			}
			const byName = new Map(cached.data.map((entry) => [entry.name, entry]));
			return rawTags.map((tag) => byName.get(tag.name) ?? tag);
		}
		this.scheduleEnrichmentRefresh(params);
		return rawTags;
	}

	private async fetchRawCategoryTags(params: {
		locale: string;
		country: string;
	}): Promise<Array<GifCategoryTagResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'categories',
			params: {
				key: apiKey,
				client_key: CLIENT_KEY,
				country: params.country,
				locale: params.locale,
				contentfilter: DEFAULT_CONTENT_FILTER,
				type: 'featured',
			},
		});
		const tags = readTagsArray(await this.fetchTenorData(url)).filter(isTenorCategoryTag);
		return tags
			.filter((tag) => Boolean(tag.searchterm) && Boolean(tag.image))
			.map((tag) => ({
				name: tag.searchterm,
				src: tag.image,
				proxy_src: this.mediaService.getExternalMediaProxyURL(tag.image),
				gif: null,
			}));
	}

	private scheduleEnrichmentRefresh(params: {locale: string; country: string}): void {
		(async () => {
			try {
				const workerService = getWorkerService();
				await workerService.addJob('refreshGifFeaturedCategories', {
					provider: this.meta.name,
					locale: params.locale,
					country: params.country,
				});
			} catch (error) {
				Logger.debug({err: error, ...params}, 'Failed to enqueue GIF featured-categories refresh');
			}
		})();
	}

	async refreshFeaturedCategories(params: {locale: string; country: string}): Promise<void> {
		const lockKey = refreshLockKey(this.meta.name, params.locale, params.country);
		const token = await this.cacheService.acquireLock(lockKey, REFRESH_LOCK_TTL_SECONDS);
		if (!token) {
			Logger.debug(params, 'Skipping enriched GIF categories refresh; another worker holds the lock');
			return;
		}
		try {
			const rawTags = await this.fetchRawCategoryTags(params);
			const enriched = await Promise.all(
				rawTags.map(async (tag) => {
					try {
						const [gif] = await this.search({q: tag.name, locale: params.locale, country: params.country});
						return {...tag, gif: gif ?? null};
					} catch (error) {
						Logger.debug({err: error, tag: tag.name, ...params}, 'Failed to enrich GIF category');
						return tag;
					}
				}),
			);
			await writeEnrichedCategoriesCache(this.cacheService, this.meta.name, params.locale, params.country, enriched);
		} catch (error) {
			Logger.warn({err: error, ...params}, 'Failed to refresh enriched GIF categories');
			throw error;
		} finally {
			await this.cacheService.releaseLock(lockKey, token).catch(() => undefined);
		}
	}

	private selectMediaFormat(mediaFormats: Record<string, TenorMediaFormat> | undefined): TenorMediaFormat | null {
		if (!mediaFormats) return null;
		const preferredKeys = ['webm', 'mp4', 'tinywebm', 'tinymp4', 'webp', 'gif', 'tinygif', 'nanogif'];
		for (const key of preferredKeys) {
			const candidate = mediaFormats[key];
			if (candidate?.url && candidate.dims?.length === 2) return candidate;
		}
		for (const candidate of Object.values(mediaFormats)) {
			if (candidate?.url && candidate.dims?.length === 2) return candidate;
		}
		return null;
	}

	private transformTenorGif(input: TenorGif): GifResponse | null {
		const best = this.selectMediaFormat(input.media_formats);
		if (!best) return null;
		const title = input.title?.trim() || input.content_description?.trim() || '';
		const fallbackUrl = `https://tenor.com/view/${encodeURIComponent(input.id)}`;
		const url = input.itemurl?.trim() || input.url?.trim() || fallbackUrl;
		const slug = this.extractSlugFromUrl(url) ?? `view/${input.id}`;
		const media: Record<string, GifMediaFormat> = {};
		for (const key of PUBLIC_MEDIA_FORMATS) {
			const candidate = input.media_formats?.[key];
			if (candidate?.url && candidate.dims?.length === 2) {
				media[key] = {
					src: candidate.url,
					proxy_src: this.mediaService.getExternalMediaProxyURL(candidate.url),
					width: candidate.dims[0],
					height: candidate.dims[1],
				};
			}
		}
		return {
			id: input.id,
			slug,
			provider: this.meta.name,
			title,
			url,
			src: best.url,
			proxy_src: this.mediaService.getExternalMediaProxyURL(best.url),
			width: best.dims[0],
			height: best.dims[1],
			media,
		};
	}

	extractSlugFromUrl(url: string): string | null {
		try {
			const parsed = new URL(url);
			const hostname = parsed.hostname.toLowerCase();
			if (hostname !== 'tenor.com' && hostname !== 'www.tenor.com') return null;
			const match = parsed.pathname.match(/^\/(?:[a-z]{2}\/)?view\/([^/]+)/i);
			if (!match?.[1]) return null;
			const slug = decodeURIComponent(match[1]).trim();
			return slug ? `view/${slug}` : null;
		} catch {
			return null;
		}
	}

	private extractIdFromSlug(slug: string): string | null {
		const normalized = slug.trim().replace(/^view\//i, '');
		if (!normalized) return null;
		const lastDashIndex = normalized.lastIndexOf('-');
		const candidate = lastDashIndex === -1 ? normalized : normalized.slice(lastDashIndex + 1);
		return candidate.trim() || null;
	}

	buildShareUrl(slug: string): string {
		const trimmed = slug.trim().replace(/^\/+|\/+$/g, '');
		const normalized = trimmed.toLowerCase().startsWith('view/') ? trimmed : `view/${trimmed}`;
		return `https://tenor.com/${normalized}`;
	}
}
