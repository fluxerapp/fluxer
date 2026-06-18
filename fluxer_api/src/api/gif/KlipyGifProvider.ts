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

const KLIPY_BASE_URL = 'https://api.klipy.com/v2';
const DEFAULT_CONTENT_FILTER = 'low';
const CLIENT_KEY = 'fluxer';
const MAX_RETRIES = 3;
const BACKOFF_BASE_DELAY = ms('1 second');
const CACHE_EXPIRATION_TIME = ms('5 minutes');
const KLIPY_PROVIDER_META: GifProviderMeta = {
	name: 'klipy',
	displayName: 'KLIPY',
	attributionRequired: true,
};

interface KlipyFileEntry {
	url?: string;
	width?: number;
	height?: number;
	size?: number;
}

type KlipyFormatKey = 'gif' | 'webp' | 'mp4' | 'webm';
type KlipySizeKey = 'hd' | 'md' | 'sm' | 'xs';
type KlipyFileBucket = Partial<Record<KlipyFormatKey, KlipyFileEntry>>;

interface KlipyGif {
	id: string;
	slug?: string;
	title: string;
	itemurl: string;
	file?: Partial<Record<KlipySizeKey, KlipyFileBucket>>;
	media_formats?: {
		webm?: {
			url: string;
			dims: [number, number];
		};
	};
}

const KLIPY_SIZE_PREFERENCE: ReadonlyArray<KlipySizeKey> = ['hd', 'md', 'sm', 'xs'];
const KLIPY_FORMAT_KEYS: ReadonlyArray<KlipyFormatKey> = ['webm', 'mp4', 'webp', 'gif'];
const KLIPY_PUBLIC_FORMAT_KEYS: Record<KlipySizeKey, Record<KlipyFormatKey, string>> = {
	hd: {webm: 'webm', mp4: 'mp4', webp: 'webp', gif: 'gif'},
	md: {webm: 'mediumwebm', mp4: 'mediummp4', webp: 'mediumwebp', gif: 'mediumgif'},
	sm: {webm: 'tinywebm', mp4: 'tinymp4', webp: 'tinywebp', gif: 'tinygif'},
	xs: {webm: 'nanowebm', mp4: 'nanomp4', webp: 'nanowebp', gif: 'nanogif'},
};

interface KlipyCategoryTag {
	searchterm: string;
}

function isKlipyFileEntry(value: unknown): value is KlipyFileEntry {
	return (
		isJsonRecord(value) &&
		(value.url === undefined || typeof value.url === 'string') &&
		(value.width === undefined || typeof value.width === 'number') &&
		(value.height === undefined || typeof value.height === 'number') &&
		(value.size === undefined || typeof value.size === 'number')
	);
}

function isKlipyFileBucket(value: unknown): value is KlipyFileBucket {
	return isJsonRecord(value) && Object.values(value).every(isKlipyFileEntry);
}

function isKlipyFallbackMediaFormat(value: unknown): value is NonNullable<KlipyGif['media_formats']>['webm'] {
	if (!isJsonRecord(value) || typeof value.url !== 'string' || !Array.isArray(value.dims)) return false;
	return value.dims.length === 2 && value.dims.every((dimension) => typeof dimension === 'number');
}

function isKlipyGif(value: unknown): value is KlipyGif {
	if (
		!isJsonRecord(value) ||
		typeof value.id !== 'string' ||
		typeof value.title !== 'string' ||
		typeof value.itemurl !== 'string'
	) {
		return false;
	}
	return (
		(value.slug === undefined || typeof value.slug === 'string') &&
		(value.file === undefined || (isJsonRecord(value.file) && Object.values(value.file).every(isKlipyFileBucket))) &&
		(value.media_formats === undefined ||
			(isJsonRecord(value.media_formats) &&
				(value.media_formats.webm === undefined || isKlipyFallbackMediaFormat(value.media_formats.webm))))
	);
}

function isKlipyCategoryTag(value: unknown): value is KlipyCategoryTag {
	return isJsonRecord(value) && typeof value.searchterm === 'string';
}

function readResultsArray(value: unknown): Array<unknown> {
	if (!isJsonRecord(value) || !Array.isArray(value.results)) {
		throw new Error('KLIPY API response did not include a results array');
	}
	return value.results;
}

function readTagsArray(value: unknown): Array<unknown> {
	if (!isJsonRecord(value) || !Array.isArray(value.tags)) {
		throw new Error('KLIPY API response did not include a tags array');
	}
	return value.tags;
}

type CacheEntry<T> = {
	data: T;
	timestamp: number;
};

interface KlipyPath {
	type: 'gif' | 'clip';
	slug: string;
}
type GifApiKeyResolver = () => Promise<string | null>;

export class KlipyGifProvider implements IGifProvider {
	readonly meta = KLIPY_PROVIDER_META;
	private readonly FEATURED_CACHE_KEY = 'klipy:featured';
	private readonly TRENDING_CACHE_KEY = 'klipy:trending';
	private refreshingKeys: Map<string, boolean> = new Map();

	constructor(
		private cacheService: ICacheService,
		private mediaService: IMediaService,
		private apiKeyResolver: GifApiKeyResolver = async () => Config.klipy.apiKey || null,
	) {}

	async isAvailable(): Promise<boolean> {
		return Boolean(await this.apiKeyResolver());
	}

	private async getApiKey(): Promise<string> {
		const apiKey = await this.apiKeyResolver();
		if (!apiKey) {
			throw new Error('KLIPY API key is not configured');
		}
		return apiKey;
	}

	private createURL({endpoint, params}: {endpoint: string; params: Record<string, string | number | undefined>}): URL {
		const url = new URL(`${KLIPY_BASE_URL}/${endpoint}`);
		const defaultParams = {
			client_key: CLIENT_KEY,
			contentfilter: DEFAULT_CONTENT_FILTER,
			...params,
		};
		for (const [key, value] of Object.entries(defaultParams)) {
			if (value !== undefined) {
				url.searchParams.append(key, value.toString());
			}
		}
		return url;
	}

	private async fetchKlipyData(url: URL): Promise<unknown> {
		let lastError = new Error('KLIPY API request failed');
		for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
			try {
				const response = await fetch(url.toString(), {
					headers: {'User-Agent': FLUXER_USER_AGENT},
					signal: AbortSignal.timeout(ms('30 seconds')),
				});
				if (response.ok) {
					const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
						maxBytes: EXTERNAL_RESPONSE_LIMITS.klipyApiBytes,
						headers: response.headers,
						url: response.url,
						description: 'KLIPY API response',
					});
					return parseJsonUnknown(responseText);
				}
				if (response.status === 429 || (response.status >= 400 && response.status < 500)) {
					throw new ServiceUnavailableError({
						message: `KLIPY API request failed with status ${response.status}`,
					});
				}
				lastError = new Error(`KLIPY API request failed with status ${response.status}`);
			} catch (error) {
				if (error instanceof ServiceUnavailableError) {
					throw error;
				}
				lastError = error instanceof Error ? error : new Error(String(error));
			}
			if (attempt < MAX_RETRIES - 1) {
				await new Promise((resolve) => setTimeout(resolve, BACKOFF_BASE_DELAY * 2 ** attempt));
			}
		}
		throw new ServiceUnavailableError({message: lastError.message});
	}

	private async fetchAndTransformGifs(url: URL): Promise<Array<GifResponse>> {
		const results = readResultsArray(await this.fetchKlipyData(url)).filter(isKlipyGif);
		return results.map((gif) => this.transformKlipyGif(gif)).filter((gif): gif is GifResponse => gif !== null);
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
				q: params.q,
				country: params.country,
				locale: params.locale,
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
		const cached = await this.getCache<{
			gifs: Array<GifResponse>;
			categories: Array<GifCategoryTagResponse>;
		}>(this.FEATURED_CACHE_KEY);
		if (cached) {
			if (cached.isStale) {
				this.triggerBackgroundRefresh(this.FEATURED_CACHE_KEY, () => this.fetchFeaturedData(params));
			}
			return cached.data;
		}
		const data = await this.fetchFeaturedData(params);
		await this.setCache(this.FEATURED_CACHE_KEY, data);
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
		const cached = await this.getCache<Array<GifResponse>>(this.TRENDING_CACHE_KEY);
		if (cached) {
			if (cached.isStale) {
				this.triggerBackgroundRefresh(this.TRENDING_CACHE_KEY, () => this.fetchTrendingGifs(params));
			}
			return cached.data;
		}
		const gifs = await this.fetchTrendingGifs(params);
		await this.setCache(this.TRENDING_CACHE_KEY, gifs);
		return gifs;
	}

	private async fetchTrendingGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'featured',
			params: {
				key: apiKey,
				country: params.country,
				locale: params.locale,
				limit: 50,
			},
		});
		return this.fetchAndTransformGifs(url);
	}

	async suggest(params: {q: string; locale: string}): Promise<Array<string>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'autocomplete',
			params: {
				key: apiKey,
				q: params.q,
				locale: params.locale,
			},
		});
		return readResultsArray(await this.fetchKlipyData(url)).filter(
			(result): result is string => typeof result === 'string',
		);
	}

	async resolveByUrl(params: {url: string; locale: string; country: string}): Promise<GifResponse | null> {
		const slug = this.extractSlugFromUrl(params.url);
		if (!slug) return null;
		const results = await this.search({q: slug, locale: params.locale, country: params.country});
		return results.find((gif) => gif.slug === slug || this.extractSlugFromUrl(gif.url) === slug) ?? results[0] ?? null;
	}

	private async getFeaturedGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const apiKey = await this.getApiKey();
		const url = this.createURL({
			endpoint: 'featured',
			params: {
				key: apiKey,
				country: params.country,
				locale: params.locale,
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
				country: params.country,
				locale: params.locale,
				type: 'featured',
			},
		});
		const tags = readTagsArray(await this.fetchKlipyData(url)).filter(isKlipyCategoryTag);
		return tags
			.filter((tag) => Boolean(tag.searchterm))
			.map((tag) => ({
				name: tag.searchterm,
				src: '',
				proxy_src: '',
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
						if (!gif) return tag;
						return {
							...tag,
							src: gif.src,
							proxy_src: gif.proxy_src,
							gif,
						};
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

	private parseKlipyPath(url: string): KlipyPath | null {
		try {
			const parsed = new URL(url);
			const hostname = parsed.hostname.toLowerCase();
			if (hostname !== 'klipy.com' && hostname !== 'www.klipy.com') return null;
			const match = parsed.pathname.match(/^\/(gif|gifs|clip|clips)\/([^/]+)/i);
			if (!match?.[1] || !match[2]) return null;
			const type = match[1].toLowerCase().startsWith('clip') ? 'clip' : 'gif';
			const slug = decodeURIComponent(match[2]).trim();
			return slug ? {type, slug} : null;
		} catch {
			return null;
		}
	}

	private buildKlipyPageUrl(path: KlipyPath): string {
		const basePath = path.type === 'clip' ? 'clips' : 'gifs';
		return `https://klipy.com/${basePath}/${encodeURIComponent(path.slug)}`;
	}

	private toMediaFormat(entry: KlipyFileEntry | undefined): GifMediaFormat | null {
		if (!entry?.url) return null;
		const width = typeof entry.width === 'number' && entry.width > 0 ? entry.width : 0;
		const height = typeof entry.height === 'number' && entry.height > 0 ? entry.height : 0;
		if (!width || !height) return null;
		return {
			src: entry.url,
			proxy_src: this.mediaService.getExternalMediaProxyURL(entry.url),
			width,
			height,
		};
	}

	private collectKlipyMedia(input: KlipyGif): {
		media: Record<string, GifMediaFormat>;
		preferred: GifMediaFormat | null;
	} {
		const media: Record<string, GifMediaFormat> = {};
		let preferred: GifMediaFormat | null = null;
		for (const size of KLIPY_SIZE_PREFERENCE) {
			const bucket = input.file?.[size];
			if (!bucket) continue;
			for (const format of KLIPY_FORMAT_KEYS) {
				const entry = this.toMediaFormat(bucket[format]);
				if (!entry) continue;
				const publicKey = KLIPY_PUBLIC_FORMAT_KEYS[size][format];
				media[publicKey] = entry;
				if (!preferred) preferred = entry;
			}
		}
		if (Object.keys(media).length === 0 && input.media_formats?.webm) {
			const webm = input.media_formats.webm;
			const fallback: GifMediaFormat = {
				src: webm.url,
				proxy_src: this.mediaService.getExternalMediaProxyURL(webm.url),
				width: webm.dims[0],
				height: webm.dims[1],
			};
			media.webm = fallback;
			preferred = fallback;
		}
		return {media, preferred};
	}

	private transformKlipyGif(input: KlipyGif): GifResponse | null {
		const parsedPath = this.parseKlipyPath(input.itemurl);
		const explicitSlug = input.slug?.trim();
		const normalizedSlug = explicitSlug || parsedPath?.slug || input.id;
		const normalizedType = parsedPath?.type ?? 'gif';
		const normalizedUrl =
			parsedPath || explicitSlug ? this.buildKlipyPageUrl({type: normalizedType, slug: normalizedSlug}) : input.itemurl;
		const {media, preferred} = this.collectKlipyMedia(input);
		const top = media.webm ?? preferred;
		if (!top) return null;
		return {
			id: normalizedSlug,
			slug: normalizedSlug,
			provider: this.meta.name,
			title: input.title,
			url: normalizedUrl,
			src: top.src,
			proxy_src: top.proxy_src,
			width: top.width,
			height: top.height,
			media,
		};
	}

	extractSlugFromUrl(url: string): string | null {
		return this.parseKlipyPath(url)?.slug ?? null;
	}

	buildShareUrl(slug: string): string {
		const trimmed = slug.trim();
		if (!trimmed) return 'https://klipy.com/gifs';
		return this.buildKlipyPageUrl({type: 'gif', slug: trimmed});
	}
}
