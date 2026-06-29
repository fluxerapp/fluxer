// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {
	type GifCategoryTagResponse,
	GifCategoryTagResponse as GifCategoryTagResponseSchema,
	type GifResponse,
	GifResponse as GifResponseSchema,
} from '@fluxer/schema/src/domains/gif/GifSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {StringCodec} from 'nats';
import {Config} from '../Config';
import {Logger} from '../Logger';
import {isJsonRecord, parseJsonUnknown} from '../utils/JsonBoundaryUtils';
import type {GifProviderMeta, IGifProvider} from './IGifProvider';

const GIF_SERVICE_SUBJECT = process.env.FLUXER_GIF_SERVICE_SUBJECT || 'svc.gifs';
const DEFAULT_GIF_SERVICE_TIMEOUT_MS = 12_000;
const DEFAULT_GIF_SERVICE_REGISTER_SHARE_TIMEOUT_MS = 3_000;
const GIF_PROVIDER_META: GifProviderMeta = {
	name: 'klipy',
	displayName: 'KLIPY',
	attributionRequired: true,
};
const KLIPY_SHARE_ORIGIN = 'https://klipy.com';
const KLIPY_SHARE_HOSTS = new Set(['klipy.com', 'www.klipy.com']);

type GifApiKeyResolver = () => Promise<string | null>;

type NatsGifRequest =
	| {op: 'IsAvailable'; api_key: string | null}
	| {op: 'Search'; api_key: string; q: string; locale: string; country: string}
	| {op: 'GetFeatured'; api_key: string; locale: string; country: string}
	| {op: 'GetTrendingGifs'; api_key: string; locale: string; country: string}
	| {op: 'Suggest'; api_key: string; q: string; locale: string}
	| {op: 'RegisterShare'; api_key: string; id: string; q: string; locale: string; country: string}
	| {op: 'ResolveByUrl'; api_key: string; url: string; locale: string; country: string};

export function extractKlipySlugFromUrl(rawUrl: string): string | null {
	let parsed: URL;
	try {
		parsed = new URL(rawUrl);
	} catch {
		return null;
	}
	if (!KLIPY_SHARE_HOSTS.has(parsed.hostname.toLowerCase())) {
		return null;
	}
	const segments = parsed.pathname.split('/').filter(Boolean);
	const kind = segments[0]?.toLowerCase();
	const slug = segments[1]?.trim();
	if (!slug) {
		return null;
	}
	switch (kind) {
		case 'gif':
		case 'gifs':
		case 'clip':
		case 'clips':
			return slug;
		default:
			return null;
	}
}

export function buildKlipyShareUrl(slug: string): string {
	const trimmed = slug.trim();
	if (!trimmed) {
		return `${KLIPY_SHARE_ORIGIN}/gifs`;
	}
	return `${KLIPY_SHARE_ORIGIN}/gifs/${encodeURIComponent(trimmed)}`;
}

function readPositiveIntegerEnv(name: string, fallback: number): number {
	const value = process.env[name];
	if (!value) return fallback;
	const parsed = Number(value);
	return Number.isInteger(parsed) && parsed > 0 ? parsed : fallback;
}

function readFailedMessage(value: unknown): string | null {
	if (!isJsonRecord(value)) return null;
	if (typeof value.error === 'string') return value.error;
	if (!('Failed' in value)) return null;
	const failed = value.Failed;
	return isJsonRecord(failed) && typeof failed.message === 'string' ? failed.message : 'GIF service failed';
}

function readVariant(value: unknown, variant: string): unknown {
	if (!isJsonRecord(value) || !(variant in value)) {
		throw new ServiceUnavailableError({message: `GIF service returned an unexpected ${variant} response`});
	}
	return value[variant];
}

function readGifList(value: unknown, variant: string): Array<GifResponse> {
	const parsed = GifResponseSchema.array().safeParse(readVariant(value, variant));
	if (!parsed.success) {
		throw new ServiceUnavailableError({message: `GIF service returned invalid ${variant} data`});
	}
	return parsed.data;
}

function readFeatured(value: unknown): {
	gifs: Array<GifResponse>;
	categories: Array<GifCategoryTagResponse>;
} {
	const featured = readVariant(value, 'Featured');
	if (!isJsonRecord(featured)) {
		throw new ServiceUnavailableError({message: 'GIF service returned invalid featured data'});
	}
	const gifs = GifResponseSchema.array().safeParse(featured.gifs);
	const categories = GifCategoryTagResponseSchema.array().safeParse(featured.categories);
	if (!gifs.success || !categories.success) {
		throw new ServiceUnavailableError({message: 'GIF service returned invalid featured data'});
	}
	return {gifs: gifs.data, categories: categories.data};
}

function readSuggestions(value: unknown): Array<string> {
	const suggestions = readVariant(value, 'Suggestions');
	if (!Array.isArray(suggestions) || !suggestions.every((suggestion) => typeof suggestion === 'string')) {
		throw new ServiceUnavailableError({message: 'GIF service returned invalid suggestions'});
	}
	return suggestions;
}

function readResolved(value: unknown): GifResponse | null {
	const resolved = readVariant(value, 'Resolved');
	if (!isJsonRecord(resolved)) {
		throw new ServiceUnavailableError({message: 'GIF service returned invalid resolved data'});
	}
	if (resolved.gif === null || resolved.gif === undefined) return null;
	const parsed = GifResponseSchema.safeParse(resolved.gif);
	if (!parsed.success) {
		throw new ServiceUnavailableError({message: 'GIF service returned invalid resolved GIF'});
	}
	return parsed.data;
}

class NatsGifProvider implements IGifProvider {
	readonly meta = GIF_PROVIDER_META;
	private readonly codec = StringCodec();

	constructor(
		private readonly connectionManager: INatsConnectionManager,
		private readonly apiKeyResolver: GifApiKeyResolver,
		private readonly requestTimeoutMs = readPositiveIntegerEnv(
			'FLUXER_GIF_SERVICE_TIMEOUT_MS',
			DEFAULT_GIF_SERVICE_TIMEOUT_MS,
		),
		private readonly registerShareTimeoutMs = readPositiveIntegerEnv(
			'FLUXER_GIF_SERVICE_REGISTER_SHARE_TIMEOUT_MS',
			DEFAULT_GIF_SERVICE_REGISTER_SHARE_TIMEOUT_MS,
		),
		private readonly subject = GIF_SERVICE_SUBJECT,
	) {}

	async isAvailable(): Promise<boolean> {
		return Boolean((await this.apiKeyResolver())?.trim());
	}

	async search(params: {q: string; locale: string; country: string}): Promise<Array<GifResponse>> {
		const response = await this.request({
			op: 'Search',
			api_key: await this.getApiKey(),
			q: params.q,
			locale: params.locale,
			country: params.country,
		});
		return readGifList(response, 'SearchResults');
	}

	async registerShare(params: {id: string; q: string; locale: string; country: string}): Promise<void> {
		const response = await this.request(
			{
				op: 'RegisterShare',
				api_key: await this.getApiKey(),
				id: params.id,
				q: params.q,
				locale: params.locale,
				country: params.country,
			},
			this.registerShareTimeoutMs,
		);
		if (response !== 'Registered') {
			throw new ServiceUnavailableError({message: 'GIF service returned an unexpected register-share response'});
		}
	}

	async getFeatured(params: {locale: string; country: string}): Promise<{
		gifs: Array<GifResponse>;
		categories: Array<GifCategoryTagResponse>;
	}> {
		return readFeatured(
			await this.request({
				op: 'GetFeatured',
				api_key: await this.getApiKey(),
				locale: params.locale,
				country: params.country,
			}),
		);
	}

	async getTrendingGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>> {
		const response = await this.request({
			op: 'GetTrendingGifs',
			api_key: await this.getApiKey(),
			locale: params.locale,
			country: params.country,
		});
		return readGifList(response, 'TrendingResults');
	}

	async suggest(params: {q: string; locale: string}): Promise<Array<string>> {
		return readSuggestions(
			await this.request({
				op: 'Suggest',
				api_key: await this.getApiKey(),
				q: params.q,
				locale: params.locale,
			}),
		);
	}

	async resolveByUrl(params: {url: string; locale: string; country: string}): Promise<GifResponse | null> {
		return readResolved(
			await this.request({
				op: 'ResolveByUrl',
				api_key: await this.getApiKey(),
				url: params.url,
				locale: params.locale,
				country: params.country,
			}),
		);
	}

	buildShareUrl(slug: string): string {
		return buildKlipyShareUrl(slug);
	}

	extractSlugFromUrl(url: string): string | null {
		return extractKlipySlugFromUrl(url);
	}

	private async getApiKey(): Promise<string> {
		const apiKey = (await this.apiKeyResolver())?.trim();
		if (!apiKey) {
			throw new FeatureTemporarilyDisabledError();
		}
		return apiKey;
	}

	private async request(payload: NatsGifRequest, timeout = this.requestTimeoutMs): Promise<unknown> {
		try {
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			const response = await connection.request(this.subject, this.codec.encode(JSON.stringify(payload)), {timeout});
			const decoded = this.codec.decode(response.data);
			const parsed = parseJsonUnknown(decoded);
			const failedMessage = readFailedMessage(parsed);
			if (failedMessage) {
				throw new ServiceUnavailableError({message: failedMessage});
			}
			return parsed;
		} catch (error) {
			Logger.warn({error, op: payload.op}, '[gif-service] request failed');
			if (error instanceof FeatureTemporarilyDisabledError || error instanceof ServiceUnavailableError) {
				throw error;
			}
			throw new ServiceUnavailableError({message: 'GIF service is temporarily unavailable'});
		}
	}
}

export function createNatsGifProvider(apiKeyResolver: GifApiKeyResolver): NatsGifProvider {
	const manager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: process.env.FLUXER_GIF_SERVICE_NATS_CLIENT_NAME || 'fluxer-api-gifs',
	});
	void manager.connect().catch((error) => {
		Logger.warn({error}, '[gif-service] Failed to establish NATS connection');
	});
	return new NatsGifProvider(manager, apiKeyResolver);
}
