// SPDX-License-Identifier: AGPL-3.0-or-later

import {activityPubPublicKeyPem} from '@app/api/activity_pub/ActivityPub';
import type {MediaProxyNsfwMode} from '@app/api/infrastructure/IMediaService';
import {
	type ExternalFetchRequest,
	type ExternalFetchResponse,
	IUnfurlerService,
	type UnfurlOptions,
	type UnfurlResult,
} from '@app/api/infrastructure/IUnfurlerService';
import {requestSvcJson} from '@app/api/infrastructure/SvcErrorReply';
import {Logger} from '@app/api/Logger';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {RequestError, TimeoutError} from '@nats-io/transport-node';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const NATS_UNFURL_SUBJECT = 'svc.unfurl';
const NATS_UNFURL_TIMEOUT_MS = 12000;
const NATS_UNFURL_CACHE_ONLY_TIMEOUT_MS = 1000;
const NATS_VERIFY_DOMAIN_TIMEOUT_MS = 8000;
const NATS_FETCH_TIMEOUT_MS = 6000;
const NATS_RESOLVE_TXT_TIMEOUT_MS = 4000;
const NATS_ACTIVITY_PUB_KEY_TIMEOUT_MS = 2000;
const ACTIVITY_PUB_KEY_CACHE_TTL_MS = 5 * 60 * 1000;

interface NatsUnfurlRequest {
	op: 'Unfurl';
	url: string;
	nsfw_mode: MediaProxyNsfwMode;
	bypass_cache: boolean;
	cache_only: boolean;
	youtube_api_key: string | null;
	klipy_api_key: string | null;
	activity_pub_key_id: string | null;
}

interface NatsUnfurlInnerResult {
	embeds: Array<MessageEmbedResponse>;
	cache_ttl_seconds: number | null;
}

type NatsUnfurlResponse = {Resolved: NatsUnfurlInnerResult} | {Failed: {message: string}};

function isNatsUnfurlResponse(value: unknown): value is NatsUnfurlResponse {
	if (!isJsonRecord(value)) return false;
	if ('Resolved' in value) {
		const resolved = value.Resolved;
		return (
			isJsonRecord(resolved) &&
			Array.isArray(resolved.embeds) &&
			(resolved.cache_ttl_seconds === null || typeof resolved.cache_ttl_seconds === 'number')
		);
	}
	if ('Failed' in value) {
		const failed = value.Failed;
		return isJsonRecord(failed) && typeof failed.message === 'string';
	}
	return false;
}

interface NatsVerifyDomainRequest {
	op: 'VerifyDomain';
	domain: string;
	token: string;
}

type NatsVerifyDomainResponse = {Verified: boolean} | {Failed: {message: string}};

function isNatsVerifyDomainResponse(value: unknown): value is NatsVerifyDomainResponse {
	if (!isJsonRecord(value)) return false;
	if ('Verified' in value) {
		return typeof value.Verified === 'boolean';
	}
	if ('Failed' in value) {
		const failed = value.Failed;
		return isJsonRecord(failed) && typeof failed.message === 'string';
	}
	return false;
}

type NatsFetchResponse = {Fetched: ExternalFetchResponse} | {Failed: {message: string}};

function isHeaderList(value: unknown): value is Array<[string, string]> {
	return (
		Array.isArray(value) &&
		value.every(
			(entry) =>
				Array.isArray(entry) && entry.length === 2 && typeof entry[0] === 'string' && typeof entry[1] === 'string',
		)
	);
}

function isNatsFetchResponse(value: unknown): value is NatsFetchResponse {
	if (!isJsonRecord(value)) return false;
	if ('Fetched' in value) {
		const fetched = value.Fetched;
		return (
			isJsonRecord(fetched) &&
			typeof fetched.status === 'number' &&
			isHeaderList(fetched.headers) &&
			typeof fetched.body === 'string'
		);
	}
	if ('Failed' in value) {
		const failed = value.Failed;
		return isJsonRecord(failed) && typeof failed.message === 'string';
	}
	return false;
}

type NatsTxtResponse = {Txt: Array<string>} | {Failed: {message: string}};

function isNatsTxtResponse(value: unknown): value is NatsTxtResponse {
	if (!isJsonRecord(value)) return false;
	if ('Txt' in value) {
		return Array.isArray(value.Txt) && value.Txt.every((record) => typeof record === 'string');
	}
	if ('Failed' in value) {
		const failed = value.Failed;
		return isJsonRecord(failed) && typeof failed.message === 'string';
	}
	return false;
}

type NatsActivityPubKeyResponse = {ActivityPubKey: string | null};

function isNatsActivityPubKeyResponse(value: unknown): value is NatsActivityPubKeyResponse {
	return (
		isJsonRecord(value) &&
		'ActivityPubKey' in value &&
		(value.ActivityPubKey === null || typeof value.ActivityPubKey === 'string')
	);
}

function mapUnfurlTransportError(error: unknown): Error {
	if (error instanceof FluxerError) {
		return error;
	}
	if (error instanceof RequestError && error.isNoResponders()) {
		return new ServiceUnavailableError({message: '[nats-unfurl] no unfurl service is answering'});
	}
	if (error instanceof TimeoutError) {
		return new GatewayTimeoutError({message: '[nats-unfurl] unfurl service did not answer in time'});
	}
	return new BadGatewayError({message: '[nats-unfurl] request failed'});
}

export class NatsUnfurlerService extends IUnfurlerService {
	private readonly connectionManager: INatsConnectionManager;
	private activityPubKeyCache: {pem: string | null; expiresAt: number} | null = null;

	constructor(
		connectionManager: INatsConnectionManager,
		private readonly resolveYoutubeApiKey: (() => Promise<string | null>) | null = null,
		private readonly resolveKlipyApiKey: (() => Promise<string | null>) | null = null,
		private readonly activityPubKeyId: string | null = null,
	) {
		super();
		this.connectionManager = connectionManager;
	}

	async unfurlWithCachePolicy(
		url: string,
		nsfwMode: MediaProxyNsfwMode = 'block',
		options: UnfurlOptions = {},
	): Promise<UnfurlResult> {
		try {
			const request: NatsUnfurlRequest = {
				op: 'Unfurl',
				url,
				nsfw_mode: nsfwMode,
				bypass_cache: options.bypassCache === true,
				cache_only: options.cacheOnly === true,
				youtube_api_key: this.resolveYoutubeApiKey ? await this.resolveYoutubeApiKey() : null,
				klipy_api_key: this.resolveKlipyApiKey ? await this.resolveKlipyApiKey() : null,
				activity_pub_key_id: this.activityPubKeyId,
			};
			const response = await requestSvcJson(this.connectionManager, NATS_UNFURL_SUBJECT, request, {
				timeoutMs: options.cacheOnly === true ? NATS_UNFURL_CACHE_ONLY_TIMEOUT_MS : NATS_UNFURL_TIMEOUT_MS,
				guard: isNatsUnfurlResponse,
				service: 'nats-unfurl',
				invalidPayloadError: () => new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
			});
			if ('Resolved' in response) {
				return {
					embeds: response.Resolved.embeds ?? [],
					cacheTtlSeconds: response.Resolved.cache_ttl_seconds ?? null,
				};
			}
			if ('Failed' in response) {
				throw new BadGatewayError({message: `[nats-unfurl] service error: ${response.Failed.message}`});
			}
			throw new BadGatewayError({message: '[nats-unfurl] unexpected response variant'});
		} catch (error) {
			if (options.signal?.aborted) {
				Logger.warn({url}, '[nats-unfurl] request aborted');
			} else {
				Logger.error({error, url}, '[nats-unfurl] failed to unfurl URL');
			}
			throw mapUnfurlTransportError(error);
		}
	}

	async verifyDomain(domain: string, token: string): Promise<boolean> {
		try {
			const request: NatsVerifyDomainRequest = {op: 'VerifyDomain', domain, token};
			const response = await requestSvcJson(this.connectionManager, NATS_UNFURL_SUBJECT, request, {
				timeoutMs: NATS_VERIFY_DOMAIN_TIMEOUT_MS,
				guard: isNatsVerifyDomainResponse,
				service: 'nats-unfurl',
				invalidPayloadError: () => new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
			});
			if ('Verified' in response) {
				return response.Verified;
			}
			Logger.warn({domain, message: response.Failed.message}, '[nats-unfurl] domain verification rejected');
			return false;
		} catch (error) {
			Logger.error({error, domain}, '[nats-unfurl] failed to verify domain');
			throw mapUnfurlTransportError(error);
		}
	}

	async fetchExternal(request: ExternalFetchRequest): Promise<ExternalFetchResponse> {
		const response = await requestSvcJson(
			this.connectionManager,
			NATS_UNFURL_SUBJECT,
			{op: 'Fetch', ...request},
			{
				timeoutMs: NATS_FETCH_TIMEOUT_MS,
				guard: isNatsFetchResponse,
				service: 'nats-unfurl',
				invalidPayloadError: () => new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
			},
		).catch((error: unknown) => {
			throw mapUnfurlTransportError(error);
		});
		if ('Fetched' in response) {
			return response.Fetched;
		}
		throw new BadGatewayError({message: `[nats-unfurl] fetch failed: ${response.Failed.message}`});
	}

	async resolveTxt(name: string): Promise<Array<string>> {
		const response = await requestSvcJson(
			this.connectionManager,
			NATS_UNFURL_SUBJECT,
			{op: 'ResolveTxt', name},
			{
				timeoutMs: NATS_RESOLVE_TXT_TIMEOUT_MS,
				guard: isNatsTxtResponse,
				service: 'nats-unfurl',
				invalidPayloadError: () => new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
			},
		).catch((error: unknown) => {
			throw mapUnfurlTransportError(error);
		});
		if ('Txt' in response) {
			return response.Txt;
		}
		throw new BadGatewayError({message: `[nats-unfurl] TXT lookup failed: ${response.Failed.message}`});
	}

	async activityPubPublicKey(): Promise<string | null> {
		const cached = this.activityPubKeyCache;
		if (cached !== null && cached.expiresAt > Date.now()) {
			return cached.pem;
		}
		const response = await requestSvcJson(
			this.connectionManager,
			NATS_UNFURL_SUBJECT,
			{op: 'ActivityPubKey'},
			{
				timeoutMs: NATS_ACTIVITY_PUB_KEY_TIMEOUT_MS,
				guard: isNatsActivityPubKeyResponse,
				service: 'nats-unfurl',
				invalidPayloadError: () => new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
			},
		).catch((error: unknown) => {
			Logger.warn({error}, '[nats-unfurl] failed to fetch ActivityPub public key');
			throw new ServiceUnavailableError({message: '[nats-unfurl] ActivityPub public key is unavailable'});
		});
		const pem = response.ActivityPubKey === null ? null : activityPubPublicKeyPem(response.ActivityPubKey);
		this.activityPubKeyCache = {pem, expiresAt: Date.now() + ACTIVITY_PUB_KEY_CACHE_TTL_MS};
		return pem;
	}
}
