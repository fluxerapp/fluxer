// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {StringCodec} from 'nats';
import {Logger} from '../Logger';
import {isJsonRecord, parseJsonWithGuard} from '../utils/JsonBoundaryUtils';
import type {MediaProxyNsfwMode} from './IMediaService';
import {IUnfurlerService, type UnfurlOptions, type UnfurlResult} from './IUnfurlerService';

const NATS_UNFURL_SUBJECT = 'svc.unfurl';
const NATS_UNFURL_TIMEOUT_MS = 12000;
const NATS_UNFURL_CACHE_ONLY_TIMEOUT_MS = 1000;

interface NatsUnfurlRequest {
	op: 'Unfurl';
	url: string;
	nsfw_mode: MediaProxyNsfwMode;
	bypass_cache: boolean;
	cache_only: boolean;
	youtube_api_key: string | null;
	klipy_api_key: string | null;
}

interface NatsUnfurlInnerResult {
	embeds: Array<MessageEmbedResponse>;
	cache_ttl_seconds: number | null;
}

type NatsUnfurlResponse =
	| {Resolved: NatsUnfurlInnerResult}
	| {Invalidated: {invalidated: boolean}}
	| {Failed: {message: string}};

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
	if ('Invalidated' in value) {
		const invalidated = value.Invalidated;
		return isJsonRecord(invalidated) && typeof invalidated.invalidated === 'boolean';
	}
	if ('Failed' in value) {
		const failed = value.Failed;
		return isJsonRecord(failed) && typeof failed.message === 'string';
	}
	return false;
}

export class NatsUnfurlerService extends IUnfurlerService {
	private readonly connectionManager: INatsConnectionManager;
	private readonly codec = StringCodec();

	constructor(
		connectionManager: INatsConnectionManager,
		private readonly resolveYoutubeApiKey: (() => Promise<string | null>) | null = null,
		private readonly resolveKlipyApiKey: (() => Promise<string | null>) | null = null,
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
			};
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			const payload = this.codec.encode(JSON.stringify(request));
			const responseMsg = await connection.request(NATS_UNFURL_SUBJECT, payload, {
				timeout: options.cacheOnly === true ? NATS_UNFURL_CACHE_ONLY_TIMEOUT_MS : NATS_UNFURL_TIMEOUT_MS,
			});
			const responseText = this.codec.decode(responseMsg.data);
			const response = parseJsonWithGuard(responseText, isNatsUnfurlResponse);
			if (!response) {
				throw new Error(`[nats-unfurl] invalid response payload: ${responseText}`);
			}
			if ('Resolved' in response) {
				return {
					embeds: response.Resolved.embeds ?? [],
					cacheTtlSeconds: response.Resolved.cache_ttl_seconds ?? null,
				};
			}
			if ('Failed' in response) {
				throw new Error(`[nats-unfurl] service error: ${response.Failed.message}`);
			}
			throw new Error(`[nats-unfurl] unexpected response variant: ${responseText}`);
		} catch (error) {
			if (options.signal?.aborted) {
				Logger.warn({url}, '[nats-unfurl] request aborted');
			} else {
				Logger.error({error, url}, '[nats-unfurl] failed to unfurl URL');
			}
			throw error;
		}
	}
}
