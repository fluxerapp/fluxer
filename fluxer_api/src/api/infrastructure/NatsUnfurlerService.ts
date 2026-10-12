// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MediaProxyNsfwMode} from '@app/api/infrastructure/IMediaService';
import {IUnfurlerService, type UnfurlOptions, type UnfurlResult} from '@app/api/infrastructure/IUnfurlerService';
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
}
