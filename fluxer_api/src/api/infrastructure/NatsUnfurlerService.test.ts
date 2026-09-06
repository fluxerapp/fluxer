// SPDX-License-Identifier: AGPL-3.0-or-later

import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {type NatsConnection, StringCodec} from 'nats';
import {describe, expect, it} from 'vitest';
import {NatsUnfurlerService} from './NatsUnfurlerService';

interface FakeRequest {
	subject: string;
	body: Record<string, unknown>;
	timeout: number | undefined;
}

const RESOLVED_REPLY = JSON.stringify({Resolved: {embeds: [], cache_ttl_seconds: null}});

function natsErrorWithCode(code: string): Error {
	return Object.assign(new Error('nats request failed'), {code});
}

class FakeNatsConnectionManager implements INatsConnectionManager {
	private readonly codec = StringCodec();
	private closed = true;
	readonly requests: Array<FakeRequest> = [];
	connectCalls = 0;

	constructor(
		private readonly replyText: string = RESOLVED_REPLY,
		private readonly requestError: Error | null = null,
	) {}

	async connect(): Promise<void> {
		this.connectCalls += 1;
		this.closed = false;
	}

	getConnection(): NatsConnection {
		if (this.closed) {
			throw new Error('not connected');
		}
		return {
			request: async (subject: string, data: Uint8Array, options?: {timeout?: number}) => {
				this.requests.push({
					subject,
					body: JSON.parse(this.codec.decode(data)) as Record<string, unknown>,
					timeout: options?.timeout,
				});
				if (this.requestError) {
					throw this.requestError;
				}
				return {
					data: this.codec.encode(this.replyText),
				};
			},
		} as unknown as NatsConnection;
	}

	async drain(): Promise<void> {
		this.closed = true;
	}

	isClosed(): boolean {
		return this.closed;
	}
}

describe('NatsUnfurlerService', () => {
	it('does not send media proxy configuration in unfurl requests', async () => {
		const manager = new FakeNatsConnectionManager();
		const service = new NatsUnfurlerService(manager);

		await service.unfurlWithCachePolicy('https://fxtwitter.com/example/status/1', 'flag', {
			bypassCache: true,
			cacheOnly: false,
		});

		expect(manager.connectCalls).toBe(1);
		expect(manager.requests).toEqual([
			{
				subject: 'svc.unfurl',
				body: {
					op: 'Unfurl',
					url: 'https://fxtwitter.com/example/status/1',
					nsfw_mode: 'flag',
					bypass_cache: true,
					cache_only: false,
					youtube_api_key: null,
					klipy_api_key: null,
				},
				timeout: 12000,
			},
		]);
		expect(manager.requests[0]?.body).not.toHaveProperty('media_endpoint');
		expect(manager.requests[0]?.body).not.toHaveProperty('media_proxy_endpoint');
		expect(manager.requests[0]?.body).not.toHaveProperty('media_proxy_secret_key');
	});

	it('rejects with a bad gateway error when the unfurl service reports a failure', async () => {
		const manager = new FakeNatsConnectionManager(JSON.stringify({Failed: {message: 'upstream exploded'}}));
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(BadGatewayError);
	});

	it('rejects with a bad gateway error when the reply payload is unreadable', async () => {
		const manager = new FakeNatsConnectionManager('not json');
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(BadGatewayError);
	});

	it('rejects with a gateway timeout error when the request times out', async () => {
		const manager = new FakeNatsConnectionManager(RESOLVED_REPLY, natsErrorWithCode('TIMEOUT'));
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(GatewayTimeoutError);
	});

	it('rejects with a service unavailable error when no responders answer', async () => {
		const manager = new FakeNatsConnectionManager(RESOLVED_REPLY, natsErrorWithCode('503'));
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(ServiceUnavailableError);
	});

	it('rejects with a service unavailable error when the shard rejects the request', async () => {
		const manager = new FakeNatsConnectionManager(JSON.stringify({error: 'overloaded'}));
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(ServiceUnavailableError);
	});
});
