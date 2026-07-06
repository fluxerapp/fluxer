// SPDX-License-Identifier: AGPL-3.0-or-later

import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {type NatsConnection, StringCodec} from 'nats';
import {describe, expect, it} from 'vitest';
import {NatsUnfurlerService} from './NatsUnfurlerService';

interface FakeRequest {
	subject: string;
	body: Record<string, unknown>;
	timeout: number | undefined;
}

class FakeNatsConnectionManager implements INatsConnectionManager {
	private readonly codec = StringCodec();
	private closed = true;
	readonly requests: Array<FakeRequest> = [];
	connectCalls = 0;

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
				return {
					data: this.codec.encode(JSON.stringify({Resolved: {embeds: [], cache_ttl_seconds: null}})),
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
});
