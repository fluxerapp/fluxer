// SPDX-License-Identifier: AGPL-3.0-or-later

import {NatsUnfurlerService} from '@app/api/infrastructure/NatsUnfurlerService';
import {FakeNatsConnectionManager} from '@app/api/test/mocks/FakeNatsConnectionManager';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {NoRespondersError, RequestError} from '@nats-io/transport-node';
import {describe, expect, it} from 'vitest';

const RESOLVED_REPLY = JSON.stringify({Resolved: {embeds: [], cache_ttl_seconds: null}});

describe('NatsUnfurlerService', () => {
	it('does not send media proxy configuration in unfurl requests', async () => {
		const manager = new FakeNatsConnectionManager(() => RESOLVED_REPLY);
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

	it('rejects with a bad gateway error when the reply payload is unreadable', async () => {
		const manager = new FakeNatsConnectionManager(() => 'not json');
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toEqual(
			new BadGatewayError({message: '[nats-unfurl] invalid response payload'}),
		);
	});

	it('rejects with a service unavailable error when no responders answer', async () => {
		const manager = new FakeNatsConnectionManager(
			() => new RequestError("no responders: 'svc.unfurl'", {cause: new NoRespondersError('svc.unfurl')}),
		);
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(ServiceUnavailableError);
	});

	it('rejects with a service unavailable error when the shard rejects the request', async () => {
		const manager = new FakeNatsConnectionManager(() => JSON.stringify({error: 'overloaded'}));
		const service = new NatsUnfurlerService(manager);

		await expect(service.unfurlWithCachePolicy('https://example.com')).rejects.toBeInstanceOf(ServiceUnavailableError);
	});
});
