// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {MediaProxyNsfwMode} from '@app/api/infrastructure/IMediaService';
import {IUnfurlerService, type UnfurlOptions, type UnfurlResult} from '@app/api/infrastructure/IUnfurlerService';
import {setInjectedUnfurlerService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

class RecordingUnfurlerService extends IUnfurlerService {
	readonly calls: Array<{url: string; nsfwMode?: MediaProxyNsfwMode; options: UnfurlOptions}> = [];

	constructor(private readonly result: UnfurlResult) {
		super();
	}

	override async unfurlWithCachePolicy(
		url: string,
		nsfwMode?: MediaProxyNsfwMode,
		options: UnfurlOptions = {},
	): Promise<UnfurlResult> {
		this.calls.push({url, nsfwMode, options});
		return this.result;
	}
}

describe('UnfurlController', () => {
	let harness: ApiTestHarness;
	let account: TestAccount;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		account = await createTestAccount(harness);
	});

	afterEach(async () => {
		setInjectedUnfurlerService(undefined);
		await harness.shutdown();
	});

	it('returns uncached unfurler embeds for a single URL', async () => {
		const embed: MessageEmbedResponse = {
			type: 'rich',
			url: 'https://example.com/article',
			title: 'Example',
			thumbnail: {
				url: 'https://example.com/image.png',
				proxy_url: 'https://media.example.com/image.png',
				width: 640,
				height: 360,
				flags: 0,
			},
		};
		const unfurler = new RecordingUnfurlerService({embeds: [embed], cacheTtlSeconds: 120});
		setInjectedUnfurlerService(unfurler);

		const response = await createBuilder<Array<MessageEmbedResponse>>(harness, account.token)
			.post('/unfurl')
			.body({url: 'https://example.com/article'})
			.execute();

		expect(response).toEqual([embed]);
		expect(unfurler.calls).toEqual([
			{
				url: 'https://example.com/article',
				nsfwMode: 'block',
				options: {bypassCache: true},
			},
		]);
	});

	it('requires authentication', async () => {
		await createBuilderWithoutAuth(harness).post('/unfurl').body({url: 'https://example.com'}).expect(401).execute();
	});
});
