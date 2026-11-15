// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {MediaProxyNsfwMode} from '@app/api/infrastructure/IMediaService';
import type {UnfurlOptions, UnfurlResult} from '@app/api/infrastructure/IUnfurlerService';
import {MockStorageService} from '@app/api/test/mocks/MockStorageService';
import {NoopUnfurlerService} from '@app/api/test/mocks/NoopUnfurlerService';
import {TestMediaService} from '@app/api/test/TestMediaService';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {describe, expect, it} from 'vitest';

class UnreachableHostMediaService extends TestMediaService {
	override async getMetadata() {
		return null;
	}
}

class UnavailableUnfurlerService extends NoopUnfurlerService {
	override async unfurlWithCachePolicy(
		_url: string,
		_nsfwMode?: MediaProxyNsfwMode,
		_options?: UnfurlOptions,
	): Promise<UnfurlResult> {
		throw new ServiceUnavailableError({message: '[nats-unfurl] shard_unavailable'});
	}
}

describe('EmbedService custom embeds', () => {
	it('keeps an embed image whose host is down instead of failing the message', async () => {
		const service = new EmbedService(
			new UnavailableUnfurlerService(),
			new UnreachableHostMediaService(new MockStorageService()),
			{} as IWorkerService<WorkerTaskName>,
		);
		const imageUrl = 'https://media.example.invalid/Items/1/Images/Primary';

		const result = await service.getInitialEmbeds({
			content: null,
			customEmbeds: [{title: 'Now playing', image: {url: imageUrl}}],
			nsfwMode: 'block',
		});

		expect(result.embeds).toHaveLength(1);
		expect(result.embeds?.[0]?.title).toBe('Now playing');
		expect(result.embeds?.[0]?.image?.url).toBe(imageUrl);
	});
});
