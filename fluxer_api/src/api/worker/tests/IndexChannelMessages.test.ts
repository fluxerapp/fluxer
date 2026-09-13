// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {setInjectedSearchProvider} from '@app/api/SearchFactory';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import indexChannelMessages from '@app/api/worker/tasks/IndexChannelMessages';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, it, vi} from 'vitest';

const COMPLETION_KEY = 'bulk_reindex:7000:remaining';
const HELPERS = {logger: new NoopLogger()} as unknown as WorkerTaskHelpers;

function createHarness(): {kvClient: MockKVProvider; refreshIndex: ReturnType<typeof vi.fn>} {
	const kvClient = new MockKVProvider();
	const refreshIndex = vi.fn().mockResolvedValue(undefined);
	const messageSearchService = {
		bulkIndexMessages: async () => {},
		refreshIndex,
	} as unknown as IMessageSearchService;
	setInjectedSearchProvider({
		getMessageSearchService: () => messageSearchService,
	} as unknown as ISearchProvider);
	setWorkerDependenciesForTest({
		kvClient,
		channelRepository: {
			listMessages: async () => [],
			findUnique: async () => null,
		} as unknown as ChannelRepository,
		userRepository: {listUsers: async () => []} as unknown as UserRepository,
	});
	return {kvClient, refreshIndex};
}

describe('indexChannelMessages', () => {
	afterEach(() => {
		clearWorkerDependencies();
		setInjectedSearchProvider(undefined);
	});

	it('counts a redelivered channel once towards bulk reindex completion', async () => {
		const {kvClient, refreshIndex} = createHarness();
		const payload = {channelId: '5001', completionKey: COMPLETION_KEY, channelCount: 2};

		await indexChannelMessages(payload, HELPERS);
		await indexChannelMessages(payload, HELPERS);

		expect(refreshIndex).not.toHaveBeenCalled();

		await indexChannelMessages({...payload, channelId: '5002'}, HELPERS);

		expect(refreshIndex).toHaveBeenCalledTimes(1);
		expect(await kvClient.exists(COMPLETION_KEY)).toBe(0);
	});

	it('expires the completion key when the reindex never finishes', async () => {
		const {kvClient} = createHarness();

		await indexChannelMessages({channelId: '5001', completionKey: COMPLETION_KEY, channelCount: 2}, HELPERS);

		expect(await kvClient.ttl(COMPLETION_KEY)).toBeGreaterThan(0);
	});
});
