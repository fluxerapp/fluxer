// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAdminWithAcls} from '@app/api/admin/tests/AdminTestUtils';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {markChannelAsIndexed, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getSnowflakeService, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import refreshSearchIndex from '@app/api/worker/tasks/RefreshSearchIndex';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface RefreshResponse {
	job_id: string;
}

interface StatusResponse {
	status: string;
	index_type?: string;
}

describe('rebuilding every search index', () => {
	let harness: ApiTestHarness;
	let worker: SyncTaskWorkerService;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	beforeEach(async () => {
		await harness.reset();
		setWorkerDependencies(await initializeWorkerDependencies(getSnowflakeService()));
		worker = new SyncTaskWorkerService({refreshSearchIndex}, {deferred: true});
		setInjectedWorkerService(worker);
		resetApiServicesForTesting();
		admin = await createAdminWithAcls(harness, [AdminACLs.GUILD_LOOKUP]);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
		resetApiServicesForTesting();
	});

	afterAll(async () => {
		clearWorkerDependencies();
		await harness.shutdown();
	});

	function queueAll(): Promise<RefreshResponse> {
		return createBuilder<RefreshResponse>(harness, admin.token)
			.post('/admin/search/indexes/all/refreshes')
			.body({})
			.expect(200)
			.execute();
	}

	it('queues one rebuild at a time and brings back wiped messages', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Reindex Guild');
		const channelId = guild.system_channel_id!;
		const content = `reindex-everything-${Date.now()}`;
		const message = await sendMessage(harness, account.token, channelId, content);
		await markChannelAsIndexed(harness, channelId);
		const searchService = getMessageSearchService()!;
		await searchService.deleteAllDocuments();
		const search = () => searchService.searchMessages(content, {channelIds: [channelId]}, {hitsPerPage: 25, page: 1});
		expect((await search()).hits).toEqual([]);

		const first = await queueAll();
		const second = await queueAll();
		expect(second.job_id).toBe(first.job_id);
		expect(worker.pending.filter((job) => job.taskType === 'refreshSearchIndex')).toHaveLength(1);

		await worker.drain({maxAttempts: 1});
		const status = await createBuilder<StatusResponse>(harness, admin.token)
			.get(`/admin/search/index-refreshes/${first.job_id}`)
			.expect(200)
			.execute();
		expect(status).toMatchObject({status: 'completed', index_type: 'all'});
		expect((await search()).hits.map((hit) => hit.id)).toEqual([message.id]);

		const third = await queueAll();
		expect(third.job_id).not.toBe(first.job_id);
	});
});
