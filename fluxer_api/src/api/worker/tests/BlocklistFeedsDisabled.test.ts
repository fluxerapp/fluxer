// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	BANNED_FILE_SHAS_REFRESH_CHANNEL,
	BANNED_URLS_REFRESH_CHANNEL,
	ContentBlocklistCategory,
} from '@app/api/constants/ContentModeration';
import type {BannedFileShaRow} from '@app/api/database/types/AdminArchiveTypes';
import {fileShaCache} from '@app/api/middleware/FileShaCache';
import {getAdminRepository} from '@app/api/middleware/ServiceSingletons';
import {urlBlocklistCache} from '@app/api/middleware/UrlBlocklistCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {canonicalizeUrl} from '@app/api/utils/UrlNormalizer';
import syncFileShaBlocklists from '@app/api/worker/tasks/SyncFileShaBlocklists';
import syncUrlBlocklists from '@app/api/worker/tasks/SyncUrlBlocklists';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

const FEED_BUCKET = 'fluxer-geoip';
const FEED_KEY = 'blocklists/feed-urls.txt';
const FEED_SHA = 'a1'.repeat(32);
const ADMIN_BAZAAR_SHA = 'b2'.repeat(32);
const ADMIN_MANUAL_SHA = 'c3'.repeat(32);
const UNOWNED_NCMEC_SHA = 'd4'.repeat(32);
const ADMIN_USER_ID = 42n;

function fileShaRow(sha256Hex: string, category: string, addedBy: bigint | null): BannedFileShaRow {
	return {
		sha256_hex: sha256Hex,
		category,
		severity: 2,
		content_type: null,
		source_url: null,
		added_at: new Date(),
		added_by: addedBy,
		notes: null,
	};
}

async function seedFileShas(): Promise<void> {
	const adminRepository = getAdminRepository();
	await adminRepository.banFileSha(fileShaRow(FEED_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, null));
	await adminRepository.banFileSha(
		fileShaRow(ADMIN_BAZAAR_SHA, ContentBlocklistCategory.MALWARE_BAZAAR, ADMIN_USER_ID),
	);
	await adminRepository.banFileSha(fileShaRow(ADMIN_MANUAL_SHA, ContentBlocklistCategory.MANUAL, ADMIN_USER_ID));
	await adminRepository.banFileSha(fileShaRow(UNOWNED_NCMEC_SHA, ContentBlocklistCategory.NCMEC, null));
}

describe('blocklist feeds turned off', () => {
	let harness: ApiTestHarness;
	let previousFeedsEnabled: boolean;
	let fetchSpy: MockInstance<typeof fetch>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		previousFeedsEnabled = Config.blocklistFeeds.enabled;
		await harness.reset();
		harness.storageService.reset();
		setWorkerDependenciesForTest({
			adminRepository: getAdminRepository(),
			kvClient: harness.kvProvider,
			storageService: harness.storageService,
		});
		fetchSpy = vi.spyOn(globalThis, 'fetch');
	});

	afterEach(() => {
		Config.blocklistFeeds.enabled = previousFeedsEnabled;
		fetchSpy.mockRestore();
	});

	afterAll(async () => {
		clearWorkerDependencies();
		await harness?.shutdown();
	});

	function publishCalls(): Array<Array<string>> {
		return (harness.kvProvider as MockKVProvider).publishSpy.mock.calls;
	}

	it('removes MalwareBazaar feed rows and keeps every other file-SHA ban', async () => {
		await seedFileShas();
		Config.blocklistFeeds.enabled = false;

		await syncFileShaBlocklists({}, createTestWorkerHelpers({jobId: 4242n, ...{}}));

		const remaining = (await getAdminRepository().loadAllBannedFileShas()).map((row) => row.sha256_hex).sort();
		expect(remaining).toEqual([ADMIN_BAZAAR_SHA, ADMIN_MANUAL_SHA, UNOWNED_NCMEC_SHA]);
		expect(publishCalls()).toEqual([[BANNED_FILE_SHAS_REFRESH_CHANNEL, 'refresh']]);
		expect(fetchSpy).not.toHaveBeenCalled();
	});

	it('file-SHA cache skips feed rows only while feeds are off', async () => {
		await seedFileShas();

		Config.blocklistFeeds.enabled = false;
		await fileShaCache.refresh();
		expect(fileShaCache.isBanned(FEED_SHA)).toBe(false);
		expect(fileShaCache.isBanned(ADMIN_BAZAAR_SHA)).toBe(true);
		expect(fileShaCache.isBanned(ADMIN_MANUAL_SHA)).toBe(true);
		expect(fileShaCache.isBanned(UNOWNED_NCMEC_SHA)).toBe(true);

		Config.blocklistFeeds.enabled = true;
		await fileShaCache.refresh();
		expect(fileShaCache.isBanned(FEED_SHA)).toBe(true);
	});

	it('deletes the URL feed file and never fetches while feeds are off', async () => {
		await harness.storageService.uploadObject({
			bucket: FEED_BUCKET,
			key: FEED_KEY,
			body: Buffer.from('https://feed.example/x\n'),
		});
		Config.blocklistFeeds.enabled = false;

		await syncUrlBlocklists({}, createTestWorkerHelpers({jobId: 4242n, ...{}}));

		expect(harness.storageService.deleteObjectSpy).toHaveBeenCalledWith(FEED_BUCKET, FEED_KEY);
		expect(harness.storageService.hasObject(FEED_BUCKET, FEED_KEY)).toBe(false);
		expect(publishCalls()).toEqual([[BANNED_URLS_REFRESH_CHANNEL, 'refresh']]);
		expect(fetchSpy).not.toHaveBeenCalled();

		const quietLogger = new NoopLogger();
		const quietWarn = vi.spyOn(quietLogger, 'warn');
		vi.spyOn(harness.storageService, 'deleteObject').mockRejectedValueOnce(
			Object.assign(new Error('The specified bucket does not exist'), {name: 'NoSuchBucket'}),
		);
		await expect(
			syncUrlBlocklists({}, createTestWorkerHelpers({jobId: 4242n, ...{logger: quietLogger}})),
		).resolves.toBeUndefined();
		expect(quietWarn).not.toHaveBeenCalled();

		const loudLogger = new NoopLogger();
		const loudWarn = vi.spyOn(loudLogger, 'warn');
		harness.storageService.configure({shouldFailDelete: true});
		await expect(
			syncUrlBlocklists({}, createTestWorkerHelpers({jobId: 4242n, ...{logger: loudLogger}})),
		).resolves.toBeUndefined();
		expect(loudWarn).toHaveBeenCalledTimes(1);
	});

	it('URL cache ignores the feed file only while feeds are off', async () => {
		const feedUrl = canonicalizeUrl('https://feed.example/x');
		const adminUrl = canonicalizeUrl('https://admin.example/y');
		expect(feedUrl).not.toBeNull();
		expect(adminUrl).not.toBeNull();
		await harness.storageService.uploadObject({
			bucket: FEED_BUCKET,
			key: FEED_KEY,
			body: Buffer.from(`${feedUrl}\n`),
		});
		await getAdminRepository().banUrl({
			url_canonical: adminUrl!,
			category: ContentBlocklistCategory.MANUAL,
			severity: 2,
			source_url: null,
			added_at: new Date(),
			added_by: ADMIN_USER_ID,
			notes: null,
		});
		urlBlocklistCache.setStorageService(harness.storageService);

		Config.blocklistFeeds.enabled = false;
		await urlBlocklistCache.refresh();
		expect(urlBlocklistCache.isUrlBanned('https://feed.example/x')).toBe(false);
		expect(urlBlocklistCache.isUrlBanned('https://admin.example/y')).toBe(true);

		Config.blocklistFeeds.enabled = true;
		await urlBlocklistCache.refresh();
		expect(urlBlocklistCache.isUrlBanned('https://feed.example/x')).toBe(true);
	});
});
