// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {getConfig} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {signHarvestDownloadToken} from '../services/HarvestDownloadToken';
import {fetchHarvestDownload, markHarvestCompleted, requestHarvest} from './HarvestTestUtils';

const ZIP_BYTES = new TextEncoder().encode('PK pretend zip');

async function withStreamingHarvestDownloads(callback: () => Promise<void>): Promise<void> {
	const config = getConfig();
	const original = config.presignedHarvestDownloadsEnabled;
	config.presignedHarvestDownloadsEnabled = false;
	try {
		await callback();
	} finally {
		config.presignedHarvestDownloadsEnabled = original;
	}
}

async function completedHarvest(harness: ApiTestHarness) {
	const account = await createTestAccount(harness);
	const {harvest_id} = await requestHarvest(harness, account.token);
	await markHarvestCompleted(account.userId, harvest_id, new Date(Date.now() + 6 * 24 * 60 * 60 * 1000));
	const storageKey = `test/${harvest_id}.zip`;
	await harness.storageService.uploadObject({
		bucket: getConfig().s3.buckets.harvests,
		key: storageKey,
		body: ZIP_BYTES,
		contentType: 'application/zip',
	});
	return {account, harvestId: harvest_id, storageKey};
}

function downloadPath(url: string): string {
	return url.slice(url.indexOf('/harvest-downloads'));
}

async function get(harness: ApiTestHarness, path: string): Promise<Response> {
	return harness.app.request(path, {headers: {'x-forwarded-for': '203.0.113.7'}});
}

describe('Harvest download streaming', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	test('defaults to presigned URLs and leaves the streaming route closed', async () => {
		const {account, harvestId, storageKey} = await completedHarvest(harness);
		expect(getConfig().presignedHarvestDownloadsEnabled).toBe(true);

		const download = await fetchHarvestDownload(harness, account.token, harvestId);
		expect(download.download_url).toBe('https://presigned.url/test');

		const token = signHarvestDownloadToken(
			{userId: account.userId, harvestId, storageKey, expiresAt: Date.now() + 60_000},
			getConfig().auth.connectionInitiationSecret,
		);
		const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
		expect(response.status).toBe(404);
	});

	test('serves the archive through the API when presigned downloads are disabled', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const download = await fetchHarvestDownload(harness, account.token, harvestId);

			expect(download.download_url).not.toContain('presigned.url');
			expect(download.download_url).toContain(`/harvest-downloads/${harvestId}`);
			expect(download.download_url).toContain('token=');

			const response = await get(harness, downloadPath(download.download_url));
			expect(response.status).toBe(200);
			expect(response.headers.get('content-type')).toBe('application/zip');
			expect(response.headers.get('content-disposition')).toContain('attachment');
			expect(response.headers.get('cache-control')).toBe('private, no-store');
			expect(new Uint8Array(await response.arrayBuffer())).toEqual(ZIP_BYTES);
		});
	});

	test('rejects a tampered token', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const download = await fetchHarvestDownload(harness, account.token, harvestId);
			const path = downloadPath(download.download_url);
			const response = await get(harness, `${path.slice(0, -2)}xy`);
			expect(response.status).toBe(404);
		});
	});

	test('rejects a token signed with the wrong secret', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId, storageKey} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{userId: account.userId, harvestId, storageKey, expiresAt: Date.now() + 60_000},
				'not-the-configured-secret',
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(404);
		});
	});

	test('rejects an expired token even while the harvest is still valid', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId, storageKey} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{userId: account.userId, harvestId, storageKey, expiresAt: Date.now() - 1},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(404);
		});
	});

	test('rejects a token replayed against a different harvest id', async () => {
		await withStreamingHarvestDownloads(async () => {
			const first = await completedHarvest(harness);
			const second = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{
					userId: first.account.userId,
					harvestId: first.harvestId,
					storageKey: first.storageKey,
					expiresAt: Date.now() + 60_000,
				},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${second.harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(404);
		});
	});

	test('rejects a token whose storage key no longer matches the harvest', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{userId: account.userId, harvestId, storageKey: 'test/some-other-key.zip', expiresAt: Date.now() + 60_000},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(404);
		});
	});

	test('rejects a token minted for a different user', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId, storageKey} = await completedHarvest(harness);
			const other = await createTestAccount(harness);
			const token = signHarvestDownloadToken(
				{userId: other.userId, harvestId, storageKey, expiresAt: Date.now() + 60_000},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(404);
		});
	});

	test('requires a token', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId} = await completedHarvest(harness);
			const response = await get(harness, `/harvest-downloads/${harvestId}`);
			expect(response.status).toBe(404);
		});
	});
});
