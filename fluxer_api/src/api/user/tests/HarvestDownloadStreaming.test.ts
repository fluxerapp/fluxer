// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {signHarvestDownloadToken} from '@app/api/user/services/HarvestDownloadToken';
import {fetchHarvestDownload, markHarvestCompleted, requestHarvest} from '@app/api/user/tests/HarvestTestUtils';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {MS_PER_DAY, MS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import {beforeEach, describe, expect, test} from 'vitest';

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
	await markHarvestCompleted(account.userId, harvest_id, new Date(Date.now() + 6 * MS_PER_DAY));
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
			{userId: account.userId, harvestId, storageKey, expiresAt: Date.now() + MS_PER_MINUTE},
			getConfig().auth.connectionInitiationSecret,
		);
		const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
		expect(response.status).toBe(HttpStatus.NOT_FOUND);
	});

	test('serves the archive through the API when presigned downloads are disabled', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const download = await fetchHarvestDownload(harness, account.token, harvestId);

			expect(download.download_url).not.toContain('presigned.url');
			expect(download.download_url).toContain(`/harvest-downloads/${harvestId}`);
			expect(download.download_url).toContain('token=');

			const response = await get(harness, downloadPath(download.download_url));
			expect(response.status).toBe(HttpStatus.OK);
			expect(response.headers.get('content-type')).toBe('application/zip');
			expect(response.headers.get('content-disposition')).toContain('attachment');
			expect(response.headers.get('cache-control')).toBe('private, no-store');
			expect(new Uint8Array(await response.arrayBuffer())).toEqual(ZIP_BYTES);
		});
	});

	test('rejects a token signed with the wrong secret', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId, storageKey} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{userId: account.userId, harvestId, storageKey, expiresAt: Date.now() + MS_PER_MINUTE},
				'not-the-configured-secret',
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
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
					expiresAt: Date.now() + MS_PER_MINUTE,
				},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${second.harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
		});
	});

	test('rejects a token whose storage key no longer matches the harvest', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{
					userId: account.userId,
					harvestId,
					storageKey: 'test/some-other-key.zip',
					expiresAt: Date.now() + MS_PER_MINUTE,
				},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
		});
	});

	test('rejects a token minted for a different user', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId, storageKey} = await completedHarvest(harness);
			const other = await createTestAccount(harness);
			const token = signHarvestDownloadToken(
				{userId: other.userId, harvestId, storageKey, expiresAt: Date.now() + MS_PER_MINUTE},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
		});
	});

	test('requires a token', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId} = await completedHarvest(harness);
			const response = await get(harness, `/harvest-downloads/${harvestId}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
			expect(await response.text()).toBe('Not Found');
		});
	});

	test('rejects a signed token with a malformed user id', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId, storageKey} = await completedHarvest(harness);
			const token = signHarvestDownloadToken(
				{userId: 'invalid-user', harvestId, storageKey, expiresAt: Date.now() + MS_PER_MINUTE},
				getConfig().auth.connectionInitiationSecret,
			);
			const response = await get(harness, `/harvest-downloads/${harvestId}?token=${encodeURIComponent(token)}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
			expect(await response.text()).toBe('Not Found');
		});
	});

	test.each([
		['an empty token', '?token='],
		['a token longer than a download URL', `?token=${'a'.repeat(2049)}`],
		['a token given twice', '?token=a&token=b'],
	])('answers not found for %s', async (_label, query) => {
		await withStreamingHarvestDownloads(async () => {
			const {harvestId} = await completedHarvest(harness);
			const response = await get(harness, `/harvest-downloads/${harvestId}${query}`);
			expect(response.status).toBe(HttpStatus.NOT_FOUND);
			expect(await response.text()).toBe('Not Found');
		});
	});

	test('serves the archive when the link has other query parameters', async () => {
		await withStreamingHarvestDownloads(async () => {
			const {account, harvestId} = await completedHarvest(harness);
			const download = await fetchHarvestDownload(harness, account.token, harvestId);
			const response = await get(harness, `${downloadPath(download.download_url)}&source=email`);
			expect(response.status).toBe(HttpStatus.OK);
			expect(new Uint8Array(await response.arrayBuffer())).toEqual(ZIP_BYTES);
		});
	});
});
