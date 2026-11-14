// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminArchiveRepository} from '@app/api/admin/repositories/AdminArchiveRepository';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {createTestGuild} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ZIP_BYTES = new TextEncoder().encode('PK pretend guild archive');

describe('admin archive downloads', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	const presigned = getConfig().presignedHarvestDownloadsEnabled;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	afterEach(() => {
		getConfig().presignedHarvestDownloadsEnabled = presigned;
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function completedGuildArchive(): Promise<string> {
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const {archive_id: archiveId} = await createBuilder<{archive_id: string}>(harness, admin.token)
			.post(`/admin/guilds/${guild.id}/archives`)
			.body({})
			.execute();
		const repository = new AdminArchiveRepository();
		const queued = await repository.findBySubjectAndArchiveId('guild', BigInt(guild.id), BigInt(archiveId));
		const storageKey = `test/${archiveId}.zip`;
		await harness.storageService.uploadObject({
			bucket: getConfig().s3.buckets.harvests,
			key: storageKey,
			body: ZIP_BYTES,
			contentType: 'application/zip',
		});
		await repository.markAsCompleted(
			await repository.markAsStarted(queued!),
			storageKey,
			BigInt(ZIP_BYTES.length),
			new Date(Date.now() + 6 * 24 * 3600 * 1000),
		);
		return `/admin/archives/guild/${guild.id}/${archiveId}/download`;
	}

	async function fetchDownloadUrl(path: string): Promise<string> {
		return (await createBuilder<{downloadUrl: string}>(harness, admin.token).get(path).execute()).downloadUrl;
	}

	it('refuses a changed token', async () => {
		getConfig().presignedHarvestDownloadsEnabled = false;
		const url = await fetchDownloadUrl(await completedGuildArchive());
		const tampered = url.slice(url.indexOf('/signed-downloads/')).replace(/token=([^&]+)/, 'token=$1x');
		const response = await harness.app.request(tampered, {headers: {'x-forwarded-for': '203.0.113.9'}});
		expect(response.status).toBe(HTTP_STATUS.NOT_FOUND);
	});
});
