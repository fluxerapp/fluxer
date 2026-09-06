// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {createTestGuild} from '../../emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import type {AdminArchive} from '../models/AdminArchiveModel';
import {AdminArchiveRepository} from '../repositories/AdminArchiveRepository';

interface ArchiveResponse {
	archive_id: string;
	subject_id: string;
	subject_type: string;
	requested_by: string;
	completed_at: string | null;
	failed_at: string | null;
	error_message: string | null;
}

async function setAdminArchiveAcls(harness: ApiTestHarness, admin: TestAccount): Promise<TestAccount> {
	return await setUserACLs(harness, admin, ['admin:authenticate', 'archive:trigger:guild']);
}

async function triggerGuildArchive(
	harness: ApiTestHarness,
	adminToken: string,
	guildId: string,
): Promise<ArchiveResponse> {
	return await createBuilder<ArchiveResponse>(harness, `${adminToken}`)
		.post(`/admin/guilds/${guildId}/archives`)
		.body({})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function loadArchive(guildId: string, archiveId: string): Promise<AdminArchive> {
	const archive = await new AdminArchiveRepository().findBySubjectAndArchiveId(
		'guild',
		BigInt(guildId),
		BigInt(archiveId),
	);
	if (!archive) {
		throw new Error(`Archive ${archiveId} not found`);
	}
	return archive;
}

describe('Admin archive retry clears failure', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('a retry clears the previous failure and a completed retry reports no failure', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const created = await triggerGuildArchive(harness, updatedAdmin.token, guild.id);
		const repository = new AdminArchiveRepository();
		await repository.markAsFailed(await loadArchive(guild.id, created.archive_id), 'archive exploded');
		const failed = await loadArchive(guild.id, created.archive_id);
		expect(failed.failedAt).not.toBeNull();
		expect(failed.errorMessage).toBe('archive exploded');
		await repository.markAsStarted(failed);
		const retrying = await loadArchive(guild.id, created.archive_id);
		expect(retrying.failedAt).toBeNull();
		expect(retrying.errorMessage).toBeNull();
		const validTime = new Date(Date.now() + 6 * 24 * 60 * 60 * 1000);
		await repository.markAsCompleted(retrying, `test/${created.archive_id}.zip`, 1024n, validTime);
		const result = await createBuilder<{archive: ArchiveResponse | null}>(harness, `${updatedAdmin.token}`)
			.get(`/admin/archives/guild/${guild.id}/${created.archive_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.archive?.failed_at).toBeNull();
		expect(result.archive?.error_message).toBeNull();
		expect(result.archive?.completed_at).not.toBeNull();
		const download = await createBuilder<{downloadUrl: string; expiresAt: string}>(harness, `${updatedAdmin.token}`)
			.get(`/admin/archives/guild/${guild.id}/${created.archive_id}/download`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(download.downloadUrl).not.toBe('');
	});
	test('download reports the failure rather than unreadiness when the latest attempt failed', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const created = await triggerGuildArchive(harness, updatedAdmin.token, guild.id);
		const repository = new AdminArchiveRepository();
		await repository.markAsFailed(await loadArchive(guild.id, created.archive_id), 'archive exploded');
		await createBuilder(harness, `${updatedAdmin.token}`)
			.get(`/admin/archives/guild/${guild.id}/${created.archive_id}/download`)
			.expect(HTTP_STATUS.BAD_REQUEST, 'HARVEST_FAILED')
			.execute();
	});
});
