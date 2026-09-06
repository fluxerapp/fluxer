// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {createTestGuild} from '../../emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface ArchiveResponse {
	archive_id: string;
	subject_id: string;
	subject_type: string;
	requested_by: string;
}

interface ListArchivesResponse {
	archives: Array<ArchiveResponse>;
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

async function listArchivesByRequester(
	harness: ApiTestHarness,
	adminToken: string,
	requestedBy: string,
): Promise<ListArchivesResponse> {
	return await createBuilder<ListArchivesResponse>(harness, `${adminToken}`)
		.get(`/admin/archives?subject_type=guild&requested_by=${requestedBy}&include_expired=false&limit=50`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Admin archives list', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('lists archives requested by the admin', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const archive = await triggerGuildArchive(harness, updatedAdmin.token, guild.id);
		const result = await listArchivesByRequester(harness, updatedAdmin.token, updatedAdmin.userId);
		expect(result.archives.some((entry) => entry.archive_id === archive.archive_id)).toBe(true);
		expect(result.archives.some((entry) => entry.subject_id === guild.id)).toBe(true);
	});
	test('requested_by filter isolates archive results', async () => {
		const adminOne = await createTestAccount(harness);
		const adminTwo = await createTestAccount(harness);
		const updatedAdminOne = await setAdminArchiveAcls(harness, adminOne);
		const updatedAdminTwo = await setAdminArchiveAcls(harness, adminTwo);
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const archiveOne = await triggerGuildArchive(harness, updatedAdminOne.token, guild.id);
		const archiveTwo = await triggerGuildArchive(harness, updatedAdminTwo.token, guild.id);
		const resultOne = await listArchivesByRequester(harness, updatedAdminOne.token, updatedAdminOne.userId);
		const resultTwo = await listArchivesByRequester(harness, updatedAdminTwo.token, updatedAdminTwo.userId);
		expect(resultOne.archives.some((entry) => entry.archive_id === archiveOne.archive_id)).toBe(true);
		expect(resultOne.archives.some((entry) => entry.archive_id === archiveTwo.archive_id)).toBe(false);
		expect(resultTwo.archives.some((entry) => entry.archive_id === archiveTwo.archive_id)).toBe(true);
		expect(resultTwo.archives.some((entry) => entry.archive_id === archiveOne.archive_id)).toBe(false);
	});
	test('reads one archive by subject type, subject id and archive id', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		const owner = await createTestAccount(harness);
		const guild = await createTestGuild(harness, owner.token);
		const archive = await triggerGuildArchive(harness, updatedAdmin.token, guild.id);
		const result = await createBuilder<{archive: ArchiveResponse | null}>(harness, `${updatedAdmin.token}`)
			.get(`/admin/archives/guild/${guild.id}/${archive.archive_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.archive?.archive_id).toBe(archive.archive_id);
	});
	test('creates a user archive under the user resource', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setUserACLs(harness, admin, ['admin:authenticate', 'archive:trigger:user']);
		const subject = await createTestAccount(harness);
		const archive = await createBuilder<ArchiveResponse>(harness, `${updatedAdmin.token}`)
			.post(`/admin/users/${subject.userId}/archives`)
			.body({include_attachments: true})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(archive.subject_type).toBe('user');
		expect(archive.subject_id).toBe(subject.userId);
	});
	test('a guild trigger ACL does not read user archives', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		await createBuilder(harness, `${updatedAdmin.token}`)
			.get('/admin/archives/user/1/2')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('rejects a subject id supplied without a concrete subject type', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setUserACLs(harness, admin, ['admin:authenticate', 'archive:view_all']);
		const response = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
			}>;
		}>(harness, `${updatedAdmin.token}`)
			.get('/admin/archives?subject_id=123')
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.execute();
		expect(response.errors[0]?.path).toBe('subject_type');
	});
	test('accepts a subject id paired with a concrete subject type', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setUserACLs(harness, admin, ['admin:authenticate', 'archive:view_all']);
		const result = await createBuilder<ListArchivesResponse>(harness, `${updatedAdmin.token}`)
			.get('/admin/archives?subject_type=user&subject_id=123')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Array.isArray(result.archives)).toBe(true);
	});
});
