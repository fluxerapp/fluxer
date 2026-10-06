// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminArchiveRepository} from '@app/api/admin/repositories/AdminArchiveRepository';
import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestGuild} from '@app/api/emoji/tests/EmojiTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {expect} from 'vitest';

const MISSING_ARCHIVE_ID = '800000000000000201';

async function createGuildArchive(
	harness: ApiTestHarness,
	admin: TestAccount,
): Promise<{guildId: string; archiveId: string}> {
	const owner = await createTestAccount(harness);
	const guild = await createTestGuild(harness, owner.token);
	const archive = await createBuilder<{archive_id: string}>(harness, admin.token)
		.post(`/admin/guilds/${guild.id}/archives`)
		.body({})
		.expect(HTTP_STATUS.OK)
		.execute();
	return {guildId: guild.id, archiveId: archive.archive_id};
}

export const ArchiveAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/users/:user_id/archives',
		async prepare({harness}) {
			const subject = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${subject.userId}/archives`, body: {include_attachments: true}},
				expected: {
					action: 'trigger_user_archive',
					targetType: 'user',
					targetId: subject.userId,
					metadata: {archive_id: expect.any(String), include_attachments: 'true'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/guilds/:guild_id/archives',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const guild = await createTestGuild(harness, owner.token);
			return {
				request: {path: `/admin/guilds/${guild.id}/archives`, body: {}},
				expected: {
					action: 'trigger_guild_archive',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {archive_id: expect.any(String), include_attachments: 'false'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/archives',
		name: 'by subject',
		async prepare({harness, admin}) {
			const {guildId} = await createGuildArchive(harness, admin);
			return {
				request: {path: `/admin/archives?subject_type=guild&subject_id=${guildId}&limit=10`},
				expected: {
					action: 'list_archives',
					targetType: 'archive',
					targetId: '0',
					metadata: {
						subject_type: 'guild',
						subject_guild_id: guildId,
						limit: '10',
						include_expired: 'false',
						result_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/archives',
		name: 'by requester',
		async prepare({harness, admin}) {
			await createGuildArchive(harness, admin);
			return {
				request: {path: `/admin/archives?requested_by=${admin.userId}&include_expired=true`},
				expected: {
					action: 'list_archives',
					targetType: 'archive',
					targetId: '0',
					metadata: {
						subject_type: 'all',
						requested_by_user_id: admin.userId,
						limit: '50',
						include_expired: 'true',
						result_count: '1',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/archives/:subject_type/:subject_id/:archive_id',
		name: 'found',
		async prepare({harness, admin}) {
			const {guildId, archiveId} = await createGuildArchive(harness, admin);
			return {
				request: {path: `/admin/archives/guild/${guildId}/${archiveId}`},
				expected: {
					action: 'get_archive',
					targetType: 'guild',
					targetId: guildId,
					metadata: {archive_id: archiveId, found: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/archives/:subject_type/:subject_id/:archive_id',
		name: 'missing',
		async prepare({harness}) {
			const subject = await createTestAccount(harness);
			return {
				request: {path: `/admin/archives/user/${subject.userId}/${MISSING_ARCHIVE_ID}`},
				expected: {
					action: 'get_archive',
					targetType: 'user',
					targetId: subject.userId,
					metadata: {archive_id: MISSING_ARCHIVE_ID, found: 'false'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/archives/:subject_type/:subject_id/:archive_id/download',
		async prepare({harness, admin}) {
			const {guildId, archiveId} = await createGuildArchive(harness, admin);
			const repository = new AdminArchiveRepository();
			const queued = await repository.findBySubjectAndArchiveId('guild', BigInt(guildId), BigInt(archiveId));
			if (!queued) {
				throw new Error(`Archive ${archiveId} not found`);
			}
			const started = await repository.markAsStarted(queued);
			await repository.markAsCompleted(
				started,
				`test/${archiveId}.zip`,
				1024n,
				new Date(Date.now() + 6 * 24 * 60 * 60 * 1000),
			);
			return {
				request: {path: `/admin/archives/guild/${guildId}/${archiveId}/download`},
				expected: {
					action: 'get_archive_download_url',
					targetType: 'guild',
					targetId: guildId,
					metadata: {archive_id: archiveId},
				},
			};
		},
	},
];
