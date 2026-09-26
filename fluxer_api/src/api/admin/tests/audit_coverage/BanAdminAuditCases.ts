// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {updateAvatar} from '@app/api/user/tests/UserTestUtils';
import {expect} from 'vitest';

const FILE_SHA = 'a1'.repeat(32);
const SECOND_FILE_SHA = 'b2'.repeat(32);
const AVATAR_HASH = '0123abcd';

async function addBlocklistEntry(
	{harness, admin}: AdminAuditCoverageContext,
	listType: string,
	body: Record<string, unknown>,
): Promise<void> {
	await createBuilder(harness, admin.token)
		.post(`/admin/blocklists/${listType}/entries`)
		.body(body)
		.expect(204)
		.execute();
}

export const BanAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/blocklists',
		async prepare() {
			return {
				request: {path: '/admin/blocklists'},
				expected: {
					action: 'list_blocklists',
					targetType: 'blocklist',
					targetId: '0',
					metadata: {result_count: '9'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/blocklists/:list_type/entries',
		name: 'file-sha',
		async prepare(context) {
			await addBlocklistEntry(context, 'file-sha', {sha256_hex: FILE_SHA});
			await addBlocklistEntry(context, 'file-sha', {sha256_hex: SECOND_FILE_SHA});
			return {
				request: {path: '/admin/blocklists/file-sha/entries?limit=1'},
				expected: {
					action: 'list_blocklist_entries',
					targetType: 'file_sha',
					targetId: '0',
					metadata: {
						list_type: 'file-sha',
						limit: '1',
						result_count: '1',
						has_more: 'true',
					},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/blocklists/:list_type/entries',
		name: 'profile-substring after a cursor',
		async prepare(context) {
			await addBlocklistEntry(context, 'profile-substring', {scope: 'bio', substrings: ['alpha', 'beta']});
			return {
				request: {path: '/admin/blocklists/profile-substring/entries?scope=bio&after=alpha'},
				expected: {
					action: 'list_blocklist_entries',
					targetType: 'profile_substring',
					targetId: '0',
					metadata: {
						list_type: 'profile-substring',
						scope: 'bio',
						limit: '50',
						has_after: 'true',
						result_count: '1',
						has_more: 'false',
					},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/blocklists/:list_type/entries',
		async prepare() {
			return {
				request: {
					path: '/admin/blocklists/file-sha/entries',
					body: {sha256_hex: FILE_SHA},
					expectStatus: 204,
				},
				expected: {
					action: 'ban_file_sha',
					targetType: 'file_sha',
					targetId: '0',
					metadata: {sha256: FILE_SHA},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/blocklists/:list_type/entries',
		async prepare() {
			return {
				request: {
					path: '/admin/blocklists/file-sha/entries',
					body: {sha256_list: [FILE_SHA, SECOND_FILE_SHA]},
				},
				expected: {
					action: 'queue_bulk_job',
					targetType: 'bulk_job',
					targetId: expect.any(String),
					metadata: {task: 'ban_file_shas', entity_count: '2'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/blocklists/:list_type/entries',
		async prepare(context) {
			await addBlocklistEntry(context, 'avatar-hash', {hashes: [AVATAR_HASH]});
			return {
				request: {
					path: '/admin/blocklists/avatar-hash/entries',
					body: {hashes: [AVATAR_HASH]},
					expectStatus: 204,
				},
				expected: {
					action: 'unban_avatar_hash',
					targetType: 'avatar_hash',
					targetId: '0',
					metadata: {hash_short: AVATAR_HASH},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/blocklists/:list_type/entries/:entry_value',
		name: 'file-sha banned',
		async prepare(context) {
			await addBlocklistEntry(context, 'file-sha', {sha256_hex: FILE_SHA});
			return {
				request: {path: `/admin/blocklists/file-sha/entries/${FILE_SHA}`},
				expected: {
					action: 'check_blocklist_entry',
					targetType: 'file_sha',
					targetId: '0',
					metadata: {list_type: 'file-sha', banned: 'true'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/blocklists/:list_type/entries/:entry_value',
		name: 'profile-substring not banned',
		async prepare() {
			return {
				request: {path: '/admin/blocklists/profile-substring/entries/gamma?scope=username'},
				expected: {
					action: 'check_blocklist_entry',
					targetType: 'profile_substring',
					targetId: '0',
					metadata: {list_type: 'profile-substring', scope: 'username', banned: 'false'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/blocklists/:list_type/entries/:entry_value',
		async prepare(context) {
			await addBlocklistEntry(context, 'file-sha', {sha256_hex: FILE_SHA});
			return {
				request: {
					path: `/admin/blocklists/file-sha/entries/${FILE_SHA}`,
					body: {severity: 3},
					expectStatus: 204,
				},
				expected: {
					action: 'ban_file_sha',
					targetType: 'file_sha',
					targetId: '0',
					metadata: {sha256: FILE_SHA},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/blocklists/:list_type/entries/:entry_value',
		async prepare(context) {
			await addBlocklistEntry(context, 'file-sha', {sha256_hex: FILE_SHA});
			return {
				request: {
					path: `/admin/blocklists/file-sha/entries/${FILE_SHA}`,
					expectStatus: 204,
				},
				expected: {
					action: 'unban_file_sha',
					targetType: 'file_sha',
					targetId: '0',
					metadata: {sha256: FILE_SHA},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/users/:user_id/avatar-block',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const updated = await updateAvatar(harness, target.token, getPngDataUrl());
			const hashShort = (updated.avatar ?? '').toLowerCase().replace(/^a_/, '');
			return {
				request: {
					path: `/admin/users/${target.userId}/avatar-block`,
					body: {reason: 'spam'},
				},
				expected: {
					action: 'ban_avatar_hash',
					targetType: 'avatar_hash',
					targetId: '0',
					metadata: {hash_short: hashShort, reason: 'spam'},
				},
			};
		},
	},
];
