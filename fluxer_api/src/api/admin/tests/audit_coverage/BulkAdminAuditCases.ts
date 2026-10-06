// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestGuild} from '@app/api/emoji/tests/EmojiTestUtils';
import {expect} from 'vitest';

export const BulkAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'POST',
		route: '/admin/bulk-jobs',
		name: 'update_user_flags',
		async prepare({harness}) {
			const first = await createTestAccount(harness);
			const second = await createTestAccount(harness);
			return {
				request: {
					path: '/admin/bulk-jobs',
					body: {task: 'update_user_flags', user_ids: [first.userId, second.userId], add_flags: [], remove_flags: []},
				},
				expected: {
					action: 'queue_bulk_job',
					targetType: 'bulk_job',
					targetId: expect.any(String),
					metadata: {task: 'update_user_flags', entity_count: '2'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/bulk-jobs',
		name: 'update_guild_features',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const guild = await createTestGuild(harness, owner.token);
			return {
				request: {
					path: '/admin/bulk-jobs',
					body: {task: 'update_guild_features', guild_ids: [guild.id], add_features: [], remove_features: []},
				},
				expected: {
					action: 'queue_bulk_job',
					targetType: 'bulk_job',
					targetId: expect.any(String),
					metadata: {task: 'update_guild_features', entity_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/bulk-jobs',
		name: 'add_guild_members',
		async prepare({harness}) {
			const owner = await createTestAccount(harness);
			const guild = await createTestGuild(harness, owner.token);
			const member = await createTestAccount(harness);
			return {
				request: {
					path: '/admin/bulk-jobs',
					body: {task: 'add_guild_members', guild_id: guild.id, user_ids: [member.userId]},
				},
				expected: {
					action: 'queue_bulk_job',
					targetType: 'bulk_job',
					targetId: expect.any(String),
					metadata: {task: 'add_guild_members', entity_count: '1', guild_id: guild.id},
				},
			};
		},
	},
];
