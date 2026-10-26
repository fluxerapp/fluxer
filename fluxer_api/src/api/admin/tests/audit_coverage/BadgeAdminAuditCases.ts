// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestCustomBadge} from '@app/api/admin/tests/AdminTestUtils';
import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {expect} from 'vitest';

export const BadgeAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/badges',
		async prepare({harness, admin}) {
			await createTestCustomBadge(harness, admin.token);
			return {
				request: {path: '/admin/badges'},
				expected: {
					action: 'list_badges',
					targetType: 'badge',
					targetId: '0',
					metadata: {result_count: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/badges',
		async prepare() {
			return {
				request: {path: '/admin/badges', body: {type: 'guild', name: 'Audit badge', image: getPngDataUrl()}},
				expected: {
					action: 'create_badge',
					targetType: 'badge',
					targetId: expect.any(String),
					metadata: {type: 'guild'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/badges/:badge_id',
		async prepare({harness, admin}) {
			const badge = await createTestCustomBadge(harness, admin.token);
			return {
				request: {
					path: `/admin/badges/${badge.id}`,
					body: {name: 'Renamed badge', description: null, href: null},
				},
				expected: {
					action: 'update_badge',
					targetType: 'badge',
					targetId: badge.id,
					metadata: {image_replaced: 'false'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/badges/:badge_id',
		async prepare({harness, admin}) {
			const badge = await createTestCustomBadge(harness, admin.token);
			return {
				request: {path: `/admin/badges/${badge.id}`, expectStatus: 204},
				expected: {
					action: 'delete_badge',
					targetType: 'badge',
					targetId: badge.id,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/badges',
		async prepare({harness, admin}) {
			const badge = await createTestCustomBadge(harness, admin.token);
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/badges`, body: {badge_ids: [badge.id]}},
				expected: {
					action: 'update_user_badges',
					targetType: 'user',
					targetId: target.userId,
					metadata: {badge_ids: badge.id},
				},
			};
		},
	},
];
