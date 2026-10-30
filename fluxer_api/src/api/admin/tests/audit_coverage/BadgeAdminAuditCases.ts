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
		method: 'PUT',
		route: '/admin/badges/order',
		async prepare({harness, admin}) {
			const first = await createTestCustomBadge(harness, admin.token, 'user');
			const second = await createTestCustomBadge(harness, admin.token, 'guild');
			return {
				request: {path: '/admin/badges/order', body: {badge_ids: [second.id, first.id]}},
				expected: {
					action: 'reorder_badges',
					targetType: 'badge',
					targetId: '0',
					metadata: {badge_ids: `${second.id},${first.id}`},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/badges/built-in/:kind',
		async prepare() {
			return {
				request: {path: '/admin/badges/built-in/staff', body: {image: getPngDataUrl()}},
				expected: {
					action: 'replace_built_in_badge_image',
					targetType: 'badge',
					targetId: '0',
					metadata: {kind: 'staff'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/badges/built-in/:kind',
		async prepare() {
			return {
				request: {path: '/admin/badges/built-in/guild_verified'},
				expected: {
					action: 'reset_built_in_badge_image',
					targetType: 'badge',
					targetId: '0',
					metadata: {kind: 'guild_verified'},
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
