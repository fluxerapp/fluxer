// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestBadge, TEST_BADGE_ICON} from '@app/api/badge/tests/BadgeTestUtils';
import {BadgeTypes, BuiltinBadges} from '@fluxer/constants/src/BadgeConstants';
import {expect} from 'vitest';

export const BadgeAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/badges',
		async prepare({harness, admin}) {
			await createTestBadge(harness, admin.token);
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
				request: {
					path: '/admin/badges',
					body: {type: BadgeTypes.GUILD, name: 'Audit Badge', tooltip: 'Audit badge', icon: TEST_BADGE_ICON},
				},
				expected: {
					action: 'create_badge',
					targetType: 'badge',
					targetId: expect.any(String),
					metadata: {type: BadgeTypes.GUILD, name: 'Audit Badge'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/badges/:badge_id',
		async prepare({harness, admin}) {
			const badge = await createTestBadge(harness, admin.token);
			return {
				request: {path: `/admin/badges/${badge.id}`, body: {name: 'Renamed Badge', position: 3}},
				expected: {
					action: 'update_badge',
					targetType: 'badge',
					targetId: badge.id,
					metadata: {fields: 'name,position'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/badges/:badge_id',
		async prepare({harness, admin}) {
			const badge = await createTestBadge(harness, admin.token);
			return {
				request: {path: `/admin/badges/${badge.id}`},
				expected: {
					action: 'delete_badge',
					targetType: 'badge',
					targetId: badge.id,
					metadata: {type: badge.type, name: badge.name},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/badges/builtin/:badge',
		async prepare() {
			return {
				request: {path: `/admin/badges/builtin/${BuiltinBadges.PREMIUM}`, body: {icon: TEST_BADGE_ICON}},
				expected: {
					action: 'update_builtin_badge_icon',
					targetType: 'badge',
					targetId: '0',
					metadata: {badge: BuiltinBadges.PREMIUM, reset: 'false'},
				},
			};
		},
	},
];
