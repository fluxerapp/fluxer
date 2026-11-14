// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createGuild} from '@app/api/message/tests/MessageTestUtils';

const UNKNOWN_ASSET_ID = '900000000000000002';

export const AssetAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'DELETE',
		route: '/admin/guilds/:guild_id/assets',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Audit Asset Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/assets`, body: {ids: [UNKNOWN_ASSET_ID]}},
				expected: {
					action: 'purge_asset',
					targetType: 'asset',
					targetId: UNKNOWN_ASSET_ID,
					metadata: {asset_type: 'unknown'},
				},
			};
		},
	},
];
