// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';

export const GatewayAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/gateway/stats',
		async prepare() {
			return {
				request: {path: '/admin/gateway/stats'},
				expected: {
					action: 'get_gateway_stats',
					targetType: 'gateway',
					targetId: '0',
					metadata: {node_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/gateway/memory-stats',
		async prepare() {
			return {
				request: {path: '/admin/gateway/memory-stats?limit=250'},
				expected: {
					action: 'list_guild_memory_stats',
					targetType: 'guild',
					targetId: '0',
					metadata: {limit: '250', result_count: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/gateway/voice-state-counts',
		async prepare() {
			return {
				request: {path: '/admin/gateway/voice-state-counts'},
				expected: {
					action: 'get_voice_state_counts',
					targetType: 'gateway',
					targetId: '0',
					metadata: {total_voice_states: '0', region_count: '0', server_count: '0'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/gateway/reloads',
		async prepare({harness, admin}) {
			const guild = await createGuild(harness, admin.token, 'Reload coverage guild');
			return {
				request: {path: '/admin/gateway/reloads', body: {guild_ids: [guild.id]}},
				expected: {
					action: 'reload_guilds',
					targetType: 'guild',
					targetId: '0',
					metadata: {guild_count: '1', reloaded: '0'},
				},
			};
		},
	},
];
