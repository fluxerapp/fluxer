// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';

export const ThreadAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/guilds/:guild_id/threads',
		async prepare({harness, admin}) {
			resetChannelThreadsConfig();
			const guild = await createGuild(harness, admin.token, 'Audit Thread List Guild');
			return {
				request: {path: `/admin/guilds/${guild.id}/threads`},
				expected: {
					action: 'list_guild_threads',
					targetType: 'guild',
					targetId: guild.id,
					metadata: {result_count: '0'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/channels/:channel_id',
		async prepare({harness, admin}) {
			resetChannelThreadsConfig();
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
			const guild = await createGuild(harness, admin.token, 'Audit Thread Delete Guild');
			const channel = await createChannel(harness, admin.token, guild.id, 'general');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, admin.token)
				.post(`/channels/${channel.id}/threads`)
				.body({name: 'doomed', type: ChannelTypes.PUBLIC_THREAD})
				.expect(201)
				.execute();
			return {
				request: {path: `/admin/channels/${thread.id}`, expectStatus: 204},
				expected: {
					action: 'delete_thread',
					targetType: 'channel',
					targetId: thread.id,
					metadata: {guild_id: guild.id, parent_id: channel.id, type: String(ChannelTypes.PUBLIC_THREAD)},
				},
			};
		},
	},
];
