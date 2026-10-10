// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {AdminRateLimitConfigs} from '@app/api/rate_limit_configs/AdminRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {ListGuildThreadsResponse} from '@fluxer/schema/src/domains/admin/AdminThreadSchemas';
import {ChannelIdParam, GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function ThreadAdminController(app: HonoApp) {
	app.get(
		'/admin/guilds/:guild_id/threads',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.GUILD_LOOKUP),
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_admin_guild_threads',
			summary: 'List guild threads',
			description:
				'Lists every thread of a guild, active and archived, whether or not the channel threads experiment is active for it. Requires GUILD_LOOKUP permission.',
			responseSchema: ListGuildThreadsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			experiment: 'channel_threads',
		}),
		async (ctx) => {
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const threads = await ctx.get('threadService').lists.listGuildThreadsForAdmin(guildId);
			await recordAdminRead(ctx, {
				targetType: 'guild',
				targetId: guildId,
				action: AdminAuditReadActions.LIST_GUILD_THREADS,
				metadata: {result_count: threads.length},
			});
			return ctx.json({threads});
		},
	);
	app.delete(
		'/admin/channels/:channel_id',
		RateLimitMiddleware(AdminRateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE_ALL),
		Validator('param', ChannelIdParam),
		OpenAPI({
			operationId: 'delete_admin_thread_channel',
			summary: 'Delete a thread',
			description:
				'Deletes a thread channel with its messages and memberships. Only public and private threads can be deleted here. Requires MESSAGE_DELETE_ALL permission.',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			experiment: 'channel_threads',
		}),
		async (ctx) => {
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const thread = await ctx.get('channelRepository').channelData.findUnique(channelId);
			if (!thread) throw new UnknownChannelError();
			if (!thread.isThread()) throw new InvalidChannelTypeError();
			const adminUserId = ctx.get('adminUserId');
			await ctx.get('threadService').deletion.deleteThread({
				thread,
				actorId: adminUserId,
				auditLogReason: ctx.get('auditLogReason'),
				recordGuildAudit: false,
			});
			await recordAdminWrite(ctx, {
				targetType: 'channel',
				targetId: channelId,
				action: 'delete_thread',
				metadata: {guild_id: thread.guildId?.toString(), parent_id: thread.parentId?.toString(), type: thread.type},
			});
			return ctx.body(null, 204);
		},
	);
}
