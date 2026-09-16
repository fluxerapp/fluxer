// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead} from '@app/api/admin/AdminAuditRecorder';
import {createAttachmentID, createChannelID, createMessageID, createReportID} from '@app/api/BrandedTypes';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {
	AdminChannelMessageListQuery,
	AdminMessageSearchQuery,
	AdminMessageSearchResponse,
	BrowseChannelResponse,
} from '@fluxer/schema/src/domains/admin/AdminMessageBrowseSchemas';
import {
	AdminMessageDetailQuery,
	AdminUserMessageDeleteQuery,
	AdminUserMessageShredRequest,
	DeleteAllUserMessagesResponse,
	MessageShredJobIdParam,
	MessageShredResponse,
	ReportAttachmentToNcmecRequest,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import {
	DeleteMessageResponse,
	LookupMessageResponse,
	MessageShredStatusResponse,
	NcmecAttachmentSubmitResultResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	ChannelIdMessageIdParam,
	ChannelIdParam,
	UserIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function MessageAdminController(app: HonoApp) {
	app.get(
		'/admin/messages',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('query', AdminMessageSearchQuery),
		OpenAPI({
			operationId: 'search_admin_messages',
			summary: 'Search messages',
			description:
				'Searches the messages of a channel by content, or resolves a single message by its ID or by one of its attachments. Passing message_id returns that message with the messages surrounding it; passing attachment_id together with filename returns the message carrying that attachment with its surrounding context. Requires MESSAGE_LOOKUP permission.',
			responseSchema: AdminMessageSearchResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const query = ctx.req.valid('query');
			if (query.message_id != null) {
				const messageId = query.message_id;
				const response = await adminService.messageService.lookupMessage({
					channel_id: query.channel_id,
					message_id: messageId,
					context_limit: query.context_limit,
				});
				await recordAdminRead(ctx, {
					targetType: 'message',
					targetId: messageId,
					action: AdminAuditReadActions.SEARCH_MESSAGES,
					metadata: {
						mode: 'message',
						channel_id: query.channel_id,
						context_limit: query.context_limit,
						found: response.messages.some((message) => message.id === messageId.toString()),
						result_count: response.messages.length,
					},
				});
				return ctx.json(response);
			}
			if (query.attachment_id != null) {
				if (query.filename == null) {
					throw InputValidationError.fromCode('filename', ValidationErrorCodes.INVALID_FORMAT);
				}
				const response = await adminService.messageService.lookupMessageByAttachment({
					channel_id: query.channel_id,
					attachment_id: query.attachment_id,
					filename: query.filename,
					context_limit: query.context_limit,
				});
				await recordAdminRead(ctx, {
					targetType: 'channel',
					targetId: query.channel_id,
					action: AdminAuditReadActions.SEARCH_MESSAGES,
					metadata: {
						mode: 'attachment',
						attachment_id: query.attachment_id,
						message_id: response.message_id,
						context_limit: query.context_limit,
						found: response.message_id !== null,
						result_count: response.messages.length,
					},
				});
				return ctx.json(response);
			}
			const response = await adminService.messageService.searchChannelMessages({
				channel_id: query.channel_id,
				query: query.q ?? '',
				limit: query.limit,
			});
			await recordAdminRead(ctx, {
				targetType: 'channel',
				targetId: query.channel_id,
				action: AdminAuditReadActions.SEARCH_MESSAGES,
				metadata: {
					mode: 'search',
					has_query: query.q === undefined ? undefined : true,
					limit: query.limit,
					result_count: response.messages.length,
					total: response.total,
				},
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/admin/messages/ncmec-reports',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.CSAM_SUBMIT_NCMEC),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		requireAdminACL(AdminACLs.USER_DELETE),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_USER),
		Validator('json', ReportAttachmentToNcmecRequest),
		OpenAPI({
			operationId: 'create_admin_ncmec_report',
			summary: 'Report an attachment to NCMEC',
			description:
				'Submits a specific image attachment to NCMEC, creates an audit log entry, silently disables the user, triggers one archive for the user, and schedules content deletion after the archive completes.',
			responseSchema: NcmecAttachmentSubmitResultResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const service = ctx.get('ncmecSubmissionService');
			const adminUserId = ctx.get('adminUserId');
			const body = ctx.req.valid('json');
			const result = await service.submitAttachmentToNcmec({
				channelId: createChannelID(body.channel_id),
				messageId: createMessageID(body.message_id),
				attachmentId: createAttachmentID(body.attachment_id),
				filename: body.filename,
				reporterFullName: body.reporter_full_name,
				adminUserId,
				sourceReportId: body.source_report_id ? createReportID(body.source_report_id) : null,
			});
			return ctx.json(result);
		},
	);
	app.get(
		'/admin/messages/shreds/:job_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('param', MessageShredJobIdParam),
		OpenAPI({
			operationId: 'get_admin_message_shred',
			summary: 'Get message shred job',
			description:
				'Returns the progress of a queued message shred job, including whether it is complete. Requires MESSAGE_SHRED permission.',
			responseSchema: MessageShredStatusResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {job_id} = ctx.req.valid('param');
			const response = await adminService.messageShredService.getMessageShredStatus(job_id.toString());
			await recordAdminRead(ctx, {
				targetType: 'message_shred',
				targetId: 0n,
				action: AdminAuditReadActions.GET_MESSAGE_SHRED_STATUS,
				metadata: {job_id, status: response.status},
			});
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/channels/:channel_id/messages',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('param', ChannelIdParam),
		Validator('query', AdminChannelMessageListQuery),
		OpenAPI({
			operationId: 'list_admin_channel_messages',
			summary: 'List channel messages',
			description:
				'Pages through the messages of a channel, newest first, with cursor-based pagination. Requires MESSAGE_LOOKUP permission.',
			responseSchema: BrowseChannelResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {channel_id} = ctx.req.valid('param');
			const {limit, before, after} = ctx.req.valid('query');
			const response = await adminService.messageService.browseChannel({
				channel_id,
				before,
				after,
				limit,
			});
			await recordAdminRead(ctx, {
				targetType: 'channel',
				targetId: channel_id,
				action: AdminAuditReadActions.LIST_CHANNEL_MESSAGES,
				metadata: {
					limit,
					before,
					after,
					result_count: response.messages.length,
					has_more: response.has_more,
				},
			});
			return ctx.json(response);
		},
	);
	app.get(
		'/admin/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('param', ChannelIdMessageIdParam),
		Validator('query', AdminMessageDetailQuery),
		OpenAPI({
			operationId: 'get_admin_message',
			summary: 'Get message',
			description:
				'Retrieves complete message details including content, attachments, edits, and metadata, together with the messages surrounding it. Requires MESSAGE_LOOKUP permission.',
			responseSchema: LookupMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {channel_id, message_id} = ctx.req.valid('param');
			const {context_limit} = ctx.req.valid('query');
			const response = await adminService.messageService.lookupMessage({
				channel_id,
				message_id,
				context_limit,
			});
			await recordAdminRead(ctx, {
				targetType: 'message',
				targetId: message_id,
				action: AdminAuditReadActions.GET_MESSAGE,
				metadata: {
					channel_id,
					context_limit,
					found: response.messages.some((message) => message.id === message_id.toString()),
					result_count: response.messages.length,
				},
			});
			return ctx.json(response);
		},
	);
	app.delete(
		'/admin/channels/:channel_id/messages/:message_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		Validator('param', ChannelIdMessageIdParam),
		OpenAPI({
			operationId: 'delete_admin_message',
			summary: 'Delete message',
			description:
				'Deletes a single message permanently and purges its attachments. Used for removing inappropriate or harmful content. Logged to audit log. Requires MESSAGE_DELETE permission.',
			responseSchema: DeleteMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {channel_id, message_id} = ctx.req.valid('param');
			return ctx.json(
				await adminService.messageService.deleteMessage({channel_id, message_id}, adminUserId, auditLogReason),
			);
		},
	);
	app.post(
		'/admin/users/:user_id/message-shreds',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('param', UserIdParam),
		Validator('json', AdminUserMessageShredRequest),
		OpenAPI({
			operationId: 'shred_admin_user_messages',
			summary: 'Shred user messages',
			description:
				'Queues bulk shredding of the given messages of a user, with attachment deletion. Returns a job ID to track progress asynchronously. Used for large-scale content removal. Requires MESSAGE_SHRED permission.',
			responseSchema: MessageShredResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_id} = ctx.req.valid('param');
			const {entries} = ctx.req.valid('json');
			return ctx.json(
				await adminService.messageShredService.queueMessageShred({user_id, entries}, adminUserId, auditLogReason),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/messages',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE_ALL),
		Validator('param', UserIdParam),
		Validator('query', AdminUserMessageDeleteQuery),
		OpenAPI({
			operationId: 'delete_admin_user_messages',
			summary: 'Delete all user messages',
			description:
				'Deletes all messages from a specific user across all channels. Permanent operation used for account suspension or policy violation. Pass dry_run=false to delete; the default counts without deleting. Requires MESSAGE_DELETE_ALL permission.',
			responseSchema: DeleteAllUserMessagesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_id} = ctx.req.valid('param');
			const {dry_run} = ctx.req.valid('query');
			return ctx.json(
				await adminService.messageDeletionService.deleteAllUserMessages(
					{user_id, dry_run},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
}
