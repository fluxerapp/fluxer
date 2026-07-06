// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	BrowseChannelRequest,
	BrowseChannelResponse,
	SearchChannelMessagesRequest,
	SearchChannelMessagesResponse,
} from '@fluxer/schema/src/domains/admin/AdminMessageBrowseSchemas';
import {
	DeleteAllUserMessagesRequest,
	DeleteAllUserMessagesResponse,
	DeleteMessageAttachmentRequest,
	DeleteMessageRequest,
	LookupMessageByAttachmentRequest,
	LookupMessageRequest,
	MessageShredRequest,
	MessageShredResponse,
	MessageShredStatusRequest,
	ReportAttachmentToNcmecRequest,
} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import {
	DeleteMessageResponse,
	LookupMessageResponse,
	MessageShredStatusResponse,
	NcmecAttachmentSubmitResultResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {createAttachmentID, createChannelID, createMessageID, createReportID} from '../../BrandedTypes';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function MessageAdminController(app: HonoApp) {
	app.post(
		'/admin/messages/lookup',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', LookupMessageRequest),
		OpenAPI({
			operationId: 'lookup_message',
			summary: 'Look up message details',
			description:
				'Retrieves complete message details including content, attachments, edits, and metadata. Look up by message ID and channel. Requires MESSAGE_LOOKUP permission.',
			responseSchema: LookupMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.messageService.lookupMessage(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/messages/lookup-by-attachment',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', LookupMessageByAttachmentRequest),
		OpenAPI({
			operationId: 'lookup_message_by_attachment',
			summary: 'Look up message by attachment',
			description:
				'Finds and retrieves message containing a specific attachment by ID. Used to locate messages with sensitive or illegal content. Requires MESSAGE_LOOKUP permission.',
			responseSchema: LookupMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.messageService.lookupMessageByAttachment(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/messages/report-to-ncmec',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.CSAM_SUBMIT_NCMEC),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		requireAdminACL(AdminACLs.USER_DELETE),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_USER),
		Validator('json', ReportAttachmentToNcmecRequest),
		OpenAPI({
			operationId: 'report_message_attachment_to_ncmec',
			summary: 'Report an image attachment to NCMEC',
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
	app.post(
		'/admin/messages/delete',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		Validator('json', DeleteMessageRequest),
		OpenAPI({
			operationId: 'admin_delete_message',
			summary: 'Delete single message',
			description:
				'Deletes a single message permanently. Used for removing inappropriate or harmful content. Logged to audit log. Requires MESSAGE_DELETE permission.',
			responseSchema: DeleteMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.messageService.deleteMessage(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.post(
		'/admin/messages/delete-attachment',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		Validator('json', DeleteMessageAttachmentRequest),
		OpenAPI({
			operationId: 'admin_delete_message_attachment',
			summary: 'Delete single message attachment',
			description:
				'Deletes one attachment from a message while keeping the message intact. If the attachment backs a poll option image, the poll option image reference is cleared. Logged to audit log. Requires MESSAGE_DELETE permission.',
			responseSchema: DeleteMessageResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.messageService.deleteMessageAttachment(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.post(
		'/admin/messages/shred',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('json', MessageShredRequest),
		OpenAPI({
			operationId: 'queue_message_shred',
			summary: 'Queue message shred operation',
			description:
				'Queues bulk message shredding with attachment deletion. Returns job ID to track progress asynchronously. Used for large-scale content removal. Requires MESSAGE_SHRED permission.',
			responseSchema: MessageShredResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.messageShredService.queueMessageShred(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);
	app.post(
		'/admin/messages/delete-all',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE_ALL),
		Validator('json', DeleteAllUserMessagesRequest),
		OpenAPI({
			operationId: 'delete_all_user_messages',
			summary: 'Delete all user messages',
			description:
				'Deletes all messages from a specific user across all channels. Permanent operation used for account suspension or policy violation. Requires MESSAGE_DELETE_ALL permission.',
			responseSchema: DeleteAllUserMessagesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.messageDeletionService.deleteAllUserMessages(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.post(
		'/admin/messages/shred-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('json', MessageShredStatusRequest),
		OpenAPI({
			operationId: 'get_message_shred_status',
			summary: 'Get message shred status',
			description:
				'Polls status of a queued message shred operation. Returns progress percentage and whether the job is complete. Requires MESSAGE_SHRED permission.',
			responseSchema: MessageShredStatusResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.messageShredService.getMessageShredStatus(body.job_id));
		},
	);
	app.post(
		'/admin/messages/browse',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', BrowseChannelRequest),
		OpenAPI({
			operationId: 'browse_channel_messages',
			summary: 'Browse channel messages',
			description:
				'Browses messages in a channel with cursor-based pagination. Returns messages in reverse chronological order. Requires MESSAGE_LOOKUP permission.',
			responseSchema: BrowseChannelResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.messageService.browseChannel(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/messages/search',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', SearchChannelMessagesRequest),
		OpenAPI({
			operationId: 'search_channel_messages',
			summary: 'Search channel messages',
			description: 'Searches messages within a channel by content. Requires MESSAGE_LOOKUP permission.',
			responseSchema: SearchChannelMessagesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.messageService.searchChannelMessages(ctx.req.valid('json')));
		},
	);
}
