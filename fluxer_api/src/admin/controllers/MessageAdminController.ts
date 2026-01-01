/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {HonoApp} from '~/App';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import {
	DeleteAllUserMessagesRequest,
	DeleteMessageRequest,
	LookupMessageByAttachmentRequest,
	LookupMessageRequest,
	MessageShredRequest,
	MessageShredStatusRequest,
} from '../AdminModel';

export const MessageAdminController = (app: HonoApp) => {
	app.post(
		'/admin/messages/lookup',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', LookupMessageRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.lookupMessage(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/messages/lookup-by-attachment',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_LOOKUP),
		Validator('json', LookupMessageByAttachmentRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.lookupMessageByAttachment(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/messages/delete',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE),
		Validator('json', DeleteMessageRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.deleteMessage(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/messages/shred',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('json', MessageShredRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.queueMessageShred(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/messages/delete-all',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_DELETE_ALL),
		Validator('json', DeleteAllUserMessagesRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.deleteAllUserMessages(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/messages/shred-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_MESSAGE_OPERATION),
		requireAdminACL(AdminACLs.MESSAGE_SHRED),
		Validator('json', MessageShredStatusRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.getMessageShredStatus(body.job_id));
		},
	);
};
