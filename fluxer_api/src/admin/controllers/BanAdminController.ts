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
import {BanEmailRequest, BanIpRequest, BanPhoneRequest} from '../AdminModel';

export const BanAdminController = (app: HonoApp) => {
	app.post(
		'/admin/bans/ip/add',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_IP_ADD),
		Validator('json', BanIpRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.banIp(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/ip/remove',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_IP_REMOVE),
		Validator('json', BanIpRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.unbanIp(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/ip/check',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_IP_CHECK),
		Validator('json', BanIpRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.checkIpBan(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/bans/email/add',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_EMAIL_ADD),
		Validator('json', BanEmailRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.banEmail(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/email/remove',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_EMAIL_REMOVE),
		Validator('json', BanEmailRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.unbanEmail(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/email/check',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_EMAIL_CHECK),
		Validator('json', BanEmailRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.checkEmailBan(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/bans/phone/add',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_PHONE_ADD),
		Validator('json', BanPhoneRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.banPhone(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/phone/remove',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_PHONE_REMOVE),
		Validator('json', BanPhoneRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.unbanPhone(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bans/phone/check',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BAN_OPERATION),
		requireAdminACL(AdminACLs.BAN_PHONE_CHECK),
		Validator('json', BanPhoneRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.checkPhoneBan(ctx.req.valid('json')));
		},
	);
};
