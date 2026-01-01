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
import {createGuildID, createUserID} from '~/BrandedTypes';
import {AdminACLs} from '~/Constants';
import {MissingACLError} from '~/Errors';
import {requireAdminACL, requireAnyAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import type {ListArchivesRequest} from '../models';
import {
	ListArchivesRequest as ListArchivesSchema,
	TriggerGuildArchiveRequest,
	TriggerUserArchiveRequest,
} from '../models';

const canViewArchive = (adminAcls: Set<string>, subjectType: 'user' | 'guild'): boolean => {
	if (adminAcls.has(AdminACLs.WILDCARD) || adminAcls.has(AdminACLs.ARCHIVE_VIEW_ALL)) return true;
	if (subjectType === 'user') return adminAcls.has(AdminACLs.ARCHIVE_TRIGGER_USER);
	return adminAcls.has(AdminACLs.ARCHIVE_TRIGGER_GUILD);
};

export const ArchiveAdminController = (app: HonoApp) => {
	app.post(
		'/admin/archives/user',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_USER),
		Validator('json', TriggerUserArchiveRequest),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminUserId = ctx.get('adminUserId');
			const body = ctx.req.valid('json');
			const result = await adminArchiveService.triggerUserArchive(createUserID(BigInt(body.user_id)), adminUserId);
			return ctx.json(result, 200);
		},
	);

	app.post(
		'/admin/archives/guild',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ARCHIVE_TRIGGER_GUILD),
		Validator('json', TriggerGuildArchiveRequest),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminUserId = ctx.get('adminUserId');
			const body = ctx.req.valid('json');
			const result = await adminArchiveService.triggerGuildArchive(createGuildID(BigInt(body.guild_id)), adminUserId);
			return ctx.json(result, 200);
		},
	);

	app.post(
		'/admin/archives/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		Validator('json', ListArchivesSchema),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const body = ctx.req.valid('json') as ListArchivesRequest;

			if (
				body.subject_type === 'all' &&
				!adminAcls.has(AdminACLs.ARCHIVE_VIEW_ALL) &&
				!adminAcls.has(AdminACLs.WILDCARD)
			) {
				throw new MissingACLError(AdminACLs.ARCHIVE_VIEW_ALL);
			}

			if (
				body.subject_type !== 'all' &&
				!canViewArchive(adminAcls, body.subject_type) &&
				!adminAcls.has(AdminACLs.WILDCARD)
			) {
				throw new MissingACLError(
					body.subject_type === 'user' ? AdminACLs.ARCHIVE_TRIGGER_USER : AdminACLs.ARCHIVE_TRIGGER_GUILD,
				);
			}

			const result = await adminArchiveService.listArchives({
				subjectType: body.subject_type as 'user' | 'guild' | 'all',
				subjectId: body.subject_id ? BigInt(body.subject_id) : undefined,
				requestedBy: body.requested_by ? BigInt(body.requested_by) : undefined,
				limit: body.limit,
				includeExpired: body.include_expired,
			});

			return ctx.json({archives: result}, 200);
		},
	);

	app.get(
		'/admin/archives/:subjectType/:subjectId/:archiveId',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const subjectType = ctx.req.param('subjectType') as 'user' | 'guild';

			if (!canViewArchive(adminAcls, subjectType) && !adminAcls.has(AdminACLs.WILDCARD)) {
				throw new MissingACLError(
					subjectType === 'user' ? AdminACLs.ARCHIVE_TRIGGER_USER : AdminACLs.ARCHIVE_TRIGGER_GUILD,
				);
			}

			const subjectId = BigInt(ctx.req.param('subjectId'));
			const archiveId = BigInt(ctx.req.param('archiveId'));

			const archive = await adminArchiveService.getArchive(subjectType, subjectId, archiveId);
			return ctx.json({archive}, 200);
		},
	);

	app.get(
		'/admin/archives/:subjectType/:subjectId/:archiveId/download',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAnyAdminACL([AdminACLs.ARCHIVE_VIEW_ALL, AdminACLs.ARCHIVE_TRIGGER_USER, AdminACLs.ARCHIVE_TRIGGER_GUILD]),
		async (ctx) => {
			const adminArchiveService = ctx.get('adminArchiveService');
			const adminAcls = ctx.get('adminUserAcls');
			const subjectType = ctx.req.param('subjectType') as 'user' | 'guild';

			if (!canViewArchive(adminAcls, subjectType) && !adminAcls.has(AdminACLs.WILDCARD)) {
				throw new MissingACLError(
					subjectType === 'user' ? AdminACLs.ARCHIVE_TRIGGER_USER : AdminACLs.ARCHIVE_TRIGGER_GUILD,
				);
			}

			const subjectId = BigInt(ctx.req.param('subjectId'));
			const archiveId = BigInt(ctx.req.param('archiveId'));

			const result = await adminArchiveService.getDownloadUrl(subjectType, subjectId, archiveId);
			return ctx.json(result, 200);
		},
	);
};
