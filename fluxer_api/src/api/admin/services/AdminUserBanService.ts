// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminUserUpdatePropagator} from '@app/api/admin/services/AdminUserUpdatePropagator';
import * as AuthSession from '@app/api/auth/AuthSession';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {AdminUserBanNoteRequest, TempBanUserRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';

interface AdminUserBanServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
}

export class AdminUserBanService {
	constructor(private readonly deps: AdminUserBanServiceDeps) {}

	async tempBanUser(
		data: TempBanUserRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const tempBannedUntil = new Date();
		if (data.duration_hours <= 0) {
			tempBannedUntil.setFullYear(tempBannedUntil.getFullYear() + 100);
		} else {
			tempBannedUntil.setHours(tempBannedUntil.getHours() + data.duration_hours);
		}
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				temp_banned_until: tempBannedUntil,
				flags: user.flags | UserFlags.DISABLED,
			},
			user.toRow(),
		);
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email && data.duration_hours > 0) {
			await emailService.sendAccountTempBannedEmail(
				user.email,
				user.username,
				data.reason ?? null,
				data.duration_hours,
				tempBannedUntil,
				user.locale,
			);
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'temp_ban',
			auditLogReason,
			metadata: new Map([
				['duration_hours', data.duration_hours.toString()],
				['reason', data.reason ?? 'null'],
				['banned_until', tempBannedUntil.toISOString()],
			]),
		});
		await emitAdminAction(adminUserId, userId, 'temp_ban', {durationHours: data.duration_hours});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async annotateBan(data: AdminUserBanNoteRequest & {user_id: bigint}, adminUserId: UserID): Promise<void> {
		const {users: userRepository} = this.deps.apiContext.services;
		const {auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const banLog = await auditService.findAuditLog(data.ban_audit_log_id);
		if (banLog?.action !== 'temp_ban' || banLog.targetType !== 'user' || banLog.targetId !== BigInt(userId)) {
			throw new BadRequestError({
				code: APIErrorCodes.INVALID_FORM_BODY,
				message: 'ban_audit_log_id does not name a ban of this user',
			});
		}
		const bannedUntil = user.tempBannedUntil;
		if (
			(user.flags & UserFlags.DISABLED) === 0n ||
			!bannedUntil ||
			bannedUntil.getTime() <= Date.now() ||
			banLog.metadata.get('banned_until') !== bannedUntil.toISOString()
		) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'ban_audit_log_id does not name the current ban of this user',
			});
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'annotate_ban',
			auditLogReason: data.note,
			metadata: new Map([['ban_audit_log_id', data.ban_audit_log_id.toString()]]),
		});
	}

	async unbanUser(
		data: {
			user_id: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				temp_banned_until: null,
				flags: user.flags & ~UserFlags.DISABLED,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (user.email) {
			await emailService.sendUnbanNotification(user.email, user.username, auditLogReason || null, user.locale);
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'unban',
			auditLogReason,
			metadata: new Map(),
		});
		await emitAdminAction(adminUserId, userId, 'unban');
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}
}
