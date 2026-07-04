// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {TempBanUserRequest} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ApiContext} from '../../ApiContext';
import * as AuthSession from '../../auth/AuthSession';
import {createUserID, type UserID} from '../../BrandedTypes';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

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
		const {users: userRepository, email: emailService, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
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
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}

	async unbanUser(
		data: {
			user_id: bigint;
		},
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, email: emailService, cache: cacheService, gateway: gatewayService} = this.deps.apiContext.services;
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
			await emailService.sendUnbanNotification(
				user.email,
				user.username,
				auditLogReason || 'administrative action',
				user.locale,
			);
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'unban',
			auditLogReason,
			metadata: new Map(),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls, gatewayService),
		};
	}
}
