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

import {mapUserToAdminResponse} from '~/admin/AdminModel';
import type {AuthService} from '~/auth/AuthService';
import {createPasswordResetToken, createUserID, type UserID} from '~/BrandedTypes';
import {UserFlags} from '~/Constants';
import {InputValidationError, UnknownUserError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {UserContactChangeLogService} from '~/user/services/UserContactChangeLogService';
import * as IpUtils from '~/utils/IpUtils';
import {resolveSessionClientInfo} from '~/utils/UserAgentUtils';
import type {
	BulkUpdateUserFlagsRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	TerminateSessionsRequest,
	UnlinkPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
} from '../AdminModel';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';

interface AdminUserSecurityServiceDeps {
	userRepository: IUserRepository;
	authService: AuthService;
	emailService: IEmailService;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
	botMfaMirrorService?: BotMfaMirrorService;
	contactChangeLogService: UserContactChangeLogService;
	cacheService: ICacheService;
}

export class AdminUserSecurityService {
	constructor(private readonly deps: AdminUserSecurityServiceDeps) {}

	async updateUserFlags({
		userId,
		data,
		adminUserId,
		auditLogReason,
	}: {
		userId: UserID;
		data: {addFlags: Array<bigint>; removeFlags: Array<bigint>};
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		let newFlags = user.flags;
		for (const flag of data.addFlags) {
			newFlags |= flag;
		}
		for (const flag of data.removeFlags) {
			newFlags &= ~flag;
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			flags: newFlags,
		});

		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_flags',
			auditLogReason,
			metadata: new Map([
				['add_flags', data.addFlags.map((f) => f.toString()).join(',')],
				['remove_flags', data.removeFlags.map((f) => f.toString()).join(',')],
				['new_flags', newFlags.toString()],
			]),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser!, this.deps.cacheService),
		};
	}

	async disableMfa(data: DisableMfaRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			totp_secret: null,
			authenticator_types: null,
		});

		if (updatedUser) {
			await this.deps.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}

		await userRepository.clearMfaBackupCodes(userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'disable_mfa',
			auditLogReason,
			metadata: new Map(),
		});
	}

	async sendPasswordReset(data: SendPasswordResetRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, emailService, authService, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.email) {
			throw InputValidationError.create('email', 'User does not have an email address');
		}

		const token = createPasswordResetToken(await authService.generateSecureToken());
		await userRepository.createPasswordResetToken({
			token_: token,
			user_id: userId,
			email: user.email,
		});

		await emailService.sendPasswordResetEmail(user.email, user.username, token, user.locale);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'send_password_reset',
			auditLogReason,
			metadata: new Map([['email', user.email]]),
		});
	}

	async terminateSessions(data: TerminateSessionsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, authService, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		await authService.terminateAllUserSessions(userId);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'terminate_sessions',
			auditLogReason,
			metadata: new Map(),
		});
	}

	async setUserAcls(data: SetUserAclsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			acls: new Set(data.acls),
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_acls',
			auditLogReason,
			metadata: new Map([['acls', data.acls.join(',')]]),
		});
	}

	async unlinkPhone(data: UnlinkPhoneRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator, contactChangeLogService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			phone: null,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser!,
			reason: 'admin_action',
			actorUserId: adminUserId,
		});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'unlink_phone',
			auditLogReason,
			metadata: new Map([['phone', user.phone ?? 'null']]),
		});
	}

	async updateSuspiciousActivityFlags(
		data: UpdateSuspiciousActivityFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			suspicious_activity_flags: data.flags,
		});
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_suspicious_activity_flags',
			auditLogReason,
			metadata: new Map([['flags', data.flags.toString()]]),
		});
	}

	async disableForSuspiciousActivity(
		data: DisableForSuspiciousActivityRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {userRepository, authService, emailService, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(userId, {
			flags: user.flags | UserFlags.DISABLED_SUSPICIOUS_ACTIVITY,
			suspicious_activity_flags: data.flags,
			password_hash: null,
		});

		await authService.terminateAllUserSessions(userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser!});

		if (user.email) {
			await emailService.sendAccountDisabledForSuspiciousActivityEmail(user.email, user.username, null, user.locale);
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'disable_suspicious_activity',
			auditLogReason,
			metadata: new Map([['flags', data.flags.toString()]]),
		});
	}

	async bulkUpdateUserFlags(data: BulkUpdateUserFlagsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{id: string; error: string}> = [];

		for (const userIdBigInt of data.user_ids) {
			try {
				const userId = createUserID(userIdBigInt);
				await this.updateUserFlags({
					userId,
					data: {addFlags: data.add_flags, removeFlags: data.remove_flags},
					adminUserId,
					auditLogReason: null,
				});
				successful.push(userId.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
		}

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(0),
			action: 'bulk_update_user_flags',
			auditLogReason,
			metadata: new Map([
				['user_count', data.user_ids.length.toString()],
				['add_flags', data.add_flags.map((f) => f.toString()).join(',')],
				['remove_flags', data.remove_flags.map((f) => f.toString()).join(',')],
			]),
		});

		return {
			successful,
			failed,
		};
	}

	async listUserSessions(userId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService} = this.deps;
		const userIdTyped = createUserID(userId);
		const user = await userRepository.findUnique(userIdTyped);
		if (!user) {
			throw new UnknownUserError();
		}

		const sessions = await userRepository.listAuthSessions(userIdTyped);
		const locationResults = await Promise.allSettled(
			sessions.map((session) => IpUtils.getLocationLabelFromIp(session.clientIp)),
		);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: userId,
			action: 'list_user_sessions',
			auditLogReason,
			metadata: new Map([['session_count', sessions.length.toString()]]),
		});

		return {
			sessions: sessions.map((session, index) => {
				const locationResult = locationResults[index];
				const clientLocation = locationResult?.status === 'fulfilled' ? locationResult.value : null;
				const {clientOs, clientPlatform} = resolveSessionClientInfo({
					userAgent: session.clientUserAgent,
					isDesktopClient: session.clientIsDesktop,
				});
				return {
					session_id_hash: session.sessionIdHash.toString('base64url'),
					created_at: session.createdAt.toISOString(),
					approx_last_used_at: session.approximateLastUsedAt.toISOString(),
					client_ip: session.clientIp,
					client_os: clientOs,
					client_platform: clientPlatform,
					client_location: clientLocation,
				};
			}),
		};
	}
}
