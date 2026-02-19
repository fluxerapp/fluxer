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

import {mapUserToAdminResponse} from '@fluxer/api/src/admin/models/UserTypes';
import type {AdminAuditService} from '@fluxer/api/src/admin/services/AdminAuditService';
import type {AdminUserUpdatePropagator} from '@fluxer/api/src/admin/services/AdminUserUpdatePropagator';
import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import {createPasswordResetToken, createUserID, type UserID} from '@fluxer/api/src/BrandedTypes';
import {Logger} from '@fluxer/api/src/Logger';
import type {BotMfaMirrorService} from '@fluxer/api/src/oauth/BotMfaMirrorService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {UserContactChangeLogService} from '@fluxer/api/src/user/services/UserContactChangeLogService';
import {getIpAddressReverse, getLocationLabelFromIp} from '@fluxer/api/src/utils/IpUtils';
import {resolveSessionClientInfo} from '@fluxer/api/src/utils/UserAgentUtils';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BulkUpdateUserFlagsRequest,
	DeleteWebAuthnCredentialRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	ListWebAuthnCredentialsRequest,
	ResendVerificationEmailRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserTraitsRequest,
	TerminateSessionsRequest,
	UnlinkPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';

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

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				flags: newFlags,
			},
			user.toRow(),
		);

		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_flags',
			auditLogReason,
			metadata: new Map(
				(
					[
						['add_flags', data.addFlags.map((f) => f.toString()).join(',')],
						['remove_flags', data.removeFlags.map((f) => f.toString()).join(',')],
						['new_flags', newFlags.toString()],
					] as Array<[string, string]>
				).filter(([_, v]) => v.length > 0),
			),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
	}

	async disableMfa(data: DisableMfaRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				totp_secret: null,
				authenticator_types: null,
			},
			user.toRow(),
		);

		if (updatedUser) {
			await this.deps.botMfaMirrorService?.syncAuthenticatorTypesForOwner(updatedUser);
		}

		await userRepository.clearMfaBackupCodes(userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

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
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
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

	async resendVerificationEmail(
		data: ResendVerificationEmailRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {userRepository, authService, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}

		await authService.resendVerificationEmail(user);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'resend_verification_email',
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

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				acls: new Set(data.acls),
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_acls',
			auditLogReason,
			metadata: new Map([['acls', data.acls.join(',')]]),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
	}

	async setUserTraits(data: SetUserTraitsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const traitSet = data.traits.length > 0 ? new Set(data.traits) : null;
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				traits: traitSet,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'set_traits',
			auditLogReason,
			metadata: new Map(data.traits.length > 0 ? [['traits', data.traits.join(',')]] : []),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
	}

	async unlinkPhone(data: UnlinkPhoneRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, updatePropagator, contactChangeLogService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				phone: null,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

		await contactChangeLogService.recordDiff({
			oldUser: user,
			newUser: updatedUser,
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

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
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

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				suspicious_activity_flags: data.flags,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_suspicious_activity_flags',
			auditLogReason,
			metadata: new Map([['flags', data.flags.toString()]]),
		});

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
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

		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				flags: user.flags | UserFlags.DISABLED_SUSPICIOUS_ACTIVITY,
				suspicious_activity_flags: data.flags,
				password_hash: null,
			},
			user.toRow(),
		);

		await authService.terminateAllUserSessions(userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});

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

		return {
			user: await mapUserToAdminResponse(updatedUser, this.deps.cacheService),
		};
	}

	async bulkUpdateUserFlags(data: BulkUpdateUserFlagsRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{id: string; error: string}> = [];
		const addFlags = data.add_flags.map((flag) => BigInt(flag));
		const removeFlags = data.remove_flags.map((flag) => BigInt(flag));

		for (const userIdBigInt of data.user_ids) {
			try {
				const userId = createUserID(userIdBigInt);
				await this.updateUserFlags({
					userId,
					data: {addFlags, removeFlags},
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
			metadata: new Map(
				(
					[
						['user_count', data.user_ids.length.toString()],
						['add_flags', data.add_flags.map((f) => f.toString()).join(',')],
						['remove_flags', data.remove_flags.map((f) => f.toString()).join(',')],
					] as Array<[string, string]>
				).filter(([_, v]) => v.length > 0),
			),
		});

		return {
			successful,
			failed,
		};
	}

	async listWebAuthnCredentials(
		data: ListWebAuthnCredentialsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<WebAuthnCredentialListResponse> {
		const {userRepository, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const credentials = await userRepository.listWebAuthnCredentials(userId);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'list_webauthn_credentials',
			auditLogReason,
			metadata: new Map([['credential_count', credentials.length.toString()]]),
		});

		return credentials.map((cred) => ({
			id: cred.credentialId,
			name: cred.name,
			created_at: cred.createdAt.toISOString(),
			last_used_at: cred.lastUsedAt?.toISOString() ?? null,
		}));
	}

	async deleteWebAuthnCredential(
		data: DeleteWebAuthnCredentialRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {userRepository, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const credential = await userRepository.getWebAuthnCredential(userId, data.credential_id);
		if (!credential) {
			throw new UnknownUserError();
		}

		await userRepository.deleteWebAuthnCredential(userId, data.credential_id);

		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'delete_webauthn_credential',
			auditLogReason,
			metadata: new Map([['credential_id', data.credential_id]]),
		});
	}

	async listUserSessions(userId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		const {userRepository, auditService, cacheService} = this.deps;
		const userIdTyped = createUserID(userId);
		const user = await userRepository.findUnique(userIdTyped);
		if (!user) {
			throw new UnknownUserError();
		}

		const sessions = await userRepository.listAuthSessions(userIdTyped);
		const locationResults = await Promise.allSettled(
			sessions.map((session) => getLocationLabelFromIp(session.clientIp)),
		);
		const reverseDnsResults = await Promise.allSettled(
			sessions.map((session) => getIpAddressReverse(session.clientIp, cacheService)),
		);

		let failedCount = 0;
		for (const result of locationResults) {
			if (result.status === 'rejected') {
				failedCount++;
				Logger.warn({error: result.reason, userId: userId.toString()}, 'IP geolocation lookup failed');
			}
		}

		if (locationResults.length > 0 && failedCount === locationResults.length) {
			throw new ServiceUnavailableError({
				code: 'GEOLOCATION_SERVICE_UNAVAILABLE',
				message: 'Geolocation service is unavailable. All IP lookups failed.',
			});
		}

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
				const clientLocation = locationResult.status === 'fulfilled' ? locationResult.value : null;
				const reverseDnsResult = reverseDnsResults[index];
				const clientIpReverse = reverseDnsResult?.status === 'fulfilled' ? reverseDnsResult.value : null;
				const {clientOs, clientPlatform} = resolveSessionClientInfo({
					userAgent: session.clientUserAgent,
					isDesktopClient: session.clientIsDesktop,
				});
				return {
					session_id_hash: session.sessionIdHash.toString('base64url'),
					created_at: session.createdAt.toISOString(),
					approx_last_used_at: session.approximateLastUsedAt.toISOString(),
					client_ip: session.clientIp,
					client_ip_reverse: clientIpReverse,
					client_os: clientOs,
					client_platform: clientPlatform,
					client_location: clientLocation,
				};
			}),
		};
	}
}
