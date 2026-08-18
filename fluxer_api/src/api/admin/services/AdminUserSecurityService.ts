// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	ADMIN_PHONE_TOGGLE_CLEARABLE_FLAGS,
	ALL_SUSPICIOUS_ACTIVITY_FLAGS,
	DEFERRABLE_PHONE_FLAGS,
	DEFERRED_PHONE_ON_COMMUNITY_JOIN,
	imposePhoneRequirements,
	SuspiciousActivityFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BulkUpdateSuspiciousActivityFlagsRequest,
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
	UpdateHasVerifiedPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {ApiContext} from '../../ApiContext';
import * as AuthEmail from '../../auth/AuthEmail';
import * as AuthMfa from '../../auth/AuthMfa';
import * as AuthSession from '../../auth/AuthSession';
import * as AuthUtility from '../../auth/AuthUtility';
import {createPasswordResetToken, createUserID, type UserID} from '../../BrandedTypes';
import type {UserRow} from '../../database/types/UserTypes';
import {Logger} from '../../Logger';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import type {IRiskHistoryRepository} from '../../risk/HistoricalOutcomeRepository';
import type {HistoricalOutcomeCode} from '../../risk/RiskHistoryTypes';
import {getIpAddressReverse, getLocationLabelFromIp} from '../../utils/IpUtils';
import {resolveSessionClientInfo} from '../../utils/SessionClientIdentity';
import {mapUserToAdminResponse} from '../models/UserTypes';
import type {AdminAuditService} from './AdminAuditService';
import type {AdminUserUpdatePropagator} from './AdminUserUpdatePropagator';
import {BulkCancelledError, type BulkProgressHelpers} from './BulkProgressHelpers';

interface AdminUserSecurityServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	updatePropagator: AdminUserUpdatePropagator;
	riskHistoryRepository: Pick<IRiskHistoryRepository, 'recordOutcomeForUser'>;
}

interface FlagAuditMetadataParams {
	userCount?: number;
	addFlags: ReadonlyArray<bigint | number | string>;
	removeFlags: ReadonlyArray<bigint | number | string>;
	newFlags?: bigint | number | string;
}

function compactAuditMetadata(entries: Array<[string, string]>): Map<string, string> {
	return new Map(entries.filter(([, value]) => value.length > 0));
}

function joinAuditValues(values: ReadonlyArray<bigint | number | string>): string {
	return values.map((value) => value.toString()).join(',');
}

function createFlagAuditMetadata({
	userCount,
	addFlags,
	removeFlags,
	newFlags,
}: FlagAuditMetadataParams): Map<string, string> {
	const entries: Array<[string, string]> = [
		['add_flags', joinAuditValues(addFlags)],
		['remove_flags', joinAuditValues(removeFlags)],
	];
	if (userCount !== undefined) {
		entries.unshift(['user_count', userCount.toString()]);
	}
	if (newFlags !== undefined) {
		entries.push(['new_flags', newFlags.toString()]);
	}
	return compactAuditMetadata(entries);
}

function applyBigIntFlagUpdates(
	currentFlags: bigint,
	addFlags: ReadonlyArray<bigint>,
	removeFlags: ReadonlyArray<bigint>,
): bigint {
	let nextFlags = currentFlags;
	for (const flag of addFlags) {
		nextFlags |= flag;
	}
	for (const flag of removeFlags) {
		nextFlags &= ~flag;
	}
	return nextFlags;
}

function applyNumberFlagUpdates(
	currentFlags: number,
	addFlags: ReadonlyArray<number>,
	removeFlags: ReadonlyArray<number>,
): number {
	let nextFlags = currentFlags;
	for (const flag of addFlags) {
		nextFlags |= flag;
	}
	for (const flag of removeFlags) {
		nextFlags &= ~flag;
	}
	return nextFlags;
}

export class AdminUserSecurityService {
	constructor(private readonly deps: AdminUserSecurityServiceDeps) {}

	async updateUserFlags({
		userId,
		data,
		adminUserId,
		auditLogReason,
		acls,
	}: {
		userId: UserID;
		data: {
			addFlags: Array<bigint>;
			removeFlags: Array<bigint>;
		};
		adminUserId: UserID;
		auditLogReason: string | null;
		acls: ReadonlySet<string>;
	}) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const newFlags = applyBigIntFlagUpdates(user.flags, data.addFlags, data.removeFlags);
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
			metadata: createFlagAuditMetadata({
				addFlags: data.addFlags,
				removeFlags: data.removeFlags,
				newFlags,
			}),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async updatePremiumFlags({
		userId,
		data,
		adminUserId,
		auditLogReason,
		acls,
	}: {
		userId: UserID;
		data: {
			addFlags: Array<number>;
			removeFlags: Array<number>;
		};
		adminUserId: UserID;
		auditLogReason: string | null;
		acls: ReadonlySet<string>;
	}) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const newPremiumFlags = applyNumberFlagUpdates(user.premiumFlags, data.addFlags, data.removeFlags);
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				premium_flags: newPremiumFlags,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_premium_flags',
			auditLogReason,
			metadata: createFlagAuditMetadata({
				addFlags: data.addFlags,
				removeFlags: data.removeFlags,
				newFlags: newPremiumFlags,
			}),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async disableMfa(data: DisableMfaRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {users: userRepository, botMfaMirror: botMfaMirrorService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
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
			await botMfaMirrorService.syncAuthenticatorTypesForOwner(updatedUser);
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
		const {users: userRepository, email: emailService} = this.deps.apiContext.services;
		const {apiContext, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}
		const token = createPasswordResetToken(await AuthUtility.generateSecureToken(apiContext));
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
		const {users: userRepository} = this.deps.apiContext.services;
		const {apiContext, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}
		await AuthEmail.resendVerificationEmail(apiContext, user);
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
		const {users: userRepository} = this.deps.apiContext.services;
		const {auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'terminate_sessions',
			auditLogReason,
			metadata: new Map(),
		});
	}

	async setUserAcls(
		data: SetUserAclsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		if (userId === adminUserId) {
			throw new AccessDeniedError();
		}
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		if (!acls.has(AdminACLs.WILDCARD)) {
			const invalidAcls = data.acls.filter((acl) => !acls.has(acl));
			if (invalidAcls.length > 0) {
				throw new MissingACLError(invalidAcls[0]);
			}
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
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async setUserTraits(
		data: SetUserTraitsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
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
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async updateHasVerifiedPhone(
		data: UpdateHasVerifiedPhoneRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const phonePatch: Partial<UserRow> = {has_verified_phone: data.has_verified_phone};
		if (data.has_verified_phone) {
			const clearedFlags = (user.suspiciousActivityFlags ?? 0) & ~ADMIN_PHONE_TOGGLE_CLEARABLE_FLAGS;
			if (clearedFlags !== (user.suspiciousActivityFlags ?? 0)) {
				phonePatch.suspicious_activity_flags = clearedFlags;
			}
		}
		const updatedUser = await userRepository.patchUpsert(userId, phonePatch, user.toRow());
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_has_verified_phone',
			auditLogReason,
			metadata: new Map(
				phonePatch.suspicious_activity_flags === undefined
					? [['has_verified_phone', String(data.has_verified_phone)]]
					: [
							['has_verified_phone', String(data.has_verified_phone)],
							['suspicious_activity_flags_before', String(user.suspiciousActivityFlags ?? 0)],
							['suspicious_activity_flags_after', String(phonePatch.suspicious_activity_flags)],
						],
			),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async updateSuspiciousActivityFlags(
		data: UpdateSuspiciousActivityFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const currentFlags = user.suspiciousActivityFlags ?? 0;
		const keepsDeferral =
			(currentFlags & DEFERRED_PHONE_ON_COMMUNITY_JOIN) !== 0 &&
			(data.flags & DEFERRABLE_PHONE_FLAGS) !== 0 &&
			(data.flags & DEFERRABLE_PHONE_FLAGS) === (currentFlags & DEFERRABLE_PHONE_FLAGS);
		const newFlags = keepsDeferral ? data.flags | DEFERRED_PHONE_ON_COMMUNITY_JOIN : data.flags;
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				suspicious_activity_flags: newFlags,
			},
			user.toRow(),
		);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		if (
			(currentFlags & ALL_SUSPICIOUS_ACTIVITY_FLAGS) !== (newFlags & ALL_SUSPICIOUS_ACTIVITY_FLAGS) &&
			(newFlags & ALL_SUSPICIOUS_ACTIVITY_FLAGS) !== 0
		) {
			await this.recordRiskOutcomes(userId, ['challenged'], 'admin_update_suspicious_activity_flags');
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'update_suspicious_activity_flags',
			auditLogReason,
			metadata: new Map([['flags', data.flags.toString()]]),
		});
		return {
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async disableForSuspiciousActivity(
		data: DisableForSuspiciousActivityRequest,
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
				flags: user.flags | UserFlags.DISABLED_SUSPICIOUS_ACTIVITY,
				suspicious_activity_flags: data.flags,
				password_hash: null,
			},
			user.toRow(),
		);
		await AuthSession.terminateAllUserSessions(this.deps.apiContext, userId);
		await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser: updatedUser});
		await this.recordRiskOutcomes(
			userId,
			data.flags !== 0 ? ['challenged', 'disabled_suspicious'] : ['disabled_suspicious'],
			'admin_disable_suspicious_activity',
		);
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
			user: await mapUserToAdminResponse(updatedUser, cacheService, acls),
		};
	}

	async bulkUpdateUserFlags(
		data: BulkUpdateUserFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
		helpers?: BulkProgressHelpers,
	) {
		const {auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{
			id: string;
			error: string;
		}> = [];
		const addFlags = data.add_flags.map((flag) => BigInt(flag));
		const removeFlags = data.remove_flags.map((flag) => BigInt(flag));
		const total = data.user_ids.length;
		await helpers?.reportProgress(0, total, `Updating flags on ${total} users`);
		let processed = 0;
		for (const userIdBigInt of data.user_ids) {
			if (helpers && (await helpers.shouldCancel())) throw new BulkCancelledError();
			try {
				const userId = createUserID(userIdBigInt);
				await this.updateUserFlags({
					userId,
					data: {addFlags, removeFlags},
					adminUserId,
					auditLogReason: null,
					acls,
				});
				successful.push(userId.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
			processed++;
			if (helpers && processed % 25 === 0) {
				await helpers.reportProgress(processed, total, null);
			}
		}
		await helpers?.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(0),
			action: 'bulk_update_user_flags',
			auditLogReason,
			metadata: createFlagAuditMetadata({
				userCount: data.user_ids.length,
				addFlags: data.add_flags,
				removeFlags: data.remove_flags,
			}),
		});
		return {
			successful,
			failed,
		};
	}

	async bulkUpdateSuspiciousActivityFlags(
		data: BulkUpdateSuspiciousActivityFlagsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		helpers?: BulkProgressHelpers,
	) {
		const {users: userRepository} = this.deps.apiContext.services;
		const {auditService, updatePropagator} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{
			id: string;
			error: string;
		}> = [];
		const addMask = data.add_flags.reduce((mask, flagName) => {
			const value = SuspiciousActivityFlags[flagName as keyof typeof SuspiciousActivityFlags];
			return value !== undefined ? mask | value : mask;
		}, 0);
		const removeMask = data.remove_flags.reduce((mask, flagName) => {
			const value = SuspiciousActivityFlags[flagName as keyof typeof SuspiciousActivityFlags];
			return value !== undefined ? mask | value : mask;
		}, 0);
		const total = data.user_ids.length;
		await helpers?.reportProgress(0, total, `Updating suspicious flags on ${total} users`);
		let processed = 0;
		for (const userIdBigInt of data.user_ids) {
			if (helpers && (await helpers.shouldCancel())) throw new BulkCancelledError();
			try {
				const userId = createUserID(userIdBigInt);
				const user = await userRepository.findUnique(userId);
				if (!user) {
					throw new UnknownUserError();
				}
				const currentFlags = user.suspiciousActivityFlags ?? 0;
				const newFlags = imposePhoneRequirements(currentFlags, addMask) & ~removeMask;
				const updatedUser = await userRepository.patchUpsert(
					userId,
					{suspicious_activity_flags: newFlags},
					user.toRow(),
				);
				await updatePropagator.propagateUserUpdate({userId, oldUser: user, updatedUser});
				if (newFlags !== currentFlags && newFlags !== 0) {
					await this.recordRiskOutcomes(userId, ['challenged'], 'admin_bulk_update_suspicious_activity_flags');
				}
				successful.push(userId.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
			processed++;
			if (helpers && processed % 25 === 0) {
				await helpers.reportProgress(processed, total, null);
			}
		}
		await helpers?.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(0),
			action: 'bulk_update_suspicious_activity_flags',
			auditLogReason,
			metadata: createFlagAuditMetadata({
				userCount: data.user_ids.length,
				addFlags: data.add_flags,
				removeFlags: data.remove_flags,
			}),
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
		const {users: userRepository} = this.deps.apiContext.services;
		const {auditService} = this.deps;
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
		const {users: userRepository} = this.deps.apiContext.services;
		const {apiContext, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const credential = await userRepository.getWebAuthnCredential(userId, data.credential_id);
		if (!credential) {
			throw new UnknownUserError();
		}
		await AuthMfa.deleteWebAuthnCredential(apiContext, userId, data.credential_id);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'delete_webauthn_credential',
			auditLogReason,
			metadata: new Map([['credential_id', data.credential_id]]),
		});
	}

	async listUserSessions(
		userId: bigint,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService} = this.deps;
		const userIdTyped = createUserID(userId);
		const user = await userRepository.findUnique(userIdTyped);
		if (!user) {
			throw new UnknownUserError();
		}
		const [activeSessions, tombstones] = await Promise.all([
			userRepository.listAuthSessions(userIdTyped),
			userRepository.listAuthSessionTombstones(userIdTyped),
		]);
		const entries: Array<{
			sessionIdHash: Buffer;
			createdAt: Date;
			approximateLastUsedAt: Date;
			clientIp: string;
			clientUserAgent: string | null;
			clientOs: string | null;
			deletedAt: Date | null;
		}> = [
			...activeSessions.map((s) => ({
				sessionIdHash: s.sessionIdHash,
				createdAt: s.createdAt,
				approximateLastUsedAt: s.approximateLastUsedAt,
				clientIp: s.clientIp,
				clientUserAgent: s.clientUserAgent,
				clientOs: s.clientOs ?? null,
				deletedAt: null as Date | null,
			})),
			...tombstones.map((t) => ({
				sessionIdHash: t.sessionIdHash,
				createdAt: t.createdAt,
				approximateLastUsedAt: t.approximateLastUsedAt,
				clientIp: t.clientIp,
				clientUserAgent: t.clientUserAgent,
				clientOs: t.clientOs ?? null,
				deletedAt: t.deletedAt,
			})),
		];
		entries.sort((a, b) => {
			if (a.deletedAt === null && b.deletedAt !== null) return -1;
			if (a.deletedAt !== null && b.deletedAt === null) return 1;
			return b.createdAt.getTime() - a.createdAt.getTime();
		});
		const {branding} = await getInstanceConfigRepository().getAppPublicConfig();
		const productName = branding.product_name;
		const canViewIp = acls.has(AdminACLs.USER_VIEW_IP) || acls.has(AdminACLs.WILDCARD);
		if (!canViewIp) {
			await auditService.createAuditLog({
				adminUserId,
				targetType: 'user',
				targetId: userId,
				action: 'list_user_sessions',
				auditLogReason,
				metadata: new Map([['session_count', entries.length.toString()]]),
			});
			return {
				sessions: entries.map((entry) => {
					const clientInfo = resolveSessionClientInfo({
						userAgent: entry.clientUserAgent,
						reportedOs: entry.clientOs,
						productName,
					});
					return {
						session_id_hash: entry.sessionIdHash.toString('base64url'),
						created_at: entry.createdAt.toISOString(),
						approx_last_used_at: entry.approximateLastUsedAt.toISOString(),
						client_ip: '[redacted]',
						client_ip_reverse: null,
						client_os: clientInfo.os,
						client_platform: clientInfo.platform,
						client_location: null,
						deleted_at: entry.deletedAt?.toISOString() ?? null,
					};
				}),
			};
		}
		const locationResults = await Promise.allSettled(entries.map((entry) => getLocationLabelFromIp(entry.clientIp)));
		const reverseDnsResults = await Promise.allSettled(
			entries.map((entry) => getIpAddressReverse(entry.clientIp, cacheService)),
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
				code: APIErrorCodes.SERVICE_UNAVAILABLE,
			});
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: userId,
			action: 'list_user_sessions',
			auditLogReason,
			metadata: new Map([['session_count', entries.length.toString()]]),
		});
		return {
			sessions: entries.map((entry, index) => {
				const locationResult = locationResults[index];
				const clientLocation = locationResult.status === 'fulfilled' ? locationResult.value : null;
				const reverseDnsResult = reverseDnsResults[index];
				const clientIpReverse = reverseDnsResult?.status === 'fulfilled' ? reverseDnsResult.value : null;
				const clientInfo = resolveSessionClientInfo({
					userAgent: entry.clientUserAgent,
					reportedOs: entry.clientOs,
					productName,
				});
				return {
					session_id_hash: entry.sessionIdHash.toString('base64url'),
					created_at: entry.createdAt.toISOString(),
					approx_last_used_at: entry.approximateLastUsedAt.toISOString(),
					client_ip: entry.clientIp,
					client_ip_reverse: clientIpReverse,
					client_os: clientInfo.os,
					client_platform: clientInfo.platform,
					client_location: clientLocation,
					deleted_at: entry.deletedAt?.toISOString() ?? null,
				};
			}),
		};
	}

	private async recordRiskOutcomes(
		userId: UserID,
		outcomeCodes: ReadonlyArray<HistoricalOutcomeCode>,
		source: string,
	): Promise<void> {
		if (outcomeCodes.length === 0) {
			return;
		}
		try {
			await this.deps.riskHistoryRepository.recordOutcomeForUser({
				userId: userId.toString(),
				occurredAt: new Date(),
				source,
				outcomeCodes,
			});
		} catch (error) {
			Logger.warn({error, userId, source}, 'Failed to persist admin risk history outcome');
		}
	}
}
