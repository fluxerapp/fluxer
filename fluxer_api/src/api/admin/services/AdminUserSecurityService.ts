// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import * as AuthEmail from '@app/api/auth/AuthEmail';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {visibleWebAuthnCredentials} from '@app/api/auth/services/PasskeyRelyingParty';
import {RecoveryKitRepository} from '@app/api/auth/services/RecoveryKitRepository';
import {createPasswordResetToken, createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	FEATURE_LIMIT_PREMIUM_FLAGS,
	FEATURE_LIMIT_USER_FLAGS,
	findHoldingFeatureLimit,
} from '@app/api/enforcement/FeatureLimitState';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import {Logger} from '@app/api/Logger';
import {requireGrantedAdminACLs} from '@app/api/middleware/AdminMiddleware';
import {getEnforcementDecisionRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {AuthSession} from '@app/api/models/AuthSession';
import {User} from '@app/api/models/User';
import {clearNewConversationLimit} from '@app/api/user/NewConversationLimit';
import {PASSWORD_RESET_TOKEN_TTL_SECONDS} from '@app/api/user/repositories/auth/TokenRepository';
import {
	type PartialUserChangePropagationDeps,
	propagateUserUpdate,
} from '@app/api/user/services/PartialUserChangePropagation';
import {mapWebAuthnCredentialToResponse} from '@app/api/user/UserMappers';
import {resolveAssignedTraits} from '@app/api/user/UserTraits';
import {getIpAddressReverse, getLocationLabelFromIp} from '@app/api/utils/IpUtils';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {EnforcementLimit} from '@fluxer/constants/src/EnforcementReasons';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {EnforcementDecisionOwnsStateError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionOwnsStateError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	AdminPasswordResetLinkResponse,
	DeleteWebAuthnCredentialRequest,
	DisableMfaRequest,
	ListAppSessionsQuery,
	ListAppSessionsResponse,
	ListWebAuthnCredentialsRequest,
	ResendVerificationEmailRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserTraitsRequest,
	TerminateSessionsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';

const RESTRICTION_USER_FLAGS = UserFlags.DISABLED | UserFlags.DELETED | UserFlags.SPAMMER | UserFlags.REPORT_BANNED;

function combineUserFlags(flags: ReadonlyArray<bigint>): bigint {
	return flags.reduce((combined, flag) => combined | flag, 0n);
}

function combinePremiumFlags(flags: ReadonlyArray<number>): number {
	return flags.reduce((combined, flag) => combined | flag, 0);
}

function refuseFlag(field: 'add_flags' | 'remove_flags'): never {
	throw InputValidationError.fromCode(field, ValidationErrorCodes.FLAG_REQUIRES_ENFORCEMENT_ROUTE);
}

export function assertNoRestrictionFlagsAdded(addFlags: ReadonlyArray<bigint>): void {
	if ((combineUserFlags(addFlags) & RESTRICTION_USER_FLAGS) !== 0n) {
		refuseFlag('add_flags');
	}
}

async function assertNoFeatureLimitHeld(user: User, limits: ReadonlyArray<EnforcementLimit>): Promise<void> {
	for (const limit of limits) {
		const holding = await findHoldingFeatureLimit(getEnforcementDecisionRepository(), user.id, limit);
		if (holding) {
			throw new EnforcementDecisionOwnsStateError(holding.decision_id);
		}
	}
}

async function assertUserFlagUpdateAllowed(
	user: User,
	addFlags: ReadonlyArray<bigint>,
	removeFlags: ReadonlyArray<bigint>,
): Promise<void> {
	assertNoRestrictionFlagsAdded(addFlags);
	const removing = combineUserFlags(removeFlags) & user.flags;
	if ((removing & UserFlags.DELETED) !== 0n || ((removing & UserFlags.DISABLED) !== 0n && user.tempBannedUntil)) {
		refuseFlag('remove_flags');
	}
	await assertNoFeatureLimitHeld(
		user,
		(Object.keys(FEATURE_LIMIT_USER_FLAGS) as Array<EnforcementLimit>).filter(
			(limit) => (removing & FEATURE_LIMIT_USER_FLAGS[limit]!) !== 0n,
		),
	);
}

async function assertPremiumFlagUpdateAllowed(
	user: User,
	addFlags: ReadonlyArray<number>,
	removeFlags: ReadonlyArray<number>,
): Promise<void> {
	if ((combinePremiumFlags(addFlags) & PremiumFlags.PURCHASE_DISABLED) !== 0) {
		refuseFlag('add_flags');
	}
	const removing = combinePremiumFlags(removeFlags) & user.premiumFlags;
	await assertNoFeatureLimitHeld(
		user,
		(Object.keys(FEATURE_LIMIT_PREMIUM_FLAGS) as Array<EnforcementLimit>).filter(
			(limit) => (removing & FEATURE_LIMIT_PREMIUM_FLAGS[limit]!) !== 0,
		),
	);
}

interface AdminSessionApp {
	app_platform: string | null;
	app_version: string | null;
	app_release_channel: string | null;
	app_build_number: number | null;
	app_beta_number: number | null;
}

const EMPTY_SESSION_APP: AdminSessionApp = {
	app_platform: null,
	app_version: null,
	app_release_channel: null,
	app_build_number: null,
	app_beta_number: null,
};

function sessionApp(session: AuthSession): AdminSessionApp {
	return {
		app_platform: session.appPlatform,
		app_version: session.appVersion,
		app_release_channel: session.appReleaseChannel,
		app_build_number: session.appBuildNumber,
		app_beta_number: session.appBetaNumber,
	};
}

interface AdminUserSecurityServiceDeps {
	apiContext: ApiContext;
	auditService: AdminAuditService;
	userUpdateDeps: PartialUserChangePropagationDeps;
}

interface FlagAuditMetadataParams {
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

function createFlagAuditMetadata({addFlags, removeFlags, newFlags}: FlagAuditMetadataParams): Map<string, string> {
	const entries: Array<[string, string]> = [
		['add_flags', joinAuditValues(addFlags)],
		['remove_flags', joinAuditValues(removeFlags)],
	];
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
		const {auditService, userUpdateDeps} = this.deps;
		const user = await userRepository.findUniqueAssert(userId);
		await assertUserFlagUpdateAllowed(user, data.addFlags, data.removeFlags);
		let previousFlags = 0n;
		const updatedUser = await userRepository.updateFlags(userId, (flags) => {
			previousFlags = flags;
			return applyBigIntFlagUpdates(flags, data.addFlags, data.removeFlags);
		});
		if (!updatedUser) {
			throw new UnknownUserError();
		}
		const newFlags = updatedUser.flags;
		const trusted = (newFlags & UserFlags.LIMIT_EXEMPT) !== 0n && (previousFlags & UserFlags.LIMIT_EXEMPT) === 0n;
		if (trusted || (previousFlags & ~newFlags) !== 0n) {
			await clearNewConversationLimit(userId, {cache: cacheService});
		}
		const oldUser = new User({...updatedUser.toRow(), flags: previousFlags});
		await propagateUserUpdate(userUpdateDeps, oldUser, updatedUser);
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
		await emitAdminAction(adminUserId, userId, 'update_flags');
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
		const {auditService, userUpdateDeps} = this.deps;
		const user = await userRepository.findUniqueAssert(userId);
		await assertPremiumFlagUpdateAllowed(user, data.addFlags, data.removeFlags);
		const newPremiumFlags = applyNumberFlagUpdates(user.premiumFlags, data.addFlags, data.removeFlags);
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				premium_flags: newPremiumFlags,
			},
			user.toRow(),
		);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
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
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
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
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
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
		const {auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		if (!user.email) {
			throw InputValidationError.fromCode('email', ValidationErrorCodes.USER_DOES_NOT_HAVE_AN_EMAIL_ADDRESS);
		}
		const token = createPasswordResetToken(await RandomUtils.randomString(64));
		await userRepository.createPasswordResetToken({
			token_: token,
			user_id: userId,
			email: user.email,
		});
		await emailService.send('password_reset', user.email, {resetToken: token}, user.locale);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'send_password_reset',
			auditLogReason,
			metadata: new Map([['email', user.email]]),
		});
	}

	async createPasswordResetLink(
		data: SendPasswordResetRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	): Promise<AdminPasswordResetLinkResponse> {
		const {users: userRepository} = this.deps.apiContext.services;
		const {apiContext, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		AuthUtility.assertNonBotUser(apiContext, user);
		requireGrantedAdminACLs(acls, user.acls);
		const token = createPasswordResetToken(await RandomUtils.randomString(64));
		const expiresAt = new Date(Date.now() + PASSWORD_RESET_TOKEN_TTL_SECONDS * 1000);
		await userRepository.deleteAllPasswordResetTokens(userId);
		await new RecoveryKitRepository().delete(userId);
		await userRepository.createPasswordResetToken({
			token_: token,
			user_id: userId,
			email: user.email ?? null,
		});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'create_password_reset_link',
			auditLogReason,
			metadata: new Map(),
		});
		return {
			url: `${Config.email.appBaseUrl}/reset#token=${token}`,
			expires_at: expiresAt.toISOString(),
		};
	}

	async revokeRecoveryKit(
		data: SendPasswordResetRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	): Promise<void> {
		const {users: userRepository} = this.deps.apiContext.services;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		requireGrantedAdminACLs(acls, user.acls);
		await new RecoveryKitRepository().delete(userId);
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'revoke_recovery_kit',
			auditLogReason,
			metadata: new Map(),
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
		const user = await userRepository.findUniqueAssert(userId);
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
		await userRepository.findUniqueAssert(userId);
		const terminatedCount = await revokeAllAuthSessions(this.deps.apiContext.services, userId);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'terminate_sessions',
			auditLogReason,
			metadata: new Map(),
		});
		return {terminated_count: terminatedCount};
	}

	async setUserAcls(
		data: SetUserAclsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		acls: ReadonlySet<string>,
	) {
		const {users: userRepository, cache: cacheService} = this.deps.apiContext.services;
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		if (userId === adminUserId) {
			throw new AccessDeniedError();
		}
		const user = await userRepository.findUniqueAssert(userId);
		requireGrantedAdminACLs(acls, data.acls);
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				acls: new Set(data.acls),
			},
			user.toRow(),
		);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
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
		const {auditService, userUpdateDeps} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUniqueAssert(userId);
		const assigned = resolveAssignedTraits(user.traits ?? [], data.traits);
		const traitSet = assigned.size > 0 ? assigned : null;
		const updatedUser = await userRepository.patchUpsert(
			userId,
			{
				traits: traitSet,
			},
			user.toRow(),
		);
		await propagateUserUpdate(userUpdateDeps, user, updatedUser);
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

	async listWebAuthnCredentials(
		data: ListWebAuthnCredentialsRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<WebAuthnCredentialListResponse> {
		const {users: userRepository} = this.deps.apiContext.services;
		const {auditService} = this.deps;
		const userId = createUserID(data.user_id);
		await userRepository.findUniqueAssert(userId);
		const credentials = visibleWebAuthnCredentials(await userRepository.listWebAuthnCredentials(userId));
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'list_webauthn_credentials',
			auditLogReason,
			metadata: new Map([['credential_count', credentials.length.toString()]]),
		});
		return credentials.map((cred) =>
			mapWebAuthnCredentialToResponse(cred, this.deps.apiContext.services.config.auth.passkeys.rpId),
		);
	}

	async deleteWebAuthnCredential(
		data: DeleteWebAuthnCredentialRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	) {
		const {users: userRepository} = this.deps.apiContext.services;
		const {apiContext, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		await userRepository.findUniqueAssert(userId);
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
		await userRepository.findUniqueAssert(userIdTyped);
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
			app: AdminSessionApp;
			deletedAt: Date | null;
		}> = [
			...activeSessions.map((s) => ({
				sessionIdHash: s.sessionIdHash,
				createdAt: s.createdAt,
				approximateLastUsedAt: s.approximateLastUsedAt,
				clientIp: s.clientIp,
				clientUserAgent: s.clientUserAgent,
				clientOs: s.clientOs ?? null,
				app: sessionApp(s),
				deletedAt: null as Date | null,
			})),
			...tombstones.map((t) => ({
				sessionIdHash: t.sessionIdHash,
				createdAt: t.createdAt,
				approximateLastUsedAt: t.approximateLastUsedAt,
				clientIp: t.clientIp,
				clientUserAgent: t.clientUserAgent,
				clientOs: t.clientOs ?? null,
				app: EMPTY_SESSION_APP,
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
						...entry.app,
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
					...entry.app,
					deleted_at: entry.deletedAt?.toISOString() ?? null,
				};
			}),
		};
	}

	async listAppSessions(query: ListAppSessionsQuery): Promise<ListAppSessionsResponse> {
		const {sessions, nextCursor} = await this.deps.apiContext.services.users.listAuthSessionsByAppChannel({
			platform: query.platform,
			releaseChannel: query.release_channel,
			after: query.after === undefined ? null : Buffer.from(query.after, 'base64url'),
		});
		return {
			sessions: sessions.map((session) => ({
				user_id: session.userId.toString(),
				session_id_hash: session.sessionIdHash.toString('base64url'),
				created_at: session.createdAt.toISOString(),
				approx_last_used_at: session.approximateLastUsedAt.toISOString(),
				app_version: session.appVersion,
				app_build_number: session.appBuildNumber,
				app_beta_number: session.appBetaNumber,
			})),
			next_cursor: nextCursor?.toString('base64url') ?? null,
		};
	}
}
