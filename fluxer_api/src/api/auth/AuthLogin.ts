// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {
	createInviteCode,
	createIpAuthorizationTicket,
	createIpAuthorizationToken,
	createMfaTicket,
	createUserID,
} from '@app/api/BrandedTypes';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {IpAuthorizationRequiredError} from '@fluxer/errors/src/domains/auth/IpAuthorizationRequiredError';
import {IpAuthorizationResendCooldownError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendCooldownError';
import {IpAuthorizationResendLimitExceededError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendLimitExceededError';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {RegistrationRejectedError} from '@fluxer/errors/src/domains/auth/RegistrationRejectedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {LoginRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {formatGeoipLocation} from '@pkgs/geoip/src/GeoipLookup';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';

const DUMMY_ARGON2_HASH =
	'$argon2id$v=19$m=65536,t=3,p=4$fT6tGpAyxFiz+n1RbkRqWQ$v05UT17QGeqhsgRjcVjIWcGw6gUDYeCcAA8FiZ63MtA';

interface LoginParams {
	data: LoginRequest;
	request: Request;
}

interface LoginMfaTotpParams {
	code: string;
	ticket: string;
	request: Request;
}

interface LoginMfaWebAuthnParams {
	response: AuthenticationResponseJSON;
	challenge: string;
	ticket: string;
	request: Request;
}

export interface LoginDependencies {
	inviteService: InviteService | null;
	kvDeletionQueue: KVAccountDeletionQueueService;
}

interface LoginTokenResult {
	user_id: string;
	token: string;
}

export interface LoginMfaResult {
	mfa: true;
	ticket: string;
	allowed_methods: Array<string>;
	totp: boolean;
	webauthn: boolean;
	backup_codes: boolean;
}

type LoginResult = LoginTokenResult | LoginMfaResult;

export interface IpAuthorizationTicketCache {
	userId: string;
	email: string;
	username: string;
	origin: AuthSession.SessionOrigin;
	authToken: string;
	clientLocation: string;
	locale?: string | null;
	inviteCode?: string | null;
	resendUsed?: boolean;
	createdAt: number;
}

export function getTicketCacheKey(ticket: string): string {
	return `ip-auth-ticket-v2:${ticket}`;
}

function getTokenCacheKey(token: string): string {
	return `ip-auth-token:${token}`;
}

export async function resendIpAuthorization(
	ctx: ApiContext,
	ticket: string,
): Promise<{
	retryAfter?: number;
}> {
	const {cache, email} = ctx.services;
	const cacheKey = getTicketCacheKey(ticket);
	const payload = await cache.get<IpAuthorizationTicketCache>(cacheKey);
	if (!payload) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TICKET);
	}
	const now = Date.now();
	const secondsSinceCreation = Math.floor((now - payload.createdAt) / 1000);
	if (payload.resendUsed) {
		throw new IpAuthorizationResendLimitExceededError();
	}
	const minDelay = 30;
	if (secondsSinceCreation < minDelay) {
		throw new IpAuthorizationResendCooldownError(minDelay - secondsSinceCreation);
	}
	await email.sendIpAuthorizationEmail(
		payload.email,
		payload.username,
		payload.authToken,
		payload.origin.ip,
		payload.clientLocation,
		payload.locale ?? null,
	);
	const ttl = await cache.ttl(cacheKey);
	await cache.set(
		cacheKey,
		{
			...payload,
			resendUsed: true,
		},
		ttl > 0 ? ttl : undefined,
	);
	return {};
}

export async function completeIpAuthorization(
	ctx: ApiContext,
	token: string,
): Promise<{
	token: string;
	user_id: string;
	ticket: string;
}> {
	const {users, cache} = ctx.services;
	const tokenMapping = await cache.get<{
		ticket: string;
	}>(getTokenCacheKey(token));
	if (!tokenMapping?.ticket) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN);
	}
	const cacheKey = getTicketCacheKey(tokenMapping.ticket);
	const payload = await cache.get<IpAuthorizationTicketCache>(cacheKey);
	if (!payload) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN);
	}
	const repoResult = await AuthUtility.authorizeIpByToken(ctx, token);
	if (!repoResult || repoResult.userId.toString() !== payload.userId) {
		throw InputValidationError.fromCode('token', ValidationErrorCodes.INVALID_OR_EXPIRED_AUTHORIZATION_TOKEN);
	}
	const user = await users.findUnique(createUserID(BigInt(payload.userId)));
	if (!user) {
		throw new UnknownUserError();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	await users.createAuthorizedIp(user.id, payload.origin.ip);
	const [sessionToken] = await AuthSession.createAuthSession(ctx, {user, origin: payload.origin});
	await cache.delete(cacheKey);
	await cache.delete(getTokenCacheKey(token));
	return {token: sessionToken, user_id: user.id.toString(), ticket: tokenMapping.ticket};
}

export async function login(
	ctx: ApiContext,
	deps: LoginDependencies,
	{data, request}: LoginParams,
): Promise<LoginResult> {
	const {users, cache, rateLimit, email, config} = ctx.services;
	const {inviteService, kvDeletionQueue} = deps;
	const skipRateLimits = config.dev.testModeEnabled || config.dev.disableRateLimits;
	const emailRateLimit = await rateLimit.checkLimit({
		identifier: `login:email:${data.email.toLowerCase()}`,
		maxAttempts: 5,
		windowMs: ms('15 minutes'),
	});
	if (!emailRateLimit.allowed && !skipRateLimits) {
		throw createRateLimitError(emailRateLimit);
	}
	const clientIp = requireClientIp(request, {
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	const ipRateLimit = await rateLimit.checkLimit({
		identifier: `login:ip:${getSameIpDecisionKey(clientIp) ?? clientIp}`,
		maxAttempts: 10,
		windowMs: ms('30 minutes'),
	});
	if (!ipRateLimit.allowed && !skipRateLimits) {
		throw createRateLimitError(ipRateLimit);
	}
	const user = await users.findByEmail(data.email);
	if (!user) {
		throw InputValidationError.fromCodes([
			{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		]);
	}
	AuthUtility.assertNonBotUser(ctx, user);
	if (!user.passwordHash) {
		await AuthPassword.verifyPassword(ctx, {password: data.password, passwordHash: DUMMY_ARGON2_HASH});
		throw InputValidationError.fromCodes([
			{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		]);
	}
	const isMatch = await AuthPassword.verifyPassword(ctx, {
		password: data.password,
		passwordHash: user.passwordHash,
	});
	if (!isMatch) {
		throw InputValidationError.fromCodes([
			{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		]);
	}
	let currentUser = await AuthUtility.handleBanStatus(ctx, user);
	if ((currentUser.flags & UserFlags.DISABLED) !== 0n && !currentUser.tempBannedUntil) {
		const updatedFlags = currentUser.flags & ~UserFlags.DISABLED;
		currentUser = await users.patchUpsert(
			currentUser.id,
			{
				flags: updatedFlags,
			},
			currentUser.toRow(),
		);
		Logger.info({userId: currentUser.id}, 'Auto-undisabled user on login');
	}
	if ((currentUser.flags & UserFlags.SELF_DELETED) !== 0n) {
		const pendingDeletionAt = currentUser.pendingDeletionAt;
		const updatedFlags = currentUser.flags & ~UserFlags.SELF_DELETED;
		currentUser = await users.updateDeletionSchedule(currentUser, {
			flags: updatedFlags,
			pending_deletion_at: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
		});
		if (pendingDeletionAt) {
			await users.removePendingDeletion(currentUser.id, pendingDeletionAt);
		}
		await kvDeletionQueue.removeFromQueue(currentUser.id);
		Logger.info({userId: currentUser.id}, 'Auto-cancelled deletion on login');
	}
	if (currentUser.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)) {
		throw new RegistrationPendingApprovalError();
	}
	if (currentUser.traits.has(REGISTRATION_REJECTED_TRAIT)) {
		throw new RegistrationRejectedError();
	}
	const hasMfa =
		currentUser.authenticatorTypes.has(UserAuthenticatorTypes.TOTP) ||
		currentUser.authenticatorTypes.has(UserAuthenticatorTypes.WEBAUTHN);
	const isAppStoreReviewer = (currentUser.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;
	if (!hasMfa && !isAppStoreReviewer) {
		const isIpAuthorized = await users.checkIpAuthorized(currentUser.id, clientIp);
		if (!isIpAuthorized) {
			const instanceConfigRepository = getInstanceConfigRepository();
			const [integrationsConfig, effectiveEmailConfig] = await Promise.all([
				instanceConfigRepository.getInstanceIntegrationsConfig(),
				instanceConfigRepository.getEffectiveEmailConfig(),
			]);
			if (integrationsConfig.email.disable_new_ip_authorization || !effectiveEmailConfig.enabled) {
				await users.createAuthorizedIp(currentUser.id, clientIp);
			} else {
				const ticket = createIpAuthorizationTicket(await AuthUtility.generateSecureToken(ctx));
				const authToken = createIpAuthorizationToken(await AuthUtility.generateSecureToken(ctx));
				const geoipResult = await lookupGeoip(clientIp);
				const clientLocation =
					formatGeoipLocation(geoipResult, currentUser.locale) ??
					getContentMessage('auth.unknown_location', currentUser.locale);
				const cachePayload: IpAuthorizationTicketCache = {
					userId: currentUser.id.toString(),
					email: currentUser.email!,
					username: currentUser.username,
					origin: AuthSession.resolveSessionOrigin(ctx, request),
					authToken,
					clientLocation,
					locale: currentUser.locale,
					inviteCode: data.invite_code ?? null,
					resendUsed: false,
					createdAt: Date.now(),
				};
				const ttlSeconds = seconds('15 minutes');
				await cache.set<IpAuthorizationTicketCache>(getTicketCacheKey(ticket), cachePayload, ttlSeconds);
				await cache.set<{
					ticket: string;
				}>(`ip-auth-token:${authToken}`, {ticket}, ttlSeconds);
				await users.createIpAuthorizationToken(currentUser.id, authToken, currentUser.email!);
				await email.sendIpAuthorizationEmail(
					currentUser.email!,
					currentUser.username,
					authToken,
					clientIp,
					clientLocation,
					currentUser.locale,
				);
				throw new IpAuthorizationRequiredError({
					ticket,
					email: currentUser.email!,
					resendAvailableIn: 30,
				});
			}
		}
	}
	if (hasMfa) {
		const webauthnIsSecondFactor = await resolveWebAuthnSecondFactor(ctx, currentUser);
		return await createMfaTicketResponse(ctx, currentUser, webauthnIsSecondFactor);
	}
	if (data.invite_code && inviteService) {
		try {
			await inviteService.acceptInvite({
				userId: currentUser.id,
				inviteCode: createInviteCode(data.invite_code),
				requestCache: createRequestCache(),
			});
		} catch (error) {
			Logger.warn({inviteCode: data.invite_code, error}, 'Failed to auto-join invite on login');
		}
	}
	const [token] = await AuthSession.createAuthSession(ctx, {
		user: currentUser,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {
		user_id: currentUser.id.toString(),
		token,
	};
}

const MFA_TICKET_MAX_ATTEMPTS = 5;
const MFA_USER_MAX_ATTEMPTS = 10;

async function consumeMfaAttempt(
	ctx: ApiContext,
	{userId, ticket, field}: {userId: string; ticket: string; field: string},
): Promise<void> {
	const {cache, rateLimit} = ctx.services;
	const userLimit = await rateLimit.checkLimit({
		identifier: `mfa:user:${userId}`,
		maxAttempts: MFA_USER_MAX_ATTEMPTS,
		windowMs: ms('15 minutes'),
	});
	if (!userLimit.allowed) {
		throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_CODE);
	}
	const ticketLimit = await rateLimit.checkLimit({
		identifier: `mfa:ticket:${ticket}`,
		maxAttempts: MFA_TICKET_MAX_ATTEMPTS,
		windowMs: ms('5 minutes'),
	});
	if (!ticketLimit.allowed) {
		await cache.delete(`mfa-ticket:${ticket}`);
		throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_CODE);
	}
}

export async function loginMfaTotp(
	ctx: ApiContext,
	{code, ticket, request}: LoginMfaTotpParams,
): Promise<LoginTokenResult> {
	const {users, cache, rateLimit} = ctx.services;
	const userId = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userId) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const user = await users.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		throw new UnknownUserError();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	const hasTotp = Boolean(user.totpSecret) && user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);
	if (!hasTotp && !(await AuthMfa.hasUnconsumedBackupCodes(ctx, user.id))) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.TOTP_NOT_ENABLED);
	}
	await consumeMfaAttempt(ctx, {userId: user.id.toString(), ticket, field: 'code'});
	const isValid = await AuthMfa.verifyMfaCode(ctx, {
		userId: user.id,
		mfaSecret: hasTotp ? user.totpSecret : null,
		code,
		allowBackup: true,
	});
	if (!isValid) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_CODE);
	}
	await cache.delete(`mfa-ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:user:${user.id}`);
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {user_id: user.id.toString(), token};
}

export async function loginMfaWebAuthn(
	ctx: ApiContext,
	{response, challenge, ticket, request}: LoginMfaWebAuthnParams,
): Promise<LoginTokenResult> {
	const {users, cache, rateLimit} = ctx.services;
	const userId = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userId) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const user = await users.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		throw new UnknownUserError();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	if (!(await resolveWebAuthnSecondFactor(ctx, user))) {
		throw new MfaNotEnabledError();
	}
	await consumeMfaAttempt(ctx, {userId: user.id.toString(), ticket, field: 'ticket'});
	await AuthMfa.verifyWebAuthnAuthentication(ctx, user.id, response, challenge, 'mfa', ticket);
	await cache.delete(`mfa-ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:user:${user.id}`);
	const [token] = await AuthSession.createAuthSession(ctx, {
		user,
		origin: AuthSession.resolveSessionOrigin(ctx, request),
	});
	return {user_id: user.id.toString(), token};
}

export async function createMfaTicketResponse(
	ctx: ApiContext,
	user: User,
	webauthnIsSecondFactor: boolean,
): Promise<LoginMfaResult> {
	const {cache} = ctx.services;
	const ticket = createMfaTicket(await AuthUtility.generateSecureToken(ctx));
	await cache.set(`mfa-ticket:${ticket}`, user.id.toString(), seconds('5 minutes'));
	const hasTotp = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);
	const hasBackupCodes = await AuthMfa.hasUnconsumedBackupCodes(ctx, user.id);
	const allowedMethods: Array<string> = [];
	if (hasTotp) allowedMethods.push('totp');
	if (webauthnIsSecondFactor) allowedMethods.push('webauthn');
	if (hasBackupCodes) allowedMethods.push('backup_codes');
	return {
		mfa: true,
		ticket,
		allowed_methods: allowedMethods,
		totp: hasTotp,
		webauthn: webauthnIsSecondFactor,
		backup_codes: hasBackupCodes,
	};
}
