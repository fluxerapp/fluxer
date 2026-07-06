// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {IpAuthorizationRequiredError} from '@fluxer/errors/src/domains/auth/IpAuthorizationRequiredError';
import {IpAuthorizationResendCooldownError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendCooldownError';
import {IpAuthorizationResendLimitExceededError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendLimitExceededError';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {RegistrationRejectedError} from '@fluxer/errors/src/domains/auth/RegistrationRejectedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import type {LoginRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {formatGeoipLocation, UNKNOWN_LOCATION} from '@pkgs/geoip/src/GeoipLookup';
import type {RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';
import type {ApiContext} from '../ApiContext';
import {
	createInviteCode,
	createIpAuthorizationTicket,
	createIpAuthorizationToken,
	createMfaTicket,
	createUserID,
} from '../BrandedTypes';
import type {KVAccountDeletionQueueService} from '../infrastructure/KVAccountDeletionQueueService';
import {REGISTRATION_PENDING_APPROVAL_TRAIT, REGISTRATION_REJECTED_TRAIT} from '../instance/InstanceConfigRepository';
import type {InviteService} from '../invite/InviteService';
import {Logger} from '../Logger';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '../middleware/ServiceSingletons';
import type {User} from '../models/User';
import {lookupGeoip} from '../utils/IpUtils';
import * as AuthMfa from './AuthMfa';
import * as AuthPassword from './AuthPassword';
import * as AuthSession from './AuthSession';
import * as AuthUtility from './AuthUtility';
import {assertFlutterClientLoginAllowed, type FlutterClientGateMemberRepository} from './FlutterClientGate';

function createRequestCache(): RequestCache {
	const userPartials = new Map();
	const messageMentionChannels = new Map();
	return {
		userPartials,
		messageMentionChannels,
		clear: () => {
			userPartials.clear();
			messageMentionChannels.clear();
		},
	};
}

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
	flutterClientGateMemberRepository: FlutterClientGateMemberRepository;
}

interface LoginTokenResult {
	user_id: string;
	token: string;
}

interface LoginMfaResult {
	mfa: true;
	ticket: string;
	allowed_methods: Array<string>;
	totp: boolean;
	webauthn: boolean;
}

type LoginResult = LoginTokenResult | LoginMfaResult;

function getRetryAfterSeconds(result: RateLimitResult): number {
	return result.retryAfter ?? Math.max(0, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000));
}

function throwLoginRateLimit(result: RateLimitResult): never {
	throw new RateLimitError({
		retryAfter: getRetryAfterSeconds(result),
		limit: result.limit,
		resetTime: result.resetTime,
	});
}

export interface IpAuthorizationTicketCache {
	userId: string;
	email: string;
	username: string;
	clientIp: string;
	userAgent: string;
	platform: string | null;
	authToken: string;
	clientLocation: string;
	inviteCode?: string | null;
	resendUsed?: boolean;
	createdAt: number;
}

function getTicketCacheKey(ticket: string): string {
	return `ip-auth-ticket:${ticket}`;
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
		payload.clientIp,
		payload.clientLocation,
		null,
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
	const {users, cache, config} = ctx.services;
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
	await users.createAuthorizedIp(user.id, payload.clientIp);
	const headers: Record<string, string> = {
		[config.proxy.client_ip_header]: payload.clientIp,
		'user-agent': payload.userAgent,
	};
	if (payload.platform) {
		headers['x-fluxer-platform'] = payload.platform;
	}
	const syntheticRequest = new Request('https://api.fluxer.app/auth/ip-authorization', {
		headers,
		method: 'POST',
	});
	const [sessionToken] = await AuthSession.createAuthSession(ctx, {user, request: syntheticRequest});
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
	const {inviteService, kvDeletionQueue, flutterClientGateMemberRepository} = deps;
	const skipRateLimits = config.dev.testModeEnabled || config.dev.disableRateLimits;
	const emailRateLimit = await rateLimit.checkLimit({
		identifier: `login:email:${data.email}`,
		maxAttempts: 5,
		windowMs: ms('15 minutes'),
	});
	if (!emailRateLimit.allowed && !skipRateLimits) {
		throwLoginRateLimit(emailRateLimit);
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
		throwLoginRateLimit(ipRateLimit);
	}
	const user = await users.findByEmail(data.email);
	if (!user) {
		throw InputValidationError.fromCodes([
			{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		]);
	}
	AuthUtility.assertNonBotUser(ctx, user);
	const isMatch = await AuthPassword.verifyPassword(ctx, {
		password: data.password,
		passwordHash: user.passwordHash!,
	});
	if (!isMatch) {
		throw InputValidationError.fromCodes([
			{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
		]);
	}
	await assertFlutterClientLoginAllowed(request, user, flutterClientGateMemberRepository);
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
		if (currentUser.pendingDeletionAt) {
			await users.removePendingDeletion(currentUser.id, currentUser.pendingDeletionAt);
		}
		await kvDeletionQueue.removeFromQueue(currentUser.id);
		const updatedFlags = currentUser.flags & ~UserFlags.SELF_DELETED;
		currentUser = await users.patchUpsert(
			currentUser.id,
			{
				flags: updatedFlags,
				pending_deletion_at: null,
				deletion_reason_code: null,
				deletion_public_reason: null,
				deletion_audit_log_reason: null,
			},
			currentUser.toRow(),
		);
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
				const clientLocation = formatGeoipLocation(geoipResult) ?? UNKNOWN_LOCATION;
				const userAgent = request.headers.get('user-agent') || '';
				const platform = request.headers.get('x-fluxer-platform');
				const cachePayload: IpAuthorizationTicketCache = {
					userId: currentUser.id.toString(),
					email: currentUser.email!,
					username: currentUser.username,
					clientIp,
					userAgent,
					platform: platform ?? null,
					authToken,
					clientLocation,
					inviteCode: data.invite_code ?? null,
					resendUsed: false,
					createdAt: Date.now(),
				};
				const ttlSeconds = seconds('15 minutes');
				await cache.set<IpAuthorizationTicketCache>(`ip-auth-ticket:${ticket}`, cachePayload, ttlSeconds);
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
		return await createMfaTicketResponse(ctx, currentUser);
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
	const [token] = await AuthSession.createAuthSession(ctx, {user: currentUser, request});
	return {
		user_id: currentUser.id.toString(),
		token,
	};
}

const MFA_TICKET_MAX_ATTEMPTS = 5;
const MFA_USER_MAX_ATTEMPTS = 10;
const MFA_USER_ATTEMPTS_WINDOW = seconds('15 minutes');

export async function loginMfaTotp(
	ctx: ApiContext,
	deps: Pick<LoginDependencies, 'flutterClientGateMemberRepository'>,
	{code, ticket, request}: LoginMfaTotpParams,
): Promise<LoginTokenResult> {
	const {users, cache} = ctx.services;
	const userId = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userId) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const user = await users.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		throw new UnknownUserError();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	await assertFlutterClientLoginAllowed(request, user, deps.flutterClientGateMemberRepository);
	if (!user.totpSecret || !user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP)) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.TOTP_NOT_ENABLED);
	}
	const userAttemptsKey = `mfa-user-attempts:${user.id}`;
	const userAttempts = (await cache.get<number>(userAttemptsKey)) ?? 0;
	if (userAttempts >= MFA_USER_MAX_ATTEMPTS) {
		throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_CODE);
	}
	const isValid = await AuthMfa.verifyMfaCode(ctx, {
		userId: user.id,
		mfaSecret: user.totpSecret,
		code,
		allowBackup: true,
	});
	const attemptsKey = `mfa-ticket-attempts:${ticket}`;
	if (!isValid) {
		await cache.set(userAttemptsKey, userAttempts + 1, MFA_USER_ATTEMPTS_WINDOW);
		const attempts = ((await cache.get<number>(attemptsKey)) ?? 0) + 1;
		if (attempts >= MFA_TICKET_MAX_ATTEMPTS) {
			await cache.delete(`mfa-ticket:${ticket}`);
			await cache.delete(attemptsKey);
		} else {
			await cache.set(attemptsKey, attempts, seconds('5 minutes'));
		}
		throw InputValidationError.fromCode('code', ValidationErrorCodes.INVALID_CODE);
	}
	await cache.delete(`mfa-ticket:${ticket}`);
	await cache.delete(attemptsKey);
	await cache.delete(userAttemptsKey);
	const [token] = await AuthSession.createAuthSession(ctx, {user, request});
	return {user_id: user.id.toString(), token};
}

export async function loginMfaWebAuthn(
	ctx: ApiContext,
	deps: Pick<LoginDependencies, 'flutterClientGateMemberRepository'>,
	{response, challenge, ticket, request}: LoginMfaWebAuthnParams,
): Promise<LoginTokenResult> {
	const {users, cache} = ctx.services;
	const userId = await cache.get<string>(`mfa-ticket:${ticket}`);
	if (!userId) {
		throw InputValidationError.fromCode('ticket', ValidationErrorCodes.SESSION_TIMEOUT);
	}
	const user = await users.findUnique(createUserID(BigInt(userId)));
	if (!user) {
		throw new UnknownUserError();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	await assertFlutterClientLoginAllowed(request, user, deps.flutterClientGateMemberRepository);
	await AuthMfa.verifyWebAuthnAuthentication(ctx, user.id, response, challenge, 'mfa', ticket);
	await cache.delete(`mfa-ticket:${ticket}`);
	const [token] = await AuthSession.createAuthSession(ctx, {user, request});
	return {user_id: user.id.toString(), token};
}

async function createMfaTicketResponse(ctx: ApiContext, user: User): Promise<LoginMfaResult> {
	const {users, cache} = ctx.services;
	const ticket = createMfaTicket(await AuthUtility.generateSecureToken(ctx));
	await cache.set(`mfa-ticket:${ticket}`, user.id.toString(), seconds('5 minutes'));
	const credentials = await users.listWebAuthnCredentials(user.id);
	const hasWebauthn = credentials.length > 0;
	const hasTotp = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);
	const allowedMethods: Array<string> = [];
	if (hasTotp) allowedMethods.push('totp');
	if (hasWebauthn) allowedMethods.push('webauthn');
	return {
		mfa: true,
		ticket,
		allowed_methods: allowedMethods,
		totp: hasTotp,
		webauthn: hasWebauthn,
	};
}
