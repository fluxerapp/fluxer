// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import * as AuthMfa from '@app/api/auth/AuthMfa';
import * as AuthPassword from '@app/api/auth/AuthPassword';
import {applyPendingRecovery} from '@app/api/auth/AuthRecoveryKit';
import * as AuthSession from '@app/api/auth/AuthSession';
import * as AuthUtility from '@app/api/auth/AuthUtility';
import {getLocalPartAtInstance, usernameFromInstanceLocalPart} from '@app/api/auth/InstanceAddress';
import {resolveWebAuthnSecondFactor} from '@app/api/auth/services/WebAuthnSecondFactor';
import {
	createInviteCode,
	createIpAuthorizationTicket,
	createIpAuthorizationToken,
	createMfaTicket,
	createUserID,
} from '@app/api/BrandedTypes';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {
	REGISTRATION_PENDING_APPROVAL_TRAIT,
	REGISTRATION_REJECTED_TRAIT,
} from '@app/api/instance/InstanceConfigRepository';
import type {InviteService} from '@app/api/invite/InviteService';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {AuthSession as AuthSessionModel} from '@app/api/models/AuthSession';
import type {User} from '@app/api/models/User';
import {findPersonByLoginHandle, type ParsedLoginHandle, parseLoginHandle} from '@app/api/user/UniqueUsernames';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {type ValidationErrorCode, ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
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
import {EmailType} from '@fluxer/schema/src/primitives/UserValidators';
import {formatGeoipLocation} from '@pkgs/geoip/src/GeoipLookup';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';

const DUMMY_ARGON2_HASH =
	'$argon2id$v=19$m=65536,t=3,p=4$fT6tGpAyxFiz+n1RbkRqWQ$v05UT17QGeqhsgRjcVjIWcGw6gUDYeCcAA8FiZ63MtA';

interface LoginParams {
	data: LoginRequest;
	request: Request;
	captchaVerified?: boolean;
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

interface LoginIdentifierRateLimit {
	identifier: string;
	maxAttempts: number;
	windowMs: number;
}

interface LoginIdentifier {
	field: 'email' | 'login';
	rateLimits: Array<LoginIdentifierRateLimit>;
	sourceRateLimits: (sourceKey: string) => Array<LoginIdentifierRateLimit>;
	failureRateLimit: LoginIdentifierRateLimit | null;
	invalidCode: ValidationErrorCode;
	lookup: () => Promise<User | null>;
}

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

function emitLogin(user: User, ok: boolean, details: {failure?: string; mfa?: boolean; newIp?: boolean} = {}): void {
	void emitActivity('login', user.id.toString(), {
		user_id: user.id.toString(),
		ok,
		failure: details.failure ?? null,
		mfa: details.mfa ?? false,
		new_ip: details.newIp ?? false,
	});
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
	emitLogin(user, true, {newIp: true});
	await cache.delete(cacheKey);
	await cache.delete(getTokenCacheKey(token));
	return {token: sessionToken, user_id: user.id.toString(), ticket: tokenMapping.ticket};
}

export async function login(
	ctx: ApiContext,
	deps: LoginDependencies,
	{data, request, captchaVerified = false}: LoginParams,
): Promise<LoginResult> {
	const {users, cache, rateLimit, email, config} = ctx.services;
	const {inviteService, kvDeletionQueue} = deps;
	const skipRateLimits = config.dev.testModeEnabled || config.dev.disableRateLimits;
	const identifier = await resolveLoginIdentifier(ctx, data);
	const invalidCredentials = () =>
		InputValidationError.fromCodes([
			{path: identifier.field, code: identifier.invalidCode},
			{path: 'password', code: identifier.invalidCode},
		]);
	const enforceRateLimits = async (limits: Array<LoginIdentifierRateLimit>) => {
		for (const limit of limits) {
			const result = await rateLimit.checkLimit(limit);
			if (!result.allowed && !skipRateLimits) {
				throw createRateLimitError(result);
			}
		}
	};
	await enforceRateLimits(identifier.rateLimits);
	const clientIp = requireClientIp(request, {
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	const sourceKey = getSameIpDecisionKey(clientIp) ?? clientIp;
	await enforceRateLimits([
		...identifier.sourceRateLimits(sourceKey),
		{identifier: `login:ip:${sourceKey}`, maxAttempts: 10, windowMs: ms('30 minutes')},
	]);
	const failureLimit = identifier.failureRateLimit;
	const failureState = failureLimit ? await rateLimit.peekLimit(failureLimit) : null;
	const failureLockout =
		failureLimit && failureState !== null && failureState.remaining === 0 && !skipRateLimits
			? {
					...failureState,
					allowed: false,
					retryAfter: Math.ceil(failureLimit.windowMs / failureLimit.maxAttempts / 1000),
				}
			: null;
	const rejectCredentials = async (): Promise<never> => {
		if (failureLimit) {
			await rateLimit.checkLimit(failureLimit);
		}
		if (failureLockout) {
			throw createRateLimitError(failureLockout);
		}
		throw invalidCredentials();
	};
	const user = await identifier.lookup();
	if (!user) {
		if (identifier.invalidCode === ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD) {
			await AuthPassword.verifyPassword(ctx, {password: data.password, passwordHash: DUMMY_ARGON2_HASH});
		}
		return await rejectCredentials();
	}
	AuthUtility.assertNonBotUser(ctx, user);
	if (!user.passwordHash) {
		await AuthPassword.verifyPassword(ctx, {password: data.password, passwordHash: DUMMY_ARGON2_HASH});
		emitLogin(user, false, {failure: 'no_password'});
		return await rejectCredentials();
	}
	const isMatch = await AuthPassword.verifyPassword(ctx, {
		password: data.password,
		passwordHash: user.passwordHash,
	});
	if (!isMatch) {
		emitLogin(user, false, {failure: 'bad_password'});
		return await rejectCredentials();
	}
	if (failureLockout && !captchaVerified && !(await users.checkIpAuthorized(user.id, clientIp))) {
		throw createRateLimitError(failureLockout);
	}
	const currentUser = await AuthUtility.reactivateOnSignIn(
		ctx,
		await AuthUtility.handleBanStatus(ctx, user),
		kvDeletionQueue,
	);
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
	let newIp = false;
	if (!hasMfa && !isAppStoreReviewer) {
		const isIpAuthorized = await users.checkIpAuthorized(currentUser.id, clientIp);
		newIp = !isIpAuthorized;
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
				emitLogin(currentUser, false, {failure: 'ip_authorization_required', newIp: true});
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
	emitLogin(currentUser, true, {newIp});
	return {
		user_id: currentUser.id.toString(),
		token,
	};
}

function emailLoginRateLimits(emailAddress: string): Array<LoginIdentifierRateLimit> {
	return [{identifier: `login:email:${emailAddress.toLowerCase()}`, maxAttempts: 5, windowMs: ms('15 minutes')}];
}

async function resolveLoginIdentifier(ctx: ApiContext, data: LoginRequest): Promise<LoginIdentifier> {
	const {users} = ctx.services;
	const mode = await getInstanceConfigRepository().getAccountIdentityMode();
	if (mode === AccountIdentityModes.USERNAME) {
		const field = data.email !== undefined ? 'email' : 'login';
		const input = (field === 'email' ? data.email : data.login) ?? '';
		const handle = field === 'email' ? parseOlderAppLoginHandle(ctx, input) : parseLoginHandle(input);
		return {
			field,
			rateLimits: [],
			sourceRateLimits: (sourceKey) => {
				if (!handle) {
					return [{identifier: `login:id-unparsed:${sourceKey}`, maxAttempts: 5, windowMs: ms('15 minutes')}];
				}
				const lowered = handle.username.toLowerCase();
				return [{identifier: `login:id:${lowered}:${sourceKey}`, maxAttempts: 5, windowMs: ms('15 minutes')}];
			},
			failureRateLimit: handle
				? {identifier: `login:id:${handle.username.toLowerCase()}`, maxAttempts: 100, windowMs: ms('1 hour')}
				: null,
			invalidCode: ValidationErrorCodes.INVALID_LOGIN_OR_PASSWORD,
			lookup: async () => (handle ? await findPersonByLoginHandle(users, handle) : null),
		};
	}
	if (data.email !== undefined) {
		const emailAddress = data.email;
		return {
			field: 'email',
			rateLimits: emailLoginRateLimits(emailAddress),
			sourceRateLimits: () => [],
			failureRateLimit: null,
			invalidCode: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD,
			lookup: () => users.findByEmail(emailAddress),
		};
	}
	const parsedEmail = EmailType.safeParse(data.login);
	if (!parsedEmail.success) {
		throw InputValidationError.fromCode('login', ValidationErrorCodes.INVALID_EMAIL_FORMAT);
	}
	const emailAddress = parsedEmail.data;
	return {
		field: 'login',
		rateLimits: emailLoginRateLimits(emailAddress),
		sourceRateLimits: () => [],
		failureRateLimit: null,
		invalidCode: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD,
		lookup: () => users.findByEmail(emailAddress),
	};
}

function parseOlderAppLoginHandle(ctx: ApiContext, input: string): ParsedLoginHandle | null {
	if (!input.includes('@')) return parseLoginHandle(input);
	const localPart = getLocalPartAtInstance(ctx.services.config, input.trim());
	const username = localPart === null ? null : usernameFromInstanceLocalPart(localPart);
	return username === null ? null : parseLoginHandle(username);
}

const MFA_TICKET_MAX_ATTEMPTS = 5;
const MFA_USER_MAX_ATTEMPTS = 10;

export async function consumeMfaAttempt(
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
	const [token] = await completeMfaLogin(ctx, user, ticket, request);
	return {user_id: user.id.toString(), token};
}

export async function createLoginSession(
	ctx: ApiContext,
	user: User,
	request: Request,
): Promise<[token: string, AuthSessionModel]> {
	return AuthSession.createAuthSession(ctx, {user, origin: AuthSession.resolveSessionOrigin(ctx, request)});
}

export async function completeMfaLogin(
	ctx: ApiContext,
	user: User,
	ticket: string,
	request: Request,
): Promise<[token: string, AuthSessionModel]> {
	const {cache, rateLimit} = ctx.services;
	const sessionUser = await applyPendingRecovery(ctx, user, ticket);
	await cache.delete(`mfa-ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:ticket:${ticket}`);
	await rateLimit.resetLimit(`mfa:user:${user.id}`);
	const session = await createLoginSession(ctx, sessionUser, request);
	emitLogin(sessionUser, true, {mfa: true});
	return session;
}

export async function loginMfaWebAuthn(
	ctx: ApiContext,
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
	if (!(await resolveWebAuthnSecondFactor(ctx, user))) {
		throw new MfaNotEnabledError();
	}
	await consumeMfaAttempt(ctx, {userId: user.id.toString(), ticket, field: 'ticket'});
	await AuthMfa.verifyWebAuthnAuthentication(ctx, user.id, response, challenge, 'mfa', ticket);
	const [token] = await completeMfaLogin(ctx, user, ticket, request);
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
