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

import {
	createInviteCode,
	createIpAuthorizationTicket,
	createIpAuthorizationToken,
	createMfaTicket,
	createUserID,
	type UserID,
} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {KVAccountDeletionQueueService} from '@fluxer/api/src/infrastructure/KVAccountDeletionQueueService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {InviteService} from '@fluxer/api/src/invite/InviteService';
import {Logger} from '@fluxer/api/src/Logger';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {AuthSession} from '@fluxer/api/src/models/AuthSession';
import type {User} from '@fluxer/api/src/models/User';
import {withBusinessSpan} from '@fluxer/api/src/telemetry/BusinessSpans';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {lookupGeoip} from '@fluxer/api/src/utils/IpUtils';
import * as RandomUtils from '@fluxer/api/src/utils/RandomUtils';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {IEmailService} from '@fluxer/email/src/IEmailService';
import {IpAuthorizationRequiredError} from '@fluxer/errors/src/domains/auth/IpAuthorizationRequiredError';
import {IpAuthorizationResendCooldownError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendCooldownError';
import {IpAuthorizationResendLimitExceededError} from '@fluxer/errors/src/domains/auth/IpAuthorizationResendLimitExceededError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {formatGeoipLocation, UNKNOWN_LOCATION} from '@fluxer/geoip/src/GeoipLookup';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {IRateLimitService, RateLimitResult} from '@fluxer/rate_limit/src/IRateLimitService';
import type {LoginRequest} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import {ms, seconds} from 'itty-time';

function createRequestCache(): RequestCache {
	return {
		userPartials: new Map(),
		clear: () => {},
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

export class AuthLoginService {
	constructor(
		private repository: IUserRepository,
		private inviteService: InviteService,
		private cacheService: ICacheService,
		private rateLimitService: IRateLimitService,
		private emailService: IEmailService,
		private kvDeletionQueue: KVAccountDeletionQueueService,
		private verifyPassword: (params: {password: string; passwordHash: string}) => Promise<boolean>,
		private handleBanStatus: (user: User) => Promise<User>,
		private assertNonBotUser: (user: User) => void,
		private createAuthSession: (params: {user: User; request: Request}) => Promise<[string, AuthSession]>,
		private generateSecureToken: () => Promise<string>,
		private verifyMfaCode: (params: {
			userId: UserID;
			mfaSecret: string;
			code: string;
			allowBackup?: boolean;
		}) => Promise<boolean>,
		private verifySmsMfaCode: (userId: UserID, code: string) => Promise<boolean>,
		private verifyWebAuthnAuthentication: (
			userId: UserID,
			response: AuthenticationResponseJSON,
			expectedChallenge: string,
			context?: 'registration' | 'discoverable' | 'mfa' | 'sudo',
			ticket?: string,
		) => Promise<void>,
	) {}

	private getTicketCacheKey(ticket: string): string {
		return `ip-auth-ticket:${ticket}`;
	}

	private getTokenCacheKey(token: string): string {
		return `ip-auth-token:${token}`;
	}

	async resendIpAuthorization(ticket: string): Promise<{retryAfter?: number}> {
		const cacheKey = this.getTicketCacheKey(ticket);
		const payload = await this.cacheService.get<IpAuthorizationTicketCache>(cacheKey);
		if (!payload) {
			throw InputValidationError.create('ticket', 'Invalid or expired authorization ticket');
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

		await this.emailService.sendIpAuthorizationEmail(
			payload.email,
			payload.username,
			payload.authToken,
			payload.clientIp,
			payload.clientLocation,
			null,
		);

		const ttl = await this.cacheService.ttl(cacheKey);
		await this.cacheService.set(
			cacheKey,
			{
				...payload,
				resendUsed: true,
			},
			ttl > 0 ? ttl : undefined,
		);

		return {};
	}

	async completeIpAuthorization(token: string): Promise<{token: string; user_id: string; ticket: string}> {
		const tokenMapping = await this.cacheService.get<{ticket: string}>(this.getTokenCacheKey(token));
		if (!tokenMapping?.ticket) {
			throw InputValidationError.create('token', 'Invalid or expired authorization token');
		}

		const cacheKey = this.getTicketCacheKey(tokenMapping.ticket);
		const payload = await this.cacheService.get<IpAuthorizationTicketCache>(cacheKey);
		if (!payload) {
			throw InputValidationError.create('token', 'Invalid or expired authorization token');
		}

		const repoResult = await this.repository.authorizeIpByToken(token);
		if (!repoResult || repoResult.userId.toString() !== payload.userId) {
			throw InputValidationError.create('token', 'Invalid or expired authorization token');
		}

		const user = await this.repository.findUnique(createUserID(BigInt(payload.userId)));
		if (!user) {
			throw new UnknownUserError();
		}

		this.assertNonBotUser(user);

		await this.repository.createAuthorizedIp(user.id, payload.clientIp);

		const headers: Record<string, string> = {
			'X-Forwarded-For': payload.clientIp,
			'user-agent': payload.userAgent,
		};
		if (payload.platform) {
			headers['x-fluxer-platform'] = payload.platform;
		}

		const syntheticRequest = new Request('https://api.fluxer.app/auth/ip-authorization', {
			headers,
			method: 'POST',
		});

		const [sessionToken] = await this.createAuthSession({user, request: syntheticRequest});

		await this.cacheService.delete(cacheKey);
		await this.cacheService.delete(this.getTokenCacheKey(token));

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'ip_authorization'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {token: sessionToken, user_id: user.id.toString(), ticket: tokenMapping.ticket};
	}

	async login({
		data,
		request,
	}: LoginParams): Promise<
		| {user_id: string; token: string}
		| {mfa: true; ticket: string; allowed_methods: Array<string>; sms_phone_hint: string | null}
	> {
		return await withBusinessSpan('fluxer.auth.login', 'fluxer.auth.logins', {}, () =>
			this.performLogin({data, request}),
		);
	}

	private async performLogin({
		data,
		request,
	}: LoginParams): Promise<
		| {user_id: string; token: string}
		| {mfa: true; ticket: string; allowed_methods: Array<string>; sms_phone_hint: string | null}
	> {
		const skipRateLimits = Config.dev.testModeEnabled || Config.dev.disableRateLimits;

		const emailRateLimit = await this.rateLimitService.checkLimit({
			identifier: `login:email:${data.email}`,
			maxAttempts: 5,
			windowMs: ms('15 minutes'),
		});

		if (!emailRateLimit.allowed && !skipRateLimits) {
			throwLoginRateLimit(emailRateLimit);
		}

		const clientIp = requireClientIp(request, {
			trustCfConnectingIp: Config.proxy.trust_cf_connecting_ip,
		});
		const ipRateLimit = await this.rateLimitService.checkLimit({
			identifier: `login:ip:${clientIp}`,
			maxAttempts: 10,
			windowMs: ms('30 minutes'),
		});

		if (!ipRateLimit.allowed && !skipRateLimits) {
			throwLoginRateLimit(ipRateLimit);
		}

		const user = await this.repository.findByEmail(data.email);
		if (!user) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'invalid_credentials'},
			});
			throw InputValidationError.fromCodes([
				{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			]);
		}

		this.assertNonBotUser(user);

		const isMatch = await this.verifyPassword({
			password: data.password,
			passwordHash: user.passwordHash!,
		});

		if (!isMatch) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'invalid_credentials'},
			});
			throw InputValidationError.fromCodes([
				{path: 'email', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
				{path: 'password', code: ValidationErrorCodes.INVALID_EMAIL_OR_PASSWORD},
			]);
		}

		let currentUser = await this.handleBanStatus(user);

		if ((currentUser.flags & UserFlags.DISABLED) !== 0n && !currentUser.tempBannedUntil) {
			const updatedFlags = currentUser.flags & ~UserFlags.DISABLED;
			currentUser = await this.repository.patchUpsert(
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
				await this.repository.removePendingDeletion(currentUser.id, currentUser.pendingDeletionAt);
			}

			await this.kvDeletionQueue.removeFromQueue(currentUser.id);

			const updatedFlags = currentUser.flags & ~UserFlags.SELF_DELETED;
			currentUser = await this.repository.patchUpsert(
				currentUser.id,
				{
					flags: updatedFlags,
					pending_deletion_at: null,
				},
				currentUser.toRow(),
			);
			Logger.info({userId: currentUser.id}, 'Auto-cancelled deletion on login');
		}

		const hasMfa = (currentUser.authenticatorTypes?.size ?? 0) > 0;
		const isAppStoreReviewer = (currentUser.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;

		if (!hasMfa && !isAppStoreReviewer) {
			const isIpAuthorized = await this.repository.checkIpAuthorized(currentUser.id, clientIp);
			if (!isIpAuthorized) {
				const ticket = createIpAuthorizationTicket(await this.generateSecureToken());
				const authToken = createIpAuthorizationToken(await this.generateSecureToken());
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
				await this.cacheService.set<IpAuthorizationTicketCache>(`ip-auth-ticket:${ticket}`, cachePayload, ttlSeconds);
				await this.cacheService.set<{ticket: string}>(`ip-auth-token:${authToken}`, {ticket}, ttlSeconds);

				await this.repository.createIpAuthorizationToken(currentUser.id, authToken, currentUser.email!);

				await this.emailService.sendIpAuthorizationEmail(
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

		if (hasMfa) {
			return await this.createMfaTicketResponse(currentUser);
		}

		if (data.invite_code && this.inviteService) {
			try {
				await this.inviteService.acceptInvite({
					userId: currentUser.id,
					inviteCode: createInviteCode(data.invite_code),
					requestCache: createRequestCache(),
				});
			} catch (error) {
				Logger.warn({inviteCode: data.invite_code, error}, 'Failed to auto-join invite on login');
			}
		}

		const [token] = await this.createAuthSession({user: currentUser, request});

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'none'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {
			user_id: currentUser.id.toString(),
			token,
		};
	}

	async loginMfaTotp({code, ticket, request}: LoginMfaTotpParams): Promise<{user_id: string; token: string}> {
		const userId = await this.cacheService.get<string>(`mfa-ticket:${ticket}`);
		if (!userId) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_ticket_expired'},
			});
			throw InputValidationError.create('code', 'Session timeout. Please refresh the page and log in again.');
		}

		const user = await this.repository.findUnique(createUserID(BigInt(userId)));
		if (!user) {
			throw new UnknownUserError();
		}
		this.assertNonBotUser(user);

		if (!user.totpSecret || !user.authenticatorTypes?.has(UserAuthenticatorTypes.TOTP)) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_not_enabled'},
			});
			throw InputValidationError.create('code', 'TOTP is not enabled for this account');
		}

		const isValid = await this.verifyMfaCode({
			userId: user.id,
			mfaSecret: user.totpSecret,
			code,
			allowBackup: true,
		});

		if (!isValid) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw InputValidationError.create('code', 'Invalid code');
		}

		await this.cacheService.delete(`mfa-ticket:${ticket}`);
		const [token] = await this.createAuthSession({user, request});

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'totp'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {user_id: user.id.toString(), token};
	}

	async loginMfaSms({
		code,
		ticket,
		request,
	}: {
		code: string;
		ticket: string;
		request: Request;
	}): Promise<{user_id: string; token: string}> {
		const userId = await this.cacheService.get<string>(`mfa-ticket:${ticket}`);
		if (!userId) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_ticket_expired'},
			});
			throw InputValidationError.create('code', 'Session timeout. Please refresh the page and log in again.');
		}

		const user = await this.repository.findUnique(createUserID(BigInt(userId)));
		if (!user) {
			throw new UnknownUserError();
		}
		this.assertNonBotUser(user);

		const isValid = await this.verifySmsMfaCode(user.id, code);
		if (!isValid) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_invalid'},
			});
			throw InputValidationError.create('code', 'Invalid code');
		}

		await this.cacheService.delete(`mfa-ticket:${ticket}`);
		const [token] = await this.createAuthSession({user, request});

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'sms'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {user_id: user.id.toString(), token};
	}

	async loginMfaWebAuthn({
		response,
		challenge,
		ticket,
		request,
	}: {
		response: AuthenticationResponseJSON;
		challenge: string;
		ticket: string;
		request: Request;
	}): Promise<{user_id: string; token: string}> {
		const userId = await this.cacheService.get<string>(`mfa-ticket:${ticket}`);
		if (!userId) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'mfa_ticket_expired'},
			});
			throw InputValidationError.create('ticket', 'Session timeout. Please refresh the page and log in again.');
		}

		const user = await this.repository.findUnique(createUserID(BigInt(userId)));
		if (!user) {
			throw new UnknownUserError();
		}
		this.assertNonBotUser(user);

		await this.verifyWebAuthnAuthentication(user.id, response, challenge, 'mfa', ticket);

		await this.cacheService.delete(`mfa-ticket:${ticket}`);
		const [token] = await this.createAuthSession({user, request});

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'webauthn'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {user_id: user.id.toString(), token};
	}

	private async createMfaTicketResponse(user: User): Promise<{
		mfa: true;
		ticket: string;
		allowed_methods: Array<string>;
		sms_phone_hint: string | null;
		sms: boolean;
		totp: boolean;
		webauthn: boolean;
	}> {
		const ticket = createMfaTicket(RandomUtils.randomString(64));
		await this.cacheService.set(`mfa-ticket:${ticket}`, user.id.toString(), seconds('5 minutes'));

		const credentials = await this.repository.listWebAuthnCredentials(user.id);
		const hasSms = user.authenticatorTypes.has(UserAuthenticatorTypes.SMS);
		const hasWebauthn = credentials.length > 0;
		const hasTotp = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);

		const allowedMethods: Array<string> = [];
		if (hasTotp) allowedMethods.push('totp');
		if (hasSms) allowedMethods.push('sms');
		if (hasWebauthn) allowedMethods.push('webauthn');

		return {
			mfa: true,
			ticket: ticket,
			allowed_methods: allowedMethods,
			sms_phone_hint: user.phone ? this.maskPhone(user.phone) : null,
			sms: hasSms,
			totp: hasTotp,
			webauthn: hasWebauthn,
		};
	}

	private maskPhone(phone: string): string {
		if (phone.length < 4) return '****';
		return `****${phone.slice(-4)}`;
	}
}
