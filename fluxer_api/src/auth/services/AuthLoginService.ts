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

import type {AuthenticationResponseJSON} from '@simplewebauthn/server';
import type {LoginRequest} from '~/auth/AuthModel';
import type {UserID} from '~/BrandedTypes';
import {
	createInviteCode,
	createIpAuthorizationTicket,
	createIpAuthorizationToken,
	createMfaTicket,
	createUserID,
} from '~/BrandedTypes';
import {Config} from '~/Config';
import {APIErrorCodes, UserAuthenticatorTypes, UserFlags} from '~/Constants';
import {FluxerAPIError, InputValidationError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import type {InviteService} from '~/invite/InviteService';
import {Logger} from '~/Logger';
import type {AuthSession, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import * as IpUtils from '~/utils/IpUtils';
import * as RandomUtils from '~/utils/RandomUtils';

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
		private redisDeletionQueue: RedisAccountDeletionQueueService,
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
			throw new FluxerAPIError({
				code: APIErrorCodes.RATE_LIMITED,
				message: 'You can only resend this email once.',
				status: 429,
			});
		}

		const minDelay = 30;
		if (secondsSinceCreation < minDelay) {
			throw new FluxerAPIError({
				code: APIErrorCodes.RATE_LIMITED,
				message: 'Please wait before resending the email.',
				status: 429,
				data: {resend_available_in: minDelay - secondsSinceCreation},
			});
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
			throw new FluxerAPIError({
				code: APIErrorCodes.UNKNOWN_USER,
				message: 'User not found',
				status: 404,
			});
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
		| {mfa: false; user_id: string; token: string; pending_verification?: boolean}
		| {mfa: true; ticket: string; sms: boolean; totp: boolean; webauthn: boolean}
	> {
		const inTests = Config.dev.testModeEnabled || process.env.CI === 'true';
		const skipRateLimits = inTests || Config.dev.disableRateLimits;

		const emailRateLimit = await this.rateLimitService.checkLimit({
			identifier: `login:email:${data.email}`,
			maxAttempts: 5,
			windowMs: 15 * 60 * 1000,
		});

		if (!emailRateLimit.allowed && !skipRateLimits) {
			throw new FluxerAPIError({
				code: APIErrorCodes.RATE_LIMITED,
				message: 'Too many login attempts. Please try again later.',
				status: 429,
			});
		}

		const clientIp = IpUtils.requireClientIp(request);
		const ipRateLimit = await this.rateLimitService.checkLimit({
			identifier: `login:ip:${clientIp}`,
			maxAttempts: 10,
			windowMs: 30 * 60 * 1000,
		});

		if (!ipRateLimit.allowed && !skipRateLimits) {
			throw new FluxerAPIError({
				code: APIErrorCodes.RATE_LIMITED,
				message: 'Too many login attempts from this IP. Please try again later.',
				status: 429,
			});
		}

		const user = await this.repository.findByEmail(data.email);
		if (!user) {
			getMetricsService().counter({
				name: 'auth.login.failure',
				dimensions: {reason: 'invalid_credentials'},
			});
			throw InputValidationError.createMultiple([
				{field: 'email', message: 'Invalid email or password'},
				{field: 'password', message: 'Invalid email or password'},
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
			throw InputValidationError.createMultiple([
				{field: 'email', message: 'Invalid email or password'},
				{field: 'password', message: 'Invalid email or password'},
			]);
		}

		let currentUser = await this.handleBanStatus(user);

		if ((currentUser.flags & UserFlags.DISABLED) !== 0n && !currentUser.tempBannedUntil) {
			const updatedFlags = currentUser.flags & ~UserFlags.DISABLED;
			const updatedUser = await this.repository.patchUpsert(currentUser.id, {
				flags: updatedFlags,
			});
			if (updatedUser) {
				currentUser = updatedUser;
				Logger.info({userId: currentUser.id}, 'Auto-undisabled user on login');
			}
		}

		if ((currentUser.flags & UserFlags.SELF_DELETED) !== 0n) {
			if (currentUser.pendingDeletionAt) {
				await this.repository.removePendingDeletion(currentUser.id, currentUser.pendingDeletionAt);
			}

			await this.redisDeletionQueue.removeFromQueue(currentUser.id);

			const updatedFlags = currentUser.flags & ~UserFlags.SELF_DELETED;
			const updatedUser = await this.repository.patchUpsert(currentUser.id, {
				flags: updatedFlags,
				pending_deletion_at: null,
			});
			if (updatedUser) {
				currentUser = updatedUser;
				Logger.info({userId: currentUser.id}, 'Auto-cancelled deletion on login');
			} else {
				Logger.error({userId: currentUser.id}, 'Failed to cancel deletion during login');
				throw new Error('Failed to cancel account deletion during login');
			}
		}

		const hasMfa = (currentUser.authenticatorTypes?.size ?? 0) > 0;
		const isAppStoreReviewer = (currentUser.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;

		if (!hasMfa && !isAppStoreReviewer) {
			const isIpAuthorized = await this.repository.checkIpAuthorized(currentUser.id, clientIp);
			if (!isIpAuthorized) {
				const ticket = createIpAuthorizationTicket(await this.generateSecureToken());
				const authToken = createIpAuthorizationToken(await this.generateSecureToken());
				const geoipResult = await IpUtils.getCountryCodeDetailed(clientIp);
				const clientLocation = IpUtils.formatGeoipLocation(geoipResult);
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

				const ttlSeconds = 15 * 60;
				await this.cacheService.set<IpAuthorizationTicketCache>(`ip-auth-ticket:${ticket}`, cachePayload, ttlSeconds);
				await this.cacheService.set<{ticket: string}>(`ip-auth-token:${authToken}`, {ticket}, ttlSeconds);

				await this.repository.createIpAuthorizationToken(currentUser.id, authToken);

				await this.emailService.sendIpAuthorizationEmail(
					currentUser.email!,
					currentUser.username,
					authToken,
					clientIp,
					clientLocation,
					currentUser.locale,
				);

				throw new FluxerAPIError({
					code: APIErrorCodes.IP_AUTHORIZATION_REQUIRED,
					message: 'New login location detected. Check your inbox for a link to authorize this device.',
					status: 403,
					data: {
						ip_authorization_required: true,
						ticket,
						email: currentUser.email,
						resend_available_in: 30,
					},
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
		const isPendingVerification = (currentUser.flags & UserFlags.PENDING_MANUAL_VERIFICATION) !== 0n;

		getMetricsService().counter({
			name: 'user.login',
			dimensions: {mfa_type: 'none'},
		});
		getMetricsService().counter({
			name: 'auth.login.success',
		});

		return {
			mfa: false,
			user_id: currentUser.id.toString(),
			token,
			pending_verification: isPendingVerification ? true : undefined,
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
			throw new Error('User not found');
		}

		this.assertNonBotUser(user);

		if (!user.totpSecret) {
			const [token] = await this.createAuthSession({user, request});
			getMetricsService().counter({
				name: 'user.login',
				dimensions: {mfa_type: 'totp'},
			});
			return {user_id: user.id.toString(), token};
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
			throw new Error('User not found');
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
			throw new Error('User not found');
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
		sms: boolean;
		totp: boolean;
		webauthn: boolean;
	}> {
		const ticket = createMfaTicket(RandomUtils.randomString(64));
		await this.cacheService.set(`mfa-ticket:${ticket}`, user.id.toString(), 60 * 5);

		const credentials = await this.repository.listWebAuthnCredentials(user.id);
		const hasSms = user.authenticatorTypes.has(UserAuthenticatorTypes.SMS);
		const hasWebauthn = credentials.length > 0;
		const hasTotp = user.authenticatorTypes.has(UserAuthenticatorTypes.TOTP);

		return {
			mfa: true,
			ticket: ticket,
			sms: hasSms,
			totp: hasTotp,
			webauthn: hasWebauthn,
		};
	}
}
