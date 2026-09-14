// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, timingSafeEqual} from 'node:crypto';
import * as AuthSession from '@app/api/auth/AuthSession';
import {Config} from '@app/api/Config';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {readRequestJsonBody} from '@app/api/utils/RequestJsonBody';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {getSameIpDecisionKey, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {BucketConfig, RateLimitResult, RateLimitScope} from '@pkgs/rate_limit/src/IRateLimitService';
import type {Context, MiddlewareHandler} from 'hono';
import {createMiddleware} from 'hono/factory';

type AccountType = 'user' | 'bot' | 'webhook';

export interface RouteRateLimitConfig {
	bucket: string;
	config: BucketConfig;
	scope?: RateLimitScope;
	trustDonorIpHeader?: boolean;
	emailBucket?: {
		bucket: string;
		config: BucketConfig;
	};
}

const TEST_ENABLE_RATE_LIMITS_HEADER = 'x-fluxer-test-enable-rate-limits';
const TEST_GLOBAL_RATE_LIMIT_OVERRIDE_HEADER = 'x-fluxer-test-global-rate-limit';
const INTERNAL_KEY_HEADER = 'x-fluxer-internal-key';
const DONOR_IP_HEADER = 'x-fluxer-donor-ip';

function shouldEnforceRateLimits(ctx: Context<HonoEnv>): boolean {
	if (!Config.dev.testModeEnabled) {
		return !Config.dev.disableRateLimits;
	}
	return ctx.req.header(TEST_ENABLE_RATE_LIMITS_HEADER) === 'true';
}

function getAccountType(ctx: Context<HonoEnv>): AccountType {
	const user = ctx.get('user');
	if (!user) {
		const params = ctx.req.param();
		if ('token' in params && 'webhook_id' in params) {
			return 'webhook';
		}
		return 'user';
	}
	if (user.isBot) {
		return 'bot';
	}
	return 'user';
}

function shouldShowHeadersOnSuccess(accountType: AccountType): boolean {
	return accountType === 'bot' || accountType === 'webhook';
}

function isTrustedInternalCaller(ctx: Context<HonoEnv>): boolean {
	const expectedKey = Config.internal.donationProxyKey;
	if (!expectedKey) return false;
	const providedKey = ctx.req.header(INTERNAL_KEY_HEADER);
	if (!providedKey) return false;
	const expectedBuffer = Buffer.from(expectedKey);
	const providedBuffer = Buffer.from(providedKey);
	if (expectedBuffer.length !== providedBuffer.length) return false;
	return timingSafeEqual(expectedBuffer, providedBuffer);
}

function getForwardedDonorIdentifier(ctx: Context<HonoEnv>): string | null {
	if (!isTrustedInternalCaller(ctx)) return null;
	const headerValue = ctx.req.header(DONOR_IP_HEADER)?.split(',', 1)[0].trim();
	if (!headerValue) return null;
	const donorIp = parseIpAddress(headerValue);
	if (!donorIp) return null;
	return `ip:${getSameIpDecisionKey(donorIp.normalized) ?? donorIp.normalized}`;
}

function getClientIdentifier(ctx: Context<HonoEnv>, routeConfig: RouteRateLimitConfig): string {
	const user = ctx.get('user');
	if (user?.id) {
		const tokenType = ctx.get('authTokenType') ?? 'session';
		if (tokenType === 'bearer') {
			return `user:${user.id}:bearer:${ctx.get('oauthBearerApplicationId') ?? 'unknown'}`;
		}
		return `user:${user.id}:${tokenType}`;
	}
	if (routeConfig.trustDonorIpHeader) {
		const donorIdentifier = getForwardedDonorIdentifier(ctx);
		if (donorIdentifier) return donorIdentifier;
	}
	const ip = getRequestClientIp(ctx);
	if (!ip) return 'internal';
	return `ip:${getSameIpDecisionKey(ip) ?? ip}`;
}

async function getRequestEmailIdentifier(ctx: Context<HonoEnv>): Promise<string | null> {
	const body = await readRequestJsonBody(ctx.req);
	if (!body.parsed || typeof body.value !== 'object' || body.value === null) return null;
	const email = Reflect.get(body.value, 'email');
	if (typeof email !== 'string') return null;
	const normalizedEmail = email.trim().toLowerCase();
	if (!normalizedEmail) return null;
	return `email:${createHash('sha256').update(normalizedEmail).digest('hex').slice(0, 32)}`;
}

function getGlobalRateLimit(ctx: Context<HonoEnv>): number {
	if (Config.dev.testModeEnabled) {
		const override = ctx.req.header(TEST_GLOBAL_RATE_LIMIT_OVERRIDE_HEADER);
		if (override) {
			const parsed = Number.parseInt(override, 10);
			if (Number.isFinite(parsed) && parsed > 0) {
				return parsed;
			}
		}
	}
	const user = ctx.get('user');
	if (user?.flags && (user.flags & UserFlags.HIGH_GLOBAL_RATE_LIMIT) !== 0n) {
		return 1200;
	}
	return 50;
}

function resolveBucket(bucket: string, clientId: string, ctx: Context<HonoEnv>): string {
	let resolved = bucket;
	const params = ctx.req.param();
	for (const [key, value] of Object.entries(params)) {
		resolved = resolved.replace(`:${key}`, String(value));
	}
	return `${clientId}:${resolved}`;
}

function getBucketHash(bucket: string): string {
	return createHash('sha256').update(bucket).digest('hex').slice(0, 16);
}

function formatRateLimitSeconds(value: number): string {
	const clampedValue = Number.isFinite(value) && value > 0 ? value : 0;
	const rounded = Math.round(clampedValue * 1000) / 1000;
	return rounded.toFixed(3).replace(/\.?0+$/, '');
}

function setRateLimitHeaders(ctx: Context<HonoEnv>, result: RateLimitResult, bucketHash: string): void {
	ctx.header('X-RateLimit-Limit', result.limit.toString());
	ctx.header('X-RateLimit-Remaining', result.remaining.toString());
	ctx.header('X-RateLimit-Reset', Math.floor(result.resetTime.getTime() / 1000).toString());
	ctx.header('X-RateLimit-Reset-After', formatRateLimitSeconds(result.resetAfterDecimal));
	ctx.header('X-RateLimit-Bucket', bucketHash);
}

function getRetryAfterSeconds(result: RateLimitResult): number {
	if (result.retryAfter !== undefined) {
		return result.retryAfter;
	}
	if (result.retryAfterDecimal !== undefined) {
		return Math.max(1, Math.ceil(result.retryAfterDecimal));
	}
	return Math.max(1, Math.ceil((result.resetTime.getTime() - Date.now()) / 1000));
}

async function revokeAuthenticatedSessionOnGlobalRateLimit(ctx: Context<HonoEnv>): Promise<void> {
	const authTokenType = ctx.get('authTokenType');
	if (authTokenType !== 'session') return;
	const user = ctx.get('user');
	if (!user || user.isBot) return;
	const token = ctx.get('authToken');
	if (!token) return;
	try {
		await AuthSession.revokeToken(ctx.get('apiContext'), token);
	} catch (_error) {}
}

export function RateLimitMiddleware(routeConfig: RouteRateLimitConfig): MiddlewareHandler<HonoEnv> {
	const routeBucketHash = getBucketHash(routeConfig.bucket);
	const emailBucketHash = routeConfig.emailBucket ? getBucketHash(routeConfig.emailBucket.bucket) : undefined;
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (!shouldEnforceRateLimits(ctx)) {
			await next();
			return;
		}
		const user = ctx.get('user');
		if (user?.flags && (user.flags & UserFlags.RATE_LIMIT_BYPASS) !== 0n) {
			await next();
			return;
		}
		const rateLimitService = ctx.get('rateLimitService');
		if (!rateLimitService) {
			await next();
			return;
		}
		const accountType = getAccountType(ctx);
		const showHeaders = shouldShowHeadersOnSuccess(accountType);
		const clientId = getClientIdentifier(ctx, routeConfig);
		if (!routeConfig.config.exemptFromGlobal) {
			const globalLimit = getGlobalRateLimit(ctx);
			const globalResult = await rateLimitService.checkGlobalLimit(clientId, globalLimit);
			if (!globalResult.allowed) {
				await revokeAuthenticatedSessionOnGlobalRateLimit(ctx);
				throw new RateLimitError({
					global: true,
					retryAfter: getRetryAfterSeconds(globalResult),
					retryAfterDecimal: globalResult.retryAfterDecimal,
					limit: globalResult.limit,
					resetTime: globalResult.resetTime,
					resetAfterDecimal: globalResult.resetAfterDecimal,
					scope: 'global',
				});
			}
		}
		const bucket = resolveBucket(routeConfig.bucket, clientId, ctx);
		const bucketConfigWithAlgorithm: BucketConfig = {
			...routeConfig.config,
			algorithm: 'leaky_bucket',
		};
		const bucketResult = await rateLimitService.checkBucketLimit(bucket, bucketConfigWithAlgorithm);
		if (!bucketResult.allowed) {
			throw new RateLimitError({
				retryAfter: getRetryAfterSeconds(bucketResult),
				retryAfterDecimal: bucketResult.retryAfterDecimal,
				limit: bucketResult.limit,
				resetTime: bucketResult.resetTime,
				resetAfterDecimal: bucketResult.resetAfterDecimal,
				bucketHash: routeBucketHash,
				scope: routeConfig.scope ?? 'user',
			});
		}
		const emailBucketConfig = routeConfig.emailBucket;
		if (emailBucketConfig) {
			const emailIdentifier = await getRequestEmailIdentifier(ctx);
			if (emailIdentifier) {
				const emailBucketResult = await rateLimitService.checkBucketLimit(
					resolveBucket(emailBucketConfig.bucket, emailIdentifier, ctx),
					{...emailBucketConfig.config, algorithm: 'leaky_bucket'},
				);
				if (!emailBucketResult.allowed) {
					throw new RateLimitError({
						retryAfter: getRetryAfterSeconds(emailBucketResult),
						retryAfterDecimal: emailBucketResult.retryAfterDecimal,
						limit: emailBucketResult.limit,
						resetTime: emailBucketResult.resetTime,
						resetAfterDecimal: emailBucketResult.resetAfterDecimal,
						bucketHash: emailBucketHash,
						scope: 'shared',
					});
				}
			}
		}
		if (showHeaders) {
			setRateLimitHeaders(ctx, bucketResult, routeBucketHash);
		}
		await next();
	});
}
