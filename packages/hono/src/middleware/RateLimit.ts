// SPDX-License-Identifier: AGPL-3.0-or-later

import {matchesAnyPathPattern} from '@fluxer/hono/src/middleware/utils/PathMatchers';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {MiddlewareHandler} from 'hono';

export interface RateLimitResult {
	allowed: boolean;
	limit: number;
	remaining: number;
	resetTime: Date;
	retryAfter?: number;
}

export interface RateLimitService {
	checkLimit(params: {identifier: string; maxAttempts: number; windowMs: number}): Promise<RateLimitResult>;
}

type KeyGenerator = (request: Request) => string | null | Promise<string | null>;

export interface RateLimitOptions {
	enabled?: boolean;
	skipPaths?: Array<string>;
	service?: RateLimitService;
	maxAttempts?: number;
	windowMs?: number;
	keyGenerator?: KeyGenerator;
	onLimitExceeded?: (identifier: string, path: string) => void;
	trustClientIpHeader?: boolean;
	clientIpHeaderName?: string;
}

function getClientIp(req: Request, trustClientIpHeader?: boolean, clientIpHeaderName?: string): string | null {
	return extractClientIp(req, {trustClientIpHeader, clientIpHeaderName});
}

function createDefaultKeyGenerator(trustClientIpHeader?: boolean, clientIpHeaderName?: string): KeyGenerator {
	return function defaultKeyGenerator(req: Request): string | null {
		return getClientIp(req, trustClientIpHeader, clientIpHeaderName);
	};
}

export function rateLimit(options: RateLimitOptions = {}): MiddlewareHandler {
	const {
		enabled = true,
		skipPaths = ['/_health', '/metrics'],
		service,
		maxAttempts = 100,
		windowMs = 60000,
		keyGenerator,
		onLimitExceeded,
		trustClientIpHeader = false,
		clientIpHeaderName,
	} = options;
	const resolvedKeyGenerator = keyGenerator ?? createDefaultKeyGenerator(trustClientIpHeader, clientIpHeaderName);
	return async (c, next) => {
		if (!enabled || !service) {
			await next();
			return;
		}
		const path = c.req.path;
		if (matchesAnyPathPattern(path, skipPaths)) {
			await next();
			return;
		}
		const identifier = await resolvedKeyGenerator(c.req.raw);
		if (!identifier) {
			await next();
			return;
		}
		const result = await service.checkLimit({
			identifier,
			maxAttempts,
			windowMs,
		});
		c.header('X-RateLimit-Limit', result.limit.toString());
		c.header('X-RateLimit-Remaining', result.remaining.toString());
		c.header('X-RateLimit-Reset', Math.floor(result.resetTime.getTime() / 1000).toString());
		if (!result.allowed) {
			if (result.retryAfter !== undefined) {
				c.header('Retry-After', result.retryAfter.toString());
			}
			if (onLimitExceeded) {
				onLimitExceeded(identifier, path);
			}
			return c.json(
				{
					error: 'Too Many Requests',
					message: 'Rate limit exceeded',
					retryAfter: result.retryAfter,
				},
				429,
			);
		}
		await next();
		return;
	};
}
