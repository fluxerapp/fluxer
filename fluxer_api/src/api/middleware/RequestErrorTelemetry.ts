// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import {Config} from '@app/api/Config';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {workerMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey, isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {createMiddleware} from 'hono/factory';

const FLUSH_INTERVAL_MS = 5000;
const MAX_IPS_PER_FLUSH = 500;
const MAX_TOKEN_HASHES = 20;

interface IpErrorCounts {
	ip: string;
	s401: number;
	s403: number;
	s404: number;
	s429: number;
	other4xx: number;
	authFailures: number;
	tokenHashes: Set<string>;
}

let pending = new Map<string, IpErrorCounts>();
let windowStartedAt = Date.now();
let flushTimer: ReturnType<typeof setInterval> | null = null;
const recordedRequests = new WeakSet<Request>();

export function hashRequestToken(token: string): string {
	return createHash('sha256').update(token).digest('hex').slice(0, 32);
}

function countsFor(ip: string | null): IpErrorCounts | null {
	if (!ip) return null;
	const parsed = parseIpAddress(ip);
	if (!parsed || !isPublicIpAddress(parsed.normalized) || isIpBanExempt(parsed.normalized)) return null;
	const key = getSameIpDecisionKey(parsed.normalized) ?? parsed.normalized;
	let counts = pending.get(key);
	if (!counts) {
		if (pending.size >= MAX_IPS_PER_FLUSH) return null;
		counts = {
			ip: parsed.normalized,
			s401: 0,
			s403: 0,
			s404: 0,
			s429: 0,
			other4xx: 0,
			authFailures: 0,
			tokenHashes: new Set(),
		};
		pending.set(key, counts);
	}
	return counts;
}

export function recordRequestStatus(request: Request, status: number): void {
	if (status < 400 || status >= 500 || recordedRequests.has(request)) return;
	recordedRequests.add(request);
	const counts = countsFor(
		extractClientIp(request, {
			trustClientIpHeader: Config.proxy.trust_client_ip_header,
			clientIpHeaderName: Config.proxy.client_ip_header,
		}),
	);
	if (!counts) return;
	if (status === 401) counts.s401++;
	else if (status === 403) counts.s403++;
	else if (status === 404) counts.s404++;
	else if (status === 429) counts.s429++;
	else counts.other4xx++;
}

export function recordAuthFailure(ip: string | null, tokenHash: string | null): void {
	const counts = countsFor(ip);
	if (!counts) return;
	counts.authFailures++;
	if (tokenHash && counts.tokenHashes.size < MAX_TOKEN_HASHES) counts.tokenHashes.add(tokenHash);
}

export const RequestErrorTelemetry = createMiddleware<HonoEnv>(async (ctx, next) => {
	await next();
	if (ctx.get('user')) return;
	recordRequestStatus(ctx.req.raw, ctx.res.status);
});

function flushRequestErrorTelemetry(nowMs = Date.now()): number {
	const flushed = pending;
	const windowMs = Math.max(0, Math.min(nowMs - windowStartedAt, 0xffffffff));
	pending = new Map();
	windowStartedAt = nowMs;
	for (const [key, counts] of flushed) {
		void emitActivity(
			'http_errors',
			`ip:${key}`,
			{
				ip: counts.ip,
				window_ms: windowMs,
				s401: counts.s401,
				s403: counts.s403,
				s404: counts.s404,
				s429: counts.s429,
				other_4xx: counts.other4xx,
				auth_failures: counts.authFailures,
				token_hashes: [...counts.tokenHashes],
			},
			{...workerMeta(), channel: 'other', ip: counts.ip},
		);
	}
	return flushed.size;
}

export function startRequestErrorTelemetry(): void {
	if (flushTimer) return;
	windowStartedAt = Date.now();
	flushTimer = setInterval(() => {
		flushRequestErrorTelemetry();
	}, FLUSH_INTERVAL_MS);
	flushTimer.unref?.();
}

export function stopRequestErrorTelemetry(): void {
	if (flushTimer) clearInterval(flushTimer);
	flushTimer = null;
	pending = new Map();
}
