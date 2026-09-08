// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoLookupBudget, IpInfoLookupPriority} from '@pkgs/geoip/src/IpInfoService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {Logger} from '../Logger';

const BURST_KEY_PREFIX = 'ipinfo:budget:burst:';
const MONTH_KEY_PREFIX = 'ipinfo:budget:month:';
const MONTH_KEY_TTL_SECONDS = 40 * 24 * 3600;
const BURST_REFILL_INTERVAL_MS = 60_000;
const BUDGET_LOG_INTERVAL_MS = 60_000;

let lastBudgetErrorLogMs = 0;

function positiveNumberFromEnv(name: string, fallback: number): number {
	const raw = process.env[name];
	if (!raw) return fallback;
	const parsed = Number(raw);
	return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function budgetEnabled(): boolean {
	return process.env.FLUXER_IPINFO_BUDGET_ENABLED !== '0';
}

function burstConfigFor(priority: IpInfoLookupPriority): {maxTokens: number; refillPerMin: number} {
	if (priority === 'critical') {
		return {
			maxTokens: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_CRITICAL_BURST', 60),
			refillPerMin: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_CRITICAL_REFILL_PER_MIN', 60),
		};
	}
	if (priority === 'background') {
		return {
			maxTokens: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_BACKGROUND_BURST', 120),
			refillPerMin: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_BACKGROUND_REFILL_PER_MIN', 30),
		};
	}
	return {
		maxTokens: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_STANDARD_BURST', 240),
		refillPerMin: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_STANDARD_REFILL_PER_MIN', 120),
	};
}

function monthlyCeilingFor(priority: IpInfoLookupPriority): number {
	const monthlyMax = positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_MONTHLY_MAX', 140000);
	if (priority === 'critical') return monthlyMax;
	const percent =
		priority === 'background'
			? positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_BACKGROUND_MONTHLY_PCT', 60)
			: positiveNumberFromEnv('FLUXER_IPINFO_BUDGET_STANDARD_MONTHLY_PCT', 90);
	return Math.floor((monthlyMax * Math.min(percent, 100)) / 100);
}

function currentMonthKey(): string {
	const now = new Date();
	const month = String(now.getUTCMonth() + 1).padStart(2, '0');
	return `${MONTH_KEY_PREFIX}${now.getUTCFullYear()}-${month}`;
}

function logThrottled(payload: Record<string, unknown>, message: string): void {
	const now = Date.now();
	if (now - lastBudgetErrorLogMs < BUDGET_LOG_INTERVAL_MS) return;
	lastBudgetErrorLogMs = now;
	Logger.warn(payload, message);
}

export function createKvIpInfoLookupBudget(opts: {getKvClient: () => IKVProvider}): IpInfoLookupBudget {
	return {
		async tryConsume(priority: IpInfoLookupPriority): Promise<boolean> {
			if (!budgetEnabled()) {
				return true;
			}
			try {
				const kv = opts.getKvClient();
				const burst = burstConfigFor(priority);
				const consumed = await kv.tryConsumeTokens(
					`${BURST_KEY_PREFIX}${priority}`,
					1,
					burst.maxTokens,
					burst.refillPerMin,
					BURST_REFILL_INTERVAL_MS,
				);
				if (consumed < 1) {
					logThrottled({priority, reason: 'burst'}, 'IPInfo lookup budget shed');
					return false;
				}
				const monthKey = currentMonthKey();
				const used = Number((await kv.get(monthKey)) ?? '0');
				if (used >= monthlyCeilingFor(priority)) {
					logThrottled({priority, reason: 'monthly', used}, 'IPInfo lookup budget shed');
					return false;
				}
				const value = await kv.incr(monthKey);
				if (value === 1) {
					await kv.expire(monthKey, MONTH_KEY_TTL_SECONDS);
				}
				return true;
			} catch (error) {
				logThrottled({error, priority}, 'IPInfo lookup budget check failed, admitting lookup');
				return true;
			}
		},
	};
}
