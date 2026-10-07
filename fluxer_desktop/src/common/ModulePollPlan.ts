// SPDX-License-Identifier: AGPL-3.0-or-later

export const MODULE_POLL_INTERVAL_MS = 10 * 60 * 1000;
const MODULE_POLL_JITTER_RATIO = 0.2;

export interface ModulePollPlan {
	readonly intervalMs: number;
	readonly jitterRatio: number;
}

export function resolveModulePollPlan(devIntervalMs: number | null): ModulePollPlan {
	if (devIntervalMs != null) {
		return {intervalMs: devIntervalMs, jitterRatio: 0};
	}
	return {intervalMs: MODULE_POLL_INTERVAL_MS, jitterRatio: MODULE_POLL_JITTER_RATIO};
}

export function nextModulePollDelay(plan: ModulePollPlan, random: () => number = Math.random): number {
	if (plan.jitterRatio <= 0) {
		return plan.intervalMs;
	}
	const spread = plan.intervalMs * plan.jitterRatio;
	return Math.max(1, Math.round(plan.intervalMs - spread + random() * spread * 2));
}
