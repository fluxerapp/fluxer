// SPDX-License-Identifier: AGPL-3.0-or-later

import {type BudgetEnv, ipKey} from '@app/api/auth/AccountBudgets';
import type {User} from '@app/api/models/User';
import {getAccountTier, isInteractionTierEnforced} from '@app/api/user/AccountTier';
import {createRateLimitError} from '@app/api/utils/RateLimitUtils';
import {getSubnet} from '@fluxer/ip_utils/src/IpAddress';

const DAY_SECONDS = 86400;
const ACCOUNT_REPORTS_PER_DAY = 5;
const IP_REPORTS_PER_DAY = 10;

type LimitedReportTargetType = 'message' | 'user' | 'guild';

export interface LimitedReportTarget {
	type: LimitedReportTargetType;
	id: bigint;
}

function dedupeKey(clientIp: string, target: LimitedReportTarget): string {
	return `report:dedupe:${getSubnet(clientIp) ?? ipKey(clientIp)}:${target.type}:${target.id}`;
}

export function isLimitedReporter(user: User): boolean {
	return isInteractionTierEnforced() && getAccountTier(user) !== 'verified';
}

export async function admitLimitedReport(
	env: BudgetEnv,
	params: {user: User; clientIp: string; target: LimitedReportTarget},
): Promise<'store' | 'drop'> {
	const {user, clientIp, target} = params;
	for (const [identifier, maxAttempts] of [
		[`report:limited:user:${user.id}`, ACCOUNT_REPORTS_PER_DAY],
		[`report:limited:ip:${ipKey(clientIp)}`, IP_REPORTS_PER_DAY],
	] as const) {
		const result = await env.rateLimit.checkLimit({identifier, maxAttempts, windowMs: DAY_SECONDS * 1000});
		if (!result.allowed && env.enforce) throw createRateLimitError(result);
	}
	const stored = await env.kv.setnx(dedupeKey(clientIp, target), '1', DAY_SECONDS);
	return stored ? 'store' : 'drop';
}
