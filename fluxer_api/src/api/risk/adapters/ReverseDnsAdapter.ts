// SPDX-License-Identifier: AGPL-3.0-or-later

import {classifyAccountPolicyReverseDnsHostname} from '@app/api/risk/AccountPolicyService';
import type {ReverseDnsResult} from '@app/api/risk/RiskTypes';
import {getIpAddressReverse} from '@app/api/utils/IpUtils';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

interface ReverseDnsAdapterContext {
	cacheService?: ICacheService;
	resolver?: (ip: string) => Promise<string | null>;
	timeoutMs?: number;
}

export function createReverseDnsLookup(ctx: ReverseDnsAdapterContext = {}) {
	const timeoutMs = ctx.timeoutMs ?? 500;
	return async function lookupReverseDns(ip: string): Promise<ReverseDnsResult> {
		const hostname = ctx.resolver
			? await ctx.resolver(ip)
			: await getIpAddressReverse(ip, ctx.cacheService, {timeoutMs});
		return {
			ip,
			hostname,
			classification: classifyAccountPolicyReverseDnsHostname(hostname),
		};
	};
}
