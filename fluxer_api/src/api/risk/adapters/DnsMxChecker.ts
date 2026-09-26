// SPDX-License-Identifier: AGPL-3.0-or-later

import {Resolver} from 'node:dns/promises';
import type {MxCheckResult} from '@app/api/risk/RiskTypes';

export interface MxResolver {
	resolveMx(domain: string): Promise<
		ReadonlyArray<{
			exchange: string;
			priority: number;
		}>
	>;
}

export class NodeDnsMxResolver implements MxResolver {
	private readonly resolver = new Resolver();

	async resolveMx(domain: string): Promise<
		ReadonlyArray<{
			exchange: string;
			priority: number;
		}>
	> {
		return this.resolver.resolveMx(domain);
	}
}

interface DnsMxCheckerContext {
	resolver: MxResolver;
	cacheTtlMs?: number;
}

interface CacheEntry {
	result: MxCheckResult;
	expiresAt: number;
}

const DEFAULT_TTL_MS = 60 * 60 * 1000;

export function createDnsMxChecker(ctx: DnsMxCheckerContext) {
	const ttl = ctx.cacheTtlMs ?? DEFAULT_TTL_MS;
	const cache = new Map<string, CacheEntry>();
	return async function checkMx(args: {domain: string}): Promise<MxCheckResult> {
		const domain = args.domain.toLowerCase().trim();
		const now = Date.now();
		const cached = cache.get(domain);
		if (cached && cached.expiresAt > now) return cached.result;
		let result: MxCheckResult;
		try {
			const records = await ctx.resolver.resolveMx(domain);
			const sorted = [...records].sort((a, b) => a.priority - b.priority);
			result = {
				domain,
				hasMx: sorted.length > 0,
				recordCount: sorted.length,
				records: sorted.slice(0, 5).map((r) => ({priority: r.priority, host: r.exchange})),
				error: sorted.length === 0 ? 'no_records' : null,
			};
		} catch (err) {
			const code = (err as NodeJS.ErrnoException).code ?? 'unknown';
			result = {
				domain,
				hasMx: false,
				recordCount: 0,
				records: [],
				error: code,
			};
		}
		cache.set(domain, {result, expiresAt: now + ttl});
		return result;
	};
}
