// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoCache} from '@pkgs/geoip/src/IpInfoService';

const DEFAULT_HOT_TTL_SECONDS = 10 * 60;

interface TieredIpInfoCacheOptions {
	hot: IpInfoCache;
	cold: IpInfoCache;
	hotTtlSeconds?: number;
	skipColdWrite?: (value: unknown) => boolean;
}

export function createTieredIpInfoCache(opts: TieredIpInfoCacheOptions): IpInfoCache {
	const hotTtl = opts.hotTtlSeconds ?? DEFAULT_HOT_TTL_SECONDS;
	return {
		async get<T>(key: string): Promise<T | null> {
			const hit = await opts.hot.get<T>(key).catch(() => null);
			if (hit !== null) return hit;
			const cold = await opts.cold.get<T>(key).catch(() => null);
			if (cold === null) return null;
			if (opts.skipColdWrite?.(cold) === true) return cold;
			void opts.hot.set(key, cold, hotTtl).catch(() => {});
			return cold;
		},
		async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
			const effectiveHotTtl = Math.max(1, Math.min(hotTtl, ttlSeconds ?? hotTtl));
			const writes: Array<Promise<void>> = [opts.hot.set(key, value, effectiveHotTtl).catch(() => {})];
			if (opts.skipColdWrite?.(value) !== true) {
				writes.push(opts.cold.set(key, value, ttlSeconds).catch(() => {}));
			}
			await Promise.all(writes);
		},
	};
}
