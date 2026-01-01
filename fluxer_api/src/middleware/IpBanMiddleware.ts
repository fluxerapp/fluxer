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

import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '~/App';
import {AdminRepository} from '~/admin/AdminRepository';
import {IpBannedError} from '~/Errors';
import {Logger} from '~/Logger';
import {type IpFamily, parseIpBanEntry, tryParseSingleIp} from '~/utils/IpRangeUtils';
import {extractClientIp} from '~/utils/IpUtils';

type FamilyMap<T> = Record<IpFamily, Map<string, T>>;

interface SingleCacheEntry {
	value: bigint;
	count: number;
}

interface RangeCacheEntry {
	start: bigint;
	end: bigint;
	count: number;
}

class IpBanCache {
	private singleIpBans: FamilyMap<SingleCacheEntry>;
	private rangeIpBans: FamilyMap<RangeCacheEntry>;
	private isInitialized = false;
	private refreshIntervalMs = 30 * 1000;
	private adminRepository = new AdminRepository();
	private consecutiveFailures = 0;
	private maxConsecutiveFailures = 5;

	constructor() {
		this.singleIpBans = this.createFamilyMaps();
		this.rangeIpBans = this.createFamilyMaps();
	}

	async initialize(): Promise<void> {
		if (this.isInitialized) return;

		await this.refresh();
		this.isInitialized = true;

		setInterval(() => {
			this.refresh().catch((err) => {
				this.consecutiveFailures++;

				if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
					console.error(
						`Failed to refresh IP ban cache ${this.consecutiveFailures} times in a row. ` +
							`Last error: ${err.message}. Cache may be stale.`,
					);
				} else {
					console.warn(
						`Failed to refresh IP ban cache (${this.consecutiveFailures}/${this.maxConsecutiveFailures}): ${err.message}`,
					);
				}
			});
		}, this.refreshIntervalMs);
	}

	async refresh(): Promise<void> {
		const ips = await this.adminRepository.listBannedIps();
		this.resetCaches();
		for (const ip of ips) {
			this.addEntry(ip);
		}
		this.consecutiveFailures = 0;
	}

	isBanned(ip: string): boolean {
		const parsed = tryParseSingleIp(ip);
		if (!parsed) return false;

		const singleMap = this.singleIpBans[parsed.family];
		if (singleMap.has(parsed.canonical)) {
			return true;
		}

		const rangeMap = this.rangeIpBans[parsed.family];
		for (const range of rangeMap.values()) {
			if (parsed.value >= range.start && parsed.value <= range.end) {
				return true;
			}
		}

		return false;
	}

	ban(ip: string): void {
		this.addEntry(ip);
	}

	unban(ip: string): void {
		this.removeEntry(ip);
	}

	private resetCaches(): void {
		this.singleIpBans = this.createFamilyMaps();
		this.rangeIpBans = this.createFamilyMaps();
	}

	private createFamilyMaps<T>(): FamilyMap<T> {
		return {
			ipv4: new Map(),
			ipv6: new Map(),
		};
	}

	private addEntry(value: string): void {
		const parsed = parseIpBanEntry(value);
		if (!parsed) {
			Logger.warn({value}, 'Skipping invalid IP ban entry');
			return;
		}

		if (parsed.type === 'single') {
			const map = this.singleIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (existing) {
				existing.count += 1;
			} else {
				map.set(parsed.canonical, {value: parsed.value, count: 1});
			}
		} else {
			const map = this.rangeIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (existing) {
				existing.count += 1;
			} else {
				map.set(parsed.canonical, {start: parsed.start, end: parsed.end, count: 1});
			}
		}
	}

	private removeEntry(value: string): void {
		const parsed = parseIpBanEntry(value);
		if (!parsed) return;

		if (parsed.type === 'single') {
			const map = this.singleIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (!existing) return;
			if (existing.count <= 1) {
				map.delete(parsed.canonical);
			} else {
				existing.count -= 1;
			}
		} else {
			const map = this.rangeIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (!existing) return;
			if (existing.count <= 1) {
				map.delete(parsed.canonical);
			} else {
				existing.count -= 1;
			}
		}
	}
}

export const ipBanCache = new IpBanCache();

export const IpBanMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const clientIp = extractClientIp(ctx.req.raw);

	if (clientIp && ipBanCache.isBanned(clientIp)) {
		throw new IpBannedError();
	}

	await next();
});
