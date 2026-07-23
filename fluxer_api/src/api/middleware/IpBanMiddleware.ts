// SPDX-License-Identifier: AGPL-3.0-or-later

import {IpBannedError} from '@fluxer/errors/src/domains/moderation/IpBannedError';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {getSameIpDecisionKey, type IpAddressFamily} from '@fluxer/ip_utils/src/IpAddress';
import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {createMiddleware} from 'hono/factory';
import {AdminRepository} from '../admin/AdminRepository';
import type {BannedIpEntry, BannedIpKind} from '../admin/IAdminRepository';
import {Config} from '../Config';
import {IP_BAN_REFRESH_CHANNEL} from '../constants/IpBan';
import {Logger} from '../Logger';
import {isIpBanExempt} from '../risk/IpBanExemptions';
import type {HonoEnv} from '../types/HonoEnv';
import {parseIpBanEntry, tryParseSingleIp} from '../utils/IpRangeUtils';

type FamilyMap<T> = Record<IpAddressFamily, Map<string, T>>;

interface IpBanMetadata {
	kind: BannedIpKind;
	expiresAt: Date | null;
}

interface IpBanCount {
	permanent: number;
	temporary: number;
	temporaryExpiresAt: Date | null;
}

interface SingleCacheEntry {
	value: bigint;
	count: IpBanCount;
}

interface RangeCacheEntry {
	start: bigint;
	end: bigint;
	count: IpBanCount;
}

interface IpBanMatch {
	ipAddress: string;
	matchedEntry: string;
	kind: BannedIpKind;
	expiresAt: Date | null;
}

interface IpBanCacheState {
	singleIpBans: FamilyMap<SingleCacheEntry>;
	rangeIpBans: FamilyMap<RangeCacheEntry>;
	sameIpDecisionBans: Map<string, IpBanCount>;
}

const PERMANENT_BAN_METADATA: IpBanMetadata = {
	kind: 'permanent',
	expiresAt: null,
};

class IpBanCache {
	private singleIpBans: FamilyMap<SingleCacheEntry>;
	private rangeIpBans: FamilyMap<RangeCacheEntry>;
	private sameIpDecisionBans: Map<string, IpBanCount>;
	private isInitialized = false;
	private adminRepository = new AdminRepository();
	private consecutiveFailures = 0;
	private maxConsecutiveFailures = 5;
	private kvClient: IKVProvider | null = null;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private messageHandler: ((channel: string) => void) | null = null;
	private periodicRefreshTimer: NodeJS.Timeout | null = null;

	constructor() {
		const state = this.createEmptyState();
		this.singleIpBans = state.singleIpBans;
		this.rangeIpBans = state.rangeIpBans;
		this.sameIpDecisionBans = state.sameIpDecisionBans;
	}

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	async initialize(): Promise<void> {
		if (this.isInitialized) return;
		await this.refresh();
		this.isInitialized = true;
		this.setupSubscriber();
		this.startPeriodicRefresh();
	}

	private startPeriodicRefresh(): void {
		if (this.periodicRefreshTimer) return;
		const intervalMs = Number(process.env.FLUXER_IP_BAN_REFRESH_INTERVAL_MS ?? '300000');
		if (!Number.isFinite(intervalMs) || intervalMs <= 0) return;
		this.periodicRefreshTimer = setInterval(() => {
			this.refresh().catch((err) => {
				const message = err instanceof Error ? err.message : String(err);
				Logger.warn({error: message}, 'Periodic IP ban cache refresh failed');
			});
		}, intervalMs);
		if (
			typeof this.periodicRefreshTimer === 'object' &&
			this.periodicRefreshTimer &&
			'unref' in this.periodicRefreshTimer
		) {
			(this.periodicRefreshTimer as {unref(): void}).unref();
		}
	}

	private setupSubscriber(): void {
		if (this.subscriberInitialized || !this.kvClient) {
			return;
		}
		const subscription = this.kvClient.duplicate();
		this.kvSubscription = subscription;
		this.messageHandler = (channel: string) => {
			if (channel === IP_BAN_REFRESH_CHANNEL) {
				this.refresh().catch((err) => {
					this.consecutiveFailures++;
					const message = err instanceof Error ? err.message : String(err);
					if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
						Logger.error({error: message}, 'Failed to refresh IP ban cache after notification');
					} else {
						Logger.warn({error: message}, 'Failed to refresh IP ban cache after notification');
					}
				});
			}
		};
		subscription
			.connect()
			.then(() => subscription.subscribe(IP_BAN_REFRESH_CHANNEL))
			.then(() => {
				if (this.messageHandler) {
					subscription.on('message', this.messageHandler);
				}
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to IP ban refresh channel');
			});
		this.subscriberInitialized = true;
	}

	async refresh(): Promise<void> {
		const entries = await this.adminRepository.loadAllBannedIpEntries();
		const state = this.createEmptyState();
		for (const entry of entries) {
			this.addEntryToState(entry.ip, this.metadataFromEntry(entry), state);
		}
		this.singleIpBans = state.singleIpBans;
		this.rangeIpBans = state.rangeIpBans;
		this.sameIpDecisionBans = state.sameIpDecisionBans;
		this.consecutiveFailures = 0;
	}

	isBanned(ip: string): boolean {
		return this.getMatch(ip) !== null;
	}

	getMatch(ip: string): IpBanMatch | null {
		if (isIpBanExempt(ip)) return null;
		const parsed = tryParseSingleIp(ip);
		if (!parsed) return null;
		const sameIpDecisionKey = getSameIpDecisionKey(parsed.canonical);
		if (sameIpDecisionKey) {
			const decisionCount = this.sameIpDecisionBans.get(sameIpDecisionKey);
			if (decisionCount) {
				return {
					ipAddress: parsed.canonical,
					matchedEntry: sameIpDecisionKey,
					...this.resolveCount(decisionCount),
				};
			}
		}
		const singleMap = this.singleIpBans[parsed.family];
		const single = singleMap.get(parsed.canonical);
		if (single) {
			return {
				ipAddress: parsed.canonical,
				matchedEntry: parsed.canonical,
				...this.resolveCount(single.count),
			};
		}
		const rangeMap = this.rangeIpBans[parsed.family];
		for (const [canonical, range] of rangeMap.entries()) {
			if (parsed.value >= range.start && parsed.value <= range.end) {
				return {
					ipAddress: parsed.canonical,
					matchedEntry: canonical,
					...this.resolveCount(range.count),
				};
			}
		}
		return null;
	}

	ban(ip: string): void {
		if (isIpBanExempt(ip)) return;
		this.addEntry(ip, PERMANENT_BAN_METADATA);
	}

	banTemp(ip: string, ttlSeconds: number): void {
		if (isIpBanExempt(ip)) return;
		const expiresAt = Number.isFinite(ttlSeconds) && ttlSeconds > 0 ? new Date(Date.now() + ttlSeconds * 1000) : null;
		this.addEntry(ip, {kind: 'temporary_24h', expiresAt});
	}

	unban(ip: string): void {
		this.removeEntry(ip);
	}

	resetCaches(): void {
		const state = this.createEmptyState();
		this.singleIpBans = state.singleIpBans;
		this.rangeIpBans = state.rangeIpBans;
		this.sameIpDecisionBans = state.sameIpDecisionBans;
	}

	private createEmptyState(): IpBanCacheState {
		return {
			singleIpBans: this.createFamilyMaps(),
			rangeIpBans: this.createFamilyMaps(),
			sameIpDecisionBans: new Map(),
		};
	}

	private createFamilyMaps<T>(): FamilyMap<T> {
		return {
			ipv4: new Map(),
			ipv6: new Map(),
		};
	}

	private metadataFromEntry(entry: BannedIpEntry): IpBanMetadata {
		return {
			kind: entry.kind,
			expiresAt: entry.expiresAt,
		};
	}

	private createCount(metadata: IpBanMetadata): IpBanCount {
		return {
			permanent: metadata.kind === 'permanent' ? 1 : 0,
			temporary: metadata.kind === 'temporary_24h' ? 1 : 0,
			temporaryExpiresAt: metadata.kind === 'temporary_24h' ? metadata.expiresAt : null,
		};
	}

	private incrementCount(count: IpBanCount, metadata: IpBanMetadata): void {
		if (metadata.kind === 'permanent') {
			count.permanent += 1;
			return;
		}
		count.temporary += 1;
		if (
			metadata.expiresAt &&
			(!count.temporaryExpiresAt || metadata.expiresAt.getTime() > count.temporaryExpiresAt.getTime())
		) {
			count.temporaryExpiresAt = metadata.expiresAt;
		}
	}

	private decrementCount(count: IpBanCount): boolean {
		if (count.permanent > 0) {
			count.permanent -= 1;
		} else if (count.temporary > 0) {
			count.temporary -= 1;
		}
		return count.permanent <= 0 && count.temporary <= 0;
	}

	private resolveCount(count: IpBanCount): {
		kind: BannedIpKind;
		expiresAt: Date | null;
	} {
		if (count.permanent > 0) {
			return {kind: 'permanent', expiresAt: null};
		}
		return {kind: 'temporary_24h', expiresAt: count.temporaryExpiresAt};
	}

	private addEntry(value: string, metadata: IpBanMetadata): void {
		this.addEntryToState(value, metadata, {
			singleIpBans: this.singleIpBans,
			rangeIpBans: this.rangeIpBans,
			sameIpDecisionBans: this.sameIpDecisionBans,
		});
	}

	private addEntryToState(value: string, metadata: IpBanMetadata, state: IpBanCacheState): void {
		const parsed = parseIpBanEntry(value);
		if (!parsed) {
			Logger.warn({value}, 'Skipping invalid IP ban entry');
			return;
		}
		if (parsed.type === 'single') {
			const map = state.singleIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (existing) {
				this.incrementCount(existing.count, metadata);
			} else {
				map.set(parsed.canonical, {value: parsed.value, count: this.createCount(metadata)});
			}
			const sameIpDecisionKey = getSameIpDecisionKey(parsed.canonical);
			if (sameIpDecisionKey) {
				const decisionCount = state.sameIpDecisionBans.get(sameIpDecisionKey);
				if (decisionCount) {
					this.incrementCount(decisionCount, metadata);
				} else {
					state.sameIpDecisionBans.set(sameIpDecisionKey, this.createCount(metadata));
				}
			}
		} else {
			const map = state.rangeIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (existing) {
				this.incrementCount(existing.count, metadata);
			} else {
				map.set(parsed.canonical, {start: parsed.start, end: parsed.end, count: this.createCount(metadata)});
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
			if (this.decrementCount(existing.count)) {
				map.delete(parsed.canonical);
			}
			const sameIpDecisionKey = getSameIpDecisionKey(parsed.canonical);
			if (sameIpDecisionKey) {
				const decisionCount = this.sameIpDecisionBans.get(sameIpDecisionKey);
				if (!decisionCount) {
					return;
				}
				if (this.decrementCount(decisionCount)) {
					this.sameIpDecisionBans.delete(sameIpDecisionKey);
				}
			}
		} else {
			const map = this.rangeIpBans[parsed.family];
			const existing = map.get(parsed.canonical);
			if (!existing) return;
			if (this.decrementCount(existing.count)) {
				map.delete(parsed.canonical);
			}
		}
	}

	shutdown(): void {
		if (this.periodicRefreshTimer) {
			clearInterval(this.periodicRefreshTimer);
			this.periodicRefreshTimer = null;
		}
		if (this.kvSubscription && this.messageHandler) {
			this.kvSubscription.off('message', this.messageHandler);
		}
		if (this.kvSubscription) {
			void this.kvSubscription.disconnect();
			this.kvSubscription = null;
		}
		this.messageHandler = null;
		this.subscriberInitialized = false;
		this.isInitialized = false;
	}
}

export const ipBanCache = new IpBanCache();
export const IpBanMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const clientIp = extractClientIp(ctx.req.raw, {
		trustClientIpHeader: Config.proxy.trust_client_ip_header,
		clientIpHeaderName: Config.proxy.client_ip_header,
	});
	const match = clientIp ? ipBanCache.getMatch(clientIp) : null;
	if (match) {
		throw new IpBannedError({
			ipAddress: match.ipAddress,
			kind: match.kind,
			expiresAt: match.expiresAt,
		});
	}
	await next();
});
