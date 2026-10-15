// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository, type BannedIpEntry, type BannedIpKind} from '@app/api/admin/AdminRepository';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import {IP_BAN_REFRESH_CHANNEL} from '@app/api/constants/IpBan';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {createNetworkBlockError, type NetworkBlock} from '@app/api/enforcement/NetworkBlockError';
import {sharedListHas} from '@app/api/infrastructure/activity/SharedLists';
import {Logger} from '@app/api/Logger';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {readOptionalEnv} from '@app/api/utils/IntegerOptions';
import {parseIpBanEntry, tryParseSingleIp} from '@app/api/utils/IpRangeUtils';
import {RefreshedCache} from '@app/api/utils/RefreshedCache';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';
import {parseAcceptLanguage} from '@fluxer/i18n/src/locale/resolution/AcceptLanguageNegotiation';
import {getSameIpDecisionKey, type IpAddressFamily} from '@fluxer/ip_utils/src/IpAddress';
import {createMiddleware} from 'hono/factory';

type FamilyMap<T> = Record<IpAddressFamily, Map<string, T>>;

interface IpBanDecision {
	decisionId: bigint;
	reasonCode: number | null;
}

interface IpBanMetadata {
	kind: BannedIpKind;
	expiresAt: Date | null;
	decision: IpBanDecision | null;
}

interface IpBanCount {
	permanent: number;
	temporary: number;
	temporaryExpiresAt: Date | null;
	decision: IpBanDecision | null;
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
	decision: IpBanDecision | null;
}

interface IpBanCacheState {
	singleIpBans: FamilyMap<SingleCacheEntry>;
	rangeIpBans: FamilyMap<RangeCacheEntry>;
	sameIpDecisionBans: Map<string, IpBanCount>;
}

const PERMANENT_BAN_METADATA: IpBanMetadata = {
	kind: 'permanent',
	expiresAt: null,
	decision: null,
};

const INSTANCE_DISCOVERY_PATH = '/.well-known/fluxer';

class IpBanCache extends RefreshedCache {
	private singleIpBans: FamilyMap<SingleCacheEntry>;
	private rangeIpBans: FamilyMap<RangeCacheEntry>;
	private sameIpDecisionBans: Map<string, IpBanCount>;
	private adminRepository = new AdminRepository();
	private decisionRepository = new EnforcementDecisionRepository();
	constructor() {
		super({
			name: 'IP ban cache',
			channels: [IP_BAN_REFRESH_CHANNEL],
			periodicIntervalMs: () => {
				const intervalMs = Number(readOptionalEnv('FLUXER_IP_BAN_REFRESH_INTERVAL_MS') ?? '300000');
				return Number.isFinite(intervalMs) && intervalMs > 0 ? intervalMs : null;
			},
			onPeriodicRefreshError: (error) =>
				Logger.warn(
					{error: error instanceof Error ? error.message : String(error)},
					'Periodic IP ban cache refresh failed',
				),
		});
		const state = this.createEmptyState();
		this.singleIpBans = state.singleIpBans;
		this.rangeIpBans = state.rangeIpBans;
		this.sameIpDecisionBans = state.sameIpDecisionBans;
	}

	protected async load(): Promise<void> {
		const entries = await this.adminRepository.loadAllBannedIpEntries();
		const decisionIds = [...new Set(entries.flatMap((entry) => (entry.decisionId === null ? [] : [entry.decisionId])))];
		const decisions = decisionIds.length > 0 ? await this.decisionRepository.getMany(decisionIds) : [];
		const reasonCodes = new Map(decisions.map((decision) => [decision.decision_id, decision.reason_code]));
		const state = this.createEmptyState();
		for (const entry of entries) {
			this.addEntryToState(entry.ip, this.metadataFromEntry(entry, reasonCodes), state);
		}
		this.singleIpBans = state.singleIpBans;
		this.rangeIpBans = state.rangeIpBans;
		this.sameIpDecisionBans = state.sameIpDecisionBans;
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
			if (decisionCount && this.isActive(decisionCount)) {
				return {
					ipAddress: parsed.canonical,
					matchedEntry: sameIpDecisionKey,
					...this.resolveCount(decisionCount),
				};
			}
		}
		const singleMap = this.singleIpBans[parsed.family];
		const single = singleMap.get(parsed.canonical);
		if (single && this.isActive(single.count)) {
			return {
				ipAddress: parsed.canonical,
				matchedEntry: parsed.canonical,
				...this.resolveCount(single.count),
			};
		}
		const rangeMap = this.rangeIpBans[parsed.family];
		for (const [canonical, range] of rangeMap.entries()) {
			if (parsed.value >= range.start && parsed.value <= range.end && this.isActive(range.count)) {
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
		this.addEntry(ip, {kind: 'temporary_24h', expiresAt, decision: null});
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

	private metadataFromEntry(entry: BannedIpEntry, reasonCodes: ReadonlyMap<bigint, number>): IpBanMetadata {
		return {
			kind: entry.kind,
			expiresAt: entry.expiresAt,
			decision:
				entry.decisionId === null
					? null
					: {decisionId: entry.decisionId, reasonCode: reasonCodes.get(entry.decisionId) ?? null},
		};
	}

	private createCount(metadata: IpBanMetadata): IpBanCount {
		return {
			permanent: metadata.kind === 'permanent' ? 1 : 0,
			temporary: metadata.kind === 'temporary_24h' ? 1 : 0,
			temporaryExpiresAt: metadata.kind === 'temporary_24h' ? metadata.expiresAt : null,
			decision: metadata.decision,
		};
	}

	private incrementCount(count: IpBanCount, metadata: IpBanMetadata): void {
		count.decision ??= metadata.decision;
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

	private isActive(count: IpBanCount): boolean {
		if (count.permanent > 0 || !count.temporaryExpiresAt) {
			return true;
		}
		return count.temporaryExpiresAt.getTime() > Date.now();
	}

	private resolveCount(count: IpBanCount): Pick<IpBanMatch, 'kind' | 'expiresAt' | 'decision'> {
		if (count.permanent > 0) {
			return {kind: 'permanent', expiresAt: null, decision: count.decision};
		}
		return {kind: 'temporary_24h', expiresAt: count.temporaryExpiresAt, decision: count.decision};
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
}

export const ipBanCache = new IpBanCache();

export const SIGN_IN_ROUTES: ReadonlySet<string> = new Set([
	'/auth/login',
	'/auth/login/mfa/totp',
	'/auth/login/mfa/webauthn/authentication-options',
	'/auth/login/mfa/webauthn',
	'/auth/webauthn/authentication-options',
	'/auth/webauthn/authenticate',
	'/auth/sso/start',
	'/auth/sso/complete',
]);

function findNetworkBlock(clientIp: string | null): NetworkBlock | null {
	if (!clientIp) return null;
	const match = ipBanCache.getMatch(clientIp);
	if (match) {
		return {
			ipAddress: match.ipAddress,
			kind: match.kind,
			expiresAt: match.expiresAt,
			reasonCode: match.decision?.reasonCode ?? null,
		};
	}
	if (!isIpBanExempt(clientIp) && sharedListHas('ip_blocked', clientIp)) {
		return {ipAddress: clientIp, kind: 'permanent', expiresAt: null, reasonCode: null};
	}
	return null;
}

export const IpBanMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	if (ctx.req.path === INSTANCE_DISCOVERY_PATH) {
		await next();
		return;
	}
	const block = findNetworkBlock(getRequestClientIp(ctx));
	if (block) {
		if (ctx.req.method !== 'POST' || !SIGN_IN_ROUTES.has(stripApiPrefix(ctx.req.path))) {
			throw createNetworkBlockError(block, parseAcceptLanguage(ctx.req.header('accept-language')));
		}
		ctx.set('blockedNetworkSignIn', block);
	}
	await next();
});
