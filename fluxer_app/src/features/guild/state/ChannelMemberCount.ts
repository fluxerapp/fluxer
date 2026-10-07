// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {makeAutoObservable} from 'mobx';

export interface ChannelMemberCounts {
	memberCount: number;
	onlineCount: number;
	fetchedAt: number;
}

interface RequestCountsOptions {
	force?: boolean;
	ttlMs?: number;
}

interface ChannelMemberCountsEntry {
	guild_id: string;
	channel_id: string;
	member_count: number;
	online_count: number;
}

const MAX_ENTRIES = 512;
const FRESH_TTL_MS = 30 * 1000;
const REQUEST_DEBOUNCE_MS = 50;

class ChannelMemberCount {
	private cache: Map<string, ChannelMemberCounts> = new Map();
	private pending: Map<string, Set<string>> = new Map();
	private flushTimer: NodeJS.Timeout | null = null;
	private cacheVersion = 0;

	constructor() {
		makeAutoObservable<ChannelMemberCount, 'cache' | 'pending' | 'flushTimer'>(
			this,
			{cache: false, pending: false, flushTimer: false},
			{autoBind: true},
		);
	}

	getCounts(guildId: string, channelId: string): ChannelMemberCounts | null {
		void this.cacheVersion;
		return this.cache.get(this.cacheKey(guildId, channelId)) ?? null;
	}

	isStale(guildId: string, channelId: string, ttlMs: number = FRESH_TTL_MS): boolean {
		const entry = this.cache.get(this.cacheKey(guildId, channelId));
		if (!entry) return true;
		return Date.now() - entry.fetchedAt >= ttlMs;
	}

	requestCounts(guildId: string, channelIds: string | ReadonlyArray<string>, options: RequestCountsOptions = {}): void {
		if (!guildId) return;
		const ids = Array.isArray(channelIds) ? channelIds : [channelIds];
		if (ids.length === 0) return;
		const force = options.force ?? false;
		const ttlMs = options.ttlMs ?? FRESH_TTL_MS;
		let pendingGuild = this.pending.get(guildId);
		for (const channelId of ids) {
			if (!channelId) continue;
			if (!force && !this.isStale(guildId, channelId, ttlMs)) continue;
			if (!pendingGuild) {
				pendingGuild = new Set();
				this.pending.set(guildId, pendingGuild);
			}
			pendingGuild.add(channelId);
		}
		if (pendingGuild && pendingGuild.size > 0) {
			this.scheduleFlush();
		}
	}

	handleCountsResponse(entries: ReadonlyArray<ChannelMemberCountsEntry>): void {
		const now = Date.now();
		let updated = false;
		for (const entry of entries) {
			const guildId = this.normalizeId(entry.guild_id);
			const channelId = this.normalizeId(entry.channel_id);
			if (!guildId || !channelId) continue;
			this.touch(guildId, channelId, {
				memberCount: typeof entry.member_count === 'number' ? entry.member_count : 0,
				onlineCount: typeof entry.online_count === 'number' ? entry.online_count : 0,
				fetchedAt: now,
			});
			this.pending.get(guildId)?.delete(channelId);
			updated = true;
		}
		this.pruneEmptyPendingGuilds();
		if (updated) {
			this.cacheVersion += 1;
		}
	}

	handleGuildDelete(guildId: string): void {
		this.pending.delete(guildId);
		let updated = false;
		for (const key of Array.from(this.cache.keys())) {
			if (!key.startsWith(`${guildId}:`)) continue;
			this.cache.delete(key);
			updated = true;
		}
		if (updated) {
			this.cacheVersion += 1;
		}
	}

	handleSessionInvalidated(): void {
		this.cache.clear();
		this.pending.clear();
		if (this.flushTimer != null) {
			clearTimeout(this.flushTimer);
			this.flushTimer = null;
		}
		this.cacheVersion += 1;
	}

	private touch(guildId: string, channelId: string, counts: ChannelMemberCounts): void {
		const key = this.cacheKey(guildId, channelId);
		if (this.cache.has(key)) {
			this.cache.delete(key);
		}
		this.cache.set(key, counts);
		while (this.cache.size > MAX_ENTRIES) {
			const oldestKey = this.cache.keys().next().value;
			if (oldestKey === undefined) break;
			this.cache.delete(oldestKey);
		}
	}

	private scheduleFlush(): void {
		if (this.flushTimer != null) return;
		this.flushTimer = setTimeout(() => {
			this.flushTimer = null;
			this.flushPending();
		}, REQUEST_DEBOUNCE_MS);
	}

	private flushPending(): void {
		if (this.pending.size === 0) return;
		const socket = GatewayConnection.socket;
		if (!socket || !GatewayConnection.isReady) {
			this.pending.clear();
			return;
		}
		for (const [guildId, channelIds] of this.pending) {
			if (channelIds.size === 0) continue;
			socket.requestChannelMemberCounts({
				guildId,
				channelIds: Array.from(channelIds),
			});
		}
		this.pending.clear();
	}

	private pruneEmptyPendingGuilds(): void {
		for (const [guildId, channelIds] of Array.from(this.pending.entries())) {
			if (channelIds.size === 0) {
				this.pending.delete(guildId);
			}
		}
	}

	private cacheKey(guildId: string, channelId: string): string {
		return `${guildId}:${channelId}`;
	}

	private normalizeId(value: string | null | undefined): string | null {
		if (typeof value === 'string' && value.length > 0) return value;
		return null;
	}
}

const channelMemberCount = new ChannelMemberCount();
ResettableStates.register(channelMemberCount, channelMemberCount.handleSessionInvalidated);
export default channelMemberCount;
