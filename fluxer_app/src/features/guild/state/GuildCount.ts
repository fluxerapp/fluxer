// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {makeAutoObservable, observable} from 'mobx';

export interface GuildCounts {
	memberCount: number;
	onlineCount: number;
	fetchedAt: number;
}

interface HydratableGuild {
	id: string;
	member_count?: number;
	online_count?: number;
	unavailable?: boolean;
}

const MAX_ENTRIES = 256;
const FRESH_TTL_MS = 60 * 1000;
const REQUEST_DEBOUNCE_MS = 100;

class GuildCount {
	private readonly cache = observable.map<string, GuildCounts>(undefined, {deep: false});
	private pending: Set<string> = new Set();
	private flushTimer: NodeJS.Timeout | null = null;

	constructor() {
		makeAutoObservable<GuildCount, 'cache' | 'pending' | 'flushTimer'>(
			this,
			{cache: false, pending: false, flushTimer: false},
			{autoBind: true},
		);
	}

	getCounts(guildId: string): GuildCounts | null {
		return this.cache.get(guildId) ?? null;
	}

	isStale(guildId: string, ttlMs: number = FRESH_TTL_MS): boolean {
		const entry = this.cache.get(guildId);
		if (!entry) return true;
		return Date.now() - entry.fetchedAt >= ttlMs;
	}

	requestCounts(
		guildId: string,
		options: {
			force?: boolean;
			ttlMs?: number;
		} = {},
	): void {
		if (!guildId) return;
		const force = options.force ?? false;
		const ttlMs = options.ttlMs ?? FRESH_TTL_MS;
		if (this.pending.has(guildId)) return;
		if (!force && !this.isStale(guildId, ttlMs)) return;
		this.pending.add(guildId);
		this.scheduleFlush();
	}

	handleConnectionOpen(guilds: ReadonlyArray<HydratableGuild>): void {
		this.hydrate(guilds);
	}

	handleGuildCreate(guild: HydratableGuild): void {
		this.hydrate([guild]);
	}

	handleCountsResponse(
		entries: ReadonlyArray<{
			guild_id: string;
			member_count: number;
			online_count: number;
		}>,
	): void {
		const now = Date.now();
		for (const entry of entries) {
			if (!entry || typeof entry.guild_id !== 'string') continue;
			this.touch(entry.guild_id, {
				memberCount: typeof entry.member_count === 'number' ? entry.member_count : 0,
				onlineCount: typeof entry.online_count === 'number' ? entry.online_count : 0,
				fetchedAt: now,
			});
			this.pending.delete(entry.guild_id);
		}
	}

	handleGuildDelete(guildId: string): void {
		this.cache.delete(guildId);
		this.pending.delete(guildId);
	}

	handleSessionInvalidated(): void {
		this.cache.clear();
		this.pending.clear();
		if (this.flushTimer != null) {
			clearTimeout(this.flushTimer);
			this.flushTimer = null;
		}
	}

	private hydrate(guilds: ReadonlyArray<HydratableGuild>): void {
		const now = Date.now();
		for (const guild of guilds) {
			if (!guild || typeof guild.id !== 'string') continue;
			if (guild.unavailable) continue;
			if (typeof guild.member_count !== 'number' || typeof guild.online_count !== 'number') continue;
			this.touch(guild.id, {
				memberCount: guild.member_count,
				onlineCount: guild.online_count,
				fetchedAt: now,
			});
		}
	}

	private touch(guildId: string, counts: GuildCounts): void {
		if (this.cache.has(guildId)) {
			this.cache.delete(guildId);
		}
		this.cache.set(guildId, counts);
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
		const guildIds = Array.from(this.pending);
		const socket = GatewayConnection.socket;
		if (!socket) {
			this.pending.clear();
			return;
		}
		socket.requestGuildCounts(guildIds);
	}
}

export default new GuildCount();
