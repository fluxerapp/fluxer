// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {makeAutoObservable, runInAction} from 'mobx';

const MEMBER_SUBSCRIPTION_MAX_SIZE = 100;
const MEMBER_SUBSCRIPTION_TTL_MS = 5 * 60 * 1000;
const MEMBER_SUBSCRIPTION_SYNC_DEBOUNCE_MS = 500;
const MEMBER_SUBSCRIPTION_PRUNE_INTERVAL_MS = 30 * 1000;

interface LRUEntry {
	userId: string;
	lastAccessed: number;
}

const MEMBER_UNSUBSCRIBE_DEBOUNCE_MS = 1000;

class MemberPresenceSubscription {
	private subscriptions: Map<string, Map<string, number>> = new Map();
	private activeGuildId: string | null = null;
	private syncTimeoutId: number | null = null;
	private pruneIntervalId: number | null = null;
	private pendingSyncGuilds: Set<string> = new Set();
	private unsubscribeTimeouts: Map<string, number> = new Map();
	private subscriptionRefs: Map<string, number> = new Map();
	subscriptionVersion = 0;

	constructor() {
		makeAutoObservable<
			this,
			| 'subscriptions'
			| 'syncTimeoutId'
			| 'pruneIntervalId'
			| 'pendingSyncGuilds'
			| 'unsubscribeTimeouts'
			| 'subscriptionRefs'
		>(
			this,
			{
				subscriptions: false,
				syncTimeoutId: false,
				pruneIntervalId: false,
				pendingSyncGuilds: false,
				unsubscribeTimeouts: false,
				subscriptionRefs: false,
			},
			{autoBind: true},
		);
	}

	private unsubscribeKey(guildId: string, userId: string): string {
		return `${guildId}:${userId}`;
	}

	private cancelPendingUnsubscribe(guildId: string, userId: string): void {
		const key = this.unsubscribeKey(guildId, userId);
		const pending = this.unsubscribeTimeouts.get(key);
		if (pending != null) {
			clearTimeout(pending);
			this.unsubscribeTimeouts.delete(key);
		}
	}

	private doUnsubscribe(guildId: string, userId: string): void {
		const key = this.unsubscribeKey(guildId, userId);
		if ((this.subscriptionRefs.get(key) ?? 0) > 0) return;
		const guildMembers = this.subscriptions.get(guildId);
		if (!guildMembers) return;
		guildMembers.delete(userId);
		if (guildMembers.size === 0) {
			this.subscriptions.delete(guildId);
		}
		this.syncPruneInterval();
		this.bumpVersion();
		this.syncToGatewayImmediate(guildId);
	}

	private startPruneInterval(): void {
		if (this.pruneIntervalId != null) {
			return;
		}
		this.pruneIntervalId = window.setInterval(() => {
			this.pruneExpiredEntries();
		}, MEMBER_SUBSCRIPTION_PRUNE_INTERVAL_MS);
	}

	private stopPruneInterval(): void {
		if (this.pruneIntervalId == null) {
			return;
		}
		window.clearInterval(this.pruneIntervalId);
		this.pruneIntervalId = null;
	}

	private syncPruneInterval(): void {
		if (this.subscriptions.size > 0) {
			this.startPruneInterval();
			return;
		}
		this.stopPruneInterval();
	}

	private getGuildSubscriptions(guildId: string): Map<string, number> {
		let guildSubs = this.subscriptions.get(guildId);
		if (!guildSubs) {
			guildSubs = new Map();
			this.subscriptions.set(guildId, guildSubs);
		}
		return guildSubs;
	}

	touchMember(guildId: string, userId: string): void {
		const key = this.unsubscribeKey(guildId, userId);
		this.subscriptionRefs.set(key, (this.subscriptionRefs.get(key) ?? 0) + 1);
		this.cancelPendingUnsubscribe(guildId, userId);
		const guildSubs = this.getGuildSubscriptions(guildId);
		const now = Date.now();
		if (guildSubs.has(userId)) {
			guildSubs.delete(userId);
		}
		guildSubs.set(userId, now);
		this.enforceMaxSize(guildId);
		this.syncPruneInterval();
		this.scheduleSyncToGateway(guildId);
		this.bumpVersion();
	}

	unsubscribe(guildId: string, userId: string): void {
		const key = this.unsubscribeKey(guildId, userId);
		const isSubscribed = this.subscriptions.get(guildId)?.has(userId) ?? false;
		const refs = (this.subscriptionRefs.get(key) ?? (isSubscribed ? 1 : 0)) - 1;
		if (refs > 0) {
			this.subscriptionRefs.set(key, refs);
			return;
		}
		this.subscriptionRefs.delete(key);
		const pending = this.unsubscribeTimeouts.get(key);
		if (pending != null) clearTimeout(pending);
		if (!isSubscribed) {
			this.unsubscribeTimeouts.delete(key);
			return;
		}
		const timeoutId = window.setTimeout(() => {
			this.unsubscribeTimeouts.delete(key);
			this.doUnsubscribe(guildId, userId);
		}, MEMBER_UNSUBSCRIBE_DEBOUNCE_MS);
		this.unsubscribeTimeouts.set(key, timeoutId);
	}

	setActiveGuild(guildId: string): void {
		if (this.activeGuildId === guildId) {
			return;
		}
		const previous = this.activeGuildId;
		this.activeGuildId = guildId;
		if (previous) {
			this.syncActiveFlagImmediate(previous, false);
		}
		this.syncActiveFlagImmediate(guildId, true);
		this.scheduleSyncToGateway(guildId);
		this.bumpVersion();
	}

	getSubscribedMembers(guildId: string): Array<string> {
		const guildSubs = this.subscriptions.get(guildId);
		if (!guildSubs) {
			return [];
		}
		return Array.from(guildSubs.keys());
	}

	clearGuild(guildId: string): void {
		for (const [key, timeoutId] of this.unsubscribeTimeouts) {
			if (key.startsWith(`${guildId}:`)) {
				clearTimeout(timeoutId);
				this.unsubscribeTimeouts.delete(key);
			}
		}
		for (const key of this.subscriptionRefs.keys()) {
			if (key.startsWith(`${guildId}:`)) {
				this.subscriptionRefs.delete(key);
			}
		}
		const hadSubscriptions = this.subscriptions.has(guildId);
		const wasActiveGuild = this.activeGuildId === guildId;
		this.subscriptions.delete(guildId);
		this.syncPruneInterval();
		if (hadSubscriptions) {
			this.syncToGatewayImmediate(guildId);
			this.bumpVersion();
		}
		if (wasActiveGuild) {
			this.activeGuildId = null;
			this.syncActiveFlagImmediate(guildId, false);
		}
	}

	clearAll(): void {
		for (const timeoutId of this.unsubscribeTimeouts.values()) {
			clearTimeout(timeoutId);
		}
		this.unsubscribeTimeouts.clear();
		this.subscriptionRefs.clear();
		const socket = GatewayConnection.socket;
		const guildIds = Array.from(this.subscriptions.keys());
		const previousActive = this.activeGuildId;
		if (socket) {
			for (const guildId of guildIds) {
				socket.updateGuildSubscriptions({
					subscriptions: {
						[guildId]: {members: []},
					},
				});
			}
			if (previousActive) {
				socket.updateGuildSubscriptions({
					subscriptions: {
						[previousActive]: {active: false},
					},
				});
			}
		}
		this.subscriptions.clear();
		this.activeGuildId = null;
		this.stopPruneInterval();
		if (this.syncTimeoutId != null) {
			clearTimeout(this.syncTimeoutId);
			this.syncTimeoutId = null;
		}
		this.pendingSyncGuilds.clear();
		this.bumpVersion();
	}

	private bumpVersion(): void {
		runInAction(() => {
			this.subscriptionVersion++;
		});
	}

	private enforceMaxSize(guildId: string): void {
		const guildSubs = this.subscriptions.get(guildId);
		if (!guildSubs || guildSubs.size <= MEMBER_SUBSCRIPTION_MAX_SIZE) {
			return;
		}
		const entries: Array<LRUEntry> = [];
		for (const [userId, lastAccessed] of guildSubs) {
			entries.push({userId, lastAccessed});
		}
		entries.sort((a, b) => a.lastAccessed - b.lastAccessed);
		const toRemove = entries.slice(0, entries.length - MEMBER_SUBSCRIPTION_MAX_SIZE);
		for (const entry of toRemove) {
			guildSubs.delete(entry.userId);
		}
	}

	private pruneExpiredEntries(): void {
		const now = Date.now();
		const cutoff = now - MEMBER_SUBSCRIPTION_TTL_MS;
		const guildsToSync = new Set<string>();
		for (const [guildId, guildSubs] of this.subscriptions) {
			const toRemove: Array<string> = [];
			for (const [userId, lastAccessed] of guildSubs) {
				if (lastAccessed < cutoff) {
					toRemove.push(userId);
				}
			}
			for (const userId of toRemove) {
				if ((this.subscriptionRefs.get(this.unsubscribeKey(guildId, userId)) ?? 0) > 0) continue;
				guildSubs.delete(userId);
				guildsToSync.add(guildId);
			}
			if (guildSubs.size === 0) {
				this.subscriptions.delete(guildId);
			}
		}
		this.syncPruneInterval();
		if (guildsToSync.size > 0) {
			for (const guildId of guildsToSync) {
				this.scheduleSyncToGateway(guildId);
			}
			this.bumpVersion();
		}
	}

	private scheduleSyncToGateway(guildId: string): void {
		this.pendingSyncGuilds.add(guildId);
		if (this.syncTimeoutId != null) {
			return;
		}
		this.syncTimeoutId = window.setTimeout(() => {
			this.syncTimeoutId = null;
			this.flushPendingSyncs();
		}, MEMBER_SUBSCRIPTION_SYNC_DEBOUNCE_MS);
	}

	private flushPendingSyncs(): void {
		const guildsToSync = Array.from(this.pendingSyncGuilds);
		this.pendingSyncGuilds.clear();
		for (const guildId of guildsToSync) {
			this.syncToGatewayImmediate(guildId);
		}
	}

	private syncActiveFlagImmediate(guildId: string, active: boolean): void {
		const socket = GatewayConnection.socket;
		if (!socket) return;
		socket.updateGuildSubscriptions({
			subscriptions: {
				[guildId]: {active, sync: active ? true : undefined},
			},
		});
	}

	private syncToGatewayImmediate(guildId: string): void {
		const socket = GatewayConnection.socket;
		if (!socket) {
			return;
		}
		const guildSubs = this.subscriptions.get(guildId);
		const members = guildSubs ? Array.from(guildSubs.keys()).sort() : [];
		socket.updateGuildSubscriptions({
			subscriptions: {
				[guildId]: {members},
			},
		});
	}
}

export default new MemberPresenceSubscription();
