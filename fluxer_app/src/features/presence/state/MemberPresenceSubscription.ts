// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable, runInAction} from 'mobx';

const MEMBER_SUBSCRIPTION_MAX_SIZE = 100;
const MEMBER_SUBSCRIPTION_TTL_MS = 5 * 60 * 1000;
const MEMBER_SUBSCRIPTION_SYNC_DEBOUNCE_MS = 500;
const MEMBER_SUBSCRIPTION_PRUNE_INTERVAL_MS = 30 * 1000;

interface LRUEntry {
	userId: string;
	lastAccessed: number;
}

class MemberPresenceSubscription {
	private subscriptions: Map<string, Map<string, number>> = new Map();
	private activeGuildId: string | null = null;
	private syncTimeoutId: number | null = null;
	private pruneIntervalId: number | null = null;
	private pendingSyncGuilds: Set<string> = new Set();
	subscriptionVersion = 0;

	constructor() {
		makeAutoObservable<this, 'subscriptions' | 'syncTimeoutId' | 'pruneIntervalId' | 'pendingSyncGuilds'>(
			this,
			{
				subscriptions: false,
				syncTimeoutId: false,
				pruneIntervalId: false,
				pendingSyncGuilds: false,
			},
			{autoBind: true},
		);
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

	clearAll(): void {
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

const memberPresenceSubscription = new MemberPresenceSubscription();

AccountScopedWork.registerCancellation(() => memberPresenceSubscription.clearAll());

export default memberPresenceSubscription;
