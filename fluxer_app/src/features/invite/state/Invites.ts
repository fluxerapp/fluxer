// SPDX-License-Identifier: AGPL-3.0-or-later

import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {isGuildInvite} from '@app/features/invite/types/InviteTypes';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {type InstanceHTTPTarget, instanceTargetIdentity} from '@app/features/platform/transport/InstanceHTTP';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {action, computed, makeAutoObservable, runInAction} from 'mobx';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

interface InviteSlot {
	loading: boolean;
	error: Error | null;
	data: Invite | null;
}

const SETTIMEOUT_LIMIT_MS = 0x7fff_ffff;

function expiryEpoch(invite: Invite): number | null {
	const stamp = invite.expires_at;
	if (!stamp) return null;
	const parsed = Date.parse(stamp);
	return Number.isFinite(parsed) ? parsed : null;
}

function expired(invite: Invite, at: number): boolean {
	const epoch = expiryEpoch(invite);
	return epoch !== null && epoch <= at;
}

function notYet(invite: Invite, at: number): boolean {
	return !expired(invite, at);
}

function withInvite(list: ReadonlyArray<Invite>, invite: Invite): Array<Invite> {
	const next: Array<Invite> = [];
	let replaced = false;
	for (const existing of list) {
		if (existing.code === invite.code) {
			next.push(invite);
			replaced = true;
		} else {
			next.push(existing);
		}
	}
	if (!replaced) next.push(invite);
	return next;
}

function unionByCode(a: ReadonlyArray<Invite>, b: ReadonlyArray<Invite>): Array<Invite> {
	const seen = new Map<string, Invite>();
	for (const invite of a) seen.set(invite.code, invite);
	for (const invite of b) seen.set(invite.code, invite);
	return Array.from(seen.values());
}

function inviteResourceKey(code: string, target: InstanceHTTPTarget): string {
	return `${instanceTargetIdentity(target)}\u0000${code}`;
}

class Invites {
	inviteSlots: Map<string, InviteSlot> = new Map();
	pendingRequests: Map<string, Promise<Invite>> = new Map();
	channelInviteCache: Map<string, Array<Invite>> = new Map();
	channelFetchStatus: Map<string, FetchStatus> = new Map();
	guildInviteCache: Map<string, Array<Invite>> = new Map();
	guildFetchStatus: Map<string, FetchStatus> = new Map();
	private expiryTimers: Map<string, NodeJS.Timeout> = new Map();

	constructor() {
		makeAutoObservable(
			this,
			{
				channelInvites: computed,
				guildInvites: computed,
			},
			{autoBind: true},
		);
	}

	get channelInvites(): Map<string, Array<Invite>> {
		const at = Date.now();
		const visible = new Map<string, Array<Invite>>();
		for (const [channelId, list] of this.channelInviteCache) {
			visible.set(
				channelId,
				list.filter((invite) => notYet(invite, at)),
			);
		}
		return visible;
	}

	get guildInvites(): Map<string, Array<Invite>> {
		const at = Date.now();
		const visible = new Map<string, Array<Invite>>();
		for (const [guildId, list] of this.guildInviteCache) {
			visible.set(
				guildId,
				list.filter((invite) => notYet(invite, at)),
			);
		}
		return visible;
	}

	getInvite(code: string, target: InstanceHTTPTarget): InviteSlot | null {
		const slot = this.inviteSlots.get(inviteResourceKey(code, target)) ?? null;
		return slot?.data && expired(slot.data, Date.now()) ? null : slot;
	}

	getChannelInvitesFetchStatus(channelId: string): FetchStatus {
		return this.channelFetchStatus.get(channelId) ?? 'idle';
	}

	getGuildInvitesFetchStatus(guildId: string): FetchStatus {
		return this.guildFetchStatus.get(guildId) ?? 'idle';
	}

	private dropTimer(code: string, target: InstanceHTTPTarget): void {
		const resourceKey = inviteResourceKey(code, target);
		const t = this.expiryTimers.get(resourceKey);
		if (t === undefined) return;
		clearTimeout(t);
		const next = new Map(this.expiryTimers);
		next.delete(resourceKey);
		this.expiryTimers = next;
	}

	private armTimer(invite: Invite, target: InstanceHTTPTarget): void {
		const resourceKey = inviteResourceKey(invite.code, target);
		this.dropTimer(invite.code, target);
		const epoch = expiryEpoch(invite);
		if (epoch === null) return;
		const remaining = epoch - Date.now();
		if (remaining <= 0) {
			this.handleInviteDelete(invite.code, target);
			return;
		}
		const wait = remaining > SETTIMEOUT_LIMIT_MS ? SETTIMEOUT_LIMIT_MS : remaining;
		const handle = setTimeout(() => {
			runInAction(() => {
				this.armTimer(invite, target);
			});
		}, wait);
		this.expiryTimers = new Map(this.expiryTimers).set(resourceKey, handle);
	}

	private filterAlive(invite: Invite, target: InstanceHTTPTarget): Invite | null {
		if (expired(invite, Date.now())) {
			this.handleInviteDelete(invite.code, target);
			return null;
		}
		this.armTimer(invite, target);
		return invite;
	}

	private filterAliveAll(invites: ReadonlyArray<Invite>, target: InstanceHTTPTarget): Array<Invite> {
		const alive: Array<Invite> = [];
		for (const invite of invites) {
			const kept = this.filterAlive(invite, target);
			if (kept !== null) alive.push(kept);
		}
		return alive;
	}

	reset(): void {
		for (const timer of this.expiryTimers.values()) {
			clearTimeout(timer);
		}
		this.expiryTimers = new Map();
		this.inviteSlots = new Map();
		this.pendingRequests = new Map();
		this.channelInviteCache = new Map();
		this.channelFetchStatus = new Map();
		this.guildInviteCache = new Map();
		this.guildFetchStatus = new Map();
	}

	fetchInvite = action(async (code: string, target: InstanceHTTPTarget): Promise<Invite> => {
		const resourceKey = inviteResourceKey(code, target);
		const inflight = this.pendingRequests.get(resourceKey);
		if (inflight) return inflight;
		const cached = this.getInvite(code, target);
		if (cached?.data) return cached.data;
		runInAction(() => {
			this.inviteSlots = new Map(this.inviteSlots).set(resourceKey, {loading: true, error: null, data: null});
		});
		const promise = InviteCommands.fetch(code, target);
		runInAction(() => {
			this.pendingRequests = new Map(this.pendingRequests).set(resourceKey, promise);
		});
		try {
			const fetched = await promise;
			runInAction(() => {
				const nextPending = new Map(this.pendingRequests);
				nextPending.delete(resourceKey);
				const alive = this.filterAlive(fetched, target);
				this.inviteSlots = new Map(this.inviteSlots).set(resourceKey, {
					loading: false,
					error: null,
					data: alive,
				});
				this.pendingRequests = nextPending;
			});
			if (!this.getInvite(code, target)?.data) {
				throw new Error(`Invite ${code} expired before it could be cached`);
			}
			return fetched;
		} catch (error) {
			runInAction(() => {
				const nextPending = new Map(this.pendingRequests);
				nextPending.delete(resourceKey);
				this.inviteSlots = new Map(this.inviteSlots).set(resourceKey, {
					loading: false,
					error: error as Error,
					data: null,
				});
				this.pendingRequests = nextPending;
			});
			throw error;
		}
	});
	handleChannelInvitesFetchPending = action((channelId: string): void => {
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'pending');
	});
	handleChannelInvitesFetchSuccess = action(
		(channelId: string, invites: Array<Invite>, target: InstanceHTTPTarget): void => {
			const merged = unionByCode(this.channelInviteCache.get(channelId) ?? [], invites);
			const alive = this.filterAliveAll(merged, target);
			this.channelInviteCache = new Map(this.channelInviteCache).set(channelId, alive);
			this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'success');
		},
	);
	handleChannelInvitesFetchError = action((channelId: string): void => {
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'error');
	});
	handleGuildInvitesFetchPending = action((guildId: string): void => {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'pending');
	});
	handleGuildInvitesFetchSuccess = action(
		(guildId: string, invites: Array<Invite>, target: InstanceHTTPTarget): void => {
			const merged = unionByCode(this.guildInviteCache.get(guildId) ?? [], invites);
			const alive = this.filterAliveAll(merged, target);
			this.guildInviteCache = new Map(this.guildInviteCache).set(guildId, alive);
			this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'success');
		},
	);
	handleGuildInvitesFetchError = action((guildId: string): void => {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'error');
	});
	handleInviteCreate = action((invite: Invite, target: InstanceHTTPTarget): void => {
		const alive = this.filterAlive(invite, target);
		if (alive === null) return;
		const channelId = alive.channel.id;
		this.channelInviteCache = new Map(this.channelInviteCache).set(
			channelId,
			withInvite(this.channelInviteCache.get(channelId) ?? [], alive),
		);
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'success');
		if (isGuildInvite(alive)) {
			const guildId = alive.guild.id;
			const next = new Map(this.guildInviteCache);
			next.set(guildId, withInvite(this.guildInviteCache.get(guildId) ?? [], alive));
			this.guildInviteCache = next;
			this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'success');
		}
		this.inviteSlots = new Map(this.inviteSlots).set(inviteResourceKey(alive.code, target), {
			loading: false,
			error: null,
			data: alive,
		});
	});
	handleInviteDelete = action((inviteCode: string, target: InstanceHTTPTarget): void => {
		this.dropTimer(inviteCode, target);
		const removeFromList = (list: Array<Invite>): Array<Invite> => list.filter((i) => i.code !== inviteCode);
		const nextChannel = new Map<string, Array<Invite>>();
		for (const [channelId, list] of this.channelInviteCache) nextChannel.set(channelId, removeFromList(list));
		const nextGuild = new Map<string, Array<Invite>>();
		for (const [guildId, list] of this.guildInviteCache) nextGuild.set(guildId, removeFromList(list));
		const nextSlots = new Map(this.inviteSlots);
		nextSlots.delete(inviteResourceKey(inviteCode, target));
		this.inviteSlots = nextSlots;
		this.channelInviteCache = nextChannel;
		this.guildInviteCache = nextGuild;
	});
	handleChannelDelete = action((channelId: string, target: InstanceHTTPTarget): void => {
		for (const invite of this.channelInviteCache.get(channelId) ?? []) this.dropTimer(invite.code, target);
		const nextCache = new Map(this.channelInviteCache);
		nextCache.delete(channelId);
		const nextStatus = new Map(this.channelFetchStatus);
		nextStatus.delete(channelId);
		this.channelInviteCache = nextCache;
		this.channelFetchStatus = nextStatus;
	});
	handleGuildDelete = action((guildId: string, target: InstanceHTTPTarget): void => {
		for (const invite of this.guildInviteCache.get(guildId) ?? []) this.dropTimer(invite.code, target);
		const nextCache = new Map(this.guildInviteCache);
		nextCache.delete(guildId);
		const nextStatus = new Map(this.guildFetchStatus);
		nextStatus.delete(guildId);
		this.guildInviteCache = nextCache;
		this.guildFetchStatus = nextStatus;
	});
}

const invites = new Invites();

AccountScopedWork.registerCancellation(() => invites.reset());

export default invites;
