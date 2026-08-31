// SPDX-License-Identifier: AGPL-3.0-or-later

import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {isGuildInvite} from '@app/features/invite/types/InviteTypes';
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
				invites: computed,
				channelInvites: computed,
				guildInvites: computed,
			},
			{autoBind: true},
		);
	}

	get invites(): Map<string, InviteSlot> {
		const at = Date.now();
		const visible = new Map<string, InviteSlot>();
		for (const [code, slot] of this.inviteSlots) {
			if (slot.data && expired(slot.data, at)) continue;
			visible.set(code, slot);
		}
		return visible;
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

	getInvite(code: string): InviteSlot | null {
		return this.invites.get(code) ?? null;
	}

	getInvites(): Map<string, InviteSlot> {
		return this.invites;
	}

	getChannelInvites(channelId: string): Array<Invite> | null {
		return this.channelInvites.get(channelId) ?? null;
	}

	getChannelInvitesFetchStatus(channelId: string): FetchStatus {
		return this.channelFetchStatus.get(channelId) ?? 'idle';
	}

	getGuildInvites(guildId: string): Array<Invite> | null {
		return this.guildInvites.get(guildId) ?? null;
	}

	getGuildInvitesFetchStatus(guildId: string): FetchStatus {
		return this.guildFetchStatus.get(guildId) ?? 'idle';
	}

	private dropTimer(code: string): void {
		const t = this.expiryTimers.get(code);
		if (t === undefined) return;
		clearTimeout(t);
		const next = new Map(this.expiryTimers);
		next.delete(code);
		this.expiryTimers = next;
	}

	private armTimer(invite: Invite): void {
		this.dropTimer(invite.code);
		const epoch = expiryEpoch(invite);
		if (epoch === null) return;
		const remaining = epoch - Date.now();
		if (remaining <= 0) {
			this.handleInviteDelete(invite.code);
			return;
		}
		const wait = remaining > SETTIMEOUT_LIMIT_MS ? SETTIMEOUT_LIMIT_MS : remaining;
		const handle = setTimeout(() => {
			runInAction(() => {
				const refreshed = this.lookupInvite(invite.code);
				if (refreshed === null) {
					this.dropTimer(invite.code);
					return;
				}
				this.armTimer(refreshed);
			});
		}, wait);
		this.expiryTimers = new Map(this.expiryTimers).set(invite.code, handle);
	}

	private lookupInvite(code: string): Invite | null {
		const slot = this.inviteSlots.get(code);
		if (slot?.data) return slot.data;
		for (const list of this.channelInviteCache.values()) {
			for (const invite of list) {
				if (invite.code === code) return invite;
			}
		}
		for (const list of this.guildInviteCache.values()) {
			for (const invite of list) {
				if (invite.code === code) return invite;
			}
		}
		return null;
	}

	private filterAlive(invite: Invite): Invite | null {
		if (expired(invite, Date.now())) {
			this.handleInviteDelete(invite.code);
			return null;
		}
		this.armTimer(invite);
		return invite;
	}

	private filterAliveAll(invites: ReadonlyArray<Invite>): Array<Invite> {
		const alive: Array<Invite> = [];
		for (const invite of invites) {
			const kept = this.filterAlive(invite);
			if (kept !== null) alive.push(kept);
		}
		return alive;
	}

	fetchInvite = action(async (code: string): Promise<Invite> => {
		const inflight = this.pendingRequests.get(code);
		if (inflight) return inflight;
		const cached = this.getInvite(code);
		if (cached?.data) return cached.data;
		runInAction(() => {
			this.inviteSlots = new Map(this.inviteSlots).set(code, {loading: true, error: null, data: null});
		});
		const promise = InviteCommands.fetch(code);
		runInAction(() => {
			this.pendingRequests = new Map(this.pendingRequests).set(code, promise);
		});
		try {
			const fetched = await promise;
			runInAction(() => {
				const nextPending = new Map(this.pendingRequests);
				nextPending.delete(code);
				const alive = this.filterAlive(fetched);
				this.inviteSlots = new Map(this.inviteSlots).set(code, {
					loading: false,
					error: null,
					data: alive,
				});
				this.pendingRequests = nextPending;
			});
			if (!this.getInvite(code)?.data) {
				throw new Error(`Invite ${code} expired before it could be cached`);
			}
			return fetched;
		} catch (error) {
			runInAction(() => {
				const nextPending = new Map(this.pendingRequests);
				nextPending.delete(code);
				this.inviteSlots = new Map(this.inviteSlots).set(code, {
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
	handleChannelInvitesFetchSuccess = action((channelId: string, invites: Array<Invite>): void => {
		const merged = unionByCode(this.channelInviteCache.get(channelId) ?? [], invites);
		const alive = this.filterAliveAll(merged);
		this.channelInviteCache = new Map(this.channelInviteCache).set(channelId, alive);
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'success');
	});
	handleChannelInvitesFetchError = action((channelId: string): void => {
		this.channelFetchStatus = new Map(this.channelFetchStatus).set(channelId, 'error');
	});
	handleGuildInvitesFetchPending = action((guildId: string): void => {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'pending');
	});
	handleGuildInvitesFetchSuccess = action((guildId: string, invites: Array<Invite>): void => {
		const merged = unionByCode(this.guildInviteCache.get(guildId) ?? [], invites);
		const alive = this.filterAliveAll(merged);
		this.guildInviteCache = new Map(this.guildInviteCache).set(guildId, alive);
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'success');
	});
	handleGuildInvitesFetchError = action((guildId: string): void => {
		this.guildFetchStatus = new Map(this.guildFetchStatus).set(guildId, 'error');
	});
	handleInviteCreate = action((invite: Invite): void => {
		const alive = this.filterAlive(invite);
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
		this.inviteSlots = new Map(this.inviteSlots).set(alive.code, {loading: false, error: null, data: alive});
	});
	handleInviteDelete = action((inviteCode: string): void => {
		this.dropTimer(inviteCode);
		const removeFromList = (list: Array<Invite>): Array<Invite> => list.filter((i) => i.code !== inviteCode);
		const nextChannel = new Map<string, Array<Invite>>();
		for (const [channelId, list] of this.channelInviteCache) nextChannel.set(channelId, removeFromList(list));
		const nextGuild = new Map<string, Array<Invite>>();
		for (const [guildId, list] of this.guildInviteCache) nextGuild.set(guildId, removeFromList(list));
		const nextSlots = new Map(this.inviteSlots);
		nextSlots.delete(inviteCode);
		this.inviteSlots = nextSlots;
		this.channelInviteCache = nextChannel;
		this.guildInviteCache = nextGuild;
	});
	handleChannelDelete = action((channelId: string): void => {
		for (const invite of this.channelInviteCache.get(channelId) ?? []) this.dropTimer(invite.code);
		const nextCache = new Map(this.channelInviteCache);
		nextCache.delete(channelId);
		const nextStatus = new Map(this.channelFetchStatus);
		nextStatus.delete(channelId);
		this.channelInviteCache = nextCache;
		this.channelFetchStatus = nextStatus;
	});
	handleGuildDelete = action((guildId: string): void => {
		for (const invite of this.guildInviteCache.get(guildId) ?? []) this.dropTimer(invite.code);
		const nextCache = new Map(this.guildInviteCache);
		nextCache.delete(guildId);
		const nextStatus = new Map(this.guildFetchStatus);
		nextStatus.delete(guildId);
		this.guildInviteCache = nextCache;
		this.guildFetchStatus = nextStatus;
	});
}

export default new Invites();
