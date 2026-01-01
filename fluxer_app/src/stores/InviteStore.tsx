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

import {action, makeAutoObservable, runInAction} from 'mobx';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import type {Invite} from '~/records/MessageRecord';
import {isGuildInvite, isPackInvite} from '~/types/InviteTypes';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

interface InviteState {
	loading: boolean;
	error: Error | null;
	data: Invite | null;
}

function upsertInviteByCode(list: Array<Invite>, invite: Invite): Array<Invite> {
	const idx = list.findIndex((i) => i.code === invite.code);
	if (idx === -1) return [...list, invite];
	const next = list.slice();
	next[idx] = invite;
	return next;
}

function mergeInvitesByCode(existing: Array<Invite>, incoming: Array<Invite>): Array<Invite> {
	const map = new Map<string, Invite>();
	for (const i of existing) map.set(i.code, i);
	for (const i of incoming) map.set(i.code, i);
	return Array.from(map.values());
}

class InviteStore {
	invites: Map<string, InviteState> = new Map();
	pendingRequests: Map<string, Promise<Invite>> = new Map();
	channelInvites: Map<string, Array<Invite>> = new Map();
	channelInvitesFetchStatus: Map<string, FetchStatus> = new Map();
	guildInvites: Map<string, Array<Invite>> = new Map();
	guildInvitesFetchStatus: Map<string, FetchStatus> = new Map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getInvite(code: string): InviteState | null {
		return this.invites.get(code) || null;
	}

	getInvites(): Map<string, InviteState> {
		return this.invites;
	}

	getChannelInvites(channelId: string): Array<Invite> | null {
		return this.channelInvites.get(channelId) || null;
	}

	getChannelInvitesFetchStatus(channelId: string): FetchStatus {
		return this.channelInvitesFetchStatus.get(channelId) || 'idle';
	}

	getGuildInvites(guildId: string): Array<Invite> | null {
		return this.guildInvites.get(guildId) || null;
	}

	getGuildInvitesFetchStatus(guildId: string): FetchStatus {
		return this.guildInvitesFetchStatus.get(guildId) || 'idle';
	}

	fetchInvite = action(async (code: string): Promise<Invite> => {
		const existingRequest = this.pendingRequests.get(code);
		if (existingRequest) {
			return existingRequest;
		}
		const existingInvite = this.invites.get(code);
		if (existingInvite?.data) {
			return existingInvite.data;
		}

		runInAction(() => {
			this.invites = new Map(this.invites).set(code, {loading: true, error: null, data: null});
		});

		const promise = InviteActionCreators.fetch(code);

		runInAction(() => {
			this.pendingRequests = new Map(this.pendingRequests).set(code, promise);
		});

		try {
			const invite = await promise;
			runInAction(() => {
				const newPendingRequests = new Map(this.pendingRequests);
				newPendingRequests.delete(code);
				this.invites = new Map(this.invites).set(code, {loading: false, error: null, data: invite});
				this.pendingRequests = newPendingRequests;
			});
			return invite;
		} catch (error) {
			runInAction(() => {
				const newPendingRequests = new Map(this.pendingRequests);
				newPendingRequests.delete(code);
				this.invites = new Map(this.invites).set(code, {loading: false, error: error as Error, data: null});
				this.pendingRequests = newPendingRequests;
			});
			throw error;
		}
	});

	handleChannelInvitesFetchPending = action((channelId: string): void => {
		this.channelInvitesFetchStatus = new Map(this.channelInvitesFetchStatus).set(channelId, 'pending');
	});

	handleChannelInvitesFetchSuccess = action((channelId: string, invites: Array<Invite>): void => {
		const existing = this.channelInvites.get(channelId) ?? [];
		const merged = mergeInvitesByCode(existing, invites);
		this.channelInvites = new Map(this.channelInvites).set(channelId, merged);
		this.channelInvitesFetchStatus = new Map(this.channelInvitesFetchStatus).set(channelId, 'success');
	});

	handleChannelInvitesFetchError = action((channelId: string): void => {
		this.channelInvitesFetchStatus = new Map(this.channelInvitesFetchStatus).set(channelId, 'error');
	});

	handleGuildInvitesFetchPending = action((guildId: string): void => {
		this.guildInvitesFetchStatus = new Map(this.guildInvitesFetchStatus).set(guildId, 'pending');
	});

	handleGuildInvitesFetchSuccess = action((guildId: string, invites: Array<Invite>): void => {
		const existing = this.guildInvites.get(guildId) ?? [];
		const merged = mergeInvitesByCode(existing, invites);
		this.guildInvites = new Map(this.guildInvites).set(guildId, merged);
		this.guildInvitesFetchStatus = new Map(this.guildInvitesFetchStatus).set(guildId, 'success');
	});

	handleGuildInvitesFetchError = action((guildId: string): void => {
		this.guildInvitesFetchStatus = new Map(this.guildInvitesFetchStatus).set(guildId, 'error');
	});

	handleInviteCreate = action((invite: Invite): void => {
		if (!isPackInvite(invite)) {
			const newChannelInvites = new Map(this.channelInvites);
			const channelId = invite.channel.id;
			const existingChannelInvites = newChannelInvites.get(channelId) ?? [];
			newChannelInvites.set(channelId, upsertInviteByCode(existingChannelInvites, invite));

			const newChannelStatus = new Map(this.channelInvitesFetchStatus);
			newChannelStatus.set(channelId, 'success');

			this.channelInvites = newChannelInvites;
			this.channelInvitesFetchStatus = newChannelStatus;
		}

		if (isGuildInvite(invite)) {
			const newGuildInvites = new Map(this.guildInvites);
			const existingGuildInvites = newGuildInvites.get(invite.guild.id) ?? [];
			newGuildInvites.set(invite.guild.id, upsertInviteByCode(existingGuildInvites, invite));

			const newGuildStatus = new Map(this.guildInvitesFetchStatus);
			newGuildStatus.set(invite.guild.id, 'success');

			this.guildInvites = newGuildInvites;
			this.guildInvitesFetchStatus = newGuildStatus;
		}

		const newInvites = new Map(this.invites);
		newInvites.set(invite.code, {loading: false, error: null, data: invite});
		this.invites = newInvites;
	});

	handleInviteDelete = action((inviteCode: string): void => {
		const newChannelInvites = new Map(this.channelInvites);
		for (const [channelId, invites] of newChannelInvites) {
			newChannelInvites.set(
				channelId,
				invites.filter((invite) => invite.code !== inviteCode),
			);
		}

		const newGuildInvites = new Map(this.guildInvites);
		for (const [guildId, invites] of newGuildInvites) {
			newGuildInvites.set(
				guildId,
				invites.filter((invite) => invite.code !== inviteCode),
			);
		}

		const newInvites = new Map(this.invites);
		newInvites.delete(inviteCode);

		this.invites = newInvites;
		this.channelInvites = newChannelInvites;
		this.guildInvites = newGuildInvites;
	});

	handleChannelDelete = action((channelId: string): void => {
		const newChannelInvites = new Map(this.channelInvites);
		newChannelInvites.delete(channelId);

		const newChannelInvitesFetchStatus = new Map(this.channelInvitesFetchStatus);
		newChannelInvitesFetchStatus.delete(channelId);

		this.channelInvites = newChannelInvites;
		this.channelInvitesFetchStatus = newChannelInvitesFetchStatus;
	});

	handleGuildDelete = action((guildId: string): void => {
		const newGuildInvites = new Map(this.guildInvites);
		newGuildInvites.delete(guildId);

		const newGuildInvitesFetchStatus = new Map(this.guildInvitesFetchStatus);
		newGuildInvitesFetchStatus.delete(guildId);

		this.guildInvites = newGuildInvites;
		this.guildInvitesFetchStatus = newGuildInvitesFetchStatus;
	});
}

export default new InviteStore();
