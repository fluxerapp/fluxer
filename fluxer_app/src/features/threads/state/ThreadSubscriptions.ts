// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadRoster from '@app/features/threads/state/ThreadRoster';
import {compareStructural, makeAutoObservable, reaction} from 'mobx';

interface SentState {
	threads: boolean;
	memberLists: string;
}

interface Desired {
	epoch: number;
	ready: boolean;
	guildId: string | null;
	active: boolean;
	memberLists: ReadonlyArray<string>;
}

class ThreadSubscriptions {
	connectionEpoch = 0;
	armed = false;
	private sent = new Map<string, SentState>();
	private lastGuildId: string | null = null;
	private listGuilds = new Set<string>();

	constructor() {
		makeAutoObservable<this, 'sent' | 'lastGuildId' | 'listGuilds'>(
			this,
			{sent: false, lastGuildId: false, listGuilds: false},
			{autoBind: true},
		);
		deferUntilModulesLoaded(() => {
			reaction(
				(): Desired => {
					const guildId = SelectedGuild.selectedGuildId;
					const active = ThreadGuilds.isActive(guildId);
					return {
						epoch: this.connectionEpoch,
						ready: this.armed && GatewayConnection.isReady,
						guildId,
						active,
						memberLists: active && guildId ? ThreadRoster.subscribedThreadIds(guildId) : [],
					};
				},
				(desired) => this.apply(desired),
				{equals: compareStructural, fireImmediately: true, scheduler: (run) => queueMicrotask(run)},
			);
			reaction(
				() => GatewayConnection.isReady,
				(ready) => {
					if (!ready) this.handleConnectionLost();
				},
			);
			reaction(
				() => ThreadGuilds.guildIds,
				(activeGuildIds) => this.handleActiveGuildsChanged(activeGuildIds),
				{equals: compareStructural},
			);
		});
	}

	handleConnectionReady(): void {
		this.sent = new Map();
		this.armed = true;
		this.connectionEpoch += 1;
	}

	private handleConnectionLost(): void {
		this.sent = new Map();
		this.armed = false;
	}

	private apply(desired: Desired): void {
		if (desired.guildId !== this.lastGuildId) {
			if (this.lastGuildId) this.sent.delete(this.lastGuildId);
			this.lastGuildId = desired.guildId;
		}
		if (!desired.ready) return;
		const guildId = desired.active ? desired.guildId : null;
		const subscriptions: Record<string, {threads?: boolean; thread_member_lists: Array<string>}> = {};
		for (const listGuildId of this.listGuilds) {
			if (listGuildId !== guildId) subscriptions[listGuildId] = {thread_member_lists: []};
		}
		const memberLists = desired.memberLists.join(',');
		const previous = guildId ? this.sent.get(guildId) : undefined;
		if (guildId && !(previous?.threads && previous.memberLists === memberLists)) {
			subscriptions[guildId] = {threads: true, thread_member_lists: [...desired.memberLists]};
		}
		if (Object.keys(subscriptions).length === 0) return;
		const socket = GatewayConnection.socket;
		if (!socket?.isConnected()) return;
		socket.updateGuildSubscriptions({subscriptions});
		for (const clearedGuildId of Object.keys(subscriptions)) {
			if (clearedGuildId !== guildId) this.listGuilds.delete(clearedGuildId);
		}
		if (guildId && subscriptions[guildId]) {
			this.sent.set(guildId, {threads: true, memberLists});
			if (desired.memberLists.length > 0) this.listGuilds.add(guildId);
			else this.listGuilds.delete(guildId);
		}
	}

	private handleActiveGuildsChanged(activeGuildIds: ReadonlyArray<string>): void {
		const active = new Set(activeGuildIds);
		const socket = GatewayConnection.socket;
		for (const guildId of Array.from(this.sent.keys())) {
			if (active.has(guildId)) continue;
			this.sent.delete(guildId);
			if (socket?.isConnected()) {
				this.listGuilds.delete(guildId);
				socket.updateGuildSubscriptions({
					subscriptions: {[guildId]: {threads: false, thread_member_lists: []}},
				});
			}
		}
	}
}

export default new ThreadSubscriptions();
