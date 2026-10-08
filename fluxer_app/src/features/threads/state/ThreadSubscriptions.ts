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

	constructor() {
		makeAutoObservable<this, 'sent' | 'lastGuildId'>(this, {sent: false, lastGuildId: false}, {autoBind: true});
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
				{equals: compareStructural, fireImmediately: true},
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
		if (!desired.ready || !desired.guildId || !desired.active) return;
		const memberLists = desired.memberLists.join(',');
		const previous = this.sent.get(desired.guildId);
		if (previous?.threads && previous.memberLists === memberLists) return;
		const socket = GatewayConnection.socket;
		if (!socket?.isConnected()) return;
		socket.updateGuildSubscriptions({
			subscriptions: {
				[desired.guildId]: {
					threads: true,
					thread_member_lists: [...desired.memberLists],
				},
			},
		});
		this.sent.set(desired.guildId, {threads: true, memberLists});
	}

	private handleActiveGuildsChanged(activeGuildIds: ReadonlyArray<string>): void {
		const active = new Set(activeGuildIds);
		const socket = GatewayConnection.socket;
		for (const guildId of Array.from(this.sent.keys())) {
			if (active.has(guildId)) continue;
			this.sent.delete(guildId);
			if (socket?.isConnected()) {
				socket.updateGuildSubscriptions({
					subscriptions: {[guildId]: {threads: false, thread_member_lists: []}},
				});
			}
		}
	}
}

export default new ThreadSubscriptions();
