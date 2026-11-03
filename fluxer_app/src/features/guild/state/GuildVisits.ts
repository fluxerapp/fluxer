// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import Guilds from '@app/features/guild/state/Guilds';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {deferUntilModulesLoaded} from '@app/features/platform/utils/DeferUntilModulesLoaded';
import {compareStructural, reaction} from 'mobx';

interface VisitFocus {
	readonly guildId: string;
	readonly channelId: string;
	readonly sessionId: string;
}

function resolveVisitFocus(): VisitFocus | null {
	const guildId = SelectedGuild.selectedGuildId;
	const sessionId = GatewaySessions.foregroundConnection.sessionId;
	if (
		guildId == null ||
		sessionId == null ||
		!GatewaySessions.foregroundConnection.isReady ||
		!(Guilds.getGuild(guildId)?.visiting === true)
	) {
		return null;
	}
	const channelId = SelectedChannel.currentChannelId;
	return channelId == null ? null : {guildId, channelId, sessionId};
}

class GuildVisits {
	private readonly endedByUser = new Set<string>();
	private focusedVisitChannelId: string | null = null;
	private focusedVisitGuildId: string | null = null;

	constructor() {
		deferUntilModulesLoaded(() => {
			reaction(resolveVisitFocus, (focus) => this.handleFocus(focus), {equals: compareStructural});
		});
	}

	private handleFocus(focus: VisitFocus | null): void {
		const previous = this.focusedVisitChannelId;
		const previousGuildId = this.focusedVisitGuildId;
		this.focusedVisitChannelId = focus?.channelId ?? null;
		this.focusedVisitGuildId = focus?.guildId ?? null;
		if (previous !== null && previous !== this.focusedVisitChannelId) {
			Messages.forgetUnfocusedVisitChannel(previous);
		}
		if (previousGuildId !== null && previousGuildId !== this.focusedVisitGuildId) {
			this.sendUnfocus(previousGuildId);
		}
		this.sendFocus(focus);
	}

	markEndedByUser(guildId: string): void {
		this.endedByUser.add(guildId);
	}

	consumeEndedByUser(guildId: string): boolean {
		return this.endedByUser.delete(guildId);
	}

	private sendUnfocus(guildId: string): void {
		if (Guilds.getGuild(guildId)?.visiting !== true) return;
		GatewaySessions.foregroundConnection.socket?.updateGuildSubscriptions({
			subscriptions: {[guildId]: {focused_channel_id: null}},
		});
	}

	private sendFocus(focus: VisitFocus | null): void {
		if (focus === null) return;
		GatewaySessions.foregroundConnection.socket?.updateGuildSubscriptions({
			subscriptions: {[focus.guildId]: {active: true, focused_channel_id: focus.channelId}},
		});
	}

	reset(): void {
		this.endedByUser.clear();
	}
}

const guildVisits = new GuildVisits();

ResettableStates.register(guildVisits, () => guildVisits.reset());

export default guildVisits;
