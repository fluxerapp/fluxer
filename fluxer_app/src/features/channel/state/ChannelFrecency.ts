// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import {
	type ChannelFrecencyEntry,
	capChannelFrecencyHistory,
	channelFrecencyHistoryFromWire,
	channelFrecencyHistoryToWire,
	computeChannelFrecency,
	mergeChannelFrecencyWireUsage,
	rankFrequentChannelIds,
	restoreChannelFrecencyHistory,
	trackChannelUse,
} from '@app/features/channel/utils/ChannelFrecencyCalculator';
import Guilds from '@app/features/guild/state/Guilds';
import Navigation from '@app/features/navigation/state/Navigation';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFrecencyStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {compareShallow, isObservableMap, makeAutoObservable, observableShallow, reaction} from 'mobx';

const TRACKABLE_ID_PATTERN = /^\d{17,19}$/;
const FRECENCY_REFRESH_INTERVAL_MS = 3_600_000;
const FRECENCY_SYNC_DEBOUNCE_MS = 1_500;

let lastChannelId: string | null = null;
let lastGuildId: string | null = null;

function isTrackableId(id: string | null): id is string {
	return id !== null && TRACKABLE_ID_PATTERN.test(id);
}

function findDMChannelIdForRecipient(userId: string): string | null {
	for (const channel of Channels.getPrivateChannels()) {
		if (channel.type === ChannelTypes.DM && channel.recipientIds.includes(userId)) {
			return channel.id;
		}
	}
	return null;
}

function resolveFrecencyRecordId(key: string): string | null {
	return Guilds.getGuild(key)?.id ?? Channels.getChannel(key)?.id ?? findDMChannelIdForRecipient(key);
}

class ChannelFrecency {
	usageHistory = new Map<string, ChannelFrecencyEntry>();

	constructor() {
		makeAutoObservable(this, {usageHistory: observableShallow}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'channelFrecency',
			schema: ChannelFrecencyStateSchema,
			persist: ['usageHistory'],
			debounceMs: FRECENCY_SYNC_DEBOUNCE_MS,
			toMessage: (store) => ({usage: channelFrecencyHistoryToWire(store.usageHistory)}),
			applyMessage: (store, message) => {
				store.usageHistory = channelFrecencyHistoryFromWire(message.usage, Date.now());
			},
			mergeRemote: (local, incoming) => ({
				usage: mergeChannelFrecencyWireUsage(local.usage, incoming.usage, Date.now()),
			}),
		});
		this.refreshHistory();
		setInterval(this.refreshHistory, FRECENCY_REFRESH_INTERVAL_MS);
		reaction(
			() => [Navigation.guildId, Navigation.channelId] as const,
			([guildId, channelId]) => this.recordSelection(guildId, channelId),
			{equals: compareShallow, fireImmediately: true},
		);
		reaction(
			() => [MediaEngine.guildId, MediaEngine.channelId] as const,
			([guildId, channelId]) => {
				if (channelId === null && MediaEngine.localDisconnectReason === 'channelMove') return;
				this.recordSelection(guildId, channelId);
			},
			{equals: compareShallow},
		);
	}

	get frequentIds(): ReadonlyArray<string> {
		return rankFrequentChannelIds(this.usageHistory, resolveFrecencyRecordId);
	}

	getScore(id: string): number {
		return this.usageHistory.get(id)?.frecency ?? 0;
	}

	track(key: string, timestamp?: number): void {
		trackChannelUse(this.usageHistory, key, timestamp);
		capChannelFrecencyHistory(this.usageHistory);
		computeChannelFrecency(this.usageHistory, Date.now());
	}

	recordSelection(guildId: string | null, channelId: string | null): void {
		const selectedGuildId = guildId === ME ? null : guildId;
		if (channelId !== lastChannelId) {
			lastChannelId = channelId;
			if (isTrackableId(channelId)) {
				this.track(channelId);
			}
		}
		if (selectedGuildId !== lastGuildId) {
			lastGuildId = selectedGuildId;
			if (isTrackableId(selectedGuildId)) {
				this.track(selectedGuildId);
			}
		}
	}

	private refreshHistory(): void {
		const current: unknown = this.usageHistory;
		if (!isObservableMap(current)) return;
		this.usageHistory = restoreChannelFrecencyHistory(current.entries(), Date.now());
	}
}

export default new ChannelFrecency();
