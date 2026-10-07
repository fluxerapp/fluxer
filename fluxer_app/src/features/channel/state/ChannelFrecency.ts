// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
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
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {isSyncExcludedChannelId} from '@app/features/threads/utils/SyncedPreferenceGuard';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFrecencyStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {compareShallow, isObservableMap, makeAutoObservable, observableShallow, reaction} from 'mobx';

const TRACKABLE_ID_PATTERN = /^\d{17,19}$/;
const FRECENCY_REFRESH_INTERVAL_MS = 3_600_000;
const FRECENCY_SYNC_DEBOUNCE_MS = 1_500;
const LOCAL_STORAGE_KEY = 'ChannelFrecencyLocal';
const THREAD_GUILDS_STORAGE_KEY = 'ChannelFrecencyThreadGuilds';

let lastChannelId: string | null = null;
let lastGuildId: string | null = null;
let localPersistence: Promise<void> | null = null;
let deferredSelections: Array<{guildId: string | null; channelId: string; timestamp: number}> = [];
let rememberedThreadGuildIds: ReadonlySet<string> = loadRememberedThreadGuildIds();

function loadRememberedThreadGuildIds(): ReadonlySet<string> {
	const stored = AppStorage.getJSON<unknown>(THREAD_GUILDS_STORAGE_KEY);
	return new Set(Array.isArray(stored) ? stored.filter((id): id is string => typeof id === 'string') : []);
}

function rememberThreadGuildIds(guildIds: ReadonlyArray<string>): void {
	if (guildIds.length === rememberedThreadGuildIds.size && guildIds.every((id) => rememberedThreadGuildIds.has(id))) {
		return;
	}
	rememberedThreadGuildIds = new Set(guildIds);
	if (guildIds.length === 0) AppStorage.removeItem(THREAD_GUILDS_STORAGE_KEY);
	else AppStorage.setJSON(THREAD_GUILDS_STORAGE_KEY, guildIds);
}

function isTrackableId(id: string | null): id is string {
	return id !== null && TRACKABLE_ID_PATTERN.test(id);
}

function isChannelTypePending(guildId: string | null, channelId: string | null): boolean {
	return (
		guildId !== null &&
		guildId !== ME &&
		isTrackableId(channelId) &&
		(Initialization.hasCompletedInitialLoad ? ThreadGuilds.isActive(guildId) : rememberedThreadGuildIds.has(guildId)) &&
		Channels.getChannel(channelId) === undefined
	);
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
	useLog = new Map<string, ChannelFrecencyEntry>();
	localUseLog = new Map<string, ChannelFrecencyEntry>();

	constructor() {
		makeAutoObservable(this, {useLog: observableShallow, localUseLog: observableShallow}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		if (AppStorage.getItem(LOCAL_STORAGE_KEY) != null) this.persistLocal();
		await makeSyncedField(this, {
			field: 'channelFrecency',
			schema: ChannelFrecencyStateSchema,
			persist: ['useLog'],
			debounceMs: FRECENCY_SYNC_DEBOUNCE_MS,
			toMessage: (store) => ({usage: channelFrecencyHistoryToWire(store.useLog)}),
			applyMessage: (store, message) => {
				store.useLog = channelFrecencyHistoryFromWire(message.usage, Date.now());
			},
			mergeRemote: (local, incoming) => ({
				usage: mergeChannelFrecencyWireUsage(local.usage, incoming.usage, Date.now()),
			}),
		});
		await localPersistence;
		this.refreshHistory();
		setInterval(this.refreshHistory, FRECENCY_REFRESH_INTERVAL_MS);
		reaction(
			() =>
				[
					Navigation.guildId,
					Navigation.channelId,
					isChannelTypePending(Navigation.guildId, Navigation.channelId),
				] as const,
			([guildId, channelId]) => this.recordSelection(guildId, channelId),
			{equals: compareShallow, fireImmediately: true},
		);
		reaction(
			() => Initialization.hasCompletedInitialLoad,
			(loaded) => {
				if (loaded) this.flushDeferredSelections();
			},
		);
		reaction(
			() => (Initialization.hasCompletedInitialLoad ? ThreadGuilds.guildIds : null),
			(guildIds) => {
				if (guildIds !== null) rememberThreadGuildIds(guildIds);
			},
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
		const history = this.localUseLog.size === 0 ? this.useLog : new Map([...this.useLog, ...this.localUseLog]);
		return rankFrequentChannelIds(history, resolveFrecencyRecordId);
	}

	scoreFor(id: string): number {
		return this.useLog.get(id)?.heat ?? this.localUseLog.get(id)?.heat ?? 0;
	}

	recordUse(key: string, timestamp?: number): void {
		const local = isSyncExcludedChannelId(key);
		if (local) this.persistLocal();
		const history = local ? this.localUseLog : this.useLog;
		trackChannelUse(history, key, timestamp);
		capChannelFrecencyHistory(history);
		computeChannelFrecency(history, Date.now());
	}

	recordSelection(guildId: string | null, channelId: string | null): void {
		const selectedGuildId = guildId === ME ? null : guildId;
		if (Initialization.hasCompletedInitialLoad) this.flushDeferredSelections();
		if (channelId !== lastChannelId) {
			if (!isChannelTypePending(guildId, channelId)) {
				lastChannelId = channelId;
				if (isTrackableId(channelId)) {
					this.recordUse(channelId);
				}
			} else if (!Initialization.hasCompletedInitialLoad && channelId !== null) {
				lastChannelId = channelId;
				deferredSelections.push({guildId, channelId, timestamp: Date.now()});
			}
		}
		if (selectedGuildId !== lastGuildId) {
			lastGuildId = selectedGuildId;
			if (isTrackableId(selectedGuildId)) {
				this.recordUse(selectedGuildId);
			}
		}
	}

	handleLogout(): void {
		if (deferredSelections.at(-1)?.channelId === lastChannelId) lastChannelId = null;
		deferredSelections = [];
	}

	private flushDeferredSelections(): void {
		if (deferredSelections.length === 0) return;
		const selections = deferredSelections;
		deferredSelections = [];
		const last = selections.at(-1);
		for (const selection of selections) {
			if (Channels.getChannel(selection.channelId) !== undefined) {
				this.recordUse(selection.channelId, selection.timestamp);
			} else if (
				selection === last &&
				selection.channelId === lastChannelId &&
				isChannelTypePending(selection.guildId, selection.channelId)
			) {
				lastChannelId = null;
			}
		}
	}

	private persistLocal(): void {
		localPersistence ??= makePersistent(this, LOCAL_STORAGE_KEY, ['localUseLog']);
	}

	private refreshHistory(): void {
		const now = Date.now();
		const current: unknown = this.useLog;
		if (isObservableMap(current)) this.useLog = restoreChannelFrecencyHistory(current.entries(), now);
		const local: unknown = this.localUseLog;
		if (isObservableMap(local)) this.localUseLog = restoreChannelFrecencyHistory(local.entries(), now);
	}
}

export default new ChannelFrecency();
