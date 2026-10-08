// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel} from '@app/features/channel/models/Channel';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {observable, runInAction} from 'mobx';
import {describe, expect, it, vi} from 'vitest';

const GUILD = '1500000000000000000';
const CONTROL_GUILD = '1600000000000000000';
const channels = observable.map<string, Channel>();
const navigation = observable({guildId: null as string | null, channelId: null as string | null});
const mediaEngine = observable({guildId: null as string | null, channelId: null as string | null});
const initialization = observable({hasCompletedInitialLoad: true});
const activeGuildIds = observable.set<string>([GUILD]);

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => undefined, cacheUsers: () => {}}}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => channels.get(id), getPrivateChannels: () => []},
}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => undefined}}));
vi.mock('@app/features/threads/state/ThreadGuilds', () => ({
	default: {
		isActive: (id: string | null) => id !== null && activeGuildIds.has(id),
		get guildIds() {
			return Array.from(activeGuildIds);
		},
	},
}));
vi.mock('@app/features/app/state/Initialization', () => ({default: initialization}));
vi.mock('@app/features/navigation/state/Navigation', () => ({default: navigation}));
vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({default: mediaEngine}));
vi.mock('@app/features/user/state/SyncedField', () => ({makeSyncedField: async () => {}}));
vi.mock('@app/features/platform/utils/MobXPersistence', () => ({makePersistent: async () => {}}));

const {default: ChannelFrecency} = await import('@app/features/channel/state/ChannelFrecency');
await new Promise((resolve) => setTimeout(resolve, 0));

const TEXT = '1500000000000000001';
const THREAD = '1500000000000000010';

function load(id: string, type: number, parentId?: string, guildId = GUILD): void {
	runInAction(() => channels.set(id, new Channel({id, guild_id: guildId, type, parent_id: parentId})));
}

function select(channelId: string, guildId = GUILD): void {
	runInAction(() => {
		navigation.guildId = guildId;
		navigation.channelId = channelId;
	});
}

describe('channel frecency selection', () => {
	it('never records a thread id selected before its channel loads', () => {
		select(THREAD);
		expect(ChannelFrecency.useLog.has(THREAD)).toBe(false);
		load(TEXT, ChannelTypes.GUILD_TEXT);
		load(THREAD, ChannelTypes.PUBLIC_THREAD, TEXT);
		expect(ChannelFrecency.useLog.has(THREAD)).toBe(false);
		expect(ChannelFrecency.localUseLog.has(THREAD)).toBe(true);
		expect(ChannelFrecency.scoreFor(THREAD)).toBeGreaterThan(0);
		expect(ChannelFrecency.frequentIds).toContain(THREAD);
		expect(ChannelFrecency.useLog.has(GUILD)).toBe(true);
		expect(ChannelFrecency.localUseLog.has(GUILD)).toBe(false);
	});

	it('records a guild channel once it loads after selection', () => {
		runInAction(() => channels.delete(TEXT));
		select(TEXT);
		expect(ChannelFrecency.useLog.has(TEXT)).toBe(false);
		load(TEXT, ChannelTypes.GUILD_TEXT);
		expect(ChannelFrecency.useLog.has(TEXT)).toBe(true);
	});

	it('records an unknown channel immediately outside thread guilds', () => {
		const unknown = '1600000000000000001';
		select(unknown, CONTROL_GUILD);
		expect(ChannelFrecency.useLog.has(unknown)).toBe(true);
	});

	it('defers an unknown channel selected before READY in a remembered thread guild', () => {
		const lateGuild = '1700000000000000000';
		const lateParent = '1700000000000000001';
		const lateThread = '1700000000000000010';
		runInAction(() => {
			activeGuildIds.add(lateGuild);
		});
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(lateThread, lateGuild);
		expect(ChannelFrecency.useLog.has(lateThread)).toBe(false);
		runInAction(() => {
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.has(lateThread)).toBe(false);
		load(lateParent, ChannelTypes.GUILD_TEXT, undefined, lateGuild);
		load(lateThread, ChannelTypes.PUBLIC_THREAD, lateParent, lateGuild);
		expect(ChannelFrecency.useLog.has(lateThread)).toBe(false);
		expect(ChannelFrecency.localUseLog.has(lateThread)).toBe(true);
	});

	it('records a control channel selected before READY synchronously', () => {
		const controlChannel = '1600000000000000002';
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(controlChannel, CONTROL_GUILD);
		expect(ChannelFrecency.useLog.get(controlChannel)?.hitCount).toBe(1);
		runInAction(() => {
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.get(controlChannel)?.hitCount).toBe(1);
	});

	it('records a deferred selection once READY leaves its guild without threads', () => {
		const formerGuild = '1800000000000000000';
		const channel = '1800000000000000001';
		runInAction(() => {
			activeGuildIds.add(formerGuild);
		});
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(channel, formerGuild);
		expect(ChannelFrecency.useLog.has(channel)).toBe(false);
		load(channel, ChannelTypes.GUILD_TEXT, undefined, formerGuild);
		runInAction(() => {
			activeGuildIds.delete(formerGuild);
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.get(channel)?.hitCount).toBe(1);
	});

	it('drops a deferred selection that stays unknown once READY leaves its guild without threads', () => {
		const killedGuild = '1900000000000000000';
		const purgedThread = '1900000000000000010';
		runInAction(() => {
			activeGuildIds.add(killedGuild);
		});
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(purgedThread, killedGuild);
		runInAction(() => {
			activeGuildIds.delete(killedGuild);
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.has(purgedThread)).toBe(false);
		expect(ChannelFrecency.localUseLog.has(purgedThread)).toBe(false);
	});

	it('records an unknown control channel selected before READY synchronously', () => {
		const controlChannel = '1600000000000000003';
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(controlChannel, CONTROL_GUILD);
		expect(ChannelFrecency.useLog.get(controlChannel)?.hitCount).toBe(1);
		runInAction(() => {
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.get(controlChannel)?.hitCount).toBe(1);
	});

	it('drops deferred selections on logout', () => {
		const queued = '1500000000000000021';
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(queued);
		expect(ChannelFrecency.useLog.has(queued)).toBe(false);
		ChannelFrecency.handleLogout();
		select(TEXT);
		runInAction(() => {
			activeGuildIds.delete(GUILD);
			initialization.hasCompletedInitialLoad = true;
		});
		expect(ChannelFrecency.useLog.has(queued)).toBe(false);
		runInAction(() => {
			activeGuildIds.add(GUILD);
		});
	});

	it('records the channel queued before logout when the next session selects it', () => {
		const queued = '1500000000000000022';
		runInAction(() => {
			initialization.hasCompletedInitialLoad = false;
		});
		select(queued);
		ChannelFrecency.handleLogout();
		runInAction(() => {
			initialization.hasCompletedInitialLoad = true;
		});
		load(queued, ChannelTypes.GUILD_TEXT);
		select(queued);
		expect(ChannelFrecency.useLog.get(queued)?.hitCount).toBe(1);
	});
});
