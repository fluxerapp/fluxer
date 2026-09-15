// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {autorun, runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

interface FakeChannel {
	readonly id: string;
	readonly type: number;
	readonly recipientIds: ReadonlyArray<string>;
}

type ChannelFrecencyStore = typeof import('@app/features/channel/state/ChannelFrecency')['default'];

interface SyncedFieldFake {
	seed: Record<string, {totalUses: number; recentUsesMs: Array<bigint>}> | null;
	toMessage: (() => {usage: Record<string, {totalUses: number; recentUsesMs: Array<bigint>}>}) | null;
}

const fakes = await vi.hoisted(async () => {
	const {observable} = await import('mobx');
	return {
		navigation: observable({guildId: null as string | null, channelId: null as string | null}),
		voice: observable({
			guildId: null as string | null,
			channelId: null as string | null,
			localDisconnectReason: null as string | null,
		}),
		guildIds: new Set<string>(),
		channels: new Map<string, FakeChannel>(),
		synced: {seed: null, toMessage: null} as SyncedFieldFake,
	};
});

vi.mock('@app/features/user/state/SyncedField', () => ({
	makeSyncedField: async (
		store: object,
		config: {
			toMessage: (store: never) => {usage: Record<string, {totalUses: number; recentUsesMs: Array<bigint>}>};
			applyMessage: (store: never, message: {usage: Record<string, unknown>}) => void;
		},
	) => {
		fakes.synced.toMessage = () => config.toMessage(store as never);
		if (fakes.synced.seed !== null) {
			config.applyMessage(store as never, {usage: fakes.synced.seed});
		}
	},
}));

vi.mock('@app/features/navigation/state/Navigation', () => ({default: fakes.navigation}));
vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({default: fakes.voice}));
vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {getGuild: (guildId: string) => (fakes.guildIds.has(guildId) ? {id: guildId} : undefined)},
}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {
		getChannel: (channelId: string) => fakes.channels.get(channelId),
		getPrivateChannels: () => Array.from(fakes.channels.values()),
	},
}));

const HOUR_MS = 3_600_000;
const DAY_MS = 24 * HOUR_MS;
const T0 = Date.UTC(2026, 6, 15, 12);
const GUILD_ID = '1400000000000000100';
const OTHER_GUILD_ID = '1400000000000000200';
const TEXT_CHANNEL_ID = '1400000000000000101';
const OTHER_TEXT_CHANNEL_ID = '1400000000000000102';
const VOICE_CHANNEL_ID = '1400000000000000103';
const OTHER_VOICE_CHANNEL_ID = '1400000000000000104';
const OTHER_GUILD_VOICE_CHANNEL_ID = '1400000000000000201';
const DM_CHANNEL_ID = '1400000000000000301';
const FRIEND_ID = '1400000000000000401';

async function startChannelFrecency(
	synced?: ReadonlyArray<readonly [string, {totalUses: number; recentUses: ReadonlyArray<number>}]>,
): Promise<ChannelFrecencyStore> {
	vi.resetModules();
	fakes.synced.seed =
		synced === undefined
			? null
			: Object.fromEntries(
					synced.map(([key, entry]) => [
						key,
						{totalUses: entry.totalUses, recentUsesMs: entry.recentUses.map((use) => BigInt(use))},
					]),
				);
	const {default: ChannelFrecency} = await import('@app/features/channel/state/ChannelFrecency');
	await vi.advanceTimersByTimeAsync(0);
	return ChannelFrecency;
}

function navigate(guildId: string | null, channelId: string | null): void {
	runInAction(() => {
		fakes.navigation.guildId = guildId;
		fakes.navigation.channelId = channelId;
	});
}

function setVoiceChannel(guildId: string | null, channelId: string | null): void {
	runInAction(() => {
		fakes.voice.localDisconnectReason = channelId == null ? 'user' : null;
		fakes.voice.guildId = guildId;
		fakes.voice.channelId = channelId;
	});
}

function moveVoiceChannel(guildId: string | null, channelId: string): void {
	runInAction(() => {
		fakes.voice.localDisconnectReason = 'channelMove';
		fakes.voice.guildId = null;
		fakes.voice.channelId = null;
	});
	setVoiceChannel(guildId, channelId);
}

function totalUses(store: ChannelFrecencyStore, key: string): number {
	return store.usageHistory.get(key)?.totalUses ?? 0;
}

function keyFor(index: number): string {
	return `1400000000000000${String(index).padStart(3, '0')}`;
}

beforeEach(() => {
	vi.useFakeTimers();
	vi.setSystemTime(T0);
	navigate(null, null);
	setVoiceChannel(null, null);
	runInAction(() => {
		fakes.voice.localDisconnectReason = null;
	});
	fakes.guildIds.clear();
	fakes.channels.clear();
});

afterEach(() => {
	vi.useRealTimers();
});

describe('ChannelFrecency tracking', () => {
	it('counts the landing channel and its guild once when tracking starts', async () => {
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		const store = await startChannelFrecency();
		expect(totalUses(store, TEXT_CHANNEL_ID)).toBe(1);
		expect(totalUses(store, GUILD_ID)).toBe(1);
		expect(store.getScore(TEXT_CHANNEL_ID)).toBe(100);
	});

	it('counts a channel again after a page without a channel but not a guild the user stays in', async () => {
		const store = await startChannelFrecency();
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		navigate(GUILD_ID, OTHER_TEXT_CHANNEL_ID);
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		expect(totalUses(store, TEXT_CHANNEL_ID)).toBe(2);
		expect(totalUses(store, GUILD_ID)).toBe(1);
		navigate(null, null);
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		expect(totalUses(store, TEXT_CHANNEL_ID)).toBe(3);
		expect(totalUses(store, GUILD_ID)).toBe(2);
	});

	it('treats direct messages as guildless and never stores ids that are not snowflakes', async () => {
		const store = await startChannelFrecency();
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		navigate('@me', DM_CHANNEL_ID);
		navigate(GUILD_ID, 'settings');
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		expect([...store.usageHistory.keys()]).toEqual([TEXT_CHANNEL_ID, GUILD_ID, DM_CHANNEL_ID]);
		expect(totalUses(store, TEXT_CHANNEL_ID)).toBe(2);
		expect(totalUses(store, GUILD_ID)).toBe(2);
		expect(totalUses(store, DM_CHANNEL_ID)).toBe(1);
	});

	it('counts voice joins through the same last selection as navigation', async () => {
		const store = await startChannelFrecency();
		navigate(GUILD_ID, VOICE_CHANNEL_ID);
		setVoiceChannel(GUILD_ID, VOICE_CHANNEL_ID);
		expect(totalUses(store, VOICE_CHANNEL_ID)).toBe(1);
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		setVoiceChannel(GUILD_ID, OTHER_VOICE_CHANNEL_ID);
		navigate(GUILD_ID, OTHER_VOICE_CHANNEL_ID);
		expect(totalUses(store, OTHER_VOICE_CHANNEL_ID)).toBe(1);
		setVoiceChannel(null, null);
		setVoiceChannel(GUILD_ID, OTHER_VOICE_CHANNEL_ID);
		expect(totalUses(store, OTHER_VOICE_CHANNEL_ID)).toBe(2);
		setVoiceChannel(OTHER_GUILD_ID, OTHER_GUILD_VOICE_CHANNEL_ID);
		expect(totalUses(store, OTHER_GUILD_VOICE_CHANNEL_ID)).toBe(1);
		expect(totalUses(store, OTHER_GUILD_ID)).toBe(1);
		expect(totalUses(store, GUILD_ID)).toBe(2);
	});

	it('counts a move between voice channels in one guild as a single guild use', async () => {
		const store = await startChannelFrecency();
		setVoiceChannel(GUILD_ID, VOICE_CHANNEL_ID);
		moveVoiceChannel(GUILD_ID, OTHER_VOICE_CHANNEL_ID);
		expect(totalUses(store, VOICE_CHANNEL_ID)).toBe(1);
		expect(totalUses(store, OTHER_VOICE_CHANNEL_ID)).toBe(1);
		expect(totalUses(store, GUILD_ID)).toBe(1);
	});

	it('keeps at most 100 keys and drops the one used longest ago', async () => {
		const store = await startChannelFrecency();
		for (let index = 0; index <= 100; index++) {
			store.track(keyFor(index), T0 + index);
		}
		expect(store.usageHistory.size).toBe(100);
		expect(store.usageHistory.has(keyFor(0))).toBe(false);
		expect(store.usageHistory.has(keyFor(100))).toBe(true);
	});
});

describe('ChannelFrecency account sync', () => {
	it('re-derives persisted frecency against the current time on start', async () => {
		const store = await startChannelFrecency([
			[TEXT_CHANNEL_ID, {totalUses: 5, recentUses: [T0 - 8 * DAY_MS - HOUR_MS, T0 - 8 * DAY_MS]}],
			[GUILD_ID, {totalUses: 1, recentUses: []}],
		]);
		expect(store.getScore(TEXT_CHANNEL_ID)).toBe(50);
		expect(store.usageHistory.has(GUILD_ID)).toBe(false);
	});

	it('re-derives frecency again as a long session ages', async () => {
		const store = await startChannelFrecency([[TEXT_CHANNEL_ID, {totalUses: 3, recentUses: [T0 - DAY_MS]}]]);
		expect(store.getScore(TEXT_CHANNEL_ID)).toBe(210);
		await vi.advanceTimersByTimeAsync(6 * DAY_MS);
		expect(store.getScore(TEXT_CHANNEL_ID)).toBe(30);
	});

	it('hands the account sync only the uses, not the derived scores', async () => {
		await startChannelFrecency();
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		await vi.advanceTimersByTimeAsync(1_000);
		expect(fakes.synced.toMessage?.()).toEqual({
			usage: {
				[TEXT_CHANNEL_ID]: {totalUses: 1, recentUsesMs: [BigInt(T0)]},
				[GUILD_ID]: {totalUses: 1, recentUsesMs: [BigInt(T0)]},
			},
		});
	});
});

describe('ChannelFrecency ranking', () => {
	it('ranks resolvable guild, channel and direct message keys by frecency', async () => {
		fakes.guildIds.add(GUILD_ID);
		fakes.channels.set(TEXT_CHANNEL_ID, {id: TEXT_CHANNEL_ID, type: ChannelTypes.GUILD_TEXT, recipientIds: []});
		fakes.channels.set(DM_CHANNEL_ID, {id: DM_CHANNEL_ID, type: ChannelTypes.DM, recipientIds: [FRIEND_ID]});
		const store = await startChannelFrecency();
		store.track(GUILD_ID);
		store.track(FRIEND_ID);
		store.track(OTHER_TEXT_CHANNEL_ID);
		store.track(TEXT_CHANNEL_ID);
		store.track(TEXT_CHANNEL_ID);
		expect(store.frequentIds).toEqual([TEXT_CHANNEL_ID, GUILD_ID, DM_CHANNEL_ID]);
		expect(store.getScore(TEXT_CHANNEL_ID)).toBe(200);
		expect(store.getScore(DM_CHANNEL_ID)).toBe(0);
	});

	it('lets observers follow the ranking as the user moves around', async () => {
		fakes.guildIds.add(GUILD_ID);
		fakes.channels.set(TEXT_CHANNEL_ID, {id: TEXT_CHANNEL_ID, type: ChannelTypes.GUILD_TEXT, recipientIds: []});
		const store = await startChannelFrecency();
		const seen: Array<ReadonlyArray<string>> = [];
		const dispose = autorun(() => {
			seen.push(store.frequentIds);
		});
		navigate(GUILD_ID, TEXT_CHANNEL_ID);
		dispose();
		expect(seen[0]).toEqual([]);
		expect(seen.at(-1)).toEqual([TEXT_CHANNEL_ID, GUILD_ID]);
	});
});
