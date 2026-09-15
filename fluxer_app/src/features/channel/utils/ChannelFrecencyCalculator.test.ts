// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelFrecencyEntry,
	type ChannelFrecencyHistory,
	capChannelFrecencyHistory,
	channelFrecencyDayDiff,
	channelFrecencyHistoryFromWire,
	channelFrecencyHistoryToWire,
	channelFrecencyWeight,
	computeChannelFrecency,
	mergeChannelFrecencyWireUsage,
	rankFrequentChannelIds,
	restoreChannelFrecencyHistory,
	trackChannelUse,
} from '@app/features/channel/utils/ChannelFrecencyCalculator';
import {afterEach, describe, expect, it, vi} from 'vitest';

const HOUR_MS = 3_600_000;
const DAY_MS = 24 * HOUR_MS;
const NOW = Date.UTC(2026, 6, 15, 12);
const CHANNEL_ID = '1400000000000000001';
const STOCKHOLM_SUMMER_TIME_START = Date.UTC(2026, 2, 29, 1);
const STOCKHOLM_SUMMER_TIME_END = Date.UTC(2026, 9, 25, 1);

function useStockholmClock(): void {
	vi.spyOn(Date.prototype, 'getTimezoneOffset').mockImplementation(function (this: Date) {
		const time = this.getTime();
		return time >= STOCKHOLM_SUMMER_TIME_START && time < STOCKHOLM_SUMMER_TIME_END ? -120 : -60;
	});
}

function computedEntry(frecency: number): ChannelFrecencyEntry {
	return {totalUses: 1, recentUses: [NOW], frecency, score: frecency};
}

function keyFor(index: number): string {
	return `1400000000000000${String(index).padStart(3, '0')}`;
}

afterEach(() => {
	vi.restoreAllMocks();
	vi.useRealTimers();
});

describe('channelFrecencyWeight', () => {
	it.each([
		[0, 100],
		[1, 70],
		[2, 50],
		[3, 50],
		[4, 30],
		[6, 30],
		[7, 10],
		[365, 10],
		[-1, 1],
		[-3, 1],
	])('weights an age of %i days as %i', (dayDiff, weight) => {
		expect(channelFrecencyWeight(dayDiff)).toBe(weight);
	});
});

describe('channelFrecencyDayDiff', () => {
	it('counts whole elapsed days and truncates toward zero', () => {
		expect(channelFrecencyDayDiff(NOW, NOW - 23 * HOUR_MS)).toBe(0);
		expect(channelFrecencyDayDiff(NOW, NOW - 25 * HOUR_MS)).toBe(1);
		expect(channelFrecencyDayDiff(NOW, NOW - 6 * DAY_MS - 23 * HOUR_MS)).toBe(6);
		expect(channelFrecencyDayDiff(NOW, NOW + 12 * HOUR_MS)).toBe(0);
		expect(channelFrecencyDayDiff(NOW, NOW + 36 * HOUR_MS)).toBe(-1);
	});

	it('counts noon to noon across the spring clock change as one day although only 23 hours pass', () => {
		useStockholmClock();
		const saturdayNoon = Date.UTC(2026, 2, 28, 11);
		const sundayNoon = Date.UTC(2026, 2, 29, 10);
		expect(sundayNoon - saturdayNoon).toBe(23 * HOUR_MS);
		expect(channelFrecencyDayDiff(sundayNoon, saturdayNoon)).toBe(1);
		expect(channelFrecencyWeight(channelFrecencyDayDiff(sundayNoon, saturdayNoon))).toBe(70);
	});

	it('keeps 24.5 hours across the autumn clock change inside the same day', () => {
		useStockholmClock();
		const saturdayNoon = Date.UTC(2026, 9, 24, 10);
		const sundayHalfPastEleven = Date.UTC(2026, 9, 25, 10, 30);
		expect(sundayHalfPastEleven - saturdayNoon).toBe(24.5 * HOUR_MS);
		expect(channelFrecencyDayDiff(sundayHalfPastEleven, saturdayNoon)).toBe(0);
	});
});

describe('trackChannelUse', () => {
	it('starts a new key with one uncomputed use', () => {
		const history: ChannelFrecencyHistory = new Map();
		trackChannelUse(history, CHANNEL_ID, NOW);
		expect(history.get(CHANNEL_ID)).toEqual({totalUses: 1, recentUses: [NOW], frecency: -1, score: 0});
	});

	it('keeps arrival order for untimed uses and sorts when a timestamp is given', () => {
		vi.useFakeTimers();
		const history: ChannelFrecencyHistory = new Map();
		vi.setSystemTime(NOW);
		trackChannelUse(history, CHANNEL_ID);
		vi.setSystemTime(NOW - HOUR_MS);
		trackChannelUse(history, CHANNEL_ID);
		expect(history.get(CHANNEL_ID)?.recentUses).toEqual([NOW, NOW - HOUR_MS]);
		trackChannelUse(history, CHANNEL_ID, NOW - 2 * HOUR_MS);
		expect(history.get(CHANNEL_ID)?.recentUses).toEqual([NOW - 2 * HOUR_MS, NOW - HOUR_MS, NOW]);
		expect(history.get(CHANNEL_ID)?.totalUses).toBe(3);
	});
});

describe('computeChannelFrecency', () => {
	it('scores 12 uses whose last 10 are 4 today, 3 yesterday and 3 five days ago as 840', () => {
		const history: ChannelFrecencyHistory = new Map();
		const uses = [
			NOW - 20 * DAY_MS,
			NOW - 20 * DAY_MS,
			NOW - 5 * DAY_MS - HOUR_MS,
			NOW - 5 * DAY_MS - 2 * HOUR_MS,
			NOW - 5 * DAY_MS - 3 * HOUR_MS,
			NOW - DAY_MS - HOUR_MS,
			NOW - DAY_MS - 2 * HOUR_MS,
			NOW - DAY_MS - 3 * HOUR_MS,
			NOW - HOUR_MS,
			NOW - 2 * HOUR_MS,
			NOW - 3 * HOUR_MS,
			NOW - 4 * HOUR_MS,
		];
		for (const use of uses) {
			trackChannelUse(history, CHANNEL_ID, use);
		}
		computeChannelFrecency(history, NOW);
		expect(history.get(CHANNEL_ID)).toEqual({
			totalUses: 12,
			recentUses: [
				NOW - 5 * DAY_MS - 3 * HOUR_MS,
				NOW - 5 * DAY_MS - 2 * HOUR_MS,
				NOW - 5 * DAY_MS - HOUR_MS,
				NOW - DAY_MS - 3 * HOUR_MS,
				NOW - DAY_MS - 2 * HOUR_MS,
				NOW - DAY_MS - HOUR_MS,
				NOW - 4 * HOUR_MS,
				NOW - 3 * HOUR_MS,
				NOW - 2 * HOUR_MS,
				NOW - HOUR_MS,
			],
			frecency: 840,
			score: 700,
		});
	});

	it('leaves computed entries alone until the key is used again', () => {
		const history: ChannelFrecencyHistory = new Map();
		trackChannelUse(history, CHANNEL_ID, NOW - 8 * DAY_MS);
		computeChannelFrecency(history, NOW - 8 * DAY_MS);
		const computed = history.get(CHANNEL_ID);
		expect(computed?.frecency).toBe(100);
		computeChannelFrecency(history, NOW);
		expect(history.get(CHANNEL_ID)).toBe(computed);
		trackChannelUse(history, CHANNEL_ID, NOW);
		computeChannelFrecency(history, NOW);
		expect(history.get(CHANNEL_ID)).toEqual({
			totalUses: 2,
			recentUses: [NOW - 8 * DAY_MS, NOW],
			frecency: 110,
			score: 110,
		});
	});
});

describe('capChannelFrecencyHistory', () => {
	it('drops the key whose last use is oldest once there are more than 100 keys', () => {
		const history: ChannelFrecencyHistory = new Map();
		trackChannelUse(history, keyFor(0), NOW - 10 * DAY_MS);
		trackChannelUse(history, keyFor(0), NOW + 500);
		for (let index = 1; index <= 100; index++) {
			trackChannelUse(history, keyFor(index), NOW + index);
		}
		capChannelFrecencyHistory(history);
		expect(history.size).toBe(100);
		expect(history.has(keyFor(0))).toBe(true);
		expect(history.has(keyFor(1))).toBe(false);
	});

	it('drops the earlier key when two share the oldest last use', () => {
		const history: ChannelFrecencyHistory = new Map();
		trackChannelUse(history, keyFor(0), NOW);
		trackChannelUse(history, keyFor(1), NOW);
		for (let index = 2; index <= 100; index++) {
			trackChannelUse(history, keyFor(index), NOW + index);
		}
		capChannelFrecencyHistory(history);
		expect(history.has(keyFor(0))).toBe(false);
		expect(history.has(keyFor(1))).toBe(true);
	});
});

describe('rankFrequentChannelIds', () => {
	it('sorts resolved ids by frecency, keeps key order on ties and drops unresolved keys', () => {
		const history: ChannelFrecencyHistory = new Map([
			['a', computedEntry(100)],
			['b', computedEntry(300)],
			['c', computedEntry(100)],
			['user', computedEntry(200)],
			['gone', computedEntry(999)],
		]);
		const resolved = new Map([
			['a', 'a'],
			['b', 'b'],
			['c', 'c'],
			['user', 'dm-channel'],
		]);
		expect(rankFrequentChannelIds(history, (key) => resolved.get(key) ?? null)).toEqual(['b', 'dm-channel', 'a', 'c']);
	});

	it('returns at most 100 ids', () => {
		const history: ChannelFrecencyHistory = new Map();
		for (let index = 0; index < 150; index++) {
			history.set(keyFor(index), computedEntry(100));
		}
		const ranked = rankFrequentChannelIds(history, (key) => key);
		expect(ranked).toHaveLength(100);
		expect(ranked[99]).toBe(keyFor(99));
	});
});

describe('restoreChannelFrecencyHistory', () => {
	it('re-derives every persisted entry against now and drops unusable ones', () => {
		const persisted: ReadonlyArray<readonly [unknown, unknown]> = [
			['a', {totalUses: 5, recentUses: [NOW - 8 * DAY_MS - HOUR_MS, NOW - 8 * DAY_MS], frecency: 999, score: 999}],
			['b', {totalUses: 2, recentUses: [], frecency: 50, score: 50}],
			['c', {totalUses: 'many', recentUses: [NOW]}],
			['d', {totalUses: 1, recentUses: [NOW - HOUR_MS, 'soon', -5, null], frecency: 1, score: 1}],
			[42, {totalUses: 1, recentUses: [NOW]}],
			['e', null],
		];
		const restored = restoreChannelFrecencyHistory(persisted, NOW);
		expect([...restored.keys()]).toEqual(['a', 'd']);
		expect(restored.get('a')).toEqual({
			totalUses: 5,
			recentUses: [NOW - 8 * DAY_MS - HOUR_MS, NOW - 8 * DAY_MS],
			frecency: 50,
			score: 20,
		});
		expect(restored.get('d')).toEqual({totalUses: 1, recentUses: [NOW - HOUR_MS], frecency: 100, score: 100});
	});
});

describe('channel frecency account sync', () => {
	it('carries only the uses over the wire and re-derives on the way back', () => {
		const history: ChannelFrecencyHistory = new Map();
		trackChannelUse(history, 'a', NOW - HOUR_MS);
		trackChannelUse(history, 'a', NOW);
		computeChannelFrecency(history, NOW);
		const usage = channelFrecencyHistoryToWire(history);
		expect(usage).toEqual({a: {totalUses: 2, recentUsesMs: [BigInt(NOW - HOUR_MS), BigInt(NOW)]}});
		const restored = channelFrecencyHistoryFromWire(usage, NOW);
		expect(restored.get('a')).toEqual({
			totalUses: 2,
			recentUses: [NOW - HOUR_MS, NOW],
			frecency: 200,
			score: 200,
		});
	});

	it('drops wire entries whose uses have all aged out of scoring', () => {
		const restored = channelFrecencyHistoryFromWire({a: {totalUses: 3, recentUsesMs: []}}, NOW);
		expect(restored.size).toBe(0);
	});

	it('merges two devices by keeping the higher count and the newest ten uses', () => {
		const merged = mergeChannelFrecencyWireUsage(
			{
				a: {totalUses: 9, recentUsesMs: [BigInt(NOW - 2 * DAY_MS), BigInt(NOW - HOUR_MS)]},
				b: {totalUses: 1, recentUsesMs: [BigInt(NOW - HOUR_MS)]},
			},
			{
				a: {totalUses: 4, recentUsesMs: [BigInt(NOW - HOUR_MS), BigInt(NOW)]},
				c: {totalUses: 2, recentUsesMs: [BigInt(NOW)]},
			},
			NOW,
		);
		expect(merged.a).toEqual({
			totalUses: 9,
			recentUsesMs: [BigInt(NOW - 2 * DAY_MS), BigInt(NOW - HOUR_MS), BigInt(NOW)],
		});
		expect(Object.keys(merged).sort()).toEqual(['a', 'b', 'c']);
	});

	it('keeps at most ten uses and a hundred keys when merging', () => {
		const uses = Array.from({length: 8}, (_value, index) => BigInt(NOW - index * HOUR_MS));
		const local: Record<string, {totalUses: number; recentUsesMs: Array<bigint>}> = {};
		const incoming: Record<string, {totalUses: number; recentUsesMs: Array<bigint>}> = {};
		for (let index = 0; index < 60; index++) {
			local[keyFor(index)] = {totalUses: 1, recentUsesMs: [BigInt(NOW - index * HOUR_MS)]};
		}
		for (let index = 60; index < 130; index++) {
			incoming[keyFor(index)] = {totalUses: 1, recentUsesMs: [BigInt(NOW - index * HOUR_MS)]};
		}
		local.a = {totalUses: 20, recentUsesMs: uses};
		incoming.a = {totalUses: 20, recentUsesMs: [BigInt(NOW + HOUR_MS), BigInt(NOW + 2 * HOUR_MS), BigInt(NOW)]};
		const merged = mergeChannelFrecencyWireUsage(local, incoming, NOW);
		expect(merged.a.recentUsesMs).toHaveLength(10);
		expect(Object.keys(merged)).toHaveLength(100);
	});
});
