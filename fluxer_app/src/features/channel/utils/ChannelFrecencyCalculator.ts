// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ChannelFrecencyEntry {
	readonly hitCount: number;
	readonly recentHits: ReadonlyArray<number>;
	readonly heat: number;
	readonly score: number;
}

export type ChannelFrecencyHistory = Map<string, ChannelFrecencyEntry>;

interface RankedChannelFrecency {
	readonly id: string;
	readonly heat: number;
}

const CHANNEL_FRECENCY_MAX_ITEMS = 100;
const CHANNEL_FRECENCY_MAX_SAMPLES = 10;
const CHANNEL_FRECENCY_UNCOMPUTED = -1;

const DAY_MS = 86_400_000;
const MINUTE_MS = 60_000;

function createEntry(
	hitCount: number,
	recentHits: ReadonlyArray<number>,
	heat: number,
	score: number,
): ChannelFrecencyEntry {
	return Object.freeze({hitCount, recentHits: Object.freeze(recentHits), heat, score});
}

function utcOffsetMinutes(timestamp: number): number {
	return -new Date(timestamp).getTimezoneOffset();
}

function compareAscending(a: number, b: number): number {
	return a - b;
}

export function channelFrecencyDayDiff(now: number, then: number): number {
	const zoneDeltaMs = (utcOffsetMinutes(then) - utcOffsetMinutes(now)) * MINUTE_MS;
	return Math.trunc((now - then - zoneDeltaMs) / DAY_MS) || 0;
}

export function channelFrecencyWeight(dayDiff: number): number {
	if (dayDiff === 0) return 100;
	if (dayDiff >= 1 && dayDiff < 2) return 70;
	if (dayDiff >= 2 && dayDiff < 4) return 50;
	if (dayDiff >= 4 && dayDiff < 7) return 30;
	if (dayDiff >= 7) return 10;
	return 1;
}

function scoreRecentUses(recentHits: ReadonlyArray<number>, now: number): number {
	const sampleCount = Math.min(recentHits.length, CHANNEL_FRECENCY_MAX_SAMPLES);
	let score = 0;
	for (let index = 0; index < sampleCount; index++) {
		score += channelFrecencyWeight(channelFrecencyDayDiff(now, recentHits[index]));
	}
	return score;
}

export function trackChannelUse(history: ChannelFrecencyHistory, key: string, timestamp?: number): void {
	const use = timestamp ?? Date.now();
	const entry = history.get(key);
	if (entry === undefined) {
		history.set(key, createEntry(1, [use], CHANNEL_FRECENCY_UNCOMPUTED, 0));
		return;
	}
	const recentHits = [...entry.recentHits, use];
	if (timestamp !== undefined) {
		recentHits.sort(compareAscending);
	}
	history.set(
		key,
		createEntry(
			entry.hitCount + 1,
			recentHits.slice(-CHANNEL_FRECENCY_MAX_SAMPLES),
			CHANNEL_FRECENCY_UNCOMPUTED,
			entry.score,
		),
	);
}

export function computeChannelFrecency(history: ChannelFrecencyHistory, now: number): void {
	for (const [key, entry] of history) {
		if (entry.heat !== CHANNEL_FRECENCY_UNCOMPUTED) continue;
		const score = scoreRecentUses(entry.recentHits, now);
		if (score > 0) {
			const heat = Math.ceil(entry.hitCount * (score / entry.recentHits.length));
			history.set(key, createEntry(entry.hitCount, entry.recentHits, heat, score));
		} else {
			history.delete(key);
		}
	}
}

function findOldestLastUseKey(history: ReadonlyMap<string, ChannelFrecencyEntry>): string | null {
	let oldestKey: string | null = null;
	let oldestLastUse = Number.POSITIVE_INFINITY;
	for (const [key, entry] of history) {
		const lastUse = entry.recentHits.at(-1);
		if (lastUse !== undefined && lastUse < oldestLastUse) {
			oldestKey = key;
			oldestLastUse = lastUse;
		}
	}
	return oldestKey;
}

export function capChannelFrecencyHistory(history: ChannelFrecencyHistory): void {
	while (history.size > CHANNEL_FRECENCY_MAX_ITEMS) {
		const oldestKey = findOldestLastUseKey(history);
		if (oldestKey === null) return;
		history.delete(oldestKey);
	}
}

export function rankFrequentChannelIds(
	history: ReadonlyMap<string, ChannelFrecencyEntry>,
	resolveRecordId: (key: string) => string | null,
): ReadonlyArray<string> {
	const ranked: Array<RankedChannelFrecency> = [];
	for (const [key, entry] of history) {
		const id = resolveRecordId(key);
		if (id !== null) {
			ranked.push({id, heat: entry.heat});
		}
	}
	ranked.sort((a, b) => b.heat - a.heat);
	return Object.freeze(ranked.slice(0, CHANNEL_FRECENCY_MAX_ITEMS).map((item) => item.id));
}

interface ChannelFrecencyWireEntry {
	readonly totalUses: number;
	readonly recentUsesMs: ReadonlyArray<bigint>;
}

export type ChannelFrecencyWireUsage = Record<string, {totalUses: number; recentUsesMs: Array<bigint>}>;

export type ChannelFrecencyWireUsageInput = Readonly<Record<string, ChannelFrecencyWireEntry | undefined>>;

function entryToWire(entry: ChannelFrecencyEntry): {totalUses: number; recentUsesMs: Array<bigint>} {
	return {totalUses: entry.hitCount, recentUsesMs: entry.recentHits.map((use) => BigInt(use))};
}

export function channelFrecencyHistoryToWire(
	history: ReadonlyMap<string, ChannelFrecencyEntry>,
): ChannelFrecencyWireUsage {
	const usage: ChannelFrecencyWireUsage = {};
	for (const [key, entry] of history) {
		usage[key] = entryToWire(entry);
	}
	return usage;
}

function readWireUses(entry: ChannelFrecencyWireEntry | undefined): Array<number> {
	if (entry === undefined) return [];
	return entry.recentUsesMs.map((use) => Number(use)).filter(isUseTimestamp);
}

export function channelFrecencyHistoryFromWire(
	usage: ChannelFrecencyWireUsageInput,
	now: number,
): ChannelFrecencyHistory {
	const revived: Array<readonly [string, unknown]> = [];
	for (const [key, entry] of Object.entries(usage)) {
		revived.push([key, {hitCount: entry?.totalUses, recentHits: readWireUses(entry)}]);
	}
	return restoreChannelFrecencyHistory(revived, now);
}

export function mergeChannelFrecencyWireUsage(
	local: ChannelFrecencyWireUsageInput,
	incoming: ChannelFrecencyWireUsageInput,
	now: number,
): ChannelFrecencyWireUsage {
	const merged: ChannelFrecencyHistory = new Map();
	const keys = new Set([...Object.keys(local), ...Object.keys(incoming)]);
	for (const key of keys) {
		const localEntry = local[key];
		const incomingEntry = incoming[key];
		const uses = [...new Set([...readWireUses(localEntry), ...readWireUses(incomingEntry)])];
		if (uses.length === 0) continue;
		uses.sort(compareAscending);
		const totalUses = Math.max(localEntry?.totalUses ?? 0, incomingEntry?.totalUses ?? 0);
		if (!Number.isFinite(totalUses) || totalUses <= 0) continue;
		merged.set(key, createEntry(totalUses, uses.slice(-CHANNEL_FRECENCY_MAX_SAMPLES), CHANNEL_FRECENCY_UNCOMPUTED, 0));
	}
	computeChannelFrecency(merged, now);
	capChannelFrecencyHistory(merged);
	return channelFrecencyHistoryToWire(merged);
}

function isUseTimestamp(value: unknown): value is number {
	return typeof value === 'number' && Number.isFinite(value) && value > 0;
}

function readPersistedEntry(value: unknown): ChannelFrecencyEntry | null {
	if (typeof value !== 'object' || value === null) return null;
	const {hitCount, recentHits} = value as {readonly hitCount?: unknown; readonly recentHits?: unknown};
	if (typeof hitCount !== 'number' || !Number.isFinite(hitCount) || !Array.isArray(recentHits)) return null;
	return createEntry(hitCount, recentHits.filter(isUseTimestamp), CHANNEL_FRECENCY_UNCOMPUTED, 0);
}

export function restoreChannelFrecencyHistory(
	revived: Iterable<readonly [unknown, unknown]>,
	now: number,
): ChannelFrecencyHistory {
	const history: ChannelFrecencyHistory = new Map();
	for (const [key, value] of revived) {
		if (typeof key !== 'string') continue;
		const entry = readPersistedEntry(value);
		if (entry !== null) {
			history.set(key, entry);
		}
	}
	computeChannelFrecency(history, now);
	return history;
}
