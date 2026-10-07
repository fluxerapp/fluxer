// SPDX-License-Identifier: AGPL-3.0-or-later

export type UsageEntry = Readonly<{
	count: number;
	lastUsed: number;
}>;

export type WireUsageStat = Readonly<{
	count: number;
	lastUsedMs: bigint;
}>;

export type UsageRanking = Readonly<{
	version: number;
	computedAt: number;
	rankedKeys: ReadonlyArray<string>;
	scoreByKey: ReadonlyMap<string, number>;
}>;

const USAGE_FRECENCY_HALF_LIFE_MS = 7 * 24 * 60 * 60 * 1000;
export const MAX_TRACKED_USAGE_KEYS = 200;
const MAX_USAGE_COUNT = 100_000;
const MAX_USAGE_KEY_LENGTH = 256;

export const EMPTY_USAGE_RANKING: UsageRanking = Object.freeze({
	version: 0,
	computedAt: 0,
	rankedKeys: Object.freeze([]),
	scoreByKey: new Map<string, number>(),
});

export function isValidUsageKey(key: string): boolean {
	return key.length > 0 && key.length <= MAX_USAGE_KEY_LENGTH;
}

export function usageFrecencyScore(entry: UsageEntry, now: number): number {
	const ageMs = Math.max(0, now - entry.lastUsed);
	return Math.log2(entry.count + 1) * 0.5 ** (ageMs / USAGE_FRECENCY_HALF_LIFE_MS);
}

function sanitizeUsageEntry(entry: {count: number; lastUsed: number}, now: number): UsageEntry | null {
	if (!Number.isFinite(entry.count) || !Number.isFinite(entry.lastUsed)) {
		return null;
	}
	const count = Math.min(Math.floor(entry.count), MAX_USAGE_COUNT);
	if (count < 1) {
		return null;
	}
	const lastUsed = Math.min(Math.max(Math.floor(entry.lastUsed), 0), now);
	return {count, lastUsed};
}

export function bumpUsageEntry(existing: UsageEntry | undefined, now: number): UsageEntry {
	return {
		count: Math.min((existing?.count ?? 0) + 1, MAX_USAGE_COUNT),
		lastUsed: now,
	};
}

type RankedUsage = Readonly<{
	key: string;
	entry: UsageEntry;
	score: number;
}>;

function compareRankedUsage(a: RankedUsage, b: RankedUsage): number {
	if (a.score !== b.score) {
		return b.score - a.score;
	}
	if (a.entry.lastUsed !== b.entry.lastUsed) {
		return b.entry.lastUsed - a.entry.lastUsed;
	}
	if (a.entry.count !== b.entry.count) {
		return b.entry.count - a.entry.count;
	}
	return a.key < b.key ? -1 : a.key > b.key ? 1 : 0;
}

function rankEntries(usage: Readonly<Record<string, UsageEntry>>, now: number): Array<RankedUsage> {
	const ranked: Array<RankedUsage> = [];
	for (const [key, entry] of Object.entries(usage)) {
		if (!entry) continue;
		ranked.push({key, entry, score: usageFrecencyScore(entry, now)});
	}
	ranked.sort(compareRankedUsage);
	return ranked;
}

export function sanitizeUsageMap(
	input: Readonly<Record<string, {count: number; lastUsed: number}>>,
	now: number,
	maxKeys: number = MAX_TRACKED_USAGE_KEYS,
): Record<string, UsageEntry> {
	const sanitized: Record<string, UsageEntry> = {};
	for (const [key, rawEntry] of Object.entries(input)) {
		if (!rawEntry || !isValidUsageKey(key)) continue;
		const entry = sanitizeUsageEntry(rawEntry, now);
		if (entry) {
			sanitized[key] = entry;
		}
	}
	if (Object.keys(sanitized).length <= maxKeys) {
		return sanitized;
	}
	const pruned: Record<string, UsageEntry> = {};
	for (const {key, entry} of rankEntries(sanitized, now).slice(0, maxKeys)) {
		pruned[key] = entry;
	}
	return pruned;
}

export function rankUsageMap(usage: Readonly<Record<string, UsageEntry>>, now: number, version: number): UsageRanking {
	const ranked = rankEntries(usage, now);
	const rankedKeys: Array<string> = [];
	const scoreByKey = new Map<string, number>();
	for (const {key, score} of ranked) {
		rankedKeys.push(key);
		scoreByKey.set(key, score);
	}
	return Object.freeze({
		version,
		computedAt: now,
		rankedKeys: Object.freeze(rankedKeys),
		scoreByKey,
	});
}

export function usageEntryToWire(entry: UsageEntry): {count: number; lastUsedMs: bigint} {
	return {count: entry.count, lastUsedMs: BigInt(entry.lastUsed)};
}

export function usageEntryFromWire(stat: {count: number; lastUsedMs: bigint}): {count: number; lastUsed: number} {
	return {count: stat.count, lastUsed: Number(stat.lastUsedMs)};
}

export function mergeWireUsageMaps(
	local: Readonly<Record<string, WireUsageStat>>,
	incoming: Readonly<Record<string, WireUsageStat>>,
	now: number,
	maxKeys: number = MAX_TRACKED_USAGE_KEYS,
): Record<string, {count: number; lastUsedMs: bigint}> {
	const merged: Record<string, {count: number; lastUsed: number}> = {};
	for (const source of [local, incoming]) {
		for (const [key, stat] of Object.entries(source)) {
			if (!stat) continue;
			const entry = usageEntryFromWire(stat);
			const existing = merged[key];
			merged[key] = existing
				? {count: Math.max(existing.count, entry.count), lastUsed: Math.max(existing.lastUsed, entry.lastUsed)}
				: entry;
		}
	}
	const sanitized = sanitizeUsageMap(merged, now, maxKeys);
	const wire: Record<string, {count: number; lastUsedMs: bigint}> = {};
	for (const [key, entry] of Object.entries(sanitized)) {
		wire[key] = usageEntryToWire(entry);
	}
	return wire;
}

export function dedupeBoundedIds(ids: ReadonlyArray<string>, maxIds: number): Array<string> {
	const deduped: Array<string> = [];
	const seen = new Set<string>();
	for (const id of ids) {
		if (id.length === 0 || id.length > MAX_USAGE_KEY_LENGTH || seen.has(id)) continue;
		seen.add(id);
		deduped.push(id);
		if (deduped.length >= maxIds) break;
	}
	return deduped;
}
