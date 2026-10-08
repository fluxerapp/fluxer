// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {MentionFrecencyStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable, observable} from 'mobx';

interface FrecencyEntry {
	count: number;
	lastAt: number;
}

const GLOBAL_SCOPE = '__global__';
const KEY_SEPARATOR = '\u0000';
const HALF_LIFE_MS = 14 * 24 * 60 * 60 * 1000;
const MAX_BOOST = 3;
const MAX_ENTRIES_PER_SCOPE = 100;
const PRUNE_SCORE_FLOOR = 0.05;

function compositeKey(scope: string, userId: string): string {
	return `${scope}${KEY_SEPARATOR}${userId}`;
}

function splitKey(key: string): {
	scope: string;
	userId: string;
} | null {
	const idx = key.indexOf(KEY_SEPARATOR);
	if (idx === -1) return null;
	return {scope: key.slice(0, idx), userId: key.slice(idx + 1)};
}

class MentionFrecencyRegistry {
	entries: Map<string, FrecencyEntry> = observable.map();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'mentionFrecency',
			schema: MentionFrecencyStateSchema,
			persist: ['entries'],
			toMessage: (s) => {
				const byScope = new Map<
					string,
					Array<{
						userId: string;
						count: number;
						lastAt: number;
					}>
				>();
				for (const [key, entry] of s.entries) {
					const split = splitKey(key);
					if (!split) continue;
					const guildId = split.scope === GLOBAL_SCOPE ? '' : split.scope;
					let list = byScope.get(guildId);
					if (!list) {
						list = [];
						byScope.set(guildId, list);
					}
					list.push({userId: split.userId, count: entry.count, lastAt: entry.lastAt});
				}
				return {
					scopes: Array.from(byScope.entries()).map(([guildId, entries]) => ({
						guildId,
						entries: entries.map((e) => ({
							userId: e.userId,
							count: e.count,
							lastAtMs: BigInt(e.lastAt),
						})),
					})),
				};
			},
			applyMessage: (s, m) => {
				s.entries.clear();
				for (const scope of m.scopes) {
					const scopeKey = scope.guildId === '' ? GLOBAL_SCOPE : scope.guildId;
					for (const entry of scope.entries) {
						s.entries.set(compositeKey(scopeKey, entry.userId), {
							count: entry.count,
							lastAt: Number(entry.lastAtMs),
						});
					}
				}
			},
		});
	}

	private scope(guildId: string | null): string {
		return guildId ?? GLOBAL_SCOPE;
	}

	recordMention(guildId: string | null, userId: string): void {
		const scope = this.scope(guildId);
		const key = compositeKey(scope, userId);
		const existing = this.entries.get(key);
		this.entries.set(key, {
			count: (existing?.count ?? 0) + 1,
			lastAt: Date.now(),
		});
		this.pruneIfNeeded(scope);
	}

	private pruneIfNeeded(scope: string): void {
		const inScope: Array<{
			key: string;
			entry: FrecencyEntry;
			score: number;
		}> = [];
		for (const [key, entry] of this.entries) {
			const split = splitKey(key);
			if (!split || split.scope !== scope) continue;
			inScope.push({key, entry, score: this.score(entry)});
		}
		if (inScope.length <= MAX_ENTRIES_PER_SCOPE) return;
		inScope.sort((a, b) => b.score - a.score);
		for (const {key} of inScope.slice(MAX_ENTRIES_PER_SCOPE)) {
			this.entries.delete(key);
		}
	}

	private score(entry: FrecencyEntry): number {
		const ageMs = Math.max(0, Date.now() - entry.lastAt);
		const decay = 0.5 ** (ageMs / HALF_LIFE_MS);
		return Math.log2(entry.count + 1) * decay;
	}

	getBoosters(guildId: string | null): Record<string, number> {
		const result: Record<string, number> = {};
		const scopes = guildId ? [guildId, GLOBAL_SCOPE] : [GLOBAL_SCOPE];
		for (const scope of scopes) {
			for (const [key, entry] of this.entries) {
				const split = splitKey(key);
				if (!split || split.scope !== scope) continue;
				if (result[split.userId] != null) continue;
				const raw = this.score(entry);
				if (raw < PRUNE_SCORE_FLOOR) continue;
				result[split.userId] = 1 + Math.min(MAX_BOOST - 1, raw);
			}
		}
		return result;
	}

	getRecentUserIds(guildId: string | null, limit: number): Array<string> {
		const seen = new Set<string>();
		const ranked: Array<{
			userId: string;
			score: number;
		}> = [];
		const scopes = guildId ? [guildId, GLOBAL_SCOPE] : [GLOBAL_SCOPE];
		for (const scope of scopes) {
			for (const [key, entry] of this.entries) {
				const split = splitKey(key);
				if (!split || split.scope !== scope) continue;
				if (seen.has(split.userId)) continue;
				seen.add(split.userId);
				ranked.push({userId: split.userId, score: this.score(entry)});
			}
		}
		ranked.sort((a, b) => b.score - a.score);
		return ranked.slice(0, limit).map((r) => r.userId);
	}

	handleLogout(): void {
		this.entries.clear();
	}
}

export default new MentionFrecencyRegistry();
