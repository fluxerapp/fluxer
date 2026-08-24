// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import type {SearchHints} from '@app/features/search/utils/SearchQueryParser';
import {action, makeAutoObservable} from 'mobx';

export const SEARCH_HISTORY_LIMIT = 10;
export const SEARCH_HISTORY_DISPLAY_LIMIT = 5;

export interface SearchHistoryEntry {
	query: string;
	hints?: SearchHints;
	ts: number;
}

class SearchHistory {
	entriesByChannel: Record<string, Array<SearchHistoryEntry>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makePersistent(this, 'SearchHistory', ['entriesByChannel']);
	}

	private getEntries(channelId?: string): Array<SearchHistoryEntry> {
		if (!channelId) return [];
		return this.entriesByChannel[channelId] ?? [];
	}

	recent(channelId?: string): ReadonlyArray<SearchHistoryEntry> {
		return this.getEntries(channelId);
	}

	search(term: string, channelId?: string): ReadonlyArray<SearchHistoryEntry> {
		const entries = this.getEntries(channelId);
		const t = term.trim().toLowerCase();
		if (!t) return entries;
		return entries.filter((e) => e.query.toLowerCase().includes(t));
	}

	@action
	add(query: string, channelId?: string, hints?: SearchHints): void {
		if (!channelId) return;
		const q = query.trim();
		if (!q) return;
		if (!this.entriesByChannel[channelId]) {
			this.entriesByChannel[channelId] = [];
		}
		const entries = this.entriesByChannel[channelId];
		const ts = Date.now();
		const entry: SearchHistoryEntry = {query: q, hints, ts};
		const existingIdx = entries.findIndex((e) => e.query === q);
		if (existingIdx !== -1) {
			entries.splice(existingIdx, 1);
		}
		entries.unshift(entry);
		if (entries.length > SEARCH_HISTORY_LIMIT) {
			this.entriesByChannel[channelId] = entries.slice(0, SEARCH_HISTORY_LIMIT);
		}
	}

	@action
	adoptLegacyEntries(targetKey: string | undefined, legacyKeys: ReadonlyArray<string>): void {
		if (!targetKey) return;
		if (this.getEntries(targetKey).length > 0) return;
		const merged: Array<SearchHistoryEntry> = [];
		for (const legacyKey of legacyKeys) {
			if (legacyKey === targetKey) continue;
			const legacyEntries = this.entriesByChannel[legacyKey];
			if (legacyEntries == null) continue;
			merged.push(...legacyEntries);
			delete this.entriesByChannel[legacyKey];
		}
		if (merged.length === 0) return;
		merged.sort((left, right) => right.ts - left.ts);
		const seen = new Set<string>();
		const adopted: Array<SearchHistoryEntry> = [];
		for (const entry of merged) {
			if (seen.has(entry.query)) continue;
			seen.add(entry.query);
			adopted.push(entry);
			if (adopted.length >= SEARCH_HISTORY_LIMIT) break;
		}
		this.entriesByChannel[targetKey] = adopted;
	}

	@action
	clear(channelId?: string): void {
		if (!channelId) return;
		delete this.entriesByChannel[channelId];
	}

	@action
	clearAll(): void {
		this.entriesByChannel = {};
	}
}

export default new SearchHistory();
