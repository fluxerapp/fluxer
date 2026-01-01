/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {action, makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import type {SearchHints} from '~/utils/SearchQueryParser';

export interface SearchHistoryEntry {
	query: string;
	hints?: SearchHints;
	ts: number;
}

class SearchHistoryStoreImpl {
	entriesByChannel: Record<string, Array<SearchHistoryEntry>> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makePersistent(this, 'SearchHistoryStore', ['entriesByChannel']);
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
		const existingIdx = entries.findIndex((e) => e.query === q);
		const entry: SearchHistoryEntry = {query: q, hints, ts};

		if (existingIdx !== -1) {
			entries.splice(existingIdx, 1);
		}
		entries.unshift(entry);
		if (entries.length > 10) {
			this.entriesByChannel[channelId] = entries.slice(0, 10);
		}
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

export default new SearchHistoryStoreImpl();
