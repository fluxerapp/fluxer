// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage, {subscribeAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import type {UserGuildSettingsPartial} from '@app/features/user/models/UserGuildSettings';

const JOURNAL_STORAGE_KEY = 'UserGuildSettings:writeJournal';
const JOURNAL_MAX_AGE_MS = 24 * 60 * 60 * 1000;
const JOURNAL_LIMIT = 100;

interface JournalEntry {
	readonly updates: UserGuildSettingsPartial;
	readonly at: number;
}

let journal: Map<string, JournalEntry> | null = null;

function load(): Map<string, JournalEntry> {
	if (journal !== null) return journal;
	const stored = AppStorage.getJSON<Record<string, Partial<JournalEntry>>>(JOURNAL_STORAGE_KEY);
	const loaded = new Map<string, JournalEntry>();
	const now = Date.now();
	if (stored && typeof stored === 'object') {
		for (const [key, entry] of Object.entries(stored)) {
			if (typeof entry?.at !== 'number' || now - entry.at > JOURNAL_MAX_AGE_MS) continue;
			if (entry.updates == null || typeof entry.updates !== 'object') continue;
			loaded.set(key, {updates: entry.updates, at: entry.at});
		}
	}
	journal = loaded;
	return loaded;
}

export function recordGuildSettingsWrite(key: string, updates: UserGuildSettingsPartial): void {
	const entries = load();
	const previous = entries.get(key)?.updates ?? {};
	const applied = updates.channel_overrides === null ? {...updates, channel_overrides: {}} : updates;
	entries.delete(key);
	entries.set(key, {updates: {...previous, ...applied}, at: Date.now()});
	while (entries.size > JOURNAL_LIMIT) entries.delete(entries.keys().next().value!);
	AppStorage.setJSON(JOURNAL_STORAGE_KEY, Object.fromEntries(entries));
}

export function journaledGuildSettingsWrites(): ReadonlyArray<[string, UserGuildSettingsPartial]> {
	return Array.from(load(), ([key, entry]) => [key, entry.updates]);
}

subscribeAppStorageScope(() => {
	journal = null;
});
