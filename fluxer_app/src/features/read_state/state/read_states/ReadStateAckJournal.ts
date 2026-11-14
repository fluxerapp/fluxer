// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage, {subscribeAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import {isNewerMessageId} from '@app/features/read_state/state/read_states/shared';

const JOURNAL_STORAGE_KEY = 'ReadStates:ackJournal';
const JOURNAL_LIMIT = 200;
const JOURNAL_MAX_AGE_MS = 24 * 60 * 60 * 1000;
const PERSIST_DELAY_MS = 250;

interface JournalEntry {
	readonly ack: string | null;
	readonly manual: boolean;
	readonly at: number;
}

let journal: Map<string, JournalEntry> | null = null;
let persistTimer: ReturnType<typeof setTimeout> | null = null;

function load(): Map<string, JournalEntry> {
	if (journal !== null) return journal;
	const stored = AppStorage.getJSON<Record<string, Partial<JournalEntry>>>(JOURNAL_STORAGE_KEY);
	const loaded = new Map<string, JournalEntry>();
	const now = Date.now();
	if (stored && typeof stored === 'object') {
		for (const [channelId, entry] of Object.entries(stored)) {
			if (typeof entry?.at !== 'number' || now - entry.at > JOURNAL_MAX_AGE_MS) continue;
			if (entry.ack !== null && typeof entry.ack !== 'string') continue;
			loaded.set(channelId, {ack: entry.ack ?? null, manual: entry.manual === true, at: entry.at});
		}
	}
	journal = loaded;
	return loaded;
}

function persistSoon(): void {
	if (persistTimer !== null) return;
	persistTimer = setTimeout(() => {
		persistTimer = null;
		const entries = load();
		if (entries.size === 0) AppStorage.removeItem(JOURNAL_STORAGE_KEY);
		else AppStorage.setJSON(JOURNAL_STORAGE_KEY, Object.fromEntries(entries));
	}, PERSIST_DELAY_MS);
}

export function recordReadStateAck(channelId: string, ack: string | null, manual: boolean): void {
	const entries = load();
	entries.delete(channelId);
	entries.set(channelId, {ack, manual, at: Date.now()});
	while (entries.size > JOURNAL_LIMIT) entries.delete(entries.keys().next().value!);
	persistSoon();
}

export function journaledAckNewerThan(channelId: string, snapshotAck: string | null): {ack: string | null} | null {
	const entry = load().get(channelId);
	if (entry === undefined) return null;
	if (entry.manual || isNewerMessageId(entry.ack, snapshotAck)) return {ack: entry.ack};
	return null;
}

subscribeAppStorageScope(() => {
	if (persistTimer !== null) {
		clearTimeout(persistTimer);
		persistTimer = null;
	}
	journal = null;
});
