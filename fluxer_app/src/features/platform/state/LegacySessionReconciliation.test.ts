// SPDX-License-Identifier: AGPL-3.0-or-later

import {FIXTURE_CURRENT_INSTANCE_SNAPSHOT} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import type {KeyedStoredAccount} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {
	type LegacySessionAccountStore,
	LegacySessionReconciliationOutcome,
	reconcileLegacySession,
} from '@app/features/platform/state/LegacySessionReconciliation';
import {describe, expect, test} from 'vitest';

const NOW = 1_790_000_000_000;
const USER_ID = '4242';
const TOKEN = 'token.4242';
const STORAGE_KEY = accountStorageKey(USER_ID, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

function liveSession(): Storage {
	const values = new Map([
		['userId', USER_ID],
		['token', TOKEN],
	]);
	return {
		get length() {
			return values.size;
		},
		clear: () => values.clear(),
		getItem: (key: string) => values.get(key) ?? null,
		key: (index: number) => [...values.keys()][index] ?? null,
		removeItem: (key: string) => void values.delete(key),
		setItem: (key: string, value: string) => void values.set(key, value),
	};
}

function recordingStore(activeAccountKey: string | null) {
	const calls: Array<string> = [];
	const store: LegacySessionAccountStore = {
		async findAccountByCredentials() {
			calls.push('find');
			return {userId: USER_ID, token: TOKEN, storageKey: STORAGE_KEY} as KeyedStoredAccount;
		},
		async readActiveAccountKey() {
			calls.push('read-active');
			return activeAccountKey;
		},
		async setActiveAccountKey(storageKey, reconciledAt) {
			calls.push(`set-active ${storageKey} ${reconciledAt}`);
		},
	};
	return {store, calls};
}

function reconcile(store: LegacySessionAccountStore) {
	return reconcileLegacySession({
		store,
		legacyStorage: liveSession(),
		currentAccount: {userId: USER_ID, token: TOKEN, instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT},
		now: NOW,
	});
}

describe('reconcileLegacySession', () => {
	test('a warm boot whose live session is already the active account writes nothing', async () => {
		const {store, calls} = recordingStore(STORAGE_KEY);

		const result = await reconcile(store);

		expect(result).toEqual({
			outcome: LegacySessionReconciliationOutcome.UNCHANGED,
			storageKey: STORAGE_KEY,
			reason: null,
		});
		expect(calls).toEqual(['read-active']);
	});

	test('a live session that is not the active account becomes active', async () => {
		const {store, calls} = recordingStore('https://other.example/api::1');

		const result = await reconcile(store);

		expect(result).toEqual({
			outcome: LegacySessionReconciliationOutcome.UNCHANGED,
			storageKey: STORAGE_KEY,
			reason: null,
		});
		expect(calls).toEqual(['read-active', 'find', `set-active ${STORAGE_KEY} ${NOW}`]);
	});
});
