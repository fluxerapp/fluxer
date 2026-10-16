// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import type {QualifiedStoredAccount} from '@app/features/auth/state/AccountStorageContract';
import type {
	ReadyStoredAccountEntry,
	StoredAccountInventory,
	StoredAccountRuntimeRecoveryCandidate,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	AuthSessionStoredAccountResolver,
	type AuthSessionStoredAccountResolverStorage,
	StoredAccountNotFoundError,
} from '@app/features/platform/state/auth_session/AuthSessionStoredAccountResolver';
import {describe, expect, it} from 'vitest';

const PRIMARY_INSTANCE_KEY = 'https://fluxer.app/api';
const FOREIGN_INSTANCE_KEY = 'https://self.hosted.example/api';

function instanceSnapshot(apiEndpoint: string): RuntimeConfigSnapshot {
	return {...FIXTURE_CURRENT_INSTANCE_SNAPSHOT, apiEndpoint, apiPublicEndpoint: apiEndpoint};
}

function accountKeyOf(instanceKey: string, userId: string): string {
	return `${instanceKey}::${userId}`;
}

function qualifiedRecord(instanceKey: string, userId: string): QualifiedStoredAccount {
	return {
		userId,
		token: `token.${instanceKey}.${userId}`,
		presenceIntent: null,
		localStorageData: {},
		managedStorageData: {},
		lastActive: FIXTURE_NOW,
		instance: instanceSnapshot(instanceKey),
		storageKey: accountKeyOf(instanceKey, userId),
	};
}

function readyEntry(instanceKey: string, userId: string): ReadyStoredAccountEntry {
	return {kind: 'ready', record: qualifiedRecord(instanceKey, userId), revision: {kind: 'stored-account-revision'}};
}

function recoveryCandidate(instanceKey: string, userId: string): StoredAccountRuntimeRecoveryCandidate {
	const record = qualifiedRecord(instanceKey, userId);
	return {
		kind: 'runtime-recovery',
		data: record,
		storageKey: record.storageKey,
		instanceKey,
		revision: {kind: 'stored-account-revision'},
	};
}

function inventory(overrides: Partial<StoredAccountInventory> = {}): StoredAccountInventory {
	return {
		source: 'idb',
		readyEntries: [],
		runtimeRecoveryCandidates: [],
		unqualifiedRecords: [],
		unavailableRecords: [],
		...overrides,
	};
}

function createResolver(stored: StoredAccountInventory): AuthSessionStoredAccountResolver {
	const accountStorage: AuthSessionStoredAccountResolverStorage = {
		getAccountInventory: async () => stored,
		replaceInventoryEntry: async () => {
			throw new Error('replaceInventoryEntry must not be reached');
		},
	};
	return new AuthSessionStoredAccountResolver({
		accountStorage,
		resolveRuntimeEndpoint: async () => {
			throw new Error('resolveRuntimeEndpoint must not be reached');
		},
	});
}

describe('AuthSessionStoredAccountResolver prepareAccount', () => {
	it('prepares the record stored under the exact key it was handed', async () => {
		const resolver = createResolver(inventory({readyEntries: [readyEntry(PRIMARY_INSTANCE_KEY, '1')]}));

		const record = await resolver.prepareAccount(accountKeyOf(PRIMARY_INSTANCE_KEY, '1'));

		expect(record.storageKey).toBe(accountKeyOf(PRIMARY_INSTANCE_KEY, '1'));
		expect(record.token).toBe(`token.${PRIMARY_INSTANCE_KEY}.1`);
	});

	it('refuses a key for one instance when only the same user on another instance is stored', async () => {
		const resolver = createResolver(inventory({readyEntries: [readyEntry(PRIMARY_INSTANCE_KEY, '1')]}));

		await expect(resolver.prepareAccount(accountKeyOf(FOREIGN_INSTANCE_KEY, '1'))).rejects.toBeInstanceOf(
			StoredAccountNotFoundError,
		);
	});

	it('refuses a key for one instance when the same user is only awaiting runtime recovery elsewhere', async () => {
		const resolver = createResolver(
			inventory({runtimeRecoveryCandidates: [recoveryCandidate(PRIMARY_INSTANCE_KEY, '1')]}),
		);

		await expect(resolver.prepareAccount(accountKeyOf(FOREIGN_INSTANCE_KEY, '1'))).rejects.toBeInstanceOf(
			StoredAccountNotFoundError,
		);
	});

	it('refuses a key naming a user that is stored on no instance', async () => {
		const resolver = createResolver(inventory({readyEntries: [readyEntry(PRIMARY_INSTANCE_KEY, '1')]}));

		await expect(resolver.prepareAccount(accountKeyOf(PRIMARY_INSTANCE_KEY, '2'))).rejects.toBeInstanceOf(
			StoredAccountNotFoundError,
		);
	});
});
