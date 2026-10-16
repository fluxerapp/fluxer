// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey, storedInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {
	InvalidRuntimeConfigSnapshotError,
	requireRuntimeConfigSnapshot,
} from '@app/features/app/state/RuntimeConfigSnapshot';
import type {
	QualifiedStoredAccount,
	StoredAccount,
	StoredAccountSource,
} from '@app/features/auth/state/AccountStorageContract';
import {accountStorageKey, accountStorageKeyFromInstanceKey} from '@app/features/auth/state/AccountStorageKey';
import {
	type ClassifiedStoredAccount,
	type ReadyStoredAccountEntry,
	type ReplaceableStoredAccountEntry,
	type StoredAccountClassificationInput,
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	type StoredAccountRuntimeRecoveryCandidate,
	StoredAccountRuntimeRecoveryError,
	type UnavailableStoredAccount,
	type UnqualifiedStoredAccount,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	bestEffortStoredAccountIdentity,
	createQualifiedStoredAccount,
	type DecodedStoredAccountRecord,
	decodeQualifiedStoredAccount,
	decodeStoredAccountRecord,
	storedAccountKeyNamesInstance,
	storedAccountLegacyInstanceKey,
} from '@app/features/auth/state/StoredAccountRecord';
import {
	createStoredAccountRevision,
	storedAccountRevisionsAreSame,
	storedAccountRevisionValue,
} from '@app/features/auth/state/StoredAccountRevision';

function unavailableStoredAccount(input: StoredAccountClassificationInput, error: unknown): UnavailableStoredAccount {
	const identity = bestEffortStoredAccountIdentity(input.value, input.authoritativeStorageKey);
	return {
		kind: 'unavailable',
		...identity,
		error:
			error instanceof StoredAccountRuntimeRecoveryError
				? error
				: new StoredAccountRuntimeRecoveryError('Stored account classification failed', {cause: error}),
	};
}

function persistedKeyAllowsStorageKey(
	decoded: DecodedStoredAccountRecord,
	input: StoredAccountClassificationInput,
	storageKey: string,
	instanceKey: string,
): boolean {
	if (decoded.persistedStorageKey === null || decoded.persistedStorageKey === storageKey) {
		return true;
	}
	return (
		input.authoritativeStorageKey === null &&
		storedAccountKeyNamesInstance(decoded.persistedStorageKey, decoded.data.userId, instanceKey)
	);
}

export function classifyStoredAccount(input: StoredAccountClassificationInput): ClassifiedStoredAccount {
	try {
		const revision = createStoredAccountRevision(input.value, input.source);
		const decoded = decodeStoredAccountRecord(
			storedAccountRevisionValue(revision, input.source),
			input.authoritativeStorageKey,
		);
		if (decoded.instance !== undefined) {
			try {
				const instance = requireRuntimeConfigSnapshot(decoded.instance);
				if (storedInstanceKey(instance.apiEndpoint) === runtimeInstanceKey(instance)) {
					const derivedStorageKey = accountStorageKey(decoded.data.userId, instance);
					if (
						derivedStorageKey === null ||
						(decoded.persistedStorageKey !== null && decoded.persistedStorageKey !== derivedStorageKey)
					) {
						throw new StoredAccountRuntimeRecoveryError('Stored account runtime does not match its storage identity');
					}
					return {
						kind: 'ready',
						record: createQualifiedStoredAccount(decoded.data, instance, derivedStorageKey),
						revision,
					};
				}
			} catch (error) {
				if (!(error instanceof InvalidRuntimeConfigSnapshotError)) {
					throw error;
				}
			}
		}

		const instanceKey = storedAccountLegacyInstanceKey(decoded);
		if (instanceKey === null) {
			return {kind: 'unqualified', data: decoded.data, reason: 'missing-instance-identity'};
		}
		const storageKey = accountStorageKeyFromInstanceKey(decoded.data.userId, instanceKey);
		if (storageKey === null || !persistedKeyAllowsStorageKey(decoded, input, storageKey, instanceKey)) {
			throw new StoredAccountRuntimeRecoveryError('Stored account legacy runtime does not match its storage identity');
		}
		return {kind: 'runtime-recovery', data: decoded.data, storageKey, instanceKey, revision};
	} catch (error) {
		return unavailableStoredAccount(input, error);
	}
}

export function createStoredAccountInventory(
	source: StoredAccountSource,
	entries: ReadonlyArray<ClassifiedStoredAccount>,
): StoredAccountInventory {
	const readyEntries: Array<ReadyStoredAccountEntry> = [];
	const runtimeRecoveryCandidates: Array<StoredAccountRuntimeRecoveryCandidate> = [];
	const unqualifiedRecords: Array<UnqualifiedStoredAccount> = [];
	const unavailableRecords: Array<UnavailableStoredAccount> = [];
	for (const entry of entries) {
		switch (entry.kind) {
			case 'ready':
				readyEntries.push(entry);
				break;
			case 'runtime-recovery':
				runtimeRecoveryCandidates.push(entry);
				break;
			case 'unqualified':
				unqualifiedRecords.push(entry);
				break;
			case 'unavailable':
				unavailableRecords.push(entry);
				break;
		}
	}
	return {source, readyEntries, runtimeRecoveryCandidates, unqualifiedRecords, unavailableRecords};
}

export function storedAccountRecordsFromInventory(inventory: StoredAccountInventory): Array<StoredAccount> {
	return [...inventory.readyEntries, ...inventory.runtimeRecoveryCandidates, ...inventory.unqualifiedRecords].flatMap(
		(entry) => {
			const record = storedAccountRecordFromClassifiedEntry(entry);
			return record === null ? [] : [record];
		},
	);
}

export function storedAccountRecordFromClassifiedEntry(entry: ClassifiedStoredAccount): StoredAccount | null {
	switch (entry.kind) {
		case 'ready':
			return entry.record;
		case 'runtime-recovery':
			return {...entry.data, storageKey: entry.storageKey};
		case 'unqualified':
			return entry.data;
		case 'unavailable':
			return null;
	}
}

export function inventoryEntryStorageKey(entry: ReplaceableStoredAccountEntry): string {
	return entry.kind === 'ready' ? entry.record.storageKey : entry.storageKey;
}

export function normalizeStoredAccountInventoryReplacement(
	request: StoredAccountInventoryReplacement,
): QualifiedStoredAccount {
	const expectedStorageKey = inventoryEntryStorageKey(request.expected);
	const expectedUserId =
		request.expected.kind === 'ready' ? request.expected.record.userId : request.expected.data.userId;
	let replacement: QualifiedStoredAccount;
	try {
		replacement = decodeQualifiedStoredAccount(request.replacement, request.replacement.storageKey);
	} catch (error) {
		if (error instanceof StoredAccountRuntimeRecoveryError) {
			throw error;
		}
		throw new StoredAccountRuntimeRecoveryError('Stored account replacement is invalid', {cause: error});
	}
	if (
		replacement.storageKey !== expectedStorageKey ||
		replacement.userId !== expectedUserId ||
		accountStorageKey(replacement.userId, replacement.instance) !== expectedStorageKey
	) {
		throw new StoredAccountRuntimeRecoveryError('Stored account replacement changed account identity');
	}
	return replacement;
}

export function inventoryEntriesHaveSameRevision(
	left: ReplaceableStoredAccountEntry,
	right: ReplaceableStoredAccountEntry,
): boolean {
	return (
		left.kind === right.kind &&
		inventoryEntryStorageKey(left) === inventoryEntryStorageKey(right) &&
		storedAccountRevisionsAreSame(left.revision, right.revision)
	);
}
