// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	QualifiedStoredAccount,
	StoredAccountData,
	StoredAccountSource,
} from '@app/features/auth/state/AccountStorageContract';

export interface StoredAccountRevision {
	readonly kind: 'stored-account-revision';
}

export interface ReadyStoredAccountEntry {
	readonly kind: 'ready';
	readonly record: QualifiedStoredAccount;
	readonly revision: StoredAccountRevision;
}

export interface StoredAccountRuntimeRecoveryCandidate {
	readonly kind: 'runtime-recovery';
	readonly data: StoredAccountData;
	readonly storageKey: string;
	readonly instanceKey: string;
	readonly revision: StoredAccountRevision;
}

export interface UnqualifiedStoredAccount {
	readonly kind: 'unqualified';
	readonly data: StoredAccountData;
	readonly reason: 'missing-instance-identity';
}

export interface UnavailableStoredAccount {
	readonly kind: 'unavailable';
	readonly storageKey: string | null;
	readonly userId: string | null;
	readonly error: StoredAccountRuntimeRecoveryError;
}

export type ReplaceableStoredAccountEntry = ReadyStoredAccountEntry | StoredAccountRuntimeRecoveryCandidate;

export type ClassifiedStoredAccount =
	| ReadyStoredAccountEntry
	| StoredAccountRuntimeRecoveryCandidate
	| UnqualifiedStoredAccount
	| UnavailableStoredAccount;

export interface StoredAccountInventory {
	readonly source: StoredAccountSource;
	readonly readyEntries: ReadonlyArray<ReadyStoredAccountEntry>;
	readonly runtimeRecoveryCandidates: ReadonlyArray<StoredAccountRuntimeRecoveryCandidate>;
	readonly unqualifiedRecords: ReadonlyArray<UnqualifiedStoredAccount>;
	readonly unavailableRecords: ReadonlyArray<UnavailableStoredAccount>;
}

export interface StoredAccountInventoryReplacement {
	readonly expected: ReplaceableStoredAccountEntry;
	readonly replacement: QualifiedStoredAccount;
}

export interface StoredAccountClassificationInput {
	readonly value: unknown;
	readonly authoritativeStorageKey: string | null;
	readonly source: StoredAccountSource;
}

export class StoredAccountRuntimeRecoveryError extends Error {
	constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'StoredAccountRuntimeRecoveryError';
	}
}

export class StoredAccountInventoryReplacementConflictError extends Error {
	readonly storageKey: string;

	constructor(storageKey: string, message: string) {
		super(message);
		this.name = 'StoredAccountInventoryReplacementConflictError';
		this.storageKey = storageKey;
	}
}
