// SPDX-License-Identifier: AGPL-3.0-or-later

import type {InstanceSnapshotResolution} from '@app/features/app/state/InstanceSnapshotStore';
import type {QualifiedStoredAccount, StoredAccountData} from '@app/features/auth/state/AccountStorageContract';
import {accountStorageKey, parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {
	type ReadyStoredAccountEntry,
	type ReplaceableStoredAccountEntry,
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	StoredAccountInventoryReplacementConflictError,
	type StoredAccountRuntimeRecoveryCandidate,
	type UnqualifiedStoredAccount,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {inventoryEntryStorageKey} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import {
	parseStoredSessionRestorationPointer,
	type StoredSessionCredential,
	type StoredSessionMirror,
	type StoredSessionRestorationPointer,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';

export interface AuthSessionStoredAccountResolverStorage {
	getAccountInventory(): Promise<StoredAccountInventory>;
	replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount>;
}

export interface AuthSessionStoredAccountResolverDependencies {
	readonly accountStorage: AuthSessionStoredAccountResolverStorage;
	readonly resolveRuntimeEndpoint: (input: string) => Promise<InstanceSnapshotResolution>;
}

export interface ResolvedActiveStoredAccountRestoration {
	readonly kind: 'resolved';
	readonly inventory: StoredAccountInventory;
	readonly record: QualifiedStoredAccount;
	readonly accountKey: string;
	readonly usedMirroredCredential: boolean;
}

export interface SignedOutActiveStoredAccountRestoration {
	readonly kind: 'signed-out';
	readonly inventory: StoredAccountInventory;
	readonly reason: 'no-session-pointer';
}

export type ActiveStoredAccountUnresolvedReason =
	| 'ambiguous-account'
	| 'desktop-qualified-pointer-required'
	| 'invalid-pointer'
	| 'missing-account'
	| 'missing-credential'
	| 'runtime-identity-unavailable'
	| 'unavailable-account';

export interface UnresolvedActiveStoredAccountRestoration {
	readonly kind: 'unresolved';
	readonly inventory: StoredAccountInventory;
	readonly reason: ActiveStoredAccountUnresolvedReason;
}

export type ActiveStoredAccountRestoration =
	| ResolvedActiveStoredAccountRestoration
	| SignedOutActiveStoredAccountRestoration
	| UnresolvedActiveStoredAccountRestoration;

export class StoredAccountNotFoundError extends Error {
	readonly accountKey: string;

	constructor(accountKey: string) {
		super(`No stored account exists for exact key ${accountKey}`);
		this.name = 'StoredAccountNotFoundError';
		this.accountKey = accountKey;
	}
}

export class StoredAccountSelectionAmbiguityError extends Error {
	readonly accountKey: string;

	constructor(accountKey: string) {
		super(`Stored account key ${accountKey} identifies multiple inventory entries`);
		this.name = 'StoredAccountSelectionAmbiguityError';
		this.accountKey = accountKey;
	}
}

export class StoredAccountRuntimeIdentityUnavailableError extends Error {
	readonly accountKey: string;

	constructor(accountKey: string) {
		super(`Stored account ${accountKey} has no runtime identity for instance discovery`);
		this.name = 'StoredAccountRuntimeIdentityUnavailableError';
		this.accountKey = accountKey;
	}
}

export class StoredAccountRuntimeIdentityMismatchError extends Error {
	readonly accountKey: string;
	readonly expectedInstanceKey: string;
	readonly resolvedInstanceKey: string;

	constructor(accountKey: string, expectedInstanceKey: string, resolvedInstanceKey: string) {
		super(`Stored account ${accountKey} resolved runtime ${resolvedInstanceKey} instead of ${expectedInstanceKey}`);
		this.name = 'StoredAccountRuntimeIdentityMismatchError';
		this.accountKey = accountKey;
		this.expectedInstanceKey = expectedInstanceKey;
		this.resolvedInstanceKey = resolvedInstanceKey;
	}
}

export class StoredAccountStorageIdentityMismatchError extends Error {
	readonly accountKey: string;
	readonly derivedAccountKey: string | null;

	constructor(accountKey: string, derivedAccountKey: string | null) {
		super(`Stored account ${accountKey} resolved to storage identity ${derivedAccountKey ?? 'null'}`);
		this.name = 'StoredAccountStorageIdentityMismatchError';
		this.accountKey = accountKey;
		this.derivedAccountKey = derivedAccountKey;
	}
}

export class StoredAccountReplacementInvariantError extends Error {
	readonly accountKey: string;

	constructor(accountKey: string, message: string) {
		super(message);
		this.name = 'StoredAccountReplacementInvariantError';
		this.accountKey = accountKey;
	}
}

type SelectableStoredAccountEntry =
	| ReadyStoredAccountEntry
	| StoredAccountRuntimeRecoveryCandidate
	| UnqualifiedStoredAccount;

interface SelectedActiveStoredAccount {
	readonly kind: 'selected';
	readonly entry: SelectableStoredAccountEntry;
	readonly mirroredCredential: StoredSessionCredential | null;
}

interface SignedOutActiveStoredAccountSelection {
	readonly kind: 'signed-out';
	readonly reason: 'no-session-pointer';
}

interface UnresolvedActiveStoredAccountSelection {
	readonly kind: 'unresolved';
	readonly reason: ActiveStoredAccountUnresolvedReason;
}

type ActiveStoredAccountSelection =
	| SelectedActiveStoredAccount
	| SignedOutActiveStoredAccountSelection
	| UnresolvedActiveStoredAccountSelection;

type UniqueEntrySelection<Entry> =
	| {readonly kind: 'none'}
	| {readonly kind: 'ambiguous'}
	| {readonly kind: 'selected'; readonly entry: Entry};

function entryData(entry: SelectableStoredAccountEntry): StoredAccountData {
	return entry.kind === 'ready' ? entry.record : entry.data;
}

function replaceableEntries(inventory: StoredAccountInventory): Array<ReplaceableStoredAccountEntry> {
	return [...inventory.readyEntries, ...inventory.runtimeRecoveryCandidates];
}

function selectableEntries(inventory: StoredAccountInventory): Array<SelectableStoredAccountEntry> {
	return [...replaceableEntries(inventory), ...inventory.unqualifiedRecords];
}

function selectUnique<Entry>(entries: ReadonlyArray<Entry>): UniqueEntrySelection<Entry> {
	if (entries.length === 0) {
		return {kind: 'none'};
	}
	if (entries.length > 1) {
		return {kind: 'ambiguous'};
	}
	const [entry] = entries;
	if (entry === undefined) {
		throw new Error('Unique stored account selection has no entry');
	}
	return {kind: 'selected', entry};
}

function exactKeySelection(
	inventory: StoredAccountInventory,
	accountKey: string,
): UniqueEntrySelection<ReplaceableStoredAccountEntry> {
	return selectUnique(replaceableEntries(inventory).filter((entry) => inventoryEntryStorageKey(entry) === accountKey));
}

function exactCredentialSelection(
	inventory: StoredAccountInventory,
	credential: StoredSessionCredential,
): UniqueEntrySelection<SelectableStoredAccountEntry> {
	return selectUnique(
		selectableEntries(inventory).filter((entry) => {
			const data = entryData(entry);
			return data.userId === credential.userId && data.token === credential.token;
		}),
	);
}

function exactUserSelection(
	inventory: StoredAccountInventory,
	userId: string,
): UniqueEntrySelection<SelectableStoredAccountEntry> {
	return selectUnique(selectableEntries(inventory).filter((entry) => entryData(entry).userId === userId));
}

function unavailableKeyExists(inventory: StoredAccountInventory, accountKey: string): boolean {
	return inventory.unavailableRecords.some((entry) => entry.storageKey === accountKey);
}

function unavailableUserExists(inventory: StoredAccountInventory, userId: string): boolean {
	return inventory.unavailableRecords.some((entry) => entry.userId === userId);
}

function selectionResult(
	selection: UniqueEntrySelection<SelectableStoredAccountEntry>,
	mirroredCredential: StoredSessionCredential | null,
): SelectedActiveStoredAccount | UnresolvedActiveStoredAccountSelection | null {
	if (selection.kind === 'none') {
		return null;
	}
	if (selection.kind === 'ambiguous') {
		return {kind: 'unresolved', reason: 'ambiguous-account'};
	}
	return {kind: 'selected', entry: selection.entry, mirroredCredential};
}

function selectDesktopActiveAccount(
	inventory: StoredAccountInventory,
	pointer: StoredSessionRestorationPointer,
): ActiveStoredAccountSelection {
	if (pointer.kind === 'none') {
		return {kind: 'signed-out', reason: 'no-session-pointer'};
	}
	if (pointer.kind === 'invalid') {
		return {kind: 'unresolved', reason: 'invalid-pointer'};
	}
	if (pointer.kind === 'legacy-credential') {
		return {kind: 'unresolved', reason: 'desktop-qualified-pointer-required'};
	}
	const selected = selectionResult(exactKeySelection(inventory, pointer.storageKey), null);
	if (selected !== null) {
		return selected;
	}
	return {
		kind: 'unresolved',
		reason: unavailableKeyExists(inventory, pointer.storageKey) ? 'unavailable-account' : 'missing-account',
	};
}

function selectIdbCredentialAccount(
	inventory: StoredAccountInventory,
	credential: StoredSessionCredential,
): ActiveStoredAccountSelection {
	const exactCredential = selectionResult(exactCredentialSelection(inventory, credential), credential);
	if (exactCredential !== null) {
		return exactCredential;
	}
	const exactUser = selectionResult(exactUserSelection(inventory, credential.userId), credential);
	if (exactUser !== null) {
		return exactUser;
	}
	return {
		kind: 'unresolved',
		reason: unavailableUserExists(inventory, credential.userId) ? 'unavailable-account' : 'missing-account',
	};
}

function selectIdbActiveAccount(
	inventory: StoredAccountInventory,
	pointer: StoredSessionRestorationPointer,
): ActiveStoredAccountSelection {
	if (pointer.kind === 'none') {
		return {kind: 'signed-out', reason: 'no-session-pointer'};
	}
	if (pointer.kind === 'invalid') {
		return {kind: 'unresolved', reason: 'invalid-pointer'};
	}
	if (pointer.kind === 'legacy-credential') {
		return selectIdbCredentialAccount(inventory, pointer.credential);
	}
	const exactKey = selectionResult(exactKeySelection(inventory, pointer.storageKey), pointer.credential);
	if (exactKey !== null) {
		return exactKey;
	}
	return {
		kind: 'unresolved',
		reason: unavailableKeyExists(inventory, pointer.storageKey) ? 'unavailable-account' : 'missing-account',
	};
}

function selectActiveAccount(
	inventory: StoredAccountInventory,
	pointer: StoredSessionRestorationPointer,
): ActiveStoredAccountSelection {
	return inventory.source === 'desktop'
		? selectDesktopActiveAccount(inventory, pointer)
		: selectIdbActiveAccount(inventory, pointer);
}

function exactPreparedEntry(inventory: StoredAccountInventory, accountKey: string): ReplaceableStoredAccountEntry {
	const selection = exactKeySelection(inventory, accountKey);
	if (selection.kind === 'ambiguous') {
		throw new StoredAccountSelectionAmbiguityError(accountKey);
	}
	if (selection.kind === 'selected') {
		return selection.entry;
	}
	if (parseAccountStorageKey(accountKey) === null) {
		const unqualified = inventory.unqualifiedRecords.filter((entry) => entry.data.userId === accountKey);
		if (unqualified.length > 1) {
			throw new StoredAccountSelectionAmbiguityError(accountKey);
		}
		if (unqualified.length === 1) {
			throw new StoredAccountRuntimeIdentityUnavailableError(accountKey);
		}
	}
	throw new StoredAccountNotFoundError(accountKey);
}

export class AuthSessionStoredAccountResolver {
	constructor(private readonly dependencies: AuthSessionStoredAccountResolverDependencies) {}

	async resolveActiveRestoration(mirror: StoredSessionMirror): Promise<ActiveStoredAccountRestoration> {
		const pointer = parseStoredSessionRestorationPointer(mirror);
		for (let attempt = 0; attempt < 2; attempt += 1) {
			const inventory = await this.dependencies.accountStorage.getAccountInventory();
			const selection = selectActiveAccount(inventory, pointer);
			if (selection.kind === 'signed-out') {
				return {...selection, inventory};
			}
			if (selection.kind === 'unresolved') {
				return {...selection, inventory};
			}
			if (selection.entry.kind === 'unqualified') {
				return {kind: 'unresolved', inventory, reason: 'runtime-identity-unavailable'};
			}
			try {
				return await this.resolveSelectedActiveAccount(inventory, selection.entry, selection.mirroredCredential);
			} catch (error) {
				if (error instanceof StoredAccountInventoryReplacementConflictError && attempt === 0) {
					continue;
				}
				throw error;
			}
		}
		throw new Error('Stored account restoration exhausted its bounded conflict retry');
	}

	async prepareAccount(accountKey: string): Promise<QualifiedStoredAccount> {
		for (let attempt = 0; attempt < 2; attempt += 1) {
			const inventory = await this.dependencies.accountStorage.getAccountInventory();
			const entry = exactPreparedEntry(inventory, accountKey);
			if (entry.kind === 'ready') {
				return entry.record;
			}
			try {
				return await this.recoverRuntime(entry, entry.data.token);
			} catch (error) {
				if (error instanceof StoredAccountInventoryReplacementConflictError && attempt === 0) {
					continue;
				}
				throw error;
			}
		}
		throw new Error(`Stored account ${accountKey} preparation exhausted its bounded conflict retry`);
	}

	private async resolveSelectedActiveAccount(
		inventory: StoredAccountInventory,
		entry: ReplaceableStoredAccountEntry,
		mirroredCredential: StoredSessionCredential | null,
	): Promise<ActiveStoredAccountRestoration> {
		const selectedToken =
			inventory.source === 'idb' && mirroredCredential !== null
				? mirroredCredential.token
				: entry.kind === 'ready'
					? entry.record.token
					: entry.data.token;
		if (selectedToken === null) {
			return {kind: 'unresolved', inventory, reason: 'missing-credential'};
		}
		if (entry.kind === 'runtime-recovery') {
			const storedToken = entry.data.token;
			const replacement = await this.recoverRuntime(entry, selectedToken);
			return await this.resolvedAfterReplacement(replacement, storedToken !== selectedToken);
		}
		const storedToken = entry.record.token;
		if (inventory.source !== 'idb' || storedToken === selectedToken) {
			return {
				kind: 'resolved',
				inventory,
				record: entry.record,
				accountKey: entry.record.storageKey,
				usedMirroredCredential: false,
			};
		}
		const replacement = await this.dependencies.accountStorage.replaceInventoryEntry({
			expected: entry,
			replacement: {...entry.record, token: selectedToken},
		});
		return await this.resolvedAfterReplacement(replacement, true);
	}

	private async recoverRuntime(
		candidate: StoredAccountRuntimeRecoveryCandidate,
		token: string | null,
	): Promise<QualifiedStoredAccount> {
		const resolution = await this.dependencies.resolveRuntimeEndpoint(candidate.instanceKey);
		if (resolution.instanceKey !== candidate.instanceKey) {
			throw new StoredAccountRuntimeIdentityMismatchError(
				candidate.storageKey,
				candidate.instanceKey,
				resolution.instanceKey,
			);
		}
		const storageKey = accountStorageKey(candidate.data.userId, resolution.snapshot);
		if (storageKey !== candidate.storageKey) {
			throw new StoredAccountStorageIdentityMismatchError(candidate.storageKey, storageKey);
		}
		return await this.dependencies.accountStorage.replaceInventoryEntry({
			expected: candidate,
			replacement: {
				...candidate.data,
				token,
				instance: resolution.snapshot,
				storageKey,
			},
		});
	}

	private async resolvedAfterReplacement(
		replacement: QualifiedStoredAccount,
		usedMirroredCredential: boolean,
	): Promise<ResolvedActiveStoredAccountRestoration> {
		const inventory = await this.dependencies.accountStorage.getAccountInventory();
		const matches = inventory.readyEntries.filter(
			(entry) => entry.record.storageKey === replacement.storageKey && entry.record.token === replacement.token,
		);
		if (matches.length !== 1) {
			throw new StoredAccountReplacementInvariantError(
				replacement.storageKey,
				`Stored account ${replacement.storageKey} was not durably visible after replacement`,
			);
		}
		return {
			kind: 'resolved',
			inventory,
			record: matches[0].record,
			accountKey: replacement.storageKey,
			usedMirroredCredential,
		};
	}
}
