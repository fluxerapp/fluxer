// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DesktopAccountStorageAuthorityError,
	normalizeStoredAccount,
	type QualifiedStoredAccount,
	type StoredAccount,
} from '@app/features/auth/state/AccountStorageContract';
import {parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	StoredAccountInventoryReplacementConflictError,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	classifyStoredAccount,
	createStoredAccountInventory,
	inventoryEntryStorageKey,
	normalizeStoredAccountInventoryReplacement,
	storedAccountRecordFromClassifiedEntry,
} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import {storedAccountRevisionValue} from '@app/features/auth/state/StoredAccountRevision';
import {
	getDesktopAccountStorageAPI,
	requireDesktopAccountRecord,
} from '@app/features/platform/state/DesktopAccountStorageAccess';
import {desktopLegacyImportIsComplete} from '@app/features/platform/state/DesktopLegacyImportState';
import type {
	DesktopAccountRecord,
	DesktopAccountStorageAPI,
	DesktopStoreJSONObject,
	DesktopStoreJSONValue,
} from '@fluxer/desktop_ipc/src/AccountContract';

export class DesktopAccountStorageRepository {
	constructor(private readonly storage: DesktopAccountStorageAPI) {}

	async listInventory(): Promise<StoredAccountInventory> {
		try {
			return createStoredAccountInventory(
				'desktop',
				(await this.storage.getAll()).map((account) =>
					classifyStoredAccount({
						value: account.record,
						authoritativeStorageKey: account.storageKey,
						source: 'desktop',
					}),
				),
			);
		} catch (error) {
			throw this.authorityError('classify accounts', error);
		}
	}

	async read(accountKey: string): Promise<StoredAccount | null> {
		try {
			const record = await this.resolveRecord(accountKey);
			return record === null ? null : this.fromDesktopRecord(record);
		} catch (error) {
			throw this.authorityError(`read account ${accountKey}`, error);
		}
	}

	async put(record: StoredAccount): Promise<void> {
		try {
			await this.storage.upsert(this.toDesktopRecord(record));
		} catch (error) {
			throw this.authorityError(`write account ${record.userId}`, error);
		}
	}

	async replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount> {
		try {
			const replacement = normalizeStoredAccountInventoryReplacement(request);
			const storageKey = inventoryEntryStorageKey(request.expected);
			const expectedValue = storedAccountRevisionValue(request.expected.revision, 'desktop');
			if (expectedValue === null || typeof expectedValue !== 'object' || Array.isArray(expectedValue)) {
				throw new DesktopAccountStorageAuthorityError(`Desktop account ${storageKey} has an unusable revision`);
			}
			const replaced = await this.storage.compareAndSwap({
				expected: {storageKey, record: serializeDesktopStoreObject(expectedValue)},
				replacement: this.toDesktopRecord(replacement),
			});
			if (!replaced) {
				throw new StoredAccountInventoryReplacementConflictError(
					storageKey,
					`Stored account ${storageKey} changed before its inventory replacement`,
				);
			}
			return replacement;
		} catch (error) {
			throw this.authorityError('replace account inventory entry', error);
		}
	}

	async delete(accountKey: string): Promise<void> {
		try {
			const record = await this.resolveRecord(accountKey);
			if (record !== null) {
				await this.storage.delete(record.storageKey);
			} else if (parseAccountStorageKey(accountKey) !== null) {
				await this.storage.delete(accountKey);
			}
		} catch (error) {
			throw this.authorityError(`delete account ${accountKey}`, error);
		}
	}

	async update(accountKey: string, label: string, update: (record: StoredAccount) => StoredAccount): Promise<void> {
		try {
			const existing = await this.resolveRecord(accountKey);
			if (existing === null) {
				return;
			}
			await this.storage.upsert(this.toDesktopRecord(update(this.fromDesktopRecord(existing))));
		} catch (error) {
			throw this.authorityError(`update ${label} for account ${accountKey}`, error);
		}
	}

	private async resolveRecord(accountKey: string): Promise<DesktopAccountRecord | null> {
		if (parseAccountStorageKey(accountKey) !== null) {
			return this.storage.get(accountKey);
		}
		const records = await this.storage.getAll();
		const matches = records.filter((record) => this.recordUserId(record) === accountKey);
		if (matches.length > 1) {
			throw new DesktopAccountStorageAuthorityError(
				`Desktop account user ID ${accountKey} matches multiple storage keys`,
			);
		}
		return matches[0] ?? null;
	}

	private fromDesktopRecord(account: DesktopAccountRecord): StoredAccount {
		const record = storedAccountRecordFromClassifiedEntry(
			classifyStoredAccount({
				value: account.record,
				authoritativeStorageKey: account.storageKey,
				source: 'desktop',
			}),
		);
		if (record === null) {
			throw new DesktopAccountStorageAuthorityError(`Desktop account ${account.storageKey} is unavailable`);
		}
		return record;
	}

	private recordUserId(account: DesktopAccountRecord): string | null {
		const classified = classifyStoredAccount({
			value: account.record,
			authoritativeStorageKey: account.storageKey,
			source: 'desktop',
		});
		switch (classified.kind) {
			case 'ready':
				return classified.record.userId;
			case 'runtime-recovery':
			case 'unqualified':
				return classified.data.userId;
			case 'unavailable':
				return classified.userId;
		}
	}

	private toDesktopRecord(record: StoredAccount): DesktopAccountRecord {
		const normalized = normalizeStoredAccount(record);
		const storageKey = normalized.storageKey;
		if (storageKey === undefined) {
			throw new Error(`Cannot store desktop account ${record.userId} without a storage key`);
		}
		const account = {
			storageKey,
			record: serializeDesktopStoreObject(normalized),
		};
		requireDesktopAccountRecord(account);
		return account;
	}

	private authorityError(
		operation: string,
		error: unknown,
	): DesktopAccountStorageAuthorityError | StoredAccountInventoryReplacementConflictError {
		if (
			error instanceof DesktopAccountStorageAuthorityError ||
			error instanceof StoredAccountInventoryReplacementConflictError
		) {
			return error;
		}
		return new DesktopAccountStorageAuthorityError(`Desktop account storage could not ${operation}`, {cause: error});
	}
}

class DesktopAccountSerializationError extends TypeError {
	constructor(path: string, value: unknown) {
		super(`Desktop account value at ${path} is not JSON-compatible: ${typeof value}`);
		this.name = 'DesktopAccountSerializationError';
	}
}

function serializeDesktopStoreObject(value: object, path = 'account'): DesktopStoreJSONObject {
	const prototype = Object.getPrototypeOf(value);
	if (prototype !== Object.prototype && prototype !== null) {
		throw new DesktopAccountSerializationError(path, value);
	}
	const serialized: DesktopStoreJSONObject = Object.create(null);
	for (const [key, entry] of Object.entries(value)) {
		if (entry === undefined) {
			continue;
		}
		serialized[key] = serializeDesktopStoreValue(entry, `${path}.${key}`);
	}
	return serialized;
}

function serializeDesktopStoreValue(value: unknown, path: string): DesktopStoreJSONValue {
	if (value === null || typeof value === 'string' || typeof value === 'boolean') {
		return value;
	}
	if (typeof value === 'number') {
		if (!Number.isFinite(value)) {
			throw new DesktopAccountSerializationError(path, value);
		}
		return value;
	}
	if (Array.isArray(value)) {
		return value.map((entry, index) => serializeDesktopStoreValue(entry, `${path}[${index}]`));
	}
	if (typeof value === 'object') {
		return serializeDesktopStoreObject(value, path);
	}
	throw new DesktopAccountSerializationError(path, value);
}

export function resolveDesktopAccountStorageRepository(): DesktopAccountStorageRepository | null {
	if (!desktopLegacyImportIsComplete()) {
		return null;
	}
	const storage = getDesktopAccountStorageAPI({allowUnavailable: true});
	if (storage === null) {
		throw new DesktopAccountStorageAuthorityError(
			'Desktop account storage is unavailable after legacy import completion',
		);
	}
	return new DesktopAccountStorageRepository(storage);
}
