// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
} from '@app/features/app/state/InstanceSnapshotStore';
import {
	type AccountPresenceIntent,
	type AccountRekeyContext,
	type AccountRekeyOutcome,
	DesktopAccountStorageAuthorityError,
	normalizeStoredAccount,
	planAccountRekey,
	type QualifiedStoredAccount,
	type StoredAccount,
	type StoredAccountList,
	type UserData,
} from '@app/features/auth/state/AccountStorageContract';
import {accountKeyUserId, accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {BrowserAccountStorageRepository} from '@app/features/auth/state/BrowserAccountStorageRepository';
import {
	type DesktopAccountStorageRepository,
	resolveDesktopAccountStorageRepository,
} from '@app/features/auth/state/DesktopAccountStorageRepository';
import type {
	StoredAccountInventory,
	StoredAccountInventoryReplacement,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {storedAccountRecordsFromInventory} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import {Logger} from '@app/features/platform/utils/AppLogger';

export {
	type AccountPresenceIntent,
	type AccountRekeyContext,
	type AccountRekeyOutcome,
	type AccountRekeyPlan,
	type AccountRekeyTarget,
	type BrowserAccountStorageOperation,
	BrowserAccountStorageTimeoutError,
	BrowserAccountStorageUnavailableError,
	CrossInstanceAccountCollisionError,
	DesktopAccountStorageAuthorityError,
	type KeyedStoredAccount,
	planAccountRekey,
	type QualifiedStoredAccount,
	type StoredAccount,
	type StoredAccountData,
	type StoredAccountList,
	type StoredAccountSource,
	type UserData,
} from '@app/features/auth/state/AccountStorageContract';

export {
	type ReadyStoredAccountEntry,
	type ReplaceableStoredAccountEntry,
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	StoredAccountInventoryReplacementConflictError,
	type StoredAccountRevision,
	type StoredAccountRuntimeRecoveryCandidate,
	StoredAccountRuntimeRecoveryError,
	type UnavailableStoredAccount,
	type UnqualifiedStoredAccount,
} from '@app/features/auth/state/StoredAccountInventoryContract';

const logger = new Logger('AccountStorage');

export class StoredAccountInstanceMismatchError extends Error {
	constructor(accountKey: string) {
		super(`Stored account ${accountKey} does not belong to the current instance`);
		this.name = 'StoredAccountInstanceMismatchError';
	}
}

type ActiveAccountRepository =
	| {readonly source: 'desktop'; readonly repository: DesktopAccountStorageRepository}
	| {readonly source: 'idb'; readonly repository: BrowserAccountStorageRepository};

class AccountStorage {
	private readonly browserRepository = new BrowserAccountStorageRepository();

	async init(): Promise<void> {
		const active = this.activeRepository();
		if (active.source === 'idb') {
			await active.repository.init();
		}
	}

	async stashAccountData(
		userId: string,
		token: string | null,
		userData?: UserData,
		instance?: RuntimeConfigSnapshot,
		presenceIntent?: AccountPresenceIntent | null,
	): Promise<void> {
		if (!userId) {
			const error = new Error('Invalid stashAccountData: missing userId');
			logger.error('Invalid parameters for stashAccountData', error);
			throw error;
		}
		if (!token) {
			const error = new Error(`Invalid stashAccountData: missing token for ${userId}`);
			logger.error('Invalid parameters for stashAccountData', error);
			throw error;
		}

		const active = this.activeRepository();
		const storageKey = instance != null ? accountStorageKey(userId, instance) : null;
		if (active.source === 'desktop' && instance != null && storageKey === null) {
			throw new DesktopAccountStorageAuthorityError(`Cannot stash desktop account ${userId} without a usable instance`);
		}
		let existing = await active.repository.read(storageKey ?? userId);
		if (active.source === 'idb' && existing === null && storageKey !== null) {
			existing = await active.repository.read(userId);
		}
		const resolvedInstance = instance ?? existing?.instance;
		const resolvedStorageKey = storageKey ?? existing?.storageKey;
		if (active.source === 'desktop' && (resolvedInstance == null || resolvedStorageKey == null)) {
			throw new DesktopAccountStorageAuthorityError(
				`Cannot stash desktop account ${userId} without an existing qualified account key or instance`,
			);
		}
		const managedStorageData = existing?.managedStorageData ?? existing?.localStorageData ?? {};
		const record = normalizeStoredAccount({
			...existing,
			userId,
			token,
			userData: userData ?? existing?.userData,
			presenceIntent: presenceIntent === undefined ? (existing?.presenceIntent ?? undefined) : presenceIntent,
			localStorageData: managedStorageData,
			managedStorageData,
			lastActive: Date.now(),
			instance: resolvedInstance,
			isValid: existing?.token === token ? existing.isValid : true,
			storageKey: resolvedStorageKey,
		});
		if (active.source === 'desktop') {
			await active.repository.put(record);
		} else if (instance !== undefined || existing === null || existing.instance == null) {
			await active.repository.putReplacingInstance(record);
		} else {
			await active.repository.putPreservingInstance(record);
		}
		logger.debug(`Stashed account data for ${userId}`);
	}

	async upsertAccount(record: StoredAccount, currentInstance: RuntimeConfigSnapshot): Promise<void> {
		if (!record.userId) {
			throw new Error('Invalid upsertAccount: missing userId');
		}
		const normalized = normalizeStoredAccount(record);
		const active = this.activeRepository();
		if (active.source === 'desktop') {
			await active.repository.put(normalized);
			return;
		}
		if (runtimeConfigSnapshotsAreSameInstance(normalized.instance, currentInstance)) {
			await active.repository.putReplacingInstance(normalized);
			return;
		}
		await active.repository.putPreservingInstance(normalized);
	}

	async restoreAccountData(
		accountKey: string,
		expectedInstance?: RuntimeConfigSnapshot,
	): Promise<StoredAccount | null> {
		if (!accountKey) {
			return null;
		}
		const active = this.activeRepository();
		const record = await active.repository.read(accountKey);
		if (record === null) {
			return null;
		}
		if (expectedInstance !== undefined && !runtimeConfigSnapshotsAreSameInstance(record.instance, expectedInstance)) {
			throw new StoredAccountInstanceMismatchError(accountKey);
		}
		await this.updateLastActive(active, accountKey);
		logger.debug(`Restored account data for ${accountKey}`);
		return record;
	}

	async getAllAccounts(): Promise<StoredAccountList> {
		const inventory = await this.getAccountInventory();
		return {records: storedAccountRecordsFromInventory(inventory), source: inventory.source};
	}

	async getAccountInventory(): Promise<StoredAccountInventory> {
		return await this.activeRepository().repository.listInventory();
	}

	async replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount> {
		return await this.activeRepository().repository.replaceInventoryEntry(request);
	}

	async migrateAccountStorageKeys(context: AccountRekeyContext): Promise<AccountRekeyOutcome> {
		const records = await this.browserRepository.list();
		const plan = planAccountRekey(records, context);
		const status = plan.deferredRecords.length === 0 ? 'complete' : 'deferred';
		const before = new Map(records.map((record) => [record.userId, record]));
		const changed = plan.qualifiedRecords.filter((record) => {
			const previous = before.get(record.userId);
			return previous?.storageKey !== record.storageKey || previous?.instance !== record.instance;
		});
		if (changed.length === 0) {
			return {status, source: 'idb', ...plan, written: 0};
		}
		await this.browserRepository.writeMigration(changed);
		logger.debug(
			`Re-keyed ${changed.length} stored accounts, ${plan.qualifiedRecords.length} qualified and ${plan.deferredRecords.length} deferred`,
		);
		return {status, source: 'idb', ...plan, written: changed.length};
	}

	async importAccounts(records: ReadonlyArray<StoredAccount>): Promise<void> {
		const active = this.activeRepository();
		for (const record of records) {
			if (!record.userId) {
				continue;
			}
			const normalized = normalizeStoredAccount(record);
			if (active.source === 'desktop') {
				await active.repository.put(normalized);
			} else {
				await active.repository.putReplacingInstance(normalized);
			}
		}
	}

	async deleteAccount(accountKey: string): Promise<void> {
		if (!accountKey) {
			return;
		}
		const active = this.activeRepository();
		await active.repository.delete(accountKey);
		if (active.source === 'desktop') {
			logger.debug(`Deleted account data for ${accountKey} (desktop)`);
			return;
		}
		logger.debug(`Deleted account data for ${accountKeyUserId(accountKey)}`);
	}

	async updateAccountUserData(accountKey: string, userData: UserData): Promise<void> {
		await this.updateAccountRecord(accountKey, 'user data', (record) => ({...record, userData}));
	}

	async updateAccountValidity(accountKey: string, isValid: boolean, expectedToken?: string): Promise<void> {
		await this.updateAccountRecord(accountKey, 'validity', (record) =>
			expectedToken !== undefined && record.token !== expectedToken ? record : {...record, isValid},
		);
	}

	private activeRepository(): ActiveAccountRepository {
		const desktopRepository = resolveDesktopAccountStorageRepository();
		if (desktopRepository !== null) {
			return {source: 'desktop', repository: desktopRepository};
		}
		return {source: 'idb', repository: this.browserRepository};
	}

	private async updateAccountRecord(
		accountKey: string,
		label: string,
		update: (record: StoredAccount) => StoredAccount,
	): Promise<void> {
		if (!accountKey) {
			return;
		}
		const active = this.activeRepository();
		if (active.source === 'desktop') {
			await active.repository.update(accountKey, label, update);
			return;
		}
		const record = await active.repository.read(accountKey);
		if (record !== null) {
			await active.repository.putPreservingInstance(update(record));
		}
	}

	private async updateLastActive(active: ActiveAccountRepository, accountKey: string): Promise<void> {
		const record = await active.repository.read(accountKey);
		if (record === null) {
			return;
		}
		const updated = {...record, lastActive: Date.now()};
		if (active.source === 'desktop') {
			await active.repository.put(updated);
			return;
		}
		await active.repository.putPreservingInstance(updated);
	}
}

export default new AccountStorage();
