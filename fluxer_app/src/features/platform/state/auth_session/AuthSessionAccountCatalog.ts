// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeConfigSnapshotsAreSameInstance} from '@app/features/app/state/RuntimeConfig';
import type {
	QualifiedStoredAccount,
	StoredAccountData,
	StoredAccountInventory,
	StoredAccountSource,
} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey, parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import type {Account} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AuthSessionAccountCatalog');

export class AccountInstanceMismatchError extends Error {
	constructor(accountKey: string) {
		super(`Account ${accountKey} does not belong to the current instance`);
		this.name = 'AccountInstanceMismatchError';
	}
}

export interface ResolvedAccount {
	readonly accountKey: string;
	readonly account: Account;
}

export interface ResolvedAccountWithInstance extends ResolvedAccount {
	readonly instance: RuntimeConfigSnapshot;
}

export interface LoadedAccountCatalog {
	readonly accounts: ReadonlyArray<Account>;
	readonly recordCount: number;
	readonly source: StoredAccountSource;
}

export interface ResolvedAccountRestoration {
	readonly kind: 'account';
	readonly accountKey: string;
	readonly account: Account;
	readonly source: StoredAccountSource;
	readonly usedMirroredCredential: boolean;
}

export interface UnresolvedAccountRestoration {
	readonly kind: 'unresolved';
	readonly reason: 'missing-account';
}

export type AccountRestorationResolution = ResolvedAccountRestoration | UnresolvedAccountRestoration;

export interface AuthSessionAccountCatalogDependencies {
	readonly getRuntimeSnapshot: () => RuntimeConfigSnapshot | null;
	readonly allowsCrossInstanceSwitching: () => boolean;
}

export class AuthSessionAccountCatalog {
	constructor(private readonly dependencies: AuthSessionAccountCatalogDependencies) {}

	fromInventory(inventory: StoredAccountInventory): LoadedAccountCatalog {
		const accounts: Array<Account> = [];
		for (const entry of inventory.readyEntries) {
			const account = this.accountFromQualifiedRecord(entry.record);
			if (account !== null) {
				accounts.push(account);
			}
		}
		for (const candidate of inventory.runtimeRecoveryCandidates) {
			const account = this.accountFromData(candidate.data, candidate.storageKey);
			if (account !== null) {
				accounts.push(account);
			}
		}
		for (const unqualified of inventory.unqualifiedRecords) {
			const account = this.accountFromData(unqualified.data, unqualified.data.userId);
			if (account !== null) {
				accounts.push(account);
			}
		}
		for (const unavailable of inventory.unavailableRecords) {
			logger.error(
				`Stored account ${unavailable.storageKey ?? unavailable.userId ?? 'with unknown identity'} is unavailable`,
				unavailable.error,
			);
		}
		return {
			accounts,
			recordCount:
				inventory.readyEntries.length +
				inventory.runtimeRecoveryCandidates.length +
				inventory.unqualifiedRecords.length +
				inventory.unavailableRecords.length,
			source: inventory.source,
		};
	}

	accountFromQualifiedRecord(record: QualifiedStoredAccount): Account | null {
		const storageKey = accountStorageKey(record.userId, record.instance);
		if (storageKey === null || record.storageKey !== storageKey) {
			logger.error(`Stored account ${record.storageKey} has an invalid runtime identity`);
			return null;
		}
		const account = this.accountFromData(record, storageKey);
		return account === null ? null : {...account, instance: record.instance};
	}

	resolve(accounts: ReadonlyArray<Account>, accountKey: string): ResolvedAccount | null {
		const account = accounts.find((candidate) => candidate.storageKey === accountKey);
		if (account !== undefined) {
			return {accountKey, account};
		}
		if (parseAccountStorageKey(accountKey) !== null) {
			return null;
		}
		return this.resolveByUserId(accounts, accountKey);
	}

	requireSwitchTarget(
		accounts: ReadonlyArray<Account>,
		currentAccountKey: string | null,
		accountKey: string,
	): ResolvedAccountWithInstance {
		const resolved = this.resolve(accounts, accountKey);
		if (resolved === null) {
			throw new Error(`No account found for ${accountKey}`);
		}
		const instance = resolved.account.instance;
		if (instance === undefined) {
			throw new AccountInstanceMismatchError(resolved.accountKey);
		}
		const activeInstance = this.dependencies.getRuntimeSnapshot();
		if (
			resolved.accountKey !== currentAccountKey &&
			activeInstance !== null &&
			!this.dependencies.allowsCrossInstanceSwitching() &&
			!runtimeConfigSnapshotsAreSameInstance(instance, activeInstance)
		) {
			throw new AccountInstanceMismatchError(resolved.accountKey);
		}
		return {...resolved, instance};
	}

	resolveRestoration(
		catalog: LoadedAccountCatalog,
		accountKey: string,
		usedMirroredCredential: boolean,
	): AccountRestorationResolution {
		const resolved = this.resolve(catalog.accounts, accountKey);
		if (resolved === null || resolved.account.instance === undefined) {
			return {kind: 'unresolved', reason: 'missing-account'};
		}
		return {
			kind: 'account',
			accountKey: resolved.accountKey,
			account: resolved.account,
			source: catalog.source,
			usedMirroredCredential,
		};
	}

	private accountFromData(data: StoredAccountData, storageKey: string): Account | null {
		if (data.token === null || data.token.length === 0) {
			logger.warn(`Skipping stored account for ${data.userId} because token is missing`);
			return null;
		}
		return {
			storageKey,
			userId: data.userId,
			token: data.token,
			userData: data.userData,
			presenceIntent: data.presenceIntent,
			lastActive: data.lastActive,
			isValid: data.isValid ?? true,
		};
	}

	private resolveByUserId(accounts: ReadonlyArray<Account>, userId: string): ResolvedAccount | null {
		const matches = accounts.filter((account) => account.userId === userId);
		if (matches.length === 1) {
			const [account] = matches;
			return {accountKey: account.storageKey, account};
		}
		const currentInstance = this.dependencies.getRuntimeSnapshot();
		if (currentInstance === null) {
			return null;
		}
		const currentMatches = matches.filter((candidate) =>
			runtimeConfigSnapshotsAreSameInstance(candidate.instance, currentInstance),
		);
		if (currentMatches.length !== 1) {
			return null;
		}
		const [account] = currentMatches;
		return {accountKey: account.storageKey, account};
	}
}
