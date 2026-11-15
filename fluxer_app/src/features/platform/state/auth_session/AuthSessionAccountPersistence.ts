// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {AuthSessionAccountStorage} from '@app/features/platform/state/auth_session/AuthSessionDependencies';
import type {Account} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import type {
	SessionCredentialMirror,
	StoredSessionMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';

export interface PersistableSessionAccount extends Account {
	readonly instance: RuntimeConfigSnapshot;
}

export interface SessionTokenPersistenceChange {
	readonly previousAccount: PersistableSessionAccount | null;
	readonly nextAccount: PersistableSessionAccount | null;
	readonly previousMirror: StoredSessionMirror;
	readonly nextMirror: StoredSessionMirror;
}

export interface AuthSessionAccountPersistenceDependencies {
	readonly accountStorage: AuthSessionAccountStorage;
	readonly credentialMirror: SessionCredentialMirror;
	readonly deleteAccountStorageScope: (accountKey: string) => Promise<void>;
}

export class AuthSessionAccountPersistence {
	constructor(private readonly dependencies: AuthSessionAccountPersistenceDependencies) {}

	pointer(account: Account): StoredSessionMirror {
		return {
			storageKey: account.storageKey,
			userId: account.userId,
			token: account.token,
		};
	}

	async stash(account: PersistableSessionAccount): Promise<void> {
		await this.dependencies.accountStorage.stashAccountData(
			account.userId,
			account.token,
			account.userData,
			account.instance,
			account.presenceIntent,
		);
	}

	async rotateStoredAccount(
		accountKey: string,
		account: PersistableSessionAccount,
		token: string,
		lastActive: number,
	): Promise<PersistableSessionAccount> {
		const updated = {...account, token, lastActive};
		if (getAccountKey(updated) !== accountKey) {
			throw new Error(`Token rotation changed account key from ${accountKey} to ${getAccountKey(updated)}`);
		}
		await this.stash(updated);
		return updated;
	}

	async persistSessionTokenChange(change: SessionTokenPersistenceChange): Promise<void> {
		this.dependencies.credentialMirror.write(change.nextMirror);
		try {
			if (change.nextAccount !== null) {
				await this.stash(change.nextAccount);
			}
			await this.dependencies.credentialMirror.persist(change.nextMirror);
		} catch (error) {
			const recoveryErrors: Array<unknown> = [];
			if (change.nextAccount !== null && change.previousAccount !== null) {
				try {
					await this.stash(change.previousAccount);
				} catch (recoveryError) {
					recoveryErrors.push(recoveryError);
				}
			}
			try {
				await this.dependencies.credentialMirror.persist(change.previousMirror);
			} catch (recoveryError) {
				recoveryErrors.push(recoveryError);
			}
			if (recoveryErrors.length > 0) {
				throw new AggregateError([error, ...recoveryErrors], 'Failed to persist and restore the rotated session token');
			}
			throw error;
		}
	}

	async delete(accountKey: string): Promise<void> {
		await this.dependencies.accountStorage.deleteAccount(accountKey);
		await this.dependencies.deleteAccountStorageScope(accountKey);
	}
}
