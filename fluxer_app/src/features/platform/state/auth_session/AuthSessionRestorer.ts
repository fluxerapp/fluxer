// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountPresenceIntent} from '@app/features/auth/state/AccountStorage';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {
	AccountInstanceMismatchError,
	type AuthSessionAccountCatalog,
	type LoadedAccountCatalog,
	type ResolvedAccountRestoration,
} from '@app/features/platform/state/auth_session/AuthSessionAccountCatalog';
import type {AuthSessionAccountPersistence} from '@app/features/platform/state/auth_session/AuthSessionAccountPersistence';
import type {AuthSessionCheckpointManager} from '@app/features/platform/state/auth_session/AuthSessionCheckpoint';
import type {AuthSessionRuntimeCommitter} from '@app/features/platform/state/auth_session/AuthSessionRuntimeCommitter';
import type {Account} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import type {
	ActiveStoredAccountUnresolvedReason,
	AuthSessionStoredAccountResolver,
} from '@app/features/platform/state/auth_session/AuthSessionStoredAccountResolver';
import type {
	SessionCredentialMirror,
	StoredSessionMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';

export type AuthSessionSignedOutReason = 'no-session-pointer' | ActiveStoredAccountUnresolvedReason;

export type AuthSessionInitialization =
	| {
			readonly kind: 'signed-out';
			readonly catalog: LoadedAccountCatalog;
			readonly reason: AuthSessionSignedOutReason;
	  }
	| {
			readonly kind: 'stored-session';
			readonly catalog: LoadedAccountCatalog;
			readonly mirror: StoredSessionMirror;
			readonly resolution: ResolvedAccountRestoration;
	  };

export interface AuthSessionRestorationDependencies {
	readonly credentialMirror: SessionCredentialMirror;
	readonly restoreLocalPresenceIntent: (intent: AccountPresenceIntent | null | undefined) => void;
}

export class AuthSessionRestorer {
	constructor(
		private readonly dependencies: AuthSessionRestorationDependencies,
		private readonly accountCatalog: AuthSessionAccountCatalog,
		private readonly accountPersistence: AuthSessionAccountPersistence,
		private readonly checkpoints: AuthSessionCheckpointManager,
		private readonly runtimeCommitter: AuthSessionRuntimeCommitter,
		private readonly storedAccountResolver: AuthSessionStoredAccountResolver,
	) {}

	async load(): Promise<AuthSessionInitialization> {
		const mirror = this.dependencies.credentialMirror.read();
		const active = await this.storedAccountResolver.resolveActiveRestoration(mirror);
		const catalog = this.accountCatalog.fromInventory(active.inventory);
		if (active.kind !== 'resolved') {
			return {kind: 'signed-out', catalog, reason: active.reason};
		}
		const resolution = this.accountCatalog.resolveRestoration(
			catalog,
			active.accountKey,
			active.usedMirroredCredential,
		);
		if (resolution.kind !== 'account') {
			throw new Error(`Resolved stored account ${active.accountKey} is absent from the classified catalog`);
		}
		return {kind: 'stored-session', catalog, mirror, resolution};
	}

	async prepareAccount(accountKey: string): Promise<Account> {
		const record = await this.storedAccountResolver.prepareAccount(accountKey);
		const account = this.accountCatalog.accountFromQualifiedRecord(record);
		if (account === null) {
			throw new Error(`Prepared stored account ${accountKey} has no usable credential`);
		}
		return account;
	}

	async activate(
		restoration: ResolvedAccountRestoration,
		mirror: StoredSessionMirror,
		publish: () => void,
	): Promise<void> {
		const {account, accountKey} = restoration;
		const instance = account.instance;
		if (instance === undefined || getAccountKey({userId: account.userId, instance}) !== accountKey) {
			throw new AccountInstanceMismatchError(accountKey);
		}
		const checkpoint = await this.checkpoints.capture(mirror);
		await this.checkpoints.runBeforeCommit(
			checkpoint,
			async () => {
				this.checkpoints.markCommitStarted(checkpoint);
				await this.dependencies.credentialMirror.persist(this.accountPersistence.pointer(account));
				this.dependencies.restoreLocalPresenceIntent(account.presenceIntent ?? null);
				const preparedRuntime = await this.checkpoints.prepareAccountScope(instance, accountKey, checkpoint);
				await this.runtimeCommitter.commit(preparedRuntime, {
					publishSessionState: publish,
					rollbackSessionState: () => this.checkpoints.restoreSessionState(checkpoint),
				});
			},
			`restoring ${accountKey}`,
		);
	}
}
