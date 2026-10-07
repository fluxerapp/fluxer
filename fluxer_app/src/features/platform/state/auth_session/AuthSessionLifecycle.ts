// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {AccountPresenceIntent, UserData} from '@app/features/auth/state/AccountStorage';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {
	type AccountScopedWorkSuspension,
	AccountScopedWorkTransitionReason,
} from '@app/features/platform/state/AccountScopedWork';
import type {AuthSessionAccountCatalog} from '@app/features/platform/state/auth_session/AuthSessionAccountCatalog';
import type {
	AuthSessionAccountPersistence,
	PersistableSessionAccount,
} from '@app/features/platform/state/auth_session/AuthSessionAccountPersistence';
import type {AuthSessionCheckpointManager} from '@app/features/platform/state/auth_session/AuthSessionCheckpoint';
import {AuthSessionRuntimeUnavailableError} from '@app/features/platform/state/auth_session/AuthSessionCheckpoint';
import type {AuthSessionCleanup} from '@app/features/platform/state/auth_session/AuthSessionCleanup';
import type {AuthSessionDependencies} from '@app/features/platform/state/auth_session/AuthSessionDependencies';
import type {AuthSessionRuntimeCommitter} from '@app/features/platform/state/auth_session/AuthSessionRuntimeCommitter';
import {
	type Account,
	type AuthSessionMachineEvent,
	SessionState,
} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import type {
	AuthSessionTransitionContext,
	AuthSessionTransitionGate,
} from '@app/features/platform/state/auth_session/AuthSessionTransitionGate';
import {
	emptyStoredSessionMirror,
	type StoredSessionMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';
import {ResetClientStateReason} from '@app/features/platform/state/ResetClientState';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AuthSessionLifecycle');

export class SessionExpiredError extends Error {
	constructor(message?: string) {
		super(message ?? 'Session expired');
		this.name = 'SessionExpiredError';
	}
}

export interface AuthSessionSwitchOptions {
	readonly validatedToken?: string;
}

export interface AuthSessionValidatedSwitchOptions {
	readonly validatedToken: string;
}

export class AccountSwitchPreparedCredentialChangedError extends Error {
	constructor(accountKey: string) {
		super(`Prepared credentials changed before ${accountKey} could be activated`);
		this.name = 'AccountSwitchPreparedCredentialChangedError';
	}
}

export type AuthSessionTokenValidation =
	| {readonly kind: 'valid'}
	| {readonly kind: 'invalid'}
	| {readonly kind: 'unavailable'; readonly cause: Error};

class SessionTokenValidationUnavailableError extends Error {
	constructor(accountKey: string, cause: Error) {
		super(`Could not validate the session for ${accountKey}: ${cause.message}`, {cause});
		this.name = 'SessionTokenValidationUnavailableError';
	}
}

export interface AuthSessionLoginRequest {
	readonly token: string;
	readonly userId: string;
	readonly userData?: UserData;
	readonly runtimeSnapshot: RuntimeConfigSnapshot;
}

type AccountScopedWorkOwnership =
	| {readonly kind: 'lifecycle'}
	| {readonly kind: 'account-activation'; readonly suspension: AccountScopedWorkSuspension};

type AccountSwitchExecution =
	| {readonly kind: 'standalone'; readonly options: AuthSessionSwitchOptions}
	| {
			readonly kind: 'account-activation';
			readonly options: AuthSessionValidatedSwitchOptions;
			readonly suspension: AccountScopedWorkSuspension;
	  };

export interface AuthSessionLifecycleState {
	readonly getState: () => SessionState;
	readonly getCurrentAccountKey: () => string | null;
	readonly getAccounts: () => ReadonlyArray<Account>;
	readonly canSwitchAccount: () => boolean;
	readonly readCredentialMirror: () => StoredSessionMirror;
	readonly dispatch: (event: AuthSessionMachineEvent) => void;
	readonly stashCurrentAccount: (capturedPresenceIntent?: AccountPresenceIntent | null) => Promise<void>;
	readonly validateToken: (
		token: string,
		userId: string,
		instance: RuntimeConfigSnapshot,
	) => Promise<AuthSessionTokenValidation>;
	readonly markAccountInvalid: (accountKey: string, expectedToken?: string) => void;
}

function requireRuntimeSnapshot(snapshot: RuntimeConfigSnapshot | null, operation: string): RuntimeConfigSnapshot {
	if (snapshot == null) {
		throw new AuthSessionRuntimeUnavailableError(operation);
	}
	return snapshot;
}

export class AuthSessionLifecycle {
	constructor(
		private readonly dependencies: AuthSessionDependencies,
		private readonly state: AuthSessionLifecycleState,
		private readonly accountCatalog: AuthSessionAccountCatalog,
		private readonly accountPersistence: AuthSessionAccountPersistence,
		private readonly checkpoints: AuthSessionCheckpointManager,
		private readonly cleanup: AuthSessionCleanup,
		private readonly runtimeCommitter: AuthSessionRuntimeCommitter,
		private readonly transitionGate: AuthSessionTransitionGate,
	) {}

	private async requireValidToken(
		accountKey: string,
		userId: string,
		token: string,
		instance: RuntimeConfigSnapshot,
	): Promise<void> {
		const validation = await this.state.validateToken(token, userId, instance);
		switch (validation.kind) {
			case 'valid':
				return;
			case 'invalid':
				this.state.markAccountInvalid(accountKey, token);
				throw new SessionExpiredError();
			case 'unavailable':
				throw new SessionTokenValidationUnavailableError(accountKey, validation.cause);
		}
	}

	private async runWithAccountScopedWork<Result>(
		reason: AccountScopedWorkTransitionReason,
		ownership: AccountScopedWorkOwnership,
		operation: () => Promise<Result>,
	): Promise<Result> {
		if (ownership.kind === 'account-activation') {
			ownership.suspension.assertActive(reason);
			return await operation();
		}
		return await this.transitionGate.runAccountScoped(reason, () => operation());
	}

	async login(request: AuthSessionLoginRequest): Promise<void> {
		await this.loginWithAccountScopedWork(request, {kind: 'lifecycle'});
	}

	async loginWithinAccountActivation(
		request: AuthSessionLoginRequest,
		suspension: AccountScopedWorkSuspension,
	): Promise<void> {
		await this.loginWithAccountScopedWork(request, {kind: 'account-activation', suspension});
	}

	private async loginWithAccountScopedWork(
		{token, userId, userData, runtimeSnapshot}: AuthSessionLoginRequest,
		ownership: AccountScopedWorkOwnership,
	): Promise<void> {
		return await this.transitionGate.runAccountTransition(async () => {
			const instance = runtimeSnapshot;
			const storageKey = getAccountKey({userId, instance});
			const previousAccountKey = this.state.getCurrentAccountKey();
			const cleanupPreviousGateway = previousAccountKey !== null && previousAccountKey !== storageKey;
			const reason = AccountScopedWorkTransitionReason.ACCOUNT_SWITCH;
			const checkpoint = await this.checkpoints.capture(
				this.state.readCredentialMirror(),
				[previousAccountKey, storageKey].filter((key): key is string => key !== null),
			);
			const commitLogin = async (): Promise<void> => {
				const account = await this.checkpoints.runBeforeCommit(
					checkpoint,
					async () => {
						if (previousAccountKey !== storageKey) {
							if (cleanupPreviousGateway) {
								await this.state.stashCurrentAccount(checkpoint.presenceIntent);
							}
						}
						await this.checkpoints.refresh(checkpoint, this.state.readCredentialMirror());
						const existing = this.state.getAccounts().find((candidate) => candidate.storageKey === storageKey);
						const account = {
							storageKey,
							userId,
							token,
							userData: userData ?? existing?.userData,
							presenceIntent: existing?.presenceIntent,
							lastActive: this.dependencies.now(),
							instance,
							isValid: true,
						} satisfies PersistableSessionAccount;
						this.checkpoints.markCommitStarted(checkpoint);
						await this.dependencies.credentialMirror.persist(this.accountPersistence.pointer(account));
						await this.accountPersistence.stash(account);
						if (cleanupPreviousGateway) {
							await this.checkpoints.captureSyncedUserSettings(checkpoint);
							await this.dependencies.resetSyncedUserSettings(reason);
						}
						const runtime = await this.checkpoints.prepareAccountScope(
							account.instance,
							account.storageKey,
							checkpoint,
						);
						await this.runtimeCommitter.commit(runtime, {
							publishSessionState: () => this.state.dispatch({type: 'account.login', account}),
							rollbackSessionState: () => this.checkpoints.restoreSessionState(checkpoint),
						});
						return account;
					},
					`logging in as ${storageKey}`,
				);
				if (cleanupPreviousGateway) {
					await this.cleanup.retireSession(reason);
				}
				if (previousAccountKey !== storageKey) {
					this.dependencies.restoreLocalPresenceIntent(account.presenceIntent);
				}
			};
			if (previousAccountKey === storageKey) {
				await commitLogin();
				return;
			}
			await this.runWithAccountScopedWork(reason, ownership, commitLogin);
		});
	}

	async switchAccount(accountKey: string, options: AuthSessionSwitchOptions): Promise<void> {
		await this.switchAccountWithScopedWork(accountKey, {kind: 'standalone', options});
	}

	async switchAccountWithinAccountActivation(
		accountKey: string,
		options: AuthSessionValidatedSwitchOptions,
		suspension: AccountScopedWorkSuspension,
	): Promise<void> {
		await this.switchAccountWithScopedWork(accountKey, {kind: 'account-activation', options, suspension});
	}

	private async switchAccountWithScopedWork(accountKey: string, execution: AccountSwitchExecution): Promise<void> {
		return await this.transitionGate.runAccountTransition(async () => {
			const target = this.accountCatalog.requireSwitchTarget(
				this.state.getAccounts(),
				this.state.getCurrentAccountKey(),
				accountKey,
			);
			if (target.accountKey === this.state.getCurrentAccountKey()) {
				logger.debug('Already on requested account');
				return;
			}
			if (!target.account.isValid) {
				throw new SessionExpiredError();
			}
			const canActivateFromInvalidatedSession =
				execution.kind === 'account-activation' && this.state.getState() === SessionState.Idle;
			if (!this.state.canSwitchAccount() && !canActivateFromInvalidatedSession) {
				throw new Error(`Cannot switch from state: ${this.state.getState()}`);
			}
			if (execution.kind === 'account-activation' && execution.options.validatedToken !== target.account.token) {
				throw new AccountSwitchPreparedCredentialChangedError(target.accountKey);
			}
			if (execution.kind === 'standalone' && execution.options.validatedToken !== target.account.token) {
				await this.requireValidToken(target.accountKey, target.account.userId, target.account.token, target.instance);
			}
			const ownership: AccountScopedWorkOwnership =
				execution.kind === 'account-activation'
					? {kind: 'account-activation', suspension: execution.suspension}
					: {kind: 'lifecycle'};
			const reason = AccountScopedWorkTransitionReason.ACCOUNT_SWITCH;
			const checkpoint = await this.checkpoints.capture(
				this.state.readCredentialMirror(),
				[this.state.getCurrentAccountKey(), target.accountKey].filter((key): key is string => key !== null),
			);
			try {
				await this.runWithAccountScopedWork(reason, ownership, async () => {
					const nextAccount = await this.checkpoints.runBeforeCommit(
						checkpoint,
						async () => {
							await this.state.stashCurrentAccount(checkpoint.presenceIntent);
							const restored = await this.dependencies.accountStorage.restoreAccountData(target.accountKey);
							if (restored === null) {
								throw new Error(`No data found for ${target.accountKey}`);
							}
							const latestTarget = this.accountCatalog.requireSwitchTarget(
								this.state.getAccounts(),
								this.state.getCurrentAccountKey(),
								target.accountKey,
							);
							if (!latestTarget.account.isValid) {
								throw new SessionExpiredError();
							}
							let token: string;
							if (execution.kind === 'account-activation') {
								if (latestTarget.account.token !== execution.options.validatedToken) {
									throw new AccountSwitchPreparedCredentialChangedError(target.accountKey);
								}
								if (restored.token !== null && restored.token !== execution.options.validatedToken) {
									throw new AccountSwitchPreparedCredentialChangedError(target.accountKey);
								}
								token = execution.options.validatedToken;
							} else {
								token = restored.token ?? target.account.token;
								if (token !== target.account.token) {
									await this.requireValidToken(target.accountKey, target.account.userId, token, target.instance);
								}
							}
							const nextAccount = {
								...latestTarget.account,
								storageKey: target.accountKey,
								token,
								userData: restored.userData ?? target.account.userData,
								presenceIntent: restored.presenceIntent ?? target.account.presenceIntent ?? null,
								lastActive: this.dependencies.now(),
								instance: latestTarget.instance,
								isValid: true,
							} satisfies PersistableSessionAccount;
							await this.checkpoints.refresh(checkpoint, this.state.readCredentialMirror());
							this.state.dispatch({type: 'account.switch.start'});
							this.checkpoints.markCommitStarted(checkpoint);
							await this.dependencies.credentialMirror.persist(this.accountPersistence.pointer(nextAccount));
							await this.checkpoints.captureSyncedUserSettings(checkpoint);
							await this.dependencies.resetSyncedUserSettings(reason);
							await this.accountPersistence.stash(nextAccount);
							const runtime = await this.checkpoints.prepareAccountScope(
								nextAccount.instance,
								nextAccount.storageKey,
								checkpoint,
							);
							await this.runtimeCommitter.commit(runtime, {
								publishSessionState: () => this.state.dispatch({type: 'account.switch.complete', account: nextAccount}),
								rollbackSessionState: () => this.checkpoints.restoreSessionState(checkpoint),
							});
							return nextAccount;
						},
						`switching to ${target.accountKey}`,
					);
					await this.cleanup.retireSession(reason);
					this.dependencies.restoreLocalPresenceIntent(nextAccount.presenceIntent);
					const cleanupErrors = await this.cleanup.collect([
						() => this.dependencies.closeLayers(),
						() => this.dependencies.clearSudoToken(),
					]);
					this.cleanup.report(
						cleanupErrors,
						`Switched to ${target.accountKey} but failed to clean up the previous session`,
					);
				});
			} catch (error) {
				logger.error('Failed to switch account', error);
				if (this.state.getState() === SessionState.Switching) {
					this.state.dispatch({type: 'account.switch.failed'});
				}
				throw error;
			}
		});
	}

	async logout(): Promise<void> {
		return await this.transitionGate.runAccountTransition(async () => {
			const state = this.state.getState();
			if (
				state !== SessionState.Idle &&
				state !== SessionState.Authenticated &&
				state !== SessionState.Connecting &&
				state !== SessionState.Connected &&
				state !== SessionState.Error
			) {
				return;
			}
			const reason = AccountScopedWorkTransitionReason.LOGOUT;
			return await this.transitionGate.runAccountScoped(reason, async (transition) => {
				await this.removeCurrentSession(this.state.getCurrentAccountKey(), reason, true, transition);
			});
		});
	}

	async removeAccount(accountKey: string): Promise<void> {
		return await this.transitionGate.runAccountTransition(async () => {
			const resolved = this.accountCatalog.resolve(this.state.getAccounts(), accountKey);
			if (resolved === null) {
				throw new Error(`No account found for ${accountKey}`);
			}
			const reason = AccountScopedWorkTransitionReason.LOGOUT;
			if (this.state.getCurrentAccountKey() === resolved.accountKey) {
				await this.transitionGate.runAccountScoped(reason, async (transition) => {
					await this.removeCurrentSession(resolved.accountKey, reason, false, transition);
				});
				return;
			}
			await this.accountPersistence.delete(resolved.accountKey);
			this.state.dispatch({type: 'account.remove', accountKey: resolved.accountKey});
		});
	}

	private async removeCurrentSession(
		accountKey: string | null,
		reason: AccountScopedWorkTransitionReason,
		notifyServer: boolean,
		transition: AuthSessionTransitionContext,
	): Promise<void> {
		const logoutNotification =
			notifyServer && accountKey !== null
				? (() => {
						const account = this.accountCatalog.resolve(this.state.getAccounts(), accountKey)?.account ?? null;
						if (account === null || account.token.length === 0) {
							throw new Error(`Cannot notify logout without the retiring credential for ${accountKey}`);
						}
						return {
							target: instanceTargetFromSnapshot(requireRuntimeSnapshot(account.instance ?? null, 'notify logout')),
							token: account.token,
						};
					})()
				: null;
		const checkpoint = await this.checkpoints.capture(
			this.state.readCredentialMirror(),
			[accountKey].filter((key): key is string => key !== null),
		);
		await this.checkpoints.runBeforeCommit(
			checkpoint,
			async () => {
				await this.checkpoints.refresh(checkpoint, this.state.readCredentialMirror());
				this.checkpoints.markCommitStarted(checkpoint);
				await this.dependencies.credentialMirror.persist(emptyStoredSessionMirror());
				if (accountKey !== null) {
					await this.dependencies.deactivateRuntime();
				}
			},
			accountKey === null ? 'logging out' : `removing ${accountKey}`,
		);
		const previousStorageScope = checkpoint.storageScope;
		this.state.dispatch({type: 'logout.start'});
		let accountRemoved = false;
		const cleanupErrors = await this.cleanup.collect([
			() => this.dependencies.closeLayers(),
			() => this.dependencies.resetSyncedUserSettings(reason),
			() => this.cleanup.retireSession(reason),
			async () => {
				if (logoutNotification === null) return;
				try {
					await transition.notifyLogout(logoutNotification.target, logoutNotification.token);
				} catch (error) {
					logger.warn('Logout request failed', error);
				}
			},
		]);
		const accountCleanupErrors: Array<unknown> = [];
		if (accountKey !== null) {
			try {
				await this.accountPersistence.delete(accountKey);
				accountRemoved = true;
			} catch (error) {
				accountCleanupErrors.push(error);
			}
		}
		try {
			await this.dependencies.resetClientState(
				{reason: ResetClientStateReason.LOGOUT, keepDrafts: false},
				previousStorageScope,
			);
		} catch (error) {
			accountCleanupErrors.push(error);
		}
		const postCleanupErrors = await this.cleanup.collect([
			() => this.checkpoints.activateStorageScope(null),
			() => this.dependencies.clearSudoToken(),
		]);
		if (accountKey !== null && accountRemoved) {
			this.state.dispatch({type: 'account.remove', accountKey});
		}
		this.state.dispatch({type: 'logout.complete'});
		const allCleanupErrors = [...cleanupErrors, ...postCleanupErrors, ...accountCleanupErrors];
		this.cleanup.report(
			allCleanupErrors,
			accountKey === null
				? 'Logged out but failed to complete local account cleanup'
				: `Removed ${accountKey} but failed to complete local account cleanup`,
		);
		const [firstAccountCleanupError] = accountCleanupErrors;
		if (firstAccountCleanupError !== undefined) {
			throw firstAccountCleanupError;
		}
	}
}
