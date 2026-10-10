// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {AccountPresenceIntent, UserData} from '@app/features/auth/state/AccountStorage';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {AccountScopedWorkSuspension} from '@app/features/platform/state/AccountScopedWork';
import {AuthSessionAccountCatalog} from '@app/features/platform/state/auth_session/AuthSessionAccountCatalog';
import {
	AuthSessionAccountPersistence,
	type PersistableSessionAccount,
} from '@app/features/platform/state/auth_session/AuthSessionAccountPersistence';
import {
	AuthSessionCheckpointManager,
	AuthSessionRuntimeUnavailableError,
} from '@app/features/platform/state/auth_session/AuthSessionCheckpoint';
import {AuthSessionCleanup} from '@app/features/platform/state/auth_session/AuthSessionCleanup';
import {
	type AuthSessionDependencies,
	createDefaultAuthSessionDependencies,
} from '@app/features/platform/state/auth_session/AuthSessionDependencies';
import {
	AuthSessionLifecycle,
	type AuthSessionLoginRequest,
	type AuthSessionSwitchOptions,
	type AuthSessionTokenValidation,
	type AuthSessionValidatedSwitchOptions,
} from '@app/features/platform/state/auth_session/AuthSessionLifecycle';
import {AuthSessionRestorer} from '@app/features/platform/state/auth_session/AuthSessionRestorer';
import {AuthSessionRuntimeCommitter} from '@app/features/platform/state/auth_session/AuthSessionRuntimeCommitter';
import {
	type Account,
	type AuthSessionMachineEvent,
	type AuthSessionSnapshot,
	createAuthSessionSnapshot,
	getAuthSessionStateValue,
	SessionState,
	selectAuthSessionAccountKey,
	selectAuthSessionAccounts,
	selectAuthSessionCanSwitch,
	transitionAuthSessionSnapshot,
} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import {AuthSessionStoredAccountResolver} from '@app/features/platform/state/auth_session/AuthSessionStoredAccountResolver';
import {AuthSessionTransitionGate} from '@app/features/platform/state/auth_session/AuthSessionTransitionGate';
import {
	emptyStoredSessionMirror,
	type StoredSessionMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';
import {mirrorGatewayPrebootSession} from '@app/features/platform/state/PrebootMirror';
import {instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {setSyncedFieldSessionManager} from '@app/features/user/state/SyncedField';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {UserPrivate} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {actionBound, makeAutoObservable} from 'mobx';

export {AccountInstanceMismatchError} from '@app/features/platform/state/auth_session/AuthSessionAccountCatalog';
export {AuthSessionRuntimeUnavailableError} from '@app/features/platform/state/auth_session/AuthSessionCheckpoint';
export type {AuthSessionDependencies} from '@app/features/platform/state/auth_session/AuthSessionDependencies';
export type {
	AuthSessionLoginRequest,
	AuthSessionSwitchOptions,
	AuthSessionValidatedSwitchOptions,
} from '@app/features/platform/state/auth_session/AuthSessionLifecycle';
export {SessionExpiredError} from '@app/features/platform/state/auth_session/AuthSessionLifecycle';
export {DesktopAccountAuthorityError} from '@app/features/platform/state/auth_session/SessionCredentialMirror';
export {type Account, SessionState};

const logger = new Logger('SessionManager');
const AUTH_SESSION_TOKEN_VALIDATION_TIMEOUT_MS = 10_000;

export interface AuthSessionConnectionCloseResult {
	readonly invalidatedCurrentSession: boolean;
}

function requireRuntimeSnapshot(snapshot: RuntimeConfigSnapshot | null, operation: string): RuntimeConfigSnapshot {
	if (snapshot == null) {
		throw new AuthSessionRuntimeUnavailableError(operation);
	}
	return snapshot;
}

export class AuthSessionManager {
	private _snapshot: AuthSessionSnapshot = createAuthSessionSnapshot();
	private _initPromise: Promise<void> | null = null;
	private readonly _sessionInvalidationPromises = new Map<string | null, Promise<AuthSessionConnectionCloseResult>>();
	private readonly accountCatalog: AuthSessionAccountCatalog;
	private readonly accountPersistence: AuthSessionAccountPersistence;
	private readonly lifecycle: AuthSessionLifecycle;
	private readonly restorer: AuthSessionRestorer;
	private readonly transitionGate: AuthSessionTransitionGate;

	constructor(private readonly deps: AuthSessionDependencies = createDefaultAuthSessionDependencies()) {
		this.accountCatalog = new AuthSessionAccountCatalog(deps);
		this.accountPersistence = new AuthSessionAccountPersistence(deps);
		const runtimeCommitter = new AuthSessionRuntimeCommitter(deps);
		this.transitionGate = new AuthSessionTransitionGate(deps.accountScopedWork);
		const checkpoints = new AuthSessionCheckpointManager(deps, runtimeCommitter, {
			capture: () => this._snapshot,
			restore: (snapshot) => this.restoreSnapshot(snapshot),
		});
		const cleanup = new AuthSessionCleanup(deps);
		const storedAccountResolver = new AuthSessionStoredAccountResolver(deps);
		this.restorer = new AuthSessionRestorer(
			deps,
			this.accountCatalog,
			this.accountPersistence,
			checkpoints,
			runtimeCommitter,
			storedAccountResolver,
		);
		this.lifecycle = new AuthSessionLifecycle(
			deps,
			{
				getState: () => this.state,
				getCurrentAccountKey: () => this.currentAccountKey,
				getAccounts: () => this.accounts,
				canSwitchAccount: () => this.canSwitchAccount(),
				readCredentialMirror: () => this.currentCredentialMirror(),
				dispatch: (event) => this.send(event),
				stashCurrentAccount: (capturedPresenceIntent) => this.stashCurrentAccount(capturedPresenceIntent),
				validateToken: (token, userId, instance) => this.validateToken(token, userId, instance),
				markAccountInvalid: (accountKey, expectedToken) => this.markAccountInvalid(accountKey, expectedToken),
			},
			this.accountCatalog,
			this.accountPersistence,
			checkpoints,
			cleanup,
			runtimeCommitter,
			this.transitionGate,
		);
		makeAutoObservable<
			AuthSessionManager,
			| '_initPromise'
			| '_sessionInvalidationPromises'
			| 'accountCatalog'
			| 'accountPersistence'
			| 'deps'
			| 'lifecycle'
			| 'restorer'
			| 'restoreSnapshot'
			| 'send'
			| 'transitionGate'
		>(
			this,
			{
				_initPromise: false,
				_sessionInvalidationPromises: false,
				accountCatalog: false,
				accountPersistence: false,
				deps: false,
				lifecycle: false,
				restorer: false,
				restoreSnapshot: actionBound,
				send: actionBound,
				setToken: actionBound,
				setUserId: actionBound,
				transitionGate: false,
				validateToken: false,
			},
			{autoBind: true},
		);
	}

	get state(): SessionState {
		return getAuthSessionStateValue(this._snapshot);
	}

	get token(): string | null {
		return this._snapshot.context.token;
	}

	get userId(): string | null {
		return this._snapshot.context.userId;
	}

	get isAuthenticated(): boolean {
		return (
			this.state === SessionState.Authenticated ||
			this.state === SessionState.Connecting ||
			this.state === SessionState.Connected
		);
	}

	get isConnected(): boolean {
		return this.state === SessionState.Connected;
	}

	get isConnecting(): boolean {
		return this.state === SessionState.Connecting;
	}

	get isSwitching(): boolean {
		return this.state === SessionState.Switching;
	}

	get isLoggingOut(): boolean {
		return this.state === SessionState.LoggingOut;
	}

	get isInitialized(): boolean {
		return this._snapshot.context.isInitialized;
	}

	get accounts(): Array<Account> {
		return selectAuthSessionAccounts(this._snapshot);
	}

	get currentAccountKey(): string | null {
		return selectAuthSessionAccountKey(this._snapshot);
	}

	get currentAccount(): Account | null {
		const accountKey = this.currentAccountKey;
		if (accountKey === null) return null;
		return this._snapshot.context.accounts.get(accountKey) ?? null;
	}

	canSwitchAccount(): boolean {
		return selectAuthSessionCanSwitch(this._snapshot);
	}

	getAccount(accountKey: string): Account | null {
		return this.accountCatalog.resolve(this.accounts, accountKey)?.account ?? null;
	}

	requireSwitchableAccount(accountKey: string): Account {
		return this.accountCatalog.requireSwitchTarget(this.accounts, this.currentAccountKey, accountKey).account;
	}

	private send(event: AuthSessionMachineEvent): void {
		const previousState = this.state;
		this._snapshot = transitionAuthSessionSnapshot(this._snapshot, event);
		const nextState = this.state;
		if (previousState !== nextState) {
			logger.debug(`Transition: ${previousState} + ${event.type} -> ${nextState}`);
		}
	}

	private restoreSnapshot(snapshot: AuthSessionSnapshot): void {
		this._snapshot = snapshot;
	}

	async setToken(token: string | null): Promise<void> {
		const requestedAccountKey = this.currentAccountKey;
		return await this.transitionGate.runExclusive(() => this.applySessionToken(token, requestedAccountKey));
	}

	async setAccountToken(accountKey: string, token: string): Promise<boolean> {
		if (token.length === 0) {
			throw new Error(`Cannot persist an empty token for ${accountKey}`);
		}
		await this.initialize();
		return await this.transitionGate.runExclusive(() => this.applyAccountToken(accountKey, token));
	}

	async refreshStoredAccount(accountKey: string, token: string, userData?: UserData): Promise<boolean> {
		if (token.length === 0) {
			throw new Error(`Cannot persist an empty token for ${accountKey}`);
		}
		await this.initialize();
		return await this.transitionGate.runExclusive(async () => {
			const resolved = this.accountCatalog.resolve(this.accounts, accountKey);
			if (resolved === null || resolved.accountKey === this.currentAccountKey) {
				return false;
			}
			const instance = requireRuntimeSnapshot(resolved.account.instance ?? null, `refresh ${resolved.accountKey}`);
			const updated = await this.accountPersistence.rotateStoredAccount(
				resolved.accountKey,
				{...resolved.account, instance, userData: userData ?? resolved.account.userData, isValid: true},
				token,
				this.deps.now(),
			);
			await this.deps.accountStorage.updateAccountValidity(resolved.accountKey, true, token);
			this.send({type: 'account.upsert', account: updated});
			return true;
		});
	}

	async setRetiringAccountToken(accountKey: string, token: string): Promise<boolean> {
		if (!this.transitionGate.isAccountTransitionActive) {
			throw new Error(`Cannot update retiring account ${accountKey} outside an account transition`);
		}
		if (accountKey === this.currentAccountKey) {
			throw new Error(`Retiring account ${accountKey} still owns the active session`);
		}
		if (token.length === 0) {
			throw new Error(`Cannot persist an empty token for ${accountKey}`);
		}
		return await this.applyAccountToken(accountKey, token);
	}

	private async applyAccountToken(accountKey: string, token: string): Promise<boolean> {
		const resolved = this.accountCatalog.resolve(this.accounts, accountKey);
		if (resolved === null) {
			return false;
		}
		if (resolved.accountKey === this.currentAccountKey) {
			await this.applySessionToken(token, resolved.accountKey);
			return this.currentAccount?.token === token;
		}
		if (resolved.account.token === token) {
			return true;
		}
		const instance = requireRuntimeSnapshot(resolved.account.instance ?? null, `rotate ${resolved.accountKey}`);
		const updated = await this.accountPersistence.rotateStoredAccount(
			resolved.accountKey,
			{...resolved.account, instance},
			token,
			this.deps.now(),
		);
		this.send({type: 'account.upsert', account: updated});
		return true;
	}

	private async applySessionToken(token: string | null, requestedAccountKey: string | null): Promise<void> {
		if (this.currentAccountKey !== requestedAccountKey) {
			logger.warn(`Dropping a token update for ${requestedAccountKey ?? 'no account'} after an account transition`);
			return;
		}
		const account = this.currentAccount;
		const previousAccount =
			account === null
				? null
				: {
						...account,
						instance: requireRuntimeSnapshot(account.instance ?? null, `rotate ${account.storageKey}`),
					};
		const previousMirror = this.activeSessionPointer() ?? emptyStoredSessionMirror();
		const updated =
			previousAccount !== null && token !== null && previousAccount.token !== token
				? {...previousAccount, token, lastActive: this.deps.now()}
				: null;
		this.send({type: 'token.set', token});
		const nextMirror = this.activeSessionPointer() ?? emptyStoredSessionMirror();
		try {
			await this.accountPersistence.persistSessionTokenChange({
				previousAccount,
				nextAccount: updated,
				previousMirror,
				nextMirror,
			});
			if (updated !== null) {
				this.send({type: 'account.upsert', account: updated});
			}
		} catch (error) {
			if (previousAccount !== null) {
				this.send({type: 'account.upsert', account: previousAccount});
			}
			this.send({type: 'token.set', token: previousMirror.token});
			throw error;
		}
	}

	setUserId(userId: string | null): void {
		if (this.userId === userId) {
			return;
		}
		this.send({type: 'userId.set', userId});
		void this.deps.credentialMirror
			.persist(this.activeSessionPointer() ?? emptyStoredSessionMirror())
			.catch((error) => logger.error('Failed to persist the session user ID mirror', error));
	}

	private activeSessionPointer(): StoredSessionMirror | null {
		const {token, userId, accountKey} = this._snapshot.context;
		if (token === null || userId === null) {
			return null;
		}
		return {
			storageKey:
				accountKey ??
				getAccountKey({
					userId,
					instance: requireRuntimeSnapshot(this.deps.getRuntimeSnapshot(), 'identify the session'),
				}),
			userId,
			token,
		};
	}

	private currentCredentialMirror(): StoredSessionMirror {
		return this.deps.credentialMirror.read();
	}

	async initialize(): Promise<void> {
		if (this._initPromise) {
			return this._initPromise;
		}
		this._initPromise = this.doInitialize();
		return this._initPromise;
	}

	private async doInitialize(): Promise<void> {
		logger.debug(`doInitialize starting, current state: ${this.state}`);
		if (this.state !== SessionState.Idle && this.state !== SessionState.Error) {
			logger.debug(`Cannot initialize from state ${this.state}`);
			return;
		}
		this.send({type: 'initialize.start'});
		try {
			const initialization = await this.restorer.load();
			const {catalog} = initialization;
			this.send({type: 'accounts.loaded', accounts: catalog.accounts});
			logger.debug(
				`Loaded ${catalog.accounts.length} of ${catalog.recordCount} stored accounts (source: ${catalog.source})`,
			);
			if (initialization.kind === 'signed-out') {
				if (initialization.reason !== 'no-session-pointer') {
					logger.warn(
						`The active session could not be restored (${initialization.reason}), keeping its credential mirror intact`,
					);
				}
				mirrorGatewayPrebootSession(false);
				this.send({type: 'initialize.noToken'});
				return;
			}
			const {mirror, resolution} = initialization;
			if (resolution.usedMirroredCredential) {
				logger.info(`Recovered the durable credential for ${resolution.accountKey} from the session mirror`);
			}
			await this.restorer.activate(resolution, mirror, () => {
				this.send({
					type: 'initialize.tokenLoaded',
					token: resolution.account.token,
					userId: resolution.account.userId,
					accountKey: resolution.accountKey,
				});
			});
			logger.debug(`Initialization complete: state=${this.state}, isAuthenticated=${this.isAuthenticated}`);
		} catch (err) {
			const failure = err instanceof Error ? err : new Error(String(err));
			logger.error('Initialization failed', failure);
			this.send({type: 'initialize.failed', error: failure});
			this._initPromise = null;
			throw failure;
		}
	}

	async stashCurrentAccount(capturedPresenceIntent?: AccountPresenceIntent | null): Promise<void> {
		const currentUserId = this.userId;
		const currentToken = this.token;
		if (!currentUserId || !currentToken) {
			return;
		}
		const existingAccount = this.currentAccount;
		const instance = requireRuntimeSnapshot(this.deps.getRuntimeSnapshot(), 'stash the current account');
		const presenceIntent =
			this.deps.captureLocalPresenceIntent() ?? capturedPresenceIntent ?? existingAccount?.presenceIntent ?? null;
		const account = {
			storageKey: getAccountKey({userId: currentUserId, instance}),
			userId: currentUserId,
			token: currentToken,
			userData: existingAccount?.userData,
			presenceIntent,
			lastActive: this.deps.now(),
			instance,
			isValid: existingAccount?.isValid ?? true,
		} satisfies PersistableSessionAccount;
		await this.accountPersistence.stash(account);
		this.send({
			type: 'account.upsert',
			account,
		});
	}

	async validateToken(
		token: string,
		userId: string,
		instance: RuntimeConfigSnapshot,
	): Promise<AuthSessionTokenValidation> {
		try {
			const response = await this.deps.http.get<UserPrivate>(Endpoints.USER_ME, {
				auth: 'none',
				headers: {Authorization: token},
				target: instanceTargetFromSnapshot(instance),
				timeoutMs: AUTH_SESSION_TOKEN_VALIDATION_TIMEOUT_MS,
				retries: 0,
			});
			return response.body.id === userId ? {kind: 'valid'} : {kind: 'invalid'};
		} catch (error) {
			if (error instanceof HttpError && error.status === HttpStatus.UNAUTHORIZED) {
				return {kind: 'invalid'};
			}
			return {kind: 'unavailable', cause: error instanceof Error ? error : new Error(String(error))};
		}
	}

	markAccountInvalid(accountKey: string, expectedToken?: string): void {
		const resolved = this.accountCatalog.resolve(this.accounts, accountKey);
		if (resolved === null || (expectedToken !== undefined && resolved.account.token !== expectedToken)) {
			return;
		}
		this.send({type: 'account.markInvalid', accountKey: resolved.accountKey});
		void this.deps.accountStorage
			.updateAccountValidity(resolved.accountKey, false, expectedToken)
			.catch((error: unknown) => {
				logger.warn(`Could not mark ${resolved.accountKey} invalid in account storage`, error);
			});
	}

	async login(request: AuthSessionLoginRequest): Promise<void> {
		await this.initialize();
		await this.lifecycle.login(request);
	}

	async loginWithinAccountActivation(
		request: AuthSessionLoginRequest,
		suspension: AccountScopedWorkSuspension,
	): Promise<void> {
		await this.initialize();
		await this.lifecycle.loginWithinAccountActivation(request, suspension);
	}

	async prepareStoredAccount(accountKey: string): Promise<Account> {
		await this.initialize();
		const resolved = this.accountCatalog.resolve(this.accounts, accountKey);
		if (resolved === null) {
			throw new Error(`No stored account found for ${accountKey}`);
		}
		if (resolved.account.instance !== undefined) {
			return await this.adoptStoredAccountToken(resolved.account);
		}
		const prepared = await this.restorer.prepareAccount(resolved.accountKey);
		this.send({type: 'account.upsert', account: prepared});
		return prepared;
	}

	private async adoptStoredAccountToken(account: Account): Promise<Account> {
		if (account.storageKey === this.currentAccountKey) {
			return account;
		}
		const {records} = await this.deps.accountStorage.getAllAccounts();
		const stored = records.find((record) => record.storageKey === account.storageKey);
		if (!stored?.token || stored.token === account.token) {
			return account;
		}
		const adopted: Account = {
			...account,
			token: stored.token,
			userData: stored.userData ?? account.userData,
			isValid: stored.isValid ?? true,
		};
		this.send({type: 'account.upsert', account: adopted});
		return adopted;
	}

	async switchAccount(accountKey: string, options: AuthSessionSwitchOptions = {}): Promise<void> {
		await this.initialize();
		await this.prepareStoredAccount(accountKey);
		await this.lifecycle.switchAccount(accountKey, options);
	}

	async switchAccountWithinAccountActivation(
		accountKey: string,
		options: AuthSessionValidatedSwitchOptions,
		suspension: AccountScopedWorkSuspension,
	): Promise<void> {
		await this.initialize();
		await this.prepareStoredAccount(accountKey);
		await this.lifecycle.switchAccountWithinAccountActivation(accountKey, options, suspension);
	}

	async logout(): Promise<void> {
		await this.initialize();
		await this.lifecycle.logout();
	}

	async removeAccount(accountKey: string): Promise<void> {
		await this.initialize();
		await this.lifecycle.removeAccount(accountKey);
	}
	handleConnectionReady(): void {
		this.send({type: 'connection.ready'});
	}

	async handleConnectionClosed(
		code: number,
		failedAccountKey: string | null = this.currentAccountKey,
	): Promise<AuthSessionConnectionCloseResult> {
		if (code === 4004) {
			const existing = this._sessionInvalidationPromises.get(failedAccountKey);
			if (existing !== undefined) {
				return await existing;
			}
			const operation = this.transitionGate.runExclusive(() => this.invalidateGatewayAccount(failedAccountKey));
			this._sessionInvalidationPromises.set(failedAccountKey, operation);
			try {
				return await operation;
			} finally {
				if (this._sessionInvalidationPromises.get(failedAccountKey) === operation) {
					this._sessionInvalidationPromises.delete(failedAccountKey);
				}
			}
		}
		this.send({type: 'connection.closed'});
		return {invalidatedCurrentSession: false};
	}

	private async invalidateGatewayAccount(accountKey: string | null): Promise<AuthSessionConnectionCloseResult> {
		const resolved = accountKey === null ? null : this.accountCatalog.resolve(this.accounts, accountKey);
		const resolvedAccountKey = resolved?.accountKey ?? accountKey;
		const invalidatedCurrentSession = resolvedAccountKey !== null && this.currentAccountKey === resolvedAccountKey;
		if (resolved !== null) {
			this.send({type: 'account.markInvalid', accountKey: resolved.accountKey});
		}
		if (invalidatedCurrentSession) {
			this.send({type: 'session.invalidated'});
		}
		const cleanupErrors: Array<unknown> = [];
		if (resolved !== null) {
			try {
				await this.deps.accountStorage.updateAccountValidity(resolved.accountKey, false);
			} catch (error) {
				cleanupErrors.push(error);
			}
		}
		if (invalidatedCurrentSession) {
			try {
				await this.deps.credentialMirror.persist(emptyStoredSessionMirror());
			} catch (error) {
				cleanupErrors.push(error);
			}
			try {
				await this.deps.deactivateRuntime();
			} catch (error) {
				cleanupErrors.push(error);
			}
		}
		if (cleanupErrors.length > 0) {
			logger.error(
				`Gateway authentication failed for ${resolvedAccountKey ?? 'an unknown account'} and durable invalidation did not fully complete`,
				new AggregateError(cleanupErrors, 'Gateway authentication failure cleanup did not fully complete'),
			);
		}
		return {invalidatedCurrentSession};
	}

	handleConnectionStarted(): void {
		this.send({type: 'connection.start'});
	}

	handleConnectionFailed(): void {
		this.send({type: 'connection.failed'});
	}

	updateAccountUserData(accountKey: string, userData: UserData): void {
		const resolved = this.accountCatalog.resolve(this.accounts, accountKey);
		if (resolved === null) {
			return;
		}
		this.send({type: 'account.userDataUpdated', accountKey: resolved.accountKey, userData});
	}

	async reset(): Promise<void> {
		await this.deps.deactivateRuntime();
		this.send({type: 'reset'});
		this._initPromise = null;
	}
}

export function createSessionManager(dependencies?: AuthSessionDependencies): AuthSessionManager {
	return new AuthSessionManager(dependencies);
}

const SessionManager = createSessionManager();
setSyncedFieldSessionManager(SessionManager);

export default SessionManager;
