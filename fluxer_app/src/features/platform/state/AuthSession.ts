// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import RuntimeConfig, {
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
} from '@app/features/app/state/RuntimeConfig';
import accountStorage, {
	type AccountPresenceIntent,
	type StoredAccount,
	type UserData,
} from '@app/features/auth/state/AccountStorage';
import Sudo from '@app/features/auth/state/AuthSudo';
import {
	type Account,
	type AuthSessionMachineEvent,
	type AuthSessionSnapshot,
	createAuthSessionSnapshot,
	getAuthSessionStateValue,
	SessionState,
	selectAuthSessionAccounts,
	selectAuthSessionCanSwitch,
	transitionAuthSessionSnapshot,
} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import {
	AuthSessionStorageKey,
	parseStoredSessionValue,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import {action, makeAutoObservable} from 'mobx';

export {type Account, SessionState};

const logger = new Logger('SessionManager');

export class SessionExpiredError extends Error {
	constructor(message?: string) {
		super(message ?? 'Session expired');
		this.name = 'SessionExpiredError';
	}
}

export class AccountInstanceMismatchError extends Error {
	constructor(userId: string) {
		super(`Account ${userId} does not belong to the current instance`);
		this.name = 'AccountInstanceMismatchError';
	}
}

interface AuthSessionAccountStorage {
	getAllAccounts(): Promise<Array<StoredAccount>>;
	stashAccountData(
		userId: string,
		token: string | null,
		userData?: UserData,
		instance?: RuntimeConfigSnapshot,
		presenceIntent?: AccountPresenceIntent | null,
	): Promise<void>;
	restoreAccountData(userId: string, expectedInstance: RuntimeConfigSnapshot): Promise<StoredAccount | null>;
	deleteAccount(userId: string): Promise<void>;
	updateAccountValidity(userId: string, isValid: boolean): Promise<void>;
}

interface AuthSessionAppStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
	clear(): void;
}

interface AuthSessionHttp {
	get<T>(url: string, options?: unknown): Promise<{body: T}>;
	post<T = unknown>(url: string, options?: unknown): Promise<{body: T} | undefined>;
}

export interface AuthSessionDependencies {
	accountStorage: AuthSessionAccountStorage;
	appStorage: AuthSessionAppStorage;
	http: AuthSessionHttp;
	getRuntimeSnapshot: () => RuntimeConfigSnapshot;
	closeLayers: () => void;
	clearSudoToken: () => void;
	sendInvisiblePresence: (reason: 'logout' | 'account-switch') => void;
	cleanupGatewaySession: (reason: 'logout' | 'account-switch') => void;
	resetSyncedUserSettings: (reason: 'logout' | 'account-switch') => void;
	captureLocalPresenceIntent: () => AccountPresenceIntent | null;
	restoreLocalPresenceIntent: (intent: AccountPresenceIntent | null | undefined) => void;
	now: () => number;
}

function createDefaultAuthSessionDependencies(): AuthSessionDependencies {
	return {
		accountStorage,
		appStorage: AppStorage,
		http,
		getRuntimeSnapshot: () => RuntimeConfig.getSnapshot(),
		closeLayers: () => {
			void import('@app/features/ui/state/LayerManager').then((module) => {
				module.default.closeAll();
			});
		},
		clearSudoToken: () => Sudo.clearToken(),
		sendInvisiblePresence: (reason) => {
			void import('@app/features/gateway/transport/GatewayConnection').then((module) => {
				module.default.sendInvisiblePresenceForCurrentSession(reason);
			});
		},
		cleanupGatewaySession: () => {
			void import('@app/features/gateway/transport/GatewayConnection').then((module) => {
				module.default.logout();
			});
		},
		resetSyncedUserSettings: () => {
			void import('@app/features/user/state/UserSettings').then((module) => {
				module.default.handleAccountTransition();
			});
		},
		captureLocalPresenceIntent: () => LocalPresence.captureIntent(),
		restoreLocalPresenceIntent: (intent) => LocalPresence.restoreIntent(intent),
		now: () => Date.now(),
	};
}

function accountFromStoredRecord(record: StoredAccount): Account | null {
	if (!record.token) {
		logger.warn(`Skipping stored account for ${record.userId} because token is missing`);
		return null;
	}
	return {
		userId: record.userId,
		token: record.token,
		userData: record.userData,
		presenceIntent: record.presenceIntent,
		lastActive: record.lastActive,
		instance: record.instance,
		isValid: record.isValid ?? true,
	};
}

export class AuthSessionManager {
	private _snapshot: AuthSessionSnapshot = createAuthSessionSnapshot();
	private _initPromise: Promise<void> | null = null;
	private _mutex: Promise<void> = Promise.resolve();

	constructor(private readonly deps: AuthSessionDependencies = createDefaultAuthSessionDependencies()) {
		makeAutoObservable(
			this,
			{
				send: action.bound,
				setToken: action.bound,
				setUserId: action.bound,
				setError: action.bound,
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

	get error(): Error | null {
		return this._snapshot.context.error;
	}

	get isIdle(): boolean {
		return this.state === SessionState.Idle;
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
		const currentInstance = this.deps.getRuntimeSnapshot();
		return selectAuthSessionAccounts(this._snapshot).filter((account) =>
			runtimeConfigSnapshotsAreSameInstance(account.instance, currentInstance),
		);
	}

	get currentAccount(): Account | null {
		const userId = this.userId;
		if (!userId) return null;
		return this._snapshot.context.accounts.get(userId) ?? null;
	}

	canSwitchAccount(): boolean {
		return selectAuthSessionCanSwitch(this._snapshot);
	}

	requireAccountOnCurrentInstance(userId: string): Account {
		const account = this._snapshot.context.accounts.get(userId);
		if (!account) {
			throw new Error(`No account found for ${userId}`);
		}
		if (!runtimeConfigSnapshotsAreSameInstance(account.instance, this.deps.getRuntimeSnapshot())) {
			throw new AccountInstanceMismatchError(userId);
		}
		return account;
	}

	send(event: AuthSessionMachineEvent): void {
		const previousState = this.state;
		this._snapshot = transitionAuthSessionSnapshot(this._snapshot, event);
		const nextState = this.state;
		if (previousState !== nextState) {
			logger.debug(`Transition: ${previousState} + ${event.type} -> ${nextState}`);
		}
	}

	setToken(token: string | null): void {
		this.send({type: 'token.set', token});
		this.persistToken(token);
	}

	setUserId(userId: string | null): void {
		this.send({type: 'userId.set', userId});
		this.persistUserId(userId);
	}

	setError(error: Error | null): void {
		if (error) {
			this.send({type: 'initialize.failed', error});
		}
	}

	private persistToken(token: string | null): void {
		if (token) {
			this.deps.appStorage.setItem(AuthSessionStorageKey.Token, token);
		} else {
			this.deps.appStorage.removeItem(AuthSessionStorageKey.Token);
		}
	}

	private persistUserId(userId: string | null): void {
		if (userId) {
			this.deps.appStorage.setItem(AuthSessionStorageKey.UserId, userId);
		} else {
			this.deps.appStorage.removeItem(AuthSessionStorageKey.UserId);
		}
	}

	private persistActiveCredentials(token: string | null, userId: string | null): void {
		this.persistToken(token);
		this.persistUserId(userId);
	}

	private async runExclusive<T>(fn: () => Promise<T>): Promise<T> {
		const run = async (): Promise<T> => {
			return await fn();
		};
		const next = this._mutex.then(run, run);
		this._mutex = next.then(
			() => undefined,
			() => undefined,
		);
		return next;
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
			const storedToken = parseStoredSessionValue(this.deps.appStorage.getItem(AuthSessionStorageKey.Token));
			const storedUserId = parseStoredSessionValue(this.deps.appStorage.getItem(AuthSessionStorageKey.UserId));
			await this.loadStoredAccounts();
			logger.debug(`Loaded from storage: token=${storedToken ? 'present' : 'null'}, userId=${storedUserId ?? 'null'}`);
			let storedAccount = storedUserId ? this._snapshot.context.accounts.get(storedUserId) : undefined;
			if (storedToken && storedUserId && storedAccount?.token !== storedToken) {
				storedAccount = this.recoverStoredActiveAccount(storedToken, storedUserId, storedAccount);
			}
			if (storedToken && storedUserId && storedAccount?.token === storedToken) {
				this.send({type: 'initialize.tokenLoaded', token: storedToken, userId: storedUserId});
				this.deps.restoreLocalPresenceIntent(storedAccount.presenceIntent ?? null);
			} else {
				this.persistActiveCredentials(null, null);
				this.send({type: 'initialize.noToken'});
			}
			logger.debug(`Initialization complete: state=${this.state}, isAuthenticated=${this.isAuthenticated}`);
		} catch (err) {
			const error = err instanceof Error ? err : new Error(String(err));
			logger.error('Initialization failed', error);
			this.send({type: 'initialize.failed', error});
		}
	}

	private recoverStoredActiveAccount(token: string, userId: string, existing?: Account): Account {
		const instance = this.deps.getRuntimeSnapshot();
		const account: Account = {
			userId,
			token,
			userData: existing?.userData,
			presenceIntent: existing?.presenceIntent ?? null,
			lastActive: this.deps.now(),
			instance,
			isValid: true,
		};
		this.send({type: 'account.upsert', account});
		void this.deps.accountStorage
			.stashAccountData(userId, token, account.userData, instance, account.presenceIntent)
			.catch((error) => logger.warn('Failed to persist recovered active account', error));
		logger.info('Recovered stored active account after local account data was unavailable');
		return account;
	}

	private async loadStoredAccounts(): Promise<void> {
		try {
			const stored = await this.deps.accountStorage.getAllAccounts();
			const currentInstance = this.deps.getRuntimeSnapshot();
			const accounts = stored.flatMap((record) => {
				const account = accountFromStoredRecord(record);
				return account && runtimeConfigSnapshotsAreSameInstance(account.instance, currentInstance) ? [account] : [];
			});
			this.send({type: 'accounts.loaded', accounts});
			logger.debug(`Loaded ${accounts.length} of ${stored.length} accounts for the current instance`);
		} catch (err) {
			logger.error('Failed to load accounts', err);
		}
	}

	async stashCurrentAccount(): Promise<void> {
		const currentUserId = this.userId;
		const currentToken = this.token;
		if (!currentUserId || !currentToken) {
			return;
		}
		const existingAccount = this._snapshot.context.accounts.get(currentUserId);
		const instance = this.deps.getRuntimeSnapshot();
		const presenceIntent = this.deps.captureLocalPresenceIntent() ?? existingAccount?.presenceIntent ?? null;
		await this.deps.accountStorage.stashAccountData(
			currentUserId,
			currentToken,
			existingAccount?.userData,
			instance,
			presenceIntent,
		);
		this.send({
			type: 'account.upsert',
			account: {
				userId: currentUserId,
				token: currentToken,
				userData: existingAccount?.userData,
				presenceIntent,
				lastActive: this.deps.now(),
				instance,
				isValid: true,
			},
		});
	}

	async validateToken(token: string): Promise<boolean> {
		try {
			await this.deps.http.get<unknown>(Endpoints.USER_ME, {
				auth: 'none',
				headers: {Authorization: token},
			});
			return true;
		} catch {
			return false;
		}
	}

	markAccountInvalid(userId: string): void {
		if (!this._snapshot.context.accounts.has(userId)) {
			return;
		}
		this.send({type: 'account.markInvalid', userId});
		void this.deps.accountStorage.updateAccountValidity(userId, false);
	}

	prepareForAccountTransition(reason: 'logout' | 'account-switch'): void {
		const currentUserId = this.userId;
		const currentToken = this.token;
		const presenceIntent = this.deps.captureLocalPresenceIntent();
		if (currentUserId && currentToken && presenceIntent) {
			const existingAccount = this._snapshot.context.accounts.get(currentUserId);
			this.send({
				type: 'account.upsert',
				account: {
					userId: currentUserId,
					token: currentToken,
					userData: existingAccount?.userData,
					presenceIntent,
					lastActive: existingAccount?.lastActive ?? this.deps.now(),
					instance: existingAccount?.instance ?? this.deps.getRuntimeSnapshot(),
					isValid: existingAccount?.isValid ?? true,
				},
			});
		}
		this.deps.sendInvisiblePresence(reason);
		this.deps.resetSyncedUserSettings(reason);
	}

	async login(token: string, userId: string, userData?: UserData): Promise<void> {
		await this.initialize();
		return await this.runExclusive(async () => {
			const previousUserId = this.userId;
			if (previousUserId && previousUserId !== userId) {
				this.prepareForAccountTransition('account-switch');
				await this.stashCurrentAccount();
				this.deps.cleanupGatewaySession('account-switch');
			}
			const snapshot = this.deps.getRuntimeSnapshot();
			const existing = this._snapshot.context.accounts.get(userId);
			const resolvedUserData = userData ?? existing?.userData;
			await this.deps.accountStorage.stashAccountData(userId, token, resolvedUserData, snapshot);
			const account: Account = {
				userId,
				token,
				userData: resolvedUserData,
				presenceIntent: existing?.presenceIntent,
				lastActive: this.deps.now(),
				instance: snapshot,
				isValid: true,
			};
			this.persistActiveCredentials(token, userId);
			this.send({type: 'account.login', account});
		});
	}

	async switchAccount(userId: string): Promise<void> {
		await this.initialize();
		return await this.runExclusive(async () => {
			if (userId === this.userId) {
				logger.debug('Already on requested account');
				return;
			}
			const account = this.requireAccountOnCurrentInstance(userId);
			if (!this.canSwitchAccount()) {
				throw new Error(`Cannot switch from state: ${this.state}`);
			}
			this.send({type: 'account.switch.start'});
			const currentSnapshot = this.deps.getRuntimeSnapshot();
			this.prepareForAccountTransition('account-switch');
			try {
				await this.stashCurrentAccount();
				this.deps.cleanupGatewaySession('account-switch');
				const isValid = await this.validateToken(account.token);
				if (!isValid) {
					this.markAccountInvalid(userId);
					throw new SessionExpiredError();
				}
				const restored = await this.deps.accountStorage.restoreAccountData(userId, currentSnapshot);
				if (!restored) {
					throw new Error(`No data found for ${userId}`);
				}
				const nextPresenceIntent = restored.presenceIntent ?? account.presenceIntent ?? null;
				const nextAccount: Account = {
					...account,
					userData: restored.userData ?? account.userData,
					presenceIntent: nextPresenceIntent,
					lastActive: this.deps.now(),
					instance: currentSnapshot,
					isValid: true,
				};
				this.deps.closeLayers();
				this.deps.clearSudoToken();
				this.deps.restoreLocalPresenceIntent(nextPresenceIntent);
				this.persistActiveCredentials(account.token, userId);
				this.send({type: 'account.switch.complete', account: nextAccount});
				await this.deps.accountStorage.stashAccountData(
					userId,
					account.token,
					restored.userData ?? account.userData,
					currentSnapshot,
					nextPresenceIntent,
				);
			} catch (err) {
				logger.error('Failed to switch account', err);
				this.send({type: 'account.switch.failed'});
				throw err;
			}
		});
	}

	async logout(): Promise<void> {
		await this.initialize();
		return await this.runExclusive(async () => {
			if (
				this.state !== SessionState.Idle &&
				this.state !== SessionState.Authenticated &&
				this.state !== SessionState.Connecting &&
				this.state !== SessionState.Connected &&
				this.state !== SessionState.Error
			) {
				return;
			}
			this.send({type: 'logout.start'});
			const currentUserId = this.userId;
			this.prepareForAccountTransition('logout');
			this.deps.cleanupGatewaySession('logout');
			try {
				try {
					await this.deps.http.post(Endpoints.AUTH_LOGOUT, {timeoutMs: 5000, retries: 0});
				} catch (err) {
					logger.warn('Logout request failed', err);
				}
				if (currentUserId) {
					try {
						await this.deps.accountStorage.deleteAccount(currentUserId);
					} catch (err) {
						logger.warn('Failed to delete account', err);
					}
				}
				this.deps.appStorage.clear();
				this.deps.closeLayers();
				this.deps.clearSudoToken();
				this.send({type: 'logout.complete'});
				if (currentUserId) {
					this.send({type: 'account.remove', userId: currentUserId});
				}
			} catch (err) {
				logger.error('Logout failed', err);
				this.send({type: 'logout.complete'});
			}
		});
	}

	async removeAccount(userId: string): Promise<void> {
		await this.initialize();
		await this.deps.accountStorage.deleteAccount(userId);
		const removingCurrentAccount = this.userId === userId;
		this.send({type: 'account.remove', userId});
		if (removingCurrentAccount) {
			this.persistActiveCredentials(null, null);
		}
	}

	handleConnectionReady(): void {
		this.send({type: 'connection.ready'});
	}

	handleConnectionClosed(code: number): void {
		if (code === 4004) {
			this.send({type: 'session.invalidated'});
			this.persistActiveCredentials(null, null);
		} else {
			this.send({type: 'connection.closed'});
		}
	}

	handleConnectionStarted(): void {
		this.send({type: 'connection.start'});
	}

	handleConnectionFailed(): void {
		this.send({type: 'connection.failed'});
	}

	updateAccountUserData(userId: string, userData: UserData): void {
		this.send({type: 'account.userDataUpdated', userId, userData});
	}

	reset(): void {
		this.send({type: 'reset'});
		this._initPromise = null;
	}
}

export function createSessionManager(dependencies?: AuthSessionDependencies): AuthSessionManager {
	return new AuthSessionManager(dependencies);
}

export default createSessionManager();
