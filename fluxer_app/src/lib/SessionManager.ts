/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {action, makeAutoObservable, runInAction} from 'mobx';
import Config from '~/Config';
import {Endpoints} from '~/Endpoints';
import accountStorage, {type UserData} from '~/lib/AccountStorage';
import AppStorage from '~/lib/AppStorage';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import ConnectionStore from '~/stores/ConnectionStore';
import LayerManager from '~/stores/LayerManager';
import type {RuntimeConfigSnapshot} from '~/stores/RuntimeConfigStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import SudoStore from '~/stores/SudoStore';

const logger = new Logger('SessionManager');

export class SessionExpiredError extends Error {
	constructor(message?: string) {
		super(message ?? 'Session expired');
		this.name = 'SessionExpiredError';
	}
}

export const SessionState = {
	Idle: 'idle',
	Initializing: 'initializing',
	Authenticated: 'authenticated',
	Connecting: 'connecting',
	Connected: 'connected',
	Switching: 'switching',
	LoggingOut: 'logging_out',
	Error: 'error',
} as const;

export type SessionState = (typeof SessionState)[keyof typeof SessionState];

const SessionEvent = {
	Initialize: 'initialize',
	TokenLoaded: 'token_loaded',
	NoToken: 'no_token',
	StartConnection: 'start_connection',
	ConnectionReady: 'connection_ready',
	ConnectionFailed: 'connection_failed',
	ConnectionClosed: 'connection_closed',
	SwitchAccount: 'switch_account',
	SwitchComplete: 'switch_complete',
	SwitchFailed: 'switch_failed',
	Logout: 'logout',
	LogoutComplete: 'logout_complete',
	SessionInvalidated: 'session_invalidated',
	Reset: 'reset',
} as const;

export type SessionEvent = (typeof SessionEvent)[keyof typeof SessionEvent];

interface StateTransition {
	from: SessionState | Array<SessionState>;
	event: SessionEvent;
	to: SessionState;
}

const VALID_TRANSITIONS: Array<StateTransition> = [
	{from: SessionState.Idle, event: SessionEvent.Initialize, to: SessionState.Initializing},
	{
		from: [SessionState.Idle, SessionState.Initializing],
		event: SessionEvent.TokenLoaded,
		to: SessionState.Authenticated,
	},
	{from: SessionState.Initializing, event: SessionEvent.NoToken, to: SessionState.Idle},
	{from: SessionState.Authenticated, event: SessionEvent.StartConnection, to: SessionState.Connecting},
	{from: SessionState.Connecting, event: SessionEvent.ConnectionReady, to: SessionState.Connected},
	{from: SessionState.Connecting, event: SessionEvent.ConnectionFailed, to: SessionState.Authenticated},
	{from: SessionState.Connected, event: SessionEvent.ConnectionClosed, to: SessionState.Authenticated},
	{
		from: [SessionState.Authenticated, SessionState.Connected],
		event: SessionEvent.SwitchAccount,
		to: SessionState.Switching,
	},
	{from: SessionState.Switching, event: SessionEvent.SwitchComplete, to: SessionState.Authenticated},
	{from: SessionState.Switching, event: SessionEvent.SwitchFailed, to: SessionState.Authenticated},
	{
		from: [SessionState.Idle, SessionState.Authenticated, SessionState.Connected, SessionState.Error],
		event: SessionEvent.Logout,
		to: SessionState.LoggingOut,
	},
	{from: SessionState.LoggingOut, event: SessionEvent.LogoutComplete, to: SessionState.Idle},
	{
		from: [SessionState.Authenticated, SessionState.Connected, SessionState.Connecting],
		event: SessionEvent.SessionInvalidated,
		to: SessionState.Idle,
	},
	{
		from: [
			SessionState.Idle,
			SessionState.Initializing,
			SessionState.Authenticated,
			SessionState.Connecting,
			SessionState.Connected,
			SessionState.Switching,
			SessionState.LoggingOut,
			SessionState.Error,
		],
		event: SessionEvent.Reset,
		to: SessionState.Idle,
	},
];

const StorageKey = {
	Token: 'token',
	UserId: 'userId',
} as const;

export interface Account {
	userId: string;
	token: string;
	userData?: UserData;
	lastActive: number;
	instance?: RuntimeConfigSnapshot;
	isValid: boolean;
}

function readNullableString(value: string | null): string | null {
	if (!value) {
		return null;
	} else if (value === 'undefined' || value === 'null') {
		return null;
	} else {
		return value;
	}
}

function buildInstanceUserMeUrl(instance: RuntimeConfigSnapshot): string {
	const endpoint = instance.apiEndpoint.replace(/\/+$/, '');
	if (!endpoint) {
		return Endpoints.USER_ME;
	}
	return `${endpoint}/v${Config.PUBLIC_API_VERSION}${Endpoints.USER_ME}`;
}

class SessionManager {
	private _state: SessionState = SessionState.Idle;
	private _token: string | null = null;
	private _userId: string | null = null;
	private _accounts: Map<string, Account> = new Map();
	private _error: Error | null = null;
	private _operationSequence = 0;
	private _initPromise: Promise<void> | null = null;
	private _mutex: Promise<void> = Promise.resolve();
	private _isInitialized = false;

	constructor() {
		makeAutoObservable(
			this,
			{
				transition: action.bound,
				setToken: action.bound,
				setUserId: action.bound,
				setError: action.bound,
			},
			{autoBind: true},
		);
	}

	get state(): SessionState {
		return this._state;
	}

	get token(): string | null {
		return this._token;
	}

	get userId(): string | null {
		return this._userId;
	}

	get error(): Error | null {
		return this._error;
	}

	get isIdle(): boolean {
		return this._state === SessionState.Idle;
	}

	get isAuthenticated(): boolean {
		return (
			this._state === SessionState.Authenticated ||
			this._state === SessionState.Connecting ||
			this._state === SessionState.Connected
		);
	}

	get isConnected(): boolean {
		return this._state === SessionState.Connected;
	}

	get isConnecting(): boolean {
		return this._state === SessionState.Connecting;
	}

	get isSwitching(): boolean {
		return this._state === SessionState.Switching;
	}

	get isLoggingOut(): boolean {
		return this._state === SessionState.LoggingOut;
	}

	canSwitchAccount(): boolean {
		return this.canTransition(SessionEvent.SwitchAccount);
	}

	get isInitialized(): boolean {
		return this._isInitialized;
	}

	get accounts(): Array<Account> {
		return Array.from(this._accounts.values()).sort((a, b) => b.lastActive - a.lastActive);
	}

	get currentAccount(): Account | null {
		if (!this._userId) {
			return null;
		} else {
			return this._accounts.get(this._userId) ?? null;
		}
	}

	private canTransition(event: SessionEvent): boolean {
		for (const transition of VALID_TRANSITIONS) {
			const fromStates = Array.isArray(transition.from) ? transition.from : [transition.from];
			if (fromStates.includes(this._state) && transition.event === event) {
				return true;
			}
		}
		return false;
	}

	private getNextState(event: SessionEvent): SessionState | null {
		for (const transition of VALID_TRANSITIONS) {
			const fromStates = Array.isArray(transition.from) ? transition.from : [transition.from];
			if (fromStates.includes(this._state) && transition.event === event) {
				return transition.to;
			}
		}
		return null;
	}

	transition(event: SessionEvent): boolean {
		const nextState = this.getNextState(event);
		if (nextState === null) {
			logger.warn(`Invalid transition: ${this._state} + ${event}`);
			return false;
		} else {
			logger.debug(`Transition: ${this._state} + ${event} -> ${nextState}`);
			this._state = nextState;
			return true;
		}
	}

	setToken(token: string | null): void {
		this._token = token;
		if (token) {
			AppStorage.setItem(StorageKey.Token, token);
		} else {
			AppStorage.removeItem(StorageKey.Token);
		}
	}

	setUserId(userId: string | null): void {
		this._userId = userId;
		if (userId) {
			AppStorage.setItem(StorageKey.UserId, userId);
		} else {
			AppStorage.removeItem(StorageKey.UserId);
		}
	}

	setError(error: Error | null): void {
		this._error = error;
		if (error) {
			this._state = SessionState.Error;
		}
	}

	private nextOperationId(): number {
		return ++this._operationSequence;
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
		logger.debug(`doInitialize starting, current state: ${this._state}`);
		if (!this.canTransition(SessionEvent.Initialize)) {
			logger.debug(`Cannot transition Initialize from state ${this._state}`);
			return;
		}

		runInAction(() => {
			this.transition(SessionEvent.Initialize);
		});

		try {
			await this.loadStoredAccounts();

			const storedToken = readNullableString(AppStorage.getItem(StorageKey.Token));
			const storedUserId = readNullableString(AppStorage.getItem(StorageKey.UserId));

			logger.debug(`Loaded from storage: token=${storedToken ? 'present' : 'null'}, userId=${storedUserId ?? 'null'}`);

			if (storedToken && storedUserId) {
				runInAction(() => {
					this._token = storedToken;
					this._userId = storedUserId;
					this.transition(SessionEvent.TokenLoaded);
				});
			} else if (storedToken) {
				runInAction(() => {
					this._token = storedToken;
					this.transition(SessionEvent.TokenLoaded);
				});
			} else {
				runInAction(() => {
					this.transition(SessionEvent.NoToken);
				});
			}

			runInAction(() => {
				this._isInitialized = true;
			});
			logger.debug(`Initialization complete: state=${this._state}, isAuthenticated=${this.isAuthenticated}`);
		} catch (err) {
			logger.error('Initialization failed', err);
			runInAction(() => {
				this._isInitialized = true;
				this.setError(err instanceof Error ? err : new Error(String(err)));
			});
		}
	}

	private async loadStoredAccounts(): Promise<void> {
		try {
			const stored = await accountStorage.getAllAccounts();
			runInAction(() => {
				this._accounts.clear();
				for (const rec of stored) {
					if (!rec.token) {
						logger.warn(`Skipping stored account for ${rec.userId} because token is missing`);
						continue;
					}

					this._accounts.set(rec.userId, {
						userId: rec.userId,
						token: rec.token,
						userData: rec.userData,
						lastActive: rec.lastActive,
						instance: rec.instance,
						isValid: rec.isValid ?? true,
					});
				}
			});
			logger.debug(`Loaded ${stored.length} accounts`);
		} catch (err) {
			logger.error('Failed to load accounts', err);
		}
	}

	async stashCurrentAccount(): Promise<void> {
		if (!this._userId || !this._token) {
			return;
		}

		const account = this._accounts.get(this._userId);

		await accountStorage.stashAccountData(
			this._userId,
			this._token,
			account?.userData,
			RuntimeConfigStore.getSnapshot(),
		);

		runInAction(() => {
			const existing = this._accounts.get(this._userId!);
			if (existing) {
				this._accounts.set(this._userId!, {
					...existing,
					token: this._token!,
					lastActive: Date.now(),
					instance: RuntimeConfigStore.getSnapshot(),
				});
			} else {
				this._accounts.set(this._userId!, {
					userId: this._userId!,
					token: this._token!,
					lastActive: Date.now(),
					instance: RuntimeConfigStore.getSnapshot(),
					isValid: true,
				});
			}
		});
	}

	async validateToken(token: string, instance?: RuntimeConfigSnapshot): Promise<boolean> {
		const url = instance ? buildInstanceUserMeUrl(instance) : Endpoints.USER_ME;

		try {
			await http.get<unknown>({
				url,
				skipAuth: true,
				headers: {Authorization: token},
			});
			return true;
		} catch {
			return false;
		}
	}

	markAccountInvalid(userId: string): void {
		const account = this._accounts.get(userId);
		if (account) {
			runInAction(() => {
				this._accounts.set(userId, {...account, isValid: false});
			});
			void accountStorage.updateAccountValidity(userId, false);
		}
	}

	async login(token: string, userId: string, userData?: UserData): Promise<void> {
		await this.initialize();

		return await this.runExclusive(async () => {
			const snapshot = RuntimeConfigStore.getSnapshot();

			await accountStorage.stashAccountData(userId, token, userData, snapshot);

			runInAction(() => {
				this._token = token;
				this._userId = userId;
				AppStorage.setItem(StorageKey.Token, token);
				AppStorage.setItem(StorageKey.UserId, userId);

				this._accounts.set(userId, {
					userId,
					token,
					userData,
					lastActive: Date.now(),
					instance: snapshot,
					isValid: true,
				});

				if (this._state !== SessionState.Authenticated && this.canTransition(SessionEvent.TokenLoaded)) {
					this.transition(SessionEvent.TokenLoaded);
				}
			});
		});
	}

	async switchAccount(userId: string): Promise<void> {
		await this.initialize();

		return await this.runExclusive(async () => {
			const opId = this.nextOperationId();

			if (userId === this._userId) {
				logger.debug('Already on requested account');
				return;
			}

			const account = this._accounts.get(userId);
			if (!account) {
				throw new Error(`No account found for ${userId}`);
			}

			if (!this.canTransition(SessionEvent.SwitchAccount)) {
				throw new Error(`Cannot switch from state: ${this._state}`);
			}

			runInAction(() => {
				this.transition(SessionEvent.SwitchAccount);
			});

			const previousSnapshot = RuntimeConfigStore.getSnapshot();

			try {
				if (this._userId && this._token) {
					await this.stashCurrentAccount();
				}

				const isValid = await this.validateToken(account.token, account.instance);
				if (!isValid) {
					this.markAccountInvalid(userId);
					throw new SessionExpiredError();
				}

				const restored = await accountStorage.restoreAccountData(userId);
				if (!restored) {
					throw new Error(`No data found for ${userId}`);
				}

				const nextSnapshot = restored.instance ?? account.instance ?? previousSnapshot;

				LayerManager.closeAll();
				ConnectionStore.logout();
				SudoStore.clearToken();

				RuntimeConfigStore.applySnapshot(nextSnapshot);

				runInAction(() => {
					this._token = account.token;
					this._userId = userId;
					AppStorage.setItem(StorageKey.Token, account.token);
					AppStorage.setItem(StorageKey.UserId, userId);

					this._accounts.set(userId, {
						...account,
						userData: restored.userData ?? account.userData,
						lastActive: Date.now(),
						instance: nextSnapshot,
						isValid: true,
					});

					this.transition(SessionEvent.SwitchComplete);
				});

				await accountStorage.stashAccountData(
					userId,
					account.token,
					restored.userData ?? account.userData,
					nextSnapshot,
				);

				void opId;
			} catch (err) {
				logger.error('Failed to switch account', err);
				RuntimeConfigStore.applySnapshot(previousSnapshot);
				runInAction(() => {
					this.transition(SessionEvent.SwitchFailed);
				});
				throw err;
			}
		});
	}

	async logout(): Promise<void> {
		await this.initialize();

		return await this.runExclusive(async () => {
			if (!this.canTransition(SessionEvent.Logout)) {
				return;
			}

			runInAction(() => {
				this.transition(SessionEvent.Logout);
			});

			const currentUserId = this._userId;

			try {
				try {
					await http.post({url: Endpoints.AUTH_LOGOUT, timeout: 5000, retries: 0});
				} catch (err) {
					logger.warn('Logout request failed', err);
				}

				if (currentUserId) {
					try {
						await accountStorage.deleteAccount(currentUserId);
					} catch (err) {
						logger.warn('Failed to delete account', err);
					}
				}

				AppStorage.clear();

				LayerManager.closeAll();
				ConnectionStore.logout();
				SudoStore.clearToken();

				runInAction(() => {
					if (currentUserId) {
						this._accounts.delete(currentUserId);
					}
					this._token = null;
					this._userId = null;
					this._error = null;
					this.transition(SessionEvent.LogoutComplete);
				});
			} catch (err) {
				logger.error('Logout failed', err);
				runInAction(() => {
					this.transition(SessionEvent.LogoutComplete);
				});
			}
		});
	}

	async removeAccount(userId: string): Promise<void> {
		await this.initialize();
		await accountStorage.deleteAccount(userId);
		runInAction(() => {
			this._accounts.delete(userId);
			if (this._userId === userId) {
				this._userId = null;
				this._token = null;
			}
		});
	}

	handleConnectionReady(): void {
		if (this.canTransition(SessionEvent.ConnectionReady)) {
			this.transition(SessionEvent.ConnectionReady);
		}
	}

	handleConnectionClosed(code: number): void {
		if (code === 4004) {
			if (this.canTransition(SessionEvent.SessionInvalidated)) {
				this.transition(SessionEvent.SessionInvalidated);
				this.setToken(null);
				this.setUserId(null);
			}
		} else {
			if (this.canTransition(SessionEvent.ConnectionClosed)) {
				this.transition(SessionEvent.ConnectionClosed);
			}
		}
	}

	handleConnectionStarted(): void {
		if (this.canTransition(SessionEvent.StartConnection)) {
			this.transition(SessionEvent.StartConnection);
		}
	}

	handleConnectionFailed(): void {
		if (this.canTransition(SessionEvent.ConnectionFailed)) {
			this.transition(SessionEvent.ConnectionFailed);
		}
	}

	updateAccountUserData(userId: string, userData: UserData): void {
		const account = this._accounts.get(userId);
		if (account) {
			runInAction(() => {
				this._accounts.set(userId, {...account, userData});
			});
		}
	}

	reset(): void {
		this.transition(SessionEvent.Reset);
		this._token = null;
		this._userId = null;
		this._error = null;
		this._initPromise = null;
	}
}

export default new SessionManager();
