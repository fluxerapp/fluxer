// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import type {AccountPresenceIntent, UserData} from '@app/features/auth/state/AccountStorage';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

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

export type SessionState = ValueOf<typeof SessionState>;

export interface Account {
	userId: string;
	token: string;
	userData?: UserData;
	presenceIntent?: AccountPresenceIntent | null;
	lastActive: number;
	instance?: RuntimeConfigSnapshot;
	isValid: boolean;
}

export interface AuthSessionMachineContext {
	token: string | null;
	userId: string | null;
	accounts: Map<string, Account>;
	error: Error | null;
	isInitialized: boolean;
}

export type AuthSessionMachineEvent =
	| {type: 'initialize.start'}
	| {type: 'initialize.tokenLoaded'; token: string; userId: string | null}
	| {type: 'initialize.noToken'}
	| {type: 'initialize.failed'; error: Error}
	| {type: 'accounts.loaded'; accounts: ReadonlyArray<Account>}
	| {type: 'account.login'; account: Account}
	| {type: 'account.upsert'; account: Account}
	| {type: 'account.switch.start'}
	| {type: 'account.switch.complete'; account: Account}
	| {type: 'account.switch.failed'}
	| {type: 'account.markInvalid'; userId: string}
	| {type: 'account.remove'; userId: string}
	| {type: 'account.userDataUpdated'; userId: string; userData: UserData}
	| {type: 'token.set'; token: string | null}
	| {type: 'userId.set'; userId: string | null}
	| {type: 'connection.start'}
	| {type: 'connection.ready'}
	| {type: 'connection.failed'}
	| {type: 'connection.closed'}
	| {type: 'session.invalidated'}
	| {type: 'logout.start'}
	| {type: 'logout.complete'}
	| {type: 'reset'};

export function createInitialAuthSessionContext(): AuthSessionMachineContext {
	return {
		token: null,
		userId: null,
		accounts: new Map(),
		error: null,
		isInitialized: false,
	};
}

function cloneAccounts(accounts: Map<string, Account>): Map<string, Account> {
	return new Map(accounts);
}

function accountMapFromAccounts(accounts: ReadonlyArray<Account>): Map<string, Account> {
	return new Map(accounts.map((account) => [account.userId, account]));
}

function upsertAccount(accounts: Map<string, Account>, account: Account): Map<string, Account> {
	const nextAccounts = cloneAccounts(accounts);
	nextAccounts.set(account.userId, account);
	return nextAccounts;
}

function patchAccount(
	accounts: Map<string, Account>,
	userId: string,
	patch: (account: Account) => Account,
): Map<string, Account> {
	const account = accounts.get(userId);
	if (!account) return accounts;
	const nextAccounts = cloneAccounts(accounts);
	nextAccounts.set(userId, patch(account));
	return nextAccounts;
}

function removeAccount(accounts: Map<string, Account>, userId: string): Map<string, Account> {
	if (!accounts.has(userId)) return accounts;
	const nextAccounts = cloneAccounts(accounts);
	nextAccounts.delete(userId);
	return nextAccounts;
}

function accountList(accounts: Map<string, Account>): Array<Account> {
	return Array.from(accounts.values()).sort((a, b) => b.lastActive - a.lastActive);
}

export const authSessionStateMachine = setup({
	types: {
		context: {} as AuthSessionMachineContext,
		events: {} as AuthSessionMachineEvent,
	},
	actions: {
		loadAccounts: assign(({event}) => {
			if (event.type !== 'accounts.loaded') return {};
			return {accounts: accountMapFromAccounts(event.accounts)};
		}),
		loadToken: assign(({event}) => {
			if (event.type !== 'initialize.tokenLoaded') return {};
			return {
				token: event.token,
				userId: event.userId,
				error: null,
				isInitialized: true,
			};
		}),
		markInitializedWithoutToken: assign(() => ({
			token: null,
			userId: null,
			error: null,
			isInitialized: true,
		})),
		failInitialize: assign(({event}) => {
			if (event.type !== 'initialize.failed') return {};
			return {
				error: event.error,
				isInitialized: true,
			};
		}),
		login: assign(({context, event}) => {
			if (event.type !== 'account.login') return {};
			return {
				token: event.account.token,
				userId: event.account.userId,
				accounts: upsertAccount(context.accounts, event.account),
				error: null,
				isInitialized: true,
			};
		}),
		upsertAccount: assign(({context, event}) => {
			if (event.type !== 'account.upsert') return {};
			return {
				accounts: upsertAccount(context.accounts, event.account),
			};
		}),
		completeSwitch: assign(({context, event}) => {
			if (event.type !== 'account.switch.complete') return {};
			return {
				token: event.account.token,
				userId: event.account.userId,
				accounts: upsertAccount(context.accounts, event.account),
				error: null,
			};
		}),
		failSwitch: assign(() => ({error: null})),
		markInvalid: assign(({context, event}) => {
			if (event.type !== 'account.markInvalid') return {};
			return {
				accounts: patchAccount(context.accounts, event.userId, (account) => ({...account, isValid: false})),
			};
		}),
		removeAccount: assign(({context, event}) => {
			if (event.type !== 'account.remove') return {};
			const isCurrentAccount = context.userId === event.userId;
			return {
				accounts: removeAccount(context.accounts, event.userId),
				...(isCurrentAccount && {token: null, userId: null}),
			};
		}),
		updateAccountUserData: assign(({context, event}) => {
			if (event.type !== 'account.userDataUpdated') return {};
			return {
				accounts: patchAccount(context.accounts, event.userId, (account) => ({
					...account,
					userData: event.userData,
				})),
			};
		}),
		setToken: assign(({event}) => {
			if (event.type !== 'token.set') return {};
			return {token: event.token};
		}),
		setUserId: assign(({event}) => {
			if (event.type !== 'userId.set') return {};
			return {userId: event.userId};
		}),
		invalidateSession: assign(() => ({
			token: null,
			userId: null,
			error: null,
		})),
		reset: assign(() => createInitialAuthSessionContext()),
	},
}).createMachine({
	id: 'authSession',
	context: () => createInitialAuthSessionContext(),
	initial: 'idle',
	on: {
		'accounts.loaded': {actions: 'loadAccounts'},
		'account.login': {target: '.authenticated', actions: 'login'},
		'account.upsert': {actions: 'upsertAccount'},
		'account.markInvalid': {actions: 'markInvalid'},
		'account.remove': {actions: 'removeAccount'},
		'account.userDataUpdated': {actions: 'updateAccountUserData'},
		'token.set': {actions: 'setToken'},
		'userId.set': {actions: 'setUserId'},
		reset: {target: '.idle', actions: 'reset'},
	},
	states: {
		idle: {
			on: {
				'initialize.start': {target: 'initializing'},
				'initialize.tokenLoaded': {target: 'authenticated', actions: 'loadToken'},
				'logout.start': {target: 'logging_out'},
			},
		},
		initializing: {
			on: {
				'initialize.tokenLoaded': {target: 'authenticated', actions: 'loadToken'},
				'initialize.noToken': {target: 'idle', actions: 'markInitializedWithoutToken'},
				'initialize.failed': {target: 'error', actions: 'failInitialize'},
				'logout.start': {target: 'logging_out'},
			},
		},
		authenticated: {
			on: {
				'connection.start': {target: 'connecting'},
				'account.switch.start': {target: 'switching'},
				'logout.start': {target: 'logging_out'},
				'session.invalidated': {target: 'idle', actions: 'invalidateSession'},
			},
		},
		connecting: {
			on: {
				'connection.ready': {target: 'connected'},
				'connection.failed': {target: 'authenticated'},
				'connection.closed': {target: 'authenticated'},
				'logout.start': {target: 'logging_out'},
				'session.invalidated': {target: 'idle', actions: 'invalidateSession'},
			},
		},
		connected: {
			on: {
				'connection.closed': {target: 'authenticated'},
				'connection.start': {target: 'connecting'},
				'account.switch.start': {target: 'switching'},
				'logout.start': {target: 'logging_out'},
				'session.invalidated': {target: 'idle', actions: 'invalidateSession'},
			},
		},
		switching: {
			on: {
				'account.switch.complete': {target: 'authenticated', actions: 'completeSwitch'},
				'account.switch.failed': {target: 'authenticated', actions: 'failSwitch'},
				'logout.start': {target: 'logging_out'},
			},
		},
		logging_out: {
			on: {
				'logout.complete': {target: 'idle', actions: 'invalidateSession'},
			},
		},
		error: {
			on: {
				'initialize.start': {target: 'initializing'},
				'logout.start': {target: 'logging_out'},
			},
		},
	},
});

export type AuthSessionSnapshot = SnapshotFrom<typeof authSessionStateMachine>;

export function createAuthSessionSnapshot(): AuthSessionSnapshot {
	return initialTransition(authSessionStateMachine)[0];
}

export function transitionAuthSessionSnapshot(
	snapshot: AuthSessionSnapshot,
	event: AuthSessionMachineEvent,
): AuthSessionSnapshot {
	return transition(authSessionStateMachine, snapshot, event)[0] as AuthSessionSnapshot;
}

export function getAuthSessionStateValue(snapshot: AuthSessionSnapshot): SessionState {
	return snapshot.value as SessionState;
}

export function selectAuthSessionAccounts(snapshot: AuthSessionSnapshot): Array<Account> {
	return accountList(snapshot.context.accounts);
}

export function selectAuthSessionCanSwitch(snapshot: AuthSessionSnapshot): boolean {
	const state = getAuthSessionStateValue(snapshot);
	return state === SessionState.Authenticated || state === SessionState.Connected;
}
