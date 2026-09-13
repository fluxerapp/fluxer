// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type Account,
	createAuthSessionSnapshot,
	getAuthSessionStateValue,
	SessionState,
	selectAuthSessionAccounts,
	selectAuthSessionCanSwitch,
	transitionAuthSessionSnapshot,
} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import {describe, expect, it} from 'vitest';

function account(userId: string, lastActive = 1): Account {
	return {
		userId,
		token: `token-${userId}`,
		lastActive,
		isValid: true,
	};
}

describe('AuthSessionStateMachine', () => {
	it('initializes without a token and records that initialization completed', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'initialize.start'});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Initializing);

		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'initialize.noToken'});

		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Idle);
		expect(snapshot.context.isInitialized).toBe(true);
		expect(snapshot.context.token).toBeNull();
		expect(snapshot.context.userId).toBeNull();
	});

	it('loads stored accounts and sorts them by last activity', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'accounts.loaded',
			accounts: [account('older', 1), account('newer', 3), account('middle', 2)],
		});

		expect(selectAuthSessionAccounts(snapshot).map((storedAccount) => storedAccount.userId)).toEqual([
			'newer',
			'middle',
			'older',
		]);
	});

	it('moves through authenticated, connecting, and connected states', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'initialize.tokenLoaded',
			token: 'token-a',
			userId: 'user-a',
		});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Authenticated);
		expect(selectAuthSessionCanSwitch(snapshot)).toBe(true);

		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'connection.start'});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Connecting);
		expect(selectAuthSessionCanSwitch(snapshot)).toBe(false);

		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'connection.ready'});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Connected);
		expect(selectAuthSessionCanSwitch(snapshot)).toBe(true);
	});

	it('keeps account bookkeeping separate from switch state', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'account.login',
			account: account('user-a'),
		});
		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'account.switch.start'});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Switching);

		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'account.upsert',
			account: account('user-a', 5),
		});

		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Switching);
		expect(snapshot.context.accounts.get('user-a')?.lastActive).toBe(5);
	});

	it('completes switches and invalidates sessions with credential cleanup', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'account.login',
			account: account('user-a'),
		});
		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'account.switch.start'});
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'account.switch.complete',
			account: account('user-b', 10),
		});

		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Authenticated);
		expect(snapshot.context.userId).toBe('user-b');
		expect(snapshot.context.token).toBe('token-user-b');

		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'session.invalidated'});

		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Idle);
		expect(snapshot.context.userId).toBeNull();
		expect(snapshot.context.token).toBeNull();
	});

	it('clears active credentials when logout completes', () => {
		let snapshot = createAuthSessionSnapshot();
		snapshot = transitionAuthSessionSnapshot(snapshot, {
			type: 'account.login',
			account: account('user-a'),
		});
		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'logout.start'});
		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.LoggingOut);

		snapshot = transitionAuthSessionSnapshot(snapshot, {type: 'logout.complete'});

		expect(getAuthSessionStateValue(snapshot)).toBe(SessionState.Idle);
		expect(snapshot.context.token).toBeNull();
		expect(snapshot.context.userId).toBeNull();
	});
});
