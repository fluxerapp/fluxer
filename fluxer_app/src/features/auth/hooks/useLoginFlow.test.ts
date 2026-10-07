// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {selectStoredAccount} from '@app/features/auth/hooks/useLoginFlow';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import type {MessageDescriptor} from '@lingui/core';
import {beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));
vi.mock('@lingui/react/macro', () => ({
	useLingui: () => ({i18n: {_: (descriptor: MessageDescriptor) => descriptor.message}}),
}));

const mocks = vi.hoisted(() => ({
	canSwitchAccounts: true,
	switchToAccount: vi.fn<(accountKey: string) => Promise<void>>(() => Promise.resolve()),
	prepareAccountCredentials:
		vi.fn<(accountKey: string) => Promise<{token: string; userId: string; runtimeSnapshot: unknown}>>(),
	getAccount: vi.fn<(accountKey: string) => unknown>(() => null),
}));

vi.mock('@app/features/auth/state/Accounts', () => ({
	default: {
		get canSwitchAccounts() {
			return mocks.canSwitchAccounts;
		},
		switchToAccount: mocks.switchToAccount,
		prepareAccountCredentials: mocks.prepareAccountCredentials,
		getAccount: mocks.getAccount,
	},
}));

const instance = {apiEndpoint: 'https://api.example.test'} as RuntimeConfigSnapshot;

function storedAccount(overrides: Partial<Account> = {}): Account {
	return {
		storageKey: 'example.test:42',
		userId: '42',
		token: 'stored-token',
		userData: {username: 'alice', discriminator: '0001', email: 'alice@example.test'},
		lastActive: 0,
		instance,
		isValid: true,
		...overrides,
	} as Account;
}

describe('selectStoredAccount', () => {
	beforeEach(() => {
		mocks.canSwitchAccounts = true;
		mocks.switchToAccount.mockReset().mockResolvedValue(undefined);
		mocks.prepareAccountCredentials.mockReset();
		mocks.getAccount.mockReset().mockReturnValue(null);
	});

	test('switches to the account when switching is available', async () => {
		const onLoginWithStoredAccount = vi.fn(() => Promise.resolve());
		const onSessionExpired = vi.fn();
		await selectStoredAccount(storedAccount(), {onLoginWithStoredAccount, onSessionExpired});
		expect(mocks.switchToAccount).toHaveBeenCalledTimes(1);
		expect(onLoginWithStoredAccount).not.toHaveBeenCalled();
		expect(onSessionExpired).not.toHaveBeenCalled();
	});

	test('logs in with the stored token when switching is unavailable', async () => {
		mocks.canSwitchAccounts = false;
		mocks.prepareAccountCredentials.mockResolvedValue({token: 'fresh-token', userId: '42', runtimeSnapshot: instance});
		const onLoginWithStoredAccount = vi.fn(() => Promise.resolve());
		const onSessionExpired = vi.fn();
		const account = storedAccount();
		await selectStoredAccount(account, {onLoginWithStoredAccount, onSessionExpired});
		expect(mocks.switchToAccount).not.toHaveBeenCalled();
		expect(onLoginWithStoredAccount).toHaveBeenCalledWith({
			token: 'fresh-token',
			userId: '42',
			userData: account.userData,
			runtimeSnapshot: instance,
		});
		expect(onSessionExpired).not.toHaveBeenCalled();
	});

	test('shows the expired form for an account already marked invalid', async () => {
		const onLoginWithStoredAccount = vi.fn(() => Promise.resolve());
		const onSessionExpired = vi.fn();
		const account = storedAccount({isValid: false});
		await selectStoredAccount(account, {onLoginWithStoredAccount, onSessionExpired});
		expect(onSessionExpired).toHaveBeenCalledWith(account);
		expect(mocks.switchToAccount).not.toHaveBeenCalled();
		expect(mocks.prepareAccountCredentials).not.toHaveBeenCalled();
	});

	test('shows the expired form when the stored token turns out expired', async () => {
		mocks.canSwitchAccounts = false;
		mocks.prepareAccountCredentials.mockRejectedValue(new SessionExpiredError());
		const invalidated = storedAccount({isValid: false});
		mocks.getAccount.mockReturnValue(invalidated);
		const onLoginWithStoredAccount = vi.fn(() => Promise.resolve());
		const onSessionExpired = vi.fn();
		await selectStoredAccount(storedAccount(), {onLoginWithStoredAccount, onSessionExpired});
		expect(onSessionExpired).toHaveBeenCalledWith(invalidated);
		expect(onLoginWithStoredAccount).not.toHaveBeenCalled();
	});

	test('rethrows other failures', async () => {
		mocks.switchToAccount.mockRejectedValue(new Error('network'));
		const onSessionExpired = vi.fn();
		await expect(
			selectStoredAccount(storedAccount(), {onLoginWithStoredAccount: vi.fn(), onSessionExpired}),
		).rejects.toThrow('network');
		expect(onSessionExpired).not.toHaveBeenCalled();
	});
});
