// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {INSTANCE_UNAVAILABLE_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import {AccountInstanceUnavailableError} from '@app/features/auth/state/AccountAccess';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {type Account, SessionExpiredError} from '@app/features/platform/state/AuthSession';
import {instanceRequest, instanceTargetFromSnapshot} from '@app/features/platform/transport/InstanceHTTP';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useCallback} from 'react';

const WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: "Couldn't switch accounts. Try again.",
	comment: 'Toast error shown when switching to a different saved account fails.',
});
const SIGNING_OUT_FAILED_TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR = msg({
	message: 'Signing out failed. Try again in a moment.',
	comment: 'Toast error shown when signing out from the account switcher fails.',
});
const WE_COULDN_T_REMOVE_THIS_ACCOUNT_PLEASE_TRY_DESCRIPTOR = msg({
	message: "Couldn't remove this account. Try again.",
	comment: 'Toast error shown when removing the current account from the device fails.',
});
const INSTANCE_UNAVAILABLE_NAMED_BODY_DESCRIPTOR = msg({
	message:
		"We couldn't reach {instanceLabel} to switch accounts. Your current account is still active. Try again when the instance is back online.",
	comment:
		'Modal body shown when switching to an account on a named unreachable instance fails. instanceLabel is a hostname or endpoint label.',
});
const INSTANCE_UNAVAILABLE_UNNAMED_BODY_DESCRIPTOR = msg({
	message:
		"We couldn't reach that account's instance to switch accounts. Your current account is still active. Try again when the instance is back online.",
	comment: 'Modal body shown when switching to an account on an unreachable instance fails.',
});

const logger = new Logger('AccountSwitcherModalUtils');
const STORED_ACCOUNT_LOGOUT_TIMEOUT_MS = 5000;

function showAccountSwitcherErrorModal(message: MessageDescriptor): void {
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message: () => i18n._(message),
		dataFlx: 'auth.account-switcher-modal-utils.error-modal',
		defer: true,
	});
}

function showAccountInstanceUnavailableModal(account: Account): void {
	const instanceLabel = resolveAccountInstanceLabel(account);
	showGenericErrorModal({
		title: () => i18n._(INSTANCE_UNAVAILABLE_DESCRIPTOR),
		message: () => {
			if (instanceLabel != null && instanceLabel.length > 0) {
				return i18n._(INSTANCE_UNAVAILABLE_NAMED_BODY_DESCRIPTOR, {instanceLabel});
			}
			return i18n._(INSTANCE_UNAVAILABLE_UNNAMED_BODY_DESCRIPTOR);
		},
		dataFlx: 'auth.account-switcher-modal-utils.instance-unavailable-modal',
		defer: true,
	});
}

export interface AccountSwitcherLogic {
	readonly accounts: Array<Account>;
	readonly currentAccount: Account | null;
	readonly currentAccountKey: string | null;
	readonly isBusy: boolean;
	readonly handleSelectAccount: (account: Account) => void;
	readonly handleSwitchAccount: (accountKey: string) => Promise<void>;
	readonly handleLogout: () => Promise<void>;
	readonly handleLogoutStoredAccount: (account: Account) => Promise<void>;
}

export interface AccountSwitcherLogicOptions {
	readonly onSelectCurrent?: (() => void) | null;
	readonly onSessionExpired: ((account: Account) => void) | null;
	readonly redirectAfterSwitch: string | null;
	readonly switchAccount: ((accountKey: string) => Promise<void>) | null;
}

export interface SwitchStoredAccountRequest extends AccountSwitcherLogicOptions {
	readonly accountKey: string;
	readonly onSuccess: (() => void) | null;
}

interface ExpiredStoredAccountRequest {
	readonly account: Account | null;
	readonly error: unknown;
	readonly onSessionExpired: ((account: Account) => void) | null;
}

function handleExpiredStoredAccount({account, error, onSessionExpired}: ExpiredStoredAccountRequest): boolean {
	if (!(error instanceof SessionExpiredError) || account == null) {
		return false;
	}
	if (onSessionExpired != null) {
		onSessionExpired(account);
		return true;
	}
	logger.warn('Stored account session expired while switching', error);
	showAccountSwitcherErrorModal(WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR);
	return true;
}

export async function switchStoredAccountFromSwitcher({
	accountKey,
	onSessionExpired,
	onSuccess,
	redirectAfterSwitch,
	switchAccount,
}: SwitchStoredAccountRequest): Promise<void> {
	if (Accounts.isSwitching || Accounts.isLoading) {
		return;
	}
	const account = Accounts.getAccount(accountKey);
	try {
		if (switchAccount != null) {
			await switchAccount(accountKey);
		} else {
			await Accounts.switchToAccount(accountKey, redirectAfterSwitch);
		}
		if (onSuccess != null) {
			onSuccess();
		}
	} catch (error) {
		const preparedAccount = Accounts.getAccount(accountKey) ?? account;
		if (handleExpiredStoredAccount({account: preparedAccount, error, onSessionExpired})) {
			return;
		}
		logger.error('Failed to switch account', error);
		if (error instanceof AccountInstanceUnavailableError && preparedAccount != null) {
			showAccountInstanceUnavailableModal(preparedAccount);
			return;
		}
		showAccountSwitcherErrorModal(WE_COULDN_T_SWITCH_ACCOUNTS_PLEASE_TRY_AGAIN_DESCRIPTOR);
	}
}

async function removeStoredAccountFromSwitcher(accountKey: string): Promise<void> {
	if (Accounts.isSwitching || Accounts.isLoading) {
		return;
	}
	try {
		await Accounts.removeStoredAccount(accountKey);
	} catch (error) {
		logger.error('Failed to remove account', error);
		showAccountSwitcherErrorModal(WE_COULDN_T_REMOVE_THIS_ACCOUNT_PLEASE_TRY_DESCRIPTOR);
	}
}

export async function logoutStoredAccountFromSwitcher(account: Account): Promise<void> {
	if (Accounts.isSwitching || Accounts.isLoading) {
		return;
	}
	if (account.instance == null) {
		logger.warn(`Stored account ${getAccountKey(account)} has no instance runtime, removing it locally`);
		await removeStoredAccountFromSwitcher(getAccountKey(account));
		return;
	}
	const target = instanceTargetFromSnapshot(account.instance);
	try {
		await instanceRequest({
			method: 'POST',
			path: Endpoints.AUTH_LOGOUT,
			target,
			headers: {Authorization: account.token},
			timeoutMs: STORED_ACCOUNT_LOGOUT_TIMEOUT_MS,
			retries: 0,
			auth: 'none',
		});
	} catch (error) {
		logger.warn('Failed to log out stored account', error);
	}
	await removeStoredAccountFromSwitcher(getAccountKey(account));
}

export function useAccountSwitcherLogic({
	onSelectCurrent,
	onSessionExpired,
	redirectAfterSwitch,
	switchAccount,
}: AccountSwitcherLogicOptions): AccountSwitcherLogic {
	const accounts = switcherAccounts();
	const currentAccount = Accounts.currentAccount;
	const currentAccountKey = Accounts.currentAccountKey;
	const isBusy = Accounts.isSwitching || Accounts.isLoading;
	const handleSwitchAccount = useCallback(
		async (accountKey: string): Promise<void> => {
			await switchStoredAccountFromSwitcher({
				accountKey,
				onSessionExpired,
				onSuccess: ModalCommands.pop,
				redirectAfterSwitch,
				switchAccount,
			});
		},
		[onSessionExpired, redirectAfterSwitch, switchAccount],
	);
	const handleSelectAccount = useCallback(
		(account: Account): void => {
			if (isBusy) {
				return;
			}
			if (account.isValid === false) {
				onSessionExpired?.(account);
				return;
			}
			const accountKey = getAccountKey(account);
			if (accountKey === currentAccountKey) {
				onSelectCurrent?.();
				return;
			}
			void handleSwitchAccount(accountKey);
		},
		[currentAccountKey, handleSwitchAccount, isBusy, onSelectCurrent, onSessionExpired],
	);
	const handleLogout = useCallback(async (): Promise<void> => {
		if (Accounts.isSwitching || Accounts.isLoading) {
			return;
		}
		try {
			await Accounts.logout();
			ModalCommands.pop();
		} catch (error) {
			logger.error('Logout failed', error);
			showAccountSwitcherErrorModal(SIGNING_OUT_FAILED_TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR);
		}
	}, []);
	return {
		accounts,
		currentAccount,
		currentAccountKey,
		isBusy,
		handleSelectAccount,
		handleSwitchAccount,
		handleLogout,
		handleLogoutStoredAccount: logoutStoredAccountFromSwitcher,
	};
}
